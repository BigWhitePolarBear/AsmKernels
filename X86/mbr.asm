core_base_address	equ	0x00040000					; Kernel loading start address	
core_start_sector	equ	0x00000001					; Kernel start sector number

;===============================================================================
SECTION mbr vstart=0x00007c00

	mov ax,cs
	mov ss,ax
	mov sp,0x7c00
	
	; Calculate the logical segment address of GDT
	mov eax,[cs:pgdt+0x02]							; GDT 32-bit physical address
	xor edx,edx
	mov ebx,16
	div ebx											; Divided into 16-bit logical address

	mov ds,eax										; Make DS point to the segment for operation
	mov ebx,edx										; Segment offset address

	; Pass the slot of the 0# descriptor
	; Create the 1# descriptor, the code segment descriptor in protected mode, privilege level 0
	mov dword [ebx+0x08],0x0000ffff					; base=0, limit=0xFFFFF, DPL=00
	mov dword [ebx+0x0c],0x00cf9800					; 4KB granularity, code segment descriptor, upward expansion

	; Create the 2# descriptor, the data segment descriptor in protected mode, privilege level 0
	mov dword [ebx+0x10],0x0000ffff					; base=0, limit=0xFFFFF, DPL=00
	mov dword [ebx+0x14],0x00cf9200					; 4KB granularity, data segment descriptor, upward expansion
         
	; Create the 3# descriptor, the code segment descriptor in protected mode, privilege level 3
	mov dword [ebx+0x18],0x0000ffff					; base=0, limit=0xFFFFF, DPL=11
	mov dword [ebx+0x1c],0x00cffa00					; 4KB granularity, code segment descriptor, upward expansion

	; Create the 4# descriptor, the data segment descriptor in protected mode, privilege level 3
	mov dword [ebx+0x20],0x0000ffff					; base=0, limit=0xFFFFF, DPL=11
	mov dword [ebx+0x24],0x00cff200					; 4KB granularity, data segment descriptor, upward expansion

	; Initialize the descriptor table register GDTR
	mov word [cs:pgdt],39							; The limit of the descriptor table
 
	lgdt [cs:pgdt]
      
	in al,0x92										; Port of South Bridge
	or al,0000_0010B
	out 0x92,al										; Turn on A20

	cli												; Interupts are not working yet

	mov eax,cr0
	or eax,1
	mov cr0,eax										; Set PE bit
      
	; Enter protected mode
	jmp dword 0x08:flush							; 16-bit descriptor selector: 32-bit offset
													; Clear the pipeline and serialize the processor
	[bits 32]
flush:                                  
	mov eax,0x10									; Load the data segment (4GB) selector
	mov ds,eax
	mov es,eax
	mov fs,eax
	mov gs,eax
	mov ss,eax										; Load the stack segment (4GB) selector
	mov esp,0x7c00									; Stack pointer
         
	; Below is the loading of the system core program
	mov edi,core_base_address 
      
	mov eax,core_start_sector
	mov ebx,edi										; Start address
	call read_hard_disk_0							; Belows read the start part of the program (a sector)
      
	; Below is the calculation of the size of the program
	mov eax,[edi]									; Size of the core program
	xor edx,edx 
	mov ecx,512										; 512 bytes per sector
	div ecx

	or edx,edx
	jnz @1											; Not divisible, so the result is one less than the actual number of sectors
	dec eax											; Alread read a sector, so the total number of sectors minus 1
	@1:
		or eax,eax									; Consider the case where the actual length < 512 bytes
		jz pge										; EAX=0 ?

		; Read the remaining sectors
		mov ecx,eax									; The loop instruction in 32-bit mode uses ECX
		mov eax,core_start_sector
		inc eax										; Read from the next logical sector
	@2:
		call read_hard_disk_0
		inc eax
		loop @2										; Loops until the entire kernel is read

	pge:
		; Ready to open the paging mechanism. From now on, there is no need to switch between segments
		
		; Create the page directory table PDT for the system core
		mov ebx,0x00020000							; Physical address of the page directory table PDT
		
		; Create a directory entry pointing to the page directory table PDT in the page directory
		mov dword [ebx+4092],0x00020003
		
		mov edx,0x00021003							; The space of the MBR is limited, so try not to use immediate numbers later
		
		; Create a directory entry pointing to linear address 0x00000000 in the page directory
		mov [ebx],edx								; Write the directory entry (physical address and attributes of the page table)
													; This directory entry is only used for transition

		; Create a directory entry corresponding to linear address 0x80000000 in the page directory
		mov [ebx+0x800],edx							; Write the directory entry (physical address and attributes of the page table)
											
		; Create a page table corresponding to the above directory entry, and initialize the page table entry
		mov ebx,0x00021000							; Physical address of the page table
		xor eax,eax									; Physical address of the starting page
		xor esi,esi
	.b1:
		mov edx,eax
		or edx,0x00000003
		mov [ebx+esi*4],edx							; Physical address of the registration page
		add eax,0x1000								; Physical address of the next adjacent page
		inc esi
		cmp esi,256									; Only the pages corresponding to the low 1MB memory are valid
		jb .b1
		
		; Set the CR3 register to point to the page directory and officially start the page function
		mov eax,0x00020000							; PCD=PWT=0
		mov cr3,eax
		
		; Project the linear address of the GDT to the same position starting from 0x80000000
		sgdt [pgdt]
		add dword [pgdt+2],0x80000000				; GDTR also uses linear address
		lgdt [pgdt]
		
		mov eax,cr0
		or eax,0x80000000
		mov cr0,eax									; Start paging mechanism
		
		; Project the stack segment to the high end, should move all the things of the kernel to the high end
		; Otherwise, it will definitely conflict with the content in the user task space being loaded,
		; and it is difficult to think that the problem lies here.
		add esp,0x80000000
		
		jmp [0x80040004]

;-------------------------------------------------------------------------------
	read_hard_disk_0:								; Read a logical sector from the hard disk
													; EAX=logical sector number
													; DS:EBX=Target buffer address
													; Return EBX=EBX+512 
		push eax 
		push ecx
		push edx
      
		push eax
         
		mov dx,0x1f2
		mov al,1
		out dx,al									; Read one sector

		inc dx										; 0x1f3
		pop eax
		out dx,al									; LBA address 7~0

		inc dx										; 0x1f4
		mov cl,8
		shr eax,cl
		out dx,al									; LBA address 15~8

		inc dx										; 0x1f5
		shr eax,cl
		out dx,al									; LBA address 23~16

		inc dx										; 0x1f6
		shr eax,cl	
		or al,0xe0									; First hard disk LBA address 27~24
		out dx,al

		inc dx										; 0x1f7
		mov al,0x20									; Read instruction
		out dx,al

		.waits:
			in al,dx
			and al,0x88
			cmp al,0x08
			jnz .waits								; Then the hard disk is ready to transfer data

		mov ecx,256									; Total number of words to be read
		mov dx,0x1f0
		.readw:
			in ax,dx
			mov [ebx],ax
			add ebx,2
			loop .readw

		pop edx
		pop ecx
		pop eax
      
		ret

;-------------------------------------------------------------------------------
pgdt	dw 0
		dd 0x00008000								; Physical/Linear address of GDT
;-------------------------------------------------------------------------------                             
; Fill up to 512 bytes
times 510-($-$$) db 0
db 0x55,0xaa