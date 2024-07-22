; Below is the constant definition part. Most of the contents of the kernel should be fixed.
flat_kernel_code_seg_sel	equ	0x0008				; Flat model kernel 4GB code (privilege level 0) segment selector
flat_kernel_data_seg_sel	equ	0x0010				; Flat model kernel 4GB data (privilege level 0) segment selector
flat_user_code_seg_sel		equ	0x001b				; Flat model user 4GB code (privilege level 3) segment selector
flat_user_data_seg_sel		equ	0x0023				; Flat model user 4GB data (privilege level 3) segment selector

idt_linear_address		equ	0x8001f000				; IDT linear address
core_lin_alloc_at		equ	0x80100000				; Start linear address available for allocation in the kernel
core_lin_tcb_addr		equ	0x8001f800				; High end linear address of the kernel task TCB

;-------------------------------------------------------------------------------
; Below is the header of the system core, used to load the core program
SECTION header vstart=0x80040000

	core_length	dd core_end							; #00 Core program total length

	core_entry	dd start							; #04 Core code segment entry point

;===============================================================================
[bits 32]
;===============================================================================
SECTION sys_routine vfollows=header					; System routine code segment
;-------------------------------------------------------------------------------
; String display routine (for flat memory model)
put_string:											; Display 0-terminated string and move cursor
                                            		; In: EBX=Linear address of the string

	push ebx
	push ecx

	pushfd
	cli												; Close interrupt while hardware operation

	.getc:
		mov cl,[ebx]
		or cl,cl									; Check the end flag (0)
		jz .exit									; Display finished, return
		call put_char
		inc ebx
		jmp .getc

	.exit:
		popfd										; Open interrupt when hardware operation is completed

		pop ecx
		pop ebx

		ret											; Return to the caller

;-------------------------------------------------------------------------------
put_char:											; Display a character at the current cursor position and move the cursor.
													; Only for intra-segment calls.
                                            		; In: CL=ASCII code of the character
	pushad

	; Below is to get the current cursor position
	mov dx,0x3d4
	mov al,0x0e
	out dx,al
	inc dx											; 0x3d5
	in al,dx										; High byte
	mov ah,al

	dec dx											; 0x3d4
	mov al,0x0f
	out dx,al
	inc dx											; 0x3d5
	in al,dx										; Low byte
	mov bx,ax										; BX=16-bit number representing cursor position
	and ebx,0x0000ffff								; Ready to access video memory using 32-bit addressing

	cmp cl,0x0d										; \r?
	jnz .put_0a
	mov ax,bx
	mov bl,80
	div bl
	mul bl
	mov bx,ax
	jmp .set_cursor

	.put_0a:
		cmp cl,0x0a									; \n?
		jnz .put_other
		add bx,80
		jmp .roll_screen

	.put_other:										; Display the character
		shl bx,1
		mov [0x800b8000+ebx],cl

	; Below is to move the cursor forward by one character
	shr bx,1
	inc bx

	.roll_screen:
		cmp bx,2000									; Roll the screen if the cursor is out of the screen
		jl .set_cursor

		push bx

		cld
		mov esi,0x800b80a0							; 32-bit mode movsb/w/d use ESI/EDI/ECX
		mov edi,0x800b8000
		mov ecx,1920
		rep movsd
		mov bx,3840									; Clear the bottom line of the screen
		mov ecx,80
		.cls:
			mov word[0x800b8000+ebx],0x0720
			add bx,2
			loop .cls

	pop BX
	sub bx,80

	.set_cursor:
		mov dx,0x3d4
		mov al,0x0e
		out dx,al
		inc dx										; 0x3d5
		mov al,bh
		out dx,al
		dec dx										; 0x3d4
		mov al,0x0f
		out dx,al
		inc dx										; 0x3d5
		mov al,bl
		out dx,al

	popad
	ret

;-------------------------------------------------------------------------------
read_hard_disk_0:									; Read a logical sector from the hard disk (flat model)
													; EAX=logical sector number
													; EBX=Target buffer linear address
													; Return: EBX=EBX+512
	cli

	push eax
	push ecx
	push edx

	push eax

	mov dx,0x1f2
	mov al,1
	out dx,al										; Number of sectors to read

	inc dx											; 0x1f3
	pop eax
	out dx,al										; LBA address 7~0

	inc dx											; 0x1f4
	mov cl,8
	shr eax,cl
	out dx,al										; LBA address 15~8

	inc dx											; 0x1f5
	shr eax,cl
	out dx,al										; LBA address 23~16

	inc dx											; 0x1f6
	shr eax,cl
	or al,0xe0										; First hard disk LBA address 27~24
	out dx,al

	inc dx											; 0x1f7
	mov al,0x20										; Read command
	out dx,al

	.waits:
		in al,dx
		and al,0x88
		cmp al,0x08
		jnz .waits									; The hard disk is not busy and ready for data transfer

	mov ecx,256										; Toal number of words to read
	mov dx,0x1f0
	.readw:
		in ax,dx
		mov [ebx],ax
		add ebx,2
		loop .readw

	pop edx
	pop ecx
	pop eax

	sti

	ret

;-------------------------------------------------------------------------------
put_hex_dword:										; Display a double word in hexadecimal at the current cursor position and move the cursor
                                            		; In: EDX=Number to be converted and displayed
                                            		; Out: None
	pushad

	mov ebx,bin_hex									; Point to the conversion table in the core address space
	mov ecx,8
	.xlt:
		rol edx,4
		mov eax,edx
		and eax,0x0000000f
		xlat

	push ecx
	mov cl,al
	call put_char
	pop ecx

	loop .xlt

	popad
	ret

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:								; Install a new descriptor in the GDT
													; In: EDX:EAX=Descriptor
													; Out: CX=Selector of the descriptor
	push eax
	push ebx
	push edx

	sgdt [pgdt]										; Get the GDT limit and linear address

	movzx ebx,word [pgdt]							; Limit of GDT
	inc bx											; Total number of bytes in GDT, also the next descriptor offset
	add ebx,[pgdt+2]								; Linear address of the next descriptor

	mov [ebx],eax
	mov [ebx+4],edx

	add word [pgdt],8								; Add one descriptor size to the GDT limit

	lgdt [pgdt]										; Make the change to the GDT effective

	mov ax,[pgdt]									; Get the GDT limit
	xor dx,dx
	mov bx,8
	div bx											; Divided by 8, remove the remainder
	mov cx,ax
	shl cx,3										; Move the index to the correct position

	pop edx
	pop ebx
	pop eax

	ret
	
;-------------------------------------------------------------------------------
make_seg_descriptor:								; Construct memory and system segment descriptors
                                            		; In: EAX=Linear base address
                                            		;     EBX=Segmentation limit
                                            		;     ECX=Attribute. All attribute bits are in the original position, and irrelevant bits are cleared
                                            		; Out: EDX:EAX=Descriptor
	mov edx,eax
	shl eax,16
	or ax,bx										; Construction of the first 32 bits of the descriptor (EAX)

	and edx,0xffff0000								; Clear irrelevant bits in the base address
	rol edx,8
	bswap edx										; Set the high 4 bits of the base address (31~24) and the low 4 bits (23~16)

	xor bx,bx
	or edx,ebx										; Set the high 4 bits of the limit

	or edx,ecx										; Set the attribute

	ret

;-------------------------------------------------------------------------------
make_gate_descriptor:								; Const Gate Descriptor(For Call Gate)
                                            		; In: EAX=Offset of the gate in the segment
                                            		;     BX=Selector of the segment where the gate code is located
                                            		;     CX=Segment type and attributes (all attribute bits are in the original position)
                                            		; Out: EDX:EAX= Complete descriptor
	push ebx
	push ecx

	mov edx,eax
	and edx,0xffff0000								; Get the high 16 bits of the offset
	or dx,cx										; Set the attribute part to EDX

	and eax,0x0000ffff								; Get the low 16 bits of the offset
	shl ebx,16
	or eax,ebx										; Set the segment selector part to EAX

	pop ecx
	pop ebx

	ret

;-------------------------------------------------------------------------------
allocate_a_4k_page:									; Allocate a 4KB page
                                            		; In: None
                                            		; Out: EAX=Physical address of the page
	push ebx
	push ecx
	push edx

	xor eax,eax
	.b1:
		bts [page_bit_map],eax
		jnc .b2
		inc eax
		cmp eax,page_map_len*8
		jl .b1

	mov ebx,message_3
	call put_string
	hlt												; Have no page to allocate, halt

	.b2:
		shl eax,12									; Multiplied by 4096 (0x1000)

		pop edx
		pop ecx
		pop ebx

		ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:									; Allocate a page and install it in the current active hierarchical paging structure
                                            		; In: EBX=Linear address of the page
	push eax
	push ebx
	push ecx
	push esi

	; Check if the page table corresponding to the linear address exists
	mov esi,ebx
	and esi,0xffc00000								; Clear the page table index and page offset part
	shr esi,20										; Multiply the page directory index by 4 as the page offset
	or esi,0xfffff000								; The linear address and the page offset of the page table corresponding to the linear address

	test dword [esi],0x00000001						; Check if the page corresponding to the linear address exists
	jnz .b1

	; Allocate a page as a page table
	call allocate_a_4k_page
	or eax,0x00000007
	mov [esi],eax									; Register this page table in the page directory

	; Clear the current page table
	mov eax,ebx
	and eax,0xffc00000
	shr eax,10
	or eax,0xffc00000
	mov ecx,1024
	.cls0:
		mov dword [eax],0x00000000
		add eax,4
		loop .cls0

	.b1:
		; Check if the page table entry (page) corresponding to the linear address exists
		mov esi,ebx
		and esi,0xfffff000							; Clear the page offset part
		shr esi,10									; Set the page directory index as the page table index, and multiply the page table index by 4 as the page offset
		or esi,0xffc00000							; Get the page table entry corresponding to the linear address

		test dword [esi],0x00000001					; Check if the page corresponding to the linear address exists
		jnz .b2

		; Create and install the page corresponding to the linear address
		call allocate_a_4k_page						; Allocate a page, this is the page to be installed
		or eax,0x00000007
		mov [esi],eax

	.b2:
		pop esi
		pop ecx
		pop ebx
		pop eax

		ret

;-------------------------------------------------------------------------------
create_copy_cur_pdir:								; Create a new page directory and copy the content of the current page directory
                                            		; In: None
                                            		; Out: EAX=Physical address of the new page directory
	push esi
	push edi
	push ebx
	push ecx

	call allocate_a_4k_page
	mov ebx,eax
	or ebx,0x00000007
	mov [0xfffffff8],ebx

	invlpg [0xfffffff8]

	mov esi,0xfffff000								; ESI->Current linear address of the page directory
	mov edi,0xffffe000								; EDI->Linear address of the new page directory
	mov ecx,1024									; ECX=Number of directory items to copy
	cld
	repe movsd

	pop ecx
	pop ebx
	pop edi
	pop esi

	ret

;-------------------------------------------------------------------------------
task_alloc_memory:									; Allocate memory in the virtual memory space of the specified task
                                            		; In: EBX=Linear address of the task control block TCB
                                            		;     ECX=Number of bytes hoped to allocate
                                            		; Out: ECX= Start linear address of the allocated memory
	push eax

	push ebx										;to A

	; Get the start linear address of the current memory allocation
	mov ebx,[ebx+0x06]								; Get the start linear address of the current memory allocation
	mov eax,ebx
	add ecx,ebx										; Linear address after the last byte of this allocation

	push ecx										;To B

	; Allocate pages for the requested memory
	and ebx,0xfffff000
	and ecx,0xfffff000
	.next:
		call alloc_inst_a_page
													; Install the page where the current linear address is located
		add ebx,0x1000								; +4096
		cmp ebx,ecx
		jle .next

	; Set the linear address used for the next allocation to be 4-byte aligned
	pop ecx											; B

	test ecx,0x00000003								; Is the linear address 4-byte aligned?
	jz .algn										; Yes, return directly
	add ecx,4										; No, force alignment to 4 bytes
	and ecx,0xfffffffc

	.algn:
		pop ebx										; A

		mov [ebx+0x06],ecx							; Store the linear address available for the next allocation back to the TCB
		mov ecx,eax

		pop eax

		ret

;-------------------------------------------------------------------------------
allocate_memory:									; Allocate memory in the address space of the current task
                                            		; In: ECX=Number of bytes hoped to allocate
                                            		; Out: ECX= Start linear address of the allocated memory
	push eax
	push ebx

	; Get the linear address of the first node of the TCB chain
	mov eax,[tcb_chain]								; EAX=Linear address of the first node

	; Search for the node with the status busy (current task)
	.s0:
		cmp word [eax+0x04],0xffff
		jz .s1										; Found the busy node, EAX=Linear address of the node
		mov eax,[eax]
		jmp .s0

	; Begin to allocate memory
	.s1:
		mov ebx,eax
		call task_alloc_memory

		pop ebx
		pop eax

		ret

; ;-------------------------------------------------------------------------------
resume_task_execute:								; Resume the execution of the specified task
													; In: EDI=Linear address of the TCB of the new task
	mov eax,[edi+10]
	mov [tss+4],eax									; Set the RSP0 field of TSS with the RSP0 of the new task

	mov eax,[edi+22]
	mov cr3,eax										; Restore the CR3 of the new task

	mov ds,[edi+34]
	mov es,[edi+36]
	mov fs,[edi+38]
	mov gs,[edi+40]
	mov ecx,[edi+50]
	mov edx,[edi+54]
	mov ebp,[edi+66]
	; There is no need to restore EAX/EBX/ESI/EDI, 
	; they will be automatically popped from the stack and restored when the task is resumed.

	test word [edi+32],3							; SS.RPL=3？
	jnz .to_r3										; Yes, jump to .to_r3
	mov esp,[edi+70]
	mov ss,[edi+32]
	jmp .do_sw

	.to_r3:
		push dword [edi+32]							; SS
		push dword [edi+70]							; ESP
	.do_sw:
		push dword [edi+74]							; EFLAGS
		push dword [edi+30]							; CS
		push dword [edi+26]							; EIP

		not word [edi+0x04]							; Status of the node with the status ready to busy

	iretd

;-------------------------------------------------------------------------------
initiate_task_switch:								; Ask for task switch
													; In: None
                                            		; Out: None
	push eax
	push ebx
	push esi
	push edi

	mov eax,[tcb_chain]
	cmp eax,0										; Is the TCB chain empty?
	jz .return

	; Search for the node with the status busy (current task)
	.b0:
		cmp word [eax+0x04],0xffff
		cmove esi,eax								; Found the busy node, ESI=Linear address of the node
		jz .b1
		mov eax,[eax]
		jmp .b0

	; Search for the node with the status ready
	.b1:
		mov ebx,[eax]
		or ebx,ebx
		jz .b2										; Have not found a ready node at the end of the list, start from the beginning
		cmp word [ebx+0x04],0						; Is the node with the status ready?
		cmove edi,ebx								; Found the ready node, EDI=Linear address of the node
		jz .b3
		mov eax,ebx
		jmp .b1

	.b2:
		mov ebx,[tcb_chain]							; EBX=Linear address of the first node
	.b20:
		cmp word [ebx+0x04],0x0000
		cmove edi,ebx								; Found the ready node, EDI=Linear address of the node
		jz .b3
		mov ebx,[ebx]
		or ebx,ebx
		jz .return									; There is no ready task in the list, return
		jmp .b20

	; Found the node with the status ready, prepare to switch to that task
	.b3:
		; Save the context of the current task
		mov eax,cr3
		mov [esi+22],eax							; Save CR3
		; There is no need to save EAX/EBX/ESI/EDI, they will be automatically popped from the stack and restored when the task is resumed
		mov [esi+50],ecx
		mov [esi+54],edx
		mov [esi+66],ebp
		mov [esi+70],esp
		mov dword [esi+26], .return					; The EIP when the task is resumed
		mov [esi+30],cs
		mov [esi+32],ss
		mov [esi+34],ds
		mov [esi+36],es
		mov [esi+38],fs
		mov [esi+40],gs
		pushfd
		pop dword [esi+74]
		not word [esi+0x04]							; Switch the ready node to the busy node
		
		jmp resume_task_execute

	.return:
		pop edi
		pop esi
		pop ebx
		pop eax

		ret

;-------------------------------------------------------------------------------
terminate_current_task:								; Terminate the current task
													; Note: When this routine is executed, the current task is still running. This routine is actually part of the current task
	
	mov eax,[tcb_chain]								; EAX=Linear address of the first node
	; Search for the node with the status busy (current task)
	.s0:
		cmp word [eax+0x04],0xffff
		jz .s1										; Find the busy node, EAX=Linear address of the node
		mov eax,[eax]
		jmp .s0

	; Set the node with the status busy to the terminated status
	.s1:
		mov word [eax+0x04],0x3333

		; Search for the node with the status ready
		mov ebx,[tcb_chain]							; EBX=Linear address of the first node
	.s2:
		cmp word [ebx+0x04],0x0000
		jz .s3										; Found the ready node, EBX=Linear address of the node
		mov ebx,[ebx]
		jmp .s2

	; Found the node with the status ready, prepare to switch to that task
	.s3:
		jmp resume_task_execute

;-------------------------------------------------------------------------------
general_interrupt_handler:							; General interrupt handling process
	push eax

	mov al,0x20										; EOI (End of Interrupt)
	out 0xa0,al										; Send to the slave
	out 0x20,al										; Send to the master

	pop eax

	iretd

;-------------------------------------------------------------------------------
general_exception_handler:							; General exception handling process
	mov ebx,excep_msg
	call put_string

	cli

	hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:							; Handling process of real-time clock interrupt

	push eax

	mov al,0x20										; EOI (End of Interrupt)
	out 0xa0,al										; Send to the slave
	out 0x20,al										; Send to the master

	mov al,0x0c										; Index of register C, and open NMI
	out 0x70,al
	in al,0x71										; Read register C of RTC, otherwise the interrupt occurs only once
													; Do not consider the alarm clock and periodic interrupt
	; Ask for task switch
	call initiate_task_switch

	pop eax

	iretd

;-------------------------------------------------------------------------------
do_task_clean:										; Clear the terminated task and recycle resources

	; Search the TCB chain list and find the node with the status terminated
	; Remove the node from the list
	; Recycle the resources occupied by the task (can be found from its TCB)
	; Not implemented
	ret

;-------------------------------------------------------------------------------
int_0x88_handler:									; System call processing procedure

	call [eax*4+sys_entries]
	iretd
	
;===============================================================================
SECTION core_data vfollows=sys_routine				; Data segment of the system core
;------------------------------------------------------------------------------- 
	pgdt			dw  0							; For setting and modifying GDT
					dd  0

	pidt			dw  0
					dd  0

	page_bit_map	db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
					db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
					db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
					db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
					db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
					db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
					db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
					db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
	page_map_len	equ $-page_bit_map

	; System call entries
	sys_entries		dd put_string
					dd read_hard_disk_0
					dd put_hex_dword
					dd terminate_current_task
					dd initiate_task_switch
					dd allocate_memory

	message_0		db 'Setup interrupt system and system-call......', 0

	message_1		db  'Done.', 0x0d, 0x0a, 0

	message_2		db  'TSS is created.', 0x0d, 0x0a, 0

	cpu_brnd0		db  0x0d,0x0a,'  ',0
	cpu_brand		times 52  db 0
	cpu_brnd1		db  0x0d,0x0a,0x0d,0x0a,0

	message_3		db  '********No more pages********',0

	excep_msg		db  '********Exception encounted********',0

	bin_hex			db '0123456789ABCDEF'			; Lookup table used by the put_hex_dword subroutine

	core_buf		times 2048 db 0					; Buffer used by the kernel

	tcb_chain		dd  0

	tss				times 128 db 0

	core_msg1		db  '  Core task created.',0x0d,0x0a,0

	core_msg2		db  '[CORE TASK]: I am working!',0x0d,0x0a,0

core_data_end:
;===============================================================================
SECTION core_code vfollows=core_data
;------------------------------------------------------------------------------- 
load_relocate_program:								; Load and relocate user program
                                            		; In: PUSH logical sector number
                                            		;     PUSH TCB base address
                                            		; Out: None
	pushad

	mov ebp,esp										; Prepare to access parameters passed through the stack

	; Clear the first half of the current page directory (corresponding to the local address space of the lower 2GB)
	mov ebx,0xfffff000
	xor esi,esi
	.clsp:
		mov dword [ebx+esi*4],0x00000000
		inc esi
		cmp esi,512
		jl .clsp

	mov ebx,cr3										; Refresh TLB
	mov cr3,ebx

	; Load the user program header data
	mov eax,[ebp+10*4]								; Get the starting sector number of the user program from the stack
	mov ebx,core_buf								; Read the program header data
	call read_hard_disk_0

	; Determine the size of the entire program
	mov eax,[core_buf]								; Size of the program
	mov ebx,eax
	and ebx,0xfffffe00								; Make it 512-byte aligned (the low 9 bits of a number that can be divided by 512 are all 0)
	add ebx,512
	test eax,0x000001ff								; Is the size of the program a multiple of 512?
	cmovnz eax,ebx									; No, use the rounded result

	mov esi,[ebp+9*4]								; Get the base address of the TCB from the stack

	mov ecx,eax										; The actual amount of memory required
	mov ebx,esi
	call task_alloc_memory

	mov ebx,ecx										; EBX -> Start linear address of the allocated memory
	xor edx,edx
	mov ecx,512
	div ecx
	mov ecx,eax										; Total number of sectors

	mov eax,[ebp+10*4]								; Start sector number
	.b1:
		call read_hard_disk_0
		inc eax
		loop .b1									; Loops to read until the entire user program is read

	; Allocate stack space for the user task
	mov ebx,esi										; TCB linear address
	mov ecx,4096									; 4KB space
	call task_alloc_memory
	mov ecx, [esi+6]								; The starting linear address of the next allocation is the stack pointer
	mov dword [esi+70],ecx

	; Create a 0 privilege level stack space for interrupts and call gates
	mov ebx,esi
	mov ecx,4096									; 4KB space
	call task_alloc_memory
	mov ecx, [esi+6]								; The starting linear address of the next allocation is the stack pointer
	mov dword [esi+10],ecx						; TCB's ESP0 field

	; Create a page directory for the user task
	; The allocation and use of pages are determined by the page bit map,
	; and the linear address space is not occupied
	call create_copy_cur_pdir
	mov [esi+22],eax								; Fill in the TCB's CR3 (PDBR) field

	mov word [esi+30],flat_user_code_seg_sel		; The CS field of the TCB
	mov word [esi+32],flat_user_data_seg_sel		; The SS field of the TCB
	mov word [esi+34],flat_user_data_seg_sel		; The DS field of the TCB
	mov word [esi+36],flat_user_data_seg_sel		; The ES field of the TCB
	mov word [esi+38],flat_user_data_seg_sel		; The FS field of the TCB
	mov word [esi+40],flat_user_data_seg_sel		; The GS field of the TCB
	mov eax,[0x04]									; Get the entry point from the 4GB address space of the task
	mov [esi+26],eax								; The EIP field of the TCB
	pushfd
	pop dword [esi+74]								; Fill in the EFLAGS field of the TCB
	mov word [esi+4],0								; Task status: ready

	popad

	ret 8											; Discard the parameters pushed before calling this routine
      
;-------------------------------------------------------------------------------
append_to_tcb_link:									; Append the task control block to the TCB chain
													; Input: ECX=TCB linear base address
	push eax
	push edx

	cli

	mov dword [ecx+0x00],0							; The current TCB pointer field is cleared to indicate that this is the last TCB
										
	mov eax,[tcb_chain]								; The TCB header pointer
	or eax,eax										; Is the list empty?
	jz .notcb 
         
	.searc:
		mov edx,eax
		mov eax,[edx+0x00]
		or eax,eax               
		jnz .searc

		mov [es: edx+0x00],ecx
		jmp .retpc
         
	.notcb:
		mov [tcb_chain],ecx							; If it is an empty table, directly point the table header pointer to TCB
         
	.retpc:
		sti

		pop edx
		pop eax
		
		ret
         
;-------------------------------------------------------------------------------
start:
	mov ebx,message_0
	call put_string

	; Create the interrupt descriptor table IDT
	; Note! During this period, interrupts must not be opened, and the put_string routine must not be called!
	; The first 20 vectors are used by processor exceptions
	mov eax,general_exception_handler				; Offset address of the gate code in the segment
	mov bx,flat_kernel_code_seg_sel					; Selector of the gate code
	mov cx,0x8e00									; 32-bit interrupt gate, 0 privilege level
	call make_gate_descriptor

	mov ebx,idt_linear_address						; Linear address of the interrupt descriptor table
	xor esi,esi
	.idt0:
		mov [ebx+esi*8],eax
		mov [ebx+esi*8+4],edx
		inc esi
		cmp esi,19									; Install the first 20 exception interrupt handling processes
		jle .idt0

	; The rest are interrupt vectors reserved or used by hardware
	mov eax,general_interrupt_handler				; Offset address of the gate code in the segment
	mov bx,flat_kernel_code_seg_sel					; Selector of the gate code
	mov cx,0x8e00									; 32-bit interrupt gate, 0 privilege level
	call make_gate_descriptor

	mov ebx,idt_linear_address						; Linear address of the interrupt descriptor table
	.idt1:
		mov [ebx+esi*8],eax
		mov [ebx+esi*8+4],edx
		inc esi
		cmp esi,255									; Install ordinary interrupt handling processes
		jle .idt1

	; Set up real-time clock interrupt handling process
	mov eax,rtm_0x70_interrupt_handle				; Offset address of the gate code in the segment
	mov bx,flat_kernel_code_seg_sel					; Selector of the gate code
	mov cx,0x8e00									; 32-bit interrupt gate, 0 privilege level
	call make_gate_descriptor

	mov ebx,idt_linear_address						; Linear address of the interrupt descriptor table
	mov [ebx+0x70*8],eax
	mov [ebx+0x70*8+4],edx

	; Set syscall interrupt handling process
	mov eax,int_0x88_handler						; Offset address of the gate code in the segment
	mov bx,flat_kernel_code_seg_sel					; Selector of the gate code
	mov cx,0xee00									; 32-bit interrupt gate, 3 privilege level
	call make_gate_descriptor

	mov ebx,idt_linear_address						; Linear address of the interrupt descriptor table
	mov [ebx+0x88*8],eax
	mov [ebx+0x88*8+4],edx

	; Prepare to open the interrupt
	mov word [pidt],256*8-1							; Limit of IDT
	mov dword [pidt+2],idt_linear_address
	lidt [pidt]										; Load the interrupt descriptor table register IDTR

	; Test syscall
	mov ebx,message_1
	mov eax,0
	int 0x88

	; Set up 8259A interrupt controller
	mov al,0x11
	out 0x20,al										; ICW1: Edge-triggered/cascade mode
	mov al,0x20
	out 0x21,al										; ICW2: Starting interrupt vector
	mov al,0x04
	out 0x21,al										; ICW3: Cascade to IR2
	mov al,0x01
	out 0x21,al										; ICW4: Non-bus buffer, full nesting, normal EOI

	mov al,0x11
	out 0xa0,al										; ICW1: Edge-triggered/cascade mode
	mov al,0x70
	out 0xa1,al										; ICW2: Starting interrupt vector
	mov al,0x02
	out 0xa1,al										; ICW3: Cascade to IR2
	mov al,0x01
	out 0xa1,al										; ICW4: Non-bus buffer, full nesting, normal EOI

	; Set up hardware related to clock interrupt
	mov al,0x0b										; RTC register B
	or al,0x80										; Block NMI
	out 0x70,al
	mov al,0x12										; Set register B, disable periodic interrupt, open update end interrupt, BCD code, 24-hour system
	out 0x71,al

	in al,0xa1										; Read the IMR register of the 8259 slave
	and al,0xfe										; Clear bit 0 (this bit is connected to RTC)
	out 0xa1,al										; Write back to this register

	mov al,0x0c
	out 0x70,al
	in al,0x71										; Read RTC register C, reset the pending interrupt status

	sti												; Open hardware interrupt

	; Display processor brand information
	mov eax,0x80000002
	cpuid
	mov [cpu_brand+0x00],eax
	mov [cpu_brand+0x04],ebx
	mov [cpu_brand+0x08],ecx
	mov [cpu_brand+0x0c],edx

	mov eax,0x80000003
	cpuid
	mov [cpu_brand+0x10],eax
	mov [cpu_brand+0x14],ebx
	mov [cpu_brand+0x18],ecx
	mov [cpu_brand+0x1c],edx

	mov eax,0x80000004
	cpuid
	mov [cpu_brand+0x20],eax
	mov [cpu_brand+0x24],ebx
	mov [cpu_brand+0x28],ecx
	mov [cpu_brand+0x2c],edx

	mov ebx,cpu_brnd0								; Display processor brand information
	call put_string
	mov ebx,cpu_brand
	call put_string
	mov ebx,cpu_brnd1
	call put_string

	; Create a descriptor for the task state segment TSS. The entire system actually only needs one TSS.
	mov ecx, 32
	xor ebx, ebx
	.clear:
		mov dword [tss+ebx],0						; Most of the fields of the TSS are useless
		add ebx,4
		loop .clear

	; The stack switch occurs when the privilege level changes, the system only switches from 3 to 0.
	; Therefore, only SS0 needs to be set in the TSS, and it must be a stack segment selector of privilege level 0.
	mov word [tss+8],flat_kernel_data_seg_sel
	mov word [tss+102],103							; No I/O permission bitmap part

	; Create a TSS descriptor and install it in the GDT
	mov eax,tss										; TSS linear address
	mov ebx,103										; Segment length (limit)
	mov ecx,0x00008900								; TSS Descriptor, DPL=0
	call make_seg_descriptor
	call set_up_gdt_descriptor

	; Load the task register TR with the selector of the TSS, never change it
	ltr cx

	; Created TSS.
	mov ebx,message_2
	call put_string

	; Create and establish the kernel task
	mov ecx, core_lin_tcb_addr						; TCB linear address of the kernel task after moving to the high end
	mov word [ecx+4],0xffff							; Task status: busy
	mov dword [ecx+6],core_lin_alloc_at				; Set the starting linear address available for allocation in the kernel
	call append_to_tcb_link							; Add the TCB of the kernel task to the TCB chain

	; Now the "Program Manager" task is considered to be executing
	mov ebx,core_msg1
	call put_string

	; Create user tasks below
	mov ecx, 128									; Allocate memory for TCB
	call allocate_memory
	mov word [ecx+0x04],0							; Task status: ready
	mov dword [ecx+0x06],0							; The initial linear address available for allocation in the task

	push dword 50									; The user program is located in logical sector 50
	push ecx										; TCB linear address
	call load_relocate_program
	call append_to_tcb_link							; Add this TCB to the TCB chain

	mov ecx, 128									; Allocate memory for TCB
	call allocate_memory
	mov word [ecx+0x04],0							; Task status: ready
	mov dword [ecx+0x06],0							; The initial linear address available for allocation in the task

	push dword 100									; The user program is located in logical sector 50
	push ecx										; TCB linear address
	call load_relocate_program
	call append_to_tcb_link							; Add this TCB to the TCB chain

	.do_switch:
		mov ebx,core_msg2
		call put_string

		; Fake
		call do_task_clean

		hlt

		jmp .do_switch

	core_code_end:
;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
	core_end: