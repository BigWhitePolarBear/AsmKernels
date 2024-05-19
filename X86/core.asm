; Below is the constant definition part. Most of the contents of the kernel should be fixed.
flat_4gb_code_seg_sel	equ	0x0008			; Flat model 4GB code segment selector
flat_4gb_data_seg_sel	equ	0x0018			; Flat model 4GB data segment selector

idt_linear_address		equ	0x8001F000		; IDT linear address
core_lin_alloc_at		equ	0x80100000		; Start linear address available for allocation in the kernel
core_lin_tcb_addr		equ	0x8001f800		; High end linear address of the kernel task TCB

;-------------------------------------------------------------------------------
; Below is the header of the system core, used to load the core program
SECTION header vstart=0x80040000

	core_length	dd core_end		; #00 Core program total length

	core_entry	dd start		; #04 Core code segment entry point

;===============================================================================
[bits 32]
;===============================================================================
SECTION sys_routine vfollows=header		; System routine code segment
;-------------------------------------------------------------------------------
; String display routine (for flat memory model)
put_string:									; Display 0-terminated string and move cursor
                                            ; In: EBX=Linear address of the string

	push ebx
	push ecx

	cli										; Close interrupt while hardware operation

	.getc:
		mov cl,[ebx]
		or cl,cl							; Check the end flag (0)
		jz .exit							; Display finished, return
		call put_char
		inc ebx
		jmp .getc

	.exit:

		sti									; Open interrupt when hardware operation is completed

		pop ecx
		pop ebx

		retf								; Return to the caller

;-------------------------------------------------------------------------------
put_char:                                   ; Display a character at the current cursor position and move the cursor. Only for intra-segment calls
                                            ; In: CL=ASCII code of the character
	pushad

	; Below is to get the current cursor position
	mov dx,0x3d4
	mov al,0x0e
	out dx,al
	inc dx									; 0x3d5
	in al,dx								; High byte
	mov ah,al

	dec dx									; 0x3d4
	mov al,0x0f
	out dx,al
	inc dx									; 0x3d5
	in al,dx								; Low byte
	mov bx,ax								; BX=16-bit number representing cursor position
	and ebx,0x0000ffff						; Ready to access video memory using 32-bit addressing

	cmp cl,0x0d								; \r?
	jnz .put_0a
	mov ax,bx
	mov bl,80
	div bl
	mul bl
	mov bx,ax
	jmp .set_cursor

.put_0a:
	cmp cl,0x0a								;\n?
	jnz .put_other
	add bx,80
	jmp .roll_screen

.put_other:									; Display the character
	shl bx,1
	mov [0x800b8000+ebx],cl

	; Below is to move the cursor forward by one character
	shr bx,1
	inc bx

.roll_screen:
	cmp bx,2000								; Roll the screen if the cursor is out of the screen
	jl .set_cursor

	push bx

	cld
	mov esi,0x800b80a0						; 32-bit mode movsb/w/d use ESI/EDI/ECX
	mov edi,0x800b8000
	mov ecx,1920
	rep movsd
	mov bx,3840								; Clear the bottom line of the screen
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
	inc dx									; 0x3d5
	mov al,bh
	out dx,al
	dec dx									; 0x3d4
	mov al,0x0f
	out dx,al
	inc dx									; 0x3d5
	mov al,bl
	out dx,al

	popad
	ret

;-------------------------------------------------------------------------------
read_hard_disk_0:							; Read a logical sector from the hard disk (flat model)
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
	out dx,al								; Number of sectors to read

	inc dx									; 0x1f3
	pop eax
	out dx,al								; LBA address 7~0

	inc dx									; 0x1f4
	mov cl,8
	shr eax,cl
	out dx,al								; LBA address 15~8

	inc dx									; 0x1f5
	shr eax,cl
	out dx,al								; LBA address 23~16

	inc dx									; 0x1f6
	shr eax,cl
	or al,0xe0								; First hard disk LBA address 27~24
	out dx,al

	inc dx									; 0x1f7
	mov al,0x20								; Read command
	out dx,al

.waits:
	in al,dx
	and al,0x88
	cmp al,0x08
	jnz .waits								; The hard disk is not busy and ready for data transfer

	mov ecx,256								; Toal number of words to read
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

	retf

;-------------------------------------------------------------------------------
put_hex_dword:                              ; Display a double word in hexadecimal at the current cursor position and move the cursor
                                            ; In: EDX=Number to be converted and displayed
                                            ; Out: None
	pushad

	mov ebx,bin_hex							; Point to the conversion table in the core address space
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
	retf

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ; Install a new descriptor in the GDT
                                            ; In: EDX:EAX=Descriptor
                                            ; Out: CX=Selector of the descriptor
	push eax
	push ebx
	push edx

	sgdt [pgdt]								; Get the GDT limit and linear address

	movzx ebx,word [pgdt]					; Limit of GDT
	inc bx									; Total number of bytes in GDT, also the next descriptor offset
	add ebx,[pgdt+2]						; Linear address of the next descriptor

	mov [ebx],eax
	mov [ebx+4],edx

	add word [pgdt],8						; Add one descriptor size to the GDT limit

	lgdt [pgdt]								; Make the change to the GDT effective

	mov ax,[pgdt]							; Get the GDT limit
	xor dx,dx
	mov bx,8
	div bx									; Divided by 8, remove the remainder
	mov cx,ax
	shl cx,3								; Move the index to the correct position

	pop edx
	pop ebx
	pop eax

	retf
	
;-------------------------------------------------------------------------------
make_seg_descriptor:						; Construct memory and system segment descriptors
                                            ; In: EAX=Linear base address
                                            ;     EBX=Segmentation limit
                                            ;     ECX=Attribute. All attribute bits are in the original position, and irrelevant bits are cleared
                                            ; Out: EDX:EAX=Descriptor
	mov edx,eax
	shl eax,16
	or ax,bx								; Construction of the first 32 bits of the descriptor (EAX)

	and edx,0xffff0000						; Clear irrelevant bits in the base address
	rol edx,8
	bswap edx								; Set the high 4 bits of the base address (31~24) and the low 4 bits (23~16)

	xor bx,bx
	or edx,ebx								; Set the high 4 bits of the limit

	or edx,ecx								; Set the attribute

	retf

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ; Const Gate Descriptor(For Call Gate)
                                            ; In: EAX=Offset of the gate in the segment
                                            ;     BX=Selector of the segment where the gate code is located
                                            ;     CX=Segment type and attributes (all attribute bits are in the original position)
                                            ; Out: EDX:EAX= Complete descriptor
	push ebx
	push ecx

	mov edx,eax
	and edx,0xffff0000						; Get the high 16 bits of the offset
	or dx,cx								; Set the attribute part to EDX

	and eax,0x0000ffff						; Get the low 16 bits of the offset
	shl ebx,16
	or eax,ebx								; Set the segment selector part to EAX

	pop ecx
	pop ebx

	retf

;-------------------------------------------------------------------------------
allocate_a_4k_page:                         ; Allocate a 4KB page
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
	call flat_4gb_code_seg_sel:put_string
	hlt										; Have no page to allocate, halt

	.b2:
		shl eax,12							; Multiplied by 4096 (0x1000)

		pop edx
		pop ecx
		pop ebx

		ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ; Allocate a page and install it in the current active hierarchical paging structure
                                            ; In: EBX=Linear address of the page
	push eax
	push ebx
	push ecx
	push esi

	; Check if the page table corresponding to the linear address exists
	mov esi,ebx
	and esi,0xffc00000						; Clear the page table index and page offset part
	shr esi,20								; Multiply the page directory index by 4 as the page offset
	or esi,0xfffff000						; The linear address and the page offset of the page table corresponding to the linear address

	test dword [esi],0x00000001				; Check if the page corresponding to the linear address exists
	jnz .b1

	; Allocate a page as a page table
	call allocate_a_4k_page
	or eax,0x00000007
	mov [esi],eax							; Register this page table in the page directory

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
		and esi,0xfffff000                 ; Clear the page offset part
		shr esi,10                         ; Set the page directory index as the page table index, and multiply the page table index by 4 as the page offset
		or esi,0xffc00000                  ; Get the page table entry corresponding to the linear address

		test dword [esi],0x00000001        ; Check if the page corresponding to the linear address exists
		jnz .b2

		; Create and install the page corresponding to the linear address
		call allocate_a_4k_page            ; Allocate a page, this is the page to be installed
		or eax,0x00000007
		mov [esi],eax

	.b2:
		pop esi
		pop ecx
		pop ebx
		pop eax

		retf

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ; Create a new page directory and copy the content of the current page directory
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

	mov esi,0xfffff000						; ESI->Current linear address of the page directory
	mov edi,0xffffe000						; EDI->Linear address of the new page directory
	mov ecx,1024							; ECX=Number of directory items to copy
	cld
	repe movsd

	pop ecx
	pop ebx
	pop edi
	pop esi

	retf

;-------------------------------------------------------------------------------
task_alloc_memory:                          ; Allocate memory in the virtual memory space of the specified task
                                            ; In: EBX=Linear address of the task control block TCB
                                            ;     ECX=Number of bytes hoped to allocate
                                            ; Out: ECX= Start linear address of the allocated memory
	push eax

	push ebx								;to A

	; Get the start linear address of the current memory allocation
	mov ebx,[ebx+0x06]						; Get the start linear address of the current memory allocation
	mov eax,ebx
	add ecx,ebx								; Linear address after the last byte of this allocation

	push ecx								;To B

	; Allocate pages for the requested memory
	and ebx,0xfffff000
	and ecx,0xfffff000
	.next:
		call flat_4gb_code_seg_sel:alloc_inst_a_page
											; Install the page where the current linear address is located
		add ebx,0x1000						; +4096
		cmp ebx,ecx
		jle .next

	; Set the linear address used for the next allocation to be 4-byte aligned
	pop ecx									; B

	test ecx,0x00000003						; Is the linear address 4-byte aligned?
	jz .algn								; Yes, return directly
	add ecx,4								; No, force alignment to 4 bytes
	and ecx,0xfffffffc

	.algn:
		pop ebx								; A

		mov [ebx+0x06],ecx					; Store the linear address available for the next allocation back to the TCB
		mov ecx,eax

		pop eax

		retf

;-------------------------------------------------------------------------------
allocate_memory:							; Allocate memory in the address space of the current task
                                            ; In: ECX=Number of bytes hoped to allocate
                                            ; Out: ECX= Start linear address of the allocated memory
	push eax
	push ebx

	; Get the linear address of the first node of the TCB chain
	mov eax,[tcb_chain]						; EAX=Linear address of the first node

	; Search for the node with the status busy (current task)
	.s0:
		cmp word [eax+0x04],0xffff
		jz .s1								; Found the busy node, EAX=Linear address of the node
		mov eax,[eax]
		jmp .s0

	; Begin to allocate memory
	.s1:
		mov ebx,eax
		call flat_4gb_code_seg_sel:task_alloc_memory

		pop ebx
		pop eax

		retf

;-------------------------------------------------------------------------------
initiate_task_switch:                       ; Ask for task switch
                                            ; In: None
                                            ; Out: None
	pushad

	mov eax,[tcb_chain]
	cmp eax,0
	jz .return

	; Search for the node with the status busy (current task)
	.b0:
		cmp word [eax+0x04],0xffff
		cmove esi,eax						; Found the busy node, ESI=Linear address of the node
		jz .b1
		mov eax,[eax]
		jmp .b0

	; Search for the node with the status ready
	.b1:
		mov ebx,[eax]
		or ebx,ebx
		jz .b2								; Have not found a ready node at the end of the list, start from the beginning
		cmp word [ebx+0x04],0x0000
		cmove edi,ebx						; Found the ready node, EDI=Linear address of the node
		jz .b3
		mov eax,ebx
		jmp .b1

	.b2:
		mov ebx,[tcb_chain]					; EBX=Linear address of the first node
	.b20:
		cmp word [ebx+0x04],0x0000
		cmove edi,ebx						; Found the ready node, EDI=Linear address of the node
		jz .b3
		mov ebx,[ebx]
		or ebx,ebx
		jz .return							; There is no ready task in the list, return
		jmp .b20

	; Found the node with the status ready, prepare to switch to that task
	.b3:
		not word [esi+0x04]					; Switch the busy node to the ready node
		not word [edi+0x04]					; Switch the ready node to the busy node
		jmp far [edi+0x14]					; Switch to the ready task

	.return:
		popad

		retf

;-------------------------------------------------------------------------------
terminate_current_task:                     ; Terminate the current task
											; Note: When this routine is executed, the current task is still running. This routine is actually part of the current task
	
	mov eax,[es:tcb_chain]					; EAX=Linear address of the first node
	; Search for the node with the status busy (current task)
	.s0:
		cmp word [eax+0x04],0xffff
		jz .s1								; Find the busy node, EAX=Linear address of the node
		mov eax,[eax]
		jmp .s0

	; Set the node with the status busy to the terminated status
	.s1:
		mov word [eax+0x04],0x3333

		; Search for the node with the status ready
		mov ebx,[es:tcb_chain]				; EBX=Linear address of the first node
	.s2:
		cmp word [ebx+0x04],0x0000
		jz .s3								; Found the ready node, EBX=Linear address of the node
		mov ebx,[ebx]
		jmp .s2

	; Found the node with the status ready, prepare to switch to that task
	.s3:
		not word [ebx+0x04]					; Set the ready node to the busy node
		jmp far [ebx+0x14]					; Switch to the ready task

;-------------------------------------------------------------------------------
general_interrupt_handler:					; General interrupt handling process
	push eax

	mov al,0x20								; EOI (End of Interrupt)
	out 0xa0,al								; Send to the slave
	out 0x20,al								; Send to the master

	pop eax

	iretd

;-------------------------------------------------------------------------------
general_exception_handler:                  ; General exception handling process
	mov ebx,excep_msg
	call flat_4gb_code_seg_sel:put_string

	cli

	hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ; Handling process of real-time clock interrupt

	pushad

	mov al,0x20								; EOI (End of Interrupt)
	out 0xa0,al								; Send to the slave
	out 0x20,al								; Send to the master

	mov al,0x0c								; Index of register C, and open NMI
	out 0x70,al
	in al,0x71								; Read register C of RTC, otherwise the interrupt occurs only once
											; Do not consider the alarm clock and periodic interrupt
	; Ask for task switch
	call flat_4gb_code_seg_sel:initiate_task_switch

	popad

	iretd

;-------------------------------------------------------------------------------
do_task_clean:                             ; Clear the terminated task and recycle resources

	; Search the TCB chain list and find the node with the status terminated
	; Remove the node from the list
	; Recycle the resources occupied by the task (can be found from its TCB)

	retf

sys_routine_end:

;===============================================================================
SECTION core_data vfollows=sys_routine		; Data segment of the system core
;------------------------------------------------------------------------------- 
	pgdt			dw  0					; For setting and modifying GDT
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

	; Symbol address lookup table
	salt:
	salt_1			db  '@PrintString'
					times 256-($-salt_1) db 0
					dd  put_string
					dw  flat_4gb_code_seg_sel

	salt_2			db  '@ReadDiskData'
					times 256-($-salt_2) db 0
					dd  read_hard_disk_0
					dw  flat_4gb_code_seg_sel

	salt_3			db  '@PrintDwordAsHexString'
					times 256-($-salt_3) db 0
					dd  put_hex_dword
					dw  flat_4gb_code_seg_sel

	salt_4			db  '@TerminateProgram'
					times 256-($-salt_4) db 0
					dd  terminate_current_task
					dw  flat_4gb_code_seg_sel

	salt_5			db  '@InitTaskSwitch'
					times 256-($-salt_5) db 0
					dd  initiate_task_switch
					dw  flat_4gb_code_seg_sel

	salt_6			db  '@MAlloc'
					times 256-($-salt_6) db 0
					dd  allocate_memory
					dw  flat_4gb_code_seg_sel

	salt_item_len   equ $-salt_6
	salt_items      equ ($-salt)/salt_item_len

	message_0		db  '  System core is runing in protect mode,'
					db  'IDT is mounted.',0x0d,0x0a,0

	cpu_brnd0		db  0x0d,0x0a,'  ',0
	cpu_brand		times 52  db 0
	cpu_brnd1		db  0x0d,0x0a,0x0d,0x0a,0

	message_1		db  '  Paging is enabled.System core is mapped to'
					db  ' linear address 0x80000000.',0x0d,0x0a,0

	message_2		db  '  System wide CALL-GATE mounted and test OK.'
					db  0x0d,0x0a,0

	message_3		db  '********No more pages********',0

	excep_msg		db  '********Exception encounted********',0

	bin_hex			db '0123456789ABCDEF'	; Lookup table used by the put_hex_dword subroutine

	core_buf		times 2048 db 0         ; Buffer used by the kernel

	tcb_chain		dd  0

	core_msg1		db  '  Core task created.',0x0d,0x0a,0

	core_msg2		db  '[CORE TASK]: I am working!',0x0d,0x0a,0

core_data_end:
               
;===============================================================================
SECTION core_code vfollows=core_data
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ; Install a new descriptor in the LDT
                                            ; In: EDX:EAX=Descriptor
                                            ;     EBX=Base address of TCB
                                            ; Out: CX=Selector of the descriptor
	push eax
	push edx
	push edi

	mov edi,[ebx+0x0c]						; Get the base address of LDT
	
	xor ecx,ecx
	mov cx,[ebx+0x0a]						; Get the LDT limit
	inc cx									; Total number of bytes in LDT, that is, the offset address of the new descriptor
	
	mov [edi+ecx+0x00],eax
	mov [edi+ecx+0x04],edx					; Install descriptor

	add cx,8                           
	dec cx									;  Get the new LDT limit

	mov [ebx+0x0a],cx						; Update the LDT limit to TCB

	mov ax,cx
	xor dx,dx
	mov cx,8
	div cx
	
	mov cx,ax
	shl cx,3								; Left shift 3 bits, and make TI=1, point to LDT, and finally make RPL=00
	or cx,0000_0000_0000_0100B

	pop edi
	pop edx
	pop eax

	ret
         
;------------------------------------------------------------------------------- 
load_relocate_program:                      ; Load and relocate user program
                                            ; In: PUSH logical sector number
                                            ;     PUSH TCB base address
                                            ; Out: None
	pushad

	mov ebp,esp								; Prepare to access parameters passed through the stack

	; Clear the first half of the current page directory (corresponding to the local address space of the lower 2GB)
	mov ebx,0xfffff000
	xor esi,esi
	.clsp:
		mov dword [ebx+esi*4],0x00000000
		inc esi
		cmp esi,512
		jl .clsp

	mov ebx,cr3								; Refresh TLB
	mov cr3,ebx

	; Load the user program header data
	mov eax,[ebp+10*4]						; Get the starting sector number of the user program from the stack
	mov ebx,core_buf						; Read the program header data
	call flat_4gb_code_seg_sel:read_hard_disk_0

	; Determine the size of the entire program
	mov eax,[core_buf]						; Size of the program
	mov ebx,eax
	and ebx,0xfffffe00						; Make it 512-byte aligned (the low 9 bits of a number that can be divided by 512 are all 0)
	add ebx,512
	test eax,0x000001ff						; Is the size of the program a multiple of 512?
	cmovnz eax,ebx							; No, use the rounded result

	mov esi,[ebp+9*4]						; Get the base address of the TCB from the stack

	mov ecx,eax								; The actual amount of memory required
	mov ebx,esi
	call flat_4gb_code_seg_sel:task_alloc_memory

	mov ebx,ecx								; EBX -> Start linear address of the allocated memory
	xor edx,edx
	mov ecx,512
	div ecx
	mov ecx,eax								; Total number of sectors

	mov eax,[ebp+10*4]						; Start sector number
	.b1:
		call flat_4gb_code_seg_sel:read_hard_disk_0
		inc eax
		loop .b1							; Loops to read until the entire user program is read

	; Create TSS for user task
	mov ecx,104								; The basic size of TSS
	mov [esi+0x12],cx
	dec word [esi+0x12]						; Record the TSS limit value to TCB
	call flat_4gb_code_seg_sel:allocate_memory
	mov [esi+0x14],ecx						; Record the linear base address of TSS to TCB

	; Below is to allocate memory needed to create LDT
	mov ebx,esi
	mov ecx,160								; Allow to install 20 LDT descriptors
	call flat_4gb_code_seg_sel:task_alloc_memory
	mov [esi+0x0c],ecx						; Register the linear base address of LDT to TCB
	mov word [esi+0x0a],0xffff				; Record the initial limit of LDT to TCB


	; Create a user task code segment descriptor
	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c0f800						; 4KB-granularity code segment descriptor, privilege level 3
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0011B				; Set the privilege level of the selector to 3

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+76],cx							; Select the code segment selector of the user task

	; Set up a user task data segment descriptor
	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c0f200						;  4KB-granularity data segment descriptor, privilege level 3 
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0011B				; Set the privilege level of the selector to 3

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+84],cx							; Write the DS field of TSS
	mov [ebx+72],cx							; Write the ES field of TSS
	mov [ebx+88],cx							; Write the FS field of TSS
	mov [ebx+92],cx							; Write the GS field of TSS

	; Allocate space for the user task stack segment
	mov ebx,esi
	mov ecx,4096							; 4KB space
	call flat_4gb_code_seg_sel:task_alloc_memory

	; Set up a user task stack segment descriptor
	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c0f200						; 4KB-granularity data segment descriptor, privilege level 3
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0011B				; Set the privilege level of the selector to 3

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+80],cx							; Write the SS field of TSS
	mov edx,[esi+0x06]						; Set the high linear address of the stack to the next available address
	mov [ebx+56],edx						; Write the ESP field of TSS
 
	; Relocate SALT
	cld

	mov ecx,[0x0c]							; Number of U-SALT entries (obtained by accessing the 4GB segment)
	mov edi,[0x08]							; Offset of U-SALT in the 4GB segment
	.b2: 
		push ecx
		push edi
	
		mov ecx,salt_items
		mov esi,salt
		.b3:
			push edi
			push esi
			push ecx

			mov ecx,64						; Compare each entry in the table
			repe cmpsd						; Compare 4 bytes each time
			jnz .b4
			mov eax,[esi]					; If matched, ESI points to the address just after it
			mov [es:edi-256],eax			; Rewrite the string as an offset address
			mov ax,[esi+4]
			or ax,0000000000000011B			; User program uses call gate with its own privilege level
											; RPL=3 
			mov [es:edi-252],ax				; Rewrite the call gate selector

			.b4:
				pop ecx
				pop esi
				add esi,salt_item_len
				pop edi						; Compare from the beginning
				loop .b3
	
				pop edi
				add edi,256
				pop ecx
				loop .b2

	mov esi,[ebp+9*4]						; Get the base address of TCB from the stack

	; Create a 0 privilege level stack in the local address space of the user task
	mov ebx,esi
	mov ecx,4096							; 4KB space 
	call flat_4gb_code_seg_sel:task_alloc_memory

	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c09200						; 4KB-granularity stack segment descriptor, privilege level 0
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0000B				; Set the privilege level of the selector to 0

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+8],cx							; Write the SS0 field of TSS
	mov edx,[esi+0x06]						; The high linear address of the stack
	mov [ebx+4],edx							; Write the ESP0 field of TSS

	; Create a 1 privilege level stack in the local address space of the user task
	mov ebx,esi
	mov ecx,4096							; 4KB space
	call flat_4gb_code_seg_sel:task_alloc_memory

	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c0b200						; 4KB-granularity, read-write, privilege level 1 
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0001				; Set the privilege level of the selector to 1

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+16],cx							; Write the SS1 field of TSS
	mov edx,[esi+0x06]						; The high linear address of the stack
	mov [ebx+12],edx						; Write the ESP1 field of TSS

	; Create a 2 privilege level stack in the local address space of the user task
	mov ebx,esi
	mov ecx,4096							; 4KB space
	call flat_4gb_code_seg_sel:task_alloc_memory

	mov eax,0x00000000
	mov ebx,0x000fffff
	mov ecx,0x00c0d200						; 4KB-granularity, read-write, privilege level 2
	call flat_4gb_code_seg_sel:make_seg_descriptor
	mov ebx,esi								; Base address of TCB
	call fill_descriptor_in_ldt
	or cx,0000_0000_0000_0010				; Set the privilege level of the selector to 2

	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+24],cx							; Write the SS2 field of TSS
	mov edx,[esi+0x06]						; The high linear address of the stack
	mov [ebx+20],edx						; Write the ESP2 field of TSS

	; Register the LDT descriptor in the GDT
	mov eax,[esi+0x0c]						; The starting linear address of LDT
	movzx ebx,word [esi+0x0a]				; The LDT segment limit
	mov ecx,0x00008200						; LDT descriptor, privilege level 0
	call flat_4gb_code_seg_sel:make_seg_descriptor
	call flat_4gb_code_seg_sel:set_up_gdt_descriptor
	mov [esi+0x10],cx						; Register the LDT selector to TCB

	; Register basic TSS table content
	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov [ebx+96],cx							; Write the LDT field of TSS

	mov word [ebx+0],0						; Reverse chain=0

	mov dx,[esi+0x12]						; Segment length (limit)
	mov [ebx+102],dx						; Write the I/O bitmap offset field of TSS

	mov word [ebx+100],0					;T=0

	mov eax,[0x04]							; Get the entry point from the 4GB address space of the task
	mov [ebx+32],eax						; Write the EIP field of TSS 

	pushfd
	pop dword [ecx+36]						;EFLAGS

	; Register the TSS descriptor in the GDT
	mov eax,[esi+0x14]						; The starting linear address of TSS
	movzx ebx,word [esi+0x12]				; The segment length (limit)
	mov ecx,0x00008900						; TSS descriptor, privilege level 0
	call flat_4gb_code_seg_sel:make_seg_descriptor
	call flat_4gb_code_seg_sel:set_up_gdt_descriptor
	mov [esi+0x18],cx						; Register the TSS selector to TCB

	; Create a page directory for the user task
	; The allocation and use of pages are determined by the page bitmap, and the linear address space can be unoccupied
	call flat_4gb_code_seg_sel:create_copy_cur_pdir
	mov ebx,[esi+0x14]						; Get the linear address of TSS from TCB
	mov dword [ebx+28],eax					; Write the CR3 (PDBR) field of TSS

	popad

	ret 8									; Discard the parameters pushed before calling this routine
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ; Append the task control block to the TCB chain
                                            ; Input: ECX=TCB linear base address
	push eax
	push edx

	cli

	mov dword [ecx+0x00],0					; The current TCB pointer field is cleared to indicate that this is the last TCB
										
	mov eax,[tcb_chain]						; The TCB header pointer
	or eax,eax								; Is the list empty?
	jz .notcb 
         
	.searc:
		mov edx,eax
		mov eax,[edx+0x00]
		or eax,eax               
		jnz .searc

		mov [es: edx+0x00],ecx
		jmp .retpc
         
	.notcb:
		mov [tcb_chain],ecx					; If it is an empty table, directly point the table header pointer to TCB
         
	.retpc:
		sti

		pop edx
		pop eax
		
		ret
         
;-------------------------------------------------------------------------------
start:
	; Create an interrupt descriptor table IDT
	; Note! During this period, interrupts must not be opened, and the put_string routine must not be called!

	; The first 20 vectors are used by processor exceptions
	mov eax,general_exception_handler		; Offset address of the gate code in the segment
	mov bx,flat_4gb_code_seg_sel			; Selector of the gate code
	mov cx,0x8e00							; 32-bit interrupt gate, 0 privilege level
	call flat_4gb_code_seg_sel:make_gate_descriptor

	mov ebx,idt_linear_address				; Linear address of the interrupt descriptor table
	xor esi,esi
	.idt0:
		mov [ebx+esi*8],eax
		mov [ebx+esi*8+4],edx
		inc esi
		cmp esi,19							; Install the first 20 exception interrupt handling processes
		jle .idt0

	; The rest are interrupt vectors reserved or used by hardware
	mov eax,general_interrupt_handler		; Offset address of the gate code in the segment
	mov bx,flat_4gb_code_seg_sel			; Selector of the gate code
	mov cx,0x8e00							; 32-bit interrupt gate, 0 privilege level
	call flat_4gb_code_seg_sel:make_gate_descriptor

	mov ebx,idt_linear_address				; Linear address of the interrupt descriptor table
	.idt1:
		mov [ebx+esi*8],eax
		mov [ebx+esi*8+4],edx
		inc esi
		cmp esi,255							; Install ordinary interrupt handling processes
		jle .idt1

	; Set up real-time clock interrupt handling process
	mov eax,rtm_0x70_interrupt_handle		; Offset address of the gate code in the segment
	mov bx,flat_4gb_code_seg_sel			; Selector of the gate code
	mov cx,0x8e00							; 32-bit interrupt gate, 0 privilege level
	call flat_4gb_code_seg_sel:make_gate_descriptor

	mov ebx,idt_linear_address				; Linear address of the interrupt descriptor table
	mov [ebx+0x70*8],eax
	mov [ebx+0x70*8+4],edx

	; Prepare to open the interrupt
	mov word [pidt],256*8-1					; Limit of IDT
	mov dword [pidt+2],idt_linear_address
	lidt [pidt]								; Load the interrupt descriptor table register IDTR

	; Set up 8259A interrupt controller
	mov al,0x11
	out 0x20,al								;ICW1: Edge-triggered/cascade mode
	mov al,0x20
	out 0x21,al								;ICW2: Starting interrupt vector
	mov al,0x04
	out 0x21,al								;ICW3: Cascade to IR2
	mov al,0x01
	out 0x21,al								;ICW4: Non-bus buffer, full nesting, normal EOI

	mov al,0x11
	out 0xa0,al								;ICW1: Edge-triggered/cascade mode
	mov al,0x70
	out 0xa1,al								;ICW2: Starting interrupt vector
	; mov al,0x04
	mov al,0x02
	out 0xa1,al								;ICW3: Cascade to IR2
	mov al,0x01
	out 0xa1,al								;ICW4: Non-bus buffer, full nesting, normal EOI

	; Set up hardware related to clock interrupt
	mov al,0x0b								; RTC register B
	or al,0x80								; Block NMI
	out 0x70,al
	mov al,0x12								; Set register B, disable periodic interrupt, open update end interrupt, BCD code, 24-hour system
	out 0x71,al

	in al,0xa1								; Read the IMR register of the 8259 slave
	and al,0xfe								; Clear bit 0 (this bit is connected to RTC)
	out 0xa1,al								; Write back to this register

	mov al,0x0c
	out 0x70,al
	in al,0x71								; Read RTC register C, reset the pending interrupt status

	sti										; Open hardware interrupt

	mov ebx,message_0
	call flat_4gb_code_seg_sel:put_string

	; Display processor brand information
	mov eax,0x80000002
	cpuid
	mov [cpu_brand + 0x00],eax
	mov [cpu_brand + 0x04],ebx
	mov [cpu_brand + 0x08],ecx
	mov [cpu_brand + 0x0c],edx

	mov eax,0x80000003
	cpuid
	mov [cpu_brand + 0x10],eax
	mov [cpu_brand + 0x14],ebx
	mov [cpu_brand + 0x18],ecx
	mov [cpu_brand + 0x1c],edx

	mov eax,0x80000004
	cpuid
	mov [cpu_brand + 0x20],eax
	mov [cpu_brand + 0x24],ebx
	mov [cpu_brand + 0x28],ecx
	mov [cpu_brand + 0x2c],edx

	mov ebx,cpu_brnd0						; Display processor brand information
	call flat_4gb_code_seg_sel:put_string
	mov ebx,cpu_brand
	call flat_4gb_code_seg_sel:put_string
	mov ebx,cpu_brnd1
	call flat_4gb_code_seg_sel:put_string

	; The following starts to install the call gate for the entire system service. Control transfer between privilege levels must use gates
	mov edi,salt							; Starting position of C-SALT table
	mov ecx,salt_items						; Number of entries in C-SALT table
	.g0:
		push ecx   
		mov eax,[edi+256]					; 32-bit offset address of the entry point of the entry
		mov bx,[edi+260]					; Segment selector of the entry point of the entry
		mov cx,1_11_0_1100_000_00000B		; Call gate with privilege level 3 (privilege levels above 3 are allowed to access), 0 parameters (because parameters are passed by registers, not by stack)
		
		call flat_4gb_code_seg_sel:make_gate_descriptor
		call flat_4gb_code_seg_sel:set_up_gdt_descriptor
		mov [edi+260],cx					; Fill back the returned gate descriptor selector
		add edi,salt_item_len				; Point to the next C-SALT entry
		pop ecx
		loop .g0

	; Test the gate
	mov ebx,message_2
	call far [salt_1+256]					; Display information through the gate (the offset will be ignored)

	; Create and establish kernel tasks
	mov ecx,core_lin_tcb_addr				; Linear address of the kernel task TCB after moving to the high end
	mov word [ecx+0x04],0xffff				; Task status is "busy"
	mov dword [ecx+0x06],core_lin_alloc_at
											; Register the starting linear address available for allocation in the kernel
	call append_to_tcb_link					; Add the TCB of the kernel task to the TCB chain

	mov esi,ecx

	; All TSS must be created in kernel space
	mov ecx,104								; Allocate memory for the TSS of the task
	call flat_4gb_code_seg_sel:allocate_memory
	mov [esi+0x14],ecx						; Save the TSS base address in the kernel TCB

	; Set the necessary items in the TSS of the program manager
	mov word [ecx+0],0						; Reverse chain=0
	mov eax,cr3
	mov dword [ecx+28],eax					; Register CR3 (PDBR)
	mov word [ecx+96],0						; No LDT. The processor allows tasks without LDT.
	mov word [ecx+100],0					; T=0
	mov word [ecx+102],103					; No I/O bitmap. In fact, 0 privilege level does not need 0, 1, 2 privilege level stack. 0 special level will not transfer control to low privilege level.
         
	; Create TSS descriptor and install it in GDT
	mov eax,ecx								; The starting linear address of TSS
	mov ebx,103								; Segment length (limit)
	mov ecx,0x00008900						; TSS descriptor, privilege level 0
	call flat_4gb_code_seg_sel:make_seg_descriptor
	call flat_4gb_code_seg_sel:set_up_gdt_descriptor
	mov word [esi+0x18],cx					; Register TSS selector to TCB

	; The content of the task register TR is the flag of the task's existence, which also determines who the current task is.
	; The following instructions are the procedures for the current 0 privilege level task "Program Manager" (TSS).
	ltr cx

	; Now it can be considered that the "Program Manager" task is in progress
	mov ebx,core_msg1
	call flat_4gb_code_seg_sel:put_string

	; Create user tasks
	mov ecx,0x1a
	call flat_4gb_code_seg_sel:allocate_memory
	mov word [ecx+0x04],0					; Task status: ready
	mov dword [ecx+0x06],0					; Initial linear address available for allocation in the task

	push dword 50							; User program is located in logical sector 50
	push ecx								; Push the starting linear address of the task control block
	call load_relocate_program
	call append_to_tcb_link					; Add this TCB to the TCB chain

	; More tasks can be created, for example:
	mov ecx,0x1a
	call flat_4gb_code_seg_sel:allocate_memory
	mov word [ecx+0x04],0					; Task status: idle
	mov dword [ecx+0x06],0					; Initial linear address available for allocation in the task

	push dword 100							; User program is located in logical sector 100
	push ecx								; Push the starting linear address of the task control block

	call load_relocate_program
	call append_to_tcb_link					; Add this TCB to the TCB chain

	.do_switch:
		mov ebx,core_msg2
		call flat_4gb_code_seg_sel:put_string

		; Clean up terminated tasks and recycle the resources they occupy
		call flat_4gb_code_seg_sel:do_task_clean

		hlt

		jmp .do_switch

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
