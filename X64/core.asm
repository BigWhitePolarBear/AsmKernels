%include ".\utils\global_defs.wid"

;===============================================================================
section core_header									; Kernel Header
	length		dd core_end							; #0: Total length of kernel
	init_entry	dd init								; #4: Kernel entry point
	position	dq 0								; #8: Virtual address of kernel

;===============================================================================
section core_data									; Kernel data segment
	acpi_error	db "ACPI is not supported or data error.", 0x0d, 0x0a, 0

	num_cpus	db 0								; Number of logical processors
	cpu_list	times 256 db 0						; List of Local APIC IDs
	lapic_addr	dd 0								; Physical address of Local APIC

	ioapic_addr	dd 0								; Physical address of I/O APIC
	ioapic_id	db 0								; I/O APIC ID

	ack_cpus	db 0								; Processor initialization response count

	clocks_1ms	dd 0								; Clocks in 1ms

	welcome		db "Executing in 64-bit mode.Init MP", 249, 0
	cpu_init_ok	db " CPU(s) ready.", 0x0d, 0x0a, 0

	buffer		times 256 db 0

	sys_entries	dq get_screen_row					; #0
				dq get_cmos_time					; #1
				dq put_cstringxy64					; #2
				dq create_process					; #3
				dq get_current_pid					; #4
				dq terminate_process				; #5
				dq get_cpu_number					; #6
				dq create_thread					; #7
				dq get_current_tid					; #8
				dq thread_exit						; #9
				dq memory_allocate					; #10
	pcb_ptr		dq 0								; Process control block PCB first node linear address

;===============================================================================
section core_code									; Kernel code segment
	bits 64

%include ".\utils\core_utils64_mp.wid"
%include ".\utils\user_static64.wid"
	bits 64

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
_ap_string	db 249, 0

ap_to_core_entry:									; Entry point for application processor (AP)
	; GDTR Enable the high linear address of GDT and load IDTR GDTR
	mov rax, UPPER_SDA_LINEAR
	lgdt [rax + 2]									; Load 64-bit linear address part only in 64-bit mode
	lidt [rax + 0x0c]

	; Create a dedicated stack in 64-bit mode for the current processor
	mov rcx, 4096
	call core_memory_allocate
	mov rsp, r14

	; Create a dedicated storage area (including TSS) for the current processor and install the TSS descriptor in GDT
	mov rcx, 256									; Length of dedicated data area, including TSS.
	call core_memory_allocate
	lea rax, [r13 + 128]							; TSS starts at offset 128 in the dedicated data area
	call make_tss_descriptor

	mov r15, UPPER_SDA_LINEAR						; High linear address of the system data area (low end also available)

	mov r8, [r15 + 4]								; R8=GDT linear address
	movzx rcx, word [r15 + 2]						; RCX=GDT limit value
	mov [r8 + rcx + 1], rsi							; Low 64 bits of TSS descriptor
	mov [r8 + rcx + 9], rdi							; High 64 bits of TSS descriptor

	add word [r15 + 2], 16
	lgdt [r15 + 2]									; Re-load GDTR

	shr cx, 3										; Divide by 8 (eliminate the remainder) to get the index number
	inc cx											; Increment index number
	shl cx, 3										; Move the index number to the correct position

	ltr cx											; Load the task register TR for the current processor

	; Save the base address of the processor's dedicated data area to the processor's model-specific register IA32_KERNEL_GS_BASE
	mov ecx, 0xc000_0102							; IA32_KERNEL_GS_BASE
	mov rax, r13									; Save only EAX
	mov rdx, r13
	shr rdx, 32										; Save only EDX
	wrmsr

	; Prepare parameters for fast system calls SYSCALL and SYSRET
	mov ecx, 0x0c0000080							; Specify model-specific register IA32_EFER
	rdmsr
	bts eax, 0										; Set the SCE bit to allow the SYSCALL instruction
	wrmsr

	mov ecx, 0xc0000081								; STAR
	mov edx, (RESVD_DESC_SEL << 16) | CORE_CODE64_SEL
	xor eax, eax
	wrmsr

	mov ecx, 0xc0000082								; LSTAR
	mov rax, [rel position]
	lea rax, [rax + syscall_procedure]				; Save only the EAX part
	mov rdx, rax
	shr rdx, 32										; Save only the EDX part
	wrmsr

	mov ecx, 0xc0000084								; FMASK 
	xor edx, edx
	mov eax, 0x00047700								; Save TF=IF=DF=AC=0; IOPL=00
	wrmsr

	mov r15, [rel position]
	lea rbx, [r15 + _ap_string]
	call put_string64								; Put in core_utils64_mp.wid

	swapgs											; Prepare to operate on the dedicated data of the current processor with GS
	mov qword [gs:8], 0								; No task is being executed
	xor rax, rax
	mov al, byte [rel ack_cpus]
	mov [gs:16], rax								; Set the number of the current processor
	mov [gs:24], rsp								; Save the stack pointer of the current processor
	swapgs

	inc byte [rel ack_cpus]							; Increment response count value

	mov byte [AP_START_UP_ADDR + lock_var], 0		; Release spin lock

	mov rsi, LAPIC_START_ADDR						; Physical address of Local APIC
	bts dword [rsi + 0xf0], 8						; Send the SVR register to allow LAPIC

	sti												; Enable interrupt

	.do_idle:
		hlt
		jmp .do_idle

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
general_interrupt_handler:							; General interrupt handling process
	iretq

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
general_exception_handler:							; General exception handling process
	; Display error information in red background and white text at row 24, column 0
	mov r15, [rel position]
	lea rbx, [r15 + exceptm]
	mov dh, 24
	mov dl, 0
	mov r9b, 0x4f
	call put_cstringxy64							; Located in core_utils64_mp.wid

	cli
	hlt												; Halt and do not accept external hardware interrupts

	exceptm	db "A exception raised,halt.", 0		; Error message when an exception occurs

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
search_for_a_ready_thread:							; Ready to find a thread and set it to busy
													; Out: R11=PCB linear address of the ready thread
													; R12=TCB linear address of the ready thread
	; This routine is usually called in the interrupt processing process, and the interrupt is closed by default.
	push rax
	push rbx
	push rcx

	mov rcx, 1										; Busy state of thread

	swapgs
	mov rbx, [gs:8]									; Get the PCB linear address of the current task
	mov r12, [gs:32]								; Get the TCB linear address of the current thread
	swapgs
	mov r11, rbx
	cmp r11, 0										; Processor is not currently executing tasks?
	jne .nextt
	mov rbx, [rel pcb_ptr]							; Yes. Search from the first TCB of the PCB list first node.
	mov r11, rbx
	mov r12, [r11 + 272]							; First TCB node of the PCB list first node
	.nextt:											; This part traverses the TCB list of the specified task
		cmp r12, 0									; At the end of the TCB list of the current PCB?
		je .nextp									; Go to switch to the next node of the PCB list.
		xor rax, rax
		lock cmpxchg [r12 + 16], rcx
		jz .retrn
		mov r12, [r12 + 280]						; Get the next TCB node
		jmp .nextt
	.nextp:											; This part controls the traversal of the task list
		mov r11, [r11 + 280]						; Get the next PCB node
		cmp r11, rbx								; Is it back to the initial PCB node?
		je .fmiss									; Yes. That is, the ready thread (node) was not found.
		mov r12, [r11 + 272]						; Not. Extract the first node of the TCB list from the new PCB
		jmp .nextt
	.fmiss:											; No ready thread in the system
		xor r11, r11
		xor r12, r12
	.retrn:
		pop rcx
		pop rbx
		pop rax

		ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resume_execute_a_thread:							; Resuming execution of a thread
													; In: R11=PCB linear address of the thread
													; R12=TCB linear address of the thread
	; This routine is called in the interrupt processing process, and the interrupt is closed by default.
	mov eax, [rel clocks_1ms]						; Calculate the new thread running time
	mov ebx, [r12 + 240]							; For the time slice specified for the thread
	mul ebx

	mov rsi, LAPIC_START_ADDR						; Physical address of Local APIC
	mov dword [rsi + 0x3e0], 0x0b					; 1 division
	mov dword [rsi + 0x320], 0xfd					; Single-shot mode, Fixed, interrupt number 0xfd

	mov rbx, [r11 + 56]
	mov cr3, rbx									; Switch address space

	swapgs
	mov [gs:8], r11									; Set the task to which the new thread belongs as the current task 
	mov [gs:32], r12								; Set the new thread as the current thread
	mov rbx, [r12 + 32]								; Get RSP0 in TCB
	mov [gs:128 + 4], rbx							; Set TSS's RSP0
	swapgs

	mov rcx, [r12 + 80]
	mov rdx, [r12 + 88]
	mov rdi, [r12 + 104]
	mov rbp, [r12 + 112]
	mov rsp, [r12 + 120]
	mov r8, [r12 + 128]
	mov r9, [r12 + 136]
	mov r10, [r12 + 144]

	mov r13, [r12 + 168]
	mov r14, [r12 + 176]
	mov r15, [r12 + 184]
	push qword [r12 + 208]							; SS
	push qword [r12 + 120]							; RSP
	push qword [r12 + 232]							; RFLAGS
	push qword [r12 + 200]							; CS
	push qword [r12 + 192]							; RIP

	mov dword [rsi + 0x380], eax					; Start timing

	mov rax, [r12 + 64]
	mov rbx, [r12 + 72]
	mov rsi, [r12 + 96]
	mov r11, [r12 + 152]
	mov r12, [r12 + 160]

	iretq											; Enter new thread execution

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
time_slice_out_handler:								; Time slice expiration interrupt processing process
	push rax
	push rbx
	push r11
	push r12
	push r13

	mov r11, LAPIC_START_ADDR						; Send the interrupt end command EOI to the Local APIC
	mov dword [r11 + 0xb0], 0

	call search_for_a_ready_thread
	or r11, r11
	jz .return										; No ready thread found

	swapgs
	mov rax, qword [gs:8]							; PCB linear address of the current task
	mov rbx, qword [gs:32]							; TCB linear address of the current thread
	swapgs

	; Save the state of the current task and thread for future recovery.
	mov r13, cr3									; Save the original task's paging system
	mov qword [rax + 56], r13
	; RAX and RBX do not need to be saved, and will be popped from the stack when restored
	mov [rbx + 80], rcx
	mov [rbx + 88], rdx
	mov [rbx + 96], rsi
	mov [rbx + 104], rdi
	mov [rbx + 112], rbp
	mov [rbx + 120], rsp
	mov [rbx + 128], r8
	mov [rbx + 136], r9
	mov [rbx + 144], r10
	; R11, R12, and R13 do not need to be set, and will be popped from the stack when restored
	mov [rbx + 176], r14
	mov [rbx + 184], r15
	mov r13, [rel position]
	lea r13, [r13 + .return]						; When restored in the future, it will return from the interrupt
	mov [rbx + 192], r13							; RIP field is the interrupt return point
	mov [rbx + 200], cs
	mov [rbx + 208], ss
	pushfq
	pop qword [rbx + 232]

	mov qword [rbx + 16], 0							; Set the thread state to ready

	jmp resume_execute_a_thread						; Resuming and executing new threads

	.return:
		pop r13
		pop r12
		pop r11
		pop rbx
		pop rax
		iretq

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; After a new task/thread is created, broadcast a new task/thread creation message to all processors, and all processors execute this interrupt service routine.
new_task_notify_handler:							; Task/thread claim interrupt processing process
	push rsi
	push r11
	push r12

	mov rsi, LAPIC_START_ADDR						; Physical address of Local APIC
	mov dword [rsi + 0xb0], 0						; Send EOI

	swapgs
	cmp qword [gs:8], 0								; Is the current processor not executing a task?
	swapgs
	jne .return										; Yes (busy). Don't bother :)

	call search_for_a_ready_thread
	or r11, r11
	jz .return										; No ready task found

	swapgs
	add rsp, 24										; Remove the three parameters pushed when entering the routine
	mov qword [gs:24], rsp							; Save the current pointer of the inherent stack for future return
	swapgs

	jmp resume_execute_a_thread						; Resuming and executing new threads

	.return:
		pop r12
		pop r11
		pop rsi

		iretq

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
_append_lock	dq 0

append_to_pcb_link:									; Append task control block to PCB chain
													; In: R11=PCB linear base address
	push rax
	push rbx

	pushfq											; -->A
	cli
	SET_SPIN_LOCK rax, qword [rel _append_lock]

	mov rbx, [rel pcb_ptr]							; Get the linear address of the first node of the list
	or rbx, rbx
	jnz .not_empty									; Not empty list, turn .not_empty
	mov [r11], r11									; The only node: the predecessor is itself
	mov [r11 + 280], r11							; The successor is also itself
	mov [rel pcb_ptr], r11							; This is the head node
	jmp .return

	.not_empty:
		mov rax, [rbx]								; Get the linear address of the predecessor node of the head node
		; Here, RBX=head node; RAX=predecessor node of head node; R11=appended node
		mov [rax + 280], r11						; The successor of the predecessor node is the appended node
		mov [r11 + 280], rbx						; The successor of the appended node is the head node
		mov [r11], rax								; The predecessor of the appended node is the predecessor of the head node
		mov [rbx], r11								; The predecessor of the head node is the appended node

	.return:
		mov qword [rel _append_lock], 0				; Release lock
		popfq										; A

		pop rbx
		pop rax

		ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_current_tid:									; Return the identifier of the current thread
	pushfq
	cli
	swapgs
	mov rax, [gs:32]
	mov rax, [rax + 8]
	swapgs
	popfq

	ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_current_pid:									; Return the identifier of the current task (process)
	pushfq
	cli
	swapgs
	mov rax, [gs:8]
	mov rax, [rax + 8]
	swapgs
	popfq

	ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
thread_exit:										; Thread termination exit
													; In: RDX=return code
	cli

	swapgs
	mov rbx, [gs:32]								; Get the TCB linear address of the current thread
	mov rsp, [gs:24]								; Switch to the processor's inherent stack
	swapgs

	mov qword [rbx + 16], 2							; Thread status: termination
	mov [rbx + 24], rdx								; Return code

	call search_for_a_ready_thread
	or r11, r11
	jz .sleep										; No ready thread found

	jmp resume_execute_a_thread						; Resuming and executing new threads

.sleep:
	iretq											; Back to the days of not executing threads:)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terminate_process:									; Terminate the current task

	mov rsi, LAPIC_START_ADDR						; Physical address of Local APIC
	mov dword [rsi + 0x320], 0x00010000				; Mask timer interrupt

	cli

	swapgs
	mov rax, [gs:8]									; Get to the PCB node of the current task
	mov qword [rax + 16], 2							; Task status=termination
	mov rax, [gs:32]								; Get to the TCB node of the current thread
	mov qword [rax + 16], 2							; Thread status=termination
	mov qword [gs:0], 0
	mov rsp, [gs:24]								; Switch to the processor's inherent stack
	swapgs

	call search_for_a_ready_thread
	or r11, r11
	jz .sleep										; No ready task found

	jmp resume_execute_a_thread						; Resuming and executing new tasks

	.sleep:
		iretq										; Back to the days of not executing tasks:)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_thread:										; Create a thread
													; In: RSI=linear address of thread entry
													; RDI=parameters passed to the thread
													; Out: RDX=thread identifier
	push rax
	push rbx
	push rcx
	push r11
	push r12
	push r13
	push r14

	; Create a thread control block TCB first
	mov rcx, 512									; Size of thread control block TCB
	call core_memory_allocate						; Must be opened in the space of the kernel

	mov rbx, r13									; R13 is used to save the TCB linear address

	call generate_thread_id
	mov [rbx + 8], rax								; Record the identifier of the current thread
	mov rdx, rax									; Used to return thread identifier

	mov qword [rbx + 16], 0							; Thread status=ready

	mov rcx, 4096 * 16								; Allocate stack space for TSS's RSP0
	call core_memory_allocate						; Must be opened in the space of the kernel
	mov [rbx + 32], r14								; Fill in the value of the RSP0 field in TCB

	pushfq
	cli
	swapgs
	mov r11, [gs:8]									; Get the PCB linear address of the current task
	mov r12, [gs:32]								; Get the TCB linear address of the current thread
	swapgs
	popfq

	mov rcx, 4096 * 16								; Allocate stack space for threads
	call user_memory_allocate
	sub r14, 32										; Open 32 bytes of space on the stack
	mov [rbx + 120], r14							; Fill in the RSP when the thread is executed.

	lea rcx, [r14 + 8]								; Get the thread return address
	mov [r14], rcx
	; Fill in the machine code of the instruction MOV RAX, 9
	mov byte [rcx], 0xb8
	mov byte [rcx + 1], 0x09
	mov byte [rcx + 2], 0x00
	mov byte [rcx + 3], 0x00
	mov byte [rcx + 4], 0x00
	; Fill in the machine code of the instruction XOR RDX, RDX
	mov byte [rcx + 5], 0x48
	mov byte [rcx + 6], 0x31
	mov byte [rcx + 7], 0xd2
	; Fill in the machine code of the instruction SYSCALL
	mov byte [rcx + 8], 0x0f
	mov byte [rcx + 9], 0x05

	mov qword [rbx + 192], rsi						; Thread entry point (RIP)

	mov qword [rbx + 200], USER_CODE64_SEL			; Thread code segment selector
	mov qword [rbx + 208], USER_STACK64_SEL			; Thread stack segment selector

	pushfq
	pop qword [rbx + 232]							; Thread flag register when executing

	mov qword [rbx + 240], SUGG_PREEM_SLICE			; Recommended thread execution time slice, from global_defs.wid

	mov qword [rbx + 280], 0						; Next TCB linear address, 0=no

	.again:
		xor rax, rax
		lock cmpxchg [r12 + 280], rbx				; If the successor of the node is 0, the new node is its successor
		jz .linkd
		mov r12, [r12 + 280]
		jmp .again
	.linkd:
		mov rcx, LAPIC_START_ADDR					; Physical address of Local APIC
		mov dword [rcx + 0x310], 0
		mov dword [rcx + 0x300], 0x000840fe			; Send thread claim interrupt to all processors

	pop r14
	pop r13
	pop r12
	pop r11
	pop rcx
	pop rbx
	pop rax

	ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_process:										; Create a new task and its main thread
													; In: R8=starting logical sector number of the program
	push rax
	push rbx
	push rcx
	push rdx
	push rsi
	push rdi
	push rbp
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15

	; Create a task control block PCB at the high end of the address space (kernel)
	mov rcx, 512									; Size of task control block PCB
	call core_memory_allocate						; Allocate memory at the high end (kernel) of the virtual address space
	mov r11, r13									; R11 is used to save the PCB linear address

	call core_memory_allocate						; Allocate memory for thread control block TCB
	mov r12, r13									; R12 is used to save the TCB linear address

	mov qword [r11 + 272], r12						; Register the first TCB in the PCB

	mov qword [r11 + 24], USER_ALLOC_START			; Fill in the next available linear address field of the PCB 

	; Copy and create a new task's 4-level header table from the currently active 4-level header table.
	call copy_current_pml4
	mov [r11 + 56], rax								; Fill in the CR3 field of the PCB, default PCD=PWT=0

	; Because we are executing at the high end of the address space, we can execute kernel code and access kernel data normally, 
	; after all, all the high-end (global) parts of the tasks are the same.
	; At the same time, the stack currently in use is the stack at the high end of the address space.
	; So, we can switch to the new task's address space and clear the first half of its 4-level header table.

	mov r15, cr3									; Save the value of control register CR3
	mov cr3, rax									; Switch to the new address space mapped by the new 4-level header table

	; Clear the first half of the current 4-level header table (corresponding to the task's local address space)
	mov rax, 0xffff_ffff_ffff_f000					; Linear address of the current active 4-level header table itself
	mov rcx, 256
	.clsp:
		mov qword [rax], 0
		add rax, 8
		loop .clsp

	mov rax, cr3									; Refresh TLB
	mov cr3, rax

	mov rcx, 4096 * 16								; Allocate stack space for TSS's RSP0
	call core_memory_allocate						; Must be opened in the space of the kernel
	mov [r12 + 32], r14								; Fill in the value of the RSP0 field in TCB

	mov rcx, 4096 * 16								; Allocate stack space for the main thread
	call user_memory_allocate
	mov [r12 + 120], r14							; Fill in the RSP when the main thread is executed.

	mov qword [r11 + 16], 0							; Task status=running
	mov qword [r12 + 16], 0							; Thread status=ready

	; Load the user program
	mov rcx, 512									; Allocate a buffer in the private space
	call user_memory_allocate
	mov rbx, r13
	mov rax, r8										; Starting sector number of the user program
	call read_hard_disk_0

	mov [r13 + 16], r13								; Fill in its own starting linear address in the program
	mov r14, r13
	add r14, [r13 + 8]
	mov [r12 + 192], r14							; Register the linear address of the program's entry point in the TCB

	; Determine the size of the entire program
	mov rcx, [r13]									; Program size
	test rcx, 0x1ff									; Can it be divided by 512?
	jz .y512
	shr rcx, 9										; No? Make up.
	shl rcx, 9
	add rcx, 512
	.y512:
		sub rcx, 512								; Subtract the length of a sector that has been read
		jz .rdok
		call user_memory_allocate
		;mov rbx, r13
		shr rcx, 9									; Number of sectors to be read after dividing by 512
		inc rax										; Start sector number
	.b1:
		call read_hard_disk_0
		inc rax
		loop .b1									; Loop read until the entire user program is read

	.rdok:
		mov qword [r12 + 200], USER_CODE64_SEL		; Main thread code segment selector
		mov qword [r12 + 208], USER_STACK64_SEL		; Main thread stack segment selector

		pushfq
		pop qword [r12 + 232]

		mov qword [r12 + 240], SUGG_PREEM_SLICE		; Recommended thread execution time slice, from global_defs.wid

		call generate_process_id
		mov [r11 + 8], rax							; Record the identifier of the new task

		call generate_thread_id
		mov [r12 + 8], rax							; Record the identifier of the main thread

		mov qword [r12 + 280], 0					; Next TCB linear address (0=no)

		call append_to_pcb_link						; Add PCB to the end of the process control block list

		mov cr3, r15								; Switch to the address space of the original task

		mov rsi, LAPIC_START_ADDR					; Physical address of Local APIC
		mov dword [rsi + 0x310], 0
		mov dword [rsi + 0x300], 0x000840fe			; Send task/thread claim interrupt to all processors

		pop r15
		pop r14
		pop r13
		pop r12
		pop r11
		pop r10
		pop r9
		pop r8
		pop rbp
		pop rdi
		pop rsi
		pop rdx
		pop rcx
		pop rbx
		pop rax

		ret
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
syscall_procedure:									; System call processing process
	; RCX and R11 are used by the processor to save the contents of RIP and RFLAGS; the interrupt is disabled when entering
	swapgs											; Switch GS to the data area of the current processor
	mov [gs:0], rsp									; Temporarily save the current 3 privilege level stack pointer
	mov rsp, [gs:128+4]								; Use TSS's RSP0 as a secure stack pointer
	push qword [gs:0]
	swapgs
	sti												; All preparations are completed, and there is no problem with interrupts and task switching

	push r15
	mov r15, [rel position]
	add r15, [r15 + rax * 8 + sys_entries]			; Get the linear address of the specified system call function
	call r15
	pop r15

	cli
	pop rsp											; Restore the original 3 privilege level stack pointer
	o64 sysret
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init:												; Initialization of the working environment of the kernel

	; Map the linear address of the GDT to the same position at the high end of virtual memory.
	; The processor does not support the operation of 64-bit immediate numbers to memory addresses, so it is completed with two instructions.
	mov rax, UPPER_GDT_LINEAR						; High-end linear address of GDT
	mov qword [SDA_PHY_ADDR + 4], rax				; Note: It must be an extended address

	lgdt [SDA_PHY_ADDR + 2]							; Load only the 64-bit linear address part in 64-bit mode

	; Map the stack to the high end, otherwise, when the stack is pushed, it is still pushed to the low end and conflicts with the content of the low end.
	; Add operations with source operands as 64-bit immediate numbers are not supported in 64-bit mode.
	mov rax, 0xffff800000000000						; Add UPPER_LINEAR_START
	add rsp, rax									; The stack pointer must be converted to a high-end address and must be an extended address

	; Prepare the processor to start executing from the high end of the virtual address space (now still executing from the low end)
	mov rax, 0xffff800000000000						; Use constant UPPER_LINEAR_START
	add [rel position], rax							; The starting position data of the kernel program must also be converted to an extended address

	; The starting address of the kernel + the assembly address of the label .to_upper = the runtime extended address of the location where the label .to_upper is located
	mov rax, [rel position]
	add rax, .to_upper
	jmp rax											; Absolute indirect near transfer, from now on, execute the instructions behind in the high end

	.to_upper:
		; Initialize the interrupt descriptor table IDT, and install gate descriptors for 32 exceptions and 224 interrupts

		; Create interrupt gates for 32 exceptions with general processing procedures
		mov r9, [rel position]
		lea rax, [r9 + general_exception_handler]	; Get the linear address of the general exception handling process
		call make_interrupt_gate					; make_interrupt_gate in core_utils64_mp.wid

	xor r8, r8
	.idt0:
		call mount_idt_entry						; Mount_idt_entry in core_utils64_mp.wid
		inc r8
		cmp r8, 31
		jle .idt0

	; Create an interrupt gate for the general processing procedure corresponding to other interrupts
	lea rax, [r9 + general_interrupt_handler]		; Get the linear address of the general interrupt handling process
	call make_interrupt_gate						; make_interrupt_gate in core_utils64_mp.wid

	mov r8, 32
	.idt1:
		call mount_idt_entry						; Mount_idt_entry in core_utils64_mp.wid
		inc r8
		cmp r8, 255
		jle .idt1

	mov rax, UPPER_IDT_LINEAR						; High-end linear address of the interrupt descriptor table IDT
	mov rbx, UPPER_SDA_LINEAR						; High-end linear address of the system data area SDA
	mov qword [rbx + 0x0e], rax
	mov word [rbx + 0x0c], 256 * 16 - 1

	lidt [rbx + 0x0c]								; Load only the 64-bit linear address part in 64-bit mode

	mov al, 0xff									; Mask all interrupt signals sent to the 8259A master chip
	out 0x21, al									; Mask all interrupt signals sent to the 8259A master chip

	; The first message displayed in 64-bit mode!
	mov r15, [rel position]
	lea rbx, [r15 + welcome]
	call put_string64								; Put_string64 in core_utils64_mp.wid

	; Install the code segment and stack segment descriptors required for system services (SYSCALL/SYSRET)
	mov r15, UPPER_SDA_LINEAR						; Linear address of the system data area SDA
	xor rbx, rbx
	mov bx, [r15 + 2]								; BX=GDT limit value
	inc bx											; BX=GDT length
	add rbx, [r15 + 4]								; RBX=Append position of new descriptor

	mov dword [rbx], 0x0000ffff						; 64-bit mode does not support 64-bit immediate number transfer
	mov dword [rbx + 4], 0x00cf9200					; Data segment descriptor, DPL=00
	mov dword [rbx + 8], 0							; Reserved descriptor slot
	mov dword [rbx + 12], 0
	mov dword [rbx + 16], 0x0000ffff				; Data segment descriptor, DPL=11
	mov dword [rbx + 20], 0x00cff200
	mov dword [rbx + 24], 0x0000ffff				; Code segment descriptor, DPL=11
	mov dword [rbx + 28], 0x00aff800

	; Prepare an exclusive data area for each logical processor, which is pointed to by GS of each processor.
	; We prepare an exclusive data area for each logical processor, which is pointed to by GS of each processor.
	mov rcx, 256									; Length of exclusive data area, including TSS.
	call core_memory_allocate
	mov qword [r13 + 8], 0							; Clear the "current task PCB pointer field" in advance
	mov qword [r13 + 16], 0							;#0 Set the number of the current processor to #0
	mov [r13 + 24], rsp								; Set the exclusive stack of the current processor
	lea rax, [r13 + 128]							; TSS starts at an offset of 128 within the exclusive data area
	call make_tss_descriptor
	mov qword [rbx + 32], rsi						; Low 64 bits of TSS descriptor
	mov qword [rbx + 40], rdi						; High 64 bits of TSS descriptor

	add word [r15 + 2], 48							; Total number of bytes of 4 segment descriptors and 1 TSS descriptor
	lgdt [r15 + 2]

	mov cx, 0x0040									; TSS descriptor selector
	ltr cx

	; Save the starting address of the processor's exclusive data area to the processor's model-specific register IA32_KERNEL_GS_BASE
	mov ecx, 0xc000_0102							; IA32_KERNEL_GS_BASE 
	mov rax, r13									; Save only EAX
	mov rdx, r13
	shr rdx, 32										; Save only EDX
	wrmsr

	; Prepare parameters for fast system calls SYSCALL and SYSRET
	mov ecx, 0x0c0000080							; Select model-specific register IA32_EFER
	rdmsr
	bts eax, 0										; Set the SCE bit to allow the SYSCALL instruction
	wrmsr

	mov ecx, 0xc0000081								; STAR
	mov edx, (RESVD_DESC_SEL << 16) | CORE_CODE64_SEL
	xor eax, eax
	wrmsr

	mov ecx, 0xc0000082								; LSTAR
	mov rax, [rel position]
	lea rax, [rax + syscall_procedure]				; Save only the EAX part
	mov rdx, rax
	shr rdx, 32										; Save the EDX part
	wrmsr

	mov ecx, 0xc0000084								; FMASK 
	xor edx, edx
	mov eax, 0x00047700								; Request TF=IF=DF=AC=0; IOPL=00
	wrmsr

	; The following initializes the Advanced Programmable Interrupt Controller APIC. After the computer starts,
	; the BIOS has done the initialization of LAPIC and IOAPIC, and the creation of related Advanced Configuration 
	; and Power Interface (ACPI) entries. We can get multi-processors and APIC information from ACPI tables.

	; The memory area claimed by ACPI has been saved in our system data area (SDA), and the following reads it out.
	; This memory area may be located in a part of the paging system that has not been mapped, so the following will
	; first map this part of memory one by one (linear address=physical address)
	cmp word [SDA_PHY_ADDR + 0x16], 0
	jz .acpi_err									; Incorrect ACPI data, may not support ACPI
	mov rsi, SDA_PHY_ADDR + 0x18					; System data area: starting address of the address range descriptor structure
	.looking:
		cmp dword [rsi + 16], 3						; 3: Memory claimed by ACPI (AddressRangeACPI)
		jz .looked
		add rsi, 32									; 32: Length of each address range descriptor structure
		loop .looking

	.acpi_err:
		mov r15, [rel position]
		lea rbx, [r15 + acpi_error]
		call put_string64							; Put_string64 in core_utils64_mp.wid
		cli
		hlt

	.looked:
		mov rbx, [rsi]								; Physical address claimed by ACPI
		mov rcx, [rsi + 8]							; Memory claimed by ACPI, in bytes
		add rcx, rbx								; Physical address of the upper boundary of the memory claimed by ACPI
		mov rdx, 0xffff_ffff_ffff_f000				; Mask for generating page addresses
	.maping:
		mov r13, rbx								; R13: Linear address of this mapping
		mov rax, rbx
		and rax, rdx
		or rax, 0x07								; RAX: Physical address and attributes of this mapping
		call mapping_laddr_to_page
		add rbx, 0x1000
		cmp rbx, rcx
		jle .maping

	; Starting from physical address 0x60000, search for the Root System Descriptor Pointer structure (RSDP)
	mov rbx, 0x60000
	mov rcx, 'RSD PTR '								; Start mark of the structure (note the trailing space)
	.searc:
		cmp qword [rbx], rcx
		je .founda
		add rbx, 16									; The mark of the structure is always at a 16-byte boundary
		cmp rbx, 0xffff0							; Upper boundary of the low-end 1MB physical memory
		jl .searc
		jmp .acpi_err								; Not found RSDP, error shutdown processing.

	.founda:
		; RSDT and XSDT both point to MADT, but RSDT gives a 32-bit physical address, while XDST gives a 64-bit physical address.
		; Only VCPI 2.0 and higher versions have XSDT. Typically, VBox supports ACPI 2.0 while Bochs only supports 1.0
		cmp byte [rbx + 15], 2						; Detect whether the version of ACPI is 2
		jne .vcpi_1
		mov rbx, [rbx + 24]							; Get the physical address of the extended system description table (XSDT)

	; The following starts to traverse the XSDT to search for multiple APIC descriptor tables (MADT)
	xor rdi, rdi
	mov edi, [rbx + 4]								; Get the length of XSDT (in bytes)
	add rdi, rbx									; Calculate the physical location of the upper boundary of XSDT
	add rbx, 36										; Physical position of the tail array of XSDT
	.madt0:
		mov r11, [rbx]
		cmp dword [r11], 'APIC'						; MADT table mark
		je .foundm
		add rbx, 8									; Next element
		cmp rbx, rdi
		jl .madt0
		jmp .acpi_err								; Not found MADT, error shutdown processing.

	;The following is processed according to VCPI 1.0, and starts to traverse the RSDT to search for multiple APIC descriptor tables (MADT)
	.vcpi_1:
		mov ebx, [rbx + 16]							; Get the physical address of the root system descriptor table (RSDT)
		; The following starts to traverse the RSDT to search for multiple APIC descriptor tables (MADT)
		mov edi, [ebx + 4]							; Get the length of RSDT (in bytes)
		add edi, ebx								; Calculate the physical location of the upper boundary of RSDT
		add ebx, 36									; Physical position of the tail array of RSDT
		xor r11, r11
	.madt1:
		mov r11d, [ebx]
		cmp dword [r11], 'APIC'						; MADT table mark
		je .foundm
		add ebx, 4									; Next element
		cmp ebx, edi
		jl .madt1
		jmp .acpi_err								; Not found MADT, error shutdown processing.

	.foundm:
		; Now, R11 is the physical address of MADT
		mov edx, [r11 + 36]							; Predefined LAPIC physical address
		mov [rel lapic_addr], edx

		; Start traversing the logical processors (their LAPID ID) and I/O APIC in the system.
		mov r15, [rel position]						; Prepare linear address for accessing cpu_list
		lea r15, [r15 + cpu_list]

		xor rdi, rdi
		mov edi, [r11 + 4]							; EDI: Length of MADT, in bytes
		add rdi, r11								; RDI: Physical address of the upper boundary of MADT
		add r11, 44									; R11: Points to the interrupt controller structure list at the end of MADT
	.enumd:
		cmp byte [r11], 0							; List item type: Processor Local APIC
		je .l_apic
		cmp byte [r11], 1							; List item type: I/O APIC
		je .ioapic
		jmp .m_end
		.l_apic:
			cmp dword [r11 + 4], 0					; Local APIC Flags
			jz .m_end
			mov al, [r11 + 3]						; local APIC ID
			mov [r15], al							; Save local APIC ID to cpu_list
			inc r15
			inc byte [rel num_cpus]					; Increment the number of available CPUs
			jmp .m_end
		.ioapic:
			mov al, [r11 + 2]						; APIC ID Get I/O APIC ID
			mov [rel ioapic_id], al					; APIC ID Save I/O APIC ID
			mov eax, [r11 + 4]						; Get I/O APIC physical address
			mov [rel ioapic_addr], eax				; Save I/O APIC physical address
	.m_end:
		xor rax, rax
		mov al, [r11 + 1]
		add r11, rax								; Calculate the address of the next interrupt controller structure list item
		cmp r11, rdi
		jl .enumd

	; Map the physical address of the Local APIC to the predefined linear address LAPIC_START_ADDR
	mov r13, LAPIC_START_ADDR						; Defined in global_defs.wid
	xor rax, rax
	mov eax, [rel lapic_addr]						; Get the physical address of LAPIC
	or eax, 0x1f									; PCD=PWT=U/S=R/W=P=1, strongly non-cacheable
	call mapping_laddr_to_page

	; Map the physical address of the I/O APIC to the predefined linear address IOAPIC_START_ADDR
	mov r13, IOAPIC_START_ADDR						; Defined in global_defs.wid
	xor rax, rax
	mov eax, [rel ioapic_addr]						; Get the physical address of I/O APIC
	or eax, 0x1f									; PCD=PWT=U/S=R/W=P=1, strongly non-cacheable
	call mapping_laddr_to_page

	; Measure how many clock cycles the current processor goes through in 1 millisecond as a subsequent timing benchmark.
	mov rsi, LAPIC_START_ADDR						; Physical address of Local APIC

	mov dword [rsi + 0x320], 0x10000				; Local vector table entry register of the timer. Single-shot mode
	mov dword [rsi + 0x3e0], 0x0b					; Timer divider configuration register: 1 divider (no divider)

	mov al, 0x0b									; Real-time clock register B
	or al, 0x80										; Mask NMI
	out 0x70, al
	mov al, 0x52									; Set register B, open periodic interrupt, open more
	out 0x71, al									; Clock interrupt, BCD code, 24-hour system

	mov al, 0x8a									; Real-time clock register A
	out 0x70, al
	;in al, 0x71
	mov al, 0x2d									; 32kHz, 125ms periodic interrupt
	out 0x71, al									; Write back to CMOS register A

	mov al, 0x8c
	out 0x70, al
	in al, 0x71										; Read register C
	.w0:
		in al, 0x71									; Read register C
		bt rax, 6									; Has the update cycle end interrupt occurred?
		jnc .w0
		mov dword [rsi + 0x380], 0xffff_ffff		; Timer initial count register: set the initial value and start counting
	.w1:
		in al, 0x71									; Read register C
		bt rax, 6									; Has the update cycle end interrupt occurred?
		jnc .w1
		mov edx, [rsi + 0x390]						; Timer current count register: read the current count value

	mov eax, 0xffff_ffff
	sub eax, edx
	xor edx, edx
	mov ebx, 125									; 125 milliseconds
	div ebx											; EAX=Number of clocks the current processor goes through in 1ms

	mov [rel clocks_1ms], eax						; Register for use in other timing situations

	mov al, 0x0b									; Real-time clock register B
	or al, 0x80										; Mask NMI
	out 0x70, al
	mov al, 0x12									; Set register B, only allow update cycle end interrupt
	out 0x71, al

	; Install the processing procedure of the new task claim interrupt
	mov r9, [rel position]
	lea rax, [r9 + new_task_notify_handler]			; Get the linear address of the interrupt handling process
	call make_interrupt_gate						; make_interrupt_gate in core_utils64_mp.wid

	cli
	mov r8, 0xfe
	call mount_idt_entry							; Mount_idt_entry in core_utils64_mp.wid
	sti

	; Install the processing procedure of the time slice expiration interrupt
	mov r9, [rel position]
	lea rax, [r9 + time_slice_out_handler]			; Get the linear address of the interrupt handling process
	call make_interrupt_gate						; make_interrupt_gate in core_utils64_mp.wid

	cli
	mov r8, 0xfd
	call mount_idt_entry							; Mount_idt_entry in core_utils64_mp.wid
	sti

	; Start initializing the application processor AP. First copy the initialization code to the selected position at the lowest end of physical memory
	mov rsi, [rel position]
	lea rsi, [rsi + section.ap_init_block.start]
	mov rdi, AP_START_UP_ADDR
	mov rcx, ap_init_tail - ap_init
	cld
	repe movsb

	; All processors should increment the response count during initialization
	inc byte [rel ack_cpus]							; BSP's own response count value

	; Send INIT IPI and SIPI to other processors to command them to initialize themselves
	mov rsi, LAPIC_START_ADDR						;Physical address of Local APIC
	mov dword [rsi + 0x310], 0
	mov dword [rsi + 0x300], 0x000c4500	; IPI First send INIT IPI
	mov dword [rsi + 0x300], (AP_START_UP_ADDR >> 12) | 0x000c4600	; Start up IPI
	mov dword [rsi + 0x300], (AP_START_UP_ADDR >> 12) | 0x000c4600	; Start up IPI

	mov al, [rel num_cpus]
	.wcpus:
		cmp al, [rel ack_cpus]
		jne .wcpus									; Wait for the response of all application processors

	; Display the number of processors that have responded
	mov r15, [rel position]

	xor r8, r8
	mov r8b, [rel ack_cpus]
	lea rbx, [r15 + buffer]
	call bin64_to_dec
	call put_string64

	lea rbx, [r15 + cpu_init_ok]
	call put_string64								; Put_string64 in core_utils64_mp.wid

	; Start creating system shell tasks (processes)
	mov r8, 50
	call create_process

	jmp ap_to_core_entry.do_idle					; Go to the processor assembly rest area :)

;===============================================================================
section ap_init_block vstart=0

	bits 16											; Application processor AP starts execution from real mode

ap_init:											; The initialization code of the application processor AP
	mov ax, AP_START_UP_ADDR >> 4
	mov ds, ax

	SET_SPIN_LOCK al, byte [lock_var]				; Spin until the lock is obtained

	mov ax, SDA_PHY_ADDR >> 4						; Select the system data area
	mov ds, ax

	; Load the descriptor table register GDTR
	lgdt [2]										; Load only 6 bytes of content in real mode

	in al, 0x92										; Port in the south bridge chip
	or al, 0000_0010B
	out 0x92, al									; Open A20

	cli												; The interrupt mechanism is not working yet

	mov eax, cr0
	or eax, 1
	mov cr0, eax									; Set the PE bit

	; The following enters protected mode...
	jmp 0x0008: AP_START_UP_ADDR + .flush			; Pipeline clear and processor serialization

	[bits 32]
	.flush:
		mov eax, 0x0010								; Load the data segment (4GB) selector
		mov ss, eax									; Load the stack segment (4GB) selector
		mov esp, 0x7e00								; ESP=0x7e00 Stack pointer ESP=0x7e00

		; Set the CR3 register to point to the 4-level head table (32-bit CR3 in protected mode)
		mov eax, PML4_PHY_ADDR						; PCD=PWT=0
		mov cr3, eax

		; Enable Physical Address Extension PAE
		mov eax, cr4
		bts eax, 5
		mov cr4, eax

		; Set the model-specific register IA32_EFER.LME to allow IA_32e mode
		mov ecx, 0x0c0000080						; Select model-specific register IA32_EFER
		rdmsr
		bts eax, 8									; Set the LME bit
		wrmsr

		; Enable paging
		mov eax, cr0
		bts eax, 31									; Set CR0.PG
		mov cr0, eax

	; Enter 64-bit mode
	jmp CORE_CODE64_SEL:AP_START_UP_ADDR + .to64
	.to64:

		bits 64

		; Continue initialization in the kernel (using high-end linear address)
		mov rbx, UPPER_CORE_LINEAR + ap_to_core_entry
		jmp rbx

lock_var	db 0

ap_init_tail:

;===============================================================================
section core_tail
core_end: