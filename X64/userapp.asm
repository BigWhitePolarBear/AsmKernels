;===============================================================================
section app_header								; Application Header
	length	dq app_end                         	; #0: The total length of the user program (in bytes)
	entry	dq start                           	; #8: The entry point of the user program
	linear	dq 0                               	; #16: The virtual (linear) address where the user program is loaded

;===============================================================================
section app_data								; Application Data

	tid_prex	db "Thread ", 0					; Thread prefix
	pid_prex	db " <Task ", 0					; Task prefix
	cpu_prex	db "> on CPU ", 0				; CPU prefix
	delim		db " do 1+2+3+...+", 0			; Delimiter
	equal		db "=", 0						; Equal sign

;===============================================================================
section app_code								; Application Code

	%include ".\utils\user_static64.wid"

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	bits 64

thread_procedure:
	mov rbp, rsp                             	; RBP accesses data on the stack, local variables in high-level languages.
	sub rsp, 56

	mov rax, 10                              	; Allocate memory
	mov rdx, 288                             	; 288 bytes
	syscall
	mov [rbp-8], r13                         	; RBP-8->The linear address of the total string buffer

	add r13, 128
	mov [rbp-16], r13                        	; RBP-16->The text used to save the thread identifier

	add r13, 32
	mov [rbp-24], r13                        	; RBP-24->The text used to save the task identifier

	add r13, 32
	mov [rbp-32], r13                        	; RBP-32->The text used to save the processor number

	add r13, 32
	mov [rbp-40], r13                       	; RBP-40->The text used to save the addend

	add r13, 32
	mov [rbp-48], r13                        	; RBP-48->The text used to save the sum

	mov rax, 8                               	; Get the identifier of the current thread
	syscall
	mov r8, rax
	mov rbx, [rbp-16]
	call bin64_to_dec                        	; Convert the thread identifier to a string

	mov rax, 4                               	; Get the identifier of the current task (process)
	syscall
	mov r8, rax
	mov rbx, [rbp-24]
	call bin64_to_dec                        	; Convert the process identifier to a string

	mov r12, [rel linear]                    	; The starting linear address of the current program

	mov rax, 0                               	; Determine the display line that the current program can use
	syscall                                  	; Display line available, DH=line number

	mov dl, 0
	mov r9b, 0x0f

	mov r8, 0                                	; R8 is used to store the sum
	mov r10, 1                               	; R10 is used to provide addends
.cusum:
	add r8, r10
	mov rbx, [rbp-48]
	call bin64_to_dec                        	; Convert the result of this addition to a string

	xchg r8, r10
	mov rbx, [rbp-40]
	call bin64_to_dec                        	; Convert this addend to a string

	xchg r8, r10

	mov rax, 6                               	; Get the number of the current processor
	syscall

         push r8
         mov r8, rax
         mov rbx, [rbp-32]
         call bin64_to_dec                        ; Convert the processor number to a string
         pop r8

         mov rdi, [rbp-8]
         mov byte [rdi], 0

         lea rsi, [r12 + tid_prex]
         call string_concatenates                 ; Concatenate strings, same as strcat

         mov rsi, [rbp-16]
         call string_concatenates

         lea rsi, [r12 + pid_prex]
         call string_concatenates                 ; Concatenate strings, same as strcat

         mov rsi, [rbp-24]
         call string_concatenates

         lea rsi, [r12 + cpu_prex]
         call string_concatenates

         mov rsi, [rbp-32]
         call string_concatenates

         lea rsi, [r12 + delim]
         call string_concatenates

         mov rsi, [rbp-40]
         call string_concatenates

         lea rsi, [r12 + equal]
         call string_concatenates

         mov rsi, [rbp-48]
         call string_concatenates

         mov rax, 2                               ; Display the string at the specified coordinates
         mov rbx, rdi
         syscall

	inc r10
	cmp r10, 10000;000
	jbe .cusum

	mov rsp, rbp                             	; Stack balance to return position
	ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main:
	mov rsi, [rel linear]                    	; The starting linear address of the current program

	lea rsi, [rsi + thread_procedure]        	; The linear address of the thread routine
	mov rax, 7                               	; Create a thread
	syscall                                  	; Create the first thread
	syscall                                  	; Create the second thread

	call thread_procedure                    	; Ordinary routine call (can return)

	ret
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start:	; Entry point of the program

	; Here is the initialization code, such as initializing global data (variables)

	call main

	; Here is the cleanup code

	mov rax, 5                               	; Exit the task
	syscall

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
app_end:
