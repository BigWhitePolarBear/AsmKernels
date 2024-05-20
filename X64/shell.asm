;===============================================================================
section shell_header							; Shell Header
	length	dq shell_end                       	;#0: Total length of the shell program
	entry	dq start                           	;#8: Shell entry point
	linear	dq 0                               	;#16: Linear address of the shell program

;===============================================================================
section shell_data								; Shell Data
	shell_msg	times 128 db 0

	msg0	db "Thread ", 0
	tid		times 32 db 0                      	; Thread ID
	msg1	db " <OS SHELL> on CPU ", 0
	pcpu	times 32 db 0                      	; Processor number
	msg2	db " -", 0

	time_buff	times 32 db 0					; Current time


;===============================================================================
section shell_code								; Shell Code

%include ".\utils\user_static64.wid"

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	bits 64

main:
	; Below, simulate running 3 programs as required by the user...
	mov r8, 100
	mov rax, 3
	syscall
	syscall
	syscall										; Create 3 tasks with the same copy

	mov rax, 0
	syscall										; Display available lines, DH=line number
	mov dl, 0
	mov r9b, 0x5f

	mov r12, [rel linear]
_time:
	lea rbx, [r12 + time_buff]
	mov rax, 1
	syscall

	mov rax, 6                               	; Get the number of the current processor
	syscall
	mov r8, rax
	lea rbx, [r12 + pcpu]
	call bin64_to_dec                        	; Convert the processor number to a string

	mov rax, 8                               	; Return the identifier of the current thread
	syscall
	mov r8, rax
	lea rbx, [r12 + tid]
	call bin64_to_dec                        	; Convert the thread identifier to a string

	lea rdi, [r12 + shell_msg]
	mov byte [rdi], 0

	lea rsi, [r12 + msg0]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	lea rsi, [r12 + tid]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	lea rsi, [r12 + msg1]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	lea rsi, [r12 + pcpu]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	lea rsi, [r12 + msg2]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	lea rsi, [r12 + time_buff]
	call string_concatenates                 	; Concatenate strings, similar to strcat

	mov rbx, rdi
	mov rax, 2
	syscall

	jmp _time

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start:	; Entry point of the program
	call main
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shell_end:
