;===============================================================================
SECTION header vstart=0
	
	program_length	dd	program_end					; #0x00 Program length                                    
	entry_point		dd	start						; #0x04 Program entry point

	header_end:
;===============================================================================
SECTION data vfollows=header                

	message_1	db '[USER TASK]: ,,,,,,,,,,,,,,,,,,,,', 0x0d, 0x0a, 0

	reserved	times 4096*5 db 0					; Save a blank area to demonstrate paging
	
	data_end:
;===============================================================================
[bits 32]
;===============================================================================
SECTION code vfollows=data   

	start:
		; Allocate memory in the current task's virtual address space
		mov eax, 5
		mov ecx,128									; Call MAlloc for 128 bytes
		int 0x88
		mov ebx,ecx									; Prepare parameters for printing strings later
		
		; Copy the string to the allocated memory
		mov esi,message_1
		mov edi,ecx
		mov ecx,reserved-message_1
		cld
		repe movsb
         
		.show:
			mov eax,0
			int 0x88
			jmp .show 
    
	code_end:
;===============================================================================
SECTION trail
;===============================================================================
	program_end: