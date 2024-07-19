;===============================================================================
SECTION header vstart=0
	
	program_length	dd	program_end					; #0x00 Program length                                    
	entry_point		dd	start						; #0x04 Program entry point
	salt_position	dd	salt						; #0x08 SALT table start offset
	salt_item_cnt	dd	(header_end-salt)/256		; #0x0c SALT item count
	
;-------------------------------------------------------------------------------
; Symbol Address Lookup Table      
salt:												; #0x10
	PrintString			db  '@PrintString'
						times 256-($-PrintString) db 0

	TerminateProgram	db  '@TerminateProgram'
						times 256-($-TerminateProgram) db 0

	ReadDiskData		db  '@ReadDiskData'
						times 256-($-ReadDiskData) db 0

	InitTaskSwitch		db	'@InitTaskSwitch'
						times 256-($-InitTaskSwitch) db 0

	MAlloc				db	'@MAlloc'
						times 256-($-MAlloc) db 0

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
		mov ecx,128									; Call MAlloc for 128 bytes
		call far [MAlloc]
		mov ebx,ecx									; Prepare parameters for printing strings later
		
		; Copy the string to the allocated memory
		mov esi,message_1
		mov edi,ecx
		mov ecx,reserved-message_1
		cld
		repe movsb
         
		.show:
			call far [PrintString]
			jmp .show 
    
	code_end:
;===============================================================================
SECTION trail
;===============================================================================
	program_end: