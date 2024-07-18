;-------------------------------------------------------------------------------
%include ".\utils\global_defs.wid"
;-------------------------------------------------------------------------------
SECTION mbr vstart=0x00007c00

	mov ax, cs                                   	; CS=0, IP=0x7C00
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 0x7c00

	; Below is to load the kernel loader from the first sector of the hard disk
	push dword 0
	push dword LDR_START_SECTOR                  	; 1st sector to be transferred
	push word LDR_PHY_ADDR >> 4                  	; Buffer segment address
	push word 0                                  	; Buffer offset
	push word 0x0001                             	; Number of sectors to be transferred
	push word 0x0010                             	; Address structure size and reserved bytes
	mov si, sp
	mov ah, 0x42                                 	; Extended read function
	mov dl, 0x80                                 	; Main disk
	int 0x13                                     	; Success: CF=0, AH=0; Failure: CF=1, AH=error code

	mov bp, msg0
	mov di, msg1 - msg0
	jc go_err                                    	; Disk read error, display message and halt

	push ds

	mov cx, LDR_PHY_ADDR >> 4                    	; Switch to the segment address of the loader
	mov ds, cx

	cmp dword [0], 'AAA'                        	; Check the loader valid flag
	mov bp, msg1
	mov di, mend - msg1
	jne go_err                                   	; Loader not found, display message and halt

	; Here is to determine the size of the entire program
	mov eax, [4]                                 	; Core program size
	xor edx, edx
	mov ecx, 512                                 	; 512 bytes per sector
	div ecx

	or edx, edx
	jnz @1                                       	; Not divisible, so the result is one less than the actual number of sectors
	dec eax                                      	; One sector has been read, so the total number of sectors is reduced by 1
@1:
	or eax, eax										; Consider the case where the actual length ≤ 512 bytes
	jz go_ldr										; EAX=0 ? 

	; Read the remaining sectors
	pop ds											; Prepare to pass the disk address structure

	mov [si + 2], ax								; Reset the number of logical sectors to be read
	mov word [si + 4], 512							; Reset the next segment offset
	inc dword [si + 8]								; Start logical sector number plus one
	mov ah, 0x42									; Extended read function
	mov dl, 0x80									; Main disk
	int 0x13										; Success: CF=0, AH=0; Failure: CF=1, AH=error code

	mov bp, msg0
	mov di, msg1 - msg0
	jc go_err										; Disk read error, display message and halt

	go_ldr:
		mov sp, 0x7c00								; Restore the initial state of the stack

		mov ax, LDR_PHY_ADDR >> 4
		mov ds, ax
		mov es, ax

		push ds
		push word [8]
		retf										; Enter the loader execution

	go_err:
		mov ah, 0x03								; Get cursor position
		mov bh, 0x00
		int 0x10

		mov cx, di
		mov ax, 0x1301								; Write string, move cursor
		mov bh, 0
		mov bl, 0x07								; Attribute: black background, white text
		int 0x10									; Display string

		cli
		hlt

;-------------------------------------------------------------------------------
msg0	db "Disk error.", 0x0d, 0x0a
msg1	db "Missing loader.", 0x0d, 0x0a
mend:
;-------------------------------------------------------------------------------
; fill it up to 512 bytes
	times 510-($-$$) db 0
	db 0x55, 0xaa
