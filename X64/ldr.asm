;-------------------------------------------------------------------------------
%include ".\utils\global_defs.wid"
;===============================================================================
section loader
  marker       dd "AAA"                               ; +00 Sign of loader
  length       dd ldr_end                             ; +04 Length of loader
  entry        dd start                               ; +08 Entry point of loader

  msg0         db "x64 course learning.",0x0d,0x0a

  arch0        db "x64 available(64-bit processor installed).",0x0d,0x0a
  arch1        db "x64 not available(64-bit processor not installed).",0x0d,0x0a

  brand_msg    db "Processor:"
      brand    times 48  db 0
               db  0x0d,0x0a

  cpu_addr     db "Physical address size:"
     paddr     times 3 db ' '
               db ","
               db "Linear address size:"
     laddr     times 3 db ' '
               db 0x0d,0x0a

  protect      db "Protect mode has been entered to prepare for IA-32e mode.",0x0d,0x0a,0

  ia_32e       db "IA-32e mode(aka,long mode) is active.Specifically,compatibility mode.",0x0d,0x0a,0

;-------------------------------------------------------------------------
 no_ia_32e:
         mov ah, 0x03                                 ; Get cursor position
         mov bh, 0x00
         int 0x10

         mov bp, arch1
         mov cx, brand_msg - arch1
         mov ax, 0x1301                               ; Write string, move cursor
         mov bh, 0
         mov bl, 0x07                                 ; Attribute: red background, bright white font
         int 0x10                                     ; Display string

         cli
         hlt

  start:
         mov ah, 0x03                                 ; Get cursor position
         mov bh, 0x00
         int 0x10

         mov bp, msg0
         mov cx, arch0 - msg0
         mov ax, 0x1301                               ; Write string, move cursor
         mov bh, 0
         mov bl, 0x4f                                 ; Attribute: red background, bright white font
         int 0x10                                     ; Display string

         mov eax, 0x80000000                          ; Get the maximum extended function number supported by the processor
         cpuid
         cmp eax, 0x80000000                          ; Support function number greater than 0x80000000?
         jbe no_ia_32e                                ; Not supported, jump to no_ia_32e

         mov eax, 0x80000001                          ; Get extended signature and feature flag
         cpuid                                        ; The EDX contains the extended feature flag
         bt edx, 29                                   ; The 29th bit of EDX is the IA-32e mode support flag
         ; On the VirtualBox virtual machine, if the version of the operating system is not selected as 64-bit, this flag detection fails.
         jnc no_ia_32e                                ; Not supported, jump to no_ia_32e

         mov ah, 0x03                                 ; Get cursor position
         mov bh, 0x00
         int 0x10

         mov bp, arch0
         mov cx, arch1 - arch0
         mov ax, 0x1301                               ; Write string, move cursor
         mov bh, 0
         mov bl, 0x07                                 ; Attribute: black background, white font
         int 0x10                                     ; Display string

         ; Display processor brand information
         mov eax, 0x80000000
         cpuid                                        ; Get the maximum extended function number supported
         cmp eax, 0x80000004
         jb .no_brand

         mov eax, 0x80000002
         cpuid
         mov [brand + 0x00], eax
         mov [brand + 0x04], ebx
         mov [brand + 0x08], ecx
         mov [brand + 0x0c], edx

         mov eax, 0x80000003
         cpuid
         mov [brand + 0x10], eax
         mov [brand + 0x14], ebx
         mov [brand + 0x18], ecx
         mov [brand + 0x1c], edx

         mov eax, 0x80000004
         cpuid
         mov [brand + 0x20], eax
         mov [brand + 0x24], ebx
         mov [brand + 0x28], ecx
         mov [brand + 0x2c], edx

         mov ah, 0x03                                 ; Get cursor position
         mov bh, 0x00
         int 0x10

         mov bp, brand_msg
         mov cx, cpu_addr - brand_msg
         mov ax, 0x1301                               ; Write string, move cursor
         mov bh, 0
         mov bl, 0x07                                 ; Attribute: black background, white font
         int 0x10                                     ; Display string

 .no_brand:
         ; Get the physical memory layout information of the current system (using INT 0x15, E820 function. Commonly known as E820 memory)
         push es

         mov bx, SDA_PHY_ADDR >> 4                    ; Switch to the system data area
         mov es, bx
         mov word [es:0x16], 0
         xor ebx, ebx                                 ; First call to int 0x15 must be 0
         mov di, 0x18                                 ; Offset in the system data area
 .mlookup:
         mov eax, 0xe820
         mov ecx, 32
         mov edx, 'PAMS'
         int 0x15
         add di, 32
         inc word [es:0x16]
         or ebx, ebx
         jnz .mlookup

         pop es

         ; Get and store the physical/virtual address size information of the processor
         mov eax, 0x80000000                          ; Get the maximum extended function number supported
         cpuid
         cmp eax, 0x80000008
         mov ax, 0x3024                               ; Set the default processor physical/logical address bit number 36 and 48
         jb .no_plsize

         mov eax,0x80000008                           ; Get processor linear/physical address size
         cpuid

 .no_plsize:
         ; Save physical and virtual address size to the system data area
         push ds
         mov bx, SDA_PHY_ADDR >> 4                    ; Switch to the system data area
         mov ds, bx
         mov word [0], ax                             ; Record the physical/virtual address size of the processor
         pop ds

         ; Prepare to display the physical address size information of the memory
         push ax                                      ; Backup AX (virtual address part)

         and ax, 0x00ff                               ; Retain the physical address width part
         mov si, 2
         mov bl, 10
 .re_div0:
         div bl
         add ah, 0x30
         mov [paddr + si], ah
         dec si
         and ax, 0x00ff
         jnz .re_div0

         ; Prepare to display the virtual address size information of the processor
         pop ax

         shr ax, 8                                    ; Retain the linear address width part
         mov si, 2
         mov bl, 10
 .re_div1:
         div bl
         add ah, 0x30
         mov [laddr + si], ah
         dec si
         and ax, 0x00ff
         jnz .re_div1

         ; Display the physical/virtual address size information of the processor
         mov ah, 0x03                                 ; Get cursor position
         mov bh, 0x00
         int 0x10

         mov bp, cpu_addr
         mov cx, protect - cpu_addr
         mov ax, 0x1301                               ; Write string, move cursor
         mov bh, 0
         mov bl, 0x07                                 ; Attribute: black background, white font
         int 0x10                                     ; Display string

         ; Start entering protected mode and do the necessary preparation for IA-32e mode
         mov ax, GDT_PHY_ADDR >> 4                    ; Calculate the logical segment address where the GDT is located
         mov ds, ax

         ; Create the 0# descriptor slot
         ; Create the 1# descriptor, the code segment descriptor in protected mode
         mov dword [0x08], 0x0000ffff                 ; Base address is 0, limit is 0xFFFFF, DPL=00
         mov dword [0x0c], 0x00cf9800                 ; 4KB granularity, code segment descriptor, upward expansion

         ; Create the 2# descriptor, the data segment and stack segment descriptor in protected mode
         mov dword [0x10], 0x0000ffff                 ; Base address is 0, limit is 0xFFFFF, DPL=00
         mov dword [0x14], 0x00cf9200                 ; 4KB granularity, data segment descriptor, upward expansion

         ; Create the 3# descriptor, the code segment descriptor in 64-bit mode. Prepare in advance for entering 64-bit, its L bit is 1
         mov dword [0x18], 0x0000ffff                 ;= Base address is 0, limit is 0xFFFFF, DPL=00
         mov dword [0x1c], 0x00af9800                 ; 4KB granularity, L=1, code segment descriptor, upward expansion


         ; Record the base address and limit value of the GDT
         mov ax, SDA_PHY_ADDR >> 4                    ; Switch to the system data area
         mov ds, ax

         mov word [2], 31                             ; Limit of the descriptor table
         mov dword [4], GDT_PHY_ADDR                  ; Linear base address of the GDT

         ; Load the descriptor table register GDTR
         lgdt [2]

         in al, 0x92                                  ; The port 0x92 in the south bridge chip is used to control the A20 line
         or al, 0000_0010B
         out 0x92, al                                 ; Open A20

         cli                                          ; Interrupt mechanism is not working yet

         mov eax, cr0
         or eax, 1
         mov cr0, eax                                 ; Enable PE bit

         ; ... Enter protected mode...
         jmp 0x0008: dword LDR_PHY_ADDR + flush       ; Flush the pipeline and serialize the processor
                                                      ; Flush the pipeline and serialize the processor
         [bits 32]
  flush:
         mov eax, 0x0010                              ; Load the data segment (4GB) selector
         mov ds, eax
         mov es, eax
         mov fs, eax
         mov gs, eax
         mov ss, eax                                  ; Load the stack segment (4GB) selector
         mov esp, 0x7c00                              ; ESP=0x7c00 Stack pointer ESP=0x7c00

         ; Display information to indicate that we are preparing for IA-32e mode in protected mode
         mov ebx, protect + LDR_PHY_ADDR
         call put_string_flat32

         ; Load the system core program
         mov edi, CORE_PHY_ADDR

         mov eax, COR_START_SECTOR
         mov ebx, edi                                 ; Load address
         call read_hard_disk_0                        ; Read the start of the program (one sector)

         ; Determine the size of the entire program
         mov eax, [edi]                               ; Core program size
         xor edx, edx
         mov ecx, 512                                 ; 512 bytes per sector
         div ecx

         or edx, edx
         jnz @1                                       ; Not divisible, so the result is one less than the actual number of sectors 
         dec eax                                      ; One sector has been read, so the total number of sectors is reduced by 1
   @1:
         or eax, eax                                  ; Consider the case where the actual length ≤ 512 bytes
         jz pge                                       ;EAX=0 ? 

         ; Read the remaining sectors
         mov ecx, eax
         mov eax, COR_START_SECTOR
         inc eax                                      ; Start reading from the next logical sector
   @2:
         call read_hard_disk_0
         inc eax
         loop @2                                      ; Loop read until the entire core is read

   pge:
         ; Fill back the location information (physical/linear address) of the kernel loading to the header of the kernel program
         mov dword [CORE_PHY_ADDR + 0x08], CORE_PHY_ADDR
         mov dword [CORE_PHY_ADDR + 0x0c], 0

         ; Create a 4-level paging system for the kernel, only the basic part, covering the lower 1MB of physical memory

         ; Create the kernel 4-level header table
         mov ebx, PML4_PHY_ADDR                       ; Linear address of the 4-level header table

         ; Clear the contents of the 4-level header table
         mov ecx, 1024
         xor esi, esi
   .cls0:
         mov dword [ebx + esi], 0
         add esi, 4
         loop .cls0

         ; Create an entry in the 4-level header table that points to the 4-level header table itself
         mov dword [ebx + 511 * 8], PML4_PHY_ADDR | 3 ; Add attribute bit
         mov dword [ebx + 511 * 8 + 4], 0

         ; Create a 4-level header entry corresponding to the lower 2MB of memory corresponding to the 4-level header entry of the linear address range: 0x0000000000000000--0x00000000001FFFFF
         ; This entry ensures that the lower 2MB of physical memory (including the kernel) can be accessed normally after paging is enabled and before it is mapped to the high end
         mov dword [ebx + 0 * 8], PDPT_PHY_ADDR | 3   ; Physical address and attributes of the page directory pointer table
         mov dword [ebx + 0 * 8 + 4], 0

         ; Clear the contents of the page directory pointer table
         mov ebx, PDPT_PHY_ADDR

         mov ecx, 1024
         xor esi, esi
   .cls1:
         mov dword [ebx + esi], 0
         add esi, 4
         loop .cls1

         ; Create an entry in the page directory pointer table corresponding to the lower 2MB of memory corresponding to the entry of the linear address range: 0x0000000000000000--0x00000000001FFFFF
         mov dword [ebx + 0 * 8], PDT_PHY_ADDR | 3    ; Physical address and attributes of the page directory table
         mov dword [ebx + 0 * 8 + 4], 0

         ; Clear the contents of the page directory table
         mov ebx, PDT_PHY_ADDR

         mov ecx, 1024
         xor esi, esi
   .cls2:
         mov dword [ebx + esi], 0
         add esi, 4
         loop .cls2

         ; Create an entry in the page directory table corresponding to the lower 2MB of memory corresponding to the entry of the linear address range: 0x0000000000000000--0x00000000001FFFFF
         mov dword [ebx + 0 * 8], 0 | 0x83            ; Physical address and attributes of the 2MB page
         mov dword [ebx + 0 * 8 + 4], 0


         ; Create a 4-level header entry corresponding to the linear address range 0xFFFF800000000000--0xFFFF8000001FFFFF in the 4-level header table, and map the kernel to the high end. The kernel should work in the high end of the linear address after entering IA-32e mode.
         mov ebx, PML4_PHY_ADDR

         mov dword [ebx + 256 * 8], PDPT_PHY_ADDR | 3 ; Physical address and attributes of the page directory pointer table
         mov dword [ebx + 256 * 8 + 4], 0

         ; Create an additional 254 header entries in the upper half of the 4-level header table in advance
         mov eax, 257
         mov edx, COR_PDPT_ADDR | 3                   ; The 254 page directory pointer tables of the kernel start from this address
   .fill_pml4:
         mov dword [ebx + eax * 8], edx
         mov dword [ebx + eax * 8 + 4], 0
         add edx, 0x1000
         inc eax
         cmp eax, 510
         jbe .fill_pml4

         ; Clear all pre-allocated page directory pointer tables
         mov eax, COR_PDPT_ADDR
   .zero_pdpt:
         mov dword [eax], 0                           ; Equivalent to clearing all page directory pointer entries
         add eax, 4
         cmp eax, COR_PDPT_ADDR + 0x1000 * 254        ; End position of all page directory pointer tables of the kernel
         jb .zero_pdpt

         ; Set the CR3 register to point to the 4-level header table (32-bit CR3 in protected mode)
         mov eax, PML4_PHY_ADDR                       ; PCD=PWT=0
         mov cr3, eax

         ; Enable Physical Address Extension PAE
         mov eax, cr4
         bts eax, 5
         mov cr4, eax

         ; Set the model-specific register IA32_EFER.LME to allow IA_32e mode
         mov ecx, 0x0c0000080                         ; Specifying the model-specific register IA32_EFER
         rdmsr
         bts eax, 8                                   ; Set the LME bit
         wrmsr

         ; Enable paging
         mov eax, cr0
         bts eax, 31                                  ; Set CR0.PG
         mov cr0, eax

         ; Print IA_32e activation information
         mov ebx, ia_32e + LDR_PHY_ADDR
         call put_string_flat32

         ; Start the kernel in 64-bit mode by far return
         push word 0x0018                             ; Defined as a constant CORE_CODE64_SEL
         mov eax, dword [CORE_PHY_ADDR + 4]
         add eax, CORE_PHY_ADDR
         push eax
         retf

;-----------------------------------------------------------------------
; A string display routine with cursor tracking. Only run in 32-bit protected mode and use flat model.
put_string_flat32:                                    ; Display 0-terminated string and move cursor
                                                      ; In: EBX=linear address of the string

         push ebx
         push ecx

  .getc:
         mov cl, [ebx]
         or cl, cl                                    ; Check the end flag of the string (0)
         jz .exit                                     ; Finished displaying, return
         call put_char
         inc ebx
         jmp .getc

  .exit:
         pop ecx
         pop ebx

         ret                                          ; Return within the segment

;-------------------------------------------------------------------------------
put_char:                                             ; Display a character at the current cursor position and advance the cursor. Only used for intra-segment calls
                                                      ; Input: CL=character ASCII code
         pushad

         ; Get the current cursor position
         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         inc dx                                       ; 0x3d5
         in al, dx                                    ;  High byte
         mov ah, al

         dec dx                                       ; 0x3d4
         mov al, 0x0f
         out dx, al
         inc dx                                       ; 0x3d5
         in al, dx                                    ; Low byte
         mov bx, ax                                   ; BX= Cursor position represented by 16-bit number
         and ebx, 0x0000ffff                          ; Prepare to access video memory using 32-bit addressing

         cmp cl, 0x0d                                 ; Carriage return?
         jnz .put_0a
         mov ax, bx
         mov bl, 80
         div bl
         mul bl
         mov bx, ax
         jmp .set_cursor

  .put_0a:
         cmp cl, 0x0a                                 ; Line feed?
         jnz .put_other
         add bx, 80
         jmp .roll_screen

  .put_other:                                         ; Normal display character
         shl bx, 1
         mov [0xb8000 + ebx], cl

         ; Following advances the cursor by one character
         shr bx, 1
         inc bx

  .roll_screen:
         cmp bx, 2000                                 ; Cursor out of screen? Scroll
         jl .set_cursor

         push ebx

         cld
         mov esi, 0xb80a0
         mov edi, 0xb8000
         mov ecx, 960
         rep movsd
         mov ebx, 3840                                ; Clear the bottom line of the screen
         mov ecx, 80
  .cls:
         mov word[0xb8000 + ebx], 0x0720
         add ebx, 2
         loop .cls

         pop ebx
         sub ebx, 80

  .set_cursor:
         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         inc dx                                       ; 0x3d5
         mov al, bh
         out dx, al
         dec dx                                       ; 0x3d4
         mov al, 0x0f
         out dx, al
         inc dx                                       ; 0x3d5
         mov al, bl
         out dx, al

         popad

         ret
;-------------------------------------------------------------------------
read_hard_disk_0:                                     ; Read a logical sector from the hard disk
                                                      ; In: EAX=logical sector number
                                                      ;     EBX=destination buffer address
                                                      ; Out: EBX=EBX+512
         push eax
         push ecx
         push edx

         push eax

         mov dx, 0x1f2
         mov al, 1
         out dx, al                                   ; Number of sectors read

         inc dx                                       ; 0x1f3
         pop eax
         out dx, al                                   ; LBA address 7~0

         inc dx                                       ; 0x1f4
         mov cl, 8
         shr eax, cl
         out dx, al                                   ; LBA address 15~8

         inc dx                                       ; 0x1f5
         shr eax, cl
         out dx, al                                   ; LBA address 23~16

         inc dx                                       ; 0x1f6
         shr eax, cl
         or al, 0xe0                                  ; LBA address 27~24
         out dx, al

         inc dx                                       ; 0x1f7

         mov al, 0x20                                 ; Read command
         out dx, al

  .waits:
         in al, dx
         test al, 8
         jz .waits                                   ; Not busy, and the hard disk is ready for data transfer

         mov ecx, 256                                 ; Total number of words to read
         mov dx, 0x1f0
  .readw:
         in ax, dx
         mov [ebx], ax
         add ebx, 2
         loop .readw

         pop edx
         pop ecx
         pop eax

         ret

;-------------------------------------------------------------------------------
section trail
  ldr_end:
