; Multi text screen for MSX 2.0 and above
; Description: This tool allows the user cicle among multiple text
; screens in the basic environment.
; 
; Author: Luciano M. Christofoletti
; Date: 11/nov/2016
; Target: MSX BINARY
; 
; This program runs in MSX basic.
; To run the program type: bload"MULTXT80.BIN",r
; This source code must be compiled using the Glass Compiler
; 
; OpCode IDE - www.objectware.net
    
    include "lib/msxbios.asm"
    include "lib/msxhooks.asm"
    
START       equ 0D000H              ; Start address of "installer" routine
    
    ;; **** Some useful constants and addresses
BIT01MSK    equ 000000011B  ; Mask for the two least significant bits of a byte
LOWBITS     equ 000001111B  ; Mask for the least significant nibble of a byte
HIGHBITS    equ 011110000B  ; Mask for the most significant nibble of a byte
CR          equ 0DH         ; Carriage return
LF          equ 0AH         ; Line feed
MSXVER      equ 0002DH      ; MSX version (0 = MSX 1, 1 = MSX 2, 2 = MSX 2+, 3 = MSX turbo R)
HIMEM       equ 0FC4AH      ; Address of the end of the available RAM area
    
    ; The binary file header
    db  0FEH                        ; MSX binary file byte id
    dw  START                       ; Memory start address
    dw  END                         ; End address
    dw  EXEC                        ; Execution address (entry point)
    
    ; This org directive defines the "installer" execution address
    org START
    
    ;; **** Text strings
STARTMSG:
    db "Multi text screen 80C - ver 1.0", CR, LF
    db "Use ctrl+select keys to cycle screens", CR, LF, 0
    
COPYRMSG:
    db "(C) Astesbas 2019", CR, LF, 0
    
ABOUT:
    db "Author: Luciano M. Christofoletti", CR, LF, 0
    db "Code: https://github.com/christofoletti", CR, LF, 0
    
WRONGVER:
    db "This version works on MSX 2.0 and above!", CR, LF
    db "Please, use the MSX 1.0 version instead", CR, LF, 0
    
    ;; **** Program execution entry point ("installs" the app and exit)
EXEC:
    LD   A,(MSXVER)
    OR   A
    JR   NZ,INSTALL
    
    ; Wrong MSX version: print error message and exit :(
    LD   HL,WRONGVER
    CALL PRINT
    RET
    
INSTALL:
    ; Reallocate the multi text screen routine to high RAM address
    LD   HL,MULTITXT        ; The start address of code block to transfer
    LD   DE,HKYEAHDL        ; The address of HKEYA hook handler
    LD   BC,CODESIZE        ; The total code size (see multitxt.asm)
    LDIR
    
    DI                      ; Avoid interruptions while changing hooks
    
    ; Redirect the HKYEA hook
    LD   HL,NEWHKYEA        ; The new HKYEA hook jump code
    LD   DE,HKYEA           ; Address of HKYEA hook
    LD   BC,5               ; Transfer 5 bytes (number of bytes for each hook)
    LDIR
    
    ; Redirect the HSCRE hook
    LD   DE,HSCRE           ; Address of HSCRE hook
    LD   BC,5
    LDIR                    ; HL already points to the correct location
    
    ; Redirect the HSCRE hook
    LD   DE,HWIDT           ; Address of HWIDT hook
    LD   BC,5
    LDIR
    
    EI
    
    ; Copy current page attributes to PAGEATRB table in RAM
    LD   DE,PAGEATRB
    SCF
    CALL COPYATRB
    
    ; Reset some atributes to clear the pages when accessed for the first time
    XOR  A
    LD   (PAGEATRB),A       ; Mark page as "invalid" (CSRY set to 0)
    LD   A,' '
    LD   (PAGEATRB+2),A     ; Set char at cursor position (CURSAV)
    LD   A,(LINL40)
    LD   (PAGEATRB+7),A     ; Set stored LINLEN value to default screen 0 width
    
    ; Clear the LINTTB table of first page attributes entry 
    LD   B,LINTTBSZ
    LD   HL,PAGEATRB+ATTBNSAV
NEXT:
    LD   (HL),B
    INC  HL
    DJNZ NEXT
    
    ; Initializes all page attributes
    LD   HL,PAGEATRB
    LD   DE,PAGEATRB+PGATBSIZ
    LD   BC,(MAXPAGES-1)*PGATBSIZ
    LDIR
    
    ; Print start app message and exit :P
    LD   HL,STARTMSG
    CALL PRINT
    RET
    
; ------------------------------------------------------------------------------
; Function..: Print text on screen using CHPUT bios routine
; Input.....: HL: address of start of string in memory
;             Note: the string must be zero terminated.
; Output....: String text to screen
; Changes...: AF, HL
; ------------------------------------------------------------------------------
PRINT:
    LD   A,(HL)             ; Get the char at address HL position
    AND  A                  ; Test end of string, return if zero
    RET  Z
    
    CALL CHPUT              ; Call CHPUT MSX bios routine
    INC  HL                 ; Next char position
    JR   PRINT
    
    ;; **** New hooks entries
    
    ; **** new HKYEA handler (five bytes for each hook)
NEWHKYEA:
    JP   HKYEAHDL           ; Address of routine (three bytes)
    NOP                     ; Using JP rather than CALL to save a little CPU
    NOP
    
    ; **** new HSCRE handler
NEWHSCRE:
    JP   HSCREHDL
    NOP
    NOP
    
    ; **** new HWIDT handler
NEWHWIDT:
    JP   HWIDTHDL
    NOP
    NOP
    
    ; The multi screen 0 routine (and its auxiliary methods)
    ; Note that the multitxt.asm file sets it's own org directive
MULTITXT:
    include "multitxt.asm"
    
END     equ $-1