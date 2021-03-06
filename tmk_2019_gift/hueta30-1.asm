		DEVICE	ZXSPECTRUM128
	       ORG     26000
STARTUT:
;----------------------------------------------------


		ld	a,#10
		call	PAGER

		call	IM2INI
		ld	a,#11
		call	PAGER
		call	#c000

		ei


		ld      a,#10
        call    PAGER

        ld hl,#5800
        ld de,#5801
        ld bc,767
        ld (hl),l
        halt
        xor a
        out (#fe),a
        ldir

        ld      hl,qblogo
        ld      de,#4000
        call    DEC40

; -----------------------------------------------------------------------------
    ld b,185
w1 
    halt
    halt
    djnz w1           
; -----------------------------------------------------------------------------
        ld      a,#10
        call    PAGER      

		LD	HL,#4000
		LD	DE,#4001
		LD	BC,#1800
		LD	(HL),l
		LDIR
		LD      B,#03
		LD      (HL),56
		LDIR
        ld a,7
        out (#fe),a
        xor a
        ld (print_text),a






glagne
		
;------------I
        halt
        call sprite
        call cls
        call hat_change

		jp glagne

; -----------------------------------------------------------------------------

cls        LD      A,(faza)
           CP      16
           CALL    Z,dermo
           LD      H,0
           LD      L,A
           ADD     HL,HL
           ADD     HL,HL
           ADD     HL,HL
           LD      DE,chunx
           ADD     HL,DE
           EX      DE,HL
           HALT
           LD      HL,#4000
           LD      IX,#c000
           CALL    clr
           LD      HL,#4800
           LD      IX,#c800
           CALL    clr
           LD      A,(faza)
           INC     A
           LD      (faza),A
           RET
dermo          XOR     A
           LD      (faza),A
           RET
faza           DB      0
clr        LD      C,8
           PUSH    DE
clr0           PUSH    BC

           DUP   256
           LD      A,(DE)
           CPL
           LD      B,A
           LD      A,(HL)
           AND     B
           LD      B,A
           LD      A,(DE)
           LD      C,A
           LD      A,(IX)
           AND     C
           OR      B
           LD      (HL),A
           INC     L
           INC     IX
           EDUP

           POP     BC
           INC     DE
           INC     H
           DEC     C
           JP      NZ,clr0
           POP     DE
           RET           

;--------------------------
hat_change
            ld  a,(ay)
            or  a
            ret z
                        out (#fe),a
hat_change1 ld hl,(hat_table)
            inc hl
            inc hl
            ld a,(hl)            
            or a
            jr nz,hat_change2
            ld hl,hat_table
hat_change2 ld (hat_change1+1),hl

            ld hl,(hat_change1+1)
            ld e,(hl)
            inc hl
            ld d,(hl)
            ld (curr_hat),DE
            ret


hat_table
            dw hats
            dw hats+512
            dw hats+512+512
            dw hats+512+512+512
            dw hats+512+512+512+512
            dw hats+512+512+512+512+512
            dw hats+512+512+512+512+512+512
            dw hats+512+512+512+512+512+512+512
            dw #0000                


curr_hat    dw hats

sprite ;- spr   
        ld      de,(curr_hat)  
        ld      hl,#4000+8+32+32+32+32+32+32+32
        ld      b,64
spr1    push    hl         
        dup     8
        ld      a,(de)
;        or      (hl)        
        ld      (hl),a
        inc     de
        inc     l
        edup
        pop     hl
        call    downhl
        djnz    spr1   

		ret

downde  inc d 
        ld a,d 
        and 7
        ret nz
        ld a,e 
        add a,32
        ld e,a
        ret c
        ld a,d 
        sub 8
        ld d,a
        ret    

downhl  inc h
        ld a,h
        and 7
        ret nz
        ld a,l
        add a,32
        ld l,a
        ret c
        ld a,h
        sub 8
        ld h,a
        ret    

; -----------------------------------------------------------------------------


;Z80 depacker for megalz V4 packed files   (C) fyrex^mhm

; DESCRIPTION:
;
; Depacker is fully relocatable, not self-modifying,
;it's length is 110 bytes starting from DEC40.
;Register usage: AF,AF',BC,DE,HL. Must be CALL'ed, return is done by RET.
;Provide extra stack location for store 2 bytes (1 word). Depacker does not
;disable or enable interrupts, as well as could be interrupted at any time
;(no f*cking wicked stack usage :).

; USAGE:
;
; - put depacker anywhere you want,
; - put starting address of packed block in HL,
; - put location where you want data to be depacked in DE,
;   (much like LDIR command, but without BC)
; - make CALL to depacker (DEC40).
; - enjoy! ;)

; PRECAUTIONS:
;
; Be very careful if packed and depacked blocks coincide somewhere in memory.
;Here are some advices:
;
; 1. put packed block to the highest addresses possible.
;     Best if last byte of packed block has address #FFFF.
;
; 2. Leave some gap between ends of packed and depacked block.
;     For example, last byte of depacked block at #FF00,
;     last byte of packed block at #FFFF.
;
; 3. Place nonpackable data to the end of block.
;
; 4. Always check whether depacking occurs OK and neither corrupts depacked data
;     nor hangs computer.
;

DEC40
        LD      A,#80
        EX      AF,AF'
MS      LDI
M0      LD      BC,#2FF
M1      EX      AF,AF'
M1X     ADD     A,A
        JR      NZ,M2
        LD      A,(HL)
        INC     HL
        RLA
M2      RL      C
        JR      NC,M1X
        EX      AF,AF'
        DJNZ    X2
        LD      A,2
        SRA     C
        JR      C,N1
        INC     A
        INC     C
        JR      Z,N2
        LD      BC,#33F
        JR      M1

X2      DJNZ    X3
        SRL     C
        JR      C,MS
        INC     B
        JR      M1
X6
        ADD     A,C
N2
        LD      BC,#4FF
        JR      M1
N1
        INC     C
        JR      NZ,M4
        EX      AF,AF'
        INC     B
N5      RR      C
        RET     C
        RL      B
        ADD     A,A
        JR      NZ,N6
        LD      A,(HL)
        INC     HL
        RLA
N6      JR      NC,N5
        EX      AF,AF'
        ADD     A,B
        LD      B,6
        JR      M1
X3
        DJNZ    X4
        LD      A,1
        JR      M3
X4      DJNZ    X5
        INC     C
        JR      NZ,M4
        LD      BC,#51F
        JR      M1
X5
        DJNZ    X6
        LD      B,C
M4      LD      C,(HL)
        INC     HL
M3      DEC     B
        PUSH    HL
        LD      L,C
        LD      H,B
        ADD     HL,DE
        LD      C,A
        LD      B,0
        LDIR
        POP     HL
        JR      M0
END_DEC40 
; -----------------------------------------------------------------------------
background_color equ 56
foreground_color equ #00
begin_attr_line equ #5800 + (32 * 3)  
font equ 15616 - 256 
font_width equ 7
    
; -------------------------------------------------------
print_text:
    ret
    ; shift
    ld de,begin_attr_line
    ld hl,begin_attr_line + 1
    ld b,8
.loop_shift:
    push bc
    .31 ldi
    pop bc
    inc hl
    inc de
    djnz .loop_shift
    
    
    ; get new symbol if need
    ld a,(text_shift)
    or a
    jr nz,.skip_get_symbol
    ld de,(text_ptr)
    ld h,a
    ld a,(de)
    ld l,a
    add hl,hl
    add hl,hl
    add hl,hl  
    ld de,font
    add hl,de
    ; copy
    ld de,symbol_store
    ld bc,8
    ldir
.skip_get_symbol


    ; print symbol
    ld hl,begin_attr_line + 31
    ld de,32
    ld ix,symbol_store
    ;
    ld b,8    
.print_loop:    
    ld c,(ix)
    ld a,c
    sla c
    ld (ix),c
    inc ix
    ;
    and 128
    jr nz,.print_1
    ld a,background_color
    jr .print_end
.print_1:
   ld a,foreground_color
.print_end:
    ld (hl),a
    ;
    add hl,de
    djnz .print_loop
    
    ; new symbol?
    ld a,(text_shift)
    inc a

    cp font_width; <--------------------------- ����� ������ ������ ������
    jr c,.skip_new_symbol
    ld hl,(text_ptr)
    inc hl
    ld a,(hl)
    or a
    jr nz,.skip_restart
    ld hl,text
.skip_restart:
    ld (text_ptr),hl
    xor a
.skip_new_symbol:
    ld (text_shift),a
    
    ret
    
text:
    byte "         Hello world!   1234...    AAA!!!   ",0
text_ptr:
    dw text
text_shift:
    db 0
symbol_store:
    .8 db 0
; -----------------------------------------------------------------------------
        


qblogo		incbin	"eye-q.scr.mlz"	

	
;-----------------------------------


PAGER:          LD      (PAGE),A
                LD      BC,#7FFD
                OUT     (C),A
                RET
                ;-----        
prc_ay      
                LD      A,6
                LD      BC,65533
                OUT     (C),A
                IN      A,(C)
numfl           CP      14
                LD      A,0
                ld      (ay),a
                RET     C
cvetomuz        LD      A,7
                ld      (ay),a
                RET
ay              db 0
                ;-------                    

;; -----------------------------------------------------------------------------       

		align	256



;font        incbin  "font_.bin"
hats        incbin  "hats.bin"    
chunx       incbin  "chunx.bin"    


finalbody




;INT                    
                ORG     #BE00
                DS      257,#BF

IM2INI          DI
                LD      A,#BE
                LD      I,A
                IM      2
                EI
                RET

PAGE    DB      #10

        ORG     #BFBF
        DI
        PUSH    HL,DE,BC,AF,IX,IY
        EXX
        EX      AF,AF'
        PUSH    HL,DE,BC,AF
        ;-------
                LD      A,(PAGE)
                LD      (INT1+1),A

                LD      A,#11
                CALL    PAGER

                call print_text

                call    prc_ay
             ; out     (#fe),a

             	CALL    #C005

INT1            LD      A,0
                CALL    PAGER
        ;-------
        POP     AF,BC,DE,HL
        EXX
        EX      AF,AF'
        POP     IY,IX,AF,BC,DE,HL
        EI
        RET
finalint        	

                org 	#c000,1
                incbin  "ea.bin"
 
page0end        

FIN


			display finalbody
		SAVESNA	"hueta30-1.sna",STARTUT
		                savebin "main",26000,FIN-26000
				                display         "---------------------- Mainbody end:",/A,finalbody
                                display         "---------------------- page0 end:",/A,page0end
