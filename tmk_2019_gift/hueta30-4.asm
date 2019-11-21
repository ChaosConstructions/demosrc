		DEVICE	ZXSPECTRUM128
	       ORG     26000
STARTUT:
;----------------------------------------------------
    ;di:halt
		ld	a,#10
        call PAGER
        call lamer_effect_init




		ld	a,#10
		call	PAGER

		call	IM2INI
		ld	a,#10
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
    ld b,20;5
w1 
    halt
    push bc
    call cls
        ld      hl,qblogo
        ld      de,#4000
        call    DEC40    

 ;   halt
    pop bc
    djnz w1         
; -----------------------------------------------------------------------------
  
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



        ;di:halt


glagne
		
;------------I
        halt



        call sprite

     
        
        call cls
    
        call lamer_effect       
        
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

           LD      HL,#4800
           LD      IX,null;#c800
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
             LD      A,6
                LD      BC,65533
                OUT     (C),A
                IN      A,(C)
                CP      12
                RET     C

hat_change1 ld hl,hat_table
            inc hl
            inc hl
            ld a,(hl)            
            cp #ff
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
            dw #ffff             


curr_hat    dw hats+512

sprite ;- spr   
        ld      de,(curr_hat)  
        ld      hl,#4000+2048+2
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

;--------------------------------------------------


lamer_effect_init:
    ld de,#4000
    ld hl,#e000; <----------------------------- free mem?????
    ;    to #e800
    call fast_point_table_init
    ld ix,#e800;<----------------------------- free mem?????
    ld d,40
    call gen_sin
    ret

lamer_effect:
    ld a,(vivert_sleep)
    or a
    jr z,.run
    dec a
    ld (vivert_sleep),a
    ret

.run:

    ld a,(var_new)
    dec a
    or a
    jr nz,.skip_new
    
    call random_elite
    ld (.vivert_1+1),a
    
    call random_elite
    ld (.vivert_2+1),a

    call random_elite
    ld (.vivert_3+1),a
    
    ld a,80
.skip_new:
    ld (var_new),a

    ld a,(var_a)
    add 2;6
    ld (var_a),a
    ld ixl,a
    
    ld a,(var_b)
    add 5;9
    ld (var_b),a
    ld ixh,a
    
    ld a,(var_c)
    add 3;2
    ld (var_c),a
    ld c,a

    ld hl,p_addr
    ld b,80
    xor a
.clear_loop:
    ld d,(hl)
    inc hl
    ld e,(hl)
    inc hl
    ld (de),a
    djnz .clear_loop

    ld iy,p_addr
    ld b,80
.draw_loop:
    ld a,ixl
.vivert_1:
    add 2
    ld ixl,a
    
    ld a,ixh
.vivert_2:
    add 5
    ld ixh,a
    
    ld a,c
.vivert_3:
    add 9
    ld c,a
    
    push bc
    ; c
    ld b,#e8
    ld a,(bc)
    ;ld l,a
    
    ; d
    add ixl
    ld c,a
    ;ld c,ixl
    ld a,(bc)
    add 100
    ld d,a
    
    ; e
    ld c,ixh
    ld a,(bc)
    add 180
    ld e,a
    
    pop bc
    
    /*
    ld h,(iy)
    ld l,(iy+1)
    ld a,(iy+2)
    xor (hl)
    ld (hl),a
    */
    
    ; e-x d-y
    ld h,high #e000;7
    ; y
    ld l,d;4
    ld d,(hl);7
    inc h;4
    ; x
    ld a,(hl);7 sss
    inc h;4
    ld l,e;4
    or (hl);7 ñìåùåíèå â áàéòàõ
    ld e,a;4
    ;
    inc h;4
    ;
    ;ld a,(hl)
    ;ld (iy+2),a
    
    ld a,(de);7
    or (hl);7
    ld (de),a;7
    ld (iy),d
    ld (iy+1),e

    
    inc iy
    inc iy
    ;inc iy
    
    djnz .draw_loop



    ret
var_a:
    db 0
var_b:
    db 0
var_c:
    db 0
var_new:
    db 80
vivert_sleep:
    db 57
p_addr:
    .260 dw 0
    
random_elite:
    LD A,(random_store)
    LD D,A
    LD A,(random_store+1)
    LD (random_store),A
    ADD A,D
    LD D,A
    LD A,(random_store+2)
    LD (random_store+1),A
    ADD A,D
    RLCA
    LD (random_store+2),A
    
    and 7
    inc a
    inc a    
    ;di:halt
    
    RET

random_store:
    db 33,19,220 

    ; ix - table, align 256
    ; d - amp
gen_sin
    ld e,#bc
    LD HL,0    ;K*SIN
GENSIN0
    LD (IX),D
    LD A,D
    RLA
    SBC A,A
    LD B,A,C,D
    ADC HL,BC ;tweaking
    RR C
    RRCA
    RR C
    ADD HL,BC
    ex DE,HL
    LD B,D,A,E
    DUP 3
    SRA B
    RRA
    EDUP
    LD C,A
       ;OR A
    SBC HL,BC
    ex DE,HL
    INC LX
    jr nz,GENSIN0 
    ret

; de - screen addr/align(256) (d) 
; hl - table addr/align(256) (h)
fast_point_table_init:  
    ; ãåíåðàöèÿ òàáëèöû ñòàðøåãî áàéòà àäðåñà ïî Y
    ; çàïîëíÿåò 192(256) áàéòîâ 3 ãðóïïàìè ïî 8*8
    ; %ddd00000,%ddd00001,%ddd00010,%ddd00011,%ddd00100,%ddd00101,%ddd00110,%ddd00111
    ; ...
    ; %ddd01000,%ddd01001,%ddd01010,%ddd01011,%ddd01100,%ddd01101,%ddd01110,%ddd01111
    ; ...
    ; %ddd10000,%ddd10001,%ddd10010,%ddd10011,%ddd10100,%ddd10101,%ddd10110,%ddd10111
    ld c,#00
.loop4:
    ld b,64
.loop44:
    ld a,l; ñìåùåíèå âíóòðè çíàêîìåñòà
    and #07
    or d; íà÷àëî àäðåñà
    or c; íîìåð òðåòè
    ld (hl),a
    inc hl
    djnz .loop44
    ld a,c
    add #08
    ld c,a
    cp #18
    jr nz,.loop4  
    ; ïðîïóñêàåì 64 áàéòà
    ;ld l,0
    ;inc h
    ; ëîâóøêà, ïåðåíàïðàâëÿþùàÿ âûâîä â ïçó, ïðè ïîïûòêå íàðèñîâàòü çà ýêðàíîì
    ld b,64
    ld a,0
.hook4:
    ld (hl),a
    inc hl
    djnz .hook4
    
    ; ãåíåðàöèÿ òàáëèöû ñìåùåíèÿ çíàêîìåñòà â òðåòè ïî Y
    ; çàïîëíÿåò 192(256) áàéòîâ 3 ãðóïïàìè ïî 8*8
    ; #00,#00,#00,#00,#00,#00,#00,#00, 
    ; #20,#20,#20,#20,#20,#20,#20,#20,
    ; #40,#40,#40,#40,#40,#40,#40,#40,
    ; ...
    ; #E0,#E0,#E0,#E0,#E0,#E0,#E0,#E0,
    ; #00,#00,#00,#00,#00,#00,#00,#00, 
    ; ...
    ; ëîãèêà ïîâòîðåíèé â òîì ÷òî ïî inc(ìëàäøèé áàéò) ìîæíî áóäåò èçâëå÷ü àòðèááóòàõ â ðÿäó ïî y
    ld a,0; òåêóùåå çíà÷åíèå
    ex af,af
    ld a,8*3; ñêîëüêî ãðóïï ïî 8 áàéòîâ
.loop3:
    ex af,af
    ld b,8
.loop33:
    ld (hl),a
    inc hl
    djnz .loop33
    add #20
    ex af,af
    dec a
    and a
    jr nz,.loop3
    ; ïðîïóñêàåì 64 áàéòà
    ;ld l,0
    ;inc h
    ; ëîâóøêà, ïåðåíàïðàâëÿþùàÿ âûâîä â ïçó, ïðè ïîïûòêå íàðèñîâàòü çà ýêðàíîì
    ld b,64
    ld a,0
.hook3:
    ld (hl),a
    inc hl
    djnz .hook3

    ; ãåíåðàöèÿ òàáëèöû ñìåùåíèÿ áàéòà â ñòðîêå
    ; çàïîëíÿåò 256 áàéòîâ 32 íàðàñòàþùèìè ãðóïïàìè ïî 8 áàéò 
    ; 0,0,0,0,0,0,0,0, 1,1,1,1,1,1,1,1, 2,2,2,2,2,2,2,2...
    ; ëîãèêà ïîâòîðåíèé â òîì ÷òî ïî inc(ìëàäøèé áàéò) ìîæíî áóäåò èçâëå÷ü ñìåùåíèå
    ld a,0
.loop2:
    ld b,8
.loop22:
    ld (hl),a
    inc hl
    djnz .loop22
    inc a
    cp 32
    jr nz,.loop2    
    
    ; ãåíåðàöèÿ òàáëèöû ñìåùåíèÿ ïèêñåëà â áàéòå
    ; çàïîëíÿåò 256 áàéòîâ ïîâòîðÿþùèìñÿ ïàòòåðíîì 1,2,4,8,16,32,64,127,1,2..
.pixel_bit:
    ld a,208;208;192;208;128;128;136;208;192;128
    ld b,0
.loop1:
    ld (hl),a
    rrc a
    inc hl
    djnz .loop1
    
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
    align 256
font_file   incbin "font1.bin" 
background_color equ 56
foreground_color equ #00
begin_attr_line equ #5800 + (32 * 12)  
font equ font_file - 256 
font_width equ 8
   
    
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

    cp font_width; <--------------------------- ìîæíî ìåíÿòü øèðèøó øðèôòà
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
    byte "                                   "
    byte "Hello world! The Shapka Gift 2019 is here!!!11111one...    AAA!!!   "
    byte "        Diver's hats, EA's music and Organism's lame code with few hundreds "
    byte "of useful bytes by errorsoft wishes EVERYTHING to TmK!     Make demo, not virus!   "
    byte "        The next words is provided by Nuts:       "
    byte "Dear hedgehog-hat! Happy to congratulate you in time this time! "
    byte "This is the first time since we met at 1997, btw. During these years you did a lot of crazy things:"
    byte " magazines, games, demos, demo-parties at all. But, so far i just return from traumatology clinic,"
    byte " I do not want to wish you this common Z80 megaherzes, multicolor rasters and zx-evo-hardware sprites."
    byte " i wish you be healthy to digest a lot of green barrels of unhealthy drinks."
    byte " I wish you strong legs so that with them you can both get demoparties or river or forest - "
    byte "what yo want, and, return to home. And yes, i I wish you a strong home so that he can survive all the life shocks ;)"
    byte "                "        
    byte "words from errorsoft:          let it your effects do not fffflicker like the 'vivert' effect on the right)))"
    byte " And of course, good luck and new cool demos!"
    byte "                "        
    byte "and a Mooh said: Hi tmk, we congratulate you and wish you a strong health, stick to a healthy lifestyle"
    byte " and delight us with new demos."

    byte 0
text_ptr:
    dw text
text_shift:
    db 0
symbol_store:
    .8 db 0

; -----------------------------------------------------------------------------
        


qblogo		incbin	"eye-q.scr.mlz"	

	
;-----------------------------------

; -----------------------------------------------------------------------------



PAGER:          LD      (PAGE),A
                LD      BC,#7FFD
                OUT     (C),A
                RET
                ;-----        
                 

;; -----------------------------------------------------------------------------       

		align	256
gasilko  ds 256




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

                LD      A,#10
                CALL    PAGER

                call print_text

                ;di:halt


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

                org 	#c000,0
                incbin  "ea.bin"
                align 256
null ds 2048                
 
page0end        

FIN


			display finalbody
		SAVESNA	"hueta30-4.sna",STARTUT
		                savebin "main",26000,FIN-26000
				                display         "---------------------- Mainbody end:",/A,finalbody
                                display         "---------------------- page0 end:",/A,page0end
