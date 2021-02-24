    device zxspectrum128
    
ink_black           equ %00000000
ink_blue            equ %00000001
ink_red             equ %00000010
ink_magenta         equ %00000011
ink_green           equ %00000100
ink_cyan            equ %00000101
ink_yellow          equ %00000110
ink_white           equ %00000111

paper_black           equ %00000000
paper_blue            equ %00001000
paper_red             equ %00010000
paper_magenta         equ %00011000
paper_green           equ %00100000
paper_cyan            equ %00101000
paper_yellow          equ %00110000
paper_white           equ %00111000

attr_bright           equ %01000000
attr_flash            equ %10000000

disp equ #4000
attr equ #5800
disp2 equ #C000
attr2 equ #D800

buffer equ #9800
buffer_crop equ buffer + 32*5

    ; адрес на который компилировать
    org #8000  

begin_main_file:
    ;  шахматка
    ld a,#55
    ld hl,disp
    ld de,disp+1
    ld b,192/8
.cloop:
    push bc
    cpl
    ld bc,32*8
    ld (hl),a
    ldir
    pop bc
    djnz .cloop

    ; координаты
    ld hl,aaa
    ld (hl), 29
    ld de,aaa + 1
    ld bc,22*6
    ldir
    
    ei
.loop:
    halt
    ld hl,buffer_crop
    ld de,attr
    ld bc,768
    ldir

    xor a
    ld hl,buffer_crop
    ld de,buffer_crop + 1
    ld bc,768 - 1
    ld (hl),a
    ldir

    ld ix,aaa
    ld b,22
.aloop:
    push bc
    ;ld hl,buffer
    ; -----------------

    ld b,(ix+1); y
    ld c,(ix+2); y
    ld h,0;s
    ld l,(ix+3);s
    add hl,bc

    ;
    ld a,28
    cp h
    jr nc,.skip_regen
    call random_elite
    or 48
    ld hl,0
    ld (ix+3),a
.reg_me:
    call random_elite
    and 31
    cp 28
    jr nc,.reg_me
    ld (ix+0),a
.skip_regen:

    ld (ix+1),h; y
    ld (ix+2),l; y

    ld b,(ix+0);x
    inc ix;x
    inc ix;y
    inc ix;y
    inc ix;sy
    ;ld c,h
    ; y
    ld l,h
    ld h,0; 0
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ; x
    ld a,l
    add b
    ld l,a
    ; add buffer
    ld bc,buffer
    add hl,bc
    ; -----------------
    ld de,amiga_sprite

    ;
    ld bc,32-5
    dup 5
    call .draw
    add hl,bc
    edup
    ;rra
    and #10
    out (#FE),a
    pop bc
    djnz .aloop


    jp .loop

.draw:
    dup 5
    ld a,(de)
    or (hl)
    ld (hl),a
    inc e
    inc hl
    edup
    ret

a1 equ ink_red or paper_red or attr_bright
a2 equ ink_white or paper_white or attr_bright
a6 equ ink_red or paper_red
a4 equ ink_white or attr_bright
a5 equ ink_white or paper_white
a3 equ ink_red or attr_bright
a7 equ ink_white 
amiga_sprite:
    db  0, a1, a2, a3,  0 
    db a1, a2, a1, a4, a6 
    db a2, a1, a5, a3, a7 
    db a3, a4, a3, a7, a6 
    db  0, a6, a7, a6,  0 

; генерация случайного числа 0..255 -> A
random_elite:
    ld a,(.store)
    ld d,a
    ld a,(.store+1)
    ld (.store),a
    add a,d
    ld d,a
    ld a,(.store+2)
    ld (.store+1),a
    add a,d
    rlca
    ld (.store+2),a
    ret
.store equ begin_main_file
    db '@','e','r','r','o','r'
    ;db 0,42,109

end_main_file:

aaa:
    .32 db 0

    savehob "blit22.$C", "blit22.C", begin_main_file, end_main_file - begin_main_file
    savebin "blit22.bin",begin_main_file, end_main_file - begin_main_file
    
    ; выводим размер банарника
    display "Code size: ", /d, end_main_file - begin_main_file
    
    ; выводим end
    display "End byte: ", end_main_file
    
    ; сохраняем sna(снапшот состояния) файл
    savesna "blit22.sna", begin_main_file
    
    ; сохраняем метки
    labelslist "user.l"