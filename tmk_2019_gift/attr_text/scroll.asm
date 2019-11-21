    DEVICE ZXSPECTRUM128
    
    
background_color equ #3f
foreground_color equ #00
begin_attr_line equ #5800 + (32 * 3)  
font equ 15616 - 256 
font_width equ 7
    
    org #8000
start:
    
loop:
    ei:halt:di
    call print_text
    jp loop
    
; -------------------------------------------------------
print_text:
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

    cp font_width; <--------------------------- можно менять ширишу шрифта
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
    byte "Hello world!   1234...    AAA!!!   ",0
text_ptr:
    dw text
text_shift:
    db 0
symbol_store:
    .8 db 0
    
    
    savesna "scroll.sna",start
    LABELSLIST "user.l"