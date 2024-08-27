;
; A screen timer (remake) from a famous late Soviet Union TV show
;
; https://en.wikipedia.org/wiki/600_Seconds
;
; code : nikhotmsk
; music & gfx : Cj-Splinter
; target demoparty : cc 2024
; group : undefined admins
;
; 2024.08.16 19:54 gmt+3
;


; use this command to compile this thing:
; sjasmplus 600.asm --lst=600.lst --sym=600.sym
;
; and this to run:
; fuse -m 128 600_seconds.tap
;

DOTS_MAX:						EQU 175
stored_paging_byte_address:		EQU $5b5c

	DEVICE ZXSPECTRUM128

	org $8000
	
start_demo:
	ld a, 0
	out (254), a ; border black
	
	di
	ld hl, machine_stack_end
	ld sp, hl
	
	; find the page that was active at boot and place it to player_page
	ld a, (stored_paging_byte_address) ; which is $5b5c (23388)
	and 0b00000111
	ld (player_page), a
	
	; init memory
	ld hl, code_end
	ld d, 0
	ld bc, variables_end - code_end
	call fill_mem
	
	ld bc, DOTS_MAX
	ld hl, point_beyond_screen_0
	ld ix, screen_0_array
	ld de, 3
init_arrays_loop1:
	ld (ix+0), hl ; all unused points will point outside of screen
	add ix, de
	dec bc
	ld a, b
	or c
	jr nz, init_arrays_loop1
	
	ld bc, DOTS_MAX
	ld hl, point_beyond_screen_1
	ld ix, screen_1_array ; same for shadow screen
	ld de, 3
init_arrays_loop2:
	ld (ix+0), hl
	add ix, de
	dec bc
	ld a, b
	or c
	jr nz, init_arrays_loop2
	
	jp start_demo_continued
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interrupt routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org #8080 ; interrupt routine here
	push af
	ld a, (clock_counter_5)
	inc a
	ld (clock_counter_5), a
	ld a, (nested)
	or a
	jp nz, interrupt_quit_nested
	;ld a, 1
	;out (254), a ; border blue
	push hl
	push de
	push bc
	push ix
	push iy
	
	ld a, 1
	ld (nested), a
	ei
	
	ld a, (screen_selector)
	or a
	jr nz, screen_selector_do_1
	; selector is 0
	ld a, 8 ; display shadow screen, write to primary screen
	ld ix, modified_code_1
	ld hl, point_beyond_screen_0
	ld (ix+1), hl
	ld ix, modified_code_2
	ld hl, 0x5800
	ld (ix+1), hl
	jr screen_selector_done
screen_selector_do_1:
	; selector is 1
	ld a, 0 ; display primary screen, write to shadow screen
	ld ix, modified_code_1
	ld hl, point_beyond_screen_1
	ld (ix+1), hl
	ld ix, modified_code_2
	ld hl, 0xd800
	ld (ix+1), hl
screen_selector_done:
	ld (flip_mask), a

	; ld a, (flip_mask)
	ld l, a
	ld a, 7 ; select page 7
	;and 0b00000111
	or l
	or 16
	ld bc, #7ffd
	out (c),a ; switch page and shadow screen
	; this is interrupt code, no need to store the page byte
	
	ld a, (clock_changed_draw)
	or a
	;jp z, interrupt_no_draw ; this is for 48k compatibility
	ld a, 0 ; todo figure out screen selector
	ld (clock_changed_draw), a
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; interrupt erase array (replace by background)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ld a, (screen_selector)
	or a
	jr nz, erase_array_s_1
	; selector is 0
	ld ix, screen_0_array + (DOTS_MAX * 3)
	ld de, screen_0_background + DOTS_MAX
	ld hl, screen_0_drawing_ok
	jr erase_array_s_done
erase_array_s_1:
	; selector is 1
	ld ix, screen_1_array + (DOTS_MAX * 3)
	ld de, screen_1_background + DOTS_MAX
	ld hl, screen_1_drawing_ok
erase_array_s_done:
	ld bc, DOTS_MAX
	ld a, (hl)
	or a
	call nz, drawing_code_reverse
	
	;ld a, 2
	;out (254), a ; border red
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; scrapper
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ld hl, (scrapper_timer)
	dec hl
	ld a, h
	or l
	jr nz, scrapper_timer_skip_1
	ld hl, 800 ; timer set here
scrapper_timer_skip_1:
	ld (scrapper_timer), hl
	
	ld de, 80
	or a ; clear carry
	sbc hl, de
	add hl, de
	jr nc, scrapper_skip_altogether
	
	ld b, 100
scrapper_loop:
	ld hl, (scrapper_pos)
	ld a, 0
	ld (hl), a ; scrap part of screen
	set 7, h
	ld (hl), a ; same for shadow screen
	res 7, h
	ld de, 119
	add hl, de
	ld de, 0x5800 ; end of drawable area
	or a ; drop carry
	sbc hl, de
	add hl, de
	jr c, scrapper_skip_1
	ld de, 0x1800
	sub hl, de
scrapper_skip_1:
	ld (scrapper_pos), hl
	dec b
	jr nz, scrapper_loop
	
scrapper_skip_altogether:

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; draw color lines
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Draw Line routine
	; B = Y pixel position 1
	; C = X pixel position 1
	; D = Y pixel position 2
	; E = X pixel position 2
	
	ld hl, (draw_color_lines_vector)
	ex hl, de
	ld hl, (draw_color_lines_running_point)
	ld bc, hl
	ld a, h
	add d
	ld h, a
	; vertical overflow detection
	and 0b11000000
	cp 0b11000000
	jr nz, draw_color_lines_vertical_overflow_as_is
	call interrupt_new_vector
	jr draw_color_lines_new_vector_ready
draw_color_lines_vertical_overflow_as_is:
	ld a, l
	add e
	ld l, a
	; horisontal overflow detection
	jr c, draw_color_lines_horizontal_carry_set
	; carry flag not set
	bit 7, e
	call nz, interrupt_new_vector
	jr draw_color_lines_horizontal_carry_set_ready
draw_color_lines_horizontal_carry_set:
	; carry flag set
	bit 7, e
	call z, interrupt_new_vector
	
draw_color_lines_horizontal_carry_set_ready:
draw_color_lines_new_vector_ready:
	ld (draw_color_lines_running_point), hl
	ex hl, de
	call Draw_Line
	

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; apply color_queue
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ld b, 32
	ld de, 3
	ld ix, color_queue
apply_color_queue_loop:
	
	ld hl, (ix+0)
	ld a, h
	or a
	jr z, apply_color_queue_next ; this entry is inactive
	ld a, (screen_selector)
	or a
	jr z, apply_color_queue_screen_0
	bit 7, h
	jr z, apply_color_queue_next ; leave the entry in queue for next time
	ld a, (ix+2)
	ld (hl), a ; write color
	ld hl, 0
	ld (ix+0), hl ; mark entry as empty
	jr apply_color_queue_next
apply_color_queue_screen_0:
	bit 7, h
	jr nz, apply_color_queue_next ; leave the entry in queue for next time
	ld a, (ix+2)
	ld (hl), a ; write color
	ld hl, 0
	ld (ix+0), hl ; mark entry as empty
apply_color_queue_next:
	add ix, de
	dec b
	jr nz, apply_color_queue_loop
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; interrupt load clock
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	ld a, (rotat_x_increment)
	bit 7, a
	jr nz, load_clock_rotat_x_neg
	; increment is positive
	ld a, (rotat_x_angle)
	cp 19 ; size of rotat matrix here
	jr c, load_clock_rotat_x_as_is
	ld a, (rotat_x_increment)
	neg
	ld (rotat_x_increment), a
	jr load_clock_rotat_x_as_is
load_clock_rotat_x_neg:
	; increment is negative
	ld a, (rotat_x_angle)
	or a ; detect null
	jr nz, load_clock_rotat_x_as_is
	;ld a, (rotat_x_increment)
	;neg
	ld a, 0
	ld (rotat_x_increment), a
	
load_clock_rotat_x_as_is:
	ld a, (rotat_x_angle)
	ld d, a
	ld a, (rotat_x_increment)
	add d
	ld (rotat_x_angle), a ; apply rotation step
	
	ld bc, DOTS_MAX
	
	ld hl, rotat_x
	ld a, (rotat_x_angle)
	ld e, a
	ld d, 0
	or a ; clear carry
	rl e
	rl d
	rl e
	rl d
	rl e
	rl d
	rl e
	rl d
	rl e
	rl d
	add hl, de
	ld (rotat_x_selected), hl
	ld hl, rotat_y
	ld (rotat_y_selected), hl
	
	ld hl, screen_0_array ; [hl] [enable]
	exx
	ld hl, clock_points_array ; [hl]
	exx
	ld a, (screen_selector)
	or a
	jr z, load_clock_s_0
	ld hl, screen_1_array
load_clock_s_0:
	
	; hl - screen array
	; shadow hl - clock points
	
interrupt_load_clock_loop:
	exx ; switch shadow registers
	ld a, (hl) ; just check if point is enabled
	exx
	or a
	jr nz, interrupt_load_clock_dot_enabled
modified_code_1:
	ld de, 0 ; point_beyond_screen_0 or point_beyond_screen_1
	ld (hl), de
	jr interrupt_load_clock_dot_disabled
	
interrupt_load_clock_dot_enabled:
	ld ix, (rotat_x_selected)
	ld iy, (rotat_y_selected)
	exx
	ld a, (hl) ; read LSB (horisontal pos)
	ld d, 0
	ld e, a
	add ix, de
	inc hl
	ld a, (hl) ; read vertical pos
	ld e, a
	add iy, de
	add iy, de ; yes, two times
	dec hl
	exx
modified_code_2:
	ld de, 0x5800 ; or 0xd800
	ld a, e
	add a, (ix+0)
	add a, (iy+0)
	ld e, a
	ld a, d
	adc a, (iy+1)
	ld d, a
	ld (hl), de
	; attr pointer should be ready now
interrupt_load_clock_dot_disabled:
	inc hl
	inc hl
	inc hl
	exx
	inc hl
	inc hl
	exx
	dec bc
	ld a, b
	or c
	jr nz, interrupt_load_clock_loop
	
	;ld ix, screen_0_array
	;ld hl, 0x5803
	;ld (ix), hl
	;ld (ix+2), 1
	
	;ld a, 4
	;out (254), a ; border green
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; interrupt draw array
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ld a, (screen_selector)
	or a
	jr nz, draw_array_s_1
	; selector is 0
	ld ix, screen_0_array
	ld de, screen_0_background
	jr draw_array_s_done
draw_array_s_1:
	; selector is 1
	ld ix, screen_1_array
	ld de, screen_1_background
draw_array_s_done:
	ld bc, DOTS_MAX
	call drawing_code
	
	ld a, (screen_selector)
	or a
	jr nz, ok_mark_s_1
	; selector is 0
	ld hl, screen_0_drawing_ok
	jr ok_mark_s_done
ok_mark_s_1:
	; selector is 1
	ld hl, screen_1_drawing_ok
ok_mark_s_done:
	ld a, 1
	ld (hl), a
	
interrupt_no_draw:
	
	ld a, (screen_selector)
	xor 0b00000001
	ld (screen_selector), a
	
	di
	ld a, 0
	ld (nested), a
	
	ld a, (flip_mask)
	ld l, a
	ld a, (stored_paging_byte_address)
	and 0b11110111
	or l
	or 16 ; basic48
	ld bc, #7ffd
	out (c),a ; restore page as it was before interrupt
	
	pop iy
	pop ix
	pop bc
	pop de
	pop hl
	;ld a, 0
	;out (254), a ; border black again
interrupt_quit_nested:
	pop af
	ei
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interrupt table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ALIGN 256 ; hardcoded interrupt table here
im2_interrupt_block:
	.257 BYTE #80 ; size 257 bytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interrupt_new_vector
; returns hl - running point without changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
interrupt_new_vector:
	push bc
interrupt_new_vector_try_again:
	call patrik_rak_cmwc_rnd
	and 0b00011100
	jr z, interrupt_new_vector_try_again
	push af
interrupt_new_vector_try_again_2:
	call patrik_rak_cmwc_rnd
	and 0b00011100
	jr z, interrupt_new_vector_try_again_2
	ld l, a ; horisontal ready
	pop af
	ld h, a ; vertical ready
	add l
	cp 25 ; minimal speed
	jr c, interrupt_new_vector_try_again
	
	ld (draw_color_lines_vector), hl
	
	; set vector towards center of the screen
	ld a, (draw_color_lines_running_point + 1) ; vertical
	ld hl, draw_color_lines_vector + 1
	xor (hl) ; vertical vector
	and 0b10000000
	jr z, interrupt_new_vector_vertical_as_is
	ld a, (draw_color_lines_vector + 1)
	neg
	ld (draw_color_lines_vector + 1), a
interrupt_new_vector_vertical_as_is:
	
	ld a, (draw_color_lines_running_point + 0) ; horizontal
	ld hl, draw_color_lines_vector + 0 ; horizontal vector
	xor (hl)
	and 0b10000000
	jr z, interrupt_new_vector_horizontal_as_is
	ld a, (draw_color_lines_vector + 0)
	neg
	ld (draw_color_lines_vector + 0), a
interrupt_new_vector_try_again_3:
	call patrik_rak_cmwc_rnd
	and 0b00000111
	jr z, interrupt_new_vector_try_again_3
	ld (color), a
interrupt_new_vector_horizontal_as_is:
	pop bc
	ld hl, (draw_color_lines_running_point)
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rnd generator by Patrik Rak
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCLUDE "rnd.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start_demo_continued
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start_demo_continued:
	
	ld hl, 0x5800
	ld d, 0b01000111
	ld bc, 768 ; size of attr
	call fill_mem ; attributes black white
	
	ld hl, 0x4000 ; clear screen memory
	ld d, 0
	ld bc, 0x1800 ; size of screen
	call fill_mem
	
	ld a, (flip_mask)
	ld a, 0
	ld l, a
	ld a, 7 ; select page 7
	and 0b00000111
	or l
	or 16
	ld (stored_paging_byte_address), a
	ld bc, #7ffd
	out (c),a
	
	ld hl, 0xc000 ; clear screen memory (shadow screen this time)
	ld d, 0
	ld bc, 0x1800 ; size of screen
	call fill_mem
	
	ld hl, 0xd800
	ld d, 0b01000111
	ld bc, 768 ; size of attr
	call fill_mem ; attributes black white (on shadow screen)
	
	ld d, 0b01110110 ; fill background buffer with yellow (both of them)
	ld hl, screen_0_background
	ld bc, screen_background_end - screen_0_background
	call fill_mem
	
	di
	ld hl, im2_interrupt_block ; prepare interrupt mode 2
	ld a, h
	ld i, a
	im 2
	ei
	
	ld a, 1
	ld (rotat_x_increment), a
	
main_loop:
	halt
main_loop_counter_loop:
	ld a, (clock_counter_5) ; this demo is 50 hz
	; so clock steps one time each 5 frames
	cp 5
	jr c, main_loop_counter_skip
	sub 5
	ld (clock_counter_5), a
	ld a, 1
	ld (clock_changed), a
	call decrement_bcd
	jr main_loop_counter_loop
main_loop_counter_skip:
	
	ld hl, (bcd_3)
	ld a, 0
	or h
	or l
	ld hl, (bcd_1)
	or h
	or l
	jr nz, main_loop_not_elapsed
	ld a, (zero_timer)
	dec a
	ld (zero_timer), a
	jr nz, main_loop_not_elapsed
	; clear screen here and display end message
	di
	ld a, 0
	ld (flip_mask), a
	ld hl, 0x4000 ; clear screen memory
	ld d, 0
	ld bc, 0x1800 ; size of screen
	call fill_mem
	ld a, (flip_mask)
	ld a, 0
	ld l, a
	ld a, 7 ; select page 7
	and 0b00000111
	or l
	or 16
	ld (stored_paging_byte_address), a
	ld bc, #7ffd
	out (c),a
	
	ld hl, 0x5800
	ld d, 0b01000111
	ld bc, 768 ; size of attr
	call fill_mem ; attributes black white
	
	ld hl, svg_vectors
	ld (polyline_pointer), hl
	
	; run drawing procedure from here
busy_loop:
	call fischinger_draw_polyline
	jr busy_loop
	
main_loop_not_elapsed:
	
	ld a, (clock_disabled)
	or a
	jr nz, print_clock_skip
	; load fonts and print clock
	ld a, (clock_changed)
	or a
	jr z, print_clock_skip
	ld ix, clock_points_array
	ld hl, bcd_3
	ld c, 0
	call print_digit
	ld hl, bcd_2
	ld c, 8
	call print_digit
	ld hl, bcd_1
	ld c, 16
	call print_digit
	ld hl, bcd_0
	ld c, 25
	call print_digit
	
	ld d, 12
	ld e, 24
	ld (ix), de ; add also decimal point
	inc ix
	inc ix
	
	ld de, ix
	ld hl, clock_points_array_end
	or a ; drop carry
	sbc hl, de
	or a ; drop carry
	rr h
	rr l ; division by 2
	ld bc, hl
	ld hl, 0x0000
print_clock_terminator_loop:
	ld (ix+0), hl ; mark dot as disabled
	inc ix
	inc ix
	dec bc
	ld a, b
	or c
	jr nz, print_clock_terminator_loop
	
	ld a, 0
	ld (clock_changed), a
	ld a, 1
	ld (clock_changed_draw), a
print_clock_skip:
	; the interrupt routine will display it later

	jp main_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; print_digit
; c - position
; hl - bcd value pointer
; ix - running pointer of points array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_digit:
	ld a, (hl)
	ld d, a
	rl a
	rl a
	rl a
	add d
	ld hl, fonts
	add l
	ld l, a ; hl should now point to the right font
	ld b, 9 ; hight of the font here
	ld d, 4 ; vertical pos of the clock here
print_digit_loop:
	ld e, c
	ld a, (hl) ; load row from font
	
	inc e
	
	bit 6, a ; test bit 6
	jr z, print_digit_bit6_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit6_skip:
	inc e
	bit 5, a ; test bit 5
	jr z, print_digit_bit5_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit5_skip:
	inc e
	bit 4, a ; test bit 4
	jr z, print_digit_bit4_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit4_skip:
	inc e
	bit 3, a ; test bit 3
	jr z, print_digit_bit3_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit3_skip:
	inc e
	bit 2, a ; test bit 2
	jr z, print_digit_bit2_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit2_skip:
	inc e
	bit 1, a ; test bit 1
	jr z, print_digit_bit1_skip
	ld (ix), de ; [de] points array
	inc ix
	inc ix
print_digit_bit1_skip:
	;inc e
	
	inc hl
	inc d
	dec b
	jr nz, print_digit_loop
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fill memory with constant byte
; D  - byte
; HL - start address
; BC - size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_mem:
  ld A, D
  ld (HL), A
  inc HL
  dec BC
  ld A,B
  or C
  jr nz, fill_mem
  ret
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; drawing code
; ix - pointer to attr struct [hl] [enable]
; de - pointer to background buffer
; bc - array members
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawing_code:
	;ld a, (ix+2)
	;or a
	;jr z, drawing_code_skip
	ld hl, (ix+0)
	ld a, (de)
	ex af, af ; swap register set
	ld a, (hl) ; read from screen
	ld (de), a ; write to buffer
	ex af, af
	ld (hl), a ; write to screen
drawing_code_skip:
	inc de
	inc ix
	inc ix
	inc ix
	dec bc
	ld a, b
	or c
	jr nz, drawing_code
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; drawing code reverse
; ix - pointer to last attr struct [hl] [enable]
; de - pointer to last background buffer
; bc - array members
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawing_code_reverse:
	dec de ; the same thing but backwards
	dec ix
	dec ix
	dec ix
	;ld a, (ix+2)
	;or a
	;jr z, drawing_code_reverse_skip
	ld hl, (ix+0)
	ld a, (de)
	ex af, af ; swap register set
	ld a, (hl)
	ld (de), a
	ex af, af
	ld (hl), a
drawing_code_reverse_skip:
	dec bc
	ld a, b
	or c
	jr nz, drawing_code_reverse
	ret

;
; this function decrements the bcd_values until 0,0,0,0
;
decrement_bcd:
  ld hl, bcd_3 + 3
  ld a, (hl)
  dec a
  ld (hl), a
  and 0b10000000 ; check bcd underflow
  ret z
  ld a, 9
  ld (hl), a
  dec hl
  ld a, (hl)
  dec a
  ld (hl), a
  and 0b10000000
  ret z
  ld a, 9
  ld (hl), a
  dec hl
  ld a, (hl)
  dec a
  ld (hl), a
  and 0b10000000
  ret z
  ld a, 9
  ld (hl), a
  dec hl
  ld a, (hl)
  dec a
  ld (hl), a
  and 0b10000000
  ret z
  ld a, 0
  ld (hl), a
  inc hl
  ld (hl), a
  inc hl
  ld (hl), a
  inc hl
  ld (hl), a
  ; timer elapsed stop here
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vector.asm -- drawing routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCLUDE "vector.asm"
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scanline fill by John Metcalf
; call with d=x-coord, e=y-coord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set end marker

fill:
  ld l,255
  push hl

; calculate bit position of pixel

nextrun:
  ld a,d
  and 7
  inc a
  ld b,a
  ld a,1
bitpos:
  rrca
  djnz bitpos
  ld c,b
  ld b,a

; move left until hitting a set pixel or the screen edge

seekleft:
  ld a,d
  or a
  jr z,goright
  dec d
  rlc b
  call scrpos
  jr nz,seekleft

; move right until hitting a set pixel or the screen edge,
; setting pixels as we go. Check rows above and below and
; save their coordinates to fill later if necessary

seekright:  
  rrc b
  inc d
  jr z,rightedge
goright:
  call scrpos
  jr z,rightedge
  ld (hl),a
  inc e
  call checkadj
  dec e
  dec e
  call checkadj
  inc e
  jr seekright

; check to see if there's another row waiting to be filled

rightedge:
  pop de
  ld a,e
  inc a
  jr nz,nextrun
  ret  

; calculate the pixel address and whether or not it's set

scrpos:
  ld a,e
  and 248
  rra
  scf
  rra
  rra
  ld l,a
  xor e
  and 248
  xor e
  ld h,a
  ld a,l
  xor d
  and 7
  xor d
  rrca
  rrca
  rrca
  ld l,a
  ld a,b
  or (hl)
  cp (hl)
  ret

; check and save the coordinates of an adjacent row

checkadj:
  sla c
  ld a,e
  cp 192
  ret nc
  call scrpos+1
  ret z
  inc c
  bit 2,c
  ret nz
  pop hl
  push de
  jp (hl)
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fischinger_draw_polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fischinger_draw_polyline:
polyline_speed:
	ld b, 2 ; speed of drawing
draw_polyline_cycle: ; draw logo
	;call interrupt_page_set_player_page
	
	ld hl, (polyline_pointer)
	ld ix, hl
	ld a, (ix+0)
	or a ; check zero
	jr z, no_draw_polyline
	ld a, (ix+1)

	or a ; check zero
	jr z, no_draw_polyline
	ld a, (ix+2)
	ld e, a
	or a
	jr z, no_draw_polyline
	ld a, (ix+3)
	ld d, a
	or a
	jr z, no_draw_polyline
	
	push bc
	ld c, (ix+0)
	ld b, (ix+1)
	;
	; bc = Ya Xa
	; de = Yb Xb
	;
	; b = Ya c = Xa
	; d = Yb e = Xb
	;
	; call interrupt_page_set_7
	call Draw_Line
	; call interrupt_page_set_player_page
	pop bc
	
	ld hl, (polyline_pointer)
	inc hl
	inc hl
	inc hl
	inc hl
	ld (polyline_pointer), hl
	
	dec b
	jr nz, draw_polyline_cycle
no_draw_polyline:
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end vector graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCLUDE "message.asm"

;;;;;;;;;;;;;;;;;;;;;;;;
; fonts
;;;;;;;;;;;;;;;;;;;;;;;;
	ALIGN 128
fonts:
  DEFB %00111100 	; char30
  DEFB %01111110
  DEFB %01100110
  DEFB %01100110
  DEFB %01100110
  DEFB %01100110
  DEFB %01100110
  DEFB %01111110
  DEFB %00111100
  
  DEFB %00011000 	; char31
  DEFB %00011000
  DEFB %00111000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  
  DEFB %00011100 	; char32
  DEFB %00111110
  DEFB %00110110
  DEFB %00000110
  DEFB %00001100
  DEFB %00011000
  DEFB %00110000
  DEFB %00111110
  DEFB %00111110
  
  DEFB %00111100 	; char33
  DEFB %01111110
  DEFB %01100110
  DEFB %00000110
  DEFB %00011000
  DEFB %00000110
  DEFB %01100110
  DEFB %01111110
  DEFB %00111100
  
  DEFB %00001100 	; char34
  DEFB %00001100
  DEFB %00011000
  DEFB %00110000
  DEFB %01100000
  DEFB %01101100
  DEFB %01111110
  DEFB %00001100
  DEFB %00001100
  
  DEFB %00111100 	; char35
  DEFB %00111100
  DEFB %00100000
  DEFB %00111100
  DEFB %00000110
  DEFB %00000110
  DEFB %01100110
  DEFB %01111110
  DEFB %00111100
  
  DEFB %00011100 	; char36
  DEFB %00111100
  DEFB %01100000
  DEFB %01111100
  DEFB %01111110
  DEFB %01100110
  DEFB %01100110
  DEFB %01111110
  DEFB %00111100
  
  DEFB %01111110 	; char37
  DEFB %00000110
  DEFB %00001100
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  DEFB %00011000
  
  DEFB %00111100 	; char38
  DEFB %01111110
  DEFB %01100110
  DEFB %01100110
  DEFB %00111100
  DEFB %01100110
  DEFB %01100110
  DEFB %01111110
  DEFB %01111110
  
  DEFB %00111100 	; char39
  DEFB %01111110
  DEFB %01100110
  DEFB %01100110
  DEFB %01111110
  DEFB %00111110
  DEFB %00000110
  DEFB %01111110
  DEFB %01111000


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hardcoded rotation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
rotat_x:
	BYTE 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
	BYTE 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 25, 26, 27, 28, 29
	BYTE 4, 5, 5, 6, 7, 8, 9, 9, 10, 11, 12, 13, 13, 14, 15, 16, 16, 16, 17, 18, 19, 19, 20, 21, 22, 23, 23, 24, 25, 26, 27, 27
	BYTE 5, 6, 7, 7, 8, 9, 10, 10, 11, 12, 12, 13, 14, 14, 15, 16, 16, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 24, 25, 25, 26
	BYTE 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14, 14, 15, 15, 16, 16, 16, 17, 17, 18, 18, 19, 20, 20, 21, 21, 22, 23, 23, 24, 24
	BYTE 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 16, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23
	BYTE 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 15, 16, 16, 16, 16, 16, 17, 17, 17, 18, 18, 19, 19, 19, 20, 20, 21, 21, 21
	BYTE 12, 12, 12, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20
	BYTE 13, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18
	BYTE 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17
	BYTE 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16
	BYTE 17, 17, 17, 17, 17, 17, 17, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 15, 15, 15, 15, 15, 15
	BYTE 19, 19, 18, 18, 18, 18, 18, 17, 17, 17, 17, 17, 16, 16, 16, 16, 16, 16, 16, 16, 16, 15, 15, 15, 15, 15, 14, 14, 14, 14, 14, 13
	BYTE 20, 20, 20, 19, 19, 19, 19, 18, 18, 18, 17, 17, 17, 16, 16, 16, 16, 16, 16, 16, 15, 15, 15, 14, 14, 14, 13, 13, 13, 13, 12, 12
	BYTE 22, 22, 21, 21, 20, 20, 20, 19, 19, 18, 18, 18, 17, 17, 16, 16, 16, 16, 16, 15, 15, 14, 14, 14, 13, 13, 12, 12, 12, 11, 11, 10
	BYTE 24, 23, 23, 22, 22, 21, 21, 20, 20, 19, 19, 18, 18, 17, 17, 16, 16, 16, 15, 15, 14, 14, 13, 13, 12, 12, 11, 11, 10, 10, 9, 9
	BYTE 25, 25, 24, 23, 23, 22, 22, 21, 20, 20, 19, 19, 18, 17, 17, 16, 16, 16, 15, 15, 14, 13, 13, 12, 12, 11, 10, 10, 9, 9, 8, 7
	BYTE 27, 26, 25, 25, 24, 23, 23, 22, 21, 20, 20, 19, 18, 18, 17, 16, 16, 16, 15, 14, 14, 13, 12, 12, 11, 10, 9, 9, 8, 7, 7, 6
	BYTE 28, 28, 27, 26, 25, 24, 24, 23, 22, 21, 20, 20, 19, 18, 17, 16, 16, 16, 15, 14, 13, 12, 12, 11, 10, 9, 8, 8, 7, 6, 5, 4
	BYTE 30, 29, 28, 27, 26, 25, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 16, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 7, 6, 5, 4, 3

rotat_y:
	WORD 0x00, 0x20, 0x40, 0x60, 0x80, 0xa0, 0xc0, 0xe0, 0x100, 0x120, 0x140, 0x160, 0x180, 0x1a0, 0x1c0, 0x1e0, 0x200, 0x220, 0x240, 0x260, 0x280, 0x2a0, 0x2c0, 0x2e0
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; initialized variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nested: BYTE 0
clock_counter_5: BYTE 0
clock_changed: BYTE 1
clock_changed_draw: BYTE 0
bcd_3: BYTE 6
bcd_2: BYTE 0
bcd_1: BYTE 0
bcd_0: BYTE 0
draw_color_lines_running_point: WORD 0x20c0
paging_available: BYTE 1
draw_color_lines_vector: WORD 0x0ff1
color: BYTE 4 ; green
scrapper_pos: WORD 0x4000
scrapper_timer: WORD 800
clock_disabled: BYTE 0
zero_timer: BYTE 155
polyline_pointer: WORD 0 ; should be empty svg
code_end:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
screen_0_array: ; [hl] [enable]
	.216 BYTE 0, 0, 0
screen_1_array:
	.216 BYTE 0, 0, 0
screen_0_background:
	.216 BYTE 0
screen_1_background:
	.216 BYTE 0
screen_background_end:
clock_points_array:
	.216 BYTE 0, 0 ; [de] d = vertical, e = horisontal
	; if d == 0 then disabled
clock_points_array_end:
screen_selector: BYTE 0
point_beyond_screen_0: BYTE 0
point_beyond_screen_1: BYTE 0
screen_0_drawing_ok: BYTE 0
screen_1_drawing_ok: BYTE 0
player_page: BYTE 0
flip_mask: BYTE 0
rotat_x_selected: WORD 0
rotat_y_selected: WORD 0
rotat_x_angle: BYTE 0
rotat_y_angle: BYTE 0
rotat_x_increment: BYTE 0
color_queue:
	.32 BYTE 0, 0, 0
color_queue_i: BYTE 0
variables_end:
machine_stack:
	.128 BYTE 0
machine_stack_end:

	savesna "600_seconds.sna", start_demo

	org $4000
basic_loader:
	db $00,$0a,$15,$00,$f5 ; PRINT
	db $22,$63,$75,$73,$74,$6f,$6d,$20,$44,$49,$59,$20,$6c,$6f,$61,$64  ; "custom DIY load
	db $65,$72,$22,$0d,$00,$14,$07,$00,$20,$ef,$22,$22,$20,$af,$0d,$00  ;
	db $1e,$0f,$00,$20,$f9,$c0,$33,$32,$37,$36,$38,$0e,$00,$00,$00,$80  ; ... ..32768.....
	db $00,$0d,$80,$0d,$80,$80,$00,$00,$00,$00,$00,$36,$38,$0e,$00,$00  ; ...........68...
basic_loader_end:
	EMPTYTAP "600_seconds.tap"
	SAVETAP "600_seconds.tap",BASIC,"diy_loader",basic_loader,basic_loader_end - basic_loader, 10
	SAVETAP "600_seconds.tap",CODE,"usr32768",start_demo,code_end - start_demo,start_demo
	SAVEBIN "600_seconds.bin",start_demo,code_end - start_demo
