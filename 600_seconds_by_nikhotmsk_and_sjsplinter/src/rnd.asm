;Patrik Rak

patrik_rak_cmwc_rnd:
  ld  hl,.table
.idx:   ld  bc,0       ; i
  add  hl,bc
  ld  a,c
  inc  a
  and  7
  ld  (.idx+1),a  ; i = ( i + 1 ) & 7
  ld  c,(hl)    ; y = q[i]
  ex  de,hl
  ld  h,c    ; t = 256 * y
  ld  l,b
  sbc  hl,bc    ; t = 255 * y
  sbc  hl,bc    ; t = 254 * y
  sbc  hl,bc    ; t = 253 * y
.car:   ld  c,0    ; c
  add  hl,bc    ; t = 253 * y + c
  ld  a,h    ; c = t / 256
  ld  (.car+1),a
  ld  a,l    ; x = t % 256
  cpl      ; x = (b-1) - x = -x - 1 = ~x + 1 - 1 = ~x
  ld  (de),a
  ret

.table    db   82,97,120,111,102,116,20,12