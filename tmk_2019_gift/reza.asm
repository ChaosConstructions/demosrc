	device zxspectrum128
partyversion equ 1
	org	#6000
scr 	incbin "tmkift4.$c"
ends	



	savebin "ea.bin",scr+17,ends-scr-17

