
; ----------------------------------------------------------------------------
sub_4B97:  
	sta     off_AE
	stx     off_AE+1
L4B9B:  dey                                     ; 4B9B 88                       .
	bmi     L4BA2                           ; 4B9C 30 04                    0.
	lda     (off_AE),y
	beq     L4B9B                           ; 4BA0 F0 F9                    ..
L4BA2:  iny                                     ; 4BA2 C8                       .
	tya                                     ; 4BA3 98                       .
	sta     $A0                             ; 4BA4 85 A0                    ..
	rts                                     ; 4BA6 60                       `

