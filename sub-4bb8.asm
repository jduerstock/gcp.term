
; ----------------------------------------------------------------------------
sub_4BB8:  
	lda     $13                             ; 4BB8 A5 13                    ..
	ldy     $14                             ; 4BBA A4 14                    ..
	cmp     $13                             ; 4BBC C5 13                    ..
	beq     L4BC4                           ; 4BBE F0 04                    ..
	lda     $13                             ; 4BC0 A5 13                    ..
	ldy     #$00                            ; 4BC2 A0 00                    ..
L4BC4:  sta     $A1                             ; 4BC4 85 A1                    ..
	sty     $A0                             ; 4BC6 84 A0                    ..
	rts                                     ; 4BC8 60                       `

