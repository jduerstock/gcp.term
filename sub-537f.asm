
; ----------------------------------------------------------------------------
sub_537F:  
	ldx     #$06                            ; 537F A2 06                    ..
	lda     LB1C8                           ; 5381 AD C8 B1                 ...
	jsr     sub_532A
	rts                                     ; 5387 60                       `
