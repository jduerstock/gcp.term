
; ----------------------------------------------------------------------------
sub_696A:  
	prolog
	lda     #$1E                            ; 696D A9 1E                    ..
	asl     a                               ; 696F 0A                       .
	sta     $A2                             ; 6970 85 A2                    ..
	ldi	$A3, $00
	ldy     $A2                             ; 6976 A4 A2                    ..
	ldxa	L46E2
	jsr     bzero
	rts                                     ; 6981 60                       `
