
; ----------------------------------------------------------------------------
L6369:  .byte	$00
L636A:	.byte	$C4                             ; 636A C4                       .
L636B:	.byte	$02                             ; 636B 02                       .
L636C:  .byte	$00
L636D:  .byte	$00

; ----------------------------------------------------------------------------
sub_636E:
	prolog
	mv	L6369, L46EF
	shladdm8 off_AE, L46F5, L6369 
	ldp16	L636C
	add16i	$A2, L636C, $0003
	rdldi	$A4, $0005
	ldy     $A2                             ; 63AF A4 A2                    ..
	ldxa	L636A
	jsr     blockmove
	add16i	off_AE, L636C, $0002
	ldp8	CHBAS
	rts                                     ; 63D0 60                       `

