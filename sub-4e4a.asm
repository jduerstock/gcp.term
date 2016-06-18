
; ----------------------------------------------------------------------------
L4E42:	.byte	$20
L4E43:	.byte	$20
L4E44:	.byte	$20
L4E45:	.byte	$20
L4E46:	.byte	$20
L4E47:	.byte	$52
L4E48:	.byte	$45
L4E49:	.byte	$54

; ----------------------------------------------------------------------------
sub_4E4A:  
	prolog
	stxa	L4E42
	blkmv_imi L4E46, L4E42, $0004
	sub8m	off_AE, L4E48, L4E46
	add8i	off_AC, off_AE, $01
	sub8m	off_AE, L4E49, L4E47
	add8i	$AA, off_AE, $01
	ldi	$85, $00
	mv	$84, $AA
	lda     $AC                             ; 4E92 A5 AC                    ..
	ldx     #$00                            ; 4E94 A2 00                    ..
	jsr     MultI
	st2xa	L4E44
	rdmv	$A0, L4E44
	rts                                     ; 4EAA 60                       `

