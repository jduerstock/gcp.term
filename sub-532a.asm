
; ----------------------------------------------------------------------------
L531D:	.byte	$FF                             ; 531D FF                       .
L531E:  .byte	$00
L531F:  .byte	$65
L5320:	.byte	$00
	.byte	$00
L5322:	.addr	L5320
L5324:	.byte	$D6,$1E
	.byte	$05,$00
	.byte	$78
L5329:	.byte	$32                             ; 5329 32                       2

; ----------------------------------------------------------------------------
sub_532A:
;--	void sub_532a(uint8_t a,x)
	prolog
	stxa	L531E
	dmv	off_AE, L5322
	stp8	L531F
	mv	L5329, L474C
	sty     L464B                           ; 534A 8C 4B 46                 .KF
	mv	$A3, L5322+1
	rdldi	$A4, L5324
	ldy     L5322                           ; 535A AC 22 53                 ."S
	ldx     L531E                           ; 535D AE 1E 53                 ..S
	lda     #$01                            ; 5360 A9 01                    ..
	jsr     sub_51F7
	mv	L474C, L5329
	ldxai	L5324
	jsr     sub_5274
	rts                                     ; 5372 60                       `

