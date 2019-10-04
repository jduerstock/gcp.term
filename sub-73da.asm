
; ----------------------------------------------------------------------------
L73D3:	.byte	$EF,$A9
L73D5:  .byte	$00
L73D6:	.byte	$9D
L73D7:	.byte	$9D
L73D8:	.byte	$F0
L73D9:	.byte	$AD

; ----------------------------------------------------------------------------
sub_73DA:
	prolog
	stxa	L73D3
	ldy     #$00                            ; 73E3 A0 00                    ..
	sty     L73D7                           ; 73E5 8C D7 73                 ..s
	iny                                     ; 73E8 C8                       .
	sty     L73D6                           ; 73E9 8C D6 73                 ..s
	mv	L73FD, L4673
L73F2:  lda     L73FD                           ; 73F2 AD FD 73                 ..s
	cmp     L73D6                           ; 73F5 CD D6 73                 ..s
	bcs     L73FE                           ; 73F8 B0 04                    ..
	jmp     L7467                           ; 73FA 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L73FD:  .byte	$08

; ----------------------------------------------------------------------------
L73FE:	sub8m	off_AE, L4673, L73D6
	ldx     off_AE
	lda     L4659,x                         ; 7409 BD 59 46                 .YF
	sta     L73D5                           ; 740C 8D D5 73                 ..s
	func16_8 sub_7035, L73D8, L73D5
	add16i	off_AE, L73D8, $000C
	ldy     #$00                            ; 742E A0 00                    ..
	lda     (off_AE),y
	cmp     #$FF                            ; 7432 C9 FF                    ..
	lbcs	L7461
	lda     L73D8                           ; 7439 AD D8 73                 ..s
	ora     L73D9                           ; 743C 0D D9 73                 ..s
	lbeq	L7461
L7444:  ldy     L73D5                           ; 7444 AC D5 73                 ..s
	ldxa	L73D3
	jsr     sub_7368
	ifm8eqi	$A0, $01, L7461
	yldi	L73D7, $01
	jmp     L7467                           ; 745E 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L7461:  inc     L73D6                           ; 7461 EE D6 73                 ..s
	jmp     L73F2                           ; 7464 4C F2 73                 L.s

; ----------------------------------------------------------------------------
L7467:  lda     L73D7                           ; 7467 AD D7 73                 ..s
	eor     #$01                            ; 746A 49 01                    I.
	lbne	L7477
	mv	$A0, L73D5
	rts                                     ; 7476 60                       `

; ----------------------------------------------------------------------------
L7477:  ldi	$A0, $FF
	rts                                     ; 747B 60                       `

