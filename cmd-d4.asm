
; ----------------------------------------------------------------------------
LA3A5:  .byte	$00
LA3A6:  .byte	$00
LA3A7:  .byte	$00
LA3A8:  .byte	$00
LA3A9:  .byte	$00
LA3AA:  .byte	$00
LA3AB:  .byte	$00
LA3AC:  .byte	$00,$00
LA3AE:  .byte	$00
LA3AF:  .byte	$00
LA3B0:  .byte	$00
LA3B1:  .byte	$00
LA3B2:  .byte	$00
LA3B3:  .byte	$00
LA3B4:  .byte	$00
LA3B5:  .byte	$00
LA3B6:  .byte	$00
LA3B7:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA3BB:  .byte	$00
LA3BC:  .byte	$00

cmd_d4:						; "4" "BDDR"
	stack_prolog LA3A5, $04
	func16_8 sub_65B0, LA3AC, LA3A5
	test16	LA3AC
	lbne	LA3E2
	rts                                     ; A3E1 60                       `

; ----------------------------------------------------------------------------
LA3E2:  jsr     cmd_d0
	blkmv_imi LA3B3, LA3A8, $0004
	sub8m	off_AE, LA3B5, LA3B3
	add8i	LA3AE, off_AE, $01
	blkmv_imi LA3B7, LA3AC, $0006
	shladdm8 off_AE, LA3BB, LA3B4
	clc                                     ; A438 18                       .
	ldy     #$00                            ; A439 A0 00                    ..
	lda     (off_AE),y
	adc     LA3B3                           ; A43D 6D B3 A3                 m..
	sta     $AC                             ; A440 85 AC                    ..
	iny                                     ; A442 C8                       .
	lda     (off_AE),y
	adc     #$00                            ; A445 69 00                    i.
	sta     $AD                             ; A447 85 AD                    ..
	sub16i	LA3AA, off_AC, $0001
	mv	LA3B1, LA3AE
	mv	LA3AF, LA3B4
	mv	LA475, LA3B6
LA46A:  lda     LA475                           ; A46A AD 75 A4                 .u.
	cmp     LA3AF                           ; A46D CD AF A3                 ...
	bcs     LA476                           ; A470 B0 04                    ..
	jmp     LA52A                           ; A472 4C 2A A5                 L*.

; ----------------------------------------------------------------------------
LA475:  .byte	$00

; ----------------------------------------------------------------------------
LA476:  lda     LA3A7                           ; A476 AD A7 A3                 ...
	lbeq	LA49C
	add16i	$A0, LA3AA, $0001
	ldy     LA3AE                           ; A48D AC AE A3                 ...
	ldxa	$A0
	jsr     sub_4B97
	mv	LA3B1, $A0
LA49C:	yldi	LA3B0, $01
	mv	LA4B2, LA3B1
LA4A7:  lda     LA4B2                           ; A4A7 AD B2 A4                 ...
	cmp     LA3B0                           ; A4AA CD B0 A3                 ...
	bcs     LA4B3                           ; A4AD B0 04                    ..
	jmp     LA4F7                           ; A4AF 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4B2:  .byte	$00

; ----------------------------------------------------------------------------
LA4B3:	add16m8	off_AE, LA3AA, LA3B0
	ldp8	LA3B2
	lda     LA3A7                           ; A4CA AD A7 A3                 ...
	lbeq	LA4DD
	func8_8	sub_4BC9, LA3B2, LA3B2
LA4DD:  ldx     LA3B2                           ; A4DD AE B2 A3                 ...
	lda     LA3A6                           ; A4E0 AD A6 A3                 ...
	jsr     PutD
	lda     L464D                           ; A4E6 AD 4D 46                 .MF
	lbeq	LA4F1
LA4EE:  jmp     LA4F7                           ; A4EE 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4F1:  inc     LA3B0                           ; A4F1 EE B0 A3                 ...
	jmp     LA4A7                           ; A4F4 4C A7 A4                 L..

; ----------------------------------------------------------------------------
LA4F7:  lda     LA3A7                           ; A4F7 AD A7 A3                 ...
	lbeq	LA507
LA4FF:  ldx     #$9B                            ; A4FF A2 9B                    ..
	lda     LA3A6                           ; A501 AD A6 A3                 ...
	jsr     PutD
LA507:  lda     L464D                           ; A507 AD 4D 46                 .MF
	lbeq	LA512
	jmp     LA52A                           ; A50F 4C 2A A5                 L*.

; ----------------------------------------------------------------------------
LA512:	add16m8 LA3AA, LA3AA, LA3B7
	inc     LA3AF                           ; A524 EE AF A3                 ...
	jmp     LA46A                           ; A527 4C 6A A4                 Lj.

; ----------------------------------------------------------------------------
LA52A:  jsr     cmd_d1
	rts                                     ; A52D 60                       `

