
; ----------------------------------------------------------------------------
L924C:  .byte	$00
L924D:  .byte	$00
L924E:  .byte	$00
L924F:  .byte	$00
L9250:  .byte	$00
L9251:  .byte	$00
L9252:  .byte	$00
L9253:  .byte	$00
L9254:  .byte	$00
L9255:  .byte	$00
L9256:  .byte	$00
L9257:  .byte	$00
L9258:  .byte	$00
	.byte	$00
	.byte	$00
L925B:  .byte	$00
L925C:  .byte	$00

; ----------------------------------------------------------------------------
sub_925D:  
	prolog
	stxa	L924C
	func16_8 sub_65B0, L924E, L924C
	blkmv_imi L9257, L924E, $0006
	sub8i	$A1, L9258, $01
	ldx     $A1                             ; 9295 A6 A1                    ..
	lda     L924C                           ; 9297 AD 4C 92                 .L.
	jsr     sub_90CE
	lda     $A0                             ; 929D A5 A0                    ..
	sta     L9250                           ; 929F 8D 50 92                 .P.
	lda     L924D                           ; 92A2 AD 4D 92                 .M.
	cmp     L9258                           ; 92A5 CD 58 92                 .X.
	lbcs	L9352
	lda     L9250                           ; 92AD AD 50 92                 .P.
	lbne	L9352
	add8i	off_AE, L924D, $01
	shladdm8 off_AC, L925B, off_AE
	ldy     #$01                            ; 92D0 A0 01                    ..
	lda     ($AC),y                         ; 92D2 B1 AC                    ..
	sta     L9254                           ; 92D4 8D 54 92                 .T.
	dey                                     ; 92D7 88                       .
	lda     ($AC),y                         ; 92D8 B1 AC                    ..
	sta     L9253                           ; 92DA 8D 53 92                 .S.
	shladdm8 off_AE, L925B, L924D
	iny                                     ; 92F1 C8                       .
	lda     (off_AE),y
	sta     L9252                           ; 92F4 8D 52 92                 .R.
	dey                                     ; 92F7 88                       .
	lda     (off_AE),y
	sta     L9251                           ; 92FA 8D 51 92                 .Q.
	sec                                     ; 92FD 38                       8
	lda     L9258                           ; 92FE AD 58 92                 .X.
	sbc     L924D                           ; 9301 ED 4D 92                 .M.
	sta     $AE                             ; 9304 85 AE                    ..
	sub8i	off_AC, off_AE, $01
	ldi	$85, $00
	mv	$84, L9257
	lda     $AC                             ; 9316 A5 AC                    ..
	ldx     #$00                            ; 9318 A2 00                    ..
	jsr     MultI
	st2xa	L9255
	lda     L9252                           ; 9324 AD 52 92                 .R.
	sta     $A3                             ; 9327 85 A3                    ..
	rdmv	$A4, L9255
	ldy     L9251                           ; 9333 AC 51 92                 .Q.
	ldxa	L9253
	jsr     sub_4EB1
	ldi	$A3, $00
	ldy     L9257                           ; 9343 AC 57 92                 .W.
	ldxa	L9251
	jsr     bzero
	jmp     L935C                           ; 934F 4C 5C 93                 L\.

; ----------------------------------------------------------------------------
L9352:	proc8i	cmd_2a, $11
	ldi	$A0, $00
	rts                                     ; 935B 60                       `

; ----------------------------------------------------------------------------
L935C:	ldi	$A0, $01
	rts                                     ; 9360 60                       `

