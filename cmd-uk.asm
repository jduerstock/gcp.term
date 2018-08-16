
; ----------------------------------------------------------------------------
L6982:  .byte	$00
L6983:	.byte	$00,$00
L6985:	.byte	$00,$00
L6987:  .byte	$00
L6988:  .byte	$00
L6989:  .byte	$00
L698A:	.byte	$00
L698B:  .byte	$00
L698C:  .byte	$00
L698D:  .byte	$00
L698E:  .byte	$00
L698F:  .byte	$00
L6990:  .byte	$00
L6991:  .byte	$00,$00
L6993:	.byte	$00,$00

; ----------------------------------------------------------------------------
cmd_uk:						; "K","B"
	prolog
	sta     L6982                           ; 6998 8D 82 69                 ..i
	func16_8 sub_65B0, L6983, L6982
	test16	L6983
	lbne	L69B7
	rts                                     ; 69B6 60                       `

; ----------------------------------------------------------------------------
L69B7:	blkmv_imi L698A, L6983, $000B
	dmv	off_AE, L698E
	ldp16	L6985
	lda     L698B                           ; 69E5 AD 8B 69                 ..i
	asl     a                               ; 69E8 0A                       .
	sta     $A2                             ; 69E9 85 A2                    ..
	lda     #$00                            ; 69EB A9 00                    ..
	sta     $A3                             ; 69ED 85 A3                    ..
	ldy     $A2                             ; 69EF A4 A2                    ..
	ldxa	L698E
	jsr     sub_619A
	lda     L698D                           ; 69FA AD 8D 69                 ..i
	sta     $A3                             ; 69FD 85 A3                    ..
	ldy     L698C                           ; 69FF AC 8C 69                 ..i
	ldxa	L6985
	jsr     sub_619A
	lda     #$00                            ; 6A0B A9 00                    ..
	sta     $A3                             ; 6A0D 85 A3                    ..
	ldy     #$0B                            ; 6A0F A0 0B                    ..
	ldxa	L6983
	jsr     sub_619A
	shladdm8 off_AE, L46E2, L6982
	lda     #$00                            ; 6A2E A9 00                    ..
	ldy     #$01                            ; 6A30 A0 01                    ..
	sta     (off_AE),y
	lda     #$00                            ; 6A34 A9 00                    ..
	dey                                     ; 6A36 88                       .
	sta     (off_AE),y
	test16	L6991
	lbeq	L6A67
	rdldi	$84, $0006
	lda     L6990                           ; 6A4C AD 90 69                 ..i
	ldx     #$00                            ; 6A4F A2 00                    ..
	jsr     MultI
	sta     L6987                           ; 6A54 8D 87 69                 ..i
	lda     #$00                            ; 6A57 A9 00                    ..
	sta     $A3                             ; 6A59 85 A3                    ..
	ldy     L6987                           ; 6A5B AC 87 69                 ..i
	ldxa	L6991
	jsr     sub_619A
L6A67:	test16	L6993
	lbeq	L6AB8
	dmv	off_AE, L6993
	ldp16	L6988
	dmv	off_AE, L6988
	clc                                     ; 6A93 18                       .
	lda     (off_AE),y
	adc     #$01                            ; 6A96 69 01                    i.
	sta     $A2                             ; 6A98 85 A2                    ..
	lda     #$00                            ; 6A9A A9 00                    ..
	sta     $A3                             ; 6A9C 85 A3                    ..
	ldy     $A2                             ; 6A9E A4 A2                    ..
	ldxa	L6988
	jsr     sub_619A
	ldi	$A3, $00
	ldy     #$1A                            ; 6AAD A0 1A                    ..
	ldxa	L6993
	jsr     sub_619A
L6AB8:	yldi	L4656, $01
	rts                                     ; 6ABD 60                       `

