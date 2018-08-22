
; ----------------------------------------------------------------------------
L9E0F:	.byte	$00
L9E10:  .byte	$00
L9E11:  .byte	$00
L9E12:  .byte	$00
L9E13:  .byte	$00
L9E14:  .byte	$00
L9E15:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9E19:  .byte	$00
L9E1A:  .byte	$00
L9E1B:  .byte	$00
L9E1C:  .byte	$00
L9E1D:  .byte	$00
L9E1E:  .byte	$00
L9E1F:  .byte	$00
L9E20:  .byte	$00
L9E21:  .byte	$00
L9E22:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9E28:	.byte	$00
	.byte	$00
L9E2A:  .byte	$00
L9E2B:  .byte	$00

cmd_2b:						; "+" "C"
	prolog
	sta     L9E0F                           ; 9E2F 8D 0F 9E                 ...
	ldi	L9E19, $80
	ldy     #$00                            ; 9E37 A0 00                    ..
	sty     L9E1A                           ; 9E39 8C 1A 9E                 ...
	sty     L9E1B                           ; 9E3C 8C 1B 9E                 ...
	sty     L9E1C                           ; 9E3F 8C 1C 9E                 ...
	func16_8 sub_7035, L9E10, L46E9
	blkmv_imi L9E1D, L9E10, $0005
	ifm8eqi	L46E6, $02, L9E94
L9E73:
	sec                                     ; 9E73 38                       8
	lda     #$00                            ; 9E74 A9 00                    ..
	sbc     L9E1D                           ; 9E76 ED 1D 9E                 ...
	sta     L9E19                           ; 9E79 8D 19 9E                 ...
	;
	sec                                     ; 9E7C 38                       8
	lda     #$00                            ; 9E7D A9 00                    ..
	sbc     L9E1E                           ; 9E7F ED 1E 9E                 ...
	sta     L9E1A                           ; 9E82 8D 1A 9E                 ...
	;
	dmv	L9E1B, L9E1F
	jmp     L9F69                           ; 9E91 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9E94:	ifm8eqi	L46E6, $04, L9ED7
	ldx     L46EA                           ; 9E9E AE EA 46                 ..F
	lda     L46E9                           ; 9EA1 AD E9 46                 ..F
	jsr     sub_799B
	rdmv	L9E14, $A0
	blkmv_imi L9E28, L9E14, $0004
	dmv	L9E1B, L9E2A
	jmp     L9F69                           ; 9ED4 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9ED7:	ifm8eqi	L46E6, $03, L9F69
	dmv	L9E19, L4751
	ifm8eqi	L474F, $02, L9F69
	func16_8 sub_7035, L9E10, L4750
	blkmv_imi L9E1D, L9E10, $0005
	func16_8 sub_65B0, L9E12, L9E21
	blkmv_imi L9E22, L9E12, $0006
	sub8m	off_AE, L9E1D, L9E1F
	sub8m	L9E1B, L4751, off_AE
	sub8m	off_AE, L9E1E, L9E20
	sub8m	L9E1C, L4752, off_AE
L9F69:  lda     L9E19                           ; 9F69 AD 19 9E                 ...
	eor     #$80                            ; 9F6C 49 80                    I.
	bne     L9F74                           ; 9F6E D0 04                    ..
	ora     #$00                            ; 9F70 09 00                    ..
	eor     #$FF                            ; 9F72 49 FF                    I.
L9F74:  beq     L9F79                           ; 9F74 F0 03                    ..
	jmp     L9FB7                           ; 9F76 4C B7 9F                 L..

; ----------------------------------------------------------------------------
L9F79:  jmp     L9F82                           ; 9F79 4C 82 9F                 L..

; ----------------------------------------------------------------------------
L9F7C:	.byte	$05,"cBBBB"

; ----------------------------------------------------------------------------
L9F82:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9E1B
	ldi	$A7, $00
	mv	$A6, L9E1C
	ldi	$A9, $00
	mv	$A8, L9E19
	ldi	$AB, $00
	mv	$AA, L9E1A
	ldy     L9E0F                           ; 9FAA AC 0F 9E                 ...
	ldxai	L9F7C
	jsr     sub_55A0
	jmp     L9FDE                           ; 9FB4 4C DE 9F                 L..

; ----------------------------------------------------------------------------
L9FB7:  jmp     L9FBE                           ; 9FB7 4C BE 9F                 L..

; ----------------------------------------------------------------------------
L9FBA:	.byte	$03,"cBB"

; ----------------------------------------------------------------------------
L9FBE:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9E1B
	ldi	$A7, $00
	mv	$A6, L9E1C
	ldy     L9E0F                           ; 9FD4 AC 0F 9E                 ...
	ldxai	L9FBA
	jsr     sub_55A0
L9FDE:  ldi	$A0, $01
	rts                                     ; 9FE2 60                       `

