
; ----------------------------------------------------------------------------
L6E40:	.byte	$78
L6E41:	.byte	$6C                             ; 6E41 6C                       l
L6E42:	.byte	$66                             ; 6E42 66                       f
L6E43:	.byte	$66                             ; 6E43 66                       f
L6E44:	.byte	$6C                             ; 6E44 6C                       l
L6E45:  .byte	$78
L6E46:  .byte	$00
L6E47:  .byte	$00
L6E48:	.byte	$7E                             ; 6E48 7E                       ~
L6E49:  .byte	$60
L6E4A:	.byte	$7C                             ; 6E4A 7C                       |
L6E4B:  .byte	$60
L6E4C:  .byte	$60
L6E4D:  .addr	$7E
L6E4F:	.byte   $03,"CBS"
L6E53:	.addr	L6E4F
L6E55:	.byte	$03,"CBs"
L6E59:	.addr	L6E55
L6E5B:  ror     $3E66                           ; 6E5B 6E 66 3E                 nf>
	.byte	$00
L6E5F:  .byte	$00
L6E60:	.byte	$66                             ; 6E60 66                       f

; ----------------------------------------------------------------------------
cmd_ub:  					; "B" "BDBBBB"
	stack_prolog L6E40, $05
	func16_8 sub_65B0, L6E46, L6E40
	blkmv_imi L6E5B, L6E46, $0006
	sub8m	off_AE, L6E44, L6E42
	add8i	L6E49, off_AE, $01
	mv	L6E4A, L6E49
	shladdm8 off_AE, L6E5F, L6E43
	clc                                     ; 6EBC 18                       .
	ldy     #$00                            ; 6EBD A0 00                    ..
	lda     ($AE),y                         ; 6EBF B1 AE                    ..
	adc     L6E42                           ; 6EC1 6D 42 6E                 mBn
	sta     L6E4B                           ; 6EC4 8D 4B 6E                 .Kn
	iny                                     ; 6EC7 C8                       .
	lda     ($AE),y                         ; 6EC8 B1 AE                    ..
	adc     #$00                            ; 6ECA 69 00                    i.
	sta     L6E4C                           ; 6ECC 8D 4C 6E                 .Ln
	rdmv	L6E4D, L6E53
	lda     L6E41                           ; 6EDB AD 41 6E                 .An
	lbne	L6EEF
	rdmv	L6E4D, L6E59
L6EEF:	mv	L6E48, L6E43
	mv	L6F06, L6E45
L6EFB:	lda     L6F06                           ; 6EFB AD 06 6F                 ..o
	cmp     L6E48                           ; 6EFE CD 48 6E                 .Hn
	bcs     L6F07                           ; 6F01 B0 04                    ..
	jmp     L6F65                           ; 6F03 4C 65 6F                 Leo

; ----------------------------------------------------------------------------
L6F06:  .byte	$00				; 6F06 00                       .

; ----------------------------------------------------------------------------
L6F07:  lda     L6E41                           ; 6F07 AD 41 6E                 .An
	eor     #$01                            ; 6F0A 49 01                    I.
	lbne	L6F22
	ldy     L6E49                           ; 6F11 AC 49 6E                 .In
	ldxa	L6E4B
	jsr     sub_4B97
	mv	L6E4A, $A0
L6F22:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L6E48
	ldi	$A7, $00
	mv	$A6, L6E4A
	rdmv	$A8, L6E4B
	ldy     #$55                            ; 6F42 A0 55                    .U
	ldxa	L6E4D
	jsr     sub_55A0
	add16m8	L6E4B, L6E4B, L6E5B
	inc     L6E48                           ; 6F5F EE 48 6E                 .Hn
	jmp	L6EFB
L6F65:  rts                                     ; 6F65 60                       `

