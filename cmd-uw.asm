
; ----------------------------------------------------------------------------
L65D6:	.byte	$00
L65D7:	.byte	$00
L65D8:	.byte	$00
L65D9:	.byte	$00
L65DA:	.byte	$00
L65DB:  .byte	$00
L65DC:  .byte	$00
L65DD:  .byte	$00
L65DE:  .byte	$00
L65DF:  .byte	$00
L65E0:  .byte	$00,$00

cmd_uw:  					; "W"
	stack_prolog L65D6, $05
	func16_8 sub_65B0, L65E0, L65D6
	test16	L65E0
	lbne	L6607
	rts                                     ; 6606 60                       `

; ----------------------------------------------------------------------------
L6607:	add16i	off_AE, L65E0, $0004
	ldp16	L65DE
	sub8m	off_AE, L65D9, L65D7
	add8i	L65DC, off_AE, $01
	mv	L65DD, L65D8
	mv	L664B, L65DA
L6640:  lda     L664B                           ; 6640 AD 4B 66                 .Kf
	cmp     L65DD                           ; 6643 CD DD 65                 ..e
	bcs     L664C                           ; 6646 B0 04                    ..
	jmp     L668A                           ; 6648 4C 8A 66                 L.f

; ----------------------------------------------------------------------------
L664B:	.byte	$00

; ----------------------------------------------------------------------------
L664C:	shladdm8 off_AE, L65DE, L65DD
	clc                                     ; 6660 18                       .
	ldy     #$00                            ; 6661 A0 00                    ..
	lda     ($AE),y                         ; 6663 B1 AE                    ..
	adc	L65D7
	sta	$A0
	iny
	lda     ($AE),y                         ; 666B B1 AE                    ..
	adc     #$00                            ; 666D 69 00                    i.
	sta     $A1                             ; 666F 85 A1                    ..
	ldi	$A3, $00
	mv	$A4, L65DB
	ldy     L65DC                           ; 667A AC DC 65                 ..e
	ldxa	$A0
	jsr     memset
	inc     L65DD                           ; 6684 EE DD 65                 ..e
	jmp     L6640                           ; 6687 4C 40 66                 L@f

; ----------------------------------------------------------------------------
L668A:	yldi	L4656, $01
	rts                                     ; 668F 60                       `

