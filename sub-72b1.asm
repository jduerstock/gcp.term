
; ----------------------------------------------------------------------------
L729F:	.byte	$10                             ; 729F 10                       .
L72A0:	.byte	$D0                             ; 72A0 D0                       .
L72A1:	.byte	$E2                             ; 72A1 E2                       .
L72A2:	.byte	$A5
	.byte	$43
	.byte	$8D
	.byte	$E7
L72A6:	.byte	$02
	.byte	$A5
	.byte	$44
	.byte   $8D                             ; 72A9 8D                       .
L72AA:  .byte	$E8
L72AB:	.byte	$02                             ; 72AB 02                       .
	.byte	$4C
	.byte	$9E
	.byte	$E5
L72AF:  .byte	$18
L72B0:	.byte	$A5                             ; 72B0 A5                       .

; ----------------------------------------------------------------------------
sub_72B1:  
	prolog
	sta     L729F                           ; 72B4 8D 9F 72                 ..r
	func16_8 sub_7035, L72A0, L729F
	blkmv_imi L72AA, L72A0, $0007
	sec
	lda     #$00                            ; 72DF A9 00                    ..
	sbc     L72AA                           ; 72E1 ED AA 72                 ..r
	sta     $A2                             ; 72E4 85 A2                    ..
	sec                                     ; 72E6 38                       8
	lda     #$00                            ; 72E7 A9 00                    ..
	sbc     L72AB                           ; 72E9 ED AB 72                 ..r
	sta     $A3                             ; 72EC 85 A3                    ..
	sec                                     ; 72EE 38                       8
	lda     #$27                            ; 72EF A9 27                    .'
	sbc     L72AA                           ; 72F1 ED AA 72                 ..r
	sta     $A4                             ; 72F4 85 A4                    ..
	sec                                     ; 72F6 38                       8
	lda     #$17                            ; 72F7 A9 17                    ..
	sbc     L72AB                           ; 72F9 ED AB 72                 ..r
	sta     $A5                             ; 72FC 85 A5                    ..
	ldy     $A2                             ; 72FE A4 A2                    ..
	ldxai	L72A2
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, L72AF, $01
	sub8i	$A5, L72B0, $01
	ldy     #$00                            ; 731B A0 00                    ..
	ldxai	L72A6
	jsr     sub_4BF2
	add16i	off_AE, L72A0, $000A
	push16	off_AE
	ldi	$A3, >L72A6
	add16i	$A4, L72A0, $001A
	ldy     #<L72A6
	ldxai	$72A2
	jsr	sub_4CF5
	pull16	off_AE
	stp8	$A0
	rts                                     ; 7361 60                       `

