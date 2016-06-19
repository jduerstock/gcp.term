
; ----------------------------------------------------------------------------
L74B8:	.byte	$32                             ; 74B8 32                       2
L74B9:	.byte	$F0                             ; 74B9 F0                       .
L74BA:	.byte	$91                             ; 74BA 91                       .
L74BB:	.byte	$47                             ; 74BB 47                       G
L74BC:  .byte	$18
L74BD:  .byte	$A5

cmd_lc:						; "c" "BCS"
	stack_prolog L74B8, $03
	func16_8 sub_7035, L74BC, L74B8
	test16	L74BC
	lbne	L753B
	ldxai	$0026
	jsr     sub_606E
	rdmv	L74BC, $A0
	shladdm8 off_AE, L46A2, L74B8
	stp16	L74BC
	ldi	$A3, $00
	ldy     #$26                            ; 7518 A0 26                    .&
	ldxa	L74BC
	jsr     bzero
	inc     L4673                           ; 7523 EE 73 46                 .sF
	add16i	off_AE, L74BC, $000C
	lda     #$FF                            ; 7535 A9 FF                    ..
	ldy     #$00                            ; 7537 A0 00                    ..
	sta	(off_AE),y
L753B:	add16i	off_AE, L74BC, $000D
	stp8	L74B9
	add16i	$A0, L74BC, $000E
	add16i	$A2, L74BA, $0001
	rdldi	$A4, $0008
	ldy     $A2                             ; 7577 A4 A2                    ..
	ldxa	$A0
	jsr	blockmove
	yldi	L4656, $01
	rts                                     ; 7585 60                       `

