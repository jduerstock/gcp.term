
; ----------------------------------------------------------------------------
L794E:	.byte	$0D
L794F:	.byte	$F0,$05

; ----------------------------------------------------------------------------
cmd_ld:						; "d" "B"
	prolog
	sta     L794E                           ; 7954 8D 4E 79                 .Ny
	func16_8 sub_7035, L794F, L794E
	test16	L794F
	lbne	L7973
L7972:  rts                                     ; 7972 60                       `

; ----------------------------------------------------------------------------
L7973:  ldx     #$FF                            ; 7973 A2 FF                    ..
	lda     L794E                           ; 7975 AD 4E 79                 .Ny
	jsr     cmd_lp
	ldi	$A3, $00
	ldy     #$26                            ; 797F A0 26                    .&
	ldxa	L794F
	jsr     sub_619A
	sub8i	L4673, L4673, $01
	rts                                     ; 7993 60                       `

