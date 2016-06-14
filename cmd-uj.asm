
; ----------------------------------------------------------------------------
L9C16:  .byte	$00
L9C17:  .byte	$00,$00
L9C19:	.byte	$00,$00,$01,$01,$01,$00,$FF,$FF,$FF
L9C22:	.addr	L9C19
L9C24:	.byte	$00,$FF,$FF,$00,$01,$01,$01,$00,$FF
L9C2D:	.addr	L9C24
L9C2F:	.byte	$D6,$1E,$09,$00,$00,$00,$00,$00,$00
L9C38:	.byte	$D6,$1E,$09,$00,$00,$00,$00,$00,$00

cmd_uj:  
	stack_prolog L9C16, $02
	mv	L46E6, L9C16
	dmv	L46E9, L9C17
	rdldi	L46EB, L9C2F
	rdldi	L46ED, L9C38
	blkmv_imi L9C2F, L9C22, $0009
	blkmv_imi L9C38, L9C2D, $0009
	lda     L46E6                           ; 9C9E AD E6 46                 ..F
	lbne	L9CAB
	proc8i	cmd_us, $00
L9CAB:  rts                                     ; 9CAB 60                       `

