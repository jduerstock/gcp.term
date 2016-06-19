
; ----------------------------------------------------------------------------
L8EF8:	.byte	$00
L8EF9:  .byte	$00
L8EFA:  .byte	$00
L8EFB:  .addr	$0000

cmd_ug:						; "G" "DDB"
	stack_prolog L8EF8, $02
	shladdm8 off_AE, L46F5, L8EF8
	ldp16	L8EFB
	dmv	off_AE, L8EFB
	lda     L8EF9                           ; 8F31 AD F9 8E                 ...
	sta     ($AE),y                         ; 8F34 91 AE                    ..
	add16i	off_AE, L8EFB, $0001
	lda     L8EFA                           ; 8F45 AD FA 8E                 ...
	sta     ($AE),y                         ; 8F48 91 AE                    ..
	iny                                     ; 8F4A C8                       .
	sty	L4656
	rts                                     ; 8F4E 60                       `

