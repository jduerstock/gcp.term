
; ----------------------------------------------------------------------------
L7A52:	.byte	$F0
L7A53:	.byte	$F0
L7A54:	.byte	$03
L7A55:	.byte	$4C
L7A56:	.byte	$B6

; ----------------------------------------------------------------------------
cmd_uu:
	stack_prolog L7A52, $02
	ldxa	L7A52
	jsr     sub_799B
	rdmv	L7A55, $A0
	test16	L7A55
	lbne	L7A7F
	rts                                     ; 7A7E 60                       `

; ----------------------------------------------------------------------------
L7A7F:	dmv	off_AE, L7A55
	lda     L7A54                           ; 7A89 AD 54 7A                 .Tz
	ldy     #$00                            ; 7A8C A0 00                    ..
	sta     (off_AE),y                      ; 7A8E 91 AE                    ..
	iny                                     ; 7A90 C8                       .
	sty     L4656                           ; 7A91 8C 56 46                 .VF
	rts                                     ; 7A94 60                       `

