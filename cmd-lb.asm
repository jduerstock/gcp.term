
; ----------------------------------------------------------------------------
L708F:	.byte	$00
L7090:	.byte	$00
L7091:	.byte	$7C
L7092:	.byte	$66,$66
	.byte	$66
	.byte	$66

cmd_lb:						; "b" "BR"
	stack_prolog L708F, $02
	func16_8 sub_7035, L7092, L708F
	add16i	$A0, L7092, $001E
	blkmv_mmi $A0, L7090, $0004
	rts

