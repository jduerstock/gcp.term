
; ----------------------------------------------------------------------------
LA6CB:	.byte	$00
	.byte	$00
	.byte	$00

cmd_d9:
	stack_prolog LA6CB, $02
	rts                                     ; A6D7 60                       `

