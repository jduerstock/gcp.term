
; ----------------------------------------------------------------------------
L705B:	.byte	$66
L705C:	.byte	$3E
L705D:  .word   $7C06

cmd_la:
	prolog
	stxa	L705B
	func16_8 sub_7035, L705D, L705B
	add16i	off_AE, L705D, $0009
	stp8	L705C
	rts                                     ; 708E 60                       `

