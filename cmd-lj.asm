
; ----------------------------------------------------------------------------
L9BDD:	.byte	$00
L9BDE:	.byte	$00
L9BDF:	.byte	$00

; ----------------------------------------------------------------------------
cmd_lj:						; "j" "DBB"
	stack_prolog L9BDD, $02
	add16m8 off_AE, L46EB, L9BDD
	stp8	L9BDE
	add16m8 off_AE, L46ED, L9BDD
	lda     L9BDF                           ; 9C10 AD DF 9B                 ...
	sta     (off_AE),y
	rts                                     ; 9C15 60                       `

