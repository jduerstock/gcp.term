
; ----------------------------------------------------------------------------
L4C13:	.byte	$79,$31
L4C15:	.byte	$2C,$20
L4C17:	.byte	$72
L4C18:	.byte	$32
L4C19:  .byte	$78,$32,$2C,$20

sub_4C1D:  
	stack_prolog L4C13, $05
	blkmv_imi L4C19, L4C15, $0004
	add8m	$A2, L4C19, L4C17
	add8m	$A3, L4C19+1, L4C18
	add8m	$A4, L4C19+2, L4C17
	add8m	$A5, L4C19+3, L4C18
	ldy     $A2                             ; 4C61 A4 A2                    ..
	ldxa	L4C13
	jsr     sub_4BF2
	rts                                     ; 4C6C 60                       `

