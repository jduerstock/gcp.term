
; ----------------------------------------------------------------------------
L5FF0:  .byte	$00
L5FF1:  .byte	$00
L5FF2:  .byte	$00
L5FF3:  .byte	$00
L5FF4:  .byte	$00

sub_5FF5:  
	stack_prolog L5FF0, $04
	rdldi	sub_5EFE+1, sub_5EC4
	dmv	$A3, L5FF3
	ldy     L5FF2                           ; 6012 AC F2 5F                 .._
	ldxa	L5FF0
	jsr     sub_5F16
	rts                                     ; 601E 60                       `

