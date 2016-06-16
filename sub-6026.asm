
; ----------------------------------------------------------------------------
L601F:  .byte	$00
L6020:  .byte	$00
L6021:  .byte	$00
L6022:  .byte	$00
L6023:  .byte	$00
L6024:  .byte	$00
L6025:  .byte	$00

sub_6026:  
	stack_prolog L601F, $04
	rdldi	sub_5EFE+1, sub_5EDF
	dmv	$A3, L6022
	ldy     L6021                           ; 6043 AC 21 60                 .!`
	ldxa	L601F
	jsr     sub_5F16
	rdmv	L6024, $A0
	rdmv	$A0, L6024
	rts                                     ; 6063 60                       `

