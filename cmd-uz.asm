
; ----------------------------------------------------------------------------
L8E1F:	.byte	$00
L8E20:  .byte	$00
L8E21:  .byte	$00
L8E22:  .byte	$00
L8E23:  .byte	$00

; ----------------------------------------------------------------------------
cmd_uz:						; "Z"
	stack_prolog L8E1F, $02
	and8i	L8E22, L8E1F, $7F
	ldi	L8E23, $00
	ldi	$84, $03
	ld2xa	L8E22
	jsr     LShift				; off_AE = 8E22 << 3
	st2xa	off_AE
	add16m	$A0, L4674, off_AE		; $A0 = 
	blkmv_mmi $A0, L8E20, $0008
	rts                                     ; 8E73 60                       `

