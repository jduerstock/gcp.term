
; ----------------------------------------------------------------------------
LA843:  .byte	$00
LA844:  .byte	$00
LA845:  .byte	$00

cmd_24:
	stack_prolog LA843, $02
	jmp     LA855                           ; A84F 4C 55 A8                 LU.

; ----------------------------------------------------------------------------
LA852:	.byte   $02,"cS"

; ----------------------------------------------------------------------------
LA855:	ldi	$A3, $00
	dmv	off_AE, LA844
	ldi	$A5, $00
	ldp8	$A4
	add16i	$A6, LA844, $0001
	ldy     LA843                           ; A87C AC 43 A8                 .C.
	ldxai	$A852
	jsr     sub_55A0
	rts                                     ; A886 60                       `

