
; ----------------------------------------------------------------------------
LA7C1:  .byte	$00
LA7C2:  .byte	$00
LA7C3:  .byte	$00

cmd_d7:
	stack_prolog LA7C1, $02
	jsr     cmd_d0
	ldi	$A3, $00
	ldi	$A4, $00
	rdmv	$A5, LA7C2
	ldy     #$20                            ; A7E2 A0 20                    . 
	ldx     #$00                            ; A7E4 A2 00                    ..
	lda     LA7C1                           ; A7E6 AD C1 A7                 ...
	jsr     sub_45D0
	jsr     cmd_d1
	rts                                     ; A7EF 60                       `

