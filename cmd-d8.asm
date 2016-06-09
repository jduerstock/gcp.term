
; ----------------------------------------------------------------------------
LA7F0:  .byte	$00
LA7F1:  .byte	$00,$00

cmd_d8:
	stack_prolog LA7F0, $02
	jsr     cmd_d0
	ldi	$A3, $00
	ldi	$A4, $00
	rdmv	$A5, LA7F1
	ldy     #$21                            ; A811 A0 21                    .!
	ldx     #$00                            ; A813 A2 00                    ..
	lda     LA7F0                           ; A815 AD F0 A7                 ...
	jsr     sub_45D0
	jsr     cmd_d1
	rts                                     ; A81E 60                       `

