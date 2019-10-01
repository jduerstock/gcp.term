; ----------------------------------------------------------------------------
; is raw keystroke to be ignored?
;

L9066:  .byte	$00
L9067:  .byte	$00
L9068:  .byte	$00
L9069:	.byte   $9F	; ctrl-1
	.byte	$98	; ctrl-4
	.byte	$9D	; ctrl-5
	.byte	$9B	; ctrl-6
	.byte	$B3	; ctrl-7
	.byte	$B5	; ctrl-8
	.byte	$B0	; ctrl-9
	.byte	$B2	; ctrl-0
	.byte	$A6	; ctrl-/
	.byte	$27	; inverse
	.byte	$67	; shift-inverse
	.byte	$A7	; ctrl-inverse
	.byte	$E7	; ctrl-shift-inverse
	.byte	$11	; help
	.byte	$51	; shift-help
	.byte	$91	; ctrl-help
	.byte	$D1	; ctrl-shift-help
	.byte	$FF	; end of list
L907B:	.addr	L9069

; ----------------------------------------------------------------------------
sub_907D:
	prolog
	sta     L9066                           ; 9080 8D 66 90                 .f.
	yldi	L9067, $00
L9088:	add16m8	off_AE, L907B, L9067
	ldp8	L9068
	ifm8eqm	L9066, L9068, L90AF
L90AA:  ldi	$A0, $01
	rts                                     ; 90AE 60                       `

; ----------------------------------------------------------------------------
L90AF:  inc     L9067                           ; 90AF EE 67 90                 .g.
	ifm8eqi	L9068, $FF, L9088
	ldi	$A0, $00
	rts                                     ; 90C0 60                       `

