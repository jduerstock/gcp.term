
; ----------------------------------------------------------------------------
LA990:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ln:						; "n" "C"
	prolog
	sta     LA990                           ; A994 8D 90 A9                 ...
	lda     LA990                           ; A997 AD 90 A9                 ...
	jsr     sub_4BC9
	mv	L464A, $A0
	rts                                     ; A9A2 60                       `

