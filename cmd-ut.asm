
; ----------------------------------------------------------------------------
L9CAC:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ut:
	prolog
	sta     L9CAC                           ; 9CB0 8D AC 9C                 ...
	mv	L46E8, L9CAC
	rts                                     ; 9CB9 60                       `

