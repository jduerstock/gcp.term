
; ----------------------------------------------------------------------------
L7FE8:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ul:						; "L" "B"
	prolog
	sta     L7FE8                           ; 7FEC 8D E8 7F                 ...
	lda     L7FE8                           ; 7FEF AD E8 7F                 ...
	lsr     a                               ; 7FF2 4A                       J
	lsr     a                               ; 7FF3 4A                       J
	lsr     a                               ; 7FF4 4A                       J
	lsr     a                               ; 7FF5 4A                       J
	sta     $A0                             ; 7FF6 85 A0                    ..
	ldx     L7FE8                           ; 7FF8 AE E8 7F                 ...
	lda     $A0                             ; 7FFB A5 A0                    ..
	jsr     sub_49A2
	rts                                     ; 8000 60                       `

