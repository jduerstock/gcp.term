
; ----------------------------------------------------------------------------
L6FE7:	.byte   $F0,$F0
L6FE9:	.byte   $F0
L6FEA:	.byte	$F0

; ----------------------------------------------------------------------------
cmd_lz:
	prolog
	stxa	L6FE7
	lda     L4648                           ; 6FF4 AD 48 46                 .HF
	eor     #$FF                            ; 6FF7 49 FF                    I.
	lbne	L6FFF
L6FFE:  rts                                     ; 6FFE 60                       `

; ----------------------------------------------------------------------------
L6FFF:	func16_8 sub_65B0, L6FE9, L4648
	dmv	off_AE, L6FE9
	ldp8	$A3
	rdmv	$A4, L6FE7
	ldy     #$00                            ; 7029 A0 00                    ..
	ldx     #$00                            ; 702B A2 00                    ..
	lda     L4648
	jsr     cmd_ud
	rts                                     ; 7033 60                       `

