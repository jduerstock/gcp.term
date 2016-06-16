
; ----------------------------------------------------------------------------
L6F8B:  .byte	$00
L6F8C:  .byte	$00
L6F8D:  .byte	$00
L6F8E:  .byte	$00
L6F8F:  .byte	$00
L6F90:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ly:
	prolog
	stxa	L6F8B
	lda     L4647                           ; 6F9A AD 47 46                 .GF
	eor     #$FF                            ; 6F9D 49 FF                    I.
	lbne	L6FA5
	rts                                     ; 6FA4 60                       `

; ----------------------------------------------------------------------------
L6FA5:	func16_8 sub_65B0, L6F8D, L4647
	blkmv_imi L6F8F, L6F8D, $0002
	mv	$A3, L6F8F
	rdmv	$A4, L6F8B
L6FDB:  ldy     L6F90                           ; 6FDB AC 90 6F                 ..o
	ldx     #$00                            ; 6FDE A2 00                    ..
	lda     L4647                           ; 6FE0 AD 47 46                 .GF
	jsr     cmd_ud
	rts                                     ; 6FE6 60                       `

