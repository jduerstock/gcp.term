
; ----------------------------------------------------------------------------
L8E74:	.byte	$00
L8E75:  .byte	$00
L8E76:	.byte	$00,$00,$00,$00,$00
L8E7B:  .addr	$0000

; ----------------------------------------------------------------------------
cmd_ua:						; "A" "DDBBBBB"
	stack_prolog L8E74, $06
	shladdm8 off_AE, L46F5, L8E74
	ldp16	L8E7B
	add16i	off_AE, L8E7B, $0002
	add16m8	off_AC, L4678, L8E75
	lda     (off_AC),y
	sta     (off_AE),y
	jsr     sub_63DD
	jsr     sub_62D1
	add16i	$A0, L8E7B, $0003
	blkmv_mii $A0, L8E76, $0005
	jsr     sub_636E
	rts                                     ; 8EF7 60                       `

