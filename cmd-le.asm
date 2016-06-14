
; ----------------------------------------------------------------------------
L9682:  .byte	$00
L9683:  .byte	$00
L9684:  .byte	$00
	.byte	$00
	.byte	$00
L9687:  .byte	$00
L9688:  .byte	$00
L9689:  .byte	$00
L968A:  .byte	$00
L968B:  .byte	$00
L968C:  .byte	$00
L968D:  .byte	$00

; ----------------------------------------------------------------------------
cmd_le:
	stack_prolog L9682, $02
	mv	L9052, L9682
	func16_8 sub_7035, P9055, L9052
	add16i	off_AE, P9055, $0004
	ldp8	L9053
	func16_8 sub_65B0, L9057, L9053
	blkmv_imi L9060, L9057, $0006
	mv	L9054, L9683
	ldxa	L9053
	jsr     sub_799B
	rdmv	L968C, $A0
	yldi	L905E, $00
	ldi	$A3, $00
	ldy     #$00                            ; 970C A0 00                    ..
	ldxa	L9053
	jsr     cmd_uv
	dmv	off_AE, L968C
	ldp8	L968B
	func16_8 sub_65B0, L9687, L968B
	dmv	off_AE, L9687
	ldp8	$A1
	ldx     $A1                             ; 9748 A6 A1                    ..
	lda     L9060                           ; 974A AD 60 90                 .`.
	jsr     sub_4990
	lda     $A0                             ; 9750 A5 A0                    ..
	sta     L9060                           ; 9752 8D 60 90                 .`.
	add16i	off_AE, L9687, $0004
	ldp16	L9689
	dmv	off_AE, L9689
	iny                                     ; 977B C8                       .
	lda     ($AE),y                         ; 977C B1 AE                    ..
	sta     L905A                           ; 977E 8D 5A 90                 .Z.
	dey                                     ; 9781 88                       .
	lda     ($AE),y                         ; 9782 B1 AE                    ..
	sta     L9059                           ; 9784 8D 59 90                 .Y.
	sty     L9051                           ; 9787 8C 51 90                 .Q.
	sty     L9050                           ; 978A 8C 50 90                 .P.
	proc8i	sub_9427, $00
	jsr     sub_936A
	mv	L905F, L9684
	rts                                     ; 979B 60                       `

