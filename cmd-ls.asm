
; ----------------------------------------------------------------------------
L779E:	.byte	$02
L779F:	.byte	$91
L77A0:	.byte	$45
L77A1:	.byte	$C8,$A9

; ----------------------------------------------------------------------------
cmd_ls:  					; "s" "BBB"
	stack_prolog L779E, $02
	func16_8 sub_7035, L77A1, L779E
	add16i	off_AE, L77A1, $0005
	stp8	L779F
	add16i	off_AE, L77A1, $0006
	lda     L77A0                           ; 77E1 AD A0 77                 ..w
	sta     ($AE),y                         ; 77E4 91 AE                    ..
	add16i	off_AE, L77A1, $0007
	ldi	$85, $00
	mv	$84, L77A0
	lda     L779F                           ; 77FE AD 9F 77                 ..w
	ldx     #$00                            ; 7801 A2 00                    ..
	jsr     MultI
	sta     $AC                             ; 7806 85 AC                    ..
	txa                                     ; 7808 8A                       .
	ldy     #$01                            ; 7809 A0 01                    ..
	sta     ($AE),y                         ; 780B 91 AE                    ..
	lda     $AC                             ; 780D A5 AC                    ..
	dey                                     ; 780F 88                       .
	sta     ($AE),y                         ; 7810 91 AE                    ..
	proc8	sub_71B5, L779E
	proc8	sub_72B1, L779E
	yldi	L4656, $01
	rts                                     ; 7823 60                       `

