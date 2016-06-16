
; ----------------------------------------------------------------------------
L66F2:  .byte	$00
L66F3:  .byte	$00
L66F4:  .byte	$00
L66F5:  .byte	$00
L66F6:	.byte	$00
L66F7:	.byte	$00
	.byte	$00
	.byte	$00
L66FA:	.byte	$00
L66FB:	.byte	$00

; ----------------------------------------------------------------------------
sub_66FC:  
	prolog
	stxa	L66F2
	func16_8 sub_65B0, L66F4, L66F2
	blkmv_imi L66F6, L66F4, $0006
	lda     L66F3                           ; 672C AD F3 66                 ..f
	cmp     L66F7                           ; 672F CD F7 66                 ..f
	lbcc	L6742
	ldx     #$00                            ; 6737 A2 00                    ..
	lda     L66F2
	jsr     cmd_uf
	jmp     L67C3                           ; 673F 4C C3 67                 L.g

; ----------------------------------------------------------------------------
L6742:	dmv	off_AE, L66FA
	ldp16	$A0
	shladdm8 off_AC, L66FA, L66F3
	iny                                     ; 676B C8                       .
	lda     ($AC),y                         ; 676C B1 AC                    ..
	sta     $A3                             ; 676E 85 A3                    ..
	dey                                     ; 6770 88                       .
	lda     ($AC),y                         ; 6771 B1 AC                    ..
	sta     $A2                             ; 6773 85 A2                    ..
	sub8m	$AA, L66F7, L66F3
	ldi	$85, $00
	mv	$84, L66F6
	lda     $AA                             ; 6787 A5 AA                    ..
	ldx     #$00                            ; 6789 A2 00                    ..
	jsr     MultI
	st2xa	$A4
	ldy     $A2                             ; 6793 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	sub8m	$A2, L66F7, L66F3
	sub8i	$A3, L66F6, $01
	sub8i	$A4, L66F7, $01
	ldi	$A5, $00
	ldy     $A2                             ; 67B9 A4 A2                    ..
	ldx     #$00                            ; 67BB A2 00                    ..
	lda     L66F2
	jsr     cmd_uw
L67C3:  rts                                     ; 67C3 60                       `

