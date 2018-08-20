
; ----------------------------------------------------------------------------
L9136:  .byte	$00
L9137:  .byte	$00
L9138:  .byte	$00,$00
L913A:  .byte	$00
L913B:  .byte	$00
L913C:  .byte	$00
L913D:  .byte	$00
L913E:  .byte	$00
L913F:  .byte	$00
L9140:  .byte	$00
L9141:	.byte	$00
	.byte	$00
	.byte	$00
L9144:  .byte	$00
L9145:  .byte	$00

; ----------------------------------------------------------------------------
sub_9146:  
	prolog
	stxa	L9136
	func16_8 sub_65B0, L9138, L9136
	mv	$A3, L9138+1
	rdldi	$A4, $0006
	ldy     L9138                           ; 916C AC 38 91                 .8.
	ldxai	L9140
	jsr     blockmove
	lda     L9137                           ; 9176 AD 37 91                 .7.
	cmp     L9141                           ; 9179 CD 41 91                 .A.
	lbcs	L9242
	shladdm8 off_AE, L9144, L9137
	ldp16	L913C
	add8i	off_AE, L9137, $01
	shladdm8 off_AC, L9144, off_AE
	iny                                     ; 91BD C8                       .
	lda     ($AC),y                         ; 91BE B1 AC                    ..
	sta     L913B                           ; 91C0 8D 3B 91                 .;.
	dey                                     ; 91C3 88                       .
	lda     ($AC),y                         ; 91C4 B1 AC                    ..
	sta     L913A                           ; 91C6 8D 3A 91                 .:.
	sub8m	off_AE, L9141, L9137
	sub8i	off_AC, off_AE, $01
	ldi	$85, $00
	mv	$84, L9140
	lda     $AC                             ; 91E2 A5 AC                    ..
	ldx     #$00                            ; 91E4 A2 00                    ..
	jsr     MultI
	st2xa	L913E
	blkmv_mm16 L913C, L913A, L913E
	sub8i	off_AE, L9141, $01
	shladdm8 off_AC, L9144, off_AE
	ldy     #$01                            ; 9226 A0 01                    ..
	lda     ($AC),y                         ; 9228 B1 AC                    ..
	sta     $A1                             ; 922A 85 A1                    ..
	dey                                     ; 922C 88                       .
	lda     ($AC),y                         ; 922D B1 AC                    ..
	sta     $A0                             ; 922F 85 A0                    ..
	ldi	$A3, $00
	ldy     L9140                           ; 9235 AC 40 91                 .@.
	ldxa	$A0
	jsr     Zero
	jmp     L9247                           ; 923F 4C 47 92                 LG.

; ----------------------------------------------------------------------------
L9242:	ldi	$A0, $00
	rts                                     ; 9246 60                       `

; ----------------------------------------------------------------------------
L9247:	ldi	$A0, $01
	rts                                     ; 924B 60                       `

