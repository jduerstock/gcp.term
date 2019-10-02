
; ----------------------------------------------------------------------------
L67C4:	.byte	$6C
L67C5:	.byte	$0C
L67C6:  .byte	$00
L67C7:	.byte	$AD
L67C8:	.byte	$01
L67C9:	.byte	$D3
L67CA:	.byte	$29
L67CB:	.byte	$FE
L67CC:	.byte	$8D
L67CD:	.byte	$01
L67CE:	.byte	$D3
L67CF:  .byte	$60,$AD
L67D1:	.byte	$01
L67D2:	.byte	$D3
L67D3:	.byte	$09
L67D4:	.byte	$01
L67D5:	.byte	$8D
L67D6:	.byte	$01
L67D7:	.byte	$D3

cmd_ud:						; "D","BBBBS"
	stack_prolog L67C4, $05 
	func16_8 sub_65B0, L67CF, L67C4
	test16	L67CF
	lbne	L67FD
	rts                                     ; 67FC 60                       `

; ----------------------------------------------------------------------------
L67FD:	blkmv_imi L67D2, L67CF, $0006
	dmv	off_AE, L67C8
	ldp8	L67CA
	lda     L67D4                           ; 6825 AD D4 67                 ..g
	cmp     L67CA                           ; 6828 CD CA 67                 ..g
	lda     L67D5                           ; 682B AD D5 67                 ..g
	sbc     #$00                            ; 682E E9 00                    ..
	lbcs	L683B
	mv	L67CA, L67D4
L683B:  lda     L67CA                           ; 683B AD CA 67                 ..g
	lbne	L6844
	rts                                     ; 6843 60                       `

; ----------------------------------------------------------------------------
L6844:	sub8m	off_AE, L67D2, L67C5
	lda     off_AE
	cmp     L67C7                           ; 684F CD C7 67                 ..g
	lbcs	L6861
	sub8m	L67C7, L67D2, L67C5
L6861:	sub8i	off_AE, L67CA, $01 
	ldi	$85, $00
	mv	$84, L67C7
	lda     off_AE
	ldx     #$00                            ; 6874 A2 00                    ..
	jsr     DivI
	sta     L67CC                           ; 6879 8D CC 67                 ..g
	add8m	off_AE, L67C6, L67CC
	sub8i	off_AC, L67D3, $01
	sub8m	L67CD, off_AE, off_AC
	ldx     #$00                            ; 6895 A2 00                    ..
	lda     L67CD                           ; 6897 AD CD 67                 ..g
	jsr     sub_496E
	lda     $A0                             ; 689D A5 A0                    ..
	sta     L67CE                           ; 689F 8D CE 67                 ..g
	lda     L67CE                           ; 68A2 AD CE 67                 ..g
	eor     #$01                            ; 68A5 49 01                    I.
	lbne	L68C6
	ldx     L67CD                           ; 68AC AE CD 67                 ..g
	lda     L67C4                           ; 68AF AD C4 67                 ..g
	jsr     sub_66FC
	sub8i	off_AE, L67D3, $01
;	sec                                     ; 68B5 38                       8
;	lda     L67D3                           ; 68B6 AD D3 67                 ..g
;	sbc     #$01                            ; 68B9 E9 01                    ..
;	sta     off_AE
	sub8m	L67C6, off_AE, L67CC
L68C6:	mv	L67CB, L67C7
	yldi	L67D1, $00
	mv	L68E2, L67CC
L68D7:  lda     L68E2                           ; 68D7 AD E2 68                 ..h
	cmp     L67D1                           ; 68DA CD D1 67                 ..g
	bcs     L68E3                           ; 68DD B0 04                    ..
	jmp     L6964                           ; 68DF 4C 64 69                 Ldi

; ----------------------------------------------------------------------------
L68E2:	.byte	$07

; ----------------------------------------------------------------------------
L68E3:  lda     L67CA                           ; 68E3 AD CA 67                 ..g
	cmp     L67C7                           ; 68E6 CD C7 67                 ..g
	lbcs	L68F4
	mv	L67CB, L67CA
L68F4:	add8m	off_AE, L67D1, L67C6
	shladdm8 off_AC, L67D6, off_AE
	clc                                     ; 6910 18                       .
	ldy     #$00                            ; 6911 A0 00                    ..
	lda     (off_AC),y
	adc     L67C5                           ; 6915 6D C5 67                 m.g
	sta     $A0                             ; 6918 85 A0                    ..
	iny                                     ; 691A C8                       .
	lda     (off_AC),y
	adc     #$00                            ; 691D 69 00                    i.
	sta     $A1                             ; 691F 85 A1                    ..
	add16i	$A2, L67C8, $01
	lda     #$00                            ; 6930 A9 00                    ..
	sta     $A5                             ; 6932 85 A5                    ..
	lda     L67CB                           ; 6934 AD CB 67                 ..g
	sta     $A4                             ; 6937 85 A4                    ..
	ldy     $A2                             ; 6939 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	add16m8 L67C8, L67C8, L67CB
	sub8m	L67CA, L67CA, L67CB
	inc     L67D1                           ; 695E EE D1 67                 ..g
	jmp     L68D7                           ; 6961 4C D7 68                 L.h

; ----------------------------------------------------------------------------
L6964:	yldi	L4656, $01
	rts                                     ; 6969 60                       `

