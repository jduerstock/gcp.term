
; ----------------------------------------------------------------------------
L7994:  .byte	$08
L7995:	.byte	$F0                             ; 7995 F0                       .
L7996:	.byte	$1D,$30
L7998:	.byte	$1B,$29
L799A:	.byte	$01                             ; 799A 01                       .

sub_799B:  
	prolog
	stxa	L7994
	func16_8 sub_65B0, L7996, L7994
L79B4:  add16i	off_AE, L7996, $0007
	ldp16	L7998
	test16	L7998
	lbne	L79E4
	rdldi	$A0, $0000
	rts                                     ; 79E3 60                       `

; ----------------------------------------------------------------------------
L79E4:  ldy     #$00                            ; 79E4 A0 00                    ..
	sty     L799A                           ; 79E6 8C 9A 79                 ..y
	add16i	off_AE, L7996, $0006
	sec                                     ; 79F8 38                       8
	lda     ($AE),y                         ; 79F9 B1 AE                    ..
	sbc     #$01                            ; 79FB E9 01                    ..
	sta     L7A0B                           ; 79FD 8D 0B 7A                 ..z
L7A00:  lda     L7A0B                           ; 7A00 AD 0B 7A                 ..z
	cmp     L799A                           ; 7A03 CD 9A 79                 ..y
	bcs     L7A0C                           ; 7A06 B0 04                    ..
	jmp     L7A49                           ; 7A08 4C 49 7A                 LIz

; ----------------------------------------------------------------------------
L7A0B:	.byte	$03                             ; 7A0B 03                       .

; ----------------------------------------------------------------------------
L7A0C:	add16i	off_AE, L7998, $0001
	ldy     #$00                            ; 7A1B A0 00                    ..
	lda     ($AE),y                         ; 7A1D B1 AE                    ..
	eor     L7995                           ; 7A1F 4D 95 79                 M.y
	lbne	L7A32
	rdmv	$A0, L7998
	rts                                     ; 7A31 60                       `

; ----------------------------------------------------------------------------
L7A32:	add16i	L7998, L7998, $0006
	inc	L799A
	jmp	L7A00

; ----------------------------------------------------------------------------
L7A49:	rdldi	$A0, $00
	rts                                     ; 7A51 60                       `

