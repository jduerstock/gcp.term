
; ----------------------------------------------------------------------------
L9FE3:	.byte	$00
L9FE4:	.byte	$00
L9FE5:	.byte	$00
L9FE6:	.byte	$00
L9FE7:	.byte	$00,$00
L9FE9:  .byte	$00
L9FEA:  .byte	$00
	.byte	$00
	.byte	$00
L9FED:  .byte	$00
L9FEE:  .byte	$00
L9FEF:  .byte	$00
L9FF0:  .byte	$00
L9FF1:  .byte	$00
L9FF2:  .byte	$00
L9FF3:  .byte	$00
L9FF4:  .byte	$00
L9FF5:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9FF9:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA005:  .byte	$00
LA006:  .byte	$00
LA007:  .byte	$00
LA008:  .byte	$00
LA009:  .byte	$00
LA00A:  .byte	$00
LA00B:  .byte	$00
LA00C:  .byte	$00
LA00D:  .byte	$00
LA00E:  .byte	$00
	.byte	$00
LA010:  .byte	$00
LA011:  .byte	$00
	.byte	$00
	.byte	$00
LA014:  .byte	$00
LA015:  .byte	$00
LA016:  .byte	$00
LA017:  .byte	$00
LA018:  .byte	$00
LA019:  .byte	$00
LA01A:  .byte	$00
LA01B:  .byte	$00
	.byte	$00
	.byte	$00
LA01E:  .byte	$00
LA01F:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA023:  .byte	$00
LA024:  .byte	$00
LA025:  .byte	$00
LA026:  .byte	$00

sub_A027:
	stack_prolog L9FE3, $03
	ldxa	L9FE3
	jsr     sub_799B
	rdmv	L9FE7, $A0
	test16	L9FE7
	lbne	LA04F
	rts                                     ; A04E 60                       `

; ----------------------------------------------------------------------------
LA04F:	blkmv_imi LA00E, L9FE7, $0006
	add8m	L9FF3, LA010, L9FE5
	add8m	L9FF4, LA011, L9FE6
	func16_8 sub_65B0, L9FED, LA00E
	blkmv_imi LA014, L9FED, $0006
	func16_8 sub_65B0, L9FE9, L9FE3
	blkmv_imi $A01A, L9FE9, $0009
	yldi	LA006, $01
	ldi	$A3, $00
	sub8i	$A4, LA01A, $01
	sub8i	$A5, LA01B, $01
	ldy     #$00                            ; A0E1 A0 00                    ..
	ldxai	L9FF5
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, LA014, $01
	sub8i	$A5, LA015, $01
	ldy     #$00                            ; A0FE A0 00                    ..
	ldxai	L9FF9
	jsr     sub_4BF2
	ldi	$A3, >L9FF9
	dmv	$A4, L9FF3
	ldy     #<L9FF9
	ldxai	L9FF9
	jsr     sub_4C1D
	blkmv_iii LA023, L9FF9, $0004
	ldi     $A3, >L9FF9
	rdldi	$A4, $A001
	ldy     #<L9FF9
	ldxai	L9FF5
	jsr     sub_4CF5
	mv	LA005, $A0
	lda     LA005                           ; A14D AD 05 A0                 ...
	lbne	LA156
LA155:  rts                                     ; A155 60                       `

; ----------------------------------------------------------------------------
LA156:	ldxai	$A001
	jsr     sub_4E4A
	rdmv	LA007, $A0
	lda     LA007                           ; A167 AD 07 A0                 ...
	eor     LA016                           ; A16A 4D 16 A0                 M..
	bne     LA175                           ; A16D D0 06                    ..
	ora     LA008                           ; A16F 0D 08 A0                 ...
	eor     LA017                           ; A172 4D 17 A0                 M..
LA175:	lbeq	LA17B
	rts                                     ; A17A 60                       `

; ----------------------------------------------------------------------------
LA17B:	dmv	off_AE, LA018
	ldp16	L9FEF
	shladdm8 off_AE, LA01E, LA024
	ld2p16	L9FF1
	sty     LA009                           ; A1B2 8C 09 A0                 ...
	mv	LA00B, LA024
	mv	LA1CC, LA026
LA1C1:  lda     LA1CC                           ; A1C1 AD CC A1                 ...
	cmp     LA00B                           ; A1C4 CD 0B A0                 ...
	bcs     LA1CD                           ; A1C7 B0 04                    ..
	jmp     LA25D                           ; A1C9 4C 5D A2                 L].

; ----------------------------------------------------------------------------
LA1CC:  .byte	$00

; ----------------------------------------------------------------------------
LA1CD:	mv	LA00A, LA023
	mv	LA1E4, LA025
LA1D9:  lda     LA1E4                           ; A1D9 AD E4 A1                 ...
	cmp     LA00A                           ; A1DC CD 0A A0                 ...
	bcs     LA1E5                           ; A1DF B0 04                    ..
	jmp     LA245                           ; A1E1 4C 45 A2                 LE.

; ----------------------------------------------------------------------------
LA1E4:  .byte	$00

; ----------------------------------------------------------------------------
LA1E5:	add16m8	off_AE, L9FEF, LA009
	ldp8	LA00C
	ldy     LA00C                           ; A1FC AC 0C A0                 ...
	ldx     L9FE3                           ; A1FF AE E3 9F                 ...
	lda     #$04                            ; A202 A9 04                    ..
	jsr     sub_7E24
	lda     $A0                             ; A207 A5 A0                    ..
	lbne	LA23C
	add16m8 off_AE, L9FF1, LA00A
	ldp8	LA00D
	ldx     LA00D                           ; A225 AE 0D A0                 ...
	lda     L9FE3                           ; A228 AD E3 9F                 ...
	jsr     sub_7F93
	lda     $A0                             ; A22E A5 A0                    ..
	eor     #$01                            ; A230 49 01                    I.
	lbne	LA23C
	ldy     #$00                            ; A237 A0 00                    ..
	sty     LA006                           ; A239 8C 06 A0                 ...
LA23C:  inc     LA009                           ; A23C EE 09 A0                 ...
	inc     LA00A                           ; A23F EE 0A A0                 ...
	jmp     LA1D9                           ; A242 4C D9 A1                 L..

; ----------------------------------------------------------------------------
LA245:	add16m8	L9FF1, L9FF1, LA01A
	inc     LA00B                           ; A257 EE 0B A0                 ...
	jmp     LA1C1                           ; A25A 4C C1 A1                 L..

; ----------------------------------------------------------------------------
LA25D:  lda     LA006                           ; A25D AD 06 A0                 ...
	eor     #$01                            ; A260 49 01                    I.
	lbne	LA28B
	lda     L9FF4                           ; A267 AD F4 9F                 ...
	sta     $A3                             ; A26A 85 A3                    ..
	ldy     L9FF3                           ; A26C AC F3 9F                 ...
	ldxa	L9FE3
	jsr     cmd_uv
	lda     L474F                           ; A278 AD 4F 47                 .OG
	eor     #$03                            ; A27B 49 03                    I.
	lbne	LA28B
	ldxa	L9FE5
	jsr     sub_81F2
LA28B:  rts                                     ; A28B 60                       `

