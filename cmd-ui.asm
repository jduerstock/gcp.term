; ----------------------------------------------------------------------------
; process raw keyboard byte

.ifndef MONOLITH
	.include "action.inc"

	.import	off_AC: zeropage
	.import	off_AE: zeropage

	.import	CH

	.import blockmove
	.import	cmd_07
	.import	cmd_2a

	.import sub_45A3
	.import sub_4983
	.import cvt_atascii_from_antic
	.import sub_4EB1
	.import sub_55A0
	.import sub_9146
	.import sub_925D
	.import sub_936A
	.import sub_9427
	.import sub_961E

	.import L464E
	.import L9050
	.import L9051
	.import L9053
	.import L9059
	.import L905D
	.import L905E
	.import	L9060
	.import	L9061
.endif

; ----------------------------------------------------------------------------
L979C:  .byte	$00
L979D:  .byte	$00
L979E:  .byte	$00
L979F:  .byte	$00
L97A0:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ui:						; "I" "B" 
	prolog
	sta     L979C                           ; 97A4 8D 9C 97                 ...
	ifm8nei	L464E, $02, L97B2
	rts                                     ; 97B1 60                       `

; ----------------------------------------------------------------------------
L97B2:	yldi	L979D, $00
	and8i	off_AE, L979C, $C0
	ifm8nei	off_AE, $C0, L97E7
	ifm8nei	CH, $FF, L97DE
	func8_8i GetD, L979D, $07
	jmp     L97E4                           ; 97DB 4C E4 97                 L..

; ----------------------------------------------------------------------------
L97DE:	mv	L979D, L979C
L97E4:  jmp     L97EC                           ; 97E4 4C EC 97                 L..

; ----------------------------------------------------------------------------
L97E7:	ldi	CH, $FF
L97EC:	ifm8eqi L979D, $9B, L9810
	add8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	proc8i	sub_961E, $00
	proc8i	cmd_2a, $15
	jmp     L9BCE                           ; 980D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9810:	ifm8eqi L979C, $2C, L9830		; tab
	add8i	off_AE, L905E, $04
	and8i	$A0, off_AE, $FC
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 982D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9830:	ifm8eqi	L979C, $87, L984A		; ctrl-* (right arrow)
	add8i	$A0, L905E, $01
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 9847 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L984A:	ifm8eqi L979C, $86, L9864		; ctrl-+ (left arrow)
	sub8i	$A0, L905E, $01
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 9861 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9864:	ifm8eqi	L979C, $F6, L9876		; ctrl-shift-<
	proc8i	sub_961E, $00
	jmp     L9BCE                           ; 9873 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9876:	ifm8eqi	L979C, $F7, L9889		; ctrl-shift->
	proc8	sub_961E, L905D
	jmp     L9BCE                           ; 9886 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9889:	ifm8eqi	L979C, $8E, L98A9		; ctrl-- (up arrow)
	sub8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	proc8	sub_961E, L905E
	jmp     L9BCE                           ; 98A6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98A9:	ifm8eqi	L979C, $8F, L98C9		; ctrl-= (down arrow)
	add8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	proc8	sub_961E, L905E
	jmp     L9BCE                           ; 98C6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98C9:	ifm8eqi	L979C, $CE, L98E0		; ctrl-shift--
	proc8i	sub_9427, $00
	proc8i	sub_961E, $00
	jmp     L9BCE                           ; 98DD 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98E0:	ifm8eqi	L979C, $CF, L98FF		; ctrl-shift-=
	sub8i	$A0, L9061, $01
	proc8	sub_9427, $A0
	proc8i	sub_961E, $00
	jmp     L9BCE                           ; 98FC 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98FF:	ifm8eqi	L979C, $B4, L999C		; ctrl-backspace
	lda     L905E                           ; 9909 AD 5E 90                 .^.
	cmp     L905D                           ; 990C CD 5D 90                 .].
	lbcc	L991A
	jsr     cmd_07
	jmp     L9999                           ; 9917 4C 99 99                 L..

; ----------------------------------------------------------------------------
L991A:	add16m8 L979F, L9059, L905E
	add16i	$A2, L979F, $0001
	sub8m	$A4, L9060, L905E
	ldi	$A5, $00
	ldy     $A2                             ; 9948 A4 A2                    ..
	ldxa	L979F
	jsr     blockmove
	sub8i	off_AE, L9060, $01
	add16m8	off_AC, L9059, off_AE
	lda     #$00                            ; 996A A9 00                    ..
	ldy     #$00                            ; 996C A0 00                    ..
	sta     (off_AC),y
	sub8i	L905D, L905D, $01
	add16m8	off_AE, L9059, L905E
	lda     (off_AE),y
	eor     #$80                            ; 998B 49 80                    I.
	sta     (off_AE),y
	iny                                     ; 998F C8                       .
	sty     L9050                           ; 9990 8C 50 90                 .P.
	proc8	sub_961E, L905E
L9999:  jmp     L9BCE                           ; 9999 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L999C:	ifm8eqi	L979C, $34, L9A45		; backspace
	lda     L905E                           ; 99A6 AD 5E 90                 .^.
	lbne	L99B4
	jsr     cmd_07
	jmp     L9A42                           ; 99B1 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L99B4:  lda     L905D                           ; 99B4 AD 5D 90                 .].
	cmp     L905E                           ; 99B7 CD 5E 90                 .^.
	lbcc	L9A35
	add16m8	L979F, L9059, L905E
	sub16i	$A0, L979F, $01
	mv	$A3, L979F+1
	sub8m	$A4, L9060, L905E
	ldi	$A5, $00
	ldy     L979F                           ; 99F2 AC 9F 97                 ...
	ldxa	$A0
	jsr     blockmove
	sub8i	off_AE, L9060, $01
	add16m8	off_AC, L9059, off_AE
	lda     #$00                            ; 9A13 A9 00                    ..
	ldy     #$00                            ; 9A15 A0 00                    ..
	sta     (off_AC),y
	iny                                     ; 9A19 C8                       .
	sty     L9050                           ; 9A1A 8C 50 90                 .P.
	sub8i	L905D, L905D, $01
	sub8i	L905E, L905E, $01
	jsr     sub_936A
	jmp     L9A42                           ; 9A32 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L9A35:	sub8i	$A0, L905E, $01
	lda     $A0                             ; 9A3D A5 A0                    ..
	jsr     sub_961E
L9A42:  jmp     L9BCE                           ; 9A42 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A45:	ifm8eqi	L979C, $74, L9A96		; shift-backspace
	ldx     L9051                           ; 9A4F AE 51 90                 .Q.
	lda     L9053                           ; 9A52 AD 53 90                 .S.
	jsr     sub_9146
	lda     $A0                             ; 9A58 A5 A0                    ..
	sta     L979E                           ; 9A5A 8D 9E 97                 ...
	ifm8eqi	L979E, $01, L9A93
	yldi	L9050, $00
	jmp     L9A72                           ; 9A6C 4C 72 9A                 Lr.

; ----------------------------------------------------------------------------
L9A6F:	.byte	$02,"CB"

; ----------------------------------------------------------------------------
L9A72:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldy     #$44                            ; 9A7F A0 44                    .D
	ldxai	L9A6F
	jsr     sub_55A0
	proc8	sub_9427, L9051
	proc8i	sub_961E, $00
L9A93:  jmp     L9BCE                           ; 9A93 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A96:	ifm8eqi	L979C, $77, L9AE8		; shift->
	ldx     L9051                           ; 9AA0 AE 51 90                 .Q.
	lda     L9053                           ; 9AA3 AD 53 90                 .S.
	jsr     sub_925D
	lda     $A0                             ; 9AA9 A5 A0                    ..
	sta     L979E                           ; 9AAB 8D 9E 97                 ...
	ifm8eqi	L979E, $01, L9AE5
	proc8	sub_9427, L9051
	jmp     L9AC4                           ; 9ABE 4C C4 9A                 L..

; ----------------------------------------------------------------------------
L9AC1:	.byte	$02,"CB"

; ----------------------------------------------------------------------------
L9AC4:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldy     #$49                            ; 9AD1 A0 49                    .I
	ldxai	L9AC1
	jsr     sub_55A0
	proc8	sub_9427, L9051
	proc8i	sub_961E, $00
L9AE5:  jmp     L9BCE                           ; 9AE5 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9AE8:	ifm8eqi	L979C, $76, L9B05
	yldi	L9050, $00
	proc8	sub_9427, L9051
	proc8i	sub_961E, $00
	jmp     L9BCE                           ; 9B02 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B05:  lda     L979D                           ; 9B05 AD 9D 97                 ...
	jsr     cvt_atascii_from_antic
	lda     $A0                             ; 9B0B A5 A0                    ..
	and     #$7F                            ; 9B0D 29 7F                    ).
	sta     L979D                           ; 9B0F 8D 9D 97                 ...
	ifm8eqm	L905D, L9060, L9B23
L9B1D:  jsr     cmd_07
	jmp     L9BCE                           ; 9B20 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B23:	sub8i	off_AE, L9060, $01
	ifm8eqm	L905E, off_AE, L9B57
	add16m8	off_AE, L9059, L905E
	lda     L979D                           ; 9B45 AD 9D 97                 ...
	eor     #$80                            ; 9B48 49 80                    I.
	ldy     #$00                            ; 9B4A A0 00                    ..
	sta     (off_AE),y
	mv	L905D, L9060
	jmp     L9BC6                           ; 9B54 4C C6 9B                 L..

; ----------------------------------------------------------------------------
L9B57:	add16m8	L979F, L9059, L905E
	add16i	$A0, L979F, $0001
	mv	$A3, L97A0
	sub8m	off_AC, L9060, L905E
	sub8i	$A4, off_AC, $01
	ldi	$A5, $00
	ldy     L979F                           ; 9B91 AC 9F 97                 ...
	ldxa	$A0
	jsr     sub_4EB1
	add16m8	off_AE, L9059, L905E
	stp8	L979D
	ldxa	L905D
	jsr     sub_4983
	lda     $A0                             ; 9BBB A5 A0                    ..
	sta     L905D                           ; 9BBD 8D 5D 90                 .].
	inc     L905D                           ; 9BC0 EE 5D 90                 .].
	inc     L905E                           ; 9BC3 EE 5E 90                 .^.
L9BC6:  jsr     sub_936A
	yldi	L9050, $01
L9BCE:  rts                                     ; 9BCE 60                       `

