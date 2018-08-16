
; ----------------------------------------------------------------------------
L9416:  .byte	$00
	.byte	$00
	.byte	$00
L9419:  .byte	$00
L941A:  .byte	$00
	.byte	$00
L941C:	.byte	$00
	.byte	$00
L941E:  .byte	$00
L941F:  .byte	$00
	.byte	$00
	.byte	$00
L9422:  .byte	$00
L9423:	.byte	$00
L9424:  .byte	$00
	.byte	$00
L9426:  .byte	$00

; ----------------------------------------------------------------------------
sub_9427:  
	prolog
	sta     L9416                           ; 942A 8D 16 94                 ...
	add16m8	off_AE, L9059, L905E
	ldy     #$00                            ; 943D A0 00                    ..
	lda     (off_AE),y                      ; 943F B1 AE                    ..
	eor     #$80                            ; 9441 49 80                    I.
	sta     (off_AE),y                      ; 9443 91 AE                    ..
	lda     L9050                           ; 9445 AD 50 90                 .P.
	eor     #$01                            ; 9448 49 01                    I.
	lbne	L9499
	blkmv_mm8 L905B, L9059, L9060
	jmp     L9470                           ; 9469 4C 70 94                 Lp.

; ----------------------------------------------------------------------------
L946C:	.byte	$03,"CBS"

; ----------------------------------------------------------------------------
L9470:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldi	$A7, $00
	mv	$A6, L905D
	rdmv	$A8, L9059
	ldy     #$4C                            ; 9490 A0 4C                    .L
	ldxai	L946C
	jsr     sub_55A0
L9499:  lda     #$7F                            ; 9499 A9 7F                    ..
	cmp     L9416                           ; 949B CD 16 94                 ...
	lbcs	L94AB
	yldi	L9416, $00
	jmp     L94E8                           ; 94A8 4C E8 94                 L..

; ----------------------------------------------------------------------------
L94AB:	sub8i	off_AE, L9061, $01
	lda     $AE                             ; 94B3 A5 AE                    ..
	cmp     L9416                           ; 94B5 CD 16 94                 ...
	lbcs	L94E8
	lda     L905F                           ; 94BD AD 5F 90                 ._.
	eor     #$01                            ; 94C0 49 01                    I.
	lbne	L94DF
	sec                                     ; 94C7 38                       8
	lda     L9416                           ; 94C8 AD 16 94                 ...
	sbc     L9061                           ; 94CB ED 61 90                 .a.
	sta     $AE                             ; 94CE 85 AE                    ..
	add8i	$A1, off_AE, $01
	ldx     $A1                             ; 94D7 A6 A1                    ..
	lda     L9053                           ; 94D9 AD 53 90                 .S.
	jsr     sub_66FC
L94DF:	sub8i	L9416, L9061, $01
L94E8:	shladdm8 off_AE, L9064, L9416
	ldp16	L905B
	blkmv_mm8 L9059, L905B, L9060
	ldx     L9416                           ; 9523 AE 16 94                 ...
	lda     L9053                           ; 9526 AD 53 90                 .S.
	jsr     sub_90CE
	mv	L905D, $A0
	mv	L9051, L9416
	blkmv_imi L941C, P9055, $0007
	add16i	$A2, P9055, $001E
	rdldi	$A4, $0004
	ldy     $A2                             ; 9565 A4 A2                    ..
	ldxai	L9423
	jsr     blockmove
	add8m	L9419, L941F, L9424
	add8m	off_AE, L941F, L9426
	add8m	off_AC, off_AE, L9422
	sub8i	L941A, off_AC, $01
	ldx     L9419                           ; 9591 AE 19 94                 ...
	lda     L9051                           ; 9594 AD 51 90                 .Q.
	jsr     sub_4955
	lda     $A0                             ; 959A A5 A0                    ..
	eor     #$01                            ; 959C 49 01                    I.
	lbne	L95B8
	sub8m	off_AE, L9419, L9051
	sub8m	L941F, L941F, off_AE
	jmp     L95DC                           ; 95B5 4C DC 95                 L..

; ----------------------------------------------------------------------------
L95B8:  ldx     L941A                           ; 95B8 AE 1A 94                 ...
	lda     L9051                           ; 95BB AD 51 90                 .Q.
	jsr     sub_496E
	lda     $A0                             ; 95C1 A5 A0                    ..
	eor     #$01                            ; 95C3 49 01                    I.
	lbne	L95DC
L95CA:	add8m	off_AE, L941F, L9051
	sub8m	L941F, off_AE, L941A
L95DC:	mv	$A3, L9051
	ldy     #$80                            ; 95E1 A0 80                    ..
	ldxa	L9053
	jsr     cmd_uv
	mv	$A3, L941F
	ldy     L941E                           ; 95F1 AC 1E 94                 ...
	ldxa	L9052
	jsr     cmd_lm
	yldi	L9050, $00
	add16m8	off_AE, L9059, L905E
	lda     (off_AE),y
	eor     #$80                            ; 9614 49 80                    I.
	sta     (off_AE),y
	iny                                     ; 9618 C8                       .
	sty     L4656                           ; 9619 8C 56 46                 .VF
	rts                                     ; 961C 60                       `

