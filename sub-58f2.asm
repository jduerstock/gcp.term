
.ifndef MONOLITH
	.include "action.inc"

	.import off_AE: zeropage

	.import L4649
	.import L4654
	.import LB224

	.import SArgs

	.import sub_4B39
	.import sub_4B47
	.import cvt_atascii_from_antic
.endif

; ----------------------------------------------------------------------------
L58E7:	.byte	$44
L58E8:  .byte	$28
L58E9:	.byte	$20
L58EA:	.byte	$24
L58EB:	.byte	$46
L58EC:	.byte	$44
L58ED:	.byte	$29
L58EE:	.byte	$20
L58EF:	.byte	$20
L58F0:	.byte	$20
L58F1:	.byte	$20

sub_58F2:
	stack_prolog L58E7, $03
	ldy     #$01                            ; 58FB A0 01                    ..
	sty     L4654                           ; 58FD 8C 54 46                 .TF
	sty     L58ED                           ; 5900 8C ED 58                 ..X
	dmv	off_AE, L58E7
	dey                                     ; 590D 88                       .
	lda     (off_AE),y
	sta     L591E                           ; 5910 8D 1E 59                 ..Y
L5913:  lda     L591E                           ; 5913 AD 1E 59                 ..Y
	cmp     L58ED                           ; 5916 CD ED 58                 ..X
	bcs     L591F                           ; 5919 B0 04                    ..
	jmp     L5CFB                           ; 591B 4C FB 5C                 L.\

; ----------------------------------------------------------------------------
L591E:	.byte	$20                             ; 591E 20                        

; ----------------------------------------------------------------------------
L591F:	add16m8	off_AE, L58E7, L58ED
	ldp8	L58EE
	lda     L58EE                           ; 5936 AD EE 58                 ..X
	eor     #'A'
	lbne	L5A35
	addi16m8 L58EB, LB224, L4654
	dmv	off_AE, L58E9
	lda     L58EC                           ; 595A AD EC 58                 ..X
	iny                                     ; 595D C8                       .
	sta     (off_AE),y
	lda     L58EB                           ; 5960 AD EB 58                 ..X
	dey                                     ; 5963 88                       .
	sta     (off_AE),y
	add16i	L58E9, L58E9, $0002
	inc     L58ED                           ; 5977 EE ED 58                 ..X
	add16m8	off_AE, L58E7, L58ED
	lda     (off_AE),y
	sta     $A0                             ; 598C 85 A0                    ..
	lda     $A0                             ; 598E A5 A0                    ..
	jsr     sub_4B39
	lda     $A0                             ; 5993 A5 A0                    ..
	sta     L58EE                           ; 5995 8D EE 58                 ..X
	yldi	L58EF, $00
	lda     L58EE                           ; 599D AD EE 58                 ..X
	lbne	L59E1
	addi16m8 $A0, LB224, L4654
	ldxa	$A0
	jsr     sub_4B47
	lda     $A0                             ; 59BA A5 A0                    ..
	sta     L58EE                           ; 59BC 8D EE 58                 ..X
	add8i	L4654, L4654, $02
	dmv	off_AE, L58EB
	stp8	L58EE
	inc16	L58EB
L59E1:  lda     L58EF                           ; 59E1 AD EF 58                 ..X
	cmp     L58EE                           ; 59E4 CD EE 58                 ..X
	lbcs	L5A32
	add16m8	off_AE, L58EB, L58EF
	push16	off_AE
	addi16m8 $A0, LB224, L4654
	ldxa	$A0
	jsr     sub_4B47
	pull16	off_AE
	stp8	$A0
	add8i	L4654, L4654, $02
	inc     L58EF                           ; 5A2C EE EF 58                 ..X
	jmp     L59E1                           ; 5A2F 4C E1 59                 L.Y

; ----------------------------------------------------------------------------
L5A32:  jmp     L5CF5                           ; 5A32 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A35:	ifm8eqi L58EE, 'B', L5A90
	rdmv	L58EB, L58E9
	inc16	L58E9
L5A53:	dmv	off_AE, L58EB
	push16	off_AE
	addi16m8 $A0, LB224, L4654
	ldxa	$A0
	jsr     sub_4B47
	pull16	off_AE
	stp8	$A0
	add8i	L4654, L4654, $02
	jmp     L5CF5                           ; 5A8D 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A90:	ifm8eqi L58EE, 'C', L5ADD
	rdmv	L58EB, L58E9
	inc16	L58E9
L5AAE:	dmv	off_AE, L58EB
	push16	off_AE
	ldx     L4654                           ; 5ABE AE 54 46                 .TF
	lda     LB224,x
	sta     $A0                             ; 5AC4 85 A0                    ..
	proc8	cvt_atascii_from_antic, $A0
	pull16	off_AE
	stp8	$A0
	inc     L4654                           ; 5AD7 EE 54 46                 .TF
	jmp     L5CF5                           ; 5ADA 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5ADD:	ifm8eqi L58EE, 'D', L5B2A
	rdmv	L58EB, L58E9
	inc16	L58E9
	dmv	off_AE, L58EB
	push16	off_AE
	ldx     L4654                           ; 5B0B AE 54 46                 .TF
	lda     LB224,x                         ; 5B0E BD 24 B2                 .$.
	sta     $A0                             ; 5B11 85 A0                    ..
	proc8	sub_4B39, $A0
	pull16	off_AE
	lda     $A0                             ; 5B1E A5 A0                    ..
	ldy     #$00                            ; 5B20 A0 00                    ..
	sta     (off_AE),y
	inc     L4654                           ; 5B24 EE 54 46                 .TF
	jmp     L5CF5                           ; 5B27 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5B2A:	ifm8eqi L58EE, 'R', L5BC1
	addi16m8 L58EB, LB224, L4654
	dmv	off_AE, L58E9
	lda     L58EC                           ; 5B4E AD EC 58                 ..X
	iny                                     ; 5B51 C8                       .
	sta     (off_AE),y
	lda     L58EB                           ; 5B54 AD EB 58                 ..X
	dey                                     ; 5B57 88                       .
	sta     (off_AE),y
	add16i	L58E9, L58E9, $0002
	sty     L58EF                           ; 5B6B 8C EF 58                 ..X
L5B6E:  lda     #$03                            ; 5B6E A9 03                    ..
	cmp     L58EF                           ; 5B70 CD EF 58                 ..X
	lbcc	L5BBE
	add16m8 off_AE, L58EB, L58EF
	push16	off_AE
	addi16m8 $A0, LB224, L4654
	ldxa	$A0
	jsr     sub_4B47
	pull16	off_AE
	lda     $A0                             ; 5BA9 A5 A0                    ..
	ldy     #$00                            ; 5BAB A0 00                    ..
	sta     (off_AE),y
	add8i	L4654, L4654, $02
	inc     L58EF                           ; 5BB8 EE EF 58                 ..X
	jmp     L5B6E                           ; 5BBB 4C 6E 5B                 Ln[

; ----------------------------------------------------------------------------
L5BBE:  jmp     L5CF5                           ; 5BBE 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5BC1:  lda     L58EE                           ; 5BC1 AD EE 58                 ..X
	eor     #'S'
	beq     L5BD2                           ; 5BC6 F0 0A                    ..
	ifm8eqi L58EE, 's', L5C95
L5BD2:	addi16m8 L58EB, LB224, L4654
	dmv	off_AE, L58E9
	stp16	L58EB
	add16i	L58E9, L58E9, $0002
	addi16m8 $A0, LB224, L4654
	ldxa	$A0
	jsr     sub_4B47
	mv	L58F0, $A0
	dmv	off_AE, L58EB
	stp8	L58F0
	add8i	L4654, L4654, $02
	iny                                     ; 5C3E C8                       .
	sty     L58EF                           ; 5C3F 8C EF 58                 ..X
	mv	L5C53, L58F0
L5C48:  lda     L5C53                           ; 5C48 AD 53 5C                 .S\
	cmp     L58EF                           ; 5C4B CD EF 58                 ..X
	bcs     L5C54                           ; 5C4E B0 04                    ..
	jmp     L5C92                           ; 5C50 4C 92 5C                 L.\

; ----------------------------------------------------------------------------
L5C53:  .byte	$00

; ----------------------------------------------------------------------------
L5C54:  ldx     L4654                           ; 5C54 AE 54 46                 .TF
	lda     LB224,x                         ; 5C57 BD 24 B2                 .$.
	sta     L58F1                           ; 5C5A 8D F1 58                 ..X
	ifm8eqi L58EE, 'S', L5C72
	func8_8	cvt_atascii_from_antic, L58F1, L58F1
L5C72:	add16m8 off_AE, L58EB, L58EF
	stp8	L58F1
	inc     L4654                           ; 5C89 EE 54 46                 .TF
	inc     L58EF                           ; 5C8C EE EF 58                 ..X
	jmp     L5C48                           ; 5C8F 4C 48 5C                 LH\

; ----------------------------------------------------------------------------
L5C92:  jmp     L5CF5                           ; 5C92 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5C95:	ifm8eqi L58EE, 'X', L5CF5
	sub8i	L58F0, L4649, $01
	mv	LB224, L58F0
	yldi	L58EF, $01
	mv	L5CC4, L58F0
L5CB9:  lda     L5CC4                           ; 5CB9 AD C4 5C                 ..\
	cmp     L58EF                           ; 5CBC CD EF 58                 ..X
	bcs     L5CC5                           ; 5CBF B0 04                    ..
	jmp     L5CE0                           ; 5CC1 4C E0 5C                 L.\

; ----------------------------------------------------------------------------
L5CC4:	.byte	$00

; ----------------------------------------------------------------------------
L5CC5:  ldx     L58EF                           ; 5CC5 AE EF 58                 ..X
	lda     LB224,x                         ; 5CC8 BD 24 B2                 .$.
	sta     $A0                             ; 5CCB 85 A0                    ..
	lda     $A0                             ; 5CCD A5 A0                    ..
	jsr     cvt_atascii_from_antic
	lda     $A0                             ; 5CD2 A5 A0                    ..
	ldx     L58EF                           ; 5CD4 AE EF 58                 ..X
	sta     LB224,x                         ; 5CD7 9D 24 B2                 .$.
	inc     L58EF                           ; 5CDA EE EF 58                 ..X
	jmp     L5CB9                           ; 5CDD 4C B9 5C                 L.\

; ----------------------------------------------------------------------------
L5CE0:	dmv	off_AE, L58E9
	lda     #>LB224
	ldy     #$01                            ; 5CEC A0 01                    ..
	sta     (off_AE),y
	lda     #<LB224
	dey                                     ; 5CF2 88                       .
	sta     (off_AE),y
L5CF5:  inc     L58ED                           ; 5CF5 EE ED 58                 ..X
	jmp     L5913                           ; 5CF8 4C 13 59                 L.Y

; ----------------------------------------------------------------------------
L5CFB:  rts                                     ; 5CFB 60                       `

