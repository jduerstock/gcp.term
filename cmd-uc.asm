
; ----------------------------------------------------------------------------
L6ABE:	.byte	$90
L6ABF:  .byte	$EA
L6AC0:	.byte	$98
L6AC1:	.byte	$48
L6AC2:	.byte	$4C
L6AC3:	.byte	$B3
L6AC4:	.byte	$EA,$4C
L6AC6:	.byte   $94,$0A
L6AC8:	.byte	$4C
L6AC9:	.byte	$BB
L6ACA:	.byte	$0A
L6ACB:	.byte	$4C
L6ACC:	.byte	$C1
L6ACD:	.byte	$0A
L6ACE:	.byte	$4C,$C7
L6AD0:  .byte	$0A
L6AD1:	.byte	$9B
L6AD2:	.byte	$44
L6AD3:	.byte	$31
L6AD4:	.byte	$3A

; ----------------------------------------------------------------------------
cmd_uc:						; "C","BBBBS"
	stack_prolog L6ABE, $05
	func16_8 sub_65B0, L6AC4, L6ABE
	test16	L6AC4
	lbeq	L6AFF
	lda     L6ABE                           ; 6AF9 AD BE 6A                 ..j
	jsr     cmd_uk
L6AFF:  ldx     #$00                            ; 6AFF A2 00                    ..
	lda     #$0B                            ; 6B01 A9 0B                    ..
	jsr     sub_606E
	rdmv	L6AC4, $A0
	shladdm8 off_AE, L46E2, L6ABE
	stp16	L6AC4
	ldi	$A3, $00
	ldy     #$0B                            ; 6B35 A0 0B                    ..
	ldxa	L6AC4
	jsr     Zero
	dmv	off_AE, L6AC4
	stp8	L6ABF
	add16i	off_AE, L6AC4, $0001
	lda     L6AC0                           ; 6B60 AD C0 6A                 ..j
	sta     (off_AE),y
	ldi	$85, $00
	mv	$84, L6AC0
	lda     L6ABF                           ; 6B6E AD BF 6A                 ..j
	ldx     #$00                            ; 6B71 A2 00                    ..
	jsr     MultI
	st2xa	L6ACE
	add16i	off_AE, L6AC4, $0002
	stp16	L6ACE
	add16i	off_AE, L6AC4, $0004
	push16	off_AE
	lda     L6AC0                           ; 6BAE AD C0 6A                 ..j
	asl     a                               ; 6BB1 0A                       .
	sta     $A0                             ; 6BB2 85 A0                    ..
	ldi	$A1, $00
	ldxa	$A0
	jsr     sub_606E
	pull16	off_AE
	stp16	$A0
	ldxa	L6ACE
	jsr     sub_606E
	rdmv	L6AC6, $A0
	add16i	off_AE, L6AC4, $0004
	ldp16	L6AD1
	rdmv	L6ACA, L6AC6
	sty     L6AD0                           ; 6C0B 8C D0 6A                 ..j
	sub8i	L6C22, L6AC0, $01
L6C17:  lda     L6C22                           ; 6C17 AD 22 6C                 ."l
	cmp     L6AD0                           ; 6C1A CD D0 6A                 ..j
	bcs     L6C23                           ; 6C1D B0 04                    ..
	jmp     L6C5C                           ; 6C1F 4C 5C 6C                 L\l

; ----------------------------------------------------------------------------
L6C22:  .byte	$8A

; ----------------------------------------------------------------------------
L6C23:	shladdm8 off_AE, L6AD1, L6AD0
	stp16	L6ACA
	add16m8	L6ACA, L6ACA, L6ABF
	inc     L6AD0                           ; 6C56 EE D0 6A                 ..j
	jmp     L6C17                           ; 6C59 4C 17 6C                 L.l

; ----------------------------------------------------------------------------
L6C5C:	mv	$A3, L6ACE+1
	ldi	$A4, $00
	ldy     L6ACE                           ; 6C65 AC CE 6A                 ..j
	ldxa	L6AC6
	jsr     memset
	add16i	off_AE, L6AC4, $0006
	stp8	L6AC1
	add16i	off_AE, L6AC4, $0007
	lda     #$00                            ; 6C96 A9 00                    ..
	iny                                     ; 6C98 C8                       .
	sta     (off_AE),y
	lda     #$00                            ; 6C9B A9 00                    ..
	dey                                     ; 6C9D 88                       .
	sta     (off_AE),y
	lda     #$00                            ; 6CA0 A9 00                    ..
	cmp     L6AC1                           ; 6CA2 CD C1 6A                 ..j
	lbcs	L6D5B
	rdldi	$84, $0006
	lda     L6AC1                           ; 6CB2 AD C1 6A                 ..j
	ldx     #$00                            ; 6CB5 A2 00                    ..
	jsr     MultI
	st2xa	L6ACC
	ldxa	L6ACC
	jsr     sub_606E
	rdmv	L6AD3, $A0
	add16i	off_AE, L6AC4, $0007
	stp16	L6AD3
	mv	$A3, L6ACD
	ldy     L6ACC                           ; 6CF5 AC CC 6A                 ..j
	ldxa	L6AD3
	jsr     Zero
	yldi	L6AD0, $00
	sub8i	L6D1A, L6AC1, $01
L6D0F:  lda     L6D1A                           ; 6D0F AD 1A 6D                 ..m
	cmp     L6AD0                           ; 6D12 CD D0 6A                 ..j
	bcs     L6D1B                           ; 6D15 B0 04                    ..
	jmp     L6D5B                           ; 6D17 4C 5B 6D                 L[m

; ----------------------------------------------------------------------------
L6D1A:	.byte	$4C                             ; 6D1A 4C                       L

; ----------------------------------------------------------------------------
L6D1B:	add16i	off_AE, L6AD3, $0004
	lda     #$FF                            ; 6D2A A9 FF                    ..
	ldy     #$00                            ; 6D2C A0 00                    ..
	sta     (off_AE),y
	add16i	off_AE, L6AD3, $0001
	lda     L6AD0                           ; 6D3F AD D0 6A                 ..j
	sta     (off_AE),y
	add16i	L6AD3, L6AD3, $0006
	inc     L6AD0                           ; 6D55 EE D0 6A                 ..j
	jmp     L6D0F                           ; 6D58 4C 0F 6D                 L.m

; ----------------------------------------------------------------------------
L6D5B:	add16i	off_AE, L6AC4, $0009
	lda     #$00                            ; 6D6A A9 00                    ..
	ldy     #$01                            ; 6D6C A0 01                    ..
	sta     (off_AE),y
	lda     #$00                            ; 6D70 A9 00                    ..
	dey                                     ; 6D72 88                       .
	sta     (off_AE),y
	dmv	off_AE, L6AC2
	lda     #$00                            ; 6D7F A9 00                    ..
	cmp     (off_AE),y
	lbcs	L6E3B
L6D88:  ldx     #$00                            ; 6D88 A2 00                    ..
	lda     #$1A                            ; 6D8A A9 1A                    ..
	jsr     sub_606E
	rdmv	L6AC8, $A0
	add16i	off_AE, L6AC4, $0009
	stp16	L6AC8
	ldi	$A3, $00
	ldy     #$1A                            ; 6DB9 A0 1A                    ..
	ldxa	L6AC8
	jsr     Zero
	dmv	off_AE, L6AC8
	push16	off_AE
	dmv	off_AE, L6AC2
	clc                                     ; 6DDE 18                       .
	ldy     #$00                            ; 6DDF A0 00                    ..
	lda     (off_AE),y
	adc     #$01                            ; 6DE3 69 01                    i.
	sta     $A0                             ; 6DE5 85 A0                    ..
	ldi	$A1, $00
	ldxa	$A0
	jsr     sub_606E                        ; 6DEF 20 6E 60                  n`
	pull16	off_AE
	stp16	$A0
	dmv	off_AE, L6AC8
	iny                                     ; 6E0D C8                       .
	lda     (off_AE),y
	sta     $A1                             ; 6E10 85 A1                    ..
	dey                                     ; 6E12 88                       .
	lda     (off_AE),y
	sta     $A0                             ; 6E15 85 A0                    ..
	mv	$A3, L6AC2+1
	dmv	off_AC, L6AC2
	clc                                     ; 6E26 18                       .
	lda     ($AC),y                         ; 6E27 B1 AC                    ..
	adc     #$01                            ; 6E29 69 01                    i.
	sta     $A4                             ; 6E2B 85 A4                    ..
	ldi	$A5, $00
	ldy     L6AC2                           ; 6E31 AC C2 6A                 ..j
	ldxa	$A0
	jsr     blockmove
L6E3B:  ldi	$A0, $01
	rts                                     ; 6E3F 60                       `

