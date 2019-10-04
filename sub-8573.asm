
.ifndef MONOLITH
	.include "action.inc"

	.import off_AC: zeropage
	.import off_AE: zeropage

	.import	sub_7035
	.import	sub_8521

	.import L466D

	.import SetBlock
	.import blockmove

	.import SArgs
	.import MultI
.endif

; ----------------------------------------------------------------------------
L8550:	.byte	$9D                             ; 8550 9D                       .
L8551:  .byte	$48
L8552:	.byte	$03                             ; 8552 03                       .
L8553:  .byte	$60
L8554:	.byte	$A0                             ; 8554 A0                       .
L8555:	.byte	$EB                             ; 8555 EB                       .
L8556:	.byte	$A9                             ; 8556 A9                       .
L8557:	.byte	$09                             ; 8557 09                       .
L8558:	.byte	$20                             ; 8558 20                        
L8559:	.byte	$23                             ; 8559 23                       #
L855A:  .byte	$F8
L855B:	.byte	$A0                             ; 855B A0                       .
L855C:	.byte	$80                             ; 855C 80                       .
L855D:	.byte	$4C                             ; 855D 4C                       L
L855E:	.byte	$2B                             ; 855E 2B                       +
L855F:  .byte	$F8
L8560:	.byte	$A9                             ; 8560 A9                       .
L8561:	.byte	$07                             ; 8561 07                       .
L8562:	.byte	$8D                             ; 8562 8D                       .
L8563:	.byte	$6C                             ; 8563 6C                       l
L8564:  .byte	$0A
L8565:	.byte	$AD,$6C,$0A
	.byte	$0A
L8569:  .byte	$0A
L856A:  .byte	$0A
L856B:  .byte	$0A
L856C:  .byte	$AA
L856D:	.byte	$20                             ; 856D 20                        
L856E:	.byte	$B6                             ; 856E B6                       .
L856F:	.byte	$F7                             ; 856F F7                       .
L8570:	.byte	$CE                             ; 8570 CE                       .
L8571:	.byte	$6C                             ; 8571 6C                       l
L8572:  .byte	$0A

sub_8573:  
	stack_prolog L8550, $02
	func16_8 sub_7035, L8556, L8550
	blkmv_imi L8564, L8556, $0007
	blkmv_imi L8560, L8551, $0004
	add16i	$A2, L8556, $000E
	rdldi	$A4, $0008
	ldy     $A2                             ; 85D1 A4 A2                    ..
	ldxai	L856B
	jsr     blockmove
	yldi	$A0, $01
	rdldi	$A1, L856B
	ldx     #$08                            ; 85E6 A2 08                    ..
	ldy     #$00                            ; 85E8 A0 00                    ..
L85EA:  lda     ($A1),y                         ; 85EA B1 A1                    ..
	cmp     #$41                            ; 85EC C9 41                    .A
	beq     L85F4                           ; 85EE F0 04                    ..
	ldi	$A0, $00
L85F4:  iny                                     ; 85F4 C8                       .
	dex                                     ; 85F5 CA                       .
	bne     L85EA                           ; 85F6 D0 F2                    ..
	lda     $A0                             ; 85F8 A5 A0                    ..
	beq     L85FD                           ; 85FA F0 01                    ..
	rts                                     ; 85FC 60                       `

; ----------------------------------------------------------------------------
L85FD:	yldi	L855D, $00
	ifm8eqm	L8564, L8560, L8611
	iny
	sty     L855D                           ; 860E 8C 5D 85                 .].
L8611:	yldi	L855E, $00
	add8m	off_AE, L8564, L8569
	sub8i	off_AC, off_AE, $01
	lda     off_AC
	eor     L8562                           ; 8628 4D 62 85                 Mb.
	lbne	L8634
	iny                                     ; 8630 C8                       .
	sty     L855E                           ; 8631 8C 5E 85                 .^.
L8634:	yldi	L855F, $00
	ifm8eqm	L8565, L8561, L8648
	iny                                     ; 8644 C8                       .
	sty     L855F                           ; 8645 8C 5F 85                 ._.
L8648:	rdldi	$84, $0028
	lda     L8561                           ; 8650 AD 61 85                 .a.
	ldx     #$00                            ; 8653 A2 00                    ..
	jsr     MultI
	st2xa	off_AE
	add16m	L8558, L466D, off_AE
	sub8m	L8555, L8562, L8560
	mv	L8553, L8560
	add8m	L8554, L8553, L8555
	lda     L855F                           ; 8688 AD 5F 85                 ._.
	lbeq	L870F
	lda     L856C                           ; 8690 AD 6C 85                 .l.
	eor     #$41                            ; 8693 49 41                    IA
	lbeq	L86C8
	add16m8	L855A, L8558, L8553
	add8i	$A2, L8555, $01
	ldi	$A3, $00
	mv	$A4, L856C
	ldy     $A2                             ; 86BD A4 A2                    ..
	ldxa	L855A
	jsr     SetBlock
L86C8:	add16m8	$A2, L8558, L8553
	ldy     $A2                             ; 86D8 A4 A2                    ..
	ldx     L856B                           ; 86DA AE 6B 85                 .k.
	lda     L855D                           ; 86DD AD 5D 85                 .].
	jsr     sub_8521
	add16m8	$A2, L8558, L8554
	ldy     $A2                             ; 86F3 A4 A2                    ..
	ldx     L856D                           ; 86F5 AE 6D 85                 .m.
	lda     L855E                           ; 86F8 AD 5E 85                 .^.
	jsr     sub_8521
	add16i	L8558, L8558, $0028
L870F:	yldi	L855C, $01
	sub8m	L8729, L8563, L8561
L871E:  lda     L8729                           ; 871E AD 29 87                 .).
	cmp     L855C                           ; 8721 CD 5C 85                 .\.
	bcs     L872A                           ; 8724 B0 04                    ..
	jmp     L8777                           ; 8726 4C 77 87                 Lw.

; ----------------------------------------------------------------------------
L8729:	.byte	$00

; ----------------------------------------------------------------------------
L872A:	add16m8 $A2, L8558, L8553
	ldy     $A2                             ; 873A A4 A2                    ..
	ldx     L8572                           ; 873C AE 72 85                 .r.
	lda     L855D                           ; 873F AD 5D 85                 .].
	jsr     sub_8521
	add16m8	$A2, L8558, L8554
	ldy     $A2                             ; 8755 A4 A2                    ..
	ldx     L856E                           ; 8757 AE 6E 85                 .n.
	lda     L855E                           ; 875A AD 5E 85                 .^.
	jsr     sub_8521
	add16i	L8558, L8558, $0028
	inc     L855C                           ; 8771 EE 5C 85                 .\.
	jmp     L871E                           ; 8774 4C 1E 87                 L..

; ----------------------------------------------------------------------------
L8777:	add8m	off_AE, L8565, L856A
	sub8i	off_AC, off_AE, $01
	ifm8eqm	off_AC, L8563, L8818
	lda     L855F                           ; 8791 AD 5F 85                 ._.
	lbeq	L87AA
	sub16i	L8558, L8558, $0028
L87AA:	ifm8nei	L8570, $41, L87E2
	add16m8	L855A, L8558, L8553
	add8i	$A2, L8555, $01
	ldi	$A3, $00
	mv	$A4, L8570
	ldy     $A2                             ; 87D7 A4 A2                    ..
	ldxa	L855A
	jsr     SetBlock
L87E2:	add16m8	$A2, L8558, L8553
	ldy     $A2                             ; 87F2 A4 A2                    ..
	ldx     L8571                           ; 87F4 AE 71 85                 .q.
	lda     L855D                           ; 87F7 AD 5D 85                 .].
	jsr     sub_8521
	add16m8 $A2, L8558, L8554
	ldy     $A2                             ; 880D A4 A2                    ..
	ldx     L856F                           ; 880F AE 6F 85                 .o.
	lda     L855E                           ; 8812 AD 5E 85                 .^.
	jsr     sub_8521
L8818:  rts                                     ; 8818 60                       `

