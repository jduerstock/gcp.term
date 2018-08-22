
; ----------------------------------------------------------------------------
L5391:	.byte	$2D                             ; 5391 2D                       -
L5392:	.byte	$2D                             ; 5392 2D                       -
L5393:	.byte	$2D                             ; 5393 2D                       -

; ----------------------------------------------------------------------------
sub_5394:  
	prolog
	yldi	L5392, $00
	jsr     sub_4FC5
	mv	L5391, $A0
	mv	L5393, LB1C9
	ifm8eqi	L5391, $04, L53BE
	ldxai	LB16A
	jsr     sub_52E1
	jmp     L545B                           ; 53BB 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L53BE:	ifm8eqi	L5391, $01, L544C
	ifm8eqi	L5393, $06, L5421
	ifm8eqm	LB1C8, LB16C, L541E
	sub8m	L4650, L4650, LB16A
	addi16m8 $A2, LB16A, LB16A
	sec                                     ; 53F5 38                       8
	lda     #$5A                            ; 53F6 A9 5A                    .Z
	sbc     LB16A                           ; 53F8 ED 6A B1                 .j.
	sta     $A4                             ; 53FB 85 A4                    ..
	ldi	$A5, $00
	ldy     $A2                             ; 5401 A4 A2                    ..
	ldxai	LB16A
	jsr     blockmove
	lda     L4650                           ; 540A AD 50 46                 .PF
	lbeq	L541E
	yldi	L464B, $00
	ldxai	LB16A
	jsr     sub_52E1
L541E:  jmp     L5449                           ; 541E 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5421:	ifm8eqi	L5393, $15, L5435
	ldxai	LB16A
	jsr     sub_52E1
	jmp     L5449                           ; 5432 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5435:	yldi	L5392, $01
	lda     CDTMF3
	lbne	L5449
	ldxai	LB16A
	jsr     sub_52E1
L5449:  jmp     L545B                           ; 5449 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L544C:  lda     CDTMF3
	lbne	L545B
	ldxai	LB16A
	jsr     sub_52E1
L545B:	mv	$A0, L5392
	rts                                     ; 5460 60                       `

