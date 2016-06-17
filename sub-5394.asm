
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
	lda     L5391                           ; 53AA AD 91 53                 ..S
	eor     #$04                            ; 53AD 49 04                    I.
	lbne	L53BE
	ldxai	LB16A
	jsr     sub_52E1
	jmp     L545B                           ; 53BB 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L53BE:  lda     L5391                           ; 53BE AD 91 53                 ..S
	eor     #$01                            ; 53C1 49 01                    I.
	lbne	L544C
	lda     L5393                           ; 53C8 AD 93 53                 ..S
	eor     #$06                            ; 53CB 49 06                    I.
	lbne	L5421
	lda     LB1C8                           ; 53D2 AD C8 B1                 ...
	eor     LB16C                           ; 53D5 4D 6C B1                 Ml.
	lbne	L541E
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
L5421:  lda     L5393                           ; 5421 AD 93 53                 ..S
	eor     #$15                            ; 5424 49 15                    I.
	lbne	L5435
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

