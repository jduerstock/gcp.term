
; ----------------------------------------------------------------------------
L7586:	.byte	$A3                             ; 7586 A3                       .
L7587:	.byte	$F0                             ; 7587 F0                       .
L7588:	.byte	$BD                             ; 7588 BD                       .
L7589:	.byte	$A6                             ; 7589 A6                       .
L758A:	.byte	$F0                             ; 758A F0                       .
L758B:	.byte	$9D                             ; 758B 9D                       .

cmd_lm:  
	stack_prolog L7586, $03
	func16_8 sub_7035, L758A, L7586
	add16i	off_AE, L758A, $0004
	stp8	L7587
	lda     L7589                           ; 75BB AD 89 75                 ..u
	eor     #$80                            ; 75BE 49 80                    I.
	lbeq	L75D9
	add16i	off_AE, L758A, $0003
	lda     L7589                           ; 75D4 AD 89 75                 ..u
	sta     ($AE),y                         ; 75D7 91 AE                    ..
L75D9:  lda     L7588                           ; 75D9 AD 88 75                 ..u
	eor     #$80                            ; 75DC 49 80                    I.
	lbeq	L75F9
	add16i	off_AE, L758A, $0002
	stp8	L7588
L75F9:	proc8	sub_71B5, L7586
	yldi	L4656, $01
	rts                                     ; 7604 60                       `

