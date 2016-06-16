
; ----------------------------------------------------------------------------
L7605:  .byte	$60
L7606:  .byte	$E8
L7607:	.byte	$FA                             ; 7607 FA                       .
L7608:	.byte	$E9                             ; 7608 E9                       .
L7609:	.byte	$53                             ; 7609 53                       S

cmd_ll:
	stack_prolog L7605, $02
	func16_8 sub_7035, L7608, L7605
	lda     L7606                           ; 7623 AD 06 76                 ..v
	eor     #$80                            ; 7626 49 80                    I.
	lbeq	L763E
	dmv	off_AE, L7608
	stp8	L7606
L763E:  lda     L7607                           ; 763E AD 07 76                 ..v
	eor     #$80                            ; 7641 49 80                    I.
	lbeq	L765E
	add16i	off_AE, L7608, $0001
	stp8	L7607
L765E:	proc8	sub_72B1, L7605
	yldi	L4656, $01
	rts                                     ; 7669 60                       `

