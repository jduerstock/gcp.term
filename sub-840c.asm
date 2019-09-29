
; ----------------------------------------------------------------------------
L83E3:	.byte	$09
L83E4:	.byte	$4C
L83E5:	.byte	$DD
L83E6:	.byte	$F3
L83E7:	.byte	$A9                             ; 83E7 A9                       .
L83E8:	.byte	$00
L83E9:	.byte	$F0                             ; 83E9 F0                       .
L83EA:  inc     $AD,x                           ; 83EA F6 AD                    ..
	.byte   $B7                             ; 83EC B7                       .
L83ED:	.byte	$09                             ; 83ED 09                       .
L83EE:  and     #$BF                            ; 83EE 29 BF                    ).
	;jmp     LF6C2                           ; 83F0 4C C2 F6                 L..
	.byte	$4C,$C2,$F6
	.byte   $AD                             ; 83F3 AD                       .
L83F4:	.byte	$B7                             ; 83F4 B7                       .
L83F5:	.byte	$09                             ; 83F5 09                       .
L83F6:	.byte	$09                             ; 83F6 09                       .
L83F7:  .byte	$40
L83F8:	.byte	$D0,$E7
	.byte   $A9                             ; 83FA A9                       .
L83FB:	.byte	$09                             ; 83FB 09                       .
L83FC:  ldy     #$B8                            ; 83FC A0 B8                    ..
	ldx     #$20                            ; 83FE A2 20                    . 
	.byte	$20,$23,$F8
	lda     #$04                            ; 8403 A9 04                    ..
	.byte   $9D                             ; 8405 9D                       .
	.byte	$4A
L8407:	.byte	$03
L8408:	.byte	$A9
L8409:	.byte	$03
L840A:	.byte	$20
L840B:  .byte	$B8

; ----------------------------------------------------------------------------
sub_840C:  
	prolog
	stxa	L83E3
	add8m	L83E5, L4751, L83E3
	add8m	L83E6, L4752, L83E4
	ifm8eqi L474F, $01, L843C
	ldxa	L83E5
	jsr     sub_8047
L843C:	ifm8nei L474F, $02, L8447
	rts                                     ; 8446 60                       `

; ----------------------------------------------------------------------------
L8447:	func16_8 sub_7035, L83E7, L4750
	blkmv_imi L8407, L83E7, $0005
	func16_8 sub_65B0, L83E9, L840B
	blkmv_imi L83F7, L83E9, $0009
	sub8m	off_AE, L8407, L8409
	sub8m	L83ED, L83E5, off_AE
	sub8m	off_AE, L8408, L840A
	sub8m	L83EE, L83E6, off_AE
	lda     L83ED                           ; 84B9 AD ED 83                 ...
	cmp     L83F7                           ; 84BC CD F7 83                 ...
	bcs     L84CC                           ; 84BF B0 0B                    ..
	lda     L83ED+1                         ; 84C1 AD EE 83                 ...
	cmp     L83F7+1                         ; 84C4 CD F8 83                 ...
	lbcc	L84CD
L84CC:	rts                                     ; 84CC 60                       `

; ----------------------------------------------------------------------------
L84CD:	shladdm8 off_AE, L83FB, L83EE
	ldp16	L83F4
	add16m8 off_AE, L83F4, L83ED
	lda     (off_AE),y
L8500:  sta     L83F6                           ; 8500 8D F6 83                 ...
	ldx     L83F6                           ; 8503 AE F6 83                 ...
	lda     L840B                           ; 8506 AD 0B 84                 ...
	jsr     sub_7F93
	lda     $A0                             ; 850C A5 A0                    ..
	lbne	L851C
	ldxa	L83E3
	jsr     sub_81F2
L851C:  rts                                     ; 851C 60                       `

