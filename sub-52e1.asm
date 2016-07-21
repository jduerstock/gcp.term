
; ----------------------------------------------------------------------------
L52DF:	.byte	$32,$D8

; ----------------------------------------------------------------------------
sub_52E1:  
	prolog
	stxa	L52DF
	inc     L464B                           ; 52EA EE 4B 46                 .KF
	lda     #$0A                            ; 52ED A9 0A                    ..
	cmp     L464B                           ; 52EF CD 4B 46                 .KF
	lbcs	L5304
	yldi	L464B, $00
	proc8i	cmd_2a, $11
	jsr     sub_4749
L5304:	ldxa	L52DF
	jsr     sub_5274
	yldi	CDTMF3, $01
	rdldi	CDTMV3, $0258			; 10 seconds
	rts                                     ; 531C 60                       `

