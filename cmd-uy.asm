
; ----------------------------------------------------------------------------
L7D44:	.byte	$00
L7D45:  .byte	$00
L7D46:  .byte	$00
L7D47:  .byte	$00
L7D48:  .byte	$00,$00
L7D4A:  .byte	$00
L7D4B:  .word	$0000

; ----------------------------------------------------------------------------
cmd_uy:						; "Y" "BDBB"
	stack_prolog L7D44, $03
	lda	L7D44
	jsr	sub_65B0
	rdmv	L7D48, $A0
	lda     L7D45                           ; 7D66 AD 45 7D                 .E}
	and     #$01                            ; 7D69 29 01                    ).
	sta     L7D4A                           ; 7D6B 8D 4A 7D                 .J}
	lsr     L7D45                           ; 7D6E 4E 45 7D                 NE}
	test16	L7D48
	lbne	L7D7D
	rts                                     ; 7D7C 60                       `

; ----------------------------------------------------------------------------
L7D7D:	add16i	off_AE, L7D48, $0009
	ldp16	L7D4B
	test16	L7D4B
	lbne	L7DA5
	rts                                     ; 7DA4 60                       `

; ----------------------------------------------------------------------------
L7DA5:	add16i	off_AE, L7D4B, $0002
	ldi	$85, $00
	ldi	$84, $03
	lda     L7D45                           ; 7DBC AD 45 7D                 .E}
	ldx     #$00                            ; 7DBF A2 00                    ..
	jsr     MultI
	st2xa	off_AC
	add16m	L7D4B, off_AE, off_AC
	dmv	off_AE, L7D4B
	lda     L7D4A                           ; 7DE2 AD 4A 7D                 .J}
	ldy     #$00                            ; 7DE5 A0 00                    ..
	sta     ($AE),y                         ; 7DE7 91 AE                    ..
	add16i	off_AE, L7D4B, $0001
	lda     L7D46                           ; 7DF8 AD 46 7D                 .F}
	sta     ($AE),y                         ; 7DFB 91 AE                    ..
	add16i	off_AE, L7D4B, $0002
	lda     L7D47                           ; 7E0C AD 47 7D                 .G}
	sta     ($AE),y                         ; 7E0F 91 AE                    ..
	rts                                     ; 7E11 60                       `

