
; ----------------------------------------------------------------------------
tohex:  stx     off_AE
	sty     off_AE+1
	ldy     #$00                            ; 4B19 A0 00                    ..
	tax                                     ; 4B1B AA                       .
	lsr     a                               ; 4B1C 4A                       J
	lsr     a                               ; 4B1D 4A                       J
	lsr     a                               ; 4B1E 4A                       J
	lsr     a                               ; 4B1F 4A                       J
	ora     #$30                            ; 4B20 09 30                    .0
	cmp     #$3A                            ; 4B22 C9 3A                    .:
	bcc     L4B28                           ; 4B24 90 02                    ..
	adc     #$26                            ; 4B26 69 26                    i&
L4B28:  sta     (off_AE),y
	txa                                     ; 4B2A 8A                       .
	and     #$0F                            ; 4B2B 29 0F                    ).
	ora     #$30                            ; 4B2D 09 30                    .0
	cmp     #$3A                            ; 4B2F C9 3A                    .:
	bcc     L4B35                           ; 4B31 90 02                    ..
	adc     #$26                            ; 4B33 69 26                    i&
L4B35:  iny                                     ; 4B35 C8                       .
	sta     (off_AE),y
	rts                                     ; 4B38 60                       `

