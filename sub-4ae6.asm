
; ----------------------------------------------------------------------------
L4AE5:	.byte	$0D                             ; 4AE5 0D                       .

sub_4AE6:  
	prolog
	sta     L4AE5                           ; 4AE9 8D E5 4A                 ..J
	ldi	$A3, $00
	ldi	$A4, $00
	rdldi	$A5, L4AA1
	ldy     #$0D                            ; 4AFC A0 0D                    ..
	ldx     #$00                            ; 4AFE A2 00                    ..
	lda     L4AE5                           ; 4B00 AD E5 4A                 ..J
	jsr     XIO
	rts                                     ; 4B06 60                       `

