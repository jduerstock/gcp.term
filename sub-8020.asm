
; ----------------------------------------------------------------------------
sub_8020:  
	prolog
;	bzero(&L474F, $04);
	ldi	$A3, $00
	ldy     #$04                            ; 8027 A0 04                    ..
	ldxai	L474F
	jsr     bzero
;	bzero(^L4753, $0E);
	ldi	$A3, 00
	ldy     #$0E                            ; 8034 A0 0E                    ..
	ldxai	L4753
	jsr     bzero
	ldx     #$00                            ; 803D A2 00                    ..
	lda     #$00                            ; 803F A9 00                    ..
	jsr     cmd_up
	rts                                     ; 8044 60                       `

