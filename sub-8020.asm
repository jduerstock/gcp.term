
; ----------------------------------------------------------------------------
sub_8020:  
	prolog
	lda     #$00                            ; 8023 A9 00                    ..
	sta     $A3                             ; 8025 85 A3                    ..
	ldy     #$04                            ; 8027 A0 04                    ..
	ldxai	L474F
	jsr     bzero
	lda     #$00                            ; 8030 A9 00                    ..
	sta     $A3                             ; 8032 85 A3                    ..
	ldy     #$0E                            ; 8034 A0 0E                    ..
	ldxai	L4753
	jsr     bzero
	ldx     #$00                            ; 803D A2 00                    ..
	lda     #$00                            ; 803F A9 00                    ..
	jsr     cmd_up
	rts                                     ; 8044 60                       `

