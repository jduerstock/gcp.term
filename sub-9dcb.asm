
; ----------------------------------------------------------------------------
L9DC9:	.byte	$00
L9DCA:	.byte	$00

; ----------------------------------------------------------------------------
sub_9DCB:  
	prolog
	ifm8nei	L46E8, $01, L9DDD
	ldi	$A0, $00
	rts                                     ; 9DDC 60                       `

; ----------------------------------------------------------------------------
L9DDD:  lda     #$00                            ; 9DDD A9 00                    ..
	jsr	STrig
	mv	L9DCA, $A0
	ifm8eqm	L9DCA, L9DC9, L9DF7
	ldi	$A0, $00
	rts                                     ; 9DF6 60                       `

; ----------------------------------------------------------------------------
L9DF7:	mv	L9DC9, L9DCA
	lda     L9DCA                           ; 9DFD AD CA 9D                 ...
	lbeq	L9E0A
	ldi	$A0, $00
	rts                                     ; 9E09 60                       `

; ----------------------------------------------------------------------------
L9E0A:	ldi	$A0, $01
	rts                                     ; 9E0E 60                       `

