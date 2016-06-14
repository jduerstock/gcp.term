
; ----------------------------------------------------------------------------
L9DC9:	.byte	$00
L9DCA:	.byte	$00

; ----------------------------------------------------------------------------
sub_9DCB:  
	prolog
	lda     L46E8                           ; 9DCE AD E8 46                 ..F
	eor     #$01                            ; 9DD1 49 01                    I.
	lbeq	L9DDD
	ldi	$A0, $00
	rts                                     ; 9DDC 60                       `

; ----------------------------------------------------------------------------
L9DDD:  lda     #$00                            ; 9DDD A9 00                    ..
	jsr     read_trig
	mv	L9DCA, $A0
	lda     L9DCA                           ; 9DE7 AD CA 9D                 ...
	eor     L9DC9                           ; 9DEA 4D C9 9D                 M..
	lbne	L9DF7
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
