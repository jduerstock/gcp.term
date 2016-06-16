
; ----------------------------------------------------------------------------
L62CE:  .byte	$00
L62CF:  .byte	$00
L62D0:  .byte	$00

; ----------------------------------------------------------------------------
sub_62D1:
	prolog
	mv	L62D0, $022F
	yldi	$022F, $00
	shladdi	off_AE, L466F, $0001
	iny                                     ; 62F2 C8                       .
	lda     ($AE),y                         ; 62F3 B1 AE                    ..
	sta     $0231                           ; 62F5 8D 31 02                 .1.
	dey                                     ; 62F8 88                       .
	lda     ($AE),y                         ; 62F9 B1 AE                    ..
	sta     $0230                           ; 62FB 8D 30 02                 .0.
	lda     L62D0                           ; 62FE AD D0 62                 ..b
	sta     $022F                           ; 6301 8D 2F 02                 ./.
	ldi	$D40E, $C0
	shladdi off_AE, L466F, $0001
	iny                                     ; 631C C8                       .
	lda     ($AE),y                         ; 631D B1 AE                    ..
	sta     L62CF                           ; 631F 8D CF 62                 ..b
	dey                                     ; 6322 88                       .
	lda     ($AE),y                         ; 6323 B1 AE                    ..
	sta     L62CE                           ; 6325 8D CE 62                 ..b
L6328:	shladdi	off_AE, L466F, $0001
	dmv	off_AC, L466F
	iny                                     ; 6345 C8                       .
	lda     ($AC),y                         ; 6346 B1 AC                    ..
	sta     ($AE),y                         ; 6348 91 AE                    ..
	dey                                     ; 634A 88                       .
	lda     ($AC),y                         ; 634B B1 AC                    ..
	sta     ($AE),y                         ; 634D 91 AE                    ..
	dmv	off_AE, L466F
	lda     L62CF                           ; 6359 AD CF 62                 ..b
	iny                                     ; 635C C8                       .
	sta     ($AE),y                         ; 635D 91 AE                    ..
	lda     L62CE                           ; 635F AD CE 62                 ..b
	dey                                     ; 6362 88                       .
	sta     ($AE),y                         ; 6363 91 AE                    ..
	jsr     sub_6203
	rts                                     ; 6368 60                       `

