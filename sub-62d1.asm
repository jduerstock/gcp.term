
; ----------------------------------------------------------------------------
L62CE:  .byte	$00
L62CF:  .byte	$00
L62D0:  .byte	$00

; ----------------------------------------------------------------------------
sub_62D1:
	prolog
	mv	L62D0, SDMCTL
	yldi	SDMCTL, $00
	shladdi	off_AE, L466F, $0001
	iny                                     ; 62F2 C8                       .
	lda     (off_AE),y
	sta     DLIST+1
	dey                                     ; 62F8 88                       .
	lda     (off_AE),y
	sta     DLIST
	lda     L62D0                           ; 62FE AD D0 62                 ..b
	sta     SDMCTL
	ldi	$D40E, $C0
	shladdi off_AE, L466F, $0001
	iny                                     ; 631C C8                       .
	lda     (off_AE),y
	sta     L62CF                           ; 631F 8D CF 62                 ..b
	dey                                     ; 6322 88                       .
	lda     (off_AE),y
	sta     L62CE                           ; 6325 8D CE 62                 ..b
L6328:	shladdi	off_AE, L466F, $0001
	dmv	off_AC, L466F
	iny                                     ; 6345 C8                       .
	lda     ($AC),y                         ; 6346 B1 AC                    ..
	sta     (off_AE),y
	dey                                     ; 634A 88                       .
	lda     ($AC),y                         ; 634B B1 AC                    ..
	sta     (off_AE),y
	dmv	off_AE, L466F
	lda     L62CF                           ; 6359 AD CF 62                 ..b
	iny                                     ; 635C C8                       .
	sta     (off_AE),y
	lda     L62CE                           ; 635F AD CE 62                 ..b
	dey                                     ; 6362 88                       .
	sta     (off_AE),y
	jsr     sub_6203
	rts                                     ; 6368 60                       `

