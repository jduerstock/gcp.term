
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
	ldp16y0	DLIST
	mv	SDMCTL, L62D0
	ldi	$D40E, $C0
	shladdi off_AE, L466F, $0001
	ldp16y0	L62CE
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

