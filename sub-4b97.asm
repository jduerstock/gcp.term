
; ----------------------------------------------------------------------------
rtrim_null:					; remove nulls from end of string length
	sta	off_AE
	stx	off_AE+1
:	dey
	bmi	:+
	lda	(off_AE),y
	beq	:-
:	iny
	tya
	sta	$A0
	rts

