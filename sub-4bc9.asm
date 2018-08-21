
; ----------------------------------------------------------------------------
sub_4BC9:
	sta	$A0
	and	#$60
	sta	$A2				; $A2 = $A0 & $60
	asl	a
	sta	$A3				; $A3 = $A2 << 1
	eor	$A2				; $A1 = ($A3 ^ $A2 ^ $FF & $40) >> 1
	eor	#$FF
	and	#$40
	lsr	a
	sta	$A1				; $A1 = ($A3 ^ $A2 ^ $FF & $40) >> 1
	lda	$A0
	and	#$9F
	sta	$A0				; $A0 = $A0 & $9F
	lda	$A3
	and	#$40
	ora	$A0
	ora	$A1
	sta	$A0				; $A0 = $A3 & $40 | $A0 | $A1
	rts

