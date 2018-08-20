
; ----------------------------------------------------------------------------
read_clock:  
	lda	RTCLOK+1
	ldy	RTCLOK+2
	cmp	RTCLOK+1
	beq	:+
	lda	RTCLOK+1
	ldy     #$00                            ; 4BC2 A0 00                    ..
:	sta     $A1                             ; 4BC4 85 A1                    ..
	sty     $A0                             ; 4BC6 84 A0                    ..
	rts                                     ; 4BC8 60                       `

