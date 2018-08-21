
; ----------------------------------------------------------------------------
cvt_atascii_from_antic:  
	tay                                     ; 4B7B A8                       .
	and     #$80                            ; 4B7C 29 80                    ).
	sta     $A0                             ; 4B7E 85 A0                    ..
	tya                                     ; 4B80 98                       .
	and     #$7F                            ; 4B81 29 7F                    ).
	cmp     #$20                            ; 4B83 C9 20                    . 
	bcs	:+
	adc     #$40                            ; 4B87 69 40                    i@
	bcc     :++
:	cmp     #$60                            ; 4B8B C9 60                    .`
	bcs     :+
	sec                                     ; 4B8F 38                       8
	sbc     #$20                            ; 4B90 E9 20                    . 
:	ora     $A0                             ; 4B92 05 A0                    ..
	sta     $A0                             ; 4B94 85 A0                    ..
	rts                                     ; 4B96 60                       `

