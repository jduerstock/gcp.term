
; ----------------------------------------------------------------------------

sub_5EC4:
	sta     $A0                             ; 5EC4 85 A0                    ..
	stx     $A1                             ; 5EC6 86 A1                    ..
	sty     $A2                             ; 5EC8 84 A2                    ..
	ldy     #$00                            ; 5ECA A0 00                    ..
	eor     #$FF                            ; 5ECC 49 FF                    I.
	and     ($A1),y                         ; 5ECE 31 A1                    1.
	sta     $A4                             ; 5ED0 85 A4                    ..
	lda     $A0                             ; 5ED2 A5 A0                    ..
	and     $A3                             ; 5ED4 25 A3                    %.
	ora     $A4                             ; 5ED6 05 A4                    ..
	sta     ($A1),y                         ; 5ED8 91 A1                    ..
	lda     #$01                            ; 5EDA A9 01                    ..
	sta     $A0                             ; 5EDC 85 A0                    ..
	rts                                     ; 5EDE 60                       `

