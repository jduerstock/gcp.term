
sub_8819:  
	sta     $A0                             ; 8819 85 A0                    ..
	stx     $A1                             ; 881B 86 A1                    ..
	sty     $A2                             ; 881D 84 A2                    ..
	ldy     #$00                            ; 881F A0 00                    ..
	beq     L882C                           ; 8821 F0 09                    ..
L8823:  lda     ($A2),y                         ; 8823 B1 A2                    ..
	cmp     #$41                            ; 8825 C9 41                    .A
	beq     :+
	sta     ($A0),y                         ; 8829 91 A0                    ..
:	iny                                     ; 882B C8                       .
L882C:  cpy     $A4                             ; 882C C4 A4                    ..
	bne     L8823                           ; 882E D0 F3                    ..
	rts                                     ; 8830 60                       `

