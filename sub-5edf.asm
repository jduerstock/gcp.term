
; ----------------------------------------------------------------------------

sub_5EDF:
	sta     $A0                             ; 5EDF 85 A0                    ..
	stx     $A1                             ; 5EE1 86 A1                    ..
	sty     $A2                             ; 5EE3 84 A2                    ..
	ldy     #$00                            ; 5EE5 A0 00                    ..
	and     ($A1),y                         ; 5EE7 31 A1                    1.
	sta     $A4                             ; 5EE9 85 A4                    ..
	ldy     #$01                            ; 5EEB A0 01                    ..
	lda     $A0                             ; 5EED A5 A0                    ..
	and     $A3                             ; 5EEF 25 A3                    %.
	cmp     $A4                             ; 5EF1 C5 A4                    ..
	beq     L5EF7                           ; 5EF3 F0 02                    ..
	ldy     #$00                            ; 5EF5 A0 00                    ..
L5EF7:  sty     $A0                             ; 5EF7 84 A0                    ..
	rts                                     ; 5EF9 60                       `

