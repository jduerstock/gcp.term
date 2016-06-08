
; ----------------------------------------------------------------------------
sub_AD07:	
	pha                                     ; AD07 48                       H
	txa                                     ; AD08 8A                       .
	pha                                     ; AD09 48                       H
	tya                                     ; AD0A 98                       .
	pha                                     ; AD0B 48                       H
	lda     $E3                             ; AD0C A5 E3                    ..
	sta     $D016                           ; AD0E 8D 16 D0                 ...
	lda     $E2                             ; AD11 A5 E2                    ..
	sta     $D409                           ; AD13 8D 09 D4                 ...
	lda     $E7                             ; AD16 A5 E7                    ..
LAD18:  sta     $D01A                           ; AD18 8D 1A D0                 ...
	lda     $E5                             ; AD1B A5 E5                    ..
	sta     $D018                           ; AD1D 8D 18 D0                 ...
	lda     $E4                             ; AD20 A5 E4                    ..
	sta     $D017                           ; AD22 8D 17 D0                 ...
	jsr     sub_ACB2
	pla                                     ; AD28 68                       h
	tay                                     ; AD29 A8                       .
	pla                                     ; AD2A 68                       h
	tax                                     ; AD2B AA                       .
	pla                                     ; AD2C 68                       h
	rti                                     ; AD2D 40                       @

