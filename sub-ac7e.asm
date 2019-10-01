
; ----------------------------------------------------------------------------
; process keyboard interrupt
;

sub_AC7E:
	txa                                     ; AC7E 8A                       .
	pha                                     ; AC7F 48                       H
	lda     KBCODE
	eor     CH1
	bne     LAC8D                           ; AC86 D0 05                    ..
	lda     KEYDEL
	bne     LACA9                           ; AC8B D0 1C                    ..
LAC8D:  lda	KBCODE
	sta     CH1
	ldx     L474D                           ; AC93 AE 4D 47                 .MG
	sta     LB138,x                         ; AC96 9D 38 B1                 .8.
	inc     L474D                           ; AC99 EE 4D 47                 .MG
	and8i	L474D, L474D, $0F
	ldx     #$01                            ; ACA4 A2 01                    ..
	stx     KEYDEL
LACA9:	ldi	SRTIMR, $11
	pla                                     ; ACAE 68                       h
	tax                                     ; ACAF AA                       .
	pla                                     ; ACB0 68                       h
	rti                                     ; ACB1 40                       @

