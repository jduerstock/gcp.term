
; ----------------------------------------------------------------------------
sub_ACB2:  
	lda     $A0                             ; ACB2 A5 A0                    ..
	pha                                     ; ACB4 48                       H
	lda	off_AE
	pha
	lda	off_AE+1
	pha
	ldx     $E8                             ; ACBB A6 E8                    ..
	lda     L46EF,x                         ; ACBD BD EF 46                 ..F
	sta     $A0                             ; ACC0 85 A0                    ..
	shladdm8 off_AE, L46F5, $A0
	ldy     #$01                            ; ACD5 A0 01                    ..
	lda     (off_AE),y
	sta     $E1                             ; ACD9 85 E1                    ..
	dey                                     ; ACDB 88                       .
	lda     (off_AE),y
	sta     $E0                             ; ACDE 85 E0                    ..
	inc     $E8                             ; ACE0 E6 E8                    ..
	ldy     #$02                            ; ACE2 A0 02                    ..
	lda     ($E0),y                         ; ACE4 B1 E0                    ..
	sta     $E2                             ; ACE6 85 E2                    ..
	iny                                     ; ACE8 C8                       .
	lda     ($E0),y                         ; ACE9 B1 E0                    ..
	sta     $E3                             ; ACEB 85 E3                    ..
	iny                                     ; ACED C8                       .
	lda     ($E0),y                         ; ACEE B1 E0                    ..
	sta     $E4                             ; ACF0 85 E4                    ..
	iny                                     ; ACF2 C8                       .
	lda     ($E0),y                         ; ACF3 B1 E0                    ..
	sta     $E5                             ; ACF5 85 E5                    ..
	iny                                     ; ACF7 C8                       .
	iny                                     ; ACF8 C8                       .
	lda     ($E0),y                         ; ACF9 B1 E0                    ..
	sta     $E7                             ; ACFB 85 E7                    ..
	pla                                     ; ACFD 68                       h
	sta     off_AE+1
	pla                                     ; AD00 68                       h
	sta	off_AE
	pla                                     ; AD03 68                       h
	sta	$A0
	rts                                     ; AD06 60                       `

