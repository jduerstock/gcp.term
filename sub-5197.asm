
; ----------------------------------------------------------------------------
sub_5197:  
	sta     $A0                             ; 5197 85 A0                    ..
	stx     $A1                             ; 5199 86 A1                    ..
	sty     $A2                             ; 519B 84 A2                    ..
	yldi	$A6, $00
	sty     $A5                             ; 51A1 84 A5                    ..
L51A3:  lda     $A5                             ; 51A3 A5 A5                    ..
	cmp     $A4                             ; 51A5 C5 A4                    ..
	lbcs	L51F1
	ldy     $A5                             ; 51AC A4 A5                    ..
	lda     ($A2),y                         ; 51AE B1 A2                    ..
	sta     $A7                             ; 51B0 85 A7                    ..
	and8i	$A8, $A7, $7F
	lda     $A8                             ; 51B8 A5 A8                    ..
	eor     $A7                             ; 51BA 45 A7                    E.
	lbeq	L51E4
L51C1:  inc     $A5                             ; 51C1 E6 A5                    ..
	ldy     $A5                             ; 51C3 A4 A5                    ..
	lda     ($A2),y                         ; 51C5 B1 A2                    ..
	sta     $A7                             ; 51C7 85 A7                    ..
	inc     $A5                             ; 51C9 E6 A5                    ..
L51CB:  lda     #$00                            ; 51CB A9 00                    ..
	cmp     $A8                             ; 51CD C5 A8                    ..
	lbcs	L51E1
	lda     $A7                             ; 51D4 A5 A7                    ..
	ldy     $A6                             ; 51D6 A4 A6                    ..
	sta     ($A0),y                         ; 51D8 91 A0                    ..
	inc     $A6                             ; 51DA E6 A6                    ..
	dec     $A8                             ; 51DC C6 A8                    ..
	jmp     L51CB                           ; 51DE 4C CB 51                 L.Q

; ----------------------------------------------------------------------------
L51E1:  jmp     L51EE                           ; 51E1 4C EE 51                 L.Q

; ----------------------------------------------------------------------------
L51E4:  lda     $A7                             ; 51E4 A5 A7                    ..
	ldy     $A6                             ; 51E6 A4 A6                    ..
	sta     ($A0),y                         ; 51E8 91 A0                    ..
	inc     $A6                             ; 51EA E6 A6                    ..
	inc     $A5                             ; 51EC E6 A5                    ..
L51EE:  jmp     L51A3                           ; 51EE 4C A3 51                 L.Q

; ----------------------------------------------------------------------------
L51F1:  lda     $A6                             ; 51F1 A5 A6                    ..
	sta     L4649                           ; 51F3 8D 49 46                 .IF
	rts                                     ; 51F6 60                       `
