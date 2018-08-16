
; ----------------------------------------------------------------------------
L90C1:  .byte	$00
L90C2:  .byte	$00
L90C3:  .byte	$00
L90C4:  .byte	$00
L90C5:  .byte	$00
L90C6:  .byte	$00
L90C7:  .byte	$00
L90C8:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L90CC:  .byte	$00
L90CD:  .byte	$00

; ----------------------------------------------------------------------------
sub_90CE:
	prolog
	stx     L90C2                           ; 90D1 8E C2 90                 ...
	sta     L90C1                           ; 90D4 8D C1 90                 ...
	lda     L90C1                           ; 90D7 AD C1 90                 ...
	jsr     sub_65B0
	rdmv	L90C3, $A0
	lda     L90C4                           ; 90E7 AD C4 90                 ...
	sta     $A3                             ; 90EA 85 A3                    ..
	lda     #$00                            ; 90EC A9 00                    ..
	sta     $A5                             ; 90EE 85 A5                    ..
	lda     #$06                            ; 90F0 A9 06                    ..
	sta     $A4                             ; 90F2 85 A4                    ..
	ldy     L90C3                           ; 90F4 AC C3 90                 ...
	ldxai	L90C8
	jsr     blockmove
	shladdm8 off_AE, L90CC, L90C2
	ldy     #$01
	lda     (off_AE),y
	sta     L90C6                           ; 9116 8D C6 90                 ...
	dey                                     ; 9119 88                       .
	lda     (off_AE),y
	sta     L90C5                           ; 911C 8D C5 90                 ...
	ldy     L90C8                           ; 911F AC C8 90                 ...
	ldxa	L90C5
	jsr     sub_4B97
	lda     $A0                             ; 912B A5 A0                    ..
	sta     L90C7                           ; 912D 8D C7 90                 ...
	lda     L90C7                           ; 9130 AD C7 90                 ...
	sta     $A0                             ; 9133 85 A0                    ..
	rts                                     ; 9135 60                       `

