
; ----------------------------------------------------------------------------
sub_AD2E:
	yldi	$021B, $00
	ldi	$021A, $1E
	ldy     #$01                            ; AD38 A0 01                    ..
	lda     $D5                             ; AD3A A5 D5                    ..
	beq     LAD44                           ; AD3C F0 06                    ..
	dec     $D5                             ; AD3E C6 D5                    ..
	bne     LAD44                           ; AD40 D0 02                    ..
	sty     $D8                             ; AD42 84 D8                    ..
LAD44:  lda     $D6                             ; AD44 A5 D6                    ..
	beq     LAD4E                           ; AD46 F0 06                    ..
	dec     $D6                             ; AD48 C6 D6                    ..
	bne     LAD4E                           ; AD4A D0 02                    ..
	sty     $D9                             ; AD4C 84 D9                    ..
LAD4E:  lda     $D7                             ; AD4E A5 D7                    ..
	beq     LAD58                           ; AD50 F0 06                    ..
	dec     $D7                             ; AD52 C6 D7                    ..
	bne     LAD58                           ; AD54 D0 02                    ..
	sty     $DA                             ; AD56 84 DA                    ..
LAD58:  rts                                     ; AD58 60                       `

