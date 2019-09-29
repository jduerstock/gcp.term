
; ----------------------------------------------------------------------------
L9361:  .byte	$00
L9362:  .byte	$00
L9363:  .byte	$00
L9364:  .byte	$00
L9365:  .byte	$00
L9366:  .byte	$00
	.byte	$00
L9368:  .byte	$00
	.byte	$00

; ----------------------------------------------------------------------------
sub_936A:  
	prolog
	yldi	L9363, $00
	add16i	off_AE, P9055, $0002
	lda     (off_AE),y
	sta     L9361                           ; 9383 8D 61 93                 .a.
	add16i	$A2, P9055, $0016
	rdldi	$A4, $0004
	ldy     $A2                             ; 939D A4 A2                    ..
	ldxai	L9366
	jsr     blockmove
	mv	L9362, L905E
	add8m	L9366, L9366, L9361
	add8m	L9368, L9368, L9361
	ldx     L9366                           ; 93C0 AE 66 93                 .f.
	lda     L9362                           ; 93C3 AD 62 93                 .b.
	jsr     sub_4955
	lda     $A0                             ; 93C9 A5 A0                    ..
	sta     L9364                           ; 93CB 8D 64 93                 .d.
	ifm8eqi	L9364, $01, L93E5
	sub8m	L9363, L9366, L9362
	jmp     L9405                           ; 93E2 4C 05 94                 L..

; ----------------------------------------------------------------------------
L93E5:  ldx     L9368                           ; 93E5 AE 68 93                 .h.
	lda     L9362                           ; 93E8 AD 62 93                 .b.
	jsr     sub_4955
	lda     $A0                             ; 93EE A5 A0                    ..
	sta     L9365                           ; 93F0 8D 65 93                 .e.
	lda     L9365                           ; 93F3 AD 65 93                 .e.
	lbne	L9405
	sub8m	L9363, L9368, L9362
L9405:  lda     #$80                            ; 9405 A9 80                    ..
	sta     $A3                             ; 9407 85 A3                    ..
	ldy     L9363                           ; 9409 AC 63 93                 .c.
	ldxa	L9053
	jsr     cmd_uv
	rts                                     ; 9415 60                       `

