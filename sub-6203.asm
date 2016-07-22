
.ifndef MONOLITH
	.include "action.inc"

	.import off_AC: zeropage
	.import off_AE: zeropage

	.import L466F

	.import	blockmove
.endif

; ----------------------------------------------------------------------------
L61FF:  .byte	$00,$00
L6201:  .byte	$00,$00

; ----------------------------------------------------------------------------
sub_6203:  
	prolog
	shladdi	off_AE, L466F, $0001
	ldp16	$A0
	dmv	off_AC, L466F
	ld2p16	$A2, $AC
	rdldi	$A4, $0020
	ldy     $A2                             ; 6240 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	shladdi	off_AE, L466F, $0001
	ldp16	L61FF
	add16i	off_AE, L61FF, $0003
	add16i	L6201, off_AE, $0001
	dmv	off_AE, L6201
;--		*L6201 = L61FF + 0x20;
	clc                                     ; 6291 18                       .
	lda     L61FF                           ; 6292 AD FF 61                 ..a
	adc     #$20                            ; 6295 69 20                    i 
	sta     $AC                             ; 6297 85 AC                    ..
	lda     L61FF+1
	adc     #$00                            ; 629C 69 00                    i.
	iny                                     ; 629E C8                       .
	sta     ($AE),y                         ; 629F 91 AE                    ..
	lda     $AC                             ; 62A1 A5 AC                    ..
	dey                                     ; 62A3 88                       .
	sta     ($AE),y                         ; 62A4 91 AE                    ..
;--
	add16i	L6201, L61FF, $001E
	dmv	off_AE, L6201
	lda     L61FF+1
	iny                                     ; 62C4 C8                       .
	sta     ($AE),y                         ; 62C5 91 AE                    ..
	lda     L61FF                           ; 62C7 AD FF 61                 ..a
	dey                                     ; 62CA 88                       .
	sta     ($AE),y                         ; 62CB 91 AE                    ..
	rts                                     ; 62CD 60                       `

