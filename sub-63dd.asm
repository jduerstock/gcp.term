
; ----------------------------------------------------------------------------
L63D1:  .byte	$00
L63D2:  .byte	$00
L63D3:  .byte	$00
L63D4:  .byte	$00
L63D5:  .byte	$00
L63D6:  .byte	$00
L63D7:  .byte	$00
L63D8:  .byte	$00
L63D9:  .byte	$00
L63DA:  .byte	$00
L63DB:  .byte	$00
L63DC:  .byte	$00

sub_63DD:  
	prolog
	mv	L63DB, L46EF
	shladdm8 off_AE, L46F5, L63DB
	ldp16	L63D9
	shladdi off_AE, L466F, $0001
	ldp16y0	L63D3
	add16i	L63D7, L63D3, $0006
	dmv	off_AE, L63D9
	lda     (off_AE),y
	sta     L63DC                           ; 6443 8D DC 63                 ..c
	add16i	off_AE, L63D3, $0003
	lda     (off_AE),y
	and     #$F0                            ; 6457 29 F0                    ).
	sta     off_AC
	add8m	L63D6, off_AC, L63DC
	add16i	off_AE, L63D3, $0003
	lda     L63D6                           ; 6473 AD D6 63                 ..c
	sta     (off_AE),y
	ldi	$A3, $00
	mv	$A4, L63DC
	ldy     #$17                            ; 6481 A0 17                    ..
	ldxa	L63D7
	jsr     SetBlock
	add16i	off_AE, L63D9, $0001
	ldy     #$00                            ; 649B A0 00                    ..
	lda     (off_AE),y
	sta     $A0                             ; 649F 85 A0                    ..
	ldx     #$02                            ; 64A1 A2 02                    ..
	lda     $A0                             ; 64A3 A5 A0                    ..
	jsr     sub_4983
	sub8i	L63D5, $A0, $01
	yldi	L63D6, $01
L64B5:  lda     #$05                            ; 64B5 A9 05                    ..
	cmp     L63D6                           ; 64B7 CD D6 63                 ..c
	lbcc	L6580
	ldx     L63D6                           ; 64BF AE D6 63                 ..c
	lda     L46EF,x                         ; 64C2 BD EF 46                 ..F
	sta     L63DB                           ; 64C5 8D DB 63                 ..c
	lda     L63DB                           ; 64C8 AD DB 63                 ..c
	cmp     #$0A                            ; 64CB C9 0A                    ..
	lbcc	L64D5
	jmp     L6580                           ; 64D2 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L64D5:	shladdm8 off_AE, L46F5, L63DB
	ldp16	L63D9
	dmv	off_AE, L63D9
	lda     (off_AE),y
	sta     L63DC                           ; 6502 8D DC 63                 ..c
	sec                                     ; 6505 38                       8
	lda     L63D5                           ; 6506 AD D5 63                 ..c
	sbc     #$01                            ; 6509 E9 01                    ..
	sta     off_AE
	add16m8	off_AC, L63D7, off_AE
	lda     (off_AC),y
	ora     #$80                            ; 651E 09 80                    ..
	sta     (off_AC),y
	add16m8	$A0, L63D7, L63D5
	sec                                     ; 6532 38                       8
	lda     #$18                            ; 6533 A9 18                    ..
	sbc     L63D5                           ; 6535 ED D5 63                 ..c
	sta     off_AC
	sub8i	$A2, off_AC, $01
	ldi	$A3, $00
	mv	$A4, L63DC
	ldy     $A2                             ; 654A A4 A2                    ..
	ldxa	$A0
	jsr     SetBlock
	add16i	off_AE, L63D9, $0001
	clc                                     ; 6562 18                       .
	lda     L63D5                           ; 6563 AD D5 63                 ..c
	ldy     #$00                            ; 6566 A0 00                    ..
	adc     (off_AE),y
	sta     L63D5                           ; 656A 8D D5 63                 ..c
	lda     L63D5                           ; 656D AD D5 63                 ..c
	cmp     #$18                            ; 6570 C9 18                    ..
	lbcc	L657A
	jmp     L6580                           ; 6577 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L657A:	inc     L63D6                           ; 657A EE D6 63                 ..c
	jmp     L64B5                           ; 657D 4C B5 64                 L.d

; ----------------------------------------------------------------------------
L6580:	add16i L63D1, L63D3, $001E
	dmv	off_AE, L63D1
	stp16	L63D3
	jsr     sub_636E
	jsr     sub_62D1
	rts                                     ; 65AE 60                       `

