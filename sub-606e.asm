
; ----------------------------------------------------------------------------
L6064:  .word	$0000
L6066:  .byte	$00
L6067:  .byte	$00
L6068:  .byte	$00
L6069:  .byte	$00
L606A:  .byte	$00
L606B:  .byte	$00
L606C:  .byte	$00
L606D:  .byte	$00

sub_606E:
	prolog
	stxa	L6064
	add16i	off_AE, L6064, $0003
	ldi	$84, $02
	ld2xa	off_AE
	jsr     RShift
	st2xa	L6066
	ldy     #$00                            ; 6099 A0 00                    ..
	sty     L6069                           ; 609B 8C 69 60                 .i`
	sty     L6068                           ; 609E 8C 68 60                 .h`
L60A1:	lda     L6068                           ; 60A1 AD 68 60                 .h`
	cmp     #$F8                            ; 60A4 C9 F8                    ..
	lda     L6069                           ; 60A6 AD 69 60                 .i`
	sbc     #$07                            ; 60A9 E9 07                    ..
	lbcs	L6177
	sec                                     ; 60B0 38                       8
	lda     #$F8                            ; 60B1 A9 F8                    ..
	sbc     L6068                           ; 60B3 ED 68 60                 .h`
	sta     $AE                             ; 60B6 85 AE                    ..
	lda     #$07                            ; 60B8 A9 07                    ..
	sbc     L6069                           ; 60BA ED 69 60                 .i`
	sta     $AF                             ; 60BD 85 AF                    ..
	ldy     $A2                             ; 60BF A4 A2                    ..
	ldxa	L6068
	jsr     sub_6026
	add16m	L6068, L6068, $A0
	lda     L6068                           ; 60DB AD 68 60                 .h`
	cmp     #$F8                            ; 60DE C9 F8                    ..
	lda     L6069                           ; 60E0 AD 69 60                 .i`
	sbc     #$07                            ; 60E3 E9 07                    ..
	lbcc	L60ED
	jmp     L6177                           ; 60EA 4C 77 61                 Lwa

; ----------------------------------------------------------------------------
L60ED:  lda     L6067                           ; 60ED AD 67 60                 .g`
	sta     $A3                             ; 60F0 85 A3                    ..
	lda     #$01                            ; 60F2 A9 01                    ..
	sta     $A4                             ; 60F4 85 A4                    ..
	ldy     L6066                           ; 60F6 AC 66 60                 .f`
	ldxa	L6068
	jsr     sub_6026
	rdmv	L606A, $A0
	lda     L606A                           ; 610C AD 6A 60                 .j`
	cmp     L6066                           ; 610F CD 66 60                 .f`
	lda     L606B                           ; 6112 AD 6B 60                 .k`
	sbc     L6067                           ; 6115 ED 67 60                 .g`
	lbcc	L6161
	lda     L6067                           ; 611D AD 67 60                 .g`
	sta     $A3                             ; 6120 85 A3                    ..
	lda     #$00                            ; 6122 A9 00                    ..
	sta     $A4                             ; 6124 85 A4                    ..
	ldy     L6066                           ; 6126 AC 66 60                 .f`
	ldxa	L6068
	jsr     sub_5FF5
	ldi	$84, $02
	ld2xa	L6068
	jsr     LShift
	st2xa	off_AE
	add16m	L606C, MEMLO, off_AE
	rdmv	$A0, L606C
	rts                                     ; 6160 60                       `

; ----------------------------------------------------------------------------
L6161:	add16m	L6068, L6068, L606A
	jmp	L60A1

; ----------------------------------------------------------------------------
L6177:  jmp     L617C                           ; 6177 4C 7C 61                 L|a

; ----------------------------------------------------------------------------
L617A:	.byte	$01,"C"

; ----------------------------------------------------------------------------
L617C:	ldi	$A3, $00
	ldy     #$26                            ; 6180 A0 26                    .&
	ldxai	L617A
	jsr     sub_55A0
	rdldi	$A0, $00
	rts                                     ; 6191 60                       `

