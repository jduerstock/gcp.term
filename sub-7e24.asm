
; ----------------------------------------------------------------------------
L7E12:  .byte	$00
L7E13:	.byte	$00
L7E14:  .byte	$00
L7E15:  .byte	$00
L7E16:  .byte	$00
L7E17:  .byte	$00
L7E18:  .byte	$00
L7E19:  .byte	$00
L7E1A:  .byte	$00
L7E1B:  .byte	$00
L7E1C:  .byte	$00
L7E1D:  .byte	$00
L7E1E:  .byte	$00,$00
L7E20:  .byte	$00
L7E21:  .byte	$00
L7E22:  .byte	$00
L7E23:  .byte	$00

; ----------------------------------------------------------------------------
sub_7E24:
	stack_prolog L7E12, $02
	lda	L7E13
	jsr	sub_65B0
	rdmv	L7E15, $A0
	add16i	off_AE, L7E15, $0009
	ldp16	L7E1E
	test16	L7E1E
	lbne	L7E69
L7E64:	ldi	$A0, $00
	rts                                     ; 7E68 60                       `

; ----------------------------------------------------------------------------
L7E69:	dmv	off_AE, L7E1E
	ldp16	L7E1C
	add16i	off_AE, L7E1E, $0002
	lda     #$00                            ; 7E8F A9 00                    ..
	sta     $85                             ; 7E91 85 85                    ..
	lda     #$03                            ; 7E93 A9 03                    ..
	sta     $84                             ; 7E95 85 84                    ..
	lda     L7E12                           ; 7E97 AD 12 7E                 ..~
	ldx     #$00                            ; 7E9A A2 00                    ..
	jsr     MultI
	st2xa	off_AC
	add16m	L7E1A, $AE, $AC
	mv	$A3, L7E1A+1
	rdldi	$A4, $0003
	ldy     L7E1A                           ; 7EC0 AC 1A 7E                 ..~
	ldxai	L7E21
	jsr     blockmove
	lda     L7E22                           ; 7ECA AD 22 7E                 ."~
	lbne	L7ED7
	ldi	$A0, $00
	rts                                     ; 7ED6 60                       `

; ----------------------------------------------------------------------------
L7ED7:	dmv	off_AE, L7E1C
	add8m	$AC, L7E23, L7E22
	ldy     #$00                            ; 7EEA A0 00                    ..
	lda     (off_AE),y
	cmp     $AC                             ; 7EEE C5 AC                    ..
	lbcs	L7EFA
L7EF5:	ldi	$A0, $00
	rts                                     ; 7EF9 60                       `

; ----------------------------------------------------------------------------
L7EFA:	add16m8	off_AE, L7E1C, L7E23
	add16i	L7E17, off_AE, $0001
	ldy     #$00                            ; 7F19 A0 00                    ..
	sty     L7E19                           ; 7F1B 8C 19 7E                 ..~
	sty     L7E20                           ; 7F1E 8C 20 7E                 . ~
	sub8i	L7F35, L7E22, $01
L7F2A:  lda     L7F35                           ; 7F2A AD 35 7F                 .5.
	cmp     L7E20                           ; 7F2D CD 20 7E                 . ~
	bcs     L7F36                           ; 7F30 B0 04                    ..
	jmp     L7F5F                           ; 7F32 4C 5F 7F                 L_.

; ----------------------------------------------------------------------------
L7F35:  .byte	$00

; ----------------------------------------------------------------------------
L7F36:  add16m8 off_AE, L7E17, L7E20
	ldy     #$00                            ; 7F46 A0 00                    ..
	lda     (off_AE),y
	eor     L7E14                           ; 7F4A 4D 14 7E                 M.~
	lbne	L7F59
	iny                                     ; 7F52 C8                       .
	sty     L7E19                           ; 7F53 8C 19 7E                 ..~
	jmp     L7F5F                           ; 7F56 4C 5F 7F                 L_.

; ----------------------------------------------------------------------------
L7F59:  inc     L7E20                           ; 7F59 EE 20 7E                 . ~
	jmp     L7F2A                           ; 7F5C 4C 2A 7F                 L*.

; ----------------------------------------------------------------------------
L7F5F:  lda     L7E21                           ; 7F5F AD 21 7E                 .!~
	eor     L7E19                           ; 7F62 4D 19 7E                 M.~
	sta     $AE                             ; 7F65 85 AE                    ..
	lda     $AE                             ; 7F67 A5 AE                    ..
	eor     #$01                            ; 7F69 49 01                    I.
	sta     L7E19                           ; 7F6B 8D 19 7E                 ..~
	mv	$A0, L7E19
	rts                                     ; 7F73 60                       `

