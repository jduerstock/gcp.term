
; ----------------------------------------------------------------------------
L5F07:  .byte	$00
L5F08:  .byte	$00
L5F09:  .byte	$00
L5F0A:  .byte	$00
L5F0B:  .byte	$00
L5F0C:  .byte	$00
L5F0D:  .byte	$00
L5F0E:  .byte	$00
L5F0F:  .byte	$00
L5F10:  .byte	$00
L5F11:  .byte	$00
L5F12:  .byte	$00
L5F13:  .byte	$00
L5F14:	.word	$B280

sub_5F16:  
	stack_prolog L5F07, $04
	ldi	$84, $03
	lda     L5F08                           ; 5F23 AD 08 5F                 .._
	tax                                     ; 5F26 AA                       .
	lda     L5F07                           ; 5F27 AD 07 5F                 .._
	jsr     RShift
	st2xa	L5F0C
	lda     L5F07                           ; 5F34 AD 07 5F                 .._
	and     #$07                            ; 5F37 29 07                    ).
	sta     L5F13                           ; 5F39 8D 13 5F                 .._
	lda     L5F0B                           ; 5F3C AD 0B 5F                 .._
	eor     #$01                            ; 5F3F 49 01                    I.
	lbne	L5F4B
	lda     #$FF                            ; 5F46 A9 FF                    ..
	sta     L5F0B                           ; 5F48 8D 0B 5F                 .._
L5F4B:  lda     L5F13                           ; 5F4B AD 13 5F                 .._
	sta     $84                             ; 5F4E 85 84                    ..
	lda     #$80                            ; 5F50 A9 80                    ..
	ldx     #$00                            ; 5F52 A2 00                    ..
	jsr     RShift
	sta     L5F12                           ; 5F57 8D 12 5F                 .._
	ldy     #$00                            ; 5F5A A0 00                    ..
	sty     L5F0F                           ; 5F5C 8C 0F 5F                 .._
	sty     L5F0E                           ; 5F5F 8C 0E 5F                 .._
	sty     L5F11                           ; 5F62 8C 11 5F                 .._
	iny                                     ; 5F65 C8                       .
	sty     L5F10                           ; 5F66 8C 10 5F                 .._
	dmv	L5F86, L5F09
L5F75:  lda     L5F86                           ; 5F75 AD 86 5F                 .._
	cmp     L5F10                           ; 5F78 CD 10 5F                 .._
	lda     L5F87                           ; 5F7B AD 87 5F                 .._
	sbc     L5F11                           ; 5F7E ED 11 5F                 .._
	bcs     L5F88                           ; 5F81 B0 05                    ..
	jmp     L5FE5                           ; 5F83 4C E5 5F                 L._

; ----------------------------------------------------------------------------
L5F86:  .byte	$00
L5F87:  .byte	$00

; ----------------------------------------------------------------------------
L5F88:	add16m	$A1, L5F14, L5F0C
	mv	$A3, L5F0B
	ldy     $A2                             ; 5F9E A4 A2                    ..
	ldx     $A1                             ; 5FA0 A6 A1                    ..
	lda     L5F12                           ; 5FA2 AD 12 5F                 .._
	jsr     sub_5EFE
	lda     $A0                             ; 5FA8 A5 A0                    ..
	lbne	L5FBA
	rdmv	$A0, L5F0E
	rts                                     ; 5FB9 60                       `

; ----------------------------------------------------------------------------
L5FBA:  inc16	L5F0E
	lsr     L5F12                           ; 5FC2 4E 12 5F                 N._
	lda     L5F12                           ; 5FC5 AD 12 5F                 .._
	lbne	L5FDA
	ldi	L5F12, $80
	inc16	L5F0C
L5FDA:	inc16	L5F10
	jmp     L5F75                           ; 5FE2 4C 75 5F                 Lu_

; ----------------------------------------------------------------------------
L5FE5:	rdmv	$A0, L5F0E
	rts                                     ; 5FEF 60                       `

