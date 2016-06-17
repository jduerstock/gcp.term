
; ----------------------------------------------------------------------------
LA52E:  .byte	$00
LA52F:  .byte	$00
LA530:  .byte	$00
LA531:  .byte	$00
LA532:  .byte	$00
LA533:  .byte	$00
LA534:  .byte	$00,$00
LA536:  .byte	$00
LA537:  .byte	$00
LA538:  .byte	$00
LA539:  .byte	$00
LA53A:  .word	$0000
LA53C:  .byte	$00
LA53D:  .byte	$00
LA53E:  .byte	$00
LA53F:  .byte	$00
LA540:  .byte	$00
LA541:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA545:  .byte	$00
LA546:  .byte	$00

cmd_d5:
	stack_prolog LA52E, $03
	func16_8 sub_65B0, LA534, LA52E
	test16	LA534
	lbne	LA56C
LA56B:  rts                                     ; A56B 60                       `

; ----------------------------------------------------------------------------
LA56C:	blkmv_imi LA53D, LA530, $0004
	sub8m	off_AE, LA53F, LA53D
LA58C:	add8i	LA536, off_AE, $01
	blkmv_imi LA541, LA534, $0006
	shladdm8 off_AE, LA545, LA53E
	clc                                     ; A5BF 18                       .
	ldy     #$00                            ; A5C0 A0 00                    ..
	lda     ($AE),y                         ; A5C2 B1 AE                    ..
	adc     LA53D                           ; A5C4 6D 3D A5                 m=.
	sta     $AC                             ; A5C7 85 AC                    ..
	iny                                     ; A5C9 C8                       .
	lda     ($AE),y                         ; A5CA B1 AE                    ..
	adc     #$00                            ; A5CC 69 00                    i.
	sta     $AD                             ; A5CE 85 AD                    ..
	sub16i	LA532, off_AC, $0001
	jsr     cmd_d0
	ldy     #$00                            ; A5E2 A0 00                    ..
	sty     LA53A+1
	sty     LA53A                           ; A5E7 8C 3A A5                 .:.
	mv	LA537, LA53E
	mv	LA601, LA540
LA5F6:  lda     LA601                           ; A5F6 AD 01 A6                 ...
	cmp     LA537                           ; A5F9 CD 37 A5                 .7.
	bcs     LA602                           ; A5FC B0 04                    ..
	jmp     LA68A                           ; A5FE 4C 8A A6                 L..

; ----------------------------------------------------------------------------
LA601:  .byte	$00

; ----------------------------------------------------------------------------
LA602:	yldi	LA538, $01
	mv	LA618, LA536
LA60D:  lda     LA618                           ; A60D AD 18 A6                 ...
	cmp     LA538                           ; A610 CD 38 A5                 .8.
	bcs     LA619                           ; A613 B0 04                    ..
	jmp     LA662                           ; A615 4C 62 A6                 Lb.

; ----------------------------------------------------------------------------
LA618:  .byte	$00

; ----------------------------------------------------------------------------
LA619:	func8_8	sub_45A3, LA539, LA52F
	ldx	LA52F				; A624 AE 2F A5                 ./.
	lda     L05C0,x                         ; A627 BD C0 05                 ...
	sta     LA53C                           ; A62A 8D 3C A5                 .<.
	lda     LA53C                           ; A62D AD 3C A5                 .<.
	bne     LA63A                           ; A630 D0 08                    ..
	lda     L464D                           ; A632 AD 4D 46                 .MF
	bne     LA63A                           ; A635 D0 03                    ..
	jmp     LA63D                           ; A637 4C 3D A6                 L=.

; ----------------------------------------------------------------------------
LA63A:  jmp     LA662                           ; A63A 4C 62 A6                 Lb.

; ----------------------------------------------------------------------------
LA63D:	add16m8 off_AE, LA532, LA538
	stp8	LA539
	inc16	LA53A
	inc     LA538                           ; A65C EE 38 A5                 .8.
	jmp     LA60D                           ; A65F 4C 0D A6                 L..

; ----------------------------------------------------------------------------
LA662:  lda     LA53C                           ; A662 AD 3C A5                 .<.
	bne     LA66F                           ; A665 D0 08                    ..
	lda     L464D                           ; A667 AD 4D 46                 .MF
	bne     LA66F                           ; A66A D0 03                    ..
	jmp     LA672                           ; A66C 4C 72 A6                 Lr.

; ----------------------------------------------------------------------------
LA66F:  jmp     LA68A                           ; A66F 4C 8A A6                 L..

; ----------------------------------------------------------------------------
LA672:	add16m8 LA532, LA532, LA541
	inc     LA537                           ; A684 EE 37 A5                 .7.
	jmp     LA5F6                           ; A687 4C F6 A5                 L..

; ----------------------------------------------------------------------------
LA68A:  jsr     cmd_d1
	mv	LA537, LA53A
	ldi	$84, $08
	ld2xa	LA53A
	jsr	RShift
	sta     LA538                           ; A6A1 8D 38 A5                 .8.
	jmp     LA6AB                           ; A6A4 4C AB A6                 L..

; ----------------------------------------------------------------------------
LA6A7:	.byte	$03,"CBB"

; ----------------------------------------------------------------------------
LA6AB:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, LA538
	ldi	$A7, $00
	mv	$A6, LA537
	ldy     #$04                            ; A6C1 A0 04                    ..
	ldxai	LA6A7
	jsr     sub_55A0
	rts                                     ; A6CA 60                       `

