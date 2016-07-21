
.ifndef MONOLITH
	.include "action.inc"

	.import off_AC: zeropage
	.import off_AE: zeropage

	.import L464B
	.import L4650
	.import L4653 
	.import L466F
	.import L474C
	.import LB16A

	.import SArgs

	.import tohex
	.import sub_4BC9
	.import sub_51F7
	.import sub_52E1
	.import sub_5394
.endif

; ----------------------------------------------------------------------------
L557E:	.byte	$30,$5D
L5580:	.byte	$03,$00,$00,$11,$20
	.byte	"   " 
	.byte	"   " 
	.byte   $73                             ; 558B 73                       s
L558C:	.byte	$69,$7A
L558E:	.byte	$65,$20
L5590:	.byte	$3D                             ; 5590 3D                       =
L5591:	.byte	$3D                             ; 5591 3D                       =
L5592:	.byte	$2D                             ; 5592 2D                       -
L5593:	.byte	$20                             ; 5593 20                        
L5594:	.byte	$31                             ; 5594 31                       1
L5595:  cld                                     ; 5595 D8                       .
	dec     $1E,x                           ; 5596 D6 1E                    ..
L5598:	.byte	$02                             ; 5598 02                       .
L5599:  .byte	$00
L559A:  .byte	$00,$12
L559C:	.byte	$20,$20
L559E:	.byte	$20,$20

; ----------------------------------------------------------------------------
sub_55A0:
	stack_prolog L557E, $0D
	lda     #$00                            ; 55A9 A9 00                    ..
	cmp     L4653                           ; 55AB CD 53 46                 .SF
	lbcs	L55B4
	rts

; ----------------------------------------------------------------------------
L55B4:	shladdi	off_AE, L466F, $0001
	ldp16	L559A
	add16i	L559C, L559A, $0004
	dmv	off_AE, L559C
	clc                                     ; 55EF 18                       .
	lda     ($AE),y                         ; 55F0 B1 AE                    ..
	adc     #$02                            ; 55F2 69 02                    i.
	sta     L559E                           ; 55F4 8D 9E 55                 ..U
	iny                                     ; 55F7 C8                       .
	lda     ($AE),y                         ; 55F8 B1 AE                    ..
	adc     #$00                            ; 55FA 69 00                    i.
	sta     L559E+1
	rdldi	L558C, L5580
	dey                                     ; 5609 88                       .
	sty     L5594                           ; 560A 8C 94 55                 ..U
	iny                                     ; 560D C8                       .
	sty     L5590                           ; 560E 8C 90 55                 ..U
	dmv	off_AE, L557E
	dey                                     ; 561B 88                       .
	lda     ($AE),y                         ; 561C B1 AE                    ..
	sta     L562C                           ; 561E 8D 2C 56                 .,V
L5621:  lda     L562C                           ; 5621 AD 2C 56                 .,V
	cmp     L5590                           ; 5624 CD 90 55                 ..U
	bcs     L562D                           ; 5627 B0 04                    ..
	jmp     L587E                           ; 5629 4C 7E 58                 L~X

; ----------------------------------------------------------------------------
L562C:	.byte	$30                             ; 562C 30                       0

; ----------------------------------------------------------------------------
L562D:	add16m8	off_AE, L557E, L5590
	ldp8	L5591
	lda     L5591                           ; 5644 AD 91 55                 ..U
	eor     #'C'
	lbne	L5672
	add16m8	off_AE, L559E, L5594
	dmv	off_AC, L558C
	lda     (off_AC),y
	sta     (off_AE),y
	inc     L5594                           ; 566C EE 94 55                 ..U
	jmp     L5867                           ; 566F 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L5672:  lda     L5591                           ; 5672 AD 91 55                 ..U
	eor     #'c'
	lbne	L56B7
	add16m8	off_AE, L559E, L5594
	push16	off_AE
	dmv	off_AE, L558C
	lda     (off_AE),y
	sta     $A0                             ; 569E 85 A0                    ..
	lda     $A0                             ; 56A0 A5 A0                    ..
	jsr     sub_4BC9
	pull16	off_AE
	lda     $A0                             ; 56AB A5 A0                    ..
	ldy     #$00                            ; 56AD A0 00                    ..
	sta     ($AE),y                         ; 56AF 91 AE                    ..
	inc     L5594                           ; 56B1 EE 94 55                 ..U
	jmp     L5867                           ; 56B4 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L56B7:  lda     L5591                           ; 56B7 AD 91 55                 ..U
	eor     #'B'
	lbne	L56F4
	dmv	off_AE, L558C
	lda     (off_AE),y
	sta     $A0                             ; 56CD 85 A0                    ..
	add16m8	$A1, L559E, L5594
	ldy     $A2                             ; 56DF A4 A2                    ..
	ldxa	$A0
	jsr     tohex
	add8i	L5594, L5594, $02
	jmp     L5867                           ; 56F1 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L56F4:  lda     L5591                           ; 56F4 AD 91 55                 ..U
	eor     #'H'
	lbne	L5774
	add16i	L558E, L558C, $0001
	dmv	off_AE, L558E
	lda     (off_AE),y
	sta     $A0                             ; 571B 85 A0                    ..
	add16m8	$A1, L559E, L5594
	ldy     $A2                             ; 572D A4 A2                    ..
	ldxa	$A0
	jsr     tohex
	add8i	L5594, L5594, $02
	dmv	off_AE, L558C
	ldy     #$00                            ; 5749 A0 00                    ..
	lda     (off_AE),y
	sta     $A0                             ; 574D 85 A0                    ..
	add16m8	$A1, L559E, L5594
	ldy     $A2                             ; 575F A4 A2                    ..
	ldxa	$A0
	jsr     tohex
	add8i	L5594, L5594, $02
	jmp     L5867                           ; 5771 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L5774:  lda     L5591                           ; 5774 AD 91 55                 ..U
	eor     #'S'
	beq     L5785                           ; 5779 F0 0A                    ..
	lda     L5591                           ; 577B AD 91 55                 ..U
	eor     #'s'
	lbne	L5867
L5785:	dmv	off_AE, L558C
	ldy     #$00                            ; 578F A0 00                    ..
	lda     ($AE),y                         ; 5791 B1 AE                    ..
	sta     L5592                           ; 5793 8D 92 55                 ..U
	dmv	off_AE, L558C
	lda     ($AE),y                         ; 57A0 B1 AE                    ..
	sta     $A0                             ; 57A2 85 A0                    ..
	add16m8	$A1, L559E, L5594
	ldy     $A2                             ; 57B4 A4 A2                    ..
	ldx     $A1                             ; 57B6 A6 A1                    ..
	lda     $A0                             ; 57B8 A5 A0                    ..
	jsr     tohex
	add8i	L5594, L5594, $02
	add16i	L558C, L558C, $0002
	lda     L5592                           ; 57D7 AD 92 55                 ..U
	lbne	L57E2
	jmp     L587E                           ; 57DF 4C 7E 58                 L~X

; ----------------------------------------------------------------------------
L57E2:	dmv	off_AE, L558C
	ldp16	L5598
	lda     #$00                            ; 57F9 A9 00                    ..
	cmp     L5592                           ; 57FB CD 92 55                 ..U
	lbcs	L5867
	sty     L5593                           ; 5803 8C 93 55                 ..U
	sec                                     ; 5806 38                       8
	lda     L5592                           ; 5807 AD 92 55                 ..U
	sbc     #$01                            ; 580A E9 01                    ..
	sta     L581A                           ; 580C 8D 1A 58                 ..X
L580F:  lda     L581A                           ; 580F AD 1A 58                 ..X
	cmp     L5593                           ; 5812 CD 93 55                 ..U
	bcs     L581B                           ; 5815 B0 04                    ..
	jmp     L5867                           ; 5817 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L581A:	.byte	$73

; ----------------------------------------------------------------------------
L581B:	add16m8 off_AE, L5598, L5593
	ldp8	L5595
	lda     L5591                           ; 5832 AD 91 55                 ..U
	eor     #'S'
	lbne	L5847
	lda     L5595                           ; 583C AD 95 55                 ..U
	jsr     sub_4BC9
	mv	L5595, $A0
L5847:	add16m8	off_AE, L559E, L5594
	stp8	L5595
	inc     L5594                           ; 585E EE 94 55                 ..U
	inc     L5593                           ; 5861 EE 93 55                 ..U
	jmp     L580F                           ; 5864 4C 0F 58                 L.X

; ----------------------------------------------------------------------------
L5867:	add16i	L558C, L558C, $0002
	inc     L5590                           ; 5878 EE 90 55                 ..U
	jmp     L5621                           ; 587B 4C 21 56                 L!V

; ----------------------------------------------------------------------------
L587E:	add8i	L5592, L5594, $04
L5887:  sec                                     ; 5887 38                       8
	lda     #$59                            ; 5888 A9 59                    .Y
	sbc     L4650                           ; 588A ED 50 46                 .PF
	sta     $AE                             ; 588D 85 AE                    ..
	lda     $AE                             ; 588F A5 AE                    ..
	cmp     L5592                           ; 5891 CD 92 55                 ..U
	lbcs	L589F
	jsr     sub_5394
	jmp     L5887                           ; 589C 4C 87 58                 L.X

; ----------------------------------------------------------------------------
L589F:	add8i	$A1, L474C, $01
	mv	$A3, L559E+1
	clc                                     ; 58AC 18                       .
	lda     #<LB16A
	adc     L4650                           ; 58AF 6D 50 46                 mPF
	sta     $A4                             ; 58B2 85 A4                    ..
	lda     #>LB16A
	adc     #$00                            ; 58B6 69 00                    i.
	sta     $A5                             ; 58B8 85 A5                    ..
	ldy     L559E                           ; 58BA AC 9E 55                 ..U
	ldx     $A1                             ; 58BD A6 A1                    ..
	lda     L5594                           ; 58BF AD 94 55                 ..U
	jsr     sub_51F7
	add8m	L4650, L4650, L5592
	lda     L4650                           ; 58CF AD 50 46                 .PF
	eor     L5592                           ; 58D2 4D 92 55                 M.U
	lbne	L58E6
	yldi	L464B, $00
	ldxai	LB16A
	jsr     sub_52E1
L58E6:  rts                                     ; 58E6 60                       `

