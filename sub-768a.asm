
; ----------------------------------------------------------------------------
L766A:	.byte	$EB                             ; 766A EB                       .
L766B:	.byte	$20                             ; 766B 20                        
L766C:	.byte	$53                             ; 766C 53                       S
L766D:	.byte	$EC,$90
L766F:  .byte	$B8
L7670:	.byte	$4C                             ; 7670 4C                       L
L7671:	.byte	$11                             ; 7671 11                       .
L7672:  .byte	$F0
L7673:	.byte	$20
	.byte	$C0,$EB
	.byte	$20
L7677:	.byte	$43,$EC
	.byte	$B0,$3F
L767B:	.byte	$20                             ; 767B 20                        
L767C:	.byte	$75                             ; 767C 75                       u
L767D:  .byte	$E9
L767E:	.byte	$20
	.byte   $C2                             ; 767F C2                       .
L7680:	.byte	$EF                             ; 7680 EF                       .
L7681:	.byte	$D0                             ; 7681 D0                       .
L7682:	.byte	$03                             ; 7682 03                       .
	.byte	$20,$40,$EF
L7686:	.byte	$20                             ; 7686 20                        
L7687:	.byte	$93                             ; 7687 93                       .
L7688:	.byte	$ED                             ; 7688 ED                       .
	.byte   $20                             ; 7689 20                        

sub_768A:  
	stack_prolog L766A, $02
	func16_8 sub_7035, L766D, L766A
	mv	$A3, L766D+1
	rdldi	$A4, $000C
	ldy     L766D                           ; 76B0 AC 6D 76                 .mv
	ldxai	L767E
	jsr     blockmove
L76BA:  lda     L7688                           ; 76BA AD 88 76                 ..v
	beq     L76C7                           ; 76BD F0 08                    ..
	ifm8z	L7687, L76CC
L76C7:  ldi	$A0, $00
	rts                                     ; 76CB 60                       `

; ----------------------------------------------------------------------------
L76CC:	add8m	L766B, L766B, L7680
	add8m	L766C, L766C, L7681
	add16i	L767B, L766D, $0022
	mv	$A3, L767C
	sec                                     ; 76F6 38                       8
	lda     #$00                            ; 76F7 A9 00                    ..
	sbc     L766B                           ; 76F9 ED 6B 76                 .kv
	sta     $A4                             ; 76FC 85 A4                    ..
	sec                                     ; 76FE 38                       8
	lda     #$00                            ; 76FF A9 00                    ..
	sbc     L766C                           ; 7701 ED 6C 76                 .lv
	sta     $A5                             ; 7704 85 A5                    ..
	ldy     L767B                           ; 7706 AC 7B 76                 .{v
	ldxai	L7677
	jsr     sub_4C1D
	add16i	$A2, L766D, $001A
	rdldi	$A4, L7673
	ldy     $A2                             ; 7727 A4 A2                    ..
	ldxai	L7677
	jsr     sub_4CF5
	lda     $A0                             ; 7730 A5 A0                    ..
	sta     L767D                           ; 7732 8D 7D 76                 .}v
	lda     L767D                           ; 7735 AD 7D 76                 .}v
	eor     #$01                            ; 7738 49 01                    I.
	lbne	L7799
	add16i	$A0, L766D, $001A
	ldxa	$A0
	jsr     sub_4E4A
	rdmv	L766F, $A0
	ldxai	L7673
	jsr     sub_4E4A
	rdmv	L7671, $A0
	lda     L7671                           ; 7770 AD 71 76                 .qv
	eor     L766F                           ; 7773 4D 6F 76                 Mov
	bne     L777E                           ; 7776 D0 06                    ..
	ora     L7672                           ; 7778 0D 72 76                 .rv
	eor     L7670                           ; 777B 4D 70 76                 Mpv
L777E:	lbne	L7799
	mv	$A3, L766C
	ldy     L766B                           ; 7788 AC 6B 76                 .kv
	ldx     L7682                           ; 778B AE 82 76                 ..v
	lda     L766A                           ; 778E AD 6A 76                 .jv
	jsr     cmd_lm
	ldi	$A0, $01
	rts                                     ; 7798 60                       `

; ----------------------------------------------------------------------------
L7799:	ldi     $A0, $00
	rts                                     ; 779D 60                       `

