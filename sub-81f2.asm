
; ----------------------------------------------------------------------------
L81D9:	.byte	$01                             ; 81D9 01                       .
L81DA:  .byte	$98
L81DB:	.byte	$8D                             ; 81DB 8D                       .
L81DC:	.byte	$B4                             ; 81DC B4                       .
L81DD:	.byte	$09
L81DE:	.byte	$4C
	.byte   $DD                             ; 81DF DD                       .
L81E0:	.byte	$F3                             ; 81E0 F3                       .
L81E1:	.byte	$4D                             ; 81E1 4D                       M
L81E2:	.byte	$4F                             ; 81E2 4F                       O
L81E3:	.byte	$43                             ; 81E3 43                       C
L81E4:	.byte	$2E                             ; 81E4 2E                       .
L81E5:	.byte	$52                             ; 81E5 52                       R
L81E6:	.byte	$45                             ; 81E6 45                       E
L81E7:	.byte	$4D                             ; 81E7 4D                       M
L81E8:	.byte	$43                             ; 81E8 43                       C
L81E9:  eor     ($52,x)                         ; 81E9 41 52                    AR
	eor     $4E                             ; 81EB 45 4E                    EN
L81ED:	.byte	$44                             ; 81ED 44                       D
L81EE:	.byte	$53                             ; 81EE 53                       S
	.byte   $43                             ; 81EF 43                       C
	.byte   $52                             ; 81F0 52                       R
L81F1:	.byte	$4E                             ; 81F1 4E                       N

sub_81F2:  
	prolog
	stx     L81DA                           ; 81F5 8E DA 81                 ...
	sta     L81D9                           ; 81F8 8D D9 81                 ...
	add8m	L81E4, L4751, L81D9
	add8m	L81E5, L4752, L81DA
	func16_8 sub_7035, L81DC, L4750
	blkmv_imi L81E8, L81DC, $000A
	lda     #$00                            ; 8236 A9 00                    ..
	sta     $A3                             ; 8238 85 A3                    ..
	lda     #$27                            ; 823A A9 27                    .'
	sta     $A4                             ; 823C 85 A4                    ..
	lda     #$17                            ; 823E A9 17                    ..
	sta     $A5                             ; 8240 85 A5                    ..
	ldy     #$00                            ; 8242 A0 00                    ..
	ldxai	L81DE
	jsr     sub_4BF2
	lda     #$81                            ; 824B A9 81                    ..
	sta     $A3                             ; 824D 85 A3                    ..
	ldy     #$DE                            ; 824F A0 DE                    ..
	ldxa	L81E4
	jsr     sub_4C75
	lda     $A0                             ; 825A A5 A0                    ..
	sta     L81DB                           ; 825C 8D DB 81                 ...
	lda     L81DB                           ; 825F AD DB 81                 ...
	eor     #$01                            ; 8262 49 01                    I.
	lbne	L827A
	ldy     L4750                           ; 8269 AC 50 47                 .PG
	ldxa	L81E4
	jsr     sub_7368
	lda     $A0                             ; 8275 A5 A0                    ..
	sta     L81DB                           ; 8277 8D DB 81                 ...
L827A:  lda     L81DB                           ; 827A AD DB 81                 ...
	eor     #$01                            ; 827D 49 01                    I.
	lbne	L832F
	ldxa	L81E4
	jsr     sub_73DA
	lda     $A0                             ; 828D A5 A0                    ..
	sta     L81E3                           ; 828F 8D E3 81                 ...
	lda     L4750                           ; 8292 AD 50 47                 .PG
	eor     L81E3                           ; 8295 4D E3 81                 M..
	lbeq	L82A5
	ldi	L81E2, $02
	jmp     L832C                           ; 82A2 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L82A5:  lda     L81F1                           ; 82A5 AD F1 81                 ...
	lbne	L82B5
L82AD:	yldi	L81E2, $01
	jmp     L832C                           ; 82B2 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L82B5:	add16i	L81E6, L81DC, $001E
	dmv	$A3, L81E7
	mv	$A5, L81E9
	ldy     L81E6                           ; 82D5 AC E6 81                 ...
	ldxai	L81DE
	jsr     sub_4C1D
	add8m	off_AE, L81E0, L81ED
	sub8i	L81E0, off_AE, $01
	add8m	off_AE, L81E1, L81EE
	sub8i	L81E1, off_AE, $01
	ldi	$A3, >L81DE
	ldy     #<L81DE
	ldxa	L81E4
	jsr     sub_4C75
	lda     $A0                             ; 8310 A5 A0                    ..
	sta     L81DB                           ; 8312 8D DB 81                 ...
	lda     L81DB                           ; 8315 AD DB 81                 ...
	eor     #$01                            ; 8318 49 01                    I.
	lbne	L8327
	ldy     #$01                            ; 831F A0 01                    ..
	sty     L81E2                           ; 8321 8C E2 81                 ...
	jmp     L832C                           ; 8324 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L8327:	ldi	L81E2, $02
L832C:  jmp     L8334                           ; 832C 4C 34 83                 L4.

; ----------------------------------------------------------------------------
L832F:  lda     #$02                            ; 832F A9 02                    ..
	sta     L81E2                           ; 8331 8D E2 81                 ...
L8334:  lda     L81E2                           ; 8334 AD E2 81                 ...
	eor     #$02                            ; 8337 49 02                    I.
	lbne	L83D1
	ldy     #$00                            ; 833E A0 00                    ..
	ldx     L81D9                           ; 8340 AE D9 81                 ...
	lda     L4750                           ; 8343 AD 50 47                 .PG
	jsr     sub_768A
	lda     $A0                             ; 8349 A5 A0                    ..
	lbne	L8386
	lda     #$80                            ; 8350 A9 80                    ..
	cmp     L81D9                           ; 8352 CD D9 81                 ...
	lbcs	L8362
	proc8i	cmd_2a, $0D
	jmp     L8371                           ; 835F 4C 71 83                 Lq.

; ----------------------------------------------------------------------------
L8362:  lda     #$00                            ; 8362 A9 00                    ..
	cmp     L81D9                           ; 8364 CD D9 81                 ...
	lbcs	L8371
	proc8i	cmd_2a, $0F
L8371:  lda     L474F                           ; 8371 AD 4F 47                 .OG
	eor     #$03                            ; 8374 49 03                    I.
	lbne	L8386
	mv	L4762, L81E4
	yldi	L4657, $01
L8386:  ldy     L81DA                           ; 8386 AC DA 81                 ...
	ldx     #$00                            ; 8389 A2 00                    ..
	lda     L4750                           ; 838B AD 50 47                 .PG
	jsr     sub_768A
	lda     $A0                             ; 8391 A5 A0                    ..
	lbne	L83CE
	lda     #$80                            ; 8398 A9 80                    ..
	cmp     L81DA                           ; 839A CD DA 81                 ...
	lbcs	L83AA
	proc8i	cmd_2a, $0E
	jmp     L83B9                           ; 83A7 4C B9 83                 L..

; ----------------------------------------------------------------------------
L83AA:  lda     #$00                            ; 83AA A9 00                    ..
	cmp     L81DA                           ; 83AC CD DA 81                 ...
	lbcs	L83B9
	proc8i	cmd_2a, $10
L83B9:  lda     L474F                           ; 83B9 AD 4F 47                 .OG
	eor     #$03                            ; 83BC 49 03                    I.
	lbne	L83CE
	mv	L4763, L81E5
	yldi	L4657, $01
L83CE:  jmp     L83E2                           ; 83CE 4C E2 83                 L..

; ----------------------------------------------------------------------------
L83D1:	dmv	L4762, L81E4
	yldi	L4657, $01
L83E2:  rts                                     ; 83E2 60                       `

