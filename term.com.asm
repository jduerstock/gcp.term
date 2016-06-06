; da65 V2.15 - Git b3d84d5
; Created:    2016-05-13 20:47:02
; Input file: ../TERM.COM
; Page:       1


	.setcpu "6502"

; ----------------------------------------------------------------------------
ATRACT		:= $004D
off_82		:= $0082
off_84		:= $0084
off_AC		:= $00AC
off_AE		:= $00AE
VDSLST		:= $0200
CDTMV3		:= $021C
CDTMV5		:= $0220
CDTMF3		:= $022A
SRTIMR		:= $022B
CDTMF5		:= $022E
DLIST		:= $0230
INVFLG		:= $02B6
SHFLOK		:= $02BE
PCOLR3		:= $02C3
INITAD		:= $02E2
MEMTOP		:= $02E5
MEMLO		:= $02E7
DVSTAT		:= $02EA
CRSINH		:= $02F0
KEYDEL		:= $02F1
CH1		:= $02F2
CH		:= $02FC
CHBAS		:= $02F4
L05C0		:= $05C0
L3272           := $3272
L4253           := $4253
LB16A		:= $B16A
LB16C		:= $B16C
LB1C6		:= $B1C6
LB1C8		:= $B1C8
LB1C9		:= $B1C9
LB223		:= $B223
LB224		:= $B224
HPOSP3		:= $D003
SIZEP3		:= $D00B
TRIG0		:= $D010
CONSOL		:= $D01F
KBCODE		:= $D209
LE000		:= $E000
CIOV            := $E456
SETVBV          := $E45C
SYSVBV		:= $E45F
; ----------------------------------------------------------------------------

.macro	prolog
	.local realcode
	jmp	realcode
	realcode:
.endmacro

.macro	lbeq	addr
	.local	untrue
	bne	untrue
	jmp	addr
	untrue:
.endmacro

.macro	lbne	addr
	.local	untrue
	beq	untrue
	jmp	addr
	untrue:
.endmacro

.macro	lbcc	addr
	.local	untrue
	bcs	untrue
	jmp	addr
	untrue:
.endmacro

.macro	lbcs	addr
	.local	untrue
	bcc	untrue
	jmp	addr
	untrue:
.endmacro

.macro	stack_prolog addr, size
	prolog
	jsr	sub_44D5
	.addr	addr
	.byte	size
.endmacro

.macro	ldi	arg1, arg2
	LDA	#arg2
	STA	arg1
.endmacro

.macro	yldi	arg1, arg2
	ldy	#arg2
	sty	arg1
.endmacro

.macro	dldi	arg1, arg2
	ldi	arg1, <arg2
	ldi	arg1+1, >arg2
.endmacro

.macro	rdldi	arg1, arg2
	ldi	arg1+1, >arg2
	ldi	arg1, <arg2
.endmacro

.macro   mv	dest,source
	LDA	source
	STA	dest
.endmacro

.macro   dmv	dest,source
	LDA	source
	STA	dest
	LDA	source+1
	STA	dest+1
.endmacro

.macro   rdmv	dest,source
	LDA	source+1
	STA	dest+1
	LDA	source
	STA	dest
.endmacro

; a = b + i
.macro add16i a1, a2, a3
	clc
        lda     a2
        adc     #<a3
        sta     a1
        lda     a2+1
        adc     #>a3
        sta     a1+1
.endmacro

.macro sub16i a1, a2, a3
	sec
        lda     a2
        sbc     #<a3
        sta     a1
        lda     a2+1
        sbc     #>a3
        sta     a1+1
.endmacro

; a = b + c
.macro add16m a1, a2, a3
	clc
        lda     a2
        adc     a3
        sta     a1
        lda     a2+1
        adc     a3+1
        sta     a1+1
.endmacro

.macro sub16m a1, a2, a3
	sec
        lda     a2
        sbc     a3
        sta     a1
        lda     a2+1
        sbc     a3+1
        sta     a1+1
.endmacro

.macro add16m8 a1, a2, a3
	clc
        lda     a2
        adc     a3
        sta     a1
        lda     a2+1
        adc     #$00
        sta     a1+1
.endmacro

.macro add8m a1, a2, a3
	clc
        lda     a2
        adc     a3
        sta     a1
.endmacro

.macro sub8m a1, a2, a3
	sec
        lda     a2
        sbc     a3
        sta     a1
.endmacro

.macro add8i a1, a2, a3
	clc
        lda     a2
        adc     #a3
        sta     a1
.endmacro

.macro sub8i a1, a2, a3
	sec
        lda     a2
        sbc     #a3
        sta     a1
.endmacro

.macro ldxa a1
	ldx	a1+1
	lda	a1
.endmacro

.macro ld2xa a1
	lda	a1+1
	tax
	lda	a1
.endmacro

.macro stxa a1
	stx	a1+1
	sta	a1
.endmacro

.macro st2xa a1
	sta	a1
	txa
	sta	a1+1
.endmacro

.macro ldxai a1
	ldx	#>a1
	lda	#<a1
.endmacro

.macro ldyxi a1
	ldy	#>a1
	ldx	#<a1
.endmacro

.macro shladdm8 a1, a2, a3
	lda     a3
	asl     a
	php
	clc
	adc     a2
	sta     a1
	lda     #$00
	rol     a
	plp
	adc     a2+1
	sta     a1+1
.endmacro

.macro shladdi a1, a2, a3
	lda     #<a3
	asl     a
	php
	clc
	adc     a2
	sta     a1
	lda     #>a3
	rol     a
	plp
	adc     a2+1
	sta     a1+1
.endmacro

.macro push16 arg1
	lda arg1+1
	pha
	lda arg1
	pha
.endmacro

.macro pull16 arg1
	pla
	sta arg1
	pla
	sta arg1+1
.endmacro

.macro	inc16	addr
	inc	addr
	.local	nocarry
	bne	nocarry
	inc	addr+1
	nocarry:
.endmacro

.macro	proc8	p1, a1
	lda	a1
	jsr	p1
.endmacro

.macro	func8_8i f1, a1, i1
	lda     #i1
	jsr     f1
	mv	a1, $A0
.endmacro

.macro	func16_8 f1, a1, a2
	proc8	f1, a2
	rdmv	a1, $A0
.endmacro

.macro	ldp16	a1
	ldy	#$01
	lda	(off_AE),y
	sta	a1+1
	dey
	lda	(off_AE),y
	sta	a1
.endmacro

.macro	ld2p16	a1, a2
	iny
	.if	.paramcount = 2
	lda	(a2),y
	.else
	lda	(off_AE),y
	.endif
	sta	a1+1
	dey
	.if	.paramcount = 2
	lda	(a2),y
	.else
	lda	(off_AE),y
	.endif
	sta	a1
.endmacro

.macro	ldp8	a1
	ldy	#$00
	lda	($AE),y
	sta	a1
.endmacro

.macro	stp8	a1
	lda	a1
	ldy	#$00
	sta	(off_AE),y
.endmacro

.macro	stp16	a1
	lda     a1+1
	ldy     #$01
	sta     ($AE),y
	lda     a1
	dey
	sta     ($AE),y
.endmacro

.macro	test16	a1
	lda     a1
	ora     a1+1
.endmacro

.macro	and8i	a1, a2, i1
	lda	a2
	and	#i1
	sta	a1
.endmacro

.macro	pcode	a1
	.local	ptr
ptr:
	.byte	.strlen(a1)
	.byte	a1
	.addr	ptr
.endmacro

	.segment "HDR00"

	.word	$FFFF
	.addr	L4327
	.addr	$B0BF

	.segment "SEG00"

L4327:
	.byte	$60
	.byte	" (c)1983 Action Computer Services"

sub_4349:
	ldx     #$FF                            ; 4349 A2 FF                    ..
	stx     $A6                             ; 434B 86 A6                    ..
	ldy     #$0C                            ; 434D A0 0C                    ..
L434F:  bne     L435B                           ; 434F D0 0A                    ..

sub_4351:  
	sty     $A6                             ; 4351 84 A6                    ..
	ldy     #$0B                            ; 4353 A0 0B                    ..
	bne     L435B                           ; 4355 D0 04                    ..
L4357:  sty     $A6                             ; 4357 84 A6                    ..
	ldy     #$05                            ; 4359 A0 05                    ..
L435B:  stx     $A5                             ; 435B 86 A5                    ..
	ldx     #$00                            ; 435D A2 00                    ..
	stx     $A3                             ; 435F 86 A3                    ..

sub_4361:
	asl     a                               ; 4361 0A                       .
	asl     a                               ; 4362 0A                       .
	asl     a                               ; 4363 0A                       .
	asl     a                               ; 4364 0A                       .
	tax                                     ; 4365 AA                       .
	tya                                     ; 4366 98                       .
	sta     $0342,x                         ; 4367 9D 42 03                 .B.
	lda     $A3                             ; 436A A5 A3                    ..
	beq     L4378                           ; 436C F0 0A                    ..
	sta     $034A,x                         ; 436E 9D 4A 03                 .J.
	lda     $A4                             ; 4371 A5 A4                    ..
	sta     $034B,x                         ; 4373 9D 4B 03                 .K.
	lda     #$00                            ; 4376 A9 00                    ..
L4378:  tay                                     ; 4378 A8                       .
	sta     $0349,x                         ; 4379 9D 49 03                 .I.
	lda     ($A5),y                         ; 437C B1 A5                    ..
	sta     $0348,x                         ; 437E 9D 48 03                 .H.
	beq     L4395                           ; 4381 F0 12                    ..
	clc                                     ; 4383 18                       .
	lda     $A5                             ; 4384 A5 A5                    ..
	adc     #$01                            ; 4386 69 01                    i.
L4388:  sta     $0344,x                         ; 4388 9D 44 03                 .D.
	lda     $A6                             ; 438B A5 A6                    ..
	adc     #$00                            ; 438D 69 00                    i.
	sta     $0345,x                         ; 438F 9D 45 03                 .E.
	jmp     CIOV

; ----------------------------------------------------------------------------
L4395:  rts                                     ; 4395 60                       `

; ----------------------------------------------------------------------------
sub_4396:  
	stx     $A5                             ; 4396 86 A5                    ..
	sty     $A6                             ; 4398 84 A6                    ..
	ldy     #$03                            ; 439A A0 03                    ..
	jmp     sub_4361

; ----------------------------------------------------------------------------
sub_439F:  
	stx     $A5                             ; 439F 86 A5                    ..
	sty     $A6                             ; 43A1 84 A6                    ..
	ldx     #$00                            ; 43A3 A2 00                    ..
	stx     $A3                             ; 43A5 86 A3                    ..
	ldy     #$09                            ; 43A7 A0 09                    ..
	jsr     sub_4361
	bne     L43B8                           ; 43AC D0 0A                    ..
	lda     #$0B                            ; 43AE A9 0B                    ..
	sta     $0342,x                         ; 43B0 9D 42 03                 .B.
	lda     #$9B                            ; 43B3 A9 9B                    ..
	jmp     CIOV

; ----------------------------------------------------------------------------
L43B8:  rts                                     ; 43B8 60                       `

; ----------------------------------------------------------------------------
L43B9:	.byte   $46                             ; 43B9 46                       F

sub_43BA:
	prolog
	sta     L43B9                           ; 43BD 8D B9 43                 ..C
	jmp     ($0A)                           ; 43C0 6C 0A 00                 l..

; ----------------------------------------------------------------------------
	.byte   $13                             ; 43C3 13                       .
	.byte	$11
	.byte	$01
	.byte   $83                             ; 43C6 83                       .

sub_43C7:  
	tsx                                     ; 43C7 BA                       .
	stx     $04C1                           ; 43C8 8E C1 04                 ...
	ldy     #$80                            ; 43CB A0 80                    ..
	tya                                     ; 43CD 98                       .
	jmp     sub_43BA

; ----------------------------------------------------------------------------
sub_43D1:
	ldy     $84                             ; 43D1 A4 84                    ..
	beq     L43DF                           ; 43D3 F0 0A                    ..
	stx     $85                             ; 43D5 86 85                    ..
L43D7:  asl     a                               ; 43D7 0A                       .
	rol     $85                             ; 43D8 26 85                    &.
	dey                                     ; 43DA 88                       .
	bne     L43D7                           ; 43DB D0 FA                    ..
	ldx     $85                             ; 43DD A6 85                    ..
L43DF:  rts                                     ; 43DF 60                       `

; ----------------------------------------------------------------------------
sub_43E0:
	ldy     $84                             ; 43E0 A4 84                    ..
	beq     L43EE                           ; 43E2 F0 0A                    ..
	stx     $85                             ; 43E4 86 85                    ..
L43E6:  lsr     $85                             ; 43E6 46 85                    F.
	ror     a                               ; 43E8 6A                       j
	dey                                     ; 43E9 88                       .
	bne     L43E6                           ; 43EA D0 FA                    ..
	ldx     $85                             ; 43EC A6 85                    ..
L43EE:  rts                                     ; 43EE 60                       `

; ----------------------------------------------------------------------------
L43EF:  ldy     $D3                             ; 43EF A4 D3                    ..
	bpl     L4403                           ; 43F1 10 10                    ..

sub_43F3:
	sta     $86                             ; 43F3 85 86                    ..
	stx     $87                             ; 43F5 86 87                    ..
	sec                                     ; 43F7 38                       8
	lda     #$00                            ; 43F8 A9 00                    ..
	sbc     $86                             ; 43FA E5 86                    ..
	tay                                     ; 43FC A8                       .
	lda     #$00                            ; 43FD A9 00                    ..
	sbc     $87                             ; 43FF E5 87                    ..
	tax                                     ; 4401 AA                       .
	tya                                     ; 4402 98                       .
L4403:  rts                                     ; 4403 60                       `

; ----------------------------------------------------------------------------
sub_4404:
	stx     $D3                             ; 4404 86 D3                    ..
	cpx     #$00                            ; 4406 E0 00                    ..
	bpl     L440D                           ; 4408 10 03                    ..
	jsr     sub_43F3
L440D:  sta     $82                             ; 440D 85 82                    ..
	stx     $83                             ; 440F 86 83                    ..
	lda     $85                             ; 4411 A5 85                    ..
	bpl     L4423                           ; 4413 10 0E                    ..
	tax                                     ; 4415 AA                       .
	eor     $D3                             ; 4416 45 D3                    E.
	sta     $D3                             ; 4418 85 D3                    ..
	lda     $84                             ; 441A A5 84                    ..
	jsr     sub_43F3
	sta     $84                             ; 441F 85 84                    ..
	stx     $85                             ; 4421 86 85                    ..
L4423:  ldi	$87, $00
	rts                                     ; 4427 60                       `

; ----------------------------------------------------------------------------
L4428:  beq     L4445                           ; 4428 F0 1B                    ..
	dex                                     ; 442A CA                       .
	stx     $C7                             ; 442B 86 C7                    ..
	tax                                     ; 442D AA                       .
	beq     L4445                           ; 442E F0 15                    ..
	stx     $C6                             ; 4430 86 C6                    ..
	lda     #$00                            ; 4432 A9 00                    ..
	ldx     #$08                            ; 4434 A2 08                    ..
L4436:  asl     a                               ; 4436 0A                       .
	asl     $C6                             ; 4437 06 C6                    ..
	bcc	L443D
	adc     $C7
L443D:  dex                                     ; 443D CA                       .
	bne     L4436                           ; 443E D0 F6                    ..
	clc                                     ; 4440 18                       .
	adc     $87                             ; 4441 65 87                    e.
	sta     $87                             ; 4443 85 87                    ..
L4445:  lda     $86                             ; 4445 A5 86                    ..
	ldx     $87                             ; 4447 A6 87                    ..
	rts                                     ; 4449 60                       `

; ----------------------------------------------------------------------------
sub_444A:
	jsr     sub_4404
	ldx     $82                             ; 444D A6 82                    ..
	beq     L446C                           ; 444F F0 1B                    ..
	stx     $C6                             ; 4451 86 C6                    ..
	ldx     $84                             ; 4453 A6 84                    ..
	beq     L446C                           ; 4455 F0 15                    ..
	dex                                     ; 4457 CA                       .
	stx     $C7                             ; 4458 86 C7                    ..
	ldx     #$08                            ; 445A A2 08                    ..
L445C:  asl     a                               ; 445C 0A                       .
	rol     $87                             ; 445D 26 87                    &.
	asl     $C6                             ; 445F 06 C6                    ..
	bcc     L4469                           ; 4461 90 06                    ..
	adc     $C7                             ; 4463 65 C7                    e.
	bcc     L4469                           ; 4465 90 02                    ..
	inc     $87                             ; 4467 E6 87                    ..
L4469:  dex                                     ; 4469 CA                       .
	bne     L445C                           ; 446A D0 F0                    ..
L446C:  sta     $86                             ; 446C 85 86                    ..
	lda     $82                             ; 446E A5 82                    ..
	ldx     $85                             ; 4470 A6 85                    ..
	jsr     L4428                           ; 4472 20 28 44                  (D
	lda     $83                             ; 4475 A5 83                    ..
	ldx     $84                             ; 4477 A6 84                    ..
	jsr     L4428                           ; 4479 20 28 44                  (D
	jmp     L43EF                           ; 447C 4C EF 43                 L.C

; ----------------------------------------------------------------------------
sub_447F:  
	jsr     sub_4404
	lda     $85                             ; 4482 A5 85                    ..
	beq     L44AD                           ; 4484 F0 27                    .'
	ldx     #$08                            ; 4486 A2 08                    ..
L4488:  rol     $82                             ; 4488 26 82                    &.
	rol     $83                             ; 448A 26 83                    &.
	rol     $87                             ; 448C 26 87                    &.
	sec                                     ; 448E 38                       8
	lda     $83                             ; 448F A5 83                    ..
	sbc     $84                             ; 4491 E5 84                    ..
	tay                                     ; 4493 A8                       .
	lda     $87                             ; 4494 A5 87                    ..
	sbc     $85                             ; 4496 E5 85                    ..
	bcc     L449E                           ; 4498 90 04                    ..
	sta     $87                             ; 449A 85 87                    ..
	sty     $83                             ; 449C 84 83                    ..
L449E:  dex                                     ; 449E CA                       .
	bne     L4488                           ; 449F D0 E7                    ..
	lda     $82                             ; 44A1 A5 82                    ..
	rol     a                               ; 44A3 2A                       *
	ldx     #$00                            ; 44A4 A2 00                    ..
	ldy     $83                             ; 44A6 A4 83                    ..
	sty     $86                             ; 44A8 84 86                    ..
	jmp     L43EF                           ; 44AA 4C EF 43                 L.C

; ----------------------------------------------------------------------------
L44AD:  ldx     #$10                            ; 44AD A2 10                    ..
L44AF:  rol     $82                             ; 44AF 26 82                    &.
	rol     $83                             ; 44B1 26 83                    &.
	rol     a                               ; 44B3 2A                       *
	bcs     L44BA                           ; 44B4 B0 04                    ..
	cmp     $84                             ; 44B6 C5 84                    ..
	bcc     L44BD                           ; 44B8 90 03                    ..
L44BA:  sbc     $84                             ; 44BA E5 84                    ..
	sec                                     ; 44BC 38                       8
L44BD:  dex                                     ; 44BD CA                       .
	bne     L44AF                           ; 44BE D0 EF                    ..
	rol     $82                             ; 44C0 26 82                    &.
	rol     $83                             ; 44C2 26 83                    &.
	sta     $86                             ; 44C4 85 86                    ..
	lda     $82                             ; 44C6 A5 82                    ..
	ldx     $83                             ; 44C8 A6 83                    ..
	jmp     L43EF                           ; 44CA 4C EF 43                 L.C

; ----------------------------------------------------------------------------
	jsr     sub_447F
	lda     $86                             ; 44D0 A5 86                    ..
	ldx     $87                             ; 44D2 A6 87                    ..
	rts                                     ; 44D4 60                       `

; ----------------------------------------------------------------------------
sub_44D5:
	sta     $A0                             ; 44D5 85 A0                    ..
	stx     $A1                             ; 44D7 86 A1                    ..
	sty     $A2                             ; 44D9 84 A2                    ..
	clc                                     ; 44DB 18                       .
	pla                                     ; 44DC 68                       h
	sta     off_84                          ; 44DD 85 84                    ..
	adc     #$03                            ; 44DF 69 03                    i.
	tay                                     ; 44E1 A8                       .
	pla                                     ; 44E2 68                       h
	sta     off_84+1                        ; 44E3 85 85                    ..
	adc     #$00                            ; 44E5 69 00                    i.
	pha                                     ; 44E7 48                       H
	tya                                     ; 44E8 98                       .
	pha                                     ; 44E9 48                       H
	ldy     #$01                            ; 44EA A0 01                    ..
	lda     (off_84),y                      ; 44EC B1 84                    ..
	sta     off_82                          ; 44EE 85 82                    ..
	iny                                     ; 44F0 C8                       .
	lda     (off_84),y                      ; 44F1 B1 84                    ..
	sta     off_82+1                        ; 44F3 85 83                    ..
	iny                                     ; 44F5 C8                       .
	lda     (off_84),y                      ; 44F6 B1 84                    ..
	tay                                     ; 44F8 A8                       .
L44F9:  lda     $A0,y                           ; 44F9 B9 A0 00                 ...
	sta     (off_82),y                      ; 44FC 91 82                    ..
	dey                                     ; 44FE 88                       .
	bpl     L44F9                           ; 44FF 10 F8                    ..
	lda     $11                             ; 4501 A5 11                    ..
	bne     L4514                           ; 4503 D0 0F                    ..
	inc     $11                             ; 4505 E6 11                    ..
	jmp     sub_43C7

; ----------------------------------------------------------------------------
	php                                     ; 450A 08                       .
	.byte   $63                             ; 450B 63                       c
	ora     #$11                            ; 450C 09 11                    ..
	ora     $1318,y                         ; 450E 19 18 13                 ...
	and     ($23,x)                         ; 4511 21 23                    !#
	.byte   $33                             ; 4513 33                       3
L4514:  rts                                     ; 4514 60                       `

; ----------------------------------------------------------------------------
L4515:  bpl     L452D                           ; 4515 10 16                    ..
	cpy     #$88                            ; 4517 C0 88                    ..
	beq     L4523                           ; 4519 F0 08                    ..
	tya                                     ; 451B 98                       .
	cpy     #$80                            ; 451C C0 80                    ..
	beq     L4530+1
	jmp     sub_43BA

; ----------------------------------------------------------------------------
L4523:  txa                                     ; 4523 8A                       .
	lsr     a                               ; 4524 4A                       J
	lsr     a                               ; 4525 4A                       J
	lsr     a                               ; 4526 4A                       J
	lsr     a                               ; 4527 4A                       J
	tax                                     ; 4528 AA                       .
	tya                                     ; 4529 98                       .
	sta     L05C0,x                         ; 452A 9D C0 05                 ...
L452D:  rts                                     ; 452D 60                       `

; ----------------------------------------------------------------------------
	ldx     #$01                            ; 452E A2 01                    ..
L4530:	stx	$11
	pha
	jsr     sub_43C7
	pla                                     ; 4536 68                       h
	tay                                     ; 4537 A8                       .
	rts                                     ; 4538 60                       `

; ----------------------------------------------------------------------------
sub_4539:  
	pha                                     ; 4539 48                       H
	stx     $A1                             ; 453A 86 A1                    ..
	sty     $A2                             ; 453C 84 A2                    ..
	tay                                     ; 453E A8                       .
	lda     #$00                            ; 453F A9 00                    ..
	sta     L05C0,y                         ; 4541 99 C0 05                 ...
	tay                                     ; 4544 A8                       .
	lda     ($A1),y                         ; 4545 B1 A1                    ..
	sta     $0500                           ; 4547 8D 00 05                 ...
	tay                                     ; 454A A8                       .
	iny                                     ; 454B C8                       .
	lda     #$9B                            ; 454C A9 9B                    ..
	bne     L4552                           ; 454E D0 02                    ..
L4550:  lda     ($A1),y                         ; 4550 B1 A1                    ..
L4552:  sta     $0500,y                         ; 4552 99 00 05                 ...
	dey                                     ; 4555 88                       .
	bne     L4550                           ; 4556 D0 F8                    ..
	pla                                     ; 4558 68                       h
	ldx     #$00                            ; 4559 A2 00                    ..
	ldy     #$05                            ; 455B A0 05                    ..
	jsr     sub_4396
	jmp     L4515                           ; 4560 4C 15 45                 L.E

; ----------------------------------------------------------------------------
	jsr     sub_439F
	jmp     L4515                           ; 4566 4C 15 45                 L.E

; ----------------------------------------------------------------------------
sub_4569:
	jsr     sub_4349
	jmp     L4515                           ; 456C 4C 15 45                 L.E

; ----------------------------------------------------------------------------
	jsr     sub_4351
	jmp     L4515                           ; 4572 4C 15 45                 L.E

; ----------------------------------------------------------------------------
L4575:  jsr     L4357                           ; 4575 20 57 43                  WC
	sty     $A0                             ; 4578 84 A0                    ..
	lda     $0348,x                         ; 457A BD 48 03                 .H.
	beq     L4582                           ; 457D F0 03                    ..
	sec                                     ; 457F 38                       8
	sbc     #$01                            ; 4580 E9 01                    ..
L4582:  ldy     #$00                            ; 4582 A0 00                    ..
	sta     ($A5),y                         ; 4584 91 A5                    ..
	ldy     $A0                             ; 4586 A4 A0                    ..
	rts                                     ; 4588 60                       `

; ----------------------------------------------------------------------------
	pha                                     ; 4589 48                       H
	ldi	$A3, $FF
	pla                                     ; 458E 68                       h

sub_458F:
	pha                                     ; 458F 48                       H
	stx     $A1                             ; 4590 86 A1                    ..
	sty     $A2                             ; 4592 84 A2                    ..
	ldy     #$00                            ; 4594 A0 00                    ..
	lda     $A3                             ; 4596 A5 A3                    ..
	sta     ($A1),y                         ; 4598 91 A1                    ..
	pla                                     ; 459A 68                       h
	ldy     $A2                             ; 459B A4 A2                    ..
	jsr     L4575                           ; 459D 20 75 45                  uE
	jmp     L4515                           ; 45A0 4C 15 45                 L.E

; ----------------------------------------------------------------------------
sub_45A3:
	ldx     #$07                            ; 45A3 A2 07                    ..
L45A5:  stx     $A4                             ; 45A5 86 A4                    ..
	asl     a                               ; 45A7 0A                       .
	asl     a                               ; 45A8 0A                       .
	asl     a                               ; 45A9 0A                       .
	asl     a                               ; 45AA 0A                       .
	tax                                     ; 45AB AA                       .
	lda     $A4                             ; 45AC A5 A4                    ..
	sta     $0342,x                         ; 45AE 9D 42 03                 .B.
	lda     #$00                            ; 45B1 A9 00                    ..
	sta     $0348,x                         ; 45B3 9D 48 03                 .H.
	sta     $0349,x                         ; 45B6 9D 49 03                 .I.
	tya                                     ; 45B9 98                       .
	jsr     CIOV
	sta     $A0                             ; 45BD 85 A0                    ..
	jmp     L4515                           ; 45BF 4C 15 45                 L.E

; ----------------------------------------------------------------------------
	lda     #$9B                            ; 45C2 A9 9B                    ..

sub_45C4:
	tax                                     ; 45C4 AA                       .
	lda     $B7                             ; 45C5 A5 B7                    ..

sub_45C7:
	stx     $A1                             ; 45C7 86 A1                    ..
	ldy     $A1                             ; 45C9 A4 A1                    ..
	ldx     #$0B				; put buffer
	jmp     L45A5                           ; 45CD 4C A5 45                 L.E

; ----------------------------------------------------------------------------
sub_45D0:
	jsr     sub_4361
	jmp     L4515                           ; 45D3 4C 15 45                 L.E

; ----------------------------------------------------------------------------
sub_45D6:
	ldx     #$00                            ; 45D6 A2 00                    ..
	cmp     #$02                            ; 45D8 C9 02                    ..
	bmi     L45DF                           ; 45DA 30 03                    0.
	inx                                     ; 45DC E8                       .
	and     #$01                            ; 45DD 29 01                    ).
L45DF:  tay                                     ; 45DF A8                       .
	lda     $D300,x                         ; 45E0 BD 00 D3                 ...
	dey                                     ; 45E3 88                       .
	bne     L45EA                           ; 45E4 D0 04                    ..
	lsr     a                               ; 45E6 4A                       J
	lsr     a                               ; 45E7 4A                       J
	lsr     a                               ; 45E8 4A                       J
	lsr     a                               ; 45E9 4A                       J
L45EA:  and     #$0F                            ; 45EA 29 0F                    ).
	sta     $A0                             ; 45EC 85 A0                    ..
	rts                                     ; 45EE 60                       `

; ----------------------------------------------------------------------------
read_trig:
	tax                                     ; 45EF AA                       .
	lda     TRIG0,x                         ; 45F0 BD 10 D0                 ...
	sta     $A0                             ; 45F3 85 A0                    ..
	rts                                     ; 45F5 60                       `

; ----------------------------------------------------------------------------
sub_45F6:
	pha                                     ; 45F6 48                       H
	ldi	$A4, $00
	pla                                     ; 45FB 68                       h

sub_45FC:
	sta     $A0                             ; 45FC 85 A0                    ..
	stx     $A1                             ; 45FE 86 A1                    ..
	sty     $A2                             ; 4600 84 A2                    ..
	ldy     #$00                            ; 4602 A0 00                    ..
	lda     $A4                             ; 4604 A5 A4                    ..
	ldx     $A3                             ; 4606 A6 A3                    ..
	beq     L461A                           ; 4608 F0 10                    ..
L460A:  sta     ($A0),y                         ; 460A 91 A0                    ..
	iny                                     ; 460C C8                       .
	bne     L460A                           ; 460D D0 FB                    ..
	inc     $A1                             ; 460F E6 A1                    ..
	dec     $A3                             ; 4611 C6 A3                    ..
	bne     L460A                           ; 4613 D0 F5                    ..
	beq     L461A                           ; 4615 F0 03                    ..
L4617:  sta     ($A0),y                         ; 4617 91 A0                    ..
	iny                                     ; 4619 C8                       .
L461A:  cpy     $A2                             ; 461A C4 A2                    ..
	bne     L4617                           ; 461C D0 F9                    ..
	rts                                     ; 461E 60                       `

; ----------------------------------------------------------------------------

; block move
; $A1A0 = dest
; $A3A2 = source
; $A4A5 = count

blockmove:
	sta     $A0                             ; 461F 85 A0                    ..
	stx     $A1                             ; 4621 86 A1                    ..
	sty	$A2
	ldy	#$00
	lda     $A5                             ; 4627 A5 A5                    ..
	beq     L4641                           ; 4629 F0 16                    ..
L462B:  lda     ($A2),y                         ; 462B B1 A2                    ..
	sta     ($A0),y                         ; 462D 91 A0                    ..
	iny                                     ; 462F C8                       .
	bne     L462B                           ; 4630 D0 F9                    ..
	inc     $A1                             ; 4632 E6 A1                    ..
	inc     $A3                             ; 4634 E6 A3                    ..
	dec     $A5                             ; 4636 C6 A5                    ..
	bne     L462B                           ; 4638 D0 F1                    ..
	beq     L4641                           ; 463A F0 05                    ..
L463C:  lda     ($A2),y                         ; 463C B1 A2                    ..
	sta     ($A0),y                         ; 463E 91 A0                    ..
	iny                                     ; 4640 C8                       .
L4641:  cpy     $A4                             ; 4641 C4 A4                    ..
	bne     L463C                           ; 4643 D0 F7                    ..
	rts                                     ; 4645 60                       `

; ----------------------------------------------------------------------------
.macro	blkmv_imi s1, d1, c1
	mv	$A3, d1+1
	rdldi	$A4, c1
	ldy     d1
	ldxai	s1
	jsr     blockmove
.endmacro

.macro	blkmv_mii s1, d1, c1
	ldi	$A3, >d1
	rdldi	$A4, c1
	ldy     #<d1
	ldxa	s1
	jsr     blockmove
.endmacro

.macro	blkmv_mmi s1, d1, c1
	mv	$A3, d1+1
	rdldi	$A4, c1
	ldy     d1
	ldxa	s1
	jsr     blockmove
.endmacro

.macro	blkmv_mm8 s1, d1, c1
	mv	$A3, d1+1
	ldi	$A5, $00
	mv	$A4, c1
	ldy     d1
	ldxa	s1
	jsr	blockmove
.endmacro

.macro	blkmv_iii s1, d1, c1
	ldi	$A3, >d1
	rdldi	$A4, c1
	ldy     #<d1
	ldxai	s1
	jsr     blockmove
.endmacro

; ----------------------------------------------------------------------------
	.byte   $2D                             ; 4646 2D                       -
L4647:	.byte	$FF                             ; 4647 FF                       .
L4648:	.byte	$FF                             ; 4648 FF                       .
L4649:  .byte	$00
L464A:	.byte	$31                             ; 464A 31                       1
L464B:  .byte	$00
	.byte	$00
L464D:  .byte	$00
L464E:  .byte	$00
L464F:  .byte	$00
L4650:  .byte	$00
L4651:	.byte	$01                             ; 4651 01                       .
L4652:	.byte	$02                             ; 4652 02                       .
L4653:  .byte	$00
L4654:  .byte	$00
	.byte	$00
L4656:  .byte	$00
L4657:  .byte	$00
L4658:  .byte	$00
L4659:  .byte	$00
	.byte	$00
	.byte   $14                             ; 465B 14                       .
	.byte	$00
	.byte	">>> UTIL.ACT  <<"
L466D:	.byte	$3C                             ; 466D 3C                       <
L466E:	.byte	$3C                             ; 466E 3C                       <
L466F:	.addr	$B0D4
	.byte	$04
	.byte	$00
L4673:  .byte	$00
L4674:  .addr	$B400
L4676:  cpx     #$E0                            ; 4676 E0 E0                    ..
L4678:	.byte	$76                             ; 4678 76                       v
L4679:  lsr     $00                             ; 4679 46 00                    F.
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L46A2:	.byte	$7A                             ; 46A2 7A                       z
L46A3:  lsr     $1F                             ; 46A3 46 1F                    F.
	.byte	$B1
L46A6:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L46E2:	.addr	L46A6
L46E4:	.byte	$20
	.byte   $20                             ; 46E5 20                        
L46E6:	.byte	$50                             ; 46E6 50                       P
L46E7:	.byte	$1E                             ; 46E7 1E                       .
L46E8:	.byte	$10                             ; 46E8 10                       .
L46E9:  .byte	$00
L46EA:	.byte	$54                             ; 46EA 54                       T
L46EB:  .word   $416F                             ; 46EB 6F                       o
L46ED:	.byte	$73                             ; 46ED 73                       s
L46EE:	.byte	$63                             ; 46EE 63                       c
L46EF:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L46F5:	.byte	$C0                             ; 46F5 C0                       .
L46F6:  ;bcs     $470C                           ; 46F6 B0 14                    ..
	.byte	$B0,$14
	.byte	$00
L46F9:	dec     $1E,x                           ; 46F9 D6 1E                    ..
	;bvc     L46FD                           ; 46FB 50 00                    P.
	.byte	$50,$00
	.byte   $52                             ; 46FD 52                       R
	eor     #$50                            ; 46FE 49 50                    IP
	.byte   $54                             ; 4700 54                       T
	eor     #$4F                            ; 4701 49 4F                    IO
	.byte	$4E,$3A,$44
	.byte   $1F                             ; 4706 1F                       .
	.byte   $4F                             ; 4707 4F                       O
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	"*;      "
	.byte   $63                             ; 4713 63                       c
	.byte   $6F                             ; 4714 6F                       o
	;ror     L6576                           ; 4715 6E 76 65                 nve
	.byte	$6E,$76,$65
	.byte   $72                             ; 4718 72                       r
	.byte   $74                             ; 4719 74                       t
	;jsr     L5441                           ; 471A 20 41 54                  AT
	.byte	$20,$41,$54
	eor     ($53,x)                         ; 471D 41 53                    AS
	.byte   $43                             ; 471F 43                       C
	eor     #$49                            ; 4720 49 49                    II
	.byte	$20,$63,$6F
	.byte   $64                             ; 4725 64                       d
	adc     $20                             ; 4726 65 20                    e 
	.byte   $74                             ; 4728 74                       t
	.byte   $6F                             ; 4729 6F                       o
	;jsr     L5341                           ; 472A 20 41 53                  AS
	.byte	$20,$41,$53
	.byte   $43                             ; 472D 43                       C
	eor     #$49                            ; 472E 49 49                    II
	;jsr     L6176                           ; 4730 20 76 61                  va
	.byte	$20,$76,$61
	;jmp     (L6575)                         ; 4733 6C 75 65                 lue
	.byte	$6C,$75,$65

; ----------------------------------------------------------------------------
	.byte   $44                             ; 4736 44                       D
	.byte   $1F                             ; 4737 1F                       .
	asl     a:$00,x                         ; 4738 1E 00 00                 ...
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte   $44                             ; 4741 44                       D
	.byte   $1F                             ; 4742 1F                       .
	.byte   $13                             ; 4743 13                       .
	.byte	$00
	.byte   $A7                             ; 4745 A7                       .
	.byte   $04                             ; 4746 04                       .
	.byte   $0C                             ; 4747 0C                       .
	.byte   $20                             ; 4748 20                        

sub_4749:  
	jmp	L474C

L474C:  .byte	$00
L474D:  .byte	$00
L474E:  .byte	$00
L474F:  .byte	$00
L4750:  .byte	$00
L4751:  .byte	$28
L4752:	.byte	$32                             ; 4752 32                       2
L4753:	.byte	$29                             ; 4753 29                       )
L4754:  .byte	$D8
L4755:	.byte	$D6,$1E
	ora     a:$00                           ; 4757 0D 00 00                 ...
	.byte	$01,$0C
	.byte	$D8
	.byte	$30,$7E
	.byte	$11,$00
	.byte	$00
L4762:  .byte	$00
L4763:  .byte	$D8
L4764:  .byte	$00
L4765:	pcode	"BBBBS"		; "C"
L476D:	pcode	"B"		; "K"
L4771:	pcode	"BBBBS"		; "D"
L4779:	pcode	"BC"		; "F"
L477E:	pcode	"BBBBBC"	; "W"
L4787:	pcode	"BDBB"		; "Y"
L478E:	pcode	"BDBBBB"	; "B"
L4797:	pcode	"DB"		; "x"
L479C:	pcode	"X"		; "y"
L47A0:	pcode	"X"		; "z"
L47A4:	pcode	"BBB"		; "U"
L47AA:	pcode	"BBBB"		; "V"
L47B1:	pcode	"BBB"		; "X"
L47B7:	pcode	"BBB"		; "N"
L47BD:	pcode	"BCS"		; "c"
L47C3:	pcode	"B"		; "d"
L47C7:	pcode	"BBB"		; "s"
L47CD:	pcode	"BB"		; "p"
L47D2:	pcode	"BBBB"		; "m"
L47D9:	pcode	"BBB"		; "l"
L47DF:	pcode	"BD"		; "a"
L47E4:	pcode	"BR"		; "b"
L47E9:	pcode	"BR"		; "f"
L47EE:	pcode	"DBB"		; "J"
L47F4:	pcode	"DBB"		; "j"
L47FA:	pcode	"D"		; "S"
L47FE:	pcode	"D"		; "T"
L4802:	pcode	"C"		; "+"
L4806:	pcode	"B"		; "L"
L480A:	pcode	"BBA0"		; "M"
L4811:	pcode	"DB"		; "P"
L4816:	pcode	"BB"		; "R"
L481B:	pcode	"DDB"		; "G"
L4821:	pcode	"DDBBBBB"	; "A"
L482B:	pcode	"DDDDDD"	; "O"
L4834:	pcode	"CA8"		; "Z"
L483A:	pcode	"D"		; "E"
L483E:	.byte	$00		; "i"
L483F:	.addr	L483E
L4841:	pcode	"D"		; "H"
L4845:	pcode	"BBD"		; "e"
L484B:	pcode	"B"		; "I"
L484F:	.byte	$00		; "0"
L4850:	.addr	L484F
L4852:	.byte	$00		; "1"
L4853:	.addr	L4852
L4855:	pcode	"DDs"		; "2"
L485B:	pcode	"D"		; "3"
L485F:	pcode	"BDDR"		; "4"
L4866:	pcode	"BDR"		; "5"
L486C:	pcode	"BDs"		; "6"
L4872:	pcode	"Ds"		; "7"
L4877:	pcode	"Ds"		; "8"
L487C:	pcode	"BDB"		; "9"
L4882:	pcode	"C"		; "n"
L4886:	.byte	$00		; $07
L4887:	.addr	L4886
L4889:	pcode	"BBB"		; "#"
L488F:	pcode	"B"		; "*"
L4893:	pcode	"BBB"		; "="
L4899:	pcode	"CS"		; "$"
L489E:	pcode	"B"		; "%"
L48A2:	.byte	$00		; "&"
L48A3:	.addr	L48A2
L48A5:	pcode	"BB"		; "@"
L48AA:	.byte	$00		; "."
L48AB:	.addr	L48AA
L48AD:	pcode	"DB"		; ":"
L48B2:	pcode	"BB"		; "u"
L48B7:	pcode	"BB"		; "v"
L48BC:	pcode	"B"		; "w"
L48C0:	.byte	$4C
L48C1:  .addr	L48C3
L48C3:	.addr	L476D-2
	.addr	L4771-2
	.addr	L4779-2
	.addr	L477E-2
	.addr	L4787-2
	.addr	L478E-2
	.addr	L4797-2
	.addr	L479C-2
	.addr	L47A0-2
	.addr	L47A4-2
	.addr	L47AA-2
	.addr	L47B1-2
	.addr	L47B7-2
	.addr	L47BD-2
	.addr	L47C3-2
	.addr	L47C7-2
	.addr	L47CD-2
	.addr	L47D2-2
	.addr	L47D9-2
	.addr	L47DF-2
	.addr	L47E4-2
	.addr	L47E9-2
	.addr	L47EE-2
	.addr	L47F4-2
	.addr	L47FA-2
	.addr	L47FE-2
	.addr	L4802-2
	.addr	L4806-2
	.addr	L480A-2
	.addr	L4811-2
	.addr	L4816-2
	.addr	L481B-2
	.addr	L4821-2
	.addr	L482B-2
	.addr	L4834-2
	.addr	L483A-2
	.addr	L483E-2
	.addr	L4841-2
	.addr	L4845-2
	.addr	L484B-2
	.addr	L484F-2
	.addr	L4852-2
	.addr	L4855-2
	.addr	L485B-2
	.addr	L485F-2
	.addr	L4866-2
	.addr	L486C-2
	.addr	L4872-2
	.addr	L4877-2
	.addr	L487C-2
	.addr	L4882-2
	.addr	L4886-2
	.addr	L4889-2
	.addr	L488F-2
	.addr	L4893-2
	.addr	L4899-2
	.addr	L489E-2
	.addr	L48A2-2
	.addr	L48A5-2
	.addr	L48AA-2
	.addr	L48AD-2
	.addr	L48B2-2
	.addr	L48B7-2
	.addr	L48BC-2
	.addr	L48C0-2

; ----------------------------------------------------------------------------
sub_4945:  
	sta     $A0                             ; 4945 85 A0                    ..
	tax                                     ; 4947 AA                       .
	lda     #$00                            ; 4948 A9 00                    ..
	sta     $A1                             ; 494A 85 A1                    ..
	cpx     #$80                            ; 494C E0 80                    ..
	bmi     L4954                           ; 494E 30 04                    0.
	lda     #$FF                            ; 4950 A9 FF                    ..
	sta     $A1                             ; 4952 85 A1                    ..
L4954:  rts                                     ; 4954 60                       `

; ----------------------------------------------------------------------------
sub_4955:  
	sta     $A0                             ; 4955 85 A0                    ..
	stx     $A1                             ; 4957 86 A1                    ..
	eor     $A1                             ; 4959 45 A1                    E.
	rol     a                               ; 495B 2A                       *
	rol     a                               ; 495C 2A                       *
	sta     $A2                             ; 495D 85 A2                    ..
	lda     $A0                             ; 495F A5 A0                    ..
	sec                                     ; 4961 38                       8
	sbc     $A1                             ; 4962 E5 A1                    ..
	rol     a                               ; 4964 2A                       *
	eor     #$01                            ; 4965 49 01                    I.
	eor     $A2                             ; 4967 45 A2                    E.
	and     #$01                            ; 4969 29 01                    ).
L496B:  sta     $A0                             ; 496B 85 A0                    ..
	rts                                     ; 496D 60                       `

; ----------------------------------------------------------------------------
sub_496E:  
	stx     $A0                             ; 496E 86 A0                    ..
	cmp     $A0                             ; 4970 C5 A0                    ..
	bne     L4979                           ; 4972 D0 05                    ..
	lda     #$00                            ; 4974 A9 00                    ..
	sta     $A0                             ; 4976 85 A0                    ..
	rts                                     ; 4978 60                       `

; ----------------------------------------------------------------------------
L4979:  
	jsr     sub_4955
	lda     $A0                             ; 497C A5 A0                    ..
	eor     #$01                            ; 497E 49 01                    I.
	sta     $A0                             ; 4980 85 A0                    ..
	rts                                     ; 4982 60                       `

; ----------------------------------------------------------------------------
sub_4983:  
	tay                                     ; 4983 A8                       .
	jsr     sub_496E
L4987:  lda     $A0                             ; 4987 A5 A0                    ..
	beq     L498D                           ; 4989 F0 02                    ..
	tya                                     ; 498B 98                       .
	tax                                     ; 498C AA                       .
L498D:  stx     $A0                             ; 498D 86 A0                    ..
	rts                                     ; 498F 60                       `

; ----------------------------------------------------------------------------
sub_4990:  
	stx     $A3                             ; 4990 86 A3                    ..
	tay                                     ; 4992 A8                       .
	jsr     sub_496E
	lda     $A0                             ; 4996 A5 A0                    ..
	beq     L499C                           ; 4998 F0 02                    ..
	ldy     $A3                             ; 499A A4 A3                    ..
L499C:  sty     $A0                             ; 499C 84 A0                    ..
	rts                                     ; 499E 60                       `

; ----------------------------------------------------------------------------
L499F:  .byte	$00
L49A0:  .byte	$00
L49A1:  .byte	$00

; ----------------------------------------------------------------------------
sub_49A2:  
	prolog
	stxa	L49A0
	lda     L49A0                           ; 49AB AD A0 49                 ..I
	asl     a                               ; 49AE 0A                       .
	asl     a                               ; 49AF 0A                       .
	asl     a                               ; 49B0 0A                       .
	asl     a                               ; 49B1 0A                       .
	sta     $AE                             ; 49B2 85 AE                    ..
	lda     L49A1                           ; 49B4 AD A1 49                 ..I
	and     #$0F                            ; 49B7 29 0F                    ).
	sta     $AC                             ; 49B9 85 AC                    ..
	lda     $AE                             ; 49BB A5 AE                    ..
	ora     $AC                             ; 49BD 05 AC                    ..
	sta	PCOLR3
	rts                                     ; 49C2 60                       `

; ----------------------------------------------------------------------------
L49C3:	.byte	$20                             ; 49C3 20                        
L49C4:	.byte	$53                             ; 49C4 53                       S
L49C5:	dec     $1E,x                           ; 49C5 D6 1E                    ..
	.byte   $0C                             ; 49C7 0C                       .
	.byte	$00
	eor     ($54,x)                         ; 49C9 41 54                    AT
	eor     ($52,x)                         ; 49CB 41 52                    AR
	eor     #$D8                            ; 49CD 49 D8                    I.
	;bmi     L49DD                           ; 49CF 30 0C                    0.
	.byte	$30,$0C
L49D1:	.byte	$0F                             ; 49D1 0F                       .
L49D2:  .byte	$00

sub_49D3:  
	prolog
	stxa	L49C3
	mv	HPOSP3, L49C3
	lda	L49C4
	eor     L499F                           ; 49E5 4D 9F 49                 M.I
	lbne	L49EE
	rts                                     ; 49ED 60                       `

; ----------------------------------------------------------------------------
L49EE:  clc                                     ; 49EE 18                       .
	lda     #$80                            ; 49EF A9 80                    ..
	adc     L499F                           ; 49F1 6D 9F 49                 m.I
	sta     L49D1                           ; 49F4 8D D1 49                 ..I
	lda     #$B3                            ; 49F7 A9 B3                    ..
	adc     #$00                            ; 49F9 69 00                    i.
	sta     L49D2                           ; 49FB 8D D2 49                 ..I
	blkmv_imi L49C5, L49D1, $000C
	lda     #$00                            ; 4A15 A9 00                    ..
	sta     $A3                             ; 4A17 85 A3                    ..
	ldy     #$0C                            ; 4A19 A0 0C                    ..
	ldxa	L49D1
	jsr     sub_45F6
	lda     L49C4                           ; 4A24 AD C4 49                 ..I
	sta     L499F                           ; 4A27 8D 9F 49                 ..I
	clc                                     ; 4A2A 18                       .
	lda     #$80                            ; 4A2B A9 80                    ..
	adc     L499F                           ; 4A2D 6D 9F 49                 m.I
	sta     $A0                             ; 4A30 85 A0                    ..
	lda     #$B3                            ; 4A32 A9 B3                    ..
	adc     #$00                            ; 4A34 69 00                    i.
	sta     $A1                             ; 4A36 85 A1                    ..
	blkmv_mii $A0, L49C5, $000C
	rts                                     ; 4A4D 60                       `

; ----------------------------------------------------------------------------
L4A4E:	.byte	$43                             ; 4A4E 43                       C
L4A4F:	.byte	$52                             ; 4A4F 52                       R
L4A50:	.byte	$49                             ; 4A50 49                       I
L4A51:	.byte	$50                             ; 4A51 50                       P
L4A52:	.byte	$54                             ; 4A52 54                       T

; ----------------------------------------------------------------------------
L4A53:	stack_prolog L4A4E, $04
	lda     #$00                            ; 4A5C A9 00                    ..
	sta     $A3                             ; 4A5E 85 A3                    ..
	ldy     #$80                            ; 4A60 A0 80                    ..
	ldxai	$B380
	jsr     sub_45F6
	clc                                     ; 4A69 18                       .
	lda     #$80                            ; 4A6A A9 80                    ..
	adc     L4A52                           ; 4A6C 6D 52 4A                 mRJ
	sta     $A0                             ; 4A6F 85 A0                    ..
	lda     #$B3                            ; 4A71 A9 B3                    ..
	adc     #$00                            ; 4A73 69 00                    i.
	sta     $A1                             ; 4A75 85 A1                    ..
	blkmv_mm8 $A0, L4A4E, L4A50
	ldi	SIZEP3, $02
	mv	HPOSP3, L4A51
	mv	L499F, L4A52
	rts                                     ; 4AA0 60                       `

; ----------------------------------------------------------------------------
L4AA1:	.byte   $02,"R:"

L4AA4:  .byte	$00

L4AA5:  and     #$0F                            ; 4AA5 29 0F                    ).
	sta     $A0                             ; 4AA7 85 A0                    ..
	stx     $A1                             ; 4AA9 86 A1                    ..
	asl     a                               ; 4AAB 0A                       .
	asl     a                               ; 4AAC 0A                       .
	asl     a                               ; 4AAD 0A                       .
	asl     a                               ; 4AAE 0A                       .
	tax                                     ; 4AAF AA                       .
	lda     $A5                             ; 4AB0 A5 A5                    ..
	sta     $0342,x                         ; 4AB2 9D 42 03                 .B.
	lda     $A3                             ; 4AB5 A5 A3                    ..
	sta     $0348,x                         ; 4AB7 9D 48 03                 .H.
	lda     $A4                             ; 4ABA A5 A4                    ..
	sta     $0349,x                         ; 4ABC 9D 49 03                 .I.
	lda     $A6                             ; 4ABF A5 A6                    ..
	beq     L4ACB                           ; 4AC1 F0 08                    ..
	sta     $034A,x                         ; 4AC3 9D 4A 03                 .J.
	lda     $A7                             ; 4AC6 A5 A7                    ..
	sta     $034B,x                         ; 4AC8 9D 4B 03                 .K.
L4ACB:  tya                                     ; 4ACB 98                       .
	sta     $0345,x                         ; 4ACC 9D 45 03                 .E.
	lda     $A1                             ; 4ACF A5 A1                    ..
	sta     $0344,x                         ; 4AD1 9D 44 03                 .D.
	jsr     CIOV
	sty     L4AA4                           ; 4AD7 8C A4 4A                 ..J
	cpy     #$88                            ; 4ADA C0 88                    ..
	bne     L4AE4                           ; 4ADC D0 06                    ..
	tya                                     ; 4ADE 98                       .
	ldy     $A0                             ; 4ADF A4 A0                    ..
	sta     L05C0,y                         ; 4AE1 99 C0 05                 ...
L4AE4:  rts                                     ; 4AE4 60                       `

; ----------------------------------------------------------------------------
L4AE5:	.byte	$0D                             ; 4AE5 0D                       .

sub_4AE6:  
	prolog
	sta     L4AE5                           ; 4AE9 8D E5 4A                 ..J
	lda     #$00                            ; 4AEC A9 00                    ..
	sta     $A3                             ; 4AEE 85 A3                    ..
	lda     #$00                            ; 4AF0 A9 00                    ..
	sta     $A4                             ; 4AF2 85 A4                    ..
	rdldi	$A5, L4AA1
	ldy     #$0D                            ; 4AFC A0 0D                    ..
	ldx     #$00                            ; 4AFE A2 00                    ..
	lda     L4AE5                           ; 4B00 AD E5 4A                 ..J
	jsr     sub_45D0
	rts                                     ; 4B06 60                       `

; ----------------------------------------------------------------------------
modem_status:  
	prolog
	lda     #$02                            ; 4B0A A9 02                    ..
	jsr     sub_4AE6
	lda     DVSTAT+1
	sta     $A0                             ; 4B12 85 A0                    ..
	rts                                     ; 4B14 60                       `

; ----------------------------------------------------------------------------
tohex:  stx     $AE                             ; 4B15 86 AE                    ..
	sty     $AF                             ; 4B17 84 AF                    ..
	ldy     #$00                            ; 4B19 A0 00                    ..
	tax                                     ; 4B1B AA                       .
	lsr     a                               ; 4B1C 4A                       J
	lsr     a                               ; 4B1D 4A                       J
	lsr     a                               ; 4B1E 4A                       J
	lsr     a                               ; 4B1F 4A                       J
	ora     #$30                            ; 4B20 09 30                    .0
	cmp     #$3A                            ; 4B22 C9 3A                    .:
	bcc     L4B28                           ; 4B24 90 02                    ..
	adc     #$26                            ; 4B26 69 26                    i&
L4B28:  sta     ($AE),y                         ; 4B28 91 AE                    ..
	txa                                     ; 4B2A 8A                       .
	and     #$0F                            ; 4B2B 29 0F                    ).
	ora     #$30                            ; 4B2D 09 30                    .0
	cmp     #$3A                            ; 4B2F C9 3A                    .:
	bcc     L4B35                           ; 4B31 90 02                    ..
	adc     #$26                            ; 4B33 69 26                    i&
L4B35:  iny                                     ; 4B35 C8                       .
	sta     ($AE),y                         ; 4B36 91 AE                    ..
	rts                                     ; 4B38 60                       `

; ----------------------------------------------------------------------------
sub_4B39:  
	sec                                     ; 4B39 38                       8
	sbc     #$30                            ; 4B3A E9 30                    .0
	and     #$1F                            ; 4B3C 29 1F                    ).
	cmp     #$0A                            ; 4B3E C9 0A                    ..
	bcc     L4B44                           ; 4B40 90 02                    ..
	sbc     #$07                            ; 4B42 E9 07                    ..
L4B44:  sta     $A0                             ; 4B44 85 A0                    ..
	rts                                     ; 4B46 60                       `

; ----------------------------------------------------------------------------
sub_4B47:
	stxa	off_AE
	ldy     #$00                            ; 4B4B A0 00                    ..
	lda     ($AE),y                         ; 4B4D B1 AE                    ..
	cmp     #$24                            ; 4B4F C9 24                    .$
	bne     L4B62                           ; 4B51 D0 0F                    ..
	iny                                     ; 4B53 C8                       .
	lda     ($AE),y                         ; 4B54 B1 AE                    ..
	sec                                     ; 4B56 38                       8
	sbc     #$30                            ; 4B57 E9 30                    .0
	and     #$3F                            ; 4B59 29 3F                    )?
	tax                                     ; 4B5B AA                       .
	lda     $B118,x                         ; 4B5C BD 18 B1                 ...
	sta     $A0                             ; 4B5F 85 A0                    ..
	rts                                     ; 4B61 60                       `

; ----------------------------------------------------------------------------
L4B62:  jsr     sub_4B39
	lda     $A0                             ; 4B65 A5 A0                    ..
	asl     a                               ; 4B67 0A                       .
	asl     a                               ; 4B68 0A                       .
	asl     a                               ; 4B69 0A                       .
	asl     a                               ; 4B6A 0A                       .
	sta     $A1                             ; 4B6B 85 A1                    ..
	iny                                     ; 4B6D C8                       .
	lda     ($AE),y                         ; 4B6E B1 AE                    ..
	jsr     sub_4B39
	lda     $A0                             ; 4B73 A5 A0                    ..
	clc                                     ; 4B75 18                       .
	adc     $A1                             ; 4B76 65 A1                    e.
	sta     $A0                             ; 4B78 85 A0                    ..
	rts                                     ; 4B7A 60                       `

; ----------------------------------------------------------------------------
sub_4B7B:  
	tay                                     ; 4B7B A8                       .
	and     #$80                            ; 4B7C 29 80                    ).
	sta     $A0                             ; 4B7E 85 A0                    ..
	tya                                     ; 4B80 98                       .
	and     #$7F                            ; 4B81 29 7F                    ).
	cmp     #$20                            ; 4B83 C9 20                    . 
	bcs     L4B8B                           ; 4B85 B0 04                    ..
	adc     #$40                            ; 4B87 69 40                    i@
	bcc     L4B92                           ; 4B89 90 07                    ..
L4B8B:  cmp     #$60                            ; 4B8B C9 60                    .`
	bcs     L4B92                           ; 4B8D B0 03                    ..
	sec                                     ; 4B8F 38                       8
	sbc     #$20                            ; 4B90 E9 20                    . 
L4B92:  ora     $A0                             ; 4B92 05 A0                    ..
	sta     $A0                             ; 4B94 85 A0                    ..
	rts                                     ; 4B96 60                       `

; ----------------------------------------------------------------------------
sub_4B97:  
	sta     $AE                             ; 4B97 85 AE                    ..
	stx     $AF                             ; 4B99 86 AF                    ..
L4B9B:  dey                                     ; 4B9B 88                       .
	bmi     L4BA2                           ; 4B9C 30 04                    0.
	lda     ($AE),y                         ; 4B9E B1 AE                    ..
	beq     L4B9B                           ; 4BA0 F0 F9                    ..
L4BA2:  iny                                     ; 4BA2 C8                       .
	tya                                     ; 4BA3 98                       .
	sta     $A0                             ; 4BA4 85 A0                    ..
	rts                                     ; 4BA6 60                       `

; ----------------------------------------------------------------------------
sub_4BA7:  
	tax                                     ; 4BA7 AA                       .
	lda     $B14A,x                         ; 4BA8 BD 4A B1                 .J.
	cmp     #$80                            ; 4BAB C9 80                    ..
	bcs     L4BB7                           ; 4BAD B0 08                    ..
	lda     #$01                            ; 4BAF A9 01                    ..
	sta     $B800,x                         ; 4BB1 9D 00 B8                 ...
	sta     L4764                           ; 4BB4 8D 64 47                 .dG
L4BB7:  rts                                     ; 4BB7 60                       `

; ----------------------------------------------------------------------------
sub_4BB8:  
	lda     $13                             ; 4BB8 A5 13                    ..
	ldy     $14                             ; 4BBA A4 14                    ..
	cmp     $13                             ; 4BBC C5 13                    ..
	beq     L4BC4                           ; 4BBE F0 04                    ..
	lda     $13                             ; 4BC0 A5 13                    ..
	ldy     #$00                            ; 4BC2 A0 00                    ..
L4BC4:  sta     $A1                             ; 4BC4 85 A1                    ..
	sty     $A0                             ; 4BC6 84 A0                    ..
	rts                                     ; 4BC8 60                       `

; ----------------------------------------------------------------------------
sub_4BC9:  
	sta     $A0				; $A2 = $A0 & $60
	and     #$60
	sta     $A2
	asl     a				; $A3 = $A2 << 1
	sta     $A3                             
	eor     $A2				; $A1 = ($A3 ^ $A2 ^ $FF & $40) >> 1
	eor     #$FF
	and     #$40
	lsr     a
	sta     $A1
	lda     $A0                             ; 4BDB A5 A0                    ..
	and     #$9F                            ; 4BDD 29 9F                    ).
	sta     $A0                             ; 4BDF 85 A0                    ..
	lda     $A3                             ; 4BE1 A5 A3                    ..
	and     #$40                            ; 4BE3 29 40                    )@
	ora     $A0                             ; 4BE5 05 A0                    ..
	ora     $A1                             ; 4BE7 05 A1                    ..
	sta     $A0                             ; 4BE9 85 A0                    ..
	rts                                     ; 4BEB 60                       `

; ----------------------------------------------------------------------------
L4BEC:	.word	$7220
L4BEE:	.byte	$32,$20,$53,$42

sub_4BF2:  
	stack_prolog L4BEC, $05
	blkmv_mii L4BEC, L4BEE, $0004
	rts                                     ; 4C12 60                       `

; ----------------------------------------------------------------------------
L4C13:	.byte	$79,$31
L4C15:	.byte	$2C,$20
L4C17:	.byte	$72
L4C18:	.byte	$32
L4C19:  .byte	$78,$32,$2C,$20

sub_4C1D:  
	stack_prolog L4C13, $05
	blkmv_imi L4C19, L4C15, $0004
	add8m	$A2, L4C19, L4C17
	add8m	$A3, L4C19+1, L4C18
	add8m	$A4, L4C19+2, L4C17
	add8m	$A5, L4C19+3, L4C18
	ldy     $A2                             ; 4C61 A4 A2                    ..
	ldxa	L4C13
	jsr     sub_4BF2
	rts                                     ; 4C6C 60                       `

; ----------------------------------------------------------------------------
L4C6D:	.byte	$20                             ; 4C6D 20                        
L4C6E:	.byte	$72                             ; 4C6E 72                       r
L4C6F:	.byte	$32                             ; 4C6F 32                       2
L4C70:	.byte	$79                             ; 4C70 79                       y
L4C71:	.byte	$31                             ; 4C71 31                       1
L4C72:	.byte	$2B                             ; 4C72 2B                       +
L4C73:	.byte	$79                             ; 4C73 79                       y
L4C74:	.byte	$2C                             ; 4C74 2C                       ,

; ----------------------------------------------------------------------------
sub_4C75:  
	stack_prolog L4C6D, $03
	blkmv_imi L4C71, L4C6F, $0004
	ldx     L4C71                           ; 4C95 AE 71 4C                 .qL
	lda     L4C6D                           ; 4C98 AD 6D 4C                 .mL
	jsr     sub_4955
	lda     $A0                             ; 4C9E A5 A0                    ..
	lbeq	L4CAA
	ldi	$A0, $00
	rts                                     ; 4CA9 60                       `

; ----------------------------------------------------------------------------
L4CAA:  ldx     L4C73                           ; 4CAA AE 73 4C                 .sL
	lda     L4C6D                           ; 4CAD AD 6D 4C                 .mL
	jsr     sub_496E
	lda     $A0                             ; 4CB3 A5 A0                    ..
	lbeq	L4CBF
	ldi	$A0, $00
	rts                                     ; 4CBE 60                       `

; ----------------------------------------------------------------------------
L4CBF:  ldx     L4C72                           ; 4CBF AE 72 4C                 .rL
	lda     L4C6E                           ; 4CC2 AD 6E 4C                 .nL
	jsr     sub_4955
	lda     $A0                             ; 4CC8 A5 A0                    ..
	lbeq	L4CD4
	ldi	$A0, $00
	rts                                     ; 4CD3 60                       `

; ----------------------------------------------------------------------------
L4CD4:  ldx     L4C74                           ; 4CD4 AE 74 4C                 .tL
	lda     L4C6E                           ; 4CD7 AD 6E 4C                 .nL
	jsr     sub_496E
	lda     $A0                             ; 4CDD A5 A0                    ..
	lbeq	L4CE9
	ldi	$A0, $00
	rts                                     ; 4CE8 60                       `

; ----------------------------------------------------------------------------
L4CE9:	ldi	$A0, $01
	rts                                     ; 4CED 60                       `

; ----------------------------------------------------------------------------
L4CEE:  .byte	$00,$0D
L4CF0:	.byte	$3B,$44
L4CF2:	.byte	$45,$53
L4CF4:	.byte	$43                             ; 4CF4 43                       C

sub_4CF5:
	stack_prolog L4CEE, $05
	yldi	L4CF4, $00
L4D03:  lda     #$01                            ; 4D03 A9 01                    ..
	cmp     L4CF4                           ; 4D05 CD F4 4C                 ..L
	lbcc	L4DCC
	add16m8 off_AE, L4CF2, L4CF4
	push16	off_AE
	add16m8	off_AE, L4CEE, L4CF4
	ldp8	$A0
	add16m8 off_AE, L4CF0, L4CF4
	lda     ($AE),y                         ; 4D49 B1 AE                    ..
	sta     $A1                             ; 4D4B 85 A1                    ..
	ldxa	$A0
	jsr     sub_4983
	pull16	off_AE
	stp8	$A0
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CF2, off_AE
	push16	off_AC
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CEE, off_AE
	lda     ($AC),y                         ; 4D94 B1 AC                    ..
	sta     $A0                             ; 4D96 85 A0                    ..
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CF0, off_AE
	lda     ($AC),y                         ; 4DAF B1 AC                    ..
	sta     $A1                             ; 4DB1 85 A1                    ..
	ldxa	$A0
	jsr     sub_4990
	pull16	off_AC
	lda     $A0                             ; 4DC0 A5 A0                    ..
	ldy     #$00                            ; 4DC2 A0 00                    ..
	sta     ($AC),y                         ; 4DC4 91 AC                    ..
	inc     L4CF4                           ; 4DC6 EE F4 4C                 ..L
	jmp     L4D03                           ; 4DC9 4C 03 4D                 L.M

; ----------------------------------------------------------------------------
L4DCC:  dmv	$AE, L4CF2
	ldp8	$A0
	add16i	off_AE, L4CF2, $0002
	lda     ($AE),y                         ; 4DEB B1 AE                    ..
	sta     $A1                             ; 4DED 85 A1                    ..
	ldxa	$A0
	jsr     sub_496E
	lda     $A0                             ; 4DF6 A5 A0                    ..
	lbeq	L4E02
	ldi	$A0, $00
	rts                                     ; 4E01 60                       `

; ----------------------------------------------------------------------------
L4E02:	add16i	off_AE, L4CF2, $0001
	ldp8	$A0
	add16i	off_AE, L4CF2, $0003
	lda     ($AE),y                         ; 4E26 B1 AE                    ..
	sta     $A1                             ; 4E28 85 A1                    ..
	ldxa	$A0
	jsr     sub_496E
	lda     $A0                             ; 4E31 A5 A0                    ..
	lbeq	L4E3D
	ldi	$A0, $00
	rts                                     ; 4E3C 60                       `

; ----------------------------------------------------------------------------
L4E3D:  ldi	$A0, $01
	rts                                     ; 4E41 60                       `

; ----------------------------------------------------------------------------
L4E42:	.byte	$20
L4E43:	.byte	$20
L4E44:	.byte	$20
L4E45:	.byte	$20
L4E46:	.byte	$20
L4E47:	.byte	$52
L4E48:	.byte	$45
L4E49:	.byte	$54

; ----------------------------------------------------------------------------
sub_4E4A:  
	prolog
	stxa	L4E42
	blkmv_imi L4E46, L4E42, $0004
	sub8m	off_AE, L4E48, L4E46
	add8i	off_AC, off_AE, $01
	sub8m	off_AE, L4E49, L4E47
	add8i	$AA, off_AE, $01
	ldi	$85, $00
	mv	$84, $AA
	lda     $AC                             ; 4E92 A5 AC                    ..
	ldx     #$00                            ; 4E94 A2 00                    ..
	jsr     sub_444A
	st2xa	L4E44
	rdmv	$A0, L4E44
	rts                                     ; 4EAA 60                       `

; ----------------------------------------------------------------------------
L4EAB:	.byte	$49                             ; 4EAB 49                       I
L4EAC:	.byte	$46                             ; 4EAC 46                       F
L4EAD:	.byte	$20                             ; 4EAD 20                        
L4EAE:	.byte	$67                             ; 4EAE 67                       g
L4EAF:	.byte	$74                             ; 4EAF 74                       t
L4EB0:  plp                                     ; 4EB0 28                       (

sub_4EB1:  
	stack_prolog L4EAB, $05
	add16m	off_AE, L4EAB, L4EAF
	sub16i	L4EAB, off_AE, $0001
	add16m	off_AE, L4EAD, L4EAF
	sub16i	L4EAD, off_AE, $0001
L4EFA:  lda     #$00                            ; 4EFA A9 00                    ..
	cmp     L4EAF                           ; 4EFC CD AF 4E                 ..N
	lda     #$00                            ; 4EFF A9 00                    ..
	sbc     L4EB0                           ; 4F01 ED B0 4E                 ..N
	lbcs	L4F59
	sub16i	L4EAF, L4EAF, $0001
	dmv	off_AE, L4EAB
	dmv	off_AC, L4EAD
	ldy     #$00
	lda     (off_AC),y
	sta     (off_AE),y
	sub16i	L4EAB, L4EAB, $0001
	sub16i	L4EAD, L4EAD, $0001
	jmp     L4EFA                           ; 4F56 4C FA 4E                 L.N

; ----------------------------------------------------------------------------
L4F59:  rts                                     ; 4F59 60                       `

; ----------------------------------------------------------------------------
sub_4F5A:					; "^G" - bell?
	yldi	CRSINH, $01
	lda     #$FD                            ; 4F5F A9 FD                    ..
	jsr     sub_45C4
	yldi	L4656, $01
	rts                                     ; 4F69 60                       `

; ----------------------------------------------------------------------------
L4F6A:	.byte	$00
L4F6B:	.byte	$00,$2E

; ----------------------------------------------------------------------------
sub_4F6D:  
	prolog
	sta     L4F6A                           ; 4F70 8D 6A 4F                 .jO
	jsr     sub_4BB8
	clc                                     ; 4F76 18                       .
	lda     L4F6A                           ; 4F77 AD 6A 4F                 .jO
	adc     $A0                             ; 4F7A 65 A0                    e.
	sta     L4F6B                           ; 4F7C 8D 6B 4F                 .kO
	lda     #$00                            ; 4F7F A9 00                    ..
	adc     $A1                             ; 4F81 65 A1                    e.
	sta     L4F6B+1
L4F86:  jsr     sub_4BB8
	lda     $A0                             ; 4F89 A5 A0                    ..
	cmp     L4F6B                           ; 4F8B CD 6B 4F                 .kO
	lda     $A1                             ; 4F8E A5 A1                    ..
	sbc     L4F6B+1
	lbcs	L4F9B
L4F98:  jmp     L4F86                           ; 4F98 4C 86 4F                 L.O

; ----------------------------------------------------------------------------
L4F9B:  rts                                     ; 4F9B 60                       `

; ----------------------------------------------------------------------------
L4F9C:  .byte	$30

; ----------------------------------------------------------------------------
sub_4F9D:
	prolog
	sta     L4F9C                           ; 4FA0 8D 9C 4F                 ..O
	rdldi	$84, $0006
	lda     L4F9C                           ; 4FAB AD 9C 4F                 ..O
	ldx     #$00                            ; 4FAE A2 00                    ..
	jsr     sub_444A
	sta     $A0                             ; 4FB3 85 A0                    ..
	lda     $A0                             ; 4FB5 A5 A0                    ..
	jsr     sub_4F6D
	rts                                     ; 4FBA 60                       `

; ----------------------------------------------------------------------------
L4FBB:	.byte	$53                             ; 4FBB 53                       S
L4FBC:	.byte	$20                             ; 4FBC 20                        
L4FBD:	.byte	$69                             ; 4FBD 69                       i
L4FBE:  .byte	$00
L4FBF:  .byte	$00
L4FC0:	.byte	$74                             ; 4FC0 74                       t
L4FC1:  pla                                     ; 4FC1 68                       h
L4FC2:	.byte	$65                             ; 4FC2 65                       e
L4FC3:	.byte	$20                             ; 4FC3 20                        
	.byte   $72                             ; 4FC4 72                       r

sub_4FC5:  
	prolog
	jsr     modem_status
	ldi	L4FBD, $00
	mv	L4FBC, $A0
	test16	L4FBC
	lbne	L4FE5
	ldi	$A0, $00
	rts                                     ; 4FE4 60                       `

; ----------------------------------------------------------------------------
L4FE5:	func8_8i sub_45A3, L4FBB, $02
	lda     L4FBE                           ; 4FEF AD BE 4F                 ..O
	eor     #$01                            ; 4FF2 49 01                    I.
	lbne	L5015
	lda     L4FBB                           ; 4FF9 AD BB 4F                 ..O
	eor     #$0A                            ; 4FFC 49 0A                    I.
	lbne	L5010
	ldy     #$00                            ; 5003 A0 00                    ..
	sty     L4FBE                           ; 5005 8C BE 4F                 ..O
	sty     L464F                           ; 5008 8C 4F 46                 .OF
	ldi	$A0, $04
	rts                                     ; 500F 60                       `

; ----------------------------------------------------------------------------
L5010:	ldi	$A0, $00
	rts                                     ; 5014 60                       `

; ----------------------------------------------------------------------------
L5015:  lda     L464F                           ; 5015 AD 4F 46                 .OF
	lbne	L5085
	and8i	L4FC0, L4FBB, $7F
	lda     L4FC0                           ; 5025 AD C0 4F                 ..O
	cmp     #$05                            ; 5028 C9 05                    ..
	bcc     L5036                           ; 502A 90 0A                    ..
	lda     #$5F                            ; 502C A9 5F                    ._
	cmp     L4FC0                           ; 502E CD C0 4F                 ..O
	lbcs	L5040
L5036:	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 503F 60                       `

; ----------------------------------------------------------------------------
L5040:	mv	LB1C6, L4FBB
	rdldi	L4FC2, LB1C6
	lda     L4FBB                           ; 5050 AD BB 4F                 ..O
	sta     L4FC1                           ; 5053 8D C1 4F                 ..O
	ldy     #$01                            ; 5056 A0 01                    ..
	sty     L464F                           ; 5058 8C 4F 46                 .OF
	sty     L4FBF                           ; 505B 8C BF 4F                 ..O
	jsr     modem_status
	ldi	L4FBC+1, $00
	mv	L4FBC, $A0
	test16	L4FBC
	lbne	L507B
	ldi	$A0, $00
	rts                                     ; 507A 60                       `

; ----------------------------------------------------------------------------
L507B:	func8_8i sub_45A3, L4FBB, $02
L5085:  lda     L4FBF                           ; 5085 AD BF 4F                 ..O
	eor     #$01                            ; 5088 49 01                    I.
	lbne	L50EF
	lda     L4FBB                           ; 508F AD BB 4F                 ..O
	eor     LB1C6                           ; 5092 4D C6 B1                 M..
	lbeq	L50A4
	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 50A3 60                       `

; ----------------------------------------------------------------------------
L50A4:	add16m8 off_AE, L4FC2, L4FBF
	lda     L4FBB                           ; 50B4 AD BB 4F                 ..O
	ldy     #$00                            ; 50B7 A0 00                    ..
	sta     ($AE),y                         ; 50B9 91 AE                    ..
	add8m	L4FC1, L4FC1, L4FBB
	inc     L4FBF                           ; 50C5 EE BF 4F                 ..O
	jsr     modem_status
	lda     #$00                            ; 50CB A9 00                    ..
	sta     L4FBD                           ; 50CD 8D BD 4F                 ..O
	lda     $A0                             ; 50D0 A5 A0                    ..
	sta     L4FBC                           ; 50D2 8D BC 4F                 ..O
	test16	L4FBC
	lbne	L50E5
	ldi	$A0, $00
	rts                                     ; 50E4 60                       `

; ----------------------------------------------------------------------------
L50E5:  lda     #$02                            ; 50E5 A9 02                    ..
	jsr     sub_45A3
	mv	L4FBB, $A0
L50EF:	add16m8	off_AE, L4FC2, L4FBF
	stp8	L4FBB
	clc                                     ; 5106 18                       .
	lda     L4FC1                           ; 5107 AD C1 4F                 ..O
	adc     L4FBB                           ; 510A 6D BB 4F                 m.O
	sta     L4FC1                           ; 510D 8D C1 4F                 ..O
	inc     L4FBF                           ; 5110 EE BF 4F                 ..O
	lda     L4FBB                           ; 5113 AD BB 4F                 ..O
	eor     #$0A                            ; 5116 49 0A                    I.
	lbne	L514C
	clc                                     ; 511D 18                       .
	lda     L4FC0                           ; 511E AD C0 4F                 ..O
	adc     #$01                            ; 5121 69 01                    i.
	sta     $AE                             ; 5123 85 AE                    ..
	lda     L4FBF                           ; 5125 AD BF 4F                 ..O
	eor     $AE                             ; 5128 45 AE                    E.
	lbne	L514C
	sty     L464F                           ; 512F 8C 4F 46                 .OF
	sty     L4FBF                           ; 5132 8C BF 4F                 ..O
	lda     L4FC1                           ; 5135 AD C1 4F                 ..O
	eor     #$0A                            ; 5138 49 0A                    I.
	lbne	L5147
	ldi	$A0, $01
	rts                                     ; 5143 60                       `

; ----------------------------------------------------------------------------
	jmp     L514C                           ; 5144 4C 4C 51                 LLQ

; ----------------------------------------------------------------------------
L5147:	ldi	$A0, $04
	rts                                     ; 514B 60                       `

; ----------------------------------------------------------------------------
L514C:  lda     L4FC0                           ; 514C AD C0 4F                 ..O
	cmp     L4FBF                           ; 514F CD BF 4F                 ..O
	lbcs	L5161
	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 5160 60                       `

; ----------------------------------------------------------------------------
L5161:  jsr     modem_status
	lda     #$00                            ; 5164 A9 00                    ..
	sta     L4FBD                           ; 5166 8D BD 4F                 ..O
	lda     $A0                             ; 5169 A5 A0                    ..
	sta     L4FBC                           ; 516B 8D BC 4F                 ..O
	lda     #$00                            ; 516E A9 00                    ..
	cmp     L4FBC                           ; 5170 CD BC 4F                 ..O
	lda	#$00
	sbc     L4FBD                           ; 5175 ED BD 4F                 ..O
	lbcs	L5187
	lda     #$02                            ; 517D A9 02                    ..
	jsr     sub_45A3
	lda     $A0                             ; 5182 A5 A0                    ..
	sta     L4FBB                           ; 5184 8D BB 4F                 ..O
L5187:  test16	L4FBC
	lbne	L50EF
	ldi	$A0, $00
	rts                                     ; 5196 60                       `

; ----------------------------------------------------------------------------
L5197:  sta     $A0                             ; 5197 85 A0                    ..
	stx     $A1                             ; 5199 86 A1                    ..
	sty     $A2                             ; 519B 84 A2                    ..
	yldi	$A6, $00
	sty     $A5                             ; 51A1 84 A5                    ..
L51A3:  lda     $A5                             ; 51A3 A5 A5                    ..
	cmp     $A4                             ; 51A5 C5 A4                    ..
	lbcs	L51F1
	ldy     $A5                             ; 51AC A4 A5                    ..
	lda     ($A2),y                         ; 51AE B1 A2                    ..
	sta     $A7                             ; 51B0 85 A7                    ..
	and8i	$A8, $A7, $7F
	lda     $A8                             ; 51B8 A5 A8                    ..
	eor     $A7                             ; 51BA 45 A7                    E.
	lbeq	L51E4
L51C1:  inc     $A5                             ; 51C1 E6 A5                    ..
	ldy     $A5                             ; 51C3 A4 A5                    ..
	lda     ($A2),y                         ; 51C5 B1 A2                    ..
	sta     $A7                             ; 51C7 85 A7                    ..
	inc     $A5                             ; 51C9 E6 A5                    ..
L51CB:  lda     #$00                            ; 51CB A9 00                    ..
	cmp     $A8                             ; 51CD C5 A8                    ..
	lbcs	L51E1
	lda     $A7                             ; 51D4 A5 A7                    ..
	ldy     $A6                             ; 51D6 A4 A6                    ..
	sta     ($A0),y                         ; 51D8 91 A0                    ..
	inc     $A6                             ; 51DA E6 A6                    ..
	dec     $A8                             ; 51DC C6 A8                    ..
	jmp     L51CB                           ; 51DE 4C CB 51                 L.Q

; ----------------------------------------------------------------------------
L51E1:  jmp     L51EE                           ; 51E1 4C EE 51                 L.Q

; ----------------------------------------------------------------------------
L51E4:  lda     $A7                             ; 51E4 A5 A7                    ..
	ldy     $A6                             ; 51E6 A4 A6                    ..
	sta     ($A0),y                         ; 51E8 91 A0                    ..
	inc     $A6                             ; 51EA E6 A6                    ..
	inc     $A5                             ; 51EC E6 A5                    ..
L51EE:  jmp     L51A3                           ; 51EE 4C A3 51                 L.Q

; ----------------------------------------------------------------------------
L51F1:  lda     $A6                             ; 51F1 A5 A6                    ..
	sta     L4649                           ; 51F3 8D 49 46                 .IF
	rts                                     ; 51F6 60                       `

; ----------------------------------------------------------------------------
sub_51F7:  
	sta     $A9                             ; 51F7 85 A9                    ..
	stx     $A1                             ; 51F9 86 A1                    ..
	sty     $A2                             ; 51FB 84 A2                    ..
	clc                                     ; 51FD 18                       .
	lda     $A9                             ; 51FE A5 A9                    ..
	adc     #$04                            ; 5200 69 04                    i.
	sta     $A8                             ; 5202 85 A8                    ..
	lda     $A8                             ; 5204 A5 A8                    ..
	ldy     #$00                            ; 5206 A0 00                    ..
	sta     ($A4),y                         ; 5208 91 A4                    ..
	iny                                     ; 520A C8                       .
	sta     ($A4),y                         ; 520B 91 A4                    ..
	lda     $A1                             ; 520D A5 A1                    ..
	iny                                     ; 520F C8                       .
	sta     ($A4),y                         ; 5210 91 A4                    ..
	lda     $A1                             ; 5212 A5 A1                    ..
	sta     L474C                           ; 5214 8D 4C 47                 .LG
	clc                                     ; 5217 18                       .
	lda     $A8                             ; 5218 A5 A8                    ..
	adc     $A8                             ; 521A 65 A8                    e.
	adc     $A1                             ; 521C 65 A1                    e.
	sta     $A6                             ; 521E 85 A6                    ..
L5220:  ldy     #$00                            ; 5220 A0 00                    ..
	sty     $A7                             ; 5222 84 A7                    ..
L5224:  lda     $A7                             ; 5224 A5 A7                    ..
	cmp     $A9                             ; 5226 C5 A9                    ..
	bcc     L522D                           ; 5228 90 03                    ..
	jmp     L523B                           ; 522A 4C 3B 52                 L;R

; ----------------------------------------------------------------------------
L522D:  clc                                     ; 522D 18                       .
	ldy     $A7                             ; 522E A4 A7                    ..
	lda     ($A2),y                         ; 5230 B1 A2                    ..
	adc     $A6                             ; 5232 65 A6                    e.
	sta     $A6                             ; 5234 85 A6                    ..
	inc     $A7                             ; 5236 E6 A7                    ..
	jmp     L5224                           ; 5238 4C 24 52                 L$R

; ----------------------------------------------------------------------------
L523B:  clc                                     ; 523B 18                       .
	lda     $A9                             ; 523C A5 A9                    ..
	adc     #$03                            ; 523E 69 03                    i.
	tay                                     ; 5240 A8                       .
	sec                                     ; 5241 38                       8
	lda     #$00                            ; 5242 A9 00                    ..
	sbc     $A6                             ; 5244 E5 A6                    ..
	sta     ($A4),y                         ; 5246 91 A4                    ..
	clc                                     ; 5248 18                       .
	lda     $A4                             ; 5249 A5 A4                    ..
	adc     #$03                            ; 524B 69 03                    i.
	sta     $A0                             ; 524D 85 A0                    ..
	lda     $A5                             ; 524F A5 A5                    ..
	adc     #$00                            ; 5251 69 00                    i.
	sta	$A1
	blkmv_mm8 $A0, $A2, $A9
	rts                                     ; 526A 60                       `

; ----------------------------------------------------------------------------
L526B:  .byte	$D8
L526C:	.byte	$30                             ; 526C 30                       0
L526D:	.byte	$6F                             ; 526D 6F                       o
L526E:  asl     $00                             ; 526E 06 00                    ..
	.byte	$00
L5271:  .byte	$00
L5272:  cld                                     ; 5272 D8                       .
L5273:	.byte	$30                             ; 5273 30                       0

; ----------------------------------------------------------------------------
sub_5274:  
	prolog
	stxa	L526B
	dmv	off_AE, L526B
	ldp8	L526D
	rdmv	L5272, L526B
	sty     L526E                           ; 529A 8C 6E 52                 .nR
	sub8i	L52B1, L526D, $01
L52A6:  lda     L52B1                           ; 52A6 AD B1 52                 ..R
	cmp     L526E                           ; 52A9 CD 6E 52                 .nR
	bcs     L52B2                           ; 52AC B0 04                    ..
	jmp     L52D7                           ; 52AE 4C D7 52                 L.R

; ----------------------------------------------------------------------------
L52B1:	.byte	$65

; ----------------------------------------------------------------------------
L52B2:	add16m8	off_AE, L5272, L526E
	ldp8	L5271
	ldx     L5271                           ; 52C9 AE 71 52                 .qR
	lda     #$02                            ; 52CC A9 02                    ..
	jsr     sub_45C7
	inc     L526E                           ; 52D1 EE 6E 52                 .nR
	jmp     L52A6                           ; 52D4 4C A6 52                 L.R

; ----------------------------------------------------------------------------
L52D7:  ldx     #$0A                            ; 52D7 A2 0A                    ..
	lda     #$02                            ; 52D9 A9 02                    ..
	jsr	sub_45C7
	rts					; 52DE 60

; ----------------------------------------------------------------------------
L52DF:	.byte	$32
L52E0:  .byte	$D8

; ----------------------------------------------------------------------------
sub_52E1:  
	prolog
	stxa	L52DF
	inc     L464B                           ; 52EA EE 4B 46                 .KF
	lda     #$0A                            ; 52ED A9 0A                    ..
	cmp     L464B                           ; 52EF CD 4B 46                 .KF
	lbcs	L5304
	ldy     #$00                            ; 52F7 A0 00                    ..
	sty     L464B                           ; 52F9 8C 4B 46                 .KF
	lda     #$11                            ; 52FC A9 11                    ..
	jsr     sub_4BA7
	jsr     sub_4749
L5304:	ldxa	L52DF
	jsr     sub_5274
	yldi	CDTMF3, $01
	rdldi	CDTMV3, $0258			; 10 seconds
	rts                                     ; 531C 60                       `

; ----------------------------------------------------------------------------
L531D:	.byte	$FF                             ; 531D FF                       .
L531E:  .byte	$00
L531F:  .byte	$65,$00
	.byte	$00
L5322:	.byte	$20                             ; 5322 20                        
L5323:	.byte	$53                             ; 5323 53                       S
L5324:	.byte	$D6,$1E
	.byte	$05,$00
	.byte	$78
L5329:	.byte	$32                             ; 5329 32                       2

; ----------------------------------------------------------------------------
sub_532A:
	prolog
	stxa	L531E
	dmv	off_AE, L5322
	stp8	L531F
	mv	L5329, L474C
	sty     L464B                           ; 534A 8C 4B 46                 .KF
	mv	$A3, L5323
	rdldi	$A4, L5324
	ldy     L5322                           ; 535A AC 22 53                 ."S
	ldx     L531E                           ; 535D AE 1E 53                 ..S
	lda     #$01                            ; 5360 A9 01                    ..
	jsr     sub_51F7
	mv	L474C, L5329
	ldxai	L5324
	jsr     sub_5274
	rts                                     ; 5372 60                       `

; ----------------------------------------------------------------------------
sub_5373:
	prolog
	ldx     #$1B                            ; 5376 A2 1B                    ..
	lda     L531D                           ; 5378 AD 1D 53                 ..S
	jsr     sub_532A
	rts                                     ; 537E 60                       `

; ----------------------------------------------------------------------------
sub_537F:  
	ldx     #$06                            ; 537F A2 06                    ..
	lda     LB1C8                           ; 5381 AD C8 B1                 ...
	jsr     sub_532A
	rts                                     ; 5387 60                       `

; ----------------------------------------------------------------------------
L5388:  ldx     #$15                            ; 5388 A2 15                    ..
	lda     LB1C8                           ; 538A AD C8 B1                 ...
	jsr     sub_532A
	rts                                     ; 5390 60                       `

; ----------------------------------------------------------------------------
L5391:	.byte	$2D                             ; 5391 2D                       -
L5392:	.byte	$2D                             ; 5392 2D                       -
L5393:	.byte	$2D                             ; 5393 2D                       -

; ----------------------------------------------------------------------------
sub_5394:  
	prolog
	yldi	L5392, $00
	jsr     sub_4FC5
	mv	L5391, $A0
	mv	L5393, LB1C9
	lda     L5391                           ; 53AA AD 91 53                 ..S
	eor     #$04                            ; 53AD 49 04                    I.
	lbne	L53BE
	ldxai	LB16A
	jsr     sub_52E1
	jmp     L545B                           ; 53BB 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L53BE:  lda     L5391                           ; 53BE AD 91 53                 ..S
	eor     #$01                            ; 53C1 49 01                    I.
	lbne	L544C
	lda     L5393                           ; 53C8 AD 93 53                 ..S
	eor     #$06                            ; 53CB 49 06                    I.
	lbne	L5421
	lda     LB1C8                           ; 53D2 AD C8 B1                 ...
	eor     LB16C                           ; 53D5 4D 6C B1                 Ml.
	lbne	L541E
	sub8m	L4650, L4650, LB16A
	clc                                     ; 53E7 18                       .
	lda     #<LB16A
	adc     LB16A                           ; 53EA 6D 6A B1                 mj.
	sta     $A2                             ; 53ED 85 A2                    ..
	lda     #>LB16A
	adc     #$00                            ; 53F1 69 00                    i.
	sta     $A3                             ; 53F3 85 A3                    ..
	sec                                     ; 53F5 38                       8
	lda     #$5A                            ; 53F6 A9 5A                    .Z
	sbc     LB16A                           ; 53F8 ED 6A B1                 .j.
	sta     $A4                             ; 53FB 85 A4                    ..
	lda     #$00                            ; 53FD A9 00                    ..
	sta     $A5                             ; 53FF 85 A5                    ..
	ldy     $A2                             ; 5401 A4 A2                    ..
	ldxai	LB16A
	jsr     blockmove
	lda     L4650                           ; 540A AD 50 46                 .PF
	lbeq	L541E
	yldi	L464B, $00
	ldxai	LB16A
	jsr     sub_52E1
L541E:  jmp     L5449                           ; 541E 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5421:  lda     L5393                           ; 5421 AD 93 53                 ..S
	eor     #$15                            ; 5424 49 15                    I.
	lbne	L5435
	ldxai	LB16A
	jsr     sub_52E1
	jmp     L5449                           ; 5432 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5435:	yldi	L5392, $01
	lda     CDTMF3
	lbne	L5449
	ldxai	LB16A
	jsr     sub_52E1
L5449:  jmp     L545B                           ; 5449 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L544C:  lda     CDTMF3
	lbne	L545B
	ldxai	LB16A
	jsr     sub_52E1
L545B:  lda     L5392                           ; 545B AD 92 53                 ..S
	sta     $A0                             ; 545E 85 A0                    ..
	rts                                     ; 5460 60                       `

; ----------------------------------------------------------------------------
sub_5461:  
	prolog
	lda     L4651                           ; 5464 AD 51 46                 .QF
	lbne	L5471
	ldi	$A0, $00
	rts                                     ; 5470 60                       `

; ----------------------------------------------------------------------------
L5471:	add8i	$AE, L531D, $01
	lda     LB1C8                           ; 5479 AD C8 B1                 ...
	eor     $AE                             ; 547C 45 AE                    E.
	lbne	L54E3
	jsr     sub_537F
	mv	L531D, LB1C8
	and8i	off_AE, LB1C6, $80
	lda     $AE                             ; 5493 A5 AE                    ..
	lbeq	L54B8
	lda     #>LB1C9
	sta     $A3                             ; 549C 85 A3                    ..
	and8i	off_AE, LB1C6, $7F
	sub8i	$A4, off_AE, $04
	ldy     #<LB1C9
	ldxai	LB224
	jsr     L5197                           ; 54B2 20 97 51                  .Q
	jmp     L54D6                           ; 54B5 4C D6 54                 L.T

; ----------------------------------------------------------------------------
L54B8:	sub8i	L4649, LB1C6, $04
	blkmv_iii LB224, LB1C9, $005C
L54D6:	yldi	L4651, $00
	ldi	$A0, $01
	rts                                     ; 54DF 60                       `

; ----------------------------------------------------------------------------
	jmp     L54F7                           ; 54E0 4C F7 54                 L.T

; ----------------------------------------------------------------------------
L54E3:  lda     LB1C8                           ; 54E3 AD C8 B1                 ...
	eor     L531D                           ; 54E6 4D 1D 53                 M.S
	lbne	L54F4
	jsr     sub_537F
	jmp     L54F7                           ; 54F1 4C F7 54                 L.T

; ----------------------------------------------------------------------------
L54F4:  jsr     sub_5373
L54F7:  ldi	$A0, $00
	rts                                     ; 54FB 60                       `

; ----------------------------------------------------------------------------
L54FC:	.byte	$44                             ; 54FC 44                       D
L54FD:	.byte	$6F                             ; 54FD 6F                       o
	.byte   $77                             ; 54FE 77                       w

; ----------------------------------------------------------------------------
sub_54FF:  
	prolog
	lda     L4650                           ; 5502 AD 50 46                 .PF
	lbeq	L552F
	jsr     sub_5394
	lda     $A0                             ; 550D A5 A0                    ..
	sta     L54FC                           ; 550F 8D FC 54                 ..T
	lda     L54FC                           ; 5512 AD FC 54                 ..T
	eor     #$01                            ; 5515 49 01                    I.
	lbne	L552A
	jsr     sub_5461
	mv	L54FC, $A0
	mv	$A0, L54FC
	rts                                     ; 5529 60                       `

; ----------------------------------------------------------------------------
L552A:  ldi	$A0, $00
	rts                                     ; 552E 60                       `

; ----------------------------------------------------------------------------
L552F:  jsr     sub_4FC5
	mv	L54FC, $A0
	lda     L54FC                           ; 5537 AD FC 54                 ..T
	eor     #$04                            ; 553A 49 04                    I.
	lbne	L5547
	jsr     L5388                           ; 5541 20 88 53                  .S
	jmp     L5579                           ; 5544 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L5547:  lda     L54FC                           ; 5547 AD FC 54                 ..T
	eor     #$01                            ; 554A 49 01                    I.
	lbne	L5579
	mv	L54FD, LB1C9
	lda     L54FD                           ; 5557 AD FD 54                 ..T
	eor     #$06                            ; 555A 49 06                    I.
	lbeq	L5579
	lda     L54FD                           ; 5561 AD FD 54                 ..T
	eor     #$15                            ; 5564 49 15                    I.
	lbeq	L5579
	jsr     sub_5461
	mv	L54FC, $A0
	mv	$A0, L54FC
	rts                                     ; 5578 60                       `

; ----------------------------------------------------------------------------
L5579:  ldi	$A0, 00
	rts                                     ; 557D 60                       `

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
L559C:	.byte	$20
L559D:	.byte	$20
L559E:	.byte	$20
L559F:	.byte	$20

; ----------------------------------------------------------------------------
sub_55A0:
	stack_prolog L557E, $0D
	lda     #$00                            ; 55A9 A9 00                    ..
	cmp     L4653                           ; 55AB CD 53 46                 .SF
	lbcs	L55B4
L55B3:  rts                                     ; 55B3 60                       `

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
	sta     L559F                           ; 55FC 8D 9F 55                 ..U
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
	ldy     #$00                            ; 582B A0 00                    ..
	lda     ($AE),y                         ; 582D B1 AE                    ..
	sta     L5595                           ; 582F 8D 95 55                 ..U
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
	mv	$A3, L559F
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

; ----------------------------------------------------------------------------
L58E7:	.byte	$44
L58E8:  .byte	$28
L58E9:	.byte	$20
L58EA:	.byte	$24
L58EB:	.byte	$46
L58EC:	.byte	$44
L58ED:	.byte	$29
L58EE:	.byte	$20
L58EF:	.byte	$20
L58F0:	.byte	$20
L58F1:	.byte	$20

sub_58F2:
	stack_prolog L58E7, $03
	ldy     #$01                            ; 58FB A0 01                    ..
	sty     L4654                           ; 58FD 8C 54 46                 .TF
	sty     L58ED                           ; 5900 8C ED 58                 ..X
	dmv	off_AE, L58E7
	dey                                     ; 590D 88                       .
	lda     (off_AE),y
	sta     L591E                           ; 5910 8D 1E 59                 ..Y
L5913:  lda     L591E                           ; 5913 AD 1E 59                 ..Y
	cmp     L58ED                           ; 5916 CD ED 58                 ..X
	bcs     L591F                           ; 5919 B0 04                    ..
	jmp     L5CFB                           ; 591B 4C FB 5C                 L.\

; ----------------------------------------------------------------------------
L591E:	.byte	$20                             ; 591E 20                        

; ----------------------------------------------------------------------------
L591F:	add16m8	off_AE, L58E7, L58ED
	ldp8	L58EE
	lda     L58EE                           ; 5936 AD EE 58                 ..X
	eor     #'A'
	lbne	L5A35
	clc                                     ; 5940 18                       .
	lda     #$24                            ; 5941 A9 24                    .$
	adc     L4654                           ; 5943 6D 54 46                 mTF
	sta     L58EB                           ; 5946 8D EB 58                 ..X
	lda     #$B2                            ; 5949 A9 B2                    ..
	adc     #$00                            ; 594B 69 00                    i.
	sta     L58EC                           ; 594D 8D EC 58                 ..X
	dmv	off_AE, L58E9
	lda     L58EC                           ; 595A AD EC 58                 ..X
	iny                                     ; 595D C8                       .
	sta     ($AE),y                         ; 595E 91 AE                    ..
	lda     L58EB                           ; 5960 AD EB 58                 ..X
	dey                                     ; 5963 88                       .
	sta     ($AE),y                         ; 5964 91 AE                    ..
	add16i	L58E9, L58E9, $0002
	inc     L58ED                           ; 5977 EE ED 58                 ..X
	add16m8	off_AE, L58E7, L58ED
	lda     ($AE),y                         ; 598A B1 AE                    ..
	sta     $A0                             ; 598C 85 A0                    ..
	lda     $A0                             ; 598E A5 A0                    ..
	jsr     sub_4B39
	lda     $A0                             ; 5993 A5 A0                    ..
	sta     L58EE                           ; 5995 8D EE 58                 ..X
	yldi	L58EF, $00
	lda     L58EE                           ; 599D AD EE 58                 ..X
	lbne	L59E1
	clc                                     ; 59A5 18                       .
	lda     #$24                            ; 59A6 A9 24                    .$
	adc     L4654                           ; 59A8 6D 54 46                 mTF
	sta     $A0                             ; 59AB 85 A0                    ..
	lda     #$B2                            ; 59AD A9 B2                    ..
	adc     #$00                            ; 59AF 69 00                    i.
	sta     $A1                             ; 59B1 85 A1                    ..
	ldxa	$A0
	jsr     sub_4B47
	lda     $A0                             ; 59BA A5 A0                    ..
	sta     L58EE                           ; 59BC 8D EE 58                 ..X
	clc                                     ; 59BF 18                       .
	lda     L4654                           ; 59C0 AD 54 46                 .TF
	adc     #$02                            ; 59C3 69 02                    i.
	sta     L4654                           ; 59C5 8D 54 46                 .TF
	dmv	off_AE, L58EB
	lda     L58EE                           ; 59D2 AD EE 58                 ..X
	ldy     #$00                            ; 59D5 A0 00                    ..
	sta     ($AE),y                         ; 59D7 91 AE                    ..
	inc16	L58EB
L59E1:  lda     L58EF                           ; 59E1 AD EF 58                 ..X
	cmp     L58EE                           ; 59E4 CD EE 58                 ..X
	lbcs	L5A32
	add16m8	off_AE, L58EB, L58EF
	push16	off_AE
	clc                                     ; 5A02 18                       .
	lda     #$24                            ; 5A03 A9 24                    .$
	adc     L4654                           ; 5A05 6D 54 46                 mTF
	sta     $A0                             ; 5A08 85 A0                    ..
	lda     #$B2                            ; 5A0A A9 B2                    ..
	adc     #$00                            ; 5A0C 69 00                    i.
	sta     $A1                             ; 5A0E 85 A1                    ..
	ldx     $A1                             ; 5A10 A6 A1                    ..
	lda     $A0                             ; 5A12 A5 A0                    ..
	jsr     sub_4B47
	pull16	off_AE
	lda     $A0                             ; 5A1D A5 A0                    ..
	ldy     #$00                            ; 5A1F A0 00                    ..
	sta     ($AE),y                         ; 5A21 91 AE                    ..
	add8i	L4654, L4654, $02
	inc     L58EF                           ; 5A2C EE EF 58                 ..X
	jmp     L59E1                           ; 5A2F 4C E1 59                 L.Y

; ----------------------------------------------------------------------------
L5A32:  jmp     L5CF5                           ; 5A32 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A35:  lda     L58EE                           ; 5A35 AD EE 58                 ..X
	eor     #'B'
	lbne	L5A90
	rdmv	L58EB, L58E9
	inc16	L58E9
L5A53:	dmv	off_AE, L58EB
	push16	off_AE
	clc                                     ; 5A63 18                       .
	lda     #$24                            ; 5A64 A9 24                    .$
	adc     L4654                           ; 5A66 6D 54 46                 mTF
	sta     $A0                             ; 5A69 85 A0                    ..
	lda     #$B2                            ; 5A6B A9 B2                    ..
	adc     #$00                            ; 5A6D 69 00                    i.
	sta     $A1                             ; 5A6F 85 A1                    ..
	ldxa	$A0
	jsr     sub_4B47
	pull16	off_AE
	lda     $A0                             ; 5A7E A5 A0                    ..
	ldy     #$00                            ; 5A80 A0 00                    ..
	sta     ($AE),y                         ; 5A82 91 AE                    ..
	add8i	L4654, L4654, $02
	jmp     L5CF5                           ; 5A8D 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A90:  lda     L58EE                           ; 5A90 AD EE 58                 ..X
	eor     #'C'
	lbne	L5ADD
	rdmv	L58EB, L58E9
	inc16	L58E9
L5AAE:	dmv	off_AE, L58EB
	push16	off_AE
	ldx     L4654                           ; 5ABE AE 54 46                 .TF
	lda     LB224,x
	sta     $A0                             ; 5AC4 85 A0                    ..
	lda     $A0                             ; 5AC6 A5 A0                    ..
	jsr     sub_4B7B
	pull16	off_AE
	lda     $A0                             ; 5AD1 A5 A0                    ..
	ldy     #$00                            ; 5AD3 A0 00                    ..
	sta     (off_AE),y
	inc     L4654                           ; 5AD7 EE 54 46                 .TF
	jmp     L5CF5                           ; 5ADA 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5ADD:  lda     L58EE                           ; 5ADD AD EE 58                 ..X
	eor     #'D'
	lbne	L5B2A
	rdmv	L58EB, L58E9
	inc16	L58E9
	dmv	off_AE, L58EB
	push16	off_AE
	ldx     L4654                           ; 5B0B AE 54 46                 .TF
	lda     LB224,x                         ; 5B0E BD 24 B2                 .$.
	sta     $A0                             ; 5B11 85 A0                    ..
	proc8	sub_4B39, $A0
	pull16	off_AE
	lda     $A0                             ; 5B1E A5 A0                    ..
	ldy     #$00                            ; 5B20 A0 00                    ..
	sta     ($AE),y                         ; 5B22 91 AE                    ..
	inc     L4654                           ; 5B24 EE 54 46                 .TF
	jmp     L5CF5                           ; 5B27 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5B2A:  lda     L58EE                           ; 5B2A AD EE 58                 ..X
	eor     #'R'
	lbne	L5BC1
	clc                                     ; 5B34 18                       .
	lda     #$24                            ; 5B35 A9 24                    .$
	adc     L4654                           ; 5B37 6D 54 46                 mTF
	sta     L58EB                           ; 5B3A 8D EB 58                 ..X
	lda     #$B2                            ; 5B3D A9 B2                    ..
	adc     #$00                            ; 5B3F 69 00                    i.
	sta     L58EC                           ; 5B41 8D EC 58                 ..X
	dmv	off_AE, L58E9
	lda     L58EC                           ; 5B4E AD EC 58                 ..X
	iny                                     ; 5B51 C8                       .
	sta     ($AE),y                         ; 5B52 91 AE                    ..
	lda     L58EB                           ; 5B54 AD EB 58                 ..X
	dey                                     ; 5B57 88                       .
	sta     ($AE),y                         ; 5B58 91 AE                    ..
	add16i	L58E9, L58E9, $0002
	sty     L58EF                           ; 5B6B 8C EF 58                 ..X
L5B6E:  lda     #$03                            ; 5B6E A9 03                    ..
	cmp     L58EF                           ; 5B70 CD EF 58                 ..X
	lbcc	L5BBE
	clc                                     ; 5B78 18                       .
	lda     L58EB                           ; 5B79 AD EB 58                 ..X
	adc     L58EF                           ; 5B7C 6D EF 58                 m.X
	sta     $AE                             ; 5B7F 85 AE                    ..
	lda     L58EC                           ; 5B81 AD EC 58                 ..X
	adc     #$00                            ; 5B84 69 00                    i.
	sta     $AF                             ; 5B86 85 AF                    ..
	push16	off_AE
	clc                                     ; 5B8E 18                       .
	lda     #$24                            ; 5B8F A9 24                    .$
	adc     L4654                           ; 5B91 6D 54 46                 mTF
	sta     $A0                             ; 5B94 85 A0                    ..
	lda     #$B2                            ; 5B96 A9 B2                    ..
	adc     #$00                            ; 5B98 69 00                    i.
	sta     $A1                             ; 5B9A 85 A1                    ..
	ldxa	$A0
	jsr     sub_4B47
	pull16	off_AE
	lda     $A0                             ; 5BA9 A5 A0                    ..
	ldy     #$00                            ; 5BAB A0 00                    ..
	sta     ($AE),y                         ; 5BAD 91 AE                    ..
	add8i	L4654, L4654, $02
	inc     L58EF                           ; 5BB8 EE EF 58                 ..X
	jmp     L5B6E                           ; 5BBB 4C 6E 5B                 Ln[

; ----------------------------------------------------------------------------
L5BBE:  jmp     L5CF5                           ; 5BBE 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5BC1:  lda     L58EE                           ; 5BC1 AD EE 58                 ..X
	eor     #'S'
	beq     L5BD2                           ; 5BC6 F0 0A                    ..
	lda     L58EE                           ; 5BC8 AD EE 58                 ..X
	eor     #'s'
	lbne	L5C95
L5BD2:	clc                                     ; 5BD2 18                       .
	lda     #$24                            ; 5BD3 A9 24                    .$
	adc     L4654                           ; 5BD5 6D 54 46                 mTF
	sta     L58EB                           ; 5BD8 8D EB 58                 ..X
	lda     #$B2                            ; 5BDB A9 B2                    ..
	adc     #$00                            ; 5BDD 69 00                    i.
	sta     L58EC                           ; 5BDF 8D EC 58                 ..X
	dmv	off_AE, L58E9
	lda     L58EC                           ; 5BEC AD EC 58                 ..X
	ldy     #$01                            ; 5BEF A0 01                    ..
	sta     ($AE),y                         ; 5BF1 91 AE                    ..
	lda     L58EB                           ; 5BF3 AD EB 58                 ..X
	dey                                     ; 5BF6 88                       .
	sta     ($AE),y                         ; 5BF7 91 AE                    ..
	add16i	L58E9, L58E9, $0002
	clc                                     ; 5C0A 18                       .
	lda     #$24                            ; 5C0B A9 24                    .$
	adc     L4654                           ; 5C0D 6D 54 46                 mTF
	sta     $A0                             ; 5C10 85 A0                    ..
	lda     #$B2                            ; 5C12 A9 B2                    ..
	adc     #$00                            ; 5C14 69 00                    i.
	sta     $A1                             ; 5C16 85 A1                    ..
	ldxa	$A0
	jsr     sub_4B47
	mv	L58F0, $A0
	dmv	off_AE, L58EB
	lda     L58F0                           ; 5C2E AD F0 58                 ..X
	ldy     #$00                            ; 5C31 A0 00                    ..
	sta     ($AE),y                         ; 5C33 91 AE                    ..
	add8i	L4654, L4654, $02
	iny                                     ; 5C3E C8                       .
	sty     L58EF                           ; 5C3F 8C EF 58                 ..X
	lda     L58F0                           ; 5C42 AD F0 58                 ..X
	sta     L5C53                           ; 5C45 8D 53 5C                 .S\
L5C48:  lda     L5C53                           ; 5C48 AD 53 5C                 .S\
	cmp     L58EF                           ; 5C4B CD EF 58                 ..X
	bcs     L5C54                           ; 5C4E B0 04                    ..
	jmp     L5C92                           ; 5C50 4C 92 5C                 L.\

; ----------------------------------------------------------------------------
L5C53:  .byte	$00

; ----------------------------------------------------------------------------
L5C54:  ldx     L4654                           ; 5C54 AE 54 46                 .TF
	lda     LB224,x                         ; 5C57 BD 24 B2                 .$.
	sta     L58F1                           ; 5C5A 8D F1 58                 ..X
	lda     L58EE                           ; 5C5D AD EE 58                 ..X
	eor     #'S'
	lbne	L5C72
	lda     L58F1                           ; 5C67 AD F1 58                 ..X
	jsr     sub_4B7B
	mv	L58F1, $A0
L5C72:	add16m8 off_AE, L58EB, L58EF
	stp8	L58F1
	inc     L4654                           ; 5C89 EE 54 46                 .TF
	inc     L58EF                           ; 5C8C EE EF 58                 ..X
	jmp     L5C48                           ; 5C8F 4C 48 5C                 LH\

; ----------------------------------------------------------------------------
L5C92:  jmp     L5CF5                           ; 5C92 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5C95:  lda     L58EE                           ; 5C95 AD EE 58                 ..X
	eor     #'X'
	lbne	L5CF5
	sub8i	L58F0, L4649, $01
	mv	LB224, L58F0
	yldi	L58EF, $01
	mv	L5CC4, L58F0
L5CB9:  lda     L5CC4                           ; 5CB9 AD C4 5C                 ..\
	cmp     L58EF                           ; 5CBC CD EF 58                 ..X
	bcs     L5CC5                           ; 5CBF B0 04                    ..
	jmp     L5CE0                           ; 5CC1 4C E0 5C                 L.\

; ----------------------------------------------------------------------------
L5CC4:	.byte	$00

; ----------------------------------------------------------------------------
L5CC5:  ldx     L58EF                           ; 5CC5 AE EF 58                 ..X
	lda     LB224,x                         ; 5CC8 BD 24 B2                 .$.
	sta     $A0                             ; 5CCB 85 A0                    ..
	lda     $A0                             ; 5CCD A5 A0                    ..
	jsr     sub_4B7B
	lda     $A0                             ; 5CD2 A5 A0                    ..
	ldx     L58EF                           ; 5CD4 AE EF 58                 ..X
	sta     LB224,x                         ; 5CD7 9D 24 B2                 .$.
	inc     L58EF                           ; 5CDA EE EF 58                 ..X
	jmp     L5CB9                           ; 5CDD 4C B9 5C                 L.\

; ----------------------------------------------------------------------------
L5CE0:	dmv	off_AE, L58E9
	lda     #$B2                            ; 5CEA A9 B2                    ..
	ldy     #$01                            ; 5CEC A0 01                    ..
	sta     ($AE),y                         ; 5CEE 91 AE                    ..
	lda     #$24                            ; 5CF0 A9 24                    .$
	dey                                     ; 5CF2 88                       .
	sta     ($AE),y                         ; 5CF3 91 AE                    ..
L5CF5:  inc     L58ED                           ; 5CF5 EE ED 58                 ..X
	jmp     L5913                           ; 5CF8 4C 13 59                 L.Y

; ----------------------------------------------------------------------------
L5CFB:  rts                                     ; 5CFB 60                       `

; ----------------------------------------------------------------------------
sub_5CFC:  
	prolog
	jmp     L5D04                           ; 5CFF 4C 04 5D                 L.]

; ----------------------------------------------------------------------------
L5D02:	.byte	$01,"C"

; ----------------------------------------------------------------------------
L5D04:	ldi	$A3, $00
	ldy     #$13                            ; 5D08 A0 13                    ..
	ldxai	L5D02
	jsr     sub_55A0
L5D11:  lda     L4650                           ; 5D11 AD 50 46                 .PF
	lbeq	L5D1F
	jsr     sub_5394
	jmp     L5D11                           ; 5D1C 4C 11 5D                 L.]

; ----------------------------------------------------------------------------
L5D1F:	yldi	$022F, $00
	lda     #$03                            ; 5D24 A9 03                    ..
	jsr     sub_4F6D
L5D29:  jsr     modem_status
	lda     $A0                             ; 5D2C A5 A0                    ..
	lbeq	L5D40
L5D33:  lda     #$02                            ; 5D33 A9 02                    ..
	jsr     sub_45A3
	lda     #$02                            ; 5D38 A9 02                    ..
	jsr     sub_4F6D
	jmp     L5D29                           ; 5D3D 4C 29 5D                 L)]

; ----------------------------------------------------------------------------
L5D40:  lda     $B148                           ; 5D40 AD 48 B1                 .H.
	eor     #$01                            ; 5D43 49 01                    I.
	lbne	L5D5E
	lda     #$00                            ; 5D4A A9 00                    ..
	sta     $A3                             ; 5D4C 85 A3                    ..
	lda     #$A1                            ; 5D4E A9 A1                    ..
	sta     $A4                             ; 5D50 85 A4                    ..
	ldy     #$5A                            ; 5D52 A0 5A                    .Z
	ldx     #$00                            ; 5D54 A2 00                    ..
	lda     #$02                            ; 5D56 A9 02                    ..
	jsr     sub_45D0
	jmp     L5D63                           ; 5D5B 4C 63 5D                 Lc]

; ----------------------------------------------------------------------------
L5D5E:	lda     #$02                            ; 5D5E A9 02                    ..
	jsr     sub_4569
L5D63:  rts                                     ; 5D63 60                       `

; ----------------------------------------------------------------------------
sub_5D64:  
	prolog

sub_5D67:  
	prolog
	lda     $B148                           ; 5D6A AD 48 B1                 .H.
	eor     #$01                            ; 5D6D 49 01                    I.
	lbeq	L5DB9
	lda     #$02                            ; 5D74 A9 02                    ..
	jsr     sub_4569
	lda     #$0D                            ; 5D79 A9 0D                    ..
	sta     $A3                             ; 5D7B 85 A3                    ..
	ldy     #$4A                            ; 5D7D A0 4A                    .J
	ldx     #$A1                            ; 5D7F A2 A1                    ..
	lda     #$02                            ; 5D81 A9 02                    ..
	jsr     sub_4539
	lda     $B149                           ; 5D86 AD 49 B1                 .I.
	sta     $A3                             ; 5D89 85 A3                    ..
	lda     #$00                            ; 5D8B A9 00                    ..
	sta     $A4                             ; 5D8D 85 A4                    ..
	rdldi	$A5, L4AA1
	ldy     #$24                            ; 5D97 A0 24                    .$
	ldx     #$00                            ; 5D99 A2 00                    ..
	lda     #$02                            ; 5D9B A9 02                    ..
	jsr     sub_45D0
	lda     #$20                            ; 5DA0 A9 20                    . 
	sta     $A3                             ; 5DA2 85 A3                    ..
	lda     #$00                            ; 5DA4 A9 00                    ..
	sta     $A4                             ; 5DA6 85 A4                    ..
	rdldi	$A5, L4AA1
	ldy     #$26                            ; 5DB0 A0 26                    .&
	ldx     #$00                            ; 5DB2 A2 00                    ..
	lda     #$02                            ; 5DB4 A9 02                    ..
	jsr     sub_45D0
L5DB9:  lda     #$00                            ; 5DB9 A9 00                    ..
	sta     $A4                             ; 5DBB 85 A4                    ..
	lda     #$80                            ; 5DBD A9 80                    ..
	sta     $A3                             ; 5DBF 85 A3                    ..
	lda     #$28                            ; 5DC1 A9 28                    .(
	sta     $A5                             ; 5DC3 85 A5                    ..
	lda     #$0D                            ; 5DC5 A9 0D                    ..
	sta     $A6                             ; 5DC7 85 A6                    ..
	lda     #$00                            ; 5DC9 A9 00                    ..
	sta     $A7                             ; 5DCB 85 A7                    ..
	ldy     #$06                            ; 5DCD A0 06                    ..
	ldx     #$00                            ; 5DCF A2 00                    ..
	lda     #$02                            ; 5DD1 A9 02                    ..
	jsr     L4AA5                           ; 5DD3 20 A5 4A                  .J
	lda     $B148                           ; 5DD6 AD 48 B1                 .H.
	eor     #$01                            ; 5DD9 49 01                    I.
	beq     L5DE0                           ; 5DDB F0 03                    ..
	jmp     L5DF9                           ; 5DDD 4C F9 5D                 L.]

; ----------------------------------------------------------------------------
L5DE0:  lda     #$00                            ; 5DE0 A9 00                    ..
	sta     $A3                             ; 5DE2 85 A3                    ..
	lda     #$00                            ; 5DE4 A9 00                    ..
	sta     $A4                             ; 5DE6 85 A4                    ..
	rdldi	$A5, L4AA1
	ldy     #$59                            ; 5DF0 A0 59                    .Y
	ldx     #$00                            ; 5DF2 A2 00                    ..
	lda     #$02                            ; 5DF4 A9 02                    ..
	jsr     sub_45D0
L5DF9:  jsr     sub_5D64
	lda     #$2A                            ; 5DFC A9 2A                    .*
	sta     $022F                           ; 5DFE 8D 2F 02                 ./.
	lda     #$02                            ; 5E01 A9 02                    ..
	jsr     sub_4F6D
	ldy     #$00                            ; 5E06 A0 00                    ..
	sty     L4653                           ; 5E08 8C 53 46                 .SF
	jmp     L5E10                           ; 5E0B 4C 10 5E                 L.^

; ----------------------------------------------------------------------------
L5E0E:	.byte	$01,"C"

; ----------------------------------------------------------------------------
L5E10:  lda     #$00                            ; 5E10 A9 00                    ..
	sta     $A3                             ; 5E12 85 A3                    ..
	ldy     #$11                            ; 5E14 A0 11                    ..
	ldxai	L5E0E
	jsr     sub_55A0
	rts                                     ; 5E1D 60                       `

; ----------------------------------------------------------------------------
sub_5E1E:
	prolog 
	lda     L4653                           ; 5E21 AD 53 46                 .SF
	lbne	L5E2C
	jsr     sub_5CFC
L5E2C:  inc     L4653                           ; 5E2C EE 53 46                 .SF
	rts                                     ; 5E2F 60                       `

; ----------------------------------------------------------------------------
sub_5E30:
	prolog
	lda     L4653                           ; 5E33 AD 53 46                 .SF
	lbne	L5E3C
L5E3B:  rts                                     ; 5E3B 60                       `

; ----------------------------------------------------------------------------
L5E3C:  lda     L4653                           ; 5E3C AD 53 46                 .SF
	eor     #$01                            ; 5E3F 49 01                    I.
	lbne	L5E51
	jsr     sub_5D67
	yldi	L4656, $01
	jmp     L5E5A                           ; 5E4E 4C 5A 5E                 LZ^

; ----------------------------------------------------------------------------
L5E51:	sub8i	L4653, L4653, $01
L5E5A:  rts                                     ; 5E5A 60                       `

; ----------------------------------------------------------------------------
L5E5B:  .byte	$00
L5E5C:  .byte	$00
L5E5D:  .byte	$00

sub_5E5E:  
	prolog
	stxa	L5E5B
	ldy     #$00                            ; 5E67 A0 00                    ..
	sty     L4AA4                           ; 5E69 8C A4 4A                 ..J
	lda     L4653                           ; 5E6C AD 53 46                 .SF
	sta     L5E5D                           ; 5E6F 8D 5D 5E                 .]^
L5E72:  lda     #$00                            ; 5E72 A9 00                    ..
	cmp     L4653                           ; 5E74 CD 53 46                 .SF
	lbcs	L5E82
	jsr     sub_5E30
	jmp     L5E72                           ; 5E7F 4C 72 5E                 Lr^

; ----------------------------------------------------------------------------
L5E82:	mv	L464D, L5E5B
	jmp     L5E8F                           ; 5E88 4C 8F 5E                 L.^

; ----------------------------------------------------------------------------
L5E8B:	.byte	$03,"CBB"

; ----------------------------------------------------------------------------
L5E8F:  lda     #$00                            ; 5E8F A9 00                    ..
	sta     $A3                             ; 5E91 85 A3                    ..
	lda     #$00                            ; 5E93 A9 00                    ..
	sta     $A5                             ; 5E95 85 A5                    ..
	lda     L5E5B                           ; 5E97 AD 5B 5E                 .[^
	sta     $A4                             ; 5E9A 85 A4                    ..
	lda     L5E5C                           ; 5E9C AD 5C 5E                 .\^
	lsr     a                               ; 5E9F 4A                       J
	lsr     a                               ; 5EA0 4A                       J
	lsr     a                               ; 5EA1 4A                       J
	lsr     a                               ; 5EA2 4A                       J
	sta     $A6                             ; 5EA3 85 A6                    ..
	lda     #$00                            ; 5EA5 A9 00                    ..
	sta     $A7                             ; 5EA7 85 A7                    ..
	ldy     #$2A                            ; 5EA9 A0 2A                    .*
	ldxai	L5E8B
	jsr     sub_55A0
L5EB2:  lda     L4653                           ; 5EB2 AD 53 46                 .SF
	cmp     L5E5D                           ; 5EB5 CD 5D 5E                 .]^
	lbcs	L5EC3
	jsr     sub_5E1E
	jmp     L5EB2                           ; 5EC0 4C B2 5E                 L.^

; ----------------------------------------------------------------------------
L5EC3:  rts                                     ; 5EC3 60                       `

; ----------------------------------------------------------------------------

sub_5EC4:
	sta     $A0                             ; 5EC4 85 A0                    ..
	stx     $A1                             ; 5EC6 86 A1                    ..
	sty     $A2                             ; 5EC8 84 A2                    ..
	ldy     #$00                            ; 5ECA A0 00                    ..
	eor     #$FF                            ; 5ECC 49 FF                    I.
	and     ($A1),y                         ; 5ECE 31 A1                    1.
	sta     $A4                             ; 5ED0 85 A4                    ..
	lda     $A0                             ; 5ED2 A5 A0                    ..
	and     $A3                             ; 5ED4 25 A3                    %.
	ora     $A4                             ; 5ED6 05 A4                    ..
	sta     ($A1),y                         ; 5ED8 91 A1                    ..
	lda     #$01                            ; 5EDA A9 01                    ..
	sta     $A0                             ; 5EDC 85 A0                    ..
	rts                                     ; 5EDE 60                       `

; ----------------------------------------------------------------------------

sub_5EDF:
	sta     $A0                             ; 5EDF 85 A0                    ..
	stx     $A1                             ; 5EE1 86 A1                    ..
	sty     $A2                             ; 5EE3 84 A2                    ..
	ldy     #$00                            ; 5EE5 A0 00                    ..
	and     ($A1),y                         ; 5EE7 31 A1                    1.
	sta     $A4                             ; 5EE9 85 A4                    ..
	ldy     #$01                            ; 5EEB A0 01                    ..
	lda     $A0                             ; 5EED A5 A0                    ..
	and     $A3                             ; 5EEF 25 A3                    %.
	cmp     $A4                             ; 5EF1 C5 A4                    ..
	beq     L5EF7                           ; 5EF3 F0 02                    ..
	ldy     #$00                            ; 5EF5 A0 00                    ..
L5EF7:  sty     $A0                             ; 5EF7 84 A0                    ..
	rts                                     ; 5EF9 60                       `

; ----------------------------------------------------------------------------
L5EFA:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00

L5EFE:  prolog
	jsr     sub_44D5                        ; 5F01 20 D5 44                  .D
	.addr	L5EFA
	.byte	$03
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
L5F14:	.byte	$80                             ; 5F14 80                       .
L5F15:	.byte	$B2                             ; 5F15 B2                       .

sub_5F16:  
	stack_prolog L5F07, $04
	ldi	$84, $03
	lda     L5F08                           ; 5F23 AD 08 5F                 .._
	tax                                     ; 5F26 AA                       .
	lda     L5F07                           ; 5F27 AD 07 5F                 .._
	jsr     sub_43E0
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
	jsr     sub_43E0
	sta     L5F12                           ; 5F57 8D 12 5F                 .._
	ldy     #$00                            ; 5F5A A0 00                    ..
	sty     L5F0F                           ; 5F5C 8C 0F 5F                 .._
	sty     L5F0E                           ; 5F5F 8C 0E 5F                 .._
	sty     L5F11                           ; 5F62 8C 11 5F                 .._
	iny                                     ; 5F65 C8                       .
	sty     L5F10                           ; 5F66 8C 10 5F                 .._
	lda     L5F09                           ; 5F69 AD 09 5F                 .._
	sta     L5F86                           ; 5F6C 8D 86 5F                 .._
	lda     L5F0A                           ; 5F6F AD 0A 5F                 .._
	sta     L5F87                           ; 5F72 8D 87 5F                 .._
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
L5F88:  clc                                     ; 5F88 18                       .
	lda     L5F14                           ; 5F89 AD 14 5F                 .._
	adc     L5F0C                           ; 5F8C 6D 0C 5F                 m._
	sta     $A1                             ; 5F8F 85 A1                    ..
	lda     L5F15                           ; 5F91 AD 15 5F                 .._
	adc     L5F0D                           ; 5F94 6D 0D 5F                 m._
	sta     $A2                             ; 5F97 85 A2                    ..
	lda     L5F0B                           ; 5F99 AD 0B 5F                 .._
	sta     $A3                             ; 5F9C 85 A3                    ..
	ldy     $A2                             ; 5F9E A4 A2                    ..
	ldx     $A1                             ; 5FA0 A6 A1                    ..
	lda     L5F12                           ; 5FA2 AD 12 5F                 .._
	jsr     L5EFE                           ; 5FA5 20 FE 5E                  .^
	lda     $A0                             ; 5FA8 A5 A0                    ..
	beq     L5FAF                           ; 5FAA F0 03                    ..
	jmp     L5FBA                           ; 5FAC 4C BA 5F                 L._

; ----------------------------------------------------------------------------
L5FAF:  lda     L5F0F                           ; 5FAF AD 0F 5F                 .._
	sta     $A1                             ; 5FB2 85 A1                    ..
	lda     L5F0E                           ; 5FB4 AD 0E 5F                 .._
	sta     $A0                             ; 5FB7 85 A0                    ..
	rts                                     ; 5FB9 60                       `

; ----------------------------------------------------------------------------
L5FBA:  inc     L5F0E                           ; 5FBA EE 0E 5F                 .._
	bne     L5FC2                           ; 5FBD D0 03                    ..
	inc     L5F0F                           ; 5FBF EE 0F 5F                 .._
L5FC2:  lsr     L5F12                           ; 5FC2 4E 12 5F                 N._
	lda     L5F12                           ; 5FC5 AD 12 5F                 .._
	beq     L5FCD                           ; 5FC8 F0 03                    ..
	jmp     L5FDA                           ; 5FCA 4C DA 5F                 L._

; ----------------------------------------------------------------------------
L5FCD:  lda     #$80                            ; 5FCD A9 80                    ..
	sta     L5F12                           ; 5FCF 8D 12 5F                 .._
	inc     L5F0C                           ; 5FD2 EE 0C 5F                 .._
	bne     L5FDA                           ; 5FD5 D0 03                    ..
	inc     L5F0D                           ; 5FD7 EE 0D 5F                 .._
L5FDA:  inc     L5F10                           ; 5FDA EE 10 5F                 .._
	bne     L5FE2                           ; 5FDD D0 03                    ..
	inc     L5F11                           ; 5FDF EE 11 5F                 .._
L5FE2:  jmp     L5F75                           ; 5FE2 4C 75 5F                 Lu_

; ----------------------------------------------------------------------------
L5FE5:  lda     L5F0F                           ; 5FE5 AD 0F 5F                 .._
	sta     $A1                             ; 5FE8 85 A1                    ..
	lda     L5F0E                           ; 5FEA AD 0E 5F                 .._
	sta     $A0                             ; 5FED 85 A0                    ..
	rts                                     ; 5FEF 60                       `

; ----------------------------------------------------------------------------
L5FF0:  .byte	$00
L5FF1:  .byte	$00
L5FF2:  .byte	$00
L5FF3:  .byte	$00
L5FF4:  .byte	$00

sub_5FF5:  
	stack_prolog L5FF0, $04
	rdldi	L5EFE+1, sub_5EC4
	dmv	$A3, L5FF3
	ldy     L5FF2                           ; 6012 AC F2 5F                 .._
	ldxa	L5FF0
	jsr     sub_5F16
	rts                                     ; 601E 60                       `

; ----------------------------------------------------------------------------
L601F:  .byte	$00
L6020:  .byte	$00
L6021:  .byte	$00
L6022:  .byte	$00
L6023:  .byte	$00
L6024:  .byte	$00
L6025:  .byte	$00

L6026:  
	stack_prolog L601F, $04
	rdldi	L5EFE+1, sub_5EDF
	dmv	$A3, L6022
	ldy     L6021                           ; 6043 AC 21 60                 .!`
	ldxa	L601F
	jsr     sub_5F16
	rdmv	L6024, $A0
	rdmv	$A0, L6024
	rts                                     ; 6063 60                       `

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
	stx     L6064+1                         ; 6071 8E 65 60                 .e`
	sta     L6064                           ; 6074 8D 64 60                 .d`
	add16i	off_AE, L6064, $0003
	lda     #$02                            ; 6086 A9 02                    ..
	sta     $84                             ; 6088 85 84                    ..
	lda     off_AE+1
	tax                                     ; 608C AA                       .
	lda     off_AE
	jsr     sub_43E0
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
	jsr     L6026                           ; 60C7 20 26 60                  &`
	clc                                     ; 60CA 18                       .
	lda     L6068                           ; 60CB AD 68 60                 .h`
	adc     $A0                             ; 60CE 65 A0                    e.
	sta     L6068                           ; 60D0 8D 68 60                 .h`
	lda     L6069                           ; 60D3 AD 69 60                 .i`
	adc     $A1                             ; 60D6 65 A1                    e.
	sta     L6069                           ; 60D8 8D 69 60                 .i`
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
	jsr     L6026                           ; 60FF 20 26 60                  &`
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
	jsr     sub_43D1
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
L617C:  lda     #$00                            ; 617C A9 00                    ..
	sta     $A3                             ; 617E 85 A3                    ..
	ldy     #$26                            ; 6180 A0 26                    .&
	ldxai	L617A
	jsr     sub_55A0
	ldi	$A1, $00
	ldi	$A0, $00
	rts                                     ; 6191 60                       `

; ----------------------------------------------------------------------------
L6192:  .byte	$00
L6193:  .byte	$00
L6194:	.byte	$00,$00
L6196:  .byte	$00
L6197:  .byte	$00
L6198:  .byte	$00
L6199:  .byte	$00

; ----------------------------------------------------------------------------
sub_619A:  
	stack_prolog L6192, $03
	add16i off_AE, L6194, $0003
	ldi	$84, $02
	ld2xa	off_AE
	jsr     sub_43E0
	st2xa	L6196
	sub16m	off_AE, L6192, MEMLO
	ldi	$84, $02
	ld2xa	off_AE
	jsr     sub_43E0
	st2xa	L6198
	mv	$A3, L6197
	ldi	$A4, $01
	ldy     L6196                           ; 61F2 AC 96 61                 ..a
	ldxa	L6198
	jsr     sub_5FF5
	rts                                     ; 61FE 60                       `

; ----------------------------------------------------------------------------
L61FF:  .byte	$00
L6200:  .byte	$00
L6201:  .byte	$00
L6202:  .byte	$00

; ----------------------------------------------------------------------------
sub_6203:  
	prolog
	shladdi	off_AE, L466F, $0001
	ldp16	$A0
	dmv	off_AC, L466F
	ld2p16	$A2, $AC
	lda     #$00                            ; 6238 A9 00                    ..
	sta     $A5                             ; 623A 85 A5                    ..
	lda     #$20                            ; 623C A9 20                    . 
	sta     $A4                             ; 623E 85 A4                    ..
	ldy     $A2                             ; 6240 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	shladdi	off_AE, L466F, $0001
	ldp16	L61FF
	add16i	off_AE, L61FF, $0003
	add16i	L6201, off_AE, $0001
	dmv	off_AE, L6201
	clc                                     ; 6291 18                       .
	lda     L61FF                           ; 6292 AD FF 61                 ..a
	adc     #$20                            ; 6295 69 20                    i 
	sta     $AC                             ; 6297 85 AC                    ..
	lda     L6200                           ; 6299 AD 00 62                 ..b
	adc     #$00                            ; 629C 69 00                    i.
	iny                                     ; 629E C8                       .
	sta     ($AE),y                         ; 629F 91 AE                    ..
	lda     $AC                             ; 62A1 A5 AC                    ..
	dey                                     ; 62A3 88                       .
	sta     ($AE),y                         ; 62A4 91 AE                    ..
	add16i	L6201, L61FF, $001E
	dmv	off_AE, L6201
	lda     L6200                           ; 62C1 AD 00 62                 ..b
	iny                                     ; 62C4 C8                       .
	sta     ($AE),y                         ; 62C5 91 AE                    ..
	lda     L61FF                           ; 62C7 AD FF 61                 ..a
	dey                                     ; 62CA 88                       .
	sta     ($AE),y                         ; 62CB 91 AE                    ..
	rts                                     ; 62CD 60                       `

; ----------------------------------------------------------------------------
L62CE:  .byte	$00
L62CF:  .byte	$00
L62D0:  .byte	$00

; ----------------------------------------------------------------------------
sub_62D1:
	prolog
	mv	L62D0, $022F
	yldi	$022F, $00
	shladdi	off_AE, L466F, $0001
	iny                                     ; 62F2 C8                       .
	lda     ($AE),y                         ; 62F3 B1 AE                    ..
	sta     $0231                           ; 62F5 8D 31 02                 .1.
	dey                                     ; 62F8 88                       .
	lda     ($AE),y                         ; 62F9 B1 AE                    ..
	sta     $0230                           ; 62FB 8D 30 02                 .0.
	lda     L62D0                           ; 62FE AD D0 62                 ..b
	sta     $022F                           ; 6301 8D 2F 02                 ./.
	ldi	$D40E, $C0
	shladdi off_AE, L466F, $0001
	iny                                     ; 631C C8                       .
	lda     ($AE),y                         ; 631D B1 AE                    ..
	sta     L62CF                           ; 631F 8D CF 62                 ..b
	dey                                     ; 6322 88                       .
	lda     ($AE),y                         ; 6323 B1 AE                    ..
	sta     L62CE                           ; 6325 8D CE 62                 ..b
L6328:	shladdi	off_AE, L466F, $0001
	dmv	off_AC, L466F
	iny                                     ; 6345 C8                       .
	lda     ($AC),y                         ; 6346 B1 AC                    ..
	sta     ($AE),y                         ; 6348 91 AE                    ..
	dey                                     ; 634A 88                       .
	lda     ($AC),y                         ; 634B B1 AC                    ..
	sta     ($AE),y                         ; 634D 91 AE                    ..
	dmv	off_AE, L466F
	lda     L62CF                           ; 6359 AD CF 62                 ..b
	iny                                     ; 635C C8                       .
	sta     ($AE),y                         ; 635D 91 AE                    ..
	lda     L62CE                           ; 635F AD CE 62                 ..b
	dey                                     ; 6362 88                       .
	sta     ($AE),y                         ; 6363 91 AE                    ..
	jsr     sub_6203
	rts                                     ; 6368 60                       `

; ----------------------------------------------------------------------------
L6369:  .byte	$00
L636A:	.byte	$C4                             ; 636A C4                       .
L636B:	.byte	$02                             ; 636B 02                       .
L636C:  .byte	$00
L636D:  .byte	$00

; ----------------------------------------------------------------------------
sub_636E:
	prolog
	lda     L46EF                           ; 6371 AD EF 46                 ..F
	sta     L6369                           ; 6374 8D 69 63                 .ic
	shladdm8 off_AE, L46F5, L6369 
	ldy     #$01                            ; 638B A0 01                    ..
	lda     ($AE),y                         ; 638D B1 AE                    ..
	sta     L636D                           ; 638F 8D 6D 63                 .mc
	dey                                     ; 6392 88                       .
	lda     ($AE),y                         ; 6393 B1 AE                    ..
	sta     L636C                           ; 6395 8D 6C 63                 .lc
	add16i	$A2, L636C, $0003
	lda     #$00                            ; 63A7 A9 00                    ..
	sta     $A5                             ; 63A9 85 A5                    ..
	lda     #$05                            ; 63AB A9 05                    ..
	sta     $A4                             ; 63AD 85 A4                    ..
	ldy     $A2                             ; 63AF A4 A2                    ..
	ldxa	L636A
	jsr     blockmove
	add16i	off_AE, L636C, $0002
	ldy     #$00                            ; 63C9 A0 00                    ..
	lda     ($AE),y                         ; 63CB B1 AE                    ..
	sta	CHBAS
	rts                                     ; 63D0 60                       `

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
	lda     L46EF                           ; 63E0 AD EF 46                 ..F
	sta     L63DB                           ; 63E3 8D DB 63                 ..c
	shladdm8 off_AE, L46F5, L63DB
	ldy     #$01                            ; 63FA A0 01                    ..
	lda     ($AE),y                         ; 63FC B1 AE                    ..
	sta     L63DA                           ; 63FE 8D DA 63                 ..c
	dey                                     ; 6401 88                       .
	lda     ($AE),y                         ; 6402 B1 AE                    ..
	sta     L63D9                           ; 6404 8D D9 63                 ..c
	shladdi off_AE, L466F, $0001
	iny                                     ; 641A C8                       .
	lda     ($AE),y                         ; 641B B1 AE                    ..
	sta     L63D4                           ; 641D 8D D4 63                 ..c
	dey                                     ; 6420 88                       .
	lda     ($AE),y                         ; 6421 B1 AE                    ..
	sta     L63D3                           ; 6423 8D D3 63                 ..c
	add16i	L63D7, L63D3, $0006
	dmv	off_AE, L63D9
	lda     ($AE),y                         ; 6441 B1 AE                    ..
	sta     L63DC                           ; 6443 8D DC 63                 ..c
	clc                                     ; 6446 18                       .
	lda     L63D3                           ; 6447 AD D3 63                 ..c
	adc     #$03                            ; 644A 69 03                    i.
	sta     $AE                             ; 644C 85 AE                    ..
	lda     L63D4                           ; 644E AD D4 63                 ..c
	adc     #$00                            ; 6451 69 00                    i.
	sta     $AF                             ; 6453 85 AF                    ..
	lda     ($AE),y                         ; 6455 B1 AE                    ..
	and     #$F0                            ; 6457 29 F0                    ).
	sta     $AC                             ; 6459 85 AC                    ..
	add8m	L63D6, $AC, L63DC
	add16i	off_AE, L63D3, $0003
	lda     L63D6                           ; 6473 AD D6 63                 ..c
	sta     ($AE),y                         ; 6476 91 AE                    ..
	lda     #$00                            ; 6478 A9 00                    ..
	sta     $A3                             ; 647A 85 A3                    ..
	lda     L63DC                           ; 647C AD DC 63                 ..c
	sta     $A4                             ; 647F 85 A4                    ..
	ldy     #$17                            ; 6481 A0 17                    ..
	ldxa	L63D7
	jsr     sub_45FC
	add16i	off_AE, L63D9, $0001
	ldy     #$00                            ; 649B A0 00                    ..
	lda     ($AE),y                         ; 649D B1 AE                    ..
	sta     $A0                             ; 649F 85 A0                    ..
	ldx     #$02                            ; 64A1 A2 02                    ..
	lda     $A0                             ; 64A3 A5 A0                    ..
	jsr     sub_4983
	sec                                     ; 64A8 38                       8
	lda     $A0                             ; 64A9 A5 A0                    ..
	sbc     #$01                            ; 64AB E9 01                    ..
	sta     L63D5                           ; 64AD 8D D5 63                 ..c
	ldy     #$01                            ; 64B0 A0 01                    ..
	sty     L63D6                           ; 64B2 8C D6 63                 ..c
L64B5:  lda     #$05                            ; 64B5 A9 05                    ..
	cmp     L63D6                           ; 64B7 CD D6 63                 ..c
	bcs     L64BF                           ; 64BA B0 03                    ..
	jmp     L6580                           ; 64BC 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L64BF:  ldx     L63D6                           ; 64BF AE D6 63                 ..c
	lda     L46EF,x                         ; 64C2 BD EF 46                 ..F
	sta     L63DB                           ; 64C5 8D DB 63                 ..c
	lda     L63DB                           ; 64C8 AD DB 63                 ..c
	cmp     #$0A                            ; 64CB C9 0A                    ..
	lbcc	L64D5
	jmp     L6580                           ; 64D2 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L64D5:	shladdm8 off_AE, L46F5, L63DB
	ldy     #$01                            ; 64E9 A0 01                    ..
	lda     ($AE),y                         ; 64EB B1 AE                    ..
	sta     L63DA                           ; 64ED 8D DA 63                 ..c
	dey                                     ; 64F0 88                       .
	lda     ($AE),y                         ; 64F1 B1 AE                    ..
	sta     L63D9                           ; 64F3 8D D9 63                 ..c
	dmv	off_AE, L63D9
	lda     ($AE),y                         ; 6500 B1 AE                    ..
	sta     L63DC                           ; 6502 8D DC 63                 ..c
	sec                                     ; 6505 38                       8
	lda     L63D5                           ; 6506 AD D5 63                 ..c
	sbc     #$01                            ; 6509 E9 01                    ..
	sta     $AE                             ; 650B 85 AE                    ..
	add16m8	$AC, L63D7, $AE
	lda     ($AC),y                         ; 651C B1 AC                    ..
	ora     #$80                            ; 651E 09 80                    ..
	sta     ($AC),y                         ; 6520 91 AC                    ..
	add16m8	$A0, L63D7, L63D5
	sec                                     ; 6532 38                       8
	lda     #$18                            ; 6533 A9 18                    ..
	sbc     L63D5                           ; 6535 ED D5 63                 ..c
	sta     $AC                             ; 6538 85 AC                    ..
	sec                                     ; 653A 38                       8
	lda     $AC                             ; 653B A5 AC                    ..
	sbc     #$01                            ; 653D E9 01                    ..
	sta     $A2                             ; 653F 85 A2                    ..
	lda     #$00                            ; 6541 A9 00                    ..
	sta     $A3                             ; 6543 85 A3                    ..
	lda     L63DC                           ; 6545 AD DC 63                 ..c
	sta     $A4                             ; 6548 85 A4                    ..
	ldy     $A2                             ; 654A A4 A2                    ..
	ldx     $A1                             ; 654C A6 A1                    ..
	lda     $A0                             ; 654E A5 A0                    ..
	jsr     sub_45FC
	add16i	off_AE, L63D9, $0001
	clc                                     ; 6562 18                       .
	lda     L63D5                           ; 6563 AD D5 63                 ..c
	ldy     #$00                            ; 6566 A0 00                    ..
	adc     ($AE),y                         ; 6568 71 AE                    q.
	sta     L63D5                           ; 656A 8D D5 63                 ..c
	lda     L63D5                           ; 656D AD D5 63                 ..c
	cmp     #$18                            ; 6570 C9 18                    ..
	lbcc	L657A
	jmp     L6580                           ; 6577 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L657A:	inc     L63D6                           ; 657A EE D6 63                 ..c
	jmp     L64B5                           ; 657D 4C B5 64                 L.d

; ----------------------------------------------------------------------------
L6580:  clc                                     ; 6580 18                       .
	lda     L63D3                           ; 6581 AD D3 63                 ..c
	adc     #$1E                            ; 6584 69 1E                    i.
	sta     L63D1                           ; 6586 8D D1 63                 ..c
	lda     L63D4                           ; 6589 AD D4 63                 ..c
	adc     #$00                            ; 658C 69 00                    i.
	sta     L63D2                           ; 658E 8D D2 63                 ..c
	lda     L63D1                           ; 6591 AD D1 63                 ..c
	sta     $AE                             ; 6594 85 AE                    ..
	lda     L63D2                           ; 6596 AD D2 63                 ..c
	sta     $AF                             ; 6599 85 AF                    ..
	lda     L63D4                           ; 659B AD D4 63                 ..c
	ldy     #$01                            ; 659E A0 01                    ..
	sta     ($AE),y                         ; 65A0 91 AE                    ..
	lda     L63D3                           ; 65A2 AD D3 63                 ..c
	dey                                     ; 65A5 88                       .
	sta     ($AE),y                         ; 65A6 91 AE                    ..
	jsr     sub_636E
	jsr     sub_62D1
	rts                                     ; 65AE 60                       `

; ----------------------------------------------------------------------------
L65AF:  .byte	$00

; ----------------------------------------------------------------------------
sub_65B0:  
	prolog
	sta     L65AF                           ; 65B3 8D AF 65                 ..e
	shladdm8 off_AE, L46E2, L65AF
	ldp16	$A0
	rts                                     ; 65D5 60                       `

; ----------------------------------------------------------------------------
L65D6:	.byte	$00
L65D7:	.byte	$00
L65D8:	.byte	$00
L65D9:	.byte	$00
L65DA:	.byte	$00
L65DB:  .byte	$00
L65DC:  .byte	$00
L65DD:  .byte	$00
L65DE:  .byte	$00
L65DF:  .byte	$00
L65E0:  .byte	$00
L65E1:  .byte	$00

cmd_uw:  					; "W"
	stack_prolog L65D6, $05
	func16_8 sub_65B0, L65E0, L65D6
	lda     L65E0                           ; 65FB AD E0 65                 ..e
	ora     L65E1                           ; 65FE 0D E1 65                 ..e
	lbne	L6607
	rts                                     ; 6606 60                       `

; ----------------------------------------------------------------------------
L6607:	add16i	off_AE, L65E0, $0004
	ldp16	L65DE
	sub8m	off_AE, L65D9, L65D7
	add8i	L65DC, off_AE, $01
	mv	L65DD, L65D8
	mv	L664B, L65DA
L6640:  lda     L664B                           ; 6640 AD 4B 66                 .Kf
	cmp     L65DD                           ; 6643 CD DD 65                 ..e
	bcs     L664C                           ; 6646 B0 04                    ..
	jmp     L668A                           ; 6648 4C 8A 66                 L.f

; ----------------------------------------------------------------------------
L664B:	.byte	$00

; ----------------------------------------------------------------------------
L664C:	shladdm8 off_AE, L65DE, L65DD
	clc                                     ; 6660 18                       .
	ldy     #$00                            ; 6661 A0 00                    ..
	lda     ($AE),y                         ; 6663 B1 AE                    ..
	adc	L65D7
	sta	$A0
	iny
	lda     ($AE),y                         ; 666B B1 AE                    ..
	adc     #$00                            ; 666D 69 00                    i.
	sta     $A1                             ; 666F 85 A1                    ..
	ldi	$A3, $00
	mv	$A4, L65DB
	ldy     L65DC                           ; 667A AC DC 65                 ..e
	ldxa	$A0
	jsr     sub_45FC
	inc     L65DD                           ; 6684 EE DD 65                 ..e
	jmp     L6640                           ; 6687 4C 40 66                 L@f

; ----------------------------------------------------------------------------
L668A:	yldi	L4656, $01
	rts                                     ; 668F 60                       `

; ----------------------------------------------------------------------------
L6690:  .word	$0000
L6692:  .word	$0000
L6694:  .byte	$00
L6695:  .byte	$00

; ----------------------------------------------------------------------------
cmd_uf:						; "F"  
	prolog
	stxa	L6690
	func16_8 sub_65B0, L6692, L6690
	test16	L6692
	lbne	L66BB
	rts                                     ; 66BA 60                       `

; ----------------------------------------------------------------------------
L66BB:	mv	$A3, L6692+1
	rdldi	$A4, $0002
	ldy     L6692                           ; 66C8 AC 92 66                 ..f
	ldxai	L6694
	jsr     blockmove
	sub8i	$A3, L6694, $01
	sub8i	$A4, L6695, $01
	lda     L6690+1
	sta     $A5                             ; 66E5 85 A5                    ..
	ldy     #$00                            ; 66E7 A0 00                    ..
	ldx     #$00                            ; 66E9 A2 00                    ..
	lda     L6690                           ; 66EB AD 90 66                 ..f
	jsr     cmd_uw
	rts                                     ; 66F1 60                       `

; ----------------------------------------------------------------------------
L66F2:  .byte	$00	
L66F3:  .byte	$00
L66F4:  .byte	$00
L66F5:  .byte	$00
L66F6:	.byte	$00
L66F7:	.byte	$00
	.byte	$00
	.byte	$00
L66FA:	.byte	$00
L66FB:	.byte	$00

; ----------------------------------------------------------------------------
sub_66FC:  
	prolog
	stxa	L66F2
	func16_8 sub_65B0, L66F4, L66F2
	mv	$A3, L66F4+1
	rdldi	$A4, $0006
	ldy     L66F4                           ; 6722 AC F4 66                 ..f
	ldxai	L66F6
	jsr     blockmove
	lda     L66F3                           ; 672C AD F3 66                 ..f
	cmp     L66F7                           ; 672F CD F7 66                 ..f
	lbcc	L6742
	ldx     #$00                            ; 6737 A2 00                    ..
	lda     L66F2
	jsr     cmd_uf
	jmp     L67C3                           ; 673F 4C C3 67                 L.g

; ----------------------------------------------------------------------------
L6742:	dmv	off_AE, L66FA
	ldp16	$A0
	shladdm8 off_AC, L66FA, L66F3
	iny                                     ; 676B C8                       .
	lda     ($AC),y                         ; 676C B1 AC                    ..
	sta     $A3                             ; 676E 85 A3                    ..
	dey                                     ; 6770 88                       .
	lda     ($AC),y                         ; 6771 B1 AC                    ..
	sta     $A2                             ; 6773 85 A2                    ..
	sub8m	$AA, L66F7, L66F3
	ldi	$85, $00
	mv	$84, L66F6
	lda     $AA                             ; 6787 A5 AA                    ..
	ldx     #$00                            ; 6789 A2 00                    ..
	jsr     sub_444A
	st2xa	$A4
	ldy     $A2                             ; 6793 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	sub8m	$A2, L66F7, L66F3
	sub8i	$A3, L66F6, $01
	sub8i	$A4, L66F7, $01
	ldi	$A5, $00
	ldy     $A2                             ; 67B9 A4 A2                    ..
	ldx     #$00                            ; 67BB A2 00                    ..
	lda     L66F2
	jsr     cmd_uw
L67C3:  rts                                     ; 67C3 60                       `

; ----------------------------------------------------------------------------
L67C4:	.byte	$6C
L67C5:	.byte	$0C
L67C6:  .byte	$00
L67C7:	.byte	$AD
L67C8:	.byte	$01
L67C9:	.byte	$D3
L67CA:	.byte	$29
L67CB:	.byte	$FE
L67CC:	.byte	$8D
L67CD:	.byte	$01
L67CE:	.byte	$D3
L67CF:  .byte	$60,$AD
L67D1:	.byte	$01
L67D2:	.byte	$D3
L67D3:	.byte	$09
L67D4:	.byte	$01
L67D5:	.byte	$8D
L67D6:	.byte	$01
L67D7:	.byte	$D3

cmd_ud:					; "D" - display?
	stack_prolog L67C4, $05 
	func16_8 sub_65B0, L67CF, L67C4
	test16	L67CF
	lbne	L67FD
	rts                                     ; 67FC 60                       `

; ----------------------------------------------------------------------------
L67FD:	mv	$A3, L67CF+1
	rdldi	$A4, $0006
	ldy     L67CF                           ; 680A AC CF 67                 ..g
	ldxai	L67D2
	jsr     blockmove
	dmv	off_AE, L67C8
	ldy     #$00                            ; 681E A0 00                    ..
	lda     ($AE),y                         ; 6820 B1 AE                    ..
	sta     L67CA                           ; 6822 8D CA 67                 ..g
	lda     L67D4                           ; 6825 AD D4 67                 ..g
	cmp     L67CA                           ; 6828 CD CA 67                 ..g
	lda     L67D5                           ; 682B AD D5 67                 ..g
	sbc     #$00                            ; 682E E9 00                    ..
	lbcs	L683B
	mv	L67CA, L67D4
L683B:  lda     L67CA                           ; 683B AD CA 67                 ..g
	lbne	L6844
	rts                                     ; 6843 60                       `

; ----------------------------------------------------------------------------
L6844:	sub8m	off_AE, L67D2, L67C5
	lda     $AE                             ; 684D A5 AE                    ..
	cmp     L67C7                           ; 684F CD C7 67                 ..g
	lbcs	L6861
	sub8m	L67C7, L67D2, L67C5
L6861:	sub8i	off_AE, L67CA, $01 
	ldi	$85, $00
	mv	$84, L67C7
	lda     $AE                             ; 6872 A5 AE                    ..
	ldx     #$00                            ; 6874 A2 00                    ..
	jsr     sub_447F
	sta     L67CC                           ; 6879 8D CC 67                 ..g
	add8m	off_AE, L67C6, L67CC
	sub8i	off_AC, L67D3, $01
	sub8m	L67CD, off_AE, off_AC
	ldx     #$00                            ; 6895 A2 00                    ..
	lda     L67CD                           ; 6897 AD CD 67                 ..g
	jsr     sub_496E
	lda     $A0                             ; 689D A5 A0                    ..
	sta     L67CE                           ; 689F 8D CE 67                 ..g
	lda     L67CE                           ; 68A2 AD CE 67                 ..g
	eor     #$01                            ; 68A5 49 01                    I.
	lbne	L68C6
	ldx     L67CD                           ; 68AC AE CD 67                 ..g
	lda     L67C4                           ; 68AF AD C4 67                 ..g
	jsr     sub_66FC
	sec                                     ; 68B5 38                       8
	lda     L67D3                           ; 68B6 AD D3 67                 ..g
	sbc     #$01                            ; 68B9 E9 01                    ..
	sta     $AE                             ; 68BB 85 AE                    ..
	sub8m	L67C6, off_AE, L67CC
L68C6:	mv	L67CB, L67C7
	yldi	L67D1, $00
	mv	L68E2, L67CC
L68D7:  lda     L68E2                           ; 68D7 AD E2 68                 ..h
	cmp     L67D1                           ; 68DA CD D1 67                 ..g
	bcs     L68E3                           ; 68DD B0 04                    ..
	jmp     L6964                           ; 68DF 4C 64 69                 Ldi

; ----------------------------------------------------------------------------
L68E2:	.byte	$07

; ----------------------------------------------------------------------------
L68E3:  lda     L67CA                           ; 68E3 AD CA 67                 ..g
	cmp     L67C7                           ; 68E6 CD C7 67                 ..g
	lbcs	L68F4
	mv	L67CB, L67CA
L68F4:	add8m	off_AE, L67D1, L67C6
	shladdm8 off_AC, L67D6, off_AE
	clc                                     ; 6910 18                       .
	ldy     #$00                            ; 6911 A0 00                    ..
	lda     ($AC),y                         ; 6913 B1 AC                    ..
	adc     L67C5                           ; 6915 6D C5 67                 m.g
	sta     $A0                             ; 6918 85 A0                    ..
	iny                                     ; 691A C8                       .
	lda     ($AC),y                         ; 691B B1 AC                    ..
	adc     #$00                            ; 691D 69 00                    i.
	sta     $A1                             ; 691F 85 A1                    ..
	add16i	$A2, L67C8, $01
	lda     #$00                            ; 6930 A9 00                    ..
	sta     $A5                             ; 6932 85 A5                    ..
	lda     L67CB                           ; 6934 AD CB 67                 ..g
	sta     $A4                             ; 6937 85 A4                    ..
	ldy     $A2                             ; 6939 A4 A2                    ..
	ldxa	$A0
	jsr     blockmove
	add16m8 L67C8, L67C8, L67CB
	sub8m	L67CA, L67CA, L67CB
	inc     L67D1                           ; 695E EE D1 67                 ..g
	jmp     L68D7                           ; 6961 4C D7 68                 L.h

; ----------------------------------------------------------------------------
L6964:	yldi	L4656, $01
	rts                                     ; 6969 60                       `

; ----------------------------------------------------------------------------
sub_696A:  
	prolog
	lda     #$1E                            ; 696D A9 1E                    ..
	asl     a                               ; 696F 0A                       .
	sta     $A2                             ; 6970 85 A2                    ..
	lda     #$00                            ; 6972 A9 00                    ..
	sta     $A3                             ; 6974 85 A3                    ..
	ldy     $A2                             ; 6976 A4 A2                    ..
	ldxa	L46E2
	jsr     sub_45F6
	rts                                     ; 6981 60                       `

; ----------------------------------------------------------------------------
L6982:  .byte	$00
L6983:	.byte	$00,$00
L6985:	.byte	$00,$00
L6987:  .byte	$00
L6988:  .byte	$00
L6989:  .byte	$00
L698A:	.byte	$00
L698B:  .byte	$00
L698C:  .byte	$00
L698D:  .byte	$00
L698E:  .byte	$00
L698F:  .byte	$00
L6990:  .byte	$00
L6991:  .byte	$00,$00
L6993:	.byte	$00,$00

; ----------------------------------------------------------------------------
cmd_uk:						; "K" 
	prolog
	sta     L6982                           ; 6998 8D 82 69                 ..i
	func16_8 sub_65B0, L6983, L6982
	test16	L6983
	lbne	L69B7
	rts                                     ; 69B6 60                       `

; ----------------------------------------------------------------------------
L69B7:	mv	$A3, L6983+1
	rdldi	$A4, $000B
	ldy     L6983                           ; 69C4 AC 83 69                 ..i
	ldxai	L698A
	jsr     blockmove
	dmv	off_AE, L698E
	ldp16	L6985
	lda     L698B                           ; 69E5 AD 8B 69                 ..i
	asl     a                               ; 69E8 0A                       .
	sta     $A2                             ; 69E9 85 A2                    ..
	lda     #$00                            ; 69EB A9 00                    ..
	sta     $A3                             ; 69ED 85 A3                    ..
	ldy     $A2                             ; 69EF A4 A2                    ..
	ldxa	L698E
	jsr     sub_619A
	lda     L698D                           ; 69FA AD 8D 69                 ..i
	sta     $A3                             ; 69FD 85 A3                    ..
	ldy     L698C                           ; 69FF AC 8C 69                 ..i
	ldxa	L6985
	jsr     sub_619A
	lda     #$00                            ; 6A0B A9 00                    ..
	sta     $A3                             ; 6A0D 85 A3                    ..
	ldy     #$0B                            ; 6A0F A0 0B                    ..
	ldxa	L6983
	jsr     sub_619A
	shladdm8 off_AE, L46E2, L6982
	lda     #$00                            ; 6A2E A9 00                    ..
	ldy     #$01                            ; 6A30 A0 01                    ..
	sta     ($AE),y                         ; 6A32 91 AE                    ..
	lda     #$00                            ; 6A34 A9 00                    ..
	dey                                     ; 6A36 88                       .
	sta     ($AE),y                         ; 6A37 91 AE                    ..
	lda     L6991                           ; 6A39 AD 91 69                 ..i
	ora     L6991+1
	lbeq	L6A67
	rdldi	$84, $0006
	lda     L6990                           ; 6A4C AD 90 69                 ..i
	ldx     #$00                            ; 6A4F A2 00                    ..
	jsr     sub_444A
	sta     L6987                           ; 6A54 8D 87 69                 ..i
	lda     #$00                            ; 6A57 A9 00                    ..
	sta     $A3                             ; 6A59 85 A3                    ..
	ldy     L6987                           ; 6A5B AC 87 69                 ..i
	ldxa	L6991
	jsr     sub_619A
L6A67:  lda     L6993                           ; 6A67 AD 93 69                 ..i
	ora     L6993+1
	lbeq	L6AB8
	dmv	off_AE, L6993
	ldp16	L6988
	dmv	off_AE, L6988
	clc                                     ; 6A93 18                       .
	lda     ($AE),y                         ; 6A94 B1 AE                    ..
	adc     #$01                            ; 6A96 69 01                    i.
	sta     $A2                             ; 6A98 85 A2                    ..
	lda     #$00                            ; 6A9A A9 00                    ..
	sta     $A3                             ; 6A9C 85 A3                    ..
	ldy     $A2                             ; 6A9E A4 A2                    ..
	ldxa	L6988
	jsr     sub_619A
	lda	#$00
	sta     $A3                             ; 6AAB 85 A3                    ..
	ldy     #$1A                            ; 6AAD A0 1A                    ..
	ldxa	L6993
	jsr     sub_619A
L6AB8:	yldi	L4656, $01
	rts                                     ; 6ABD 60                       `

; ----------------------------------------------------------------------------
L6ABE:	.byte	$90
L6ABF:  .byte	$EA
L6AC0:	.byte	$98
L6AC1:	.byte	$48
L6AC2:	.byte	$4C
L6AC3:	.byte	$B3
L6AC4:	.byte	$EA,$4C
L6AC6:	.byte   $94,$0A
L6AC8:	.byte	$4C
L6AC9:	.byte	$BB
L6ACA:	.byte	$0A
L6ACB:	.byte	$4C
L6ACC:	.byte	$C1
L6ACD:	.byte	$0A
L6ACE:	.byte	$4C,$C7
L6AD0:  .byte	$0A
L6AD1:	.byte	$9B
L6AD2:	.byte	$44
L6AD3:	.byte	$31
L6AD4:	.byte	$3A

; ----------------------------------------------------------------------------
cmd_uc:						; "C","BBBBS"
	stack_prolog L6ABE, $05
	func16_8 sub_65B0, L6AC4, L6ABE
	test16	L6AC4
	lbeq	L6AFF
	lda     L6ABE                           ; 6AF9 AD BE 6A                 ..j
	jsr     cmd_uk
L6AFF:  ldx     #$00                            ; 6AFF A2 00                    ..
	lda     #$0B                            ; 6B01 A9 0B                    ..
	jsr     sub_606E
	rdmv	L6AC4, $A0
	shladdm8 off_AE, L46E2, L6ABE
	lda     L6AC4+1
	ldy     #$01                            ; 6B27 A0 01                    ..
	sta     ($AE),y                         ; 6B29 91 AE                    ..
	lda     L6AC4                           ; 6B2B AD C4 6A                 ..j
	dey                                     ; 6B2E 88                       .
	sta     ($AE),y                         ; 6B2F 91 AE                    ..
	lda     #$00                            ; 6B31 A9 00                    ..
	sta     $A3                             ; 6B33 85 A3                    ..
	ldy     #$0B                            ; 6B35 A0 0B                    ..
	ldxa	L6AC4
	jsr     sub_45F6
	dmv	off_AE, L6AC4
	lda	L6ABF
	ldy     #$00                            ; 6B4D A0 00                    ..
	sta     ($AE),y                         ; 6B4F 91 AE                    ..
	add16i	off_AE, L6AC4, $0001
	lda     L6AC0                           ; 6B60 AD C0 6A                 ..j
	sta     ($AE),y                         ; 6B63 91 AE                    ..
	ldi	$85, $00
	mv	$84, L6AC0
	lda     L6ABF                           ; 6B6E AD BF 6A                 ..j
	ldx     #$00                            ; 6B71 A2 00                    ..
	jsr     sub_444A
	st2xa	L6ACE
	add16i	off_AE, L6AC4, $0002
	stp16	L6ACE
	add16i	off_AE, L6AC4, $0004
	push16	off_AE
	lda     L6AC0                           ; 6BAE AD C0 6A                 ..j
	asl     a                               ; 6BB1 0A                       .
	sta     $A0                             ; 6BB2 85 A0                    ..
	lda     #$00                            ; 6BB4 A9 00                    ..
	sta     $A1                             ; 6BB6 85 A1                    ..
	ldxa	$A0
	jsr     sub_606E
	pull16	off_AE
	stp16	$A0
	ldxa	L6ACE
	jsr     sub_606E
	rdmv	L6AC6, $A0
	add16i	off_AE, L6AC4, $0004
	ldp16	L6AD1
	rdmv	L6ACA, L6AC6
	sty     L6AD0                           ; 6C0B 8C D0 6A                 ..j
	sub8i	L6C22, L6AC0, $01
L6C17:  lda     L6C22                           ; 6C17 AD 22 6C                 ."l
	cmp     L6AD0                           ; 6C1A CD D0 6A                 ..j
	bcs     L6C23                           ; 6C1D B0 04                    ..
	jmp     L6C5C                           ; 6C1F 4C 5C 6C                 L\l

; ----------------------------------------------------------------------------
L6C22:  .byte	$8A

; ----------------------------------------------------------------------------
L6C23:	shladdm8 off_AE, L6AD1, L6AD0
	stp16	L6ACA
	add16m8	L6ACA, L6ACA, L6ABF
	inc     L6AD0                           ; 6C56 EE D0 6A                 ..j
	jmp     L6C17                           ; 6C59 4C 17 6C                 L.l

; ----------------------------------------------------------------------------
L6C5C:	mv	$A3, L6ACE+1
	ldi	$A4, $00
	ldy     L6ACE                           ; 6C65 AC CE 6A                 ..j
	ldxa	L6AC6
	jsr     sub_45FC
	add16i	off_AE, L6AC4, $0006
	lda     L6AC1                           ; 6C80 AD C1 6A                 ..j
	ldy     #$00                            ; 6C83 A0 00                    ..
	sta     ($AE),y                         ; 6C85 91 AE                    ..
	add16i	off_AE, L6AC4, $0007
	lda     #$00                            ; 6C96 A9 00                    ..
	iny                                     ; 6C98 C8                       .
	sta     ($AE),y                         ; 6C99 91 AE                    ..
	lda     #$00                            ; 6C9B A9 00                    ..
	dey                                     ; 6C9D 88                       .
	sta     ($AE),y                         ; 6C9E 91 AE                    ..
	lda     #$00                            ; 6CA0 A9 00                    ..
	cmp     L6AC1                           ; 6CA2 CD C1 6A                 ..j
	lbcs	L6D5B
	rdldi	$84, $0006
	lda     L6AC1                           ; 6CB2 AD C1 6A                 ..j
	ldx     #$00                            ; 6CB5 A2 00                    ..
	jsr     sub_444A
	st2xa	L6ACC
	ldxa	L6ACC
	jsr     sub_606E
	rdmv	L6AD3, $A0
	add16i	off_AE, L6AC4, $0007
	stp16	L6AD3
	mv	$A3, L6ACD
	ldy     L6ACC                           ; 6CF5 AC CC 6A                 ..j
	ldxa	L6AD3
	jsr     sub_45F6
	ldy     #$00                            ; 6D01 A0 00                    ..
	sty     L6AD0                           ; 6D03 8C D0 6A                 ..j
	sub8i	L6D1A, L6AC1, $01
L6D0F:  lda     L6D1A                           ; 6D0F AD 1A 6D                 ..m
	cmp     L6AD0                           ; 6D12 CD D0 6A                 ..j
	bcs     L6D1B                           ; 6D15 B0 04                    ..
	jmp     L6D5B                           ; 6D17 4C 5B 6D                 L[m

; ----------------------------------------------------------------------------
L6D1A:	.byte	$4C                             ; 6D1A 4C                       L

; ----------------------------------------------------------------------------
L6D1B:	add16i	off_AE, L6AD3, $0004
	lda     #$FF                            ; 6D2A A9 FF                    ..
	ldy     #$00                            ; 6D2C A0 00                    ..
	sta     ($AE),y                         ; 6D2E 91 AE                    ..
	add16i	off_AE, L6AD3, $0001
	lda     L6AD0                           ; 6D3F AD D0 6A                 ..j
	sta     ($AE),y                         ; 6D42 91 AE                    ..
	add16i	L6AD3, L6AD3, $0006
	inc     L6AD0                           ; 6D55 EE D0 6A                 ..j
	jmp     L6D0F                           ; 6D58 4C 0F 6D                 L.m

; ----------------------------------------------------------------------------
L6D5B:	add16i	off_AE, L6AC4, $0009
	lda     #$00                            ; 6D6A A9 00                    ..
	ldy     #$01                            ; 6D6C A0 01                    ..
	sta     ($AE),y                         ; 6D6E 91 AE                    ..
	lda     #$00                            ; 6D70 A9 00                    ..
	dey                                     ; 6D72 88                       .
	sta     ($AE),y                         ; 6D73 91 AE                    ..
	dmv	off_AE, L6AC2
	lda     #$00                            ; 6D7F A9 00                    ..
	cmp     ($AE),y                         ; 6D81 D1 AE                    ..
	lbcs	L6E3B
L6D88:  ldx     #$00                            ; 6D88 A2 00                    ..
	lda     #$1A                            ; 6D8A A9 1A                    ..
	jsr     sub_606E
	rdmv	L6AC8, $A0
	add16i	off_AE, L6AC4, $0009
	stp16	L6AC8
	lda     #$00                            ; 6DB5 A9 00                    ..
	sta     $A3                             ; 6DB7 85 A3                    ..
	ldy     #$1A                            ; 6DB9 A0 1A                    ..
	ldxa	L6AC8
	jsr     sub_45F6
	dmv	off_AE, L6AC8
	push16	off_AE
	dmv	off_AE, L6AC2
	clc                                     ; 6DDE 18                       .
	ldy     #$00                            ; 6DDF A0 00                    ..
	lda     ($AE),y                         ; 6DE1 B1 AE                    ..
	adc     #$01                            ; 6DE3 69 01                    i.
	sta     $A0                             ; 6DE5 85 A0                    ..
	ldi	$A1, $00
	ldxa	$A0
	jsr     sub_606E                        ; 6DEF 20 6E 60                  n`
	pull16	off_AE
	lda     $A1                             ; 6DF8 A5 A1                    ..
	ldy     #$01                            ; 6DFA A0 01                    ..
	sta     ($AE),y                         ; 6DFC 91 AE                    ..
	lda     $A0                             ; 6DFE A5 A0                    ..
	dey                                     ; 6E00 88                       .
	sta     ($AE),y                         ; 6E01 91 AE                    ..
	dmv	off_AE, L6AC8
	iny                                     ; 6E0D C8                       .
	lda     ($AE),y                         ; 6E0E B1 AE                    ..
	sta     $A1                             ; 6E10 85 A1                    ..
	dey                                     ; 6E12 88                       .
	lda     ($AE),y                         ; 6E13 B1 AE                    ..
	sta     $A0                             ; 6E15 85 A0                    ..
	mv	$A3, L6AC2+1
	dmv	off_AC, L6AC2
	clc                                     ; 6E26 18                       .
	lda     ($AC),y                         ; 6E27 B1 AC                    ..
	adc     #$01                            ; 6E29 69 01                    i.
	sta     $A4                             ; 6E2B 85 A4                    ..
	ldi	$A5, $00
	ldy     L6AC2                           ; 6E31 AC C2 6A                 ..j
	ldxa	$A0
	jsr     blockmove
L6E3B:  ldi	$A0, $01
	rts                                     ; 6E3F 60                       `

; ----------------------------------------------------------------------------
L6E40:	.byte	$78
L6E41:	.byte	$6C                             ; 6E41 6C                       l
L6E42:	.byte	$66                             ; 6E42 66                       f
L6E43:	.byte	$66                             ; 6E43 66                       f
L6E44:	.byte	$6C                             ; 6E44 6C                       l
L6E45:  .byte	$78
L6E46:  .byte	$00
L6E47:  .byte	$00
L6E48:	.byte	$7E                             ; 6E48 7E                       ~
L6E49:  .byte	$60
L6E4A:	.byte	$7C                             ; 6E4A 7C                       |
L6E4B:  .byte	$60
L6E4C:  .byte	$60
L6E4D:  .addr	$7E
L6E4F:	.byte   $03,"CBS"
L6E53:	.addr	L6E4F
L6E55:	.byte	$03,"CBs"
L6E59:	.addr	L6E55
L6E5B:  ror     $3E66                           ; 6E5B 6E 66 3E                 nf>
	.byte	$00
L6E5F:  .byte	$00
L6E60:	.byte	$66                             ; 6E60 66                       f

; ----------------------------------------------------------------------------
cmd_ub:  					; "B"
	stack_prolog L6E40, $05
	func16_8 sub_65B0, L6E46, L6E40
	mv	$A3, L6E47
	rdldi	$A4, $0006
	ldy     L6E46                           ; 6E87 AC 46 6E                 .Fn
	ldxai	$6E5B
	jsr     blockmove
	sub8m	off_AE, L6E44, L6E42
	add8i	L6E49, off_AE, $01
	mv	L6E4A, L6E49
	shladdm8 off_AE, L6E5F, L6E43
	clc                                     ; 6EBC 18                       .
	ldy     #$00                            ; 6EBD A0 00                    ..
	lda     ($AE),y                         ; 6EBF B1 AE                    ..
	adc     L6E42                           ; 6EC1 6D 42 6E                 mBn
	sta     L6E4B                           ; 6EC4 8D 4B 6E                 .Kn
	iny                                     ; 6EC7 C8                       .
	lda     ($AE),y                         ; 6EC8 B1 AE                    ..
	adc     #$00                            ; 6ECA 69 00                    i.
	sta     L6E4C                           ; 6ECC 8D 4C 6E                 .Ln
	rdmv	L6E4D, L6E53
	lda     L6E41                           ; 6EDB AD 41 6E                 .An
	lbne	L6EEF
	rdmv	L6E4D, L6E59
L6EEF:	mv	L6E48, L6E43
	mv	L6F06, L6E45
L6EFB:	lda     L6F06                           ; 6EFB AD 06 6F                 ..o
	cmp     L6E48                           ; 6EFE CD 48 6E                 .Hn
	bcs     L6F07                           ; 6F01 B0 04                    ..
	jmp     L6F65                           ; 6F03 4C 65 6F                 Leo

; ----------------------------------------------------------------------------
L6F06:  .byte	$00				; 6F06 00                       .

; ----------------------------------------------------------------------------
L6F07:  lda     L6E41                           ; 6F07 AD 41 6E                 .An
	eor     #$01                            ; 6F0A 49 01                    I.
	lbne	L6F22
	ldy     L6E49                           ; 6F11 AC 49 6E                 .In
	ldxa	L6E4B
	jsr     sub_4B97
	mv	L6E4A, $A0
L6F22:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L6E48
	ldi	$A7, $00
	mv	$A6, L6E4A
	rdmv	$A8, L6E4B
	ldy     #$55                            ; 6F42 A0 55                    .U
	ldxa	L6E4D
	jsr     sub_55A0
	add16m8	L6E4B, L6E4B, L6E5B
	inc     L6E48                           ; 6F5F EE 48 6E                 .Hn
	jmp	L6EFB
L6F65:  rts                                     ; 6F65 60                       `

; ----------------------------------------------------------------------------
L6F66:	.byte	$FF
L6F67:  .byte	$00

; ----------------------------------------------------------------------------
cmd_lx:	
	prolog
	stxa	L6F66
	lda     L6F66                           ; 6F71 AD 66 6F                 .fo
	eor     #$01                            ; 6F74 49 01                    I.
	lbne	L6F84
	mv	L4647, L6F67
	jmp     L6F8A                           ; 6F81 4C 8A 6F                 L.o

; ----------------------------------------------------------------------------
L6F84:	mv	L4648, L6F67
L6F8A:  rts                                     ; 6F8A 60                       `

; ----------------------------------------------------------------------------
L6F8B:  .byte	$00
L6F8C:  .byte	$00
L6F8D:  .byte	$00
L6F8E:  .byte	$00
L6F8F:  .byte	$00
L6F90:  .byte	$00

; ----------------------------------------------------------------------------
cmd_ly:	
	prolog
	stxa	L6F8B
	lda     L4647                           ; 6F9A AD 47 46                 .GF
	eor     #$FF                            ; 6F9D 49 FF                    I.
	lbne	L6FA5
	rts                                     ; 6FA4 60                       `

; ----------------------------------------------------------------------------
L6FA5:	func16_8 sub_65B0, L6F8D, L4647
	mv	$A3, L6F8D+1
	rdldi	$A4, $0002
	ldy     L6F8D                           ; 6FC2 AC 8D 6F                 ..o
	ldxai	L6F8F
	jsr     blockmove
	mv	$A3, L6F8F
	rdmv	$A4, L6F8B
L6FDB:  ldy     L6F90                           ; 6FDB AC 90 6F                 ..o
	ldx     #$00                            ; 6FDE A2 00                    ..
	lda     L4647                           ; 6FE0 AD 47 46                 .GF
	jsr     cmd_ud
	rts                                     ; 6FE6 60                       `

; ----------------------------------------------------------------------------
L6FE7:	.byte   $F0,$F0
L6FE9:	.byte   $F0
L6FEA:	.byte	$F0

; ----------------------------------------------------------------------------
cmd_lz:	
	prolog
	stxa	L6FE7
	lda     L4648                           ; 6FF4 AD 48 46                 .HF
	eor     #$FF                            ; 6FF7 49 FF                    I.
	lbne	L6FFF
L6FFE:  rts                                     ; 6FFE 60                       `

; ----------------------------------------------------------------------------
L6FFF:  lda     L4648                           ; 6FFF AD 48 46                 .HF
	jsr     sub_65B0
	rdmv	L6FE9, $A0
	dmv	off_AE, L6FE9
	ldy     #$00                            ; 7019 A0 00                    ..
	lda     ($AE),y                         ; 701B B1 AE                    ..
	sta     $A3                             ; 701D 85 A3                    ..
	rdmv	$A4, L6FE7
	ldy     #$00                            ; 7029 A0 00                    ..
	ldx     #$00                            ; 702B A2 00                    ..
	lda     L4648
	jsr     cmd_ud
	rts                                     ; 7033 60                       `

; ----------------------------------------------------------------------------
L7034:	.byte   $66

; ----------------------------------------------------------------------------
sub_7035:  
	prolog
	sta     L7034                           ; 7038 8D 34 70                 .4p
	shladdm8 off_AE, L46A2, L7034
	ldp16	$A0
	rts                                     ; 705A 60                       `

; ----------------------------------------------------------------------------
L705B:	.byte	$66,$3E
L705D:  .word   $7C06

cmd_la:	
	prolog
	stxa	L705B
	func16_8 sub_7035, L705D, L705B
	add16i	off_AE, L705D, $0009
	lda     L705B+1
	ldy     #$00                            ; 708A A0 00                    ..
	sta     ($AE),y                         ; 708C 91 AE                    ..
	rts                                     ; 708E 60                       `

; ----------------------------------------------------------------------------
L708F:	.byte	$00
L7090:	.byte	$00
L7091:	.byte	$7C
L7092:	.byte	$66
L7093:  .byte	$66,$66
	.byte	$66

cmd_lb:
	stack_prolog L708F, $02
	func16_8 sub_7035, L7092, L708F
	add16i	$A0, L7092, $001E
	blkmv_mmi $A0, L7090, $0004
	rts

; ----------------------------------------------------------------------------
L70D6:	.byte	$00
L70D7:	.byte	$00
L70D8:	.byte	$00
L70D9:	.byte	$63,$6B
L70DB:	.byte	$7F
L70DC:	.byte	$3E
L70DD:	.byte	$36
L70DE:  .byte	$00
L70DF:  .byte	$00
L70E0:  .byte	$00
L70E1:	.byte	$66

; ----------------------------------------------------------------------------
sub_70E2:
	stack_prolog L70D6, $02
	func16_8 sub_7035, L70D9, L70D6
	add16i	L70DD, L70D9, $0022
	mv	$A3, L70D7+1
	rdldi	$A4, $0004
	ldy     L70D7                           ; 7119 AC D7 70                 ..p
	ldxa	L70DD
	jsr     blockmove
	add16i	off_AE, L70D9, $0004
	ldy     #$00                            ; 7134 A0 00                    ..
	lda     ($AE),y                         ; 7136 B1 AE                    ..
	sta     L70DF                           ; 7138 8D DF 70                 ..p
	lda     L70DF                           ; 713B AD DF 70                 ..p
	jsr     sub_65B0
	rdmv	L70DB, $A0
	lda     L70DC                           ; 714B AD DC 70                 ..p
	sta     $A3                             ; 714E 85 A3                    ..
	lda     #$00                            ; 7150 A9 00                    ..
	sta     $A5                             ; 7152 85 A5                    ..
	lda     #$02                            ; 7154 A9 02                    ..
	sta     $A4                             ; 7156 85 A4                    ..
	ldy     L70DB                           ; 7158 AC DB 70                 ..p
	ldxai	L70E0
	jsr     blockmove
	add16i	off_AE, L70DD, $0002
	clc                                     ; 7171 18                       .
	ldy     #$00                            ; 7172 A0 00                    ..
	lda     ($AE),y                         ; 7174 B1 AE                    ..
	adc     L70E0                           ; 7176 6D E0 70                 m.p
	sta     $AC                             ; 7179 85 AC                    ..
	sec                                     ; 717B 38                       8
	lda     $AC                             ; 717C A5 AC                    ..
	sbc     #$01                            ; 717E E9 01                    ..
	sta     ($AE),y                         ; 7180 91 AE                    ..
	add16i	off_AE, L70DD, $0003
	clc                                     ; 7191 18                       .
	lda     ($AE),y                         ; 7192 B1 AE                    ..
	adc     L70E1                           ; 7194 6D E1 70                 m.p
	sta     $AC                             ; 7197 85 AC                    ..
	sec                                     ; 7199 38                       8
	lda     $AC                             ; 719A A5 AC                    ..
	sbc     #$01                            ; 719C E9 01                    ..
	sta     ($AE),y                         ; 719E 91 AE                    ..
	rts                                     ; 71A0 60                       `

; ----------------------------------------------------------------------------
L71A1:	.byte	$00
L71A2:  .byte	$00
L71A3:  .byte	$00
L71A4:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L71A8:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L71AC:  .byte	$00
L71AD:  .byte	$00
L71AE:	.byte	$00
	.byte	$00
L71B0:  .byte	$00
L71B1:  .byte	$00
L71B2:  .byte	$00
L71B3:  .byte	$00
L71B4:  .byte	$00

; ----------------------------------------------------------------------------
sub_71B5:  
	prolog
	sta     L71A1                           ; 71B8 8D A1 71                 ..q
	func16_8 sub_7035, L71A2, L71A1
	lda     L71A3                           ; 71CB AD A3 71                 ..q
	sta     $A3                             ; 71CE 85 A3                    ..
	lda     #$00                            ; 71D0 A9 00                    ..
	sta     $A5                             ; 71D2 85 A5                    ..
	lda     #$07                            ; 71D4 A9 07                    ..
	sta     $A4                             ; 71D6 85 A4                    ..
	ldy     L71A2                           ; 71D8 AC A2 71                 ..q
	ldxai	L71AE
	jsr     blockmove
	func16_8 sub_65B0, L71AC, L71B2
	sec                                     ; 71F2 38                       8
	lda     #$00                            ; 71F3 A9 00                    ..
	sbc     L71B0                           ; 71F5 ED B0 71                 ..q
	sta     $A2                             ; 71F8 85 A2                    ..
	sec                                     ; 71FA 38                       8
	lda     #$00                            ; 71FB A9 00                    ..
	sbc     L71B1                           ; 71FD ED B1 71                 ..q
	sta     $A3                             ; 7200 85 A3                    ..
	dmv	$AA, L71AC
	sec                                     ; 720C 38                       8
	ldy     #$00                            ; 720D A0 00                    ..
	lda     ($AA),y                         ; 720F B1 AA                    ..
	sbc     #$01                            ; 7211 E9 01                    ..
	sta     $A8                             ; 7213 85 A8                    ..
	sec                                     ; 7215 38                       8
	lda     $A8                             ; 7216 A5 A8                    ..
	sbc     L71B0                           ; 7218 ED B0 71                 ..q
	sta     $A4                             ; 721B 85 A4                    ..
	add16i	$A8, L71AC, $0001
	sec                                     ; 722C 38                       8
	lda     ($A8),y                         ; 722D B1 A8                    ..
	sbc     #$01                            ; 722F E9 01                    ..
	sta     $A6                             ; 7231 85 A6                    ..
	sub8m	$A5, $A6, L71B1
	ldy     $A2                             ; 723B A4 A2                    ..
	ldxai	L71A4
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, L71B3, $01
	sub8i	$A5, L71B4, $01
	ldy     #$00                            ; 7258 A0 00                    ..
	ldxai	L71A8
	jsr     sub_4BF2
	add16i	off_AE, L71A2, $000B
	push16	off_AE
	ldi	$A3, $71
	add16i	$A4, L71A2, $0016
	ldy     #$A8                            ; 7289 A0 A8                    ..
	ldxai	L71A4
	jsr     sub_4CF5
	pull16	off_AE
	lda     $A0                             ; 7298 A5 A0                    ..
	ldy     #$00                            ; 729A A0 00                    ..
	sta     ($AE),y                         ; 729C 91 AE                    ..
	rts                                     ; 729E 60                       `

; ----------------------------------------------------------------------------
L729F:	.byte	$10                             ; 729F 10                       .
L72A0:	.byte	$D0                             ; 72A0 D0                       .
L72A1:	.byte	$E2                             ; 72A1 E2                       .
L72A2:	lda     $43                             ; 72A2 A5 43                    .C
	.byte	$8D,$E7
L72A6:	.byte	$02
	lda     $44                             ; 72A7 A5 44                    .D
	.byte   $8D                             ; 72A9 8D                       .
L72AA:  inx                                     ; 72AA E8                       .
L72AB:	.byte	$02                             ; 72AB 02                       .
	;jmp     LE59E                           ; 72AC 4C 9E E5                 L..
	.byte	$4C,$9E,$E5

; ----------------------------------------------------------------------------
L72AF:  clc                                     ; 72AF 18                       .
L72B0:	.byte	$A5                             ; 72B0 A5                       .

; ----------------------------------------------------------------------------
sub_72B1:  
	prolog
	sta     L729F                           ; 72B4 8D 9F 72                 ..r
	func16_8 sub_7035, L72A0, L729F
	mv	$A3, L72A0+1
	rdldi	$A4, $0007
	ldy     L72A0                           ; 72D4 AC A0 72                 ..r
	ldxai	L72AA
	jsr     blockmove
	sec                                     ; 72DE 38                       8
	lda     #$00                            ; 72DF A9 00                    ..
	sbc     L72AA                           ; 72E1 ED AA 72                 ..r
	sta     $A2                             ; 72E4 85 A2                    ..
	sec                                     ; 72E6 38                       8
	lda     #$00                            ; 72E7 A9 00                    ..
	sbc     L72AB                           ; 72E9 ED AB 72                 ..r
	sta     $A3                             ; 72EC 85 A3                    ..
	sec                                     ; 72EE 38                       8
	lda     #$27                            ; 72EF A9 27                    .'
	sbc     L72AA                           ; 72F1 ED AA 72                 ..r
	sta     $A4                             ; 72F4 85 A4                    ..
	sec                                     ; 72F6 38                       8
	lda     #$17                            ; 72F7 A9 17                    ..
	sbc     L72AB                           ; 72F9 ED AB 72                 ..r
	sta     $A5                             ; 72FC 85 A5                    ..
	ldy     $A2                             ; 72FE A4 A2                    ..
	ldxai	L72A2
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, L72AF, $01
	sub8i	$A5, L72B0, $01
	ldy     #$00                            ; 731B A0 00                    ..
	ldxai	L72A6
	jsr     sub_4BF2
	add16i	off_AE, L72A0, $000A
	push16	off_AE
	lda     #$72                            ; 7339 A9 72                    .r
	sta     $A3                             ; 733B 85 A3                    ..
	add16i	$A4, L72A0, $001A
	ldy     #$A6                            ; 734C A0 A6                    ..
	ldxai	$72A2
	jsr	sub_4CF5
	pull16	off_AE
	lda     $A0                             ; 735B A5 A0                    ..
	ldy     #$00                            ; 735D A0 00                    ..
	sta     ($AE),y                         ; 735F 91 AE                    ..
	rts                                     ; 7361 60                       `

; ----------------------------------------------------------------------------
L7362:	.byte	$2D                             ; 7362 2D                       -
L7363:	.byte	$F0                             ; 7363 F0                       .
L7364:	.byte	$20                             ; 7364 20                        
L7365:	.byte	$90                             ; 7365 90                       .
L7366:	.byte	$ED                             ; 7366 ED                       .
L7367:	.byte	$AD                             ; 7367 AD                       .

; ----------------------------------------------------------------------------
sub_7368:
	stack_prolog L7362, $02
	func16_8 sub_7035, L7365, L7364
	dmv	off_AE, L7365
	sec                                     ; 738B 38                       8
	lda     L7362                           ; 738C AD 62 73                 .bs
	ldy     #$00                            ; 738F A0 00                    ..
	sbc     ($AE),y                         ; 7391 F1 AE                    ..
	sta     L7362                           ; 7393 8D 62 73                 .bs
	add16i	off_AE, L7365, $0001
	sec                                     ; 73A5 38                       8
	lda     L7363                           ; 73A6 AD 63 73                 .cs
	sbc     ($AE),y                         ; 73A9 F1 AE                    ..
	sta     L7363                           ; 73AB 8D 63 73                 .cs
	add16i	$A2, L7365, $001A
	ldy     $A2                             ; 73BD A4 A2                    ..
	ldxa	L7362
	jsr     sub_4C75
	mv	L7367, $A0
	mv	$A0, L7367
	rts                                     ; 73D2 60                       `

; ----------------------------------------------------------------------------
L73D3:	.byte	$EF
L73D4:	.byte	$A9
L73D5:  .byte	$00
L73D6:	.byte	$9D
L73D7:	.byte	$9D
L73D8:	.byte	$F0
L73D9:	.byte	$AD

; ----------------------------------------------------------------------------
sub_73DA:
	prolog
	stxa	L73D3
	ldy     #$00                            ; 73E3 A0 00                    ..
	sty     L73D7                           ; 73E5 8C D7 73                 ..s
	iny                                     ; 73E8 C8                       .
	sty     L73D6                           ; 73E9 8C D6 73                 ..s
	mv	L73FD, L4673
L73F2:  lda     L73FD                           ; 73F2 AD FD 73                 ..s
	cmp     L73D6                           ; 73F5 CD D6 73                 ..s
	bcs     L73FE                           ; 73F8 B0 04                    ..
	jmp     L7467                           ; 73FA 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L73FD:  .byte	$08

; ----------------------------------------------------------------------------
L73FE:	sub8m	off_AE, L4673, L73D6
	ldx     $AE                             ; 7407 A6 AE                    ..
	lda     L4659,x                         ; 7409 BD 59 46                 .YF
	sta     L73D5                           ; 740C 8D D5 73                 ..s
	func16_8 sub_7035, L73D8, L73D5
	add16i	off_AE, L73D8, $000C
	ldy     #$00                            ; 742E A0 00                    ..
	lda     ($AE),y                         ; 7430 B1 AE                    ..
	cmp     #$FF                            ; 7432 C9 FF                    ..
	lbcs	L7461
	lda     L73D8                           ; 7439 AD D8 73                 ..s
	ora     L73D9                           ; 743C 0D D9 73                 ..s
	lbeq	L7461
L7444:  ldy     L73D5                           ; 7444 AC D5 73                 ..s
	ldxa	L73D3
	jsr     sub_7368
	lda     $A0                             ; 7450 A5 A0                    ..
	eor     #$01                            ; 7452 49 01                    I.
	lbne	L7461
	yldi	L73D7, $01
	jmp     L7467                           ; 745E 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L7461:  inc     L73D6                           ; 7461 EE D6 73                 ..s
	jmp     L73F2                           ; 7464 4C F2 73                 L.s

; ----------------------------------------------------------------------------
L7467:  lda     L73D7                           ; 7467 AD D7 73                 ..s
	eor     #$01                            ; 746A 49 01                    I.
	lbne	L7477
	mv	$A0, L73D5
	rts                                     ; 7476 60                       `

; ----------------------------------------------------------------------------
L7477:  ldi	$A0, $FF
	rts                                     ; 747B 60                       `

; ----------------------------------------------------------------------------
L747C:	.byte	$25                             ; 747C 25                       %

; ----------------------------------------------------------------------------
sub_747D:  
	prolog
	lda     #$14                            ; 7480 A9 14                    ..
	asl     a                               ; 7482 0A                       .
	sta     $A2                             ; 7483 85 A2                    ..
	lda     #$00                            ; 7485 A9 00                    ..
	sta     $A3                             ; 7487 85 A3                    ..
	ldy     $A2                             ; 7489 A4 A2                    ..
	ldxa	L46A2
	jsr     sub_45F6
	ldy     #$00                            ; 7494 A0 00                    ..
	sty     L747C                           ; 7496 8C 7C 74                 .|t
L7499:  lda     #$13                            ; 7499 A9 13                    ..
	cmp     L747C                           ; 749B CD 7C 74                 .|t
	lbcc	L74B2
	lda     L747C                           ; 74A3 AD 7C 74                 .|t
	ldx     L747C                           ; 74A6 AE 7C 74                 .|t
	sta     L4659,x                         ; 74A9 9D 59 46                 .YF
	inc     L747C                           ; 74AC EE 7C 74                 .|t
	jmp     L7499                           ; 74AF 4C 99 74                 L.t

; ----------------------------------------------------------------------------
L74B2:  ldy     #$00                            ; 74B2 A0 00                    ..
	sty     L4673                           ; 74B4 8C 73 46                 .sF
	rts                                     ; 74B7 60                       `

; ----------------------------------------------------------------------------
L74B8:	.byte	$32                             ; 74B8 32                       2
L74B9:	.byte	$F0                             ; 74B9 F0                       .
L74BA:	.byte	$91                             ; 74BA 91                       .
L74BB:	.byte	$47                             ; 74BB 47                       G
L74BC:  .byte	$18
L74BD:  .byte	$A5

cmd_lc:
	stack_prolog L74B8, $03
	lda     L74B8                           ; 74C7 AD B8 74                 ..t
	jsr     sub_7035
	rdmv	L74BC, $A0
	lda     L74BC                           ; 74D7 AD BC 74                 ..t
	ora     L74BD                           ; 74DA 0D BD 74                 ..t
	lbne	L753B
	ldxai	$0026
	jsr     sub_606E
	rdmv	L74BC, $A0
	shladdm8 off_AE, L46A2, L74B8
	lda     L74BD                           ; 7507 AD BD 74                 ..t
	ldy     #$01                            ; 750A A0 01                    ..
	sta     ($AE),y                         ; 750C 91 AE                    ..
	lda     L74BC                           ; 750E AD BC 74                 ..t
	dey                                     ; 7511 88                       .
	sta     ($AE),y                         ; 7512 91 AE                    ..
	ldi	$A3, $00
	ldy     #$26                            ; 7518 A0 26                    .&
	ldxa	L74BC
	jsr     sub_45F6
	inc     L4673                           ; 7523 EE 73 46                 .sF
	add16i	off_AE, L74BC, $000C
	lda     #$FF                            ; 7535 A9 FF                    ..
	ldy     #$00                            ; 7537 A0 00                    ..
	sta	(off_AE),y
L753B:	add16i	off_AE, L74BC, $000D
	lda     L74B9                           ; 754A AD B9 74                 ..t
	ldy     #$00                            ; 754D A0 00                    ..
	sta     ($AE),y                         ; 754F 91 AE                    ..
	add16i	$A0, L74BC, $000E
	add16i	$A2, L74BA, $0001
	rdldi	$A4, $0008
	ldy     $A2                             ; 7577 A4 A2                    ..
	ldxa	$A0
	jsr	blockmove
	yldi	L4656, $01
	rts                                     ; 7585 60                       `

; ----------------------------------------------------------------------------
L7586:	.byte	$A3                             ; 7586 A3                       .
L7587:	.byte	$F0                             ; 7587 F0                       .
L7588:	.byte	$BD                             ; 7588 BD                       .
L7589:	.byte	$A6                             ; 7589 A6                       .
L758A:	.byte	$F0                             ; 758A F0                       .
L758B:	.byte	$9D                             ; 758B 9D                       .

cmd_lm:  
	stack_prolog L7586, $03
	lda     L7586                           ; 7595 AD 86 75                 ..u
	jsr     sub_7035
	rdmv	L758A, $A0
	add16i	off_AE, L758A, $0004
	lda     L7587                           ; 75B4 AD 87 75                 ..u
	ldy     #$00                            ; 75B7 A0 00                    ..
	sta     ($AE),y                         ; 75B9 91 AE                    ..
	lda     L7589                           ; 75BB AD 89 75                 ..u
	eor     #$80                            ; 75BE 49 80                    I.
	lbeq	L75D9
	add16i	off_AE, L758A, $0003
	lda     L7589                           ; 75D4 AD 89 75                 ..u
	sta     ($AE),y                         ; 75D7 91 AE                    ..
L75D9:  lda     L7588                           ; 75D9 AD 88 75                 ..u
	eor     #$80                            ; 75DC 49 80                    I.
	lbeq	L75F9
	add16i	off_AE, L758A, $0002
	lda     L7588                           ; 75F2 AD 88 75                 ..u
	ldy     #$00                            ; 75F5 A0 00                    ..
	sta     ($AE),y                         ; 75F7 91 AE                    ..
L75F9:	proc8	sub_71B5, L7586
	yldi	L4656, $01
	rts                                     ; 7604 60                       `

; ----------------------------------------------------------------------------
L7605:  .byte	$60
L7606:  .byte	$E8
L7607:	.byte	$FA                             ; 7607 FA                       .
L7608:	.byte	$E9                             ; 7608 E9                       .
L7609:	.byte	$53                             ; 7609 53                       S

cmd_ll:
	stack_prolog L7605, $02
	lda     L7605                           ; 7613 AD 05 76                 ..v
	jsr     sub_7035
	rdmv	L7608, $A0
	lda     L7606                           ; 7623 AD 06 76                 ..v
	eor     #$80                            ; 7626 49 80                    I.
	lbeq	L763E
	dmv	off_AE, L7608
	lda     L7606                           ; 7637 AD 06 76                 ..v
	ldy     #$00                            ; 763A A0 00                    ..
	sta     (off_AE),y
L763E:  lda     L7607                           ; 763E AD 07 76                 ..v
	eor     #$80                            ; 7641 49 80                    I.
	lbeq	L765E
	add16i	off_AE, L7608, $0001
	lda     L7607                           ; 7657 AD 07 76                 ..v
	ldy     #$00                            ; 765A A0 00                    ..
	sta     ($AE),y                         ; 765C 91 AE                    ..
L765E:	proc8	sub_72B1, L7605
	yldi	L4656, $01
	rts                                     ; 7669 60                       `

; ----------------------------------------------------------------------------
L766A:	.byte	$EB                             ; 766A EB                       .
L766B:	.byte	$20                             ; 766B 20                        
L766C:	.byte	$53                             ; 766C 53                       S
L766D:	.byte	$EC                             ; 766D EC                       .
L766E:	.byte	$90                             ; 766E 90                       .
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
	lda     L7687                           ; 76BF AD 87 76                 ..v
	lbne	L76CC
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
L7799:	ldi	$A0, $00
	rts                                     ; 779D 60                       `

; ----------------------------------------------------------------------------
L779E:	.byte	$02
L779F:	.byte	$91
L77A0:	.byte	$45
L77A1:	.byt	$C8
L77A2:	.byte	$A9

; ----------------------------------------------------------------------------
cmd_ls:  
	stack_prolog L779E, $02
	lda     L779E                           ; 77AC AD 9E 77                 ..w
	jsr     sub_7035
	rdmv	L77A1, $A0
	add16i	off_AE, L77A1, $0005
	lda     L779F                           ; 77CB AD 9F 77                 ..w
	ldy     #$00                            ; 77CE A0 00                    ..
	sta     ($AE),y                         ; 77D0 91 AE                    ..
	add16i	off_AE, L77A1, $0006
	lda     L77A0                           ; 77E1 AD A0 77                 ..w
	sta     ($AE),y                         ; 77E4 91 AE                    ..
	add16i	off_AE, L77A1, $0007
	ldi	$85, $00
	mv	$84, L77A0
	lda     L779F                           ; 77FE AD 9F 77                 ..w
	ldx     #$00                            ; 7801 A2 00                    ..
	jsr     sub_444A
	sta     $AC                             ; 7806 85 AC                    ..
	txa                                     ; 7808 8A                       .
	ldy     #$01                            ; 7809 A0 01                    ..
	sta     ($AE),y                         ; 780B 91 AE                    ..
	lda     $AC                             ; 780D A5 AC                    ..
	dey                                     ; 780F 88                       .
	sta     ($AE),y                         ; 7810 91 AE                    ..
	proc8	sub_71B5, L779E
	proc8	sub_72B1, L779E
	yldi	L4656, $01
	rts                                     ; 7823 60                       `

; ----------------------------------------------------------------------------
L7824:	.byte	$20
L7825:	.byte	$43
L7826:	.byte	$EB
L7827:	.byte	$4C
L7828:	.byte	$17,$F0

; ----------------------------------------------------------------------------
cmd_lp:  
	prolog
	stxa	L7824
	func16_8 sub_7035, L7828, L7824
	test16	L7828
	lbne	L784F
	rts                                     ; 784E 60                       `

; ----------------------------------------------------------------------------
L784F:	add16i	off_AE, L7828, $000C
	lda     L7825                           ; 785E AD 25 78                 .%x
	ldy     #$00                            ; 7861 A0 00                    ..
	sta     ($AE),y                         ; 7863 91 AE                    ..
	sty     L7826                           ; 7865 8C 26 78                 .&x
L7868:  lda     #$13                            ; 7868 A9 13                    ..
	cmp     L7826                           ; 786A CD 26 78                 .&x
	lbcc	L7889
	ldx     L7826                           ; 7872 AE 26 78                 .&x
	lda     L4659,x                         ; 7875 BD 59 46                 .YF
	eor     L7824                           ; 7878 4D 24 78                 M$x
	lbne	L7883
	jmp     L7889                           ; 7880 4C 89 78                 L.x

; ----------------------------------------------------------------------------
L7883:  inc     L7826                           ; 7883 EE 26 78                 .&x
	jmp     L7868                           ; 7886 4C 68 78                 Lhx

; ----------------------------------------------------------------------------
L7889:  lda     L7826                           ; 7889 AD 26 78                 .&x
	cmp     #$14                            ; 788C C9 14                    ..
	lbcs	L78AC
	add8i	off_AE, L7826, $01
	ldx     $AE                             ; 789B A6 AE                    ..
	lda     L4659,x                         ; 789D BD 59 46                 .YF
	ldx     L7826                           ; 78A0 AE 26 78                 .&x
	sta     L4659,x                         ; 78A3 9D 59 46                 .YF
	inc     L7826                           ; 78A6 EE 26 78                 .&x
	jmp     L7889                           ; 78A9 4C 89 78                 L.x

; ----------------------------------------------------------------------------
L78AC:  ldy     #$00                            ; 78AC A0 00                    ..
	sty     L7826                           ; 78AE 8C 26 78                 .&x
L78B1:  lda     #$12                            ; 78B1 A9 12                    ..
	cmp     L7826                           ; 78B3 CD 26 78                 .&x
	lbcc	L78FE
	ldx     L7826                           ; 78BB AE 26 78                 .&x
	lda     L4659,x                         ; 78BE BD 59 46                 .YF
	sta     $A0                             ; 78C1 85 A0                    ..
	func16_8 sub_7035, L7828, $A0
	test16	L7828
	beq     L78F5                           ; 78D8 F0 1B                    ..
	add16i	off_AE, L7828, $000C
	ldy     #$00                            ; 78E9 A0 00                    ..
	lda     ($AE),y                         ; 78EB B1 AE                    ..
	cmp     L7825                           ; 78ED CD 25 78                 .%x
	lbcc	L78F8
L78F5:  jmp     L78FE                           ; 78F5 4C FE 78                 L.x

; ----------------------------------------------------------------------------
L78F8:  inc     L7826                           ; 78F8 EE 26 78                 .&x
	jmp     L78B1                           ; 78FB 4C B1 78                 L.x

; ----------------------------------------------------------------------------
L78FE:	yldi	L7827, $01
	sec                                     ; 7903 38                       8
	lda     #$13                            ; 7904 A9 13                    ..
	sbc     L7826                           ; 7906 ED 26 78                 .&x
	sta     L7917                           ; 7909 8D 17 79                 ..y
L790C:  lda     L7917                           ; 790C AD 17 79                 ..y
	cmp     L7827                           ; 790F CD 27 78                 .'x
	bcs     L7918                           ; 7912 B0 04                    ..
	jmp     L793F                           ; 7914 4C 3F 79                 L?y

; ----------------------------------------------------------------------------
L7917:	.byte	$2E

; ----------------------------------------------------------------------------
L7918:  sec                                     ; 7918 38                       8
	lda     #$14                            ; 7919 A9 14                    ..
	sbc     L7827                           ; 791B ED 27 78                 .'x
	sta     $AE                             ; 791E 85 AE                    ..
	sec                                     ; 7920 38                       8
	lda     #$14                            ; 7921 A9 14                    ..
	sbc     L7827                           ; 7923 ED 27 78                 .'x
	sta     $AC                             ; 7926 85 AC                    ..
	sub8i	$AA, $AC, $01
	ldx     $AA                             ; 792F A6 AA                    ..
	lda     L4659,x                         ; 7931 BD 59 46                 .YF
	ldx     $AE                             ; 7934 A6 AE                    ..
	sta     L4659,x                         ; 7936 9D 59 46                 .YF
	inc     L7827                           ; 7939 EE 27 78                 .'x
	jmp     L790C                           ; 793C 4C 0C 79                 L.y

; ----------------------------------------------------------------------------
L793F:  lda     L7824                           ; 793F AD 24 78                 .$x
	ldx     L7826                           ; 7942 AE 26 78                 .&x
	sta     L4659,x                         ; 7945 9D 59 46                 .YF
	yldi	L4656, $01
	rts                                     ; 794D 60                       `

; ----------------------------------------------------------------------------
L794E:	.byte	$0D
L794F:	.byte	$F0,$05

; ----------------------------------------------------------------------------
cmd_ld:
	prolog
	sta     L794E                           ; 7954 8D 4E 79                 .Ny
	func16_8 sub_7035, L794F, L794E
	test16	L794F
	lbne	L7973
L7972:  rts                                     ; 7972 60                       `

; ----------------------------------------------------------------------------
L7973:  ldx     #$FF                            ; 7973 A2 FF                    ..
	lda     L794E                           ; 7975 AD 4E 79                 .Ny
	jsr     cmd_lp
	ldi	$A3, $00
	ldy     #$26                            ; 797F A0 26                    .&
	ldxa	L794F
	jsr     sub_619A
	sub8i	L4673, L4673, $01
	rts                                     ; 7993 60                       `

; ----------------------------------------------------------------------------
L7994:  .byte	$08
L7995:	.byte	$F0                             ; 7995 F0                       .
L7996:	.byte	$1D,$30
L7998:	.byte	$1B,$29
L799A:	.byte	$01                             ; 799A 01                       .

sub_799B:  
	prolog
	stxa	L7994
	func16_8 sub_65B0, L7996, L7994
L79B4:  add16i	off_AE, L7996, $0007
	ldp16	L7998
	test16	L7998
	lbne	L79E4
	rdldi	$A0, $0000
	rts                                     ; 79E3 60                       `

; ----------------------------------------------------------------------------
L79E4:  ldy     #$00                            ; 79E4 A0 00                    ..
	sty     L799A                           ; 79E6 8C 9A 79                 ..y
	add16i	off_AE, L7996, $0006
	sec                                     ; 79F8 38                       8
	lda     ($AE),y                         ; 79F9 B1 AE                    ..
	sbc     #$01                            ; 79FB E9 01                    ..
	sta     L7A0B                           ; 79FD 8D 0B 7A                 ..z
L7A00:  lda     L7A0B                           ; 7A00 AD 0B 7A                 ..z
	cmp     L799A                           ; 7A03 CD 9A 79                 ..y
	bcs     L7A0C                           ; 7A06 B0 04                    ..
	jmp     L7A49                           ; 7A08 4C 49 7A                 LIz

; ----------------------------------------------------------------------------
L7A0B:	.byte	$03                             ; 7A0B 03                       .

; ----------------------------------------------------------------------------
L7A0C:	add16i	off_AE, L7998, $0001
	ldy     #$00                            ; 7A1B A0 00                    ..
	lda     ($AE),y                         ; 7A1D B1 AE                    ..
	eor     L7995                           ; 7A1F 4D 95 79                 M.y
	lbne	L7A32
	rdmv	$A0, L7998
	rts                                     ; 7A31 60                       `

; ----------------------------------------------------------------------------
L7A32:	add16i	L7998, L7998, $0006
	inc	L799A
	jmp	L7A00

; ----------------------------------------------------------------------------
L7A49:	rdldi	$A0, $00
	rts                                     ; 7A51 60                       `

; ----------------------------------------------------------------------------
L7A52:	.byte	$F0
L7A53:	.byte	$F0
L7A54:	.byte	$03
L7A55:	.byte	$4C
L7A56:	.byte	$B6

; ----------------------------------------------------------------------------
cmd_uu:
	stack_prolog L7A52, $02
	ldxa	L7A52
	jsr     sub_799B
	rdmv	L7A55, $A0
	test16	L7A55
	lbne	L7A7F
	rts                                     ; 7A7E 60                       `

; ----------------------------------------------------------------------------
L7A7F:	dmv	off_AE, L7A55
	lda     L7A54                           ; 7A89 AD 54 7A                 .Tz
	ldy     #$00                            ; 7A8C A0 00                    ..
	sta     (off_AE),y                      ; 7A8E 91 AE                    ..
	iny                                     ; 7A90 C8                       .
	sty     L4656                           ; 7A91 8C 56 46                 .VF
	rts                                     ; 7A94 60                       `

; ----------------------------------------------------------------------------
L7A95:  .byte	$68
L7A96:	.byte	$9D
L7A97:	.byte	$9E
L7A98:	.byte	$F0
L7A99:  .byte	$A9

; ----------------------------------------------------------------------------
cmd_un:
	stack_prolog L7A95, $02
	ldxa	L7A95
	jsr     sub_799B
	rdmv	L7A98, $A0
	test16	L7A98
	lbne	L7AC2
	rts                                     ; 7AC1 60                       `

; ----------------------------------------------------------------------------
L7AC2:	add16i	off_AE, L7A98, $0005
	stp8	L7A97
	rts                                     ; 7AD8 60                       `

; ----------------------------------------------------------------------------
L7AD9:  .byte	$00
L7ADA:	.byte	$91
L7ADB:	.byte	$45
L7ADC:  .byte	$38
L7ADD:  .byte	$08
L7ADE:	.byte	$A5

; ----------------------------------------------------------------------------
cmd_uv:  
	stack_prolog L7AD9, $03
	ldxa	L7AD9
	jsr     sub_799B
	rdmv	L7ADD, $A0
	test16	L7ADD
	lbne	L7B07
	rts                                     ; 7B06 60                       `

; ----------------------------------------------------------------------------
L7B07:  lda     L7ADB                           ; 7B07 AD DB 7A                 ..z
	eor     #$80                            ; 7B0A 49 80                    I.
	lbeq	L7B27
	add16i	off_AE, L7ADD, $02
	lda     L7ADB                           ; 7B20 AD DB 7A                 ..z
	ldy     #$00                            ; 7B23 A0 00                    ..
	sta     (off_AE),y
L7B27:  lda     L7ADC                           ; 7B27 AD DC 7A                 ..z
	eor     #$80                            ; 7B2A 49 80                    I.
	lbeq	L7B47
	add16i	off_AE, L7ADD, $03
	lda     L7ADC                           ; 7B40 AD DC 7A                 ..z
	ldy     #$00                            ; 7B43 A0 00                    ..
	sta     (off_AE),y                      ; 7B45 91 AE                    ..
L7B47:  ldy     #$01                            ; 7B47 A0 01                    ..
	sty     L4656                           ; 7B49 8C 56 46                 .VF
	rts                                     ; 7B4C 60                       `

; ----------------------------------------------------------------------------
L7B4D:	.byte	$54                             ; 7B4D 54                       T
L7B4E:	.byte	$B1                             ; 7B4E B1                       .
L7B4F:	.byte	$45                             ; 7B4F 45                       E
L7B50:	.byte	$F0                             ; 7B50 F0                       .
L7B51:	.byte	$F7                             ; 7B51 F7                       .
L7B52:	.byte	$8C,$33
L7B54:	.byte	$F0                             ; 7B54 F0                       .
L7B55:  .byte	$48
L7B56:	.byte	$38,$A0
	.byte	$03
	.byte	$B1,$45
	.byte   $E9                             ; 7B5B E9                       .
L7B5C:  .byte	$01
L7B5D:	.byte	$91
	eor     $C8                             ; 7B5E 45 C8                    E.
	lda     ($45),y                         ; 7B60 B1 45                    .E
	sbc     #$00                            ; 7B62 E9 00                    ..
L7B64:  sta     ($45),y                         ; 7B64 91 45                    .E
	iny                                     ; 7B66 C8                       .
L7B67:  .byte	$A9

cmd_ux:
	stack_prolog L7B4D, $02
	sec                                     ; 7B71 38                       8
	lda     L7B4F                           ; 7B72 AD 4F 7B                 .O{
	sbc     #$01                            ; 7B75 E9 01                    ..
	sta     L7B4F                           ; 7B77 8D 4F 7B                 .O{
	lda     L7B4D                           ; 7B7A AD 4D 7B                 .M{
	jsr     sub_65B0
	lda     $A1                             ; 7B80 A5 A1                    ..
	sta     L7B51                           ; 7B82 8D 51 7B                 .Q{
	lda     $A0                             ; 7B85 A5 A0                    ..
	sta     L7B50                           ; 7B87 8D 50 7B                 .P{
	add16i	off_AE, L7B50, $0007
	ldp16	L7B52
	lda     L7B52                           ; 7BA6 AD 52 7B                 .R{
	ora     L7B52+1
	lbne	L7BB2
	rts                                     ; 7BB1 60                       `

; ----------------------------------------------------------------------------
L7BB2:	rdmv	L7B56, L7B52
	ldy     #$00                            ; 7BBE A0 00                    ..
	sty     L7B55                           ; 7BC0 8C 55 7B                 .U{
	sty     L7B54                           ; 7BC3 8C 54 7B                 .T{
	sty     L7B5C                           ; 7BC6 8C 5C 7B                 .\{
	add16i	off_AE, L7B50, $0006
	sec                                     ; 7BD8 38                       8
	lda     (off_AE),y
	sbc     #$01                            ; 7BDB E9 01                    ..
	sta     L7BEB                           ; 7BDD 8D EB 7B                 ..{
L7BE0:  lda     L7BEB                           ; 7BE0 AD EB 7B                 ..{
	cmp     L7B5C                           ; 7BE3 CD 5C 7B                 .\{
	bcs     L7BEC                           ; 7BE6 B0 04                    ..
	jmp     L7C59                           ; 7BE8 4C 59 7C                 LY|

; ----------------------------------------------------------------------------
L7BEB:	.byte	$F6                             ; 7BEB F6                       .

; ----------------------------------------------------------------------------
L7BEC:  lda     L7B52+1
	sta     $A3                             ; 7BEF 85 A3                    ..
	lda     #$00                            ; 7BF1 A9 00                    ..
	sta     $A5                             ; 7BF3 85 A5                    ..
	lda     #$05                            ; 7BF5 A9 05                    ..
	sta     $A4                             ; 7BF7 85 A4                    ..
	ldy     L7B52                           ; 7BF9 AC 52 7B                 .R{
	ldxai	$7B63
	jsr     blockmove
	lda     L7B67                           ; 7C03 AD 67 7B                 .g{
	cmp     L7B4F                           ; 7C06 CD 4F 7B                 .O{
	lbcs	L7C1F
	add16i	L7B56, L7B52, $0006
L7C1F:  lda     L7B64                           ; 7C1F AD 64 7B                 .d{
	eor     L7B4E                           ; 7C22 4D 4E 7B                 MN{
	lbne	L7C42
L7C2A:  lda     L7B67                           ; 7C2A AD 67 7B                 .g{
	eor     L7B4F                           ; 7C2D 4D 4F 7B                 MO{
	lbne	L7C36
	rts                                     ; 7C35 60                       `

; ----------------------------------------------------------------------------
L7C36:	rdmv	L7B54, L7B52
L7C42:	add16i	L7B52, L7B52, $0006
	inc     L7B5C                           ; 7C53 EE 5C 7B                 .\{
	jmp     L7BE0                           ; 7C56 4C E0 7B                 L.{

; ----------------------------------------------------------------------------
L7C59:  lda     L7B54                           ; 7C59 AD 54 7B                 .T{
	ora     L7B55                           ; 7C5C 0D 55 7B                 .U{
	lbne	L7C65
	rts                                     ; 7C64 60                       `

; ----------------------------------------------------------------------------
L7C65:	mv	$A3, L7B54+1
	rdldi	$A4, $0006
	ldy     L7B54                           ; 7C72 AC 54 7B                 .T{
	ldxai	L7B5D
	jsr     blockmove
	add16i	$A2, L7B54, $0006
	sub16m	off_AC, L7B52, L7B54
	sub16i	$A4, off_AC, $0006
	ldy     $A2                             ; 7CA9 A4 A2                    ..
	ldxa	L7B54
	jsr     blockmove
	lda     L7B54                           ; 7CB4 AD 54 7B                 .T{
	cmp     L7B56                           ; 7CB7 CD 56 7B                 .V{
	lda     L7B55                           ; 7CBA AD 55 7B                 .U{
	sbc     L7B56+1
	lbcs	L7CD6
	sub16i	L7B56, L7B56, $0006
L7CD6:	add16i	$A0, L7B56, $0006
	mv	$A3, L7B56+1
	sub16m	off_AC, L7B52, L7B56
	sub16i	$A4, off_AC, $0006
	ldy     L7B56                           ; 7D08 AC 56 7B                 .V{
	ldxa	$A0
	jsr     sub_4EB1
	lda     #$7B                            ; 7D12 A9 7B                    .{
	sta     $A3                             ; 7D14 85 A3                    ..
	lda     #$00                            ; 7D16 A9 00                    ..
	sta     $A5                             ; 7D18 85 A5                    ..
	lda     #$06                            ; 7D1A A9 06                    ..
	sta     $A4                             ; 7D1C 85 A4                    ..
	ldy     #$5D                            ; 7D1E A0 5D                    .]
	ldxa	L7B56
	jsr     blockmove
	add16i	off_AE, L7B56, $0004
	lda     L7B4F                           ; 7D38 AD 4F 7B                 .O{
	ldy     #$00                            ; 7D3B A0 00                    ..
	sta     ($AE),y                         ; 7D3D 91 AE                    ..
	iny                                     ; 7D3F C8                       .
	sty     L4656                           ; 7D40 8C 56 46                 .VF
	rts                                     ; 7D43 60                       `

; ----------------------------------------------------------------------------
L7D44:	.byte	$00
L7D45:  .byte	$00
L7D46:  .byte	$00
L7D47:  .byte	$00
L7D48:  .byte	$00,$00
L7D4A:  .byte	$00
L7D4B:  .word	$0000

cmd_uy:						; "Y"
	stack_prolog L7D44, $03
	lda	L7D44
	jsr	sub_65B0
	rdmv	L7D48, $A0
	lda     L7D45                           ; 7D66 AD 45 7D                 .E}
	and     #$01                            ; 7D69 29 01                    ).
	sta     L7D4A                           ; 7D6B 8D 4A 7D                 .J}
	lsr     L7D45                           ; 7D6E 4E 45 7D                 NE}
	test16	L7D48
	lbne	L7D7D
	rts                                     ; 7D7C 60                       `

; ----------------------------------------------------------------------------
L7D7D:	add16i	off_AE, L7D48, $0009
	ldp16	L7D4B
	test16	L7D4B
	lbne	L7DA5
	rts                                     ; 7DA4 60                       `

; ----------------------------------------------------------------------------
L7DA5:	add16i	off_AE, L7D4B, $0002
	ldi	$85, $00
	ldi	$84, $03
	lda     L7D45                           ; 7DBC AD 45 7D                 .E}
	ldx     #$00                            ; 7DBF A2 00                    ..
	jsr     sub_444A
	st2xa	off_AC
	add16m	L7D4B, off_AE, off_AC
	dmv	off_AE, L7D4B
	lda     L7D4A                           ; 7DE2 AD 4A 7D                 .J}
	ldy     #$00                            ; 7DE5 A0 00                    ..
	sta     ($AE),y                         ; 7DE7 91 AE                    ..
	add16i	off_AE, L7D4B, $0001
	lda     L7D46                           ; 7DF8 AD 46 7D                 .F}
	sta     ($AE),y                         ; 7DFB 91 AE                    ..
	add16i	off_AE, L7D4B, $0002
	lda     L7D47                           ; 7E0C AD 47 7D                 .G}
	sta     ($AE),y                         ; 7E0F 91 AE                    ..
	rts                                     ; 7E11 60                       `

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
	jsr     sub_444A
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
	lda     ($AE),y                         ; 7EEC B1 AE                    ..
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
	lda     ($AE),y                         ; 7F48 B1 AE                    ..
	eor     L7E14                           ; 7F4A 4D 14 7E                 M.~
	beq     L7F52                           ; 7F4D F0 03                    ..
	jmp     L7F59                           ; 7F4F 4C 59 7F                 LY.

; ----------------------------------------------------------------------------
L7F52:  iny                                     ; 7F52 C8                       .
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
	lda     L7E19                           ; 7F6E AD 19 7E                 ..~
	sta     $A0                             ; 7F71 85 A0                    ..
	rts                                     ; 7F73 60                       `

; ----------------------------------------------------------------------------
L7F74:  .byte	$00
L7F75:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00

; ----------------------------------------------------------------------------
L7F80:	prolog
	stxa	L7F74
	ldi	$A0, $00
	rts                                     ; 7F8D 60                       `

; ----------------------------------------------------------------------------
L7F8E:  .byte	$00
L7F8F:  .byte	$00
L7F90:  .byte	$00
L7F91:  .byte	$00
L7F92:  .byte	$00

; ----------------------------------------------------------------------------
sub_7F93:
	prolog
	stxa	L7F8E
	ldy     #$00                            ; 7F9C A0 00                    ..
	sty     L7F90                           ; 7F9E 8C 90 7F                 ...
L7FA1:  lda     #$07                            ; 7FA1 A9 07                    ..
	cmp     L7F90                           ; 7FA3 CD 90 7F                 ...
	lbcc	L7FE2
	sec                                     ; 7FAB 38                       8
	lda     #$07                            ; 7FAC A9 07                    ..
	sbc     L7F90                           ; 7FAE ED 90 7F                 ...
	sta     L7F91                           ; 7FB1 8D 91 7F                 ...
	ldy     L7F8F                           ; 7FB4 AC 8F 7F                 ...
	ldx     L7F8E                           ; 7FB7 AE 8E 7F                 ...
	lda     L7F91                           ; 7FBA AD 91 7F                 ...
	jsr     sub_7E24
	lda     $A0                             ; 7FC0 A5 A0                    ..
	sta     L7F92                           ; 7FC2 8D 92 7F                 ...
	lda     L7F92                           ; 7FC5 AD 92 7F                 ...
	eor     #$01                            ; 7FC8 49 01                    I.
	lbne	L7FDC
	clc                                     ; 7FCF 18                       .
	lda     #$05                            ; 7FD0 A9 05                    ..
	adc     L7F91                           ; 7FD2 6D 91 7F                 m..
	sta     $A0                             ; 7FD5 85 A0                    ..
	lda     $A0                             ; 7FD7 A5 A0                    ..
	jsr     sub_4BA7
L7FDC:  inc     L7F90                           ; 7FDC EE 90 7F                 ...
	jmp     L7FA1                           ; 7FDF 4C A1 7F                 L..

; ----------------------------------------------------------------------------
L7FE2:  lda     L7F92                           ; 7FE2 AD 92 7F                 ...
	sta     $A0                             ; 7FE5 85 A0                    ..
	rts                                     ; 7FE7 60                       `

; ----------------------------------------------------------------------------
L7FE8:  .byte	$00

; ----------------------------------------------------------------------------
sub_7FE9:	
	prolog
	sta     L7FE8                           ; 7FEC 8D E8 7F                 ...
	lda     L7FE8                           ; 7FEF AD E8 7F                 ...
	lsr     a                               ; 7FF2 4A                       J
	lsr     a                               ; 7FF3 4A                       J
	lsr     a                               ; 7FF4 4A                       J
	lsr     a                               ; 7FF5 4A                       J
	sta     $A0                             ; 7FF6 85 A0                    ..
	ldx     L7FE8                           ; 7FF8 AE E8 7F                 ...
	lda     $A0                             ; 7FFB A5 A0                    ..
	jsr     sub_49A2
	rts                                     ; 8000 60                       `

; ----------------------------------------------------------------------------
L8001:  .word	$0000

; ----------------------------------------------------------------------------
sub_8003:  
	prolog
	stxa	L8001
	dmv	L474F, L8001
	ldxai	$0000
	jsr     sub_49D3
	rts                                     ; 801F 60                       `

; ----------------------------------------------------------------------------
sub_8020:  
	prolog
	lda     #$00                            ; 8023 A9 00                    ..
	sta     $A3                             ; 8025 85 A3                    ..
	ldy     #$04                            ; 8027 A0 04                    ..
	ldxai	L474F
	jsr     sub_45F6
	lda     #$00                            ; 8030 A9 00                    ..
	sta     $A3                             ; 8032 85 A3                    ..
	ldy     #$0E                            ; 8034 A0 0E                    ..
	ldx     #$47                            ; 8036 A2 47                    .G
	lda     #$53                            ; 8038 A9 53                    .S
	jsr     sub_45F6
	ldx     #$00                            ; 803D A2 00                    ..
	lda     #$00                            ; 803F A9 00                    ..
	jsr     sub_8003
	rts                                     ; 8044 60                       `

; ----------------------------------------------------------------------------
L8045:	.byte	$A9
L8046:  .byte	$48

; ----------------------------------------------------------------------------
sub_8047:  
	prolog
	stxa	L8045
	lda     #$27                            ; 8050 A9 27                    .'
	cmp     L8045                           ; 8052 CD 45 80                 .E.
	lbcs	L805B
	rts                                     ; 805A 60                       `

; ----------------------------------------------------------------------------
L805B:  lda     #$17                            ; 805B A9 17                    ..
	cmp     L8046                           ; 805D CD 46 80                 .F.
	lbcs	L8066
L8065:  rts                                     ; 8065 60                       `

; ----------------------------------------------------------------------------
L8066:	dmv	L4751, L8045
	lda     L8045                           ; 8072 AD 45 80                 .E.
	asl     a                               ; 8075 0A                       .
	asl     a                               ; 8076 0A                       .
	sta     $AE                             ; 8077 85 AE                    ..
	add8m	off_AC, off_AE, L4753
	add8i	L8045, off_AC, $30
	lda     L8046                           ; 8089 AD 46 80                 .F.
	asl     a                               ; 808C 0A                       .
	asl     a                               ; 808D 0A                       .
	sta     $AE                             ; 808E 85 AE                    ..
	add8m	off_AC, off_AE, L4754
	add8i	L8046, off_AC, $10
	ldxa	L8045
	jsr     sub_49D3
	rts                                     ; 80A9 60                       `

; ----------------------------------------------------------------------------
L80AA:	.byte	$4F                             ; 80AA 4F                       O
L80AB:	.byte	$4D                             ; 80AB 4D                       M
L80AC:	.byte	$9B                             ; 80AC 9B                       .
L80AD:	.byte	$A2                             ; 80AD A2                       .
L80AE:  .byte	$00
L80AF:	.byte	$B9                             ; 80AF B9                       .
L80B0:	.byte	$5B                             ; 80B0 5B                       [
L80B1:	.byte	$F3                             ; 80B1 F3                       .
L80B2:	.byte	$9D                             ; 80B2 9D                       .
L80B3:	.byte	$CD                             ; 80B3 CD                       .
L80B4:	.byte	$09                             ; 80B4 09                       .
L80B5:	.byte	$9D                             ; 80B5 9D                       .
L80B6:  clv                                     ; 80B6 B8                       .
	ora     #$C8                            ; 80B7 09 C8                    ..
L80B9:  inx                                     ; 80B9 E8                       .
L80BA:  .byte	$C9

sub_80BB:	
	prolog
	stxa	L80AA
	lda     L474F                           ; 80C4 AD 4F 47                 .OG
	eor     #$01                            ; 80C7 49 01                    I.
	beq     L80CE                           ; 80C9 F0 03                    ..
	jmp     L80DA                           ; 80CB 4C DA 80                 L..

; ----------------------------------------------------------------------------
L80CE:	ldxa	L80AA
	jsr     sub_8047
	jmp     L8176                           ; 80D7 4C 76 81                 Lv.

; ----------------------------------------------------------------------------
L80DA:  lda     L4750                           ; 80DA AD 50 47                 .PG
	jsr     sub_7035
	lda     $A1                             ; 80E0 A5 A1                    ..
	sta     L80AD                           ; 80E2 8D AD 80                 ...
	lda     $A0                             ; 80E5 A5 A0                    ..
	sta     L80AC                           ; 80E7 8D AC 80                 ...
	lda     L80AD                           ; 80EA AD AD 80                 ...
	sta     $A3                             ; 80ED 85 A3                    ..
	lda     #$00                            ; 80EF A9 00                    ..
	sta     $A5                             ; 80F1 85 A5                    ..
	lda     #$05                            ; 80F3 A9 05                    ..
	sta     $A4                             ; 80F5 85 A4                    ..
	ldy     L80AC                           ; 80F7 AC AC 80                 ...
	ldxai	$80B2
	jsr     blockmove
	clc                                     ; 8101 18                       .
	lda     L80AA                           ; 8102 AD AA 80                 ...
	adc     L80B2                           ; 8105 6D B2 80                 m..
	sta     L80B0                           ; 8108 8D B0 80                 ...
	clc                                     ; 810B 18                       .
	lda     L80AB                           ; 810C AD AB 80                 ...
	adc     L80B3                           ; 810F 6D B3 80                 m..
	sta     L80B1                           ; 8112 8D B1 80                 ...
	lda     L474F                           ; 8115 AD 4F 47                 .OG
	eor     #$03                            ; 8118 49 03                    I.
	beq     L811F                           ; 811A F0 03                    ..
	jmp     L816D                           ; 811C 4C 6D 81                 Lm.

; ----------------------------------------------------------------------------
L811F:  ldx     L46EA                           ; 811F AE EA 46                 ..F
	lda     L80B6                           ; 8122 AD B6 80                 ...
	jsr     sub_799B
	lda     $A1                             ; 8128 A5 A1                    ..
	sta     L80AF                           ; 812A 8D AF 80                 ...
	lda     $A0                             ; 812D A5 A0                    ..
	sta     L80AE                           ; 812F 8D AE 80                 ...
	lda     L80AF                           ; 8132 AD AF 80                 ...
	sta     $A3                             ; 8135 85 A3                    ..
	lda     #$00                            ; 8137 A9 00                    ..
	sta     $A5                             ; 8139 85 A5                    ..
	lda     #$04                            ; 813B A9 04                    ..
	sta     $A4                             ; 813D 85 A4                    ..
	ldy     L80AE                           ; 813F AC AE 80                 ...
	ldx     #$80                            ; 8142 A2 80                    ..
	lda     #$B7                            ; 8144 A9 B7                    ..
	jsr     blockmove
	clc                                     ; 8149 18                       .
	lda     L80B0                           ; 814A AD B0 80                 ...
	adc     L80B9                           ; 814D 6D B9 80                 m..
	sta     $AE                             ; 8150 85 AE                    ..
	sec                                     ; 8152 38                       8
	lda     $AE                             ; 8153 A5 AE                    ..
	sbc     L80B4                           ; 8155 ED B4 80                 ...
	sta     L80B0                           ; 8158 8D B0 80                 ...
	clc                                     ; 815B 18                       .
	lda     L80B1                           ; 815C AD B1 80                 ...
	adc     L80BA                           ; 815F 6D BA 80                 m..
	sta     $AE                             ; 8162 85 AE                    ..
	sec                                     ; 8164 38                       8
	lda     $AE                             ; 8165 A5 AE                    ..
	sbc     L80B5                           ; 8167 ED B5 80                 ...
	sta     L80B1                           ; 816A 8D B1 80                 ...
L816D:	ldxa	L80B0
	jsr     sub_8047
L8176:  rts                                     ; 8176 60                       `

; ----------------------------------------------------------------------------
L8177:	.byte   $F4                             ; 8177 F4                       .
	.byte	$48
L8179:  .byte	$60
L817A:	.byte	$AD                             ; 817A AD                       .
L817B:	.byte	$6F                             ; 817B 6F                       o

; ----------------------------------------------------------------------------
sub_817C:	
	stack_prolog L8177, $03
	lda     #$81                            ; 8185 A9 81                    ..
	sta     $A3                             ; 8187 85 A3                    ..
	lda     #$00                            ; 8189 A9 00                    ..
	sta     $A5                             ; 818B 85 A5                    ..
	lda     #$02                            ; 818D A9 02                    ..
	sta     $A4                             ; 818F 85 A4                    ..
	ldy     #$77                            ; 8191 A0 77                    .w
	ldxai	L4753
	jsr     blockmove
	lda     L8179                           ; 819A AD 79 81                 .y.
	sta     $AE                             ; 819D 85 AE                    ..
	lda     L817A                           ; 819F AD 7A 81                 .z.
	sta     $AF                             ; 81A2 85 AF                    ..
	ldy     #$00                            ; 81A4 A0 00                    ..
	lda     ($AE),y                         ; 81A6 B1 AE                    ..
	sta     L817B                           ; 81A8 8D 7B 81                 .{.
	lda     L817A                           ; 81AB AD 7A 81                 .z.
	sta     $A3                             ; 81AE 85 A3                    ..
	clc                                     ; 81B0 18                       .
	lda     L817B                           ; 81B1 AD 7B 81                 .{.
	adc     #$01                            ; 81B4 69 01                    i.
	sta     $A4                             ; 81B6 85 A4                    ..
	lda     #$00                            ; 81B8 A9 00                    ..
	sta     $A5                             ; 81BA 85 A5                    ..
	ldy     L8179                           ; 81BC AC 79 81                 .y.
	ldxai	L4755
	jsr     blockmove
	lda     #$00                            ; 81C6 A9 00                    ..
	sta     $A3                             ; 81C8 85 A3                    ..
	lda     #$00                            ; 81CA A9 00                    ..
	sta     $A4                             ; 81CC 85 A4                    ..
	ldy     L817B                           ; 81CE AC 7B 81                 .{.
	ldx     #$47                            ; 81D1 A2 47                    .G
	lda     #$56                            ; 81D3 A9 56                    .V
	jsr     L4A53                           ; 81D5 20 53 4A                  SJ
	rts                                     ; 81D8 60                       `

; ----------------------------------------------------------------------------
L81D9:	.byte	$01                             ; 81D9 01                       .
L81DA:  tya                                     ; 81DA 98                       .
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
	lda     L4750                           ; 820F AD 50 47                 .PG
	jsr     sub_7035
	rdmv	L81DC, $A0
	mv	$A3, L81DD
	lda     #$00                            ; 8224 A9 00                    ..
	sta     $A5                             ; 8226 85 A5                    ..
	lda     #$0A                            ; 8228 A9 0A                    ..
	sta     $A4                             ; 822A 85 A4                    ..
	ldy     L81DC                           ; 822C AC DC 81                 ...
	ldxai	L81E8
	jsr     blockmove
	lda     #$00                            ; 8236 A9 00                    ..
	sta     $A3                             ; 8238 85 A3                    ..
	lda     #$27                            ; 823A A9 27                    .'
	sta     $A4                             ; 823C 85 A4                    ..
	lda     #$17                            ; 823E A9 17                    ..
	sta     $A5                             ; 8240 85 A5                    ..
	ldy     #$00                            ; 8242 A0 00                    ..
	ldx     #$81                            ; 8244 A2 81                    ..
	lda     #$DE                            ; 8246 A9 DE                    ..
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
L82AD:  ldy     #$01                            ; 82AD A0 01                    ..
	sty     L81E2                           ; 82AF 8C E2 81                 ...
	jmp     L832C                           ; 82B2 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L82B5:	add16i	L81E6, L81DC, $001E
	dmv	$A3, L81E7
	lda     L81E9                           ; 82D0 AD E9 81                 ...
	sta     $A5                             ; 82D3 85 A5                    ..
	ldy     L81E6                           ; 82D5 AC E6 81                 ...
	ldxai	L81DE
	jsr     sub_4C1D
	add8m	off_AE, L81E0, L81ED
	sub8i	L81E0, off_AE, $01
	add8m	off_AE, L81E1, L81EE
	sub8i	L81E1, off_AE, $01
	lda     #$81                            ; 8301 A9 81                    ..
	sta     $A3                             ; 8303 85 A3                    ..
	ldy     #$DE                            ; 8305 A0 DE                    ..
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
	lda     #$0D                            ; 835A A9 0D                    ..
	jsr     sub_4BA7
	jmp     L8371                           ; 835F 4C 71 83                 Lq.

; ----------------------------------------------------------------------------
L8362:  lda     #$00                            ; 8362 A9 00                    ..
	cmp     L81D9                           ; 8364 CD D9 81                 ...
	lbcs	L8371
	lda     #$0F                            ; 836C A9 0F                    ..
	jsr     sub_4BA7
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
	lda     #$0E                            ; 83A2 A9 0E                    ..
	jsr     sub_4BA7
	jmp     L83B9                           ; 83A7 4C B9 83                 L..

; ----------------------------------------------------------------------------
L83AA:  lda     #$00                            ; 83AA A9 00                    ..
	cmp     L81DA                           ; 83AC CD DA 81                 ...
	lbcs	L83B9
	lda     #$10                            ; 83B4 A9 10                    ..
	jsr     sub_4BA7
L83B9:  lda     L474F                           ; 83B9 AD 4F 47                 .OG
	eor     #$03                            ; 83BC 49 03                    I.
	lbne	L83CE
	lda     L81E5                           ; 83C3 AD E5 81                 ...
	sta     L4763                           ; 83C6 8D 63 47                 .cG
	ldy     #$01                            ; 83C9 A0 01                    ..
	sty     L4657                           ; 83CB 8C 57 46                 .WF
L83CE:  jmp     L83E2                           ; 83CE 4C E2 83                 L..

; ----------------------------------------------------------------------------
L83D1:	dmv	L4762, L81E4
	yldi	L4657, $01
L83E2:  rts                                     ; 83E2 60                       `

; ----------------------------------------------------------------------------
L83E3:	.byte	$09                             ; 83E3 09                       .
L83E4:	.byte	$4C                             ; 83E4 4C                       L
L83E5:	.byte	$DD                             ; 83E5 DD                       .
L83E6:	.byte	$F3                             ; 83E6 F3                       .
L83E7:	.byte	$A9                             ; 83E7 A9                       .
L83E8:	.byte	$00
L83E9:	.byte	$F0                             ; 83E9 F0                       .
L83EA:  inc     $AD,x                           ; 83EA F6 AD                    ..
	.byte   $B7                             ; 83EC B7                       .
L83ED:	.byte	$09                             ; 83ED 09                       .
L83EE:  and     #$BF                            ; 83EE 29 BF                    ).
	;jmp     LF6C2                           ; 83F0 4C C2 F6                 L..
	.byte	$4C,$C2,$F6
	.byte   $AD                             ; 83F3 AD                       .
L83F4:	.byte	$B7                             ; 83F4 B7                       .
L83F5:	.byte	$09                             ; 83F5 09                       .
L83F6:	.byte	$09                             ; 83F6 09                       .
L83F7:  .byte	$40
L83F8:	.byte	$D0,$E7
	.byte   $A9                             ; 83FA A9                       .
L83FB:	.byte	$09                             ; 83FB 09                       .
L83FC:  ldy     #$B8                            ; 83FC A0 B8                    ..
	ldx     #$20                            ; 83FE A2 20                    . 
	;jsr     LF823                           ; 8400 20 23 F8                  #.
	.byte	$20,$23,$F8
	lda     #$04                            ; 8403 A9 04                    ..
	.byte   $9D                             ; 8405 9D                       .
	lsr     a                               ; 8406 4A                       J
L8407:	.byte	$03                             ; 8407 03                       .
L8408:	.byte	$A9                             ; 8408 A9                       .
L8409:	.byte	$03                             ; 8409 03                       .
L840A:	.byte	$20                             ; 840A 20                        
L840B:  clv                                     ; 840B B8                       .

; ----------------------------------------------------------------------------
sub_840C:  
	prolog
	stxa	L83E3
	add8m	L83E5, L4751, L83E3
	add8m	L83E6, L4752, L83E4
	lda     L474F                           ; 8429 AD 4F 47                 .OG
	eor     #$01                            ; 842C 49 01                    I.
	lbne	L843C
	ldxa	L83E5
	jsr     sub_8047
L843C:  lda     L474F                           ; 843C AD 4F 47                 .OG
	eor     #$02                            ; 843F 49 02                    I.
	lbeq	L8447
	rts                                     ; 8446 60                       `

; ----------------------------------------------------------------------------
L8447:  lda     L4750                           ; 8447 AD 50 47                 .PG
	jsr     sub_7035
	rdmv	L83E7, $A0
	lda     L83E8                           ; 8457 AD E8 83                 ...
	sta     $A3                             ; 845A 85 A3                    ..
	lda     #$00                            ; 845C A9 00                    ..
	sta     $A5                             ; 845E 85 A5                    ..
	lda     #$05                            ; 8460 A9 05                    ..
	sta     $A4                             ; 8462 85 A4                    ..
	ldy     L83E7                           ; 8464 AC E7 83                 ...
	ldx     #$84                            ; 8467 A2 84                    ..
	lda     #$07                            ; 8469 A9 07                    ..
	jsr     blockmove
	lda     L840B                           ; 846E AD 0B 84                 ...
	jsr     sub_65B0
	lda     $A1                             ; 8474 A5 A1                    ..
	sta     L83EA                           ; 8476 8D EA 83                 ...
	lda     $A0                             ; 8479 A5 A0                    ..
	sta     L83E9                           ; 847B 8D E9 83                 ...
	lda     L83EA                           ; 847E AD EA 83                 ...
	sta     $A3                             ; 8481 85 A3                    ..
	lda     #$00                            ; 8483 A9 00                    ..
	sta     $A5                             ; 8485 85 A5                    ..
	lda     #$09                            ; 8487 A9 09                    ..
	sta     $A4                             ; 8489 85 A4                    ..
	ldy     L83E9                           ; 848B AC E9 83                 ...
	ldx     #$83                            ; 848E A2 83                    ..
	lda     #$F7                            ; 8490 A9 F7                    ..
	jsr     blockmove
	sec                                     ; 8495 38                       8
	lda     L8407                           ; 8496 AD 07 84                 ...
	sbc     L8409                           ; 8499 ED 09 84                 ...
	sta     $AE                             ; 849C 85 AE                    ..
	sec                                     ; 849E 38                       8
	lda     L83E5                           ; 849F AD E5 83                 ...
	sbc     $AE                             ; 84A2 E5 AE                    ..
	sta     L83ED                           ; 84A4 8D ED 83                 ...
	sec                                     ; 84A7 38                       8
	lda     L8408                           ; 84A8 AD 08 84                 ...
	sbc     L840A                           ; 84AB ED 0A 84                 ...
	sta     $AE                             ; 84AE 85 AE                    ..
	sec                                     ; 84B0 38                       8
	lda     L83E6                           ; 84B1 AD E6 83                 ...
	sbc     $AE                             ; 84B4 E5 AE                    ..
	sta     L83EE                           ; 84B6 8D EE 83                 ...
	lda     L83ED                           ; 84B9 AD ED 83                 ...
	cmp     L83F7                           ; 84BC CD F7 83                 ...
	bcs     L84CC                           ; 84BF B0 0B                    ..
	lda     L83EE                           ; 84C1 AD EE 83                 ...
	cmp     L83F8                           ; 84C4 CD F8 83                 ...
	lbcc	L84CD
L84CC:	rts                                     ; 84CC 60                       `

; ----------------------------------------------------------------------------
L84CD:	shladdm8 off_AE, L83FB, L83EE
	ldp16 L83F4
	add16m8 off_AE, L83F4, L83ED
	lda     ($AE),y                         ; 84FE B1 AE                    ..
L8500:  sta     L83F6                           ; 8500 8D F6 83                 ...
	ldx     L83F6                           ; 8503 AE F6 83                 ...
	lda     L840B                           ; 8506 AD 0B 84                 ...
	jsr     sub_7F93
	lda     $A0                             ; 850C A5 A0                    ..
	lbne	L851C
	ldxa	L83E3
	jsr     sub_81F2
L851C:  rts                                     ; 851C 60                       `

; ----------------------------------------------------------------------------
L851D:	.byte	$F8
L851E:	.byte	$4C
L851F:	.byte	$DD
L8520:	.byte	$F3

; ----------------------------------------------------------------------------
sub_8521:  
	stack_prolog L851D, $03
	lda     L851D                           ; 852A AD 1D 85                 ...
	eor     #$01                            ; 852D 49 01                    I.
	lbne	L854F
	lda     L851E                           ; 8534 AD 1E 85                 ...
	eor     #$41                            ; 8537 49 41                    IA
	lbeq	L854F
	dmv	off_AE, L851F
L8548:  lda     L851E                           ; 8548 AD 1E 85                 ...
	ldy     #$00                            ; 854B A0 00                    ..
	sta     ($AE),y                         ; 854D 91 AE                    ..
L854F:  rts                                     ; 854F 60                       `

; ----------------------------------------------------------------------------
L8550:	.byte	$9D                             ; 8550 9D                       .
L8551:  .byte	$48
L8552:	.byte	$03                             ; 8552 03                       .
L8553:  .byte	$60
L8554:	.byte	$A0                             ; 8554 A0                       .
L8555:	.byte	$EB                             ; 8555 EB                       .
L8556:	.byte	$A9                             ; 8556 A9                       .
L8557:	.byte	$09                             ; 8557 09                       .
L8558:	.byte	$20                             ; 8558 20                        
L8559:	.byte	$23                             ; 8559 23                       #
L855A:  .byte	$F8
L855B:	.byte	$A0                             ; 855B A0                       .
L855C:	.byte	$80                             ; 855C 80                       .
L855D:	.byte	$4C                             ; 855D 4C                       L
L855E:	.byte	$2B                             ; 855E 2B                       +
L855F:  .byte	$F8
L8560:	.byte	$A9                             ; 8560 A9                       .
L8561:	.byte	$07                             ; 8561 07                       .
L8562:	.byte	$8D                             ; 8562 8D                       .
L8563:	.byte	$6C                             ; 8563 6C                       l
L8564:  .byte	$0A
L8565:  lda     $0A6C                           ; 8565 AD 6C 0A                 .l.
	asl     a                               ; 8568 0A                       .
L8569:  .byte	$0A
L856A:  .byte	$0A
L856B:  .byte	$0A
L856C:  .byte	$AA
L856D:	.byte	$20                             ; 856D 20                        
L856E:	.byte	$B6                             ; 856E B6                       .
L856F:	.byte	$F7                             ; 856F F7                       .
L8570:	.byte	$CE                             ; 8570 CE                       .
L8571:	.byte	$6C                             ; 8571 6C                       l
L8572:  .byte	$0A

sub_8573:  
	stack_prolog L8550, $02
	lda     L8550                           ; 857C AD 50 85                 .P.
	jsr     sub_7035
	rdmv	L8556, $A0
	mv	$A3, L8557
	rdldi	$A4, $0007
	ldy     L8556                           ; 8599 AC 56 85                 .V.
	ldxai	L8564
	jsr     blockmove
	mv	$A3, L8551+1
	rdldi	$A4, $0004
	ldy     L8551                           ; 85B0 AC 51 85                 .Q.
	ldxai	L8560
	jsr     blockmove
	add16i	$A2, L8556, $000E
	rdldi	$A4, $0008
	ldy     $A2                             ; 85D1 A4 A2                    ..
	ldxai	L856B
	jsr     blockmove
	yldi	$A0, $01
	rdldi	$A1, L856B
	ldx     #$08                            ; 85E6 A2 08                    ..
	ldy     #$00                            ; 85E8 A0 00                    ..
L85EA:  lda     ($A1),y                         ; 85EA B1 A1                    ..
	cmp     #$41                            ; 85EC C9 41                    .A
	beq     L85F4                           ; 85EE F0 04                    ..
	lda     #$00                            ; 85F0 A9 00                    ..
	sta     $A0                             ; 85F2 85 A0                    ..
L85F4:  iny                                     ; 85F4 C8                       .
	dex                                     ; 85F5 CA                       .
	bne     L85EA                           ; 85F6 D0 F2                    ..
	lda     $A0                             ; 85F8 A5 A0                    ..
	beq     L85FD                           ; 85FA F0 01                    ..
	rts                                     ; 85FC 60                       `

; ----------------------------------------------------------------------------
L85FD:  ldy     #$00                            ; 85FD A0 00                    ..
	sty     L855D                           ; 85FF 8C 5D 85                 .].
	lda     L8564                           ; 8602 AD 64 85                 .d.
	eor     L8560                           ; 8605 4D 60 85                 M`.
	lbne	L8611
L860D:  iny                                     ; 860D C8                       .
	sty     L855D                           ; 860E 8C 5D 85                 .].
L8611:  ldy     #$00                            ; 8611 A0 00                    ..
	sty     L855E                           ; 8613 8C 5E 85                 .^.
	add8m	off_AE, L8564, L8569
	sub8i	off_AC, off_AE, $01
	lda     $AC                             ; 8626 A5 AC                    ..
	eor     L8562                           ; 8628 4D 62 85                 Mb.
	lbne	L8634
	iny                                     ; 8630 C8                       .
	sty     L855E                           ; 8631 8C 5E 85                 .^.
L8634:  ldy     #$00                            ; 8634 A0 00                    ..
	sty     L855F                           ; 8636 8C 5F 85                 ._.
	lda     L8565                           ; 8639 AD 65 85                 .e.
	eor     L8561                           ; 863C 4D 61 85                 Ma.
	lbne	L8648
	iny                                     ; 8644 C8                       .
	sty     L855F                           ; 8645 8C 5F 85                 ._.
L8648:	rdldi	$84, $0028
	lda     L8561                           ; 8650 AD 61 85                 .a.
	ldx     #$00                            ; 8653 A2 00                    ..
	jsr     sub_444A
	st2xa	off_AE
	add16m	L8558, L466D, $AE
	sub8m	L8555, L8562, L8560
	mv	L8553, L8560
	add8m	L8554, L8553, L8555
	lda     L855F                           ; 8688 AD 5F 85                 ._.
	lbeq	L870F
	lda     L856C                           ; 8690 AD 6C 85                 .l.
	eor     #$41                            ; 8693 49 41                    IA
	lbeq	L86C8
	add16m8	L855A, L8558, L8553
	add8i	$A2, L8555, $01
	lda     #$00                            ; 86B4 A9 00                    ..
	sta     $A3                             ; 86B6 85 A3                    ..
	lda     L856C                           ; 86B8 AD 6C 85                 .l.
	sta     $A4                             ; 86BB 85 A4                    ..
	ldy     $A2                             ; 86BD A4 A2                    ..
	ldxa	L855A
	jsr     sub_45FC
L86C8:	add16m8	$A2, L8558, L8553
	ldy     $A2                             ; 86D8 A4 A2                    ..
	ldx     L856B                           ; 86DA AE 6B 85                 .k.
	lda     L855D                           ; 86DD AD 5D 85                 .].
	jsr     sub_8521
	add16m8	$A2, L8558, L8554
	ldy     $A2                             ; 86F3 A4 A2                    ..
	ldx     L856D                           ; 86F5 AE 6D 85                 .m.
	lda     L855E                           ; 86F8 AD 5E 85                 .^.
	jsr     sub_8521
	add16i	L8558, L8558, $0028
L870F:  ldy     #$01                            ; 870F A0 01                    ..
	sty     L855C                           ; 8711 8C 5C 85                 .\.
	sec                                     ; 8714 38                       8
	lda     L8563                           ; 8715 AD 63 85                 .c.
	sbc     L8561                           ; 8718 ED 61 85                 .a.
	sta     L8729                           ; 871B 8D 29 87                 .).
L871E:  lda     L8729                           ; 871E AD 29 87                 .).
	cmp     L855C                           ; 8721 CD 5C 85                 .\.
	bcs     L872A                           ; 8724 B0 04                    ..
	jmp     L8777                           ; 8726 4C 77 87                 Lw.

; ----------------------------------------------------------------------------
L8729:	.byte	$00

; ----------------------------------------------------------------------------
L872A:	add16m8 $A2, L8558, L8553
	ldy     $A2                             ; 873A A4 A2                    ..
	ldx     L8572                           ; 873C AE 72 85                 .r.
	lda     L855D                           ; 873F AD 5D 85                 .].
	jsr     sub_8521
	add16m8	$A2, L8558, L8554
	ldy     $A2                             ; 8755 A4 A2                    ..
	ldx     L856E                           ; 8757 AE 6E 85                 .n.
	lda     L855E                           ; 875A AD 5E 85                 .^.
	jsr     sub_8521
	add16i	L8558, L8558, $0028
	inc     L855C                           ; 8771 EE 5C 85                 .\.
	jmp     L871E                           ; 8774 4C 1E 87                 L..

; ----------------------------------------------------------------------------
L8777:	add8m	off_AE, L8565, L856A
	sub8i	off_AC, off_AE, $01
	lda     $AC                             ; 8787 A5 AC                    ..
	eor     L8563                           ; 8789 4D 63 85                 Mc.
	lbne	L8818
	lda     L855F                           ; 8791 AD 5F 85                 ._.
	lbeq	L87AA
	sub16i	L8558, L8558, $0028
L87AA:  lda     L8570                           ; 87AA AD 70 85                 .p.
	eor     #$41                            ; 87AD 49 41                    IA
	lbeq	L87E2
	add16m8	L855A, L8558, L8553
	clc                                     ; 87C6 18                       .
	lda     L8555                           ; 87C7 AD 55 85                 .U.
	adc     #$01                            ; 87CA 69 01                    i.
	sta     $A2                             ; 87CC 85 A2                    ..
	lda     #$00                            ; 87CE A9 00                    ..
	sta     $A3                             ; 87D0 85 A3                    ..
	lda     L8570                           ; 87D2 AD 70 85                 .p.
	sta     $A4                             ; 87D5 85 A4                    ..
	ldy     $A2                             ; 87D7 A4 A2                    ..
	ldxa	L855A
	jsr     sub_45FC
L87E2:  clc                                     ; 87E2 18                       .
	lda     L8558                           ; 87E3 AD 58 85                 .X.
	adc     L8553                           ; 87E6 6D 53 85                 mS.
	sta     $A2                             ; 87E9 85 A2                    ..
	lda     L8559                           ; 87EB AD 59 85                 .Y.
	adc     #$00                            ; 87EE 69 00                    i.
	sta     $A3                             ; 87F0 85 A3                    ..
	ldy     $A2                             ; 87F2 A4 A2                    ..
	ldx     L8571                           ; 87F4 AE 71 85                 .q.
	lda     L855D                           ; 87F7 AD 5D 85                 .].
	jsr     sub_8521
	clc                                     ; 87FD 18                       .
	lda     L8558                           ; 87FE AD 58 85                 .X.
	adc     L8554                           ; 8801 6D 54 85                 mT.
	sta     $A2                             ; 8804 85 A2                    ..
	lda     L8559                           ; 8806 AD 59 85                 .Y.
	adc     #$00                            ; 8809 69 00                    i.
	sta     $A3                             ; 880B 85 A3                    ..
	ldy     $A2                             ; 880D A4 A2                    ..
	ldx     L856F                           ; 880F AE 6F 85                 .o.
	lda     L855E                           ; 8812 AD 5E 85                 .^.
	jsr     sub_8521
L8818:  rts                                     ; 8818 60                       `

; ----------------------------------------------------------------------------
sub_8819:  
	sta     $A0                             ; 8819 85 A0                    ..
	stx     $A1                             ; 881B 86 A1                    ..
	sty     $A2                             ; 881D 84 A2                    ..
	ldy     #$00                            ; 881F A0 00                    ..
	beq     L882C                           ; 8821 F0 09                    ..
L8823:  lda     ($A2),y                         ; 8823 B1 A2                    ..
	cmp     #$41                            ; 8825 C9 41                    .A
	beq     L882B                           ; 8827 F0 02                    ..
	sta     ($A0),y                         ; 8829 91 A0                    ..
L882B:  iny                                     ; 882B C8                       .
L882C:  cpy     $A4                             ; 882C C4 A4                    ..
	bne     L8823                           ; 882E D0 F3                    ..
	rts                                     ; 8830 60                       `

; ----------------------------------------------------------------------------
L8831:  .byte	$00
L8832:  .byte	$00
L8833:  .byte	$00
L8834:	.byte	$00
L8835:  .byte	$00
L8836:  .byte	$00
L8837:  .byte	$00
L8838:  .byte	$00
L8839:  .byte	$00
L883A:  .byte	$00
L883B:  .byte	$00
L883C:  .byte	$00
	.byte	$00
L883E:  .byte	$00
L883F:  .byte	$00
L8840:  .byte	$00
L8841:  .byte	$00
L8842:  .byte	$00
L8843:  .byte	$00
	.byte	$00
	.byte	$00
L8846:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L884A:  .byte	$00
L884B:  .byte	$00

; ----------------------------------------------------------------------------
sub_884C:
	stack_prolog L8831, $05
	mv	$A3, L8833+1
	rdldi	$A4, $0004
	ldy     L8833                           ; 8862 AC 33 88                 .3.
	ldxai	L883E
	jsr     blockmove
	mv	$A3, L8835+1
	rdldi	$A4, $0004
	ldy     L8835                           ; 8879 AC 35 88                 .5.
	ldxai	$8842
	jsr     blockmove
	mv	$A3, L8831+1
	rdldi	$A4, $0006
	ldy     L8831                           ; 8890 AC 31 88                 .1.
	ldxai	L8846
	jsr     blockmove
	shladdm8 off_AE, L884A, L883F
	clc                                     ; 88AE 18                       .
	ldy     #$00                            ; 88AF A0 00                    ..
	lda     ($AE),y                         ; 88B1 B1 AE                    ..
	adc     L883E                           ; 88B3 6D 3E 88                 m>.
	sta     L8839                           ; 88B6 8D 39 88                 .9.
	iny                                     ; 88B9 C8                       .
	lda     ($AE),y                         ; 88BA B1 AE                    ..
	adc     #$00                            ; 88BC 69 00                    i.
	sta     L883A                           ; 88BE 8D 3A 88                 .:.
	lda     #$00                            ; 88C1 A9 00                    ..
	sta     $85                             ; 88C3 85 85                    ..
	lda     #$28                            ; 88C5 A9 28                    .(
	sta     $84                             ; 88C7 85 84                    ..
	lda     L8843                           ; 88C9 AD 43 88                 .C.
	ldx     #$00                            ; 88CC A2 00                    ..
	jsr     sub_444A
	st2xa	off_AE
	add16m	off_AC, L466D, off_AE
	add16m8	L8837, off_AC, L8842
	sub8m	off_AE, L8840, L883E
	add8i	L883C, off_AE, $01
	yldi	L883B, $00
	sub8m	L8920, L8841, L883F
L8915:  lda     L8920                           ; 8915 AD 20 89                 . .
	cmp     L883B                           ; 8918 CD 3B 88                 .;.
	bcs     L8921                           ; 891B B0 04                    ..
	jmp     L8960                           ; 891D 4C 60 89                 L`.

; ----------------------------------------------------------------------------
L8920:	.byte	$00

; ----------------------------------------------------------------------------
L8921:	mv	$A3, L883A
	mv	$A4, L883C
	ldy     L8839                           ; 892B AC 39 88                 .9.
	ldxa	L8837
	jsr     sub_8819
	add16i	L8837, L8837, $0028
	add16m8 L8839, L8839, L8846
	inc     L883B                           ; 895A EE 3B 88                 .;.
	jmp     L8915                           ; 895D 4C 15 89                 L..

; ----------------------------------------------------------------------------
L8960:  rts                                     ; 8960 60                       `

; ----------------------------------------------------------------------------
L8961:  .byte	$00
L8962:  .byte	$00
L8963:  .byte	$00
L8964:  .byte	$00
L8965:  .byte	$00
	.byte	$00
	.byte	$00
L8968:  .byte	$00
L8969:  .byte	$00
L896A:  .byte	$00
L896B:  .byte	$00
L896C:  .byte	$00
L896D:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8971:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8975:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8979:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L897D:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8981:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8985:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8989:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L898D:  .byte	$00
L898E:  .byte	$00
L898F:  .byte	$00
L8990:  .byte	$00
L8991:  .byte	$00
L8992:  .byte	$00
L8993:  .byte	$00
L8994:  .byte	$00
L8995:  .byte	$00
L8996:  .byte	$00
L8997:  .byte	$00
L8998:  .byte	$00
L8999:  .byte	$00
L899A:  .byte	$00
L899B:  .byte	$00
L899C:  .byte	$00
L899D:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L89A2:  .byte	$00
	.byte	$00
L89A4:  .byte	$00
L89A5:  .byte	$00
L89A6:  .byte	$00
L89A7:  .byte	$00
L89A8:  .byte	$00
L89A9:  .byte	$00
	.byte	$00
L89AB:  .byte	$00
L89AC:  .byte	$00
L89AD:  .byte	$00

sub_89AE:  
	prolog
	sta     L8961                           ; 89B1 8D 61 89                 .a.
	func16_8 sub_7035, L8962, L8961
	mv	$A3, L8962+1
	rdldi	$A4, $000E
	ldy     L8962                           ; 89D1 AC 62 89                 .b.
	ldxai	L8997
	jsr     blockmove
	func16_8 sub_65B0, L898D, L899B
	add16i	L896B, L8962, $001A
	mv	$A3, L896B+1
	rdldi	$A4, $0004
	ldy     L896B                           ; 8A09 AC 6B 89                 .k.
	ldxai	$89A5
	jsr	blockmove
	sub8m	off_AE, L89A7, L89A5
	add8i	L8968, off_AE, $01
	add8m	$A0, L89A6, L8998
	func16_8 sub_4945, L8995, $A0
	add8m	$A0, L89A5, L8997
	func16_8 sub_4945, L8993, $A0
	rdldi	$84, $0028
	ld2xa	L8995
	jsr     sub_444A
	st2xa	off_AE
	add16m	off_AC, L466D, off_AE
	add16m	L8964, off_AC, L8993
	yldi	L8992, $01
	sub8m	off_AE, L89A8, L89A6
	add8i	L8AAC, off_AE, $01
L8AA1:  lda     L8AAC                           ; 8AA1 AD AC 8A                 ...
	cmp     L8992                           ; 8AA4 CD 92 89                 ...
	bcs     L8AAD                           ; 8AA7 B0 04                    ..
	jmp     L8AD9                           ; 8AA9 4C D9 8A                 L..

; ----------------------------------------------------------------------------
L8AAC:	.byte	$00

; ----------------------------------------------------------------------------
L8AAD:	ldi	$A3, $00
	mv	$A4, L89A4
	ldy     L8968                           ; 8AB6 AC 68 89                 .h.
	ldxa	L8964
	jsr     sub_45FC
	add16i	L8964, L8964, $0028
	inc     L8992                           ; 8AD3 EE 92 89                 ...
	jmp     L8AA1                           ; 8AD6 4C A1 8A                 L..

; ----------------------------------------------------------------------------
L8AD9:  lda     L89A2                           ; 8AD9 AD A2 89                 ...
	eor     #$01                            ; 8ADC 49 01                    I.
	lbne	L8B4E
	add16i	$A0, L8962, $0016
	mv	$A3, L896C
	rdldi	$A4, L896D
	ldy     L896B                           ; 8AFF AC 6B 89                 .k.
	ldxa	$A0
	jsr     sub_4CF5
	lda     #>L896D
	sta     $A3                             ; 8B0B 85 A3                    ..
	dmv	$A4, L8997
	ldy     #<L896D
	ldxai	L897D
	jsr     sub_4C1D
	ldi	$A3, >L896D
	dmv	$A4, L8999
	ldy     #<L896D
	ldxai	L8979
	jsr     sub_4C1D
	ldi	$A3, >L8979
	rdldi	$A4, L897D
	ldy     #<L8979
	ldxa	L898D
	jsr     sub_884C
L8B4E:	add16i	off_AE, L898D, $0007
	ldp16	L8969
	ldi	$A3, $00
	sub8i	$A4, L899C, $01
	sub8i	$A5, L899D, $01
	ldy     #$00                            ; 8B7E A0 00                    ..
	ldxai	$8971
	jsr     sub_4BF2
	yldi	L8992, $01
	add16i	off_AE, L898D, $0006
	dey                                     ; 8B9B 88                       .
	lda     ($AE),y                         ; 8B9C B1 AE                    ..
	sta     L8BAC                           ; 8B9E 8D AC 8B                 ...
L8BA1:  lda     L8BAC                           ; 8BA1 AD AC 8B                 ...
	cmp     L8992                           ; 8BA4 CD 92 89                 ...
	bcs     L8BAD                           ; 8BA7 B0 04                    ..
	jmp     L8CD5                           ; 8BA9 4C D5 8C                 L..

; ----------------------------------------------------------------------------
L8BAC:  .byte	$00

; ----------------------------------------------------------------------------
L8BAD:	mv	$A3, L8969+1
	rdldi	$A4, $0005
	ldy     L8969                           ; 8BBA AC 69 89                 .i.
	ldxai	L89A9
	jsr     blockmove
	lda     L89AD                           ; 8BC4 AD AD 89                 ...
	eor     #$FF                            ; 8BC7 49 FF                    I.
	lbne    L8BD1
	jmp     L8CD5                           ; 8BCE 4C D5 8C                 L..

; ----------------------------------------------------------------------------
L8BD1:	func16_8 sub_65B0, L898F, L89A9
	ldi	$A3, $00
	dmv	off_AE, L898F
	sec                                     ; 8BEF 38                       8
	ldy     #$00                            ; 8BF0 A0 00                    ..
	lda     ($AE),y                         ; 8BF2 B1 AE                    ..
	sbc     #$01                            ; 8BF4 E9 01                    ..
	sta     $A4                             ; 8BF6 85 A4                    ..
	add16i	off_AE, L898F, $0001
	sec                                     ; 8C07 38                       8
	lda     ($AE),y                         ; 8C08 B1 AE                    ..
	sbc     #$01                            ; 8C0A E9 01                    ..
	sta     $A5                             ; 8C0C 85 A5                    ..
	ldy     #$00                            ; 8C0E A0 00                    ..
	ldxai	L8985
	jsr     sub_4BF2
	ldi	$A3, >L8985
	sub8m	$A4, L89AB, L8999
	sub8m	$A5, L89AC, L899A
	ldy     #<L8985
	ldxai	L8985
	jsr     sub_4C1D
	ldi	$A3, >L8971
	rdldi	$A4, L8989
	ldy     #<L8971
	ldxai	L8985
	jsr     sub_4CF5
	mv	L8991, $A0
	lda     L8991                           ; 8C50 AD 91 89                 ...
	eor     #$01                            ; 8C53 49 01                    I.
	lbne	L8CBE
	mv	$A3, L896B+1
	rdldi	$A4, L8979
	ldy     L896B                           ; 8C67 AC 6B 89                 .k.
	ldxai	L8989
	jsr     sub_4CF5
	ldi	$A3, >L8979
	lda     L8997                           ; 8C75 AD 97 89                 ...
	sta     $A4                             ; 8C78 85 A4                    ..
	lda     L8998                           ; 8C7A AD 98 89                 ...
	sta     $A5                             ; 8C7D 85 A5                    ..
	ldy     #<L8979
	ldxai	L8975
	jsr     sub_4C1D
	lda     #$89                            ; 8C88 A9 89                    ..
	sta     $A3                             ; 8C8A 85 A3                    ..
	sec                                     ; 8C8C 38                       8
	lda     L8999                           ; 8C8D AD 99 89                 ...
	sbc     L89AB                           ; 8C90 ED AB 89                 ...
	sta     $A4                             ; 8C93 85 A4                    ..
	sec                                     ; 8C95 38                       8
	lda     L899A                           ; 8C96 AD 9A 89                 ...
	sbc     L89AC                           ; 8C99 ED AC 89                 ...
	sta     $A5                             ; 8C9C 85 A5                    ..
	ldy     #$79                            ; 8C9E A0 79                    .y
	ldxai	L8981
	jsr     sub_4C1D
	lda     #$89                            ; 8CA7 A9 89                    ..
	sta     $A3                             ; 8CA9 85 A3                    ..
	lda     #$89                            ; 8CAB A9 89                    ..
	sta     $A5                             ; 8CAD 85 A5                    ..
	lda     #$75                            ; 8CAF A9 75                    .u
	sta     $A4                             ; 8CB1 85 A4                    ..
	ldy     #$81                            ; 8CB3 A0 81                    ..
	ldxa	L898F
	jsr     sub_884C
L8CBE:	add16i	L8969, L8969, $0006
	inc     L8992                           ; 8CCF EE 92 89                 ...
	jmp     L8BA1                           ; 8CD2 4C A1 8B                 L..

; ----------------------------------------------------------------------------
L8CD5:  lda     L896C                           ; 8CD5 AD 6C 89                 .l.
	sta     $A3                             ; 8CD8 85 A3                    ..
	dmv	$A4, L8997
	ldy     L896B                           ; 8CE4 AC 6B 89                 .k.
	ldxai	L897D
	jsr     sub_4C1D
	ldyxi	L897D
	lda     L8961                           ; 8CF2 AD 61 89                 .a.
	jsr     sub_8573
	rts                                     ; 8CF8 60                       `

; ----------------------------------------------------------------------------
L8CF9:  .byte	$00
L8CFA:  .byte	$00
L8CFB:  .byte	$00
L8CFC:  .byte	$00
L8CFD:  .byte	$00
L8CFE:  .byte	$00
L8CFF:  .byte	$00
L8D00:  .byte	$00

; ----------------------------------------------------------------------------
sub_8D01:
	prolog
	lda     L4658                           ; 8D04 AD 58 46                 .XF
	lbne	L8D0D
	rts                                     ; 8D0C 60                       `

; ----------------------------------------------------------------------------
L8D0D:  lda     L4656                           ; 8D0D AD 56 46                 .VF
	eor     #$01                            ; 8D10 49 01                    I.
	lbne	L8E06
	shladdi off_AE, L466F, $01
	ldy     #$01                            ; 8D2A A0 01                    ..
	lda     ($AE),y                         ; 8D2C B1 AE                    ..
	sta     L8D00                           ; 8D2E 8D 00 8D                 ...
	dey                                     ; 8D31 88                       .
	lda     ($AE),y                         ; 8D32 B1 AE                    ..
	sta     L8CFF                           ; 8D34 8D FF 8C                 ...
	add16i	off_AE, L8CFF, $03
	add16i	L8CFD, off_AE, $01
	dmv	off_AE, L8CFD
	ld2p16	L466D
	dldi	$A3, $0003
	ldy     #$C0                            ; 8D73 A0 C0                    ..
	ldxa	L466D
	jsr     sub_45FC
	ldy     #$01                            ; 8D7E A0 01                    ..
	sty     L8CF9                           ; 8D80 8C F9 8C                 ...
	lda     L4673                           ; 8D83 AD 73 46                 .sF
	sta     L8D94                           ; 8D86 8D 94 8D                 ...
L8D89:  lda     L8D94                           ; 8D89 AD 94 8D                 ...
	cmp     L8CF9                           ; 8D8C CD F9 8C                 ...
	bcs     L8D95                           ; 8D8F B0 04                    ..
	jmp     L8DFE                           ; 8D91 4C FE 8D                 L..

; ----------------------------------------------------------------------------
L8D94:  .byte	$00

; ----------------------------------------------------------------------------
L8D95:	sub8i	off_AE, L8CF9, $01
	ldx     $AE                             ; 8D9D A6 AE                    ..
	lda     L4659,x                         ; 8D9F BD 59 46                 .YF
	sta     L8CFA                           ; 8DA2 8D FA 8C                 ...
	func16_8 sub_7035, L8CFB, L8CFA
	add16i	off_AE, L8CFB, $000C
	ldy     #$00                            ; 8DC4 A0 00                    ..
	lda     ($AE),y                         ; 8DC6 B1 AE                    ..
	eor     #$FF                            ; 8DC8 49 FF                    I.
	lbeq	L8DF8
	add16i	off_AE, L8CFB, $000A
	lda     ($AE),y                         ; 8DDE B1 AE                    ..
	eor     #$01                            ; 8DE0 49 01                    I.
	lbne	L8DF8
	lda     L8CFB                           ; 8DE7 AD FB 8C                 ...
	ora     L8CFC                           ; 8DEA 0D FC 8C                 ...
	lbeq	L8DF8
	lda     L8CFA                           ; 8DF2 AD FA 8C                 ...
	jsr     sub_89AE
L8DF8:  inc     L8CF9                           ; 8DF8 EE F9 8C                 ...
	jmp     L8D89                           ; 8DFB 4C 89 8D                 L..

; ----------------------------------------------------------------------------
L8DFE:  ldy     #$00                            ; 8DFE A0 00                    ..
	sty     L4656                           ; 8E00 8C 56 46                 .VF
	jsr     sub_63DD
L8E06:  lda     L4657                           ; 8E06 AD 57 46                 .WF
	eor     #$01                            ; 8E09 49 01                    I.
	beq     L8E10                           ; 8E0B F0 03                    ..
	jmp     L8E1E                           ; 8E0D 4C 1E 8E                 L..

; ----------------------------------------------------------------------------
L8E10:	ldxa	L4762
	jsr     sub_8047
	ldy     #$00                            ; 8E19 A0 00                    ..
	sty     L4657                           ; 8E1B 8C 57 46                 .WF
L8E1E:  rts                                     ; 8E1E 60                       `

; ----------------------------------------------------------------------------
L8E1F:	.byte	$00
L8E20:  .byte	$00
L8E21:  .byte	$00
L8E22:  .byte	$00
L8E23:  .byte	$00

; ----------------------------------------------------------------------------
sub_8E24:					; "Z"
	stack_prolog L8E1F, $02
	and8i	L8E22, L8E1F, $7F
	ldi	L8E23, $00
	ldi	$84, $03
	ld2xa	L8E22
	jsr     sub_43D1			; off_AE = 8E22 << 3
	st2xa	off_AE
	add16m	$A0, L4674, off_AE		; $A0 = 
	mv	$A3, L8E20+1
	rdldi	$A4, $0008
	ldy     L8E20                           ; 8E69 AC 20 8E                 . .
	ldxa	$A0
	jsr     blockmove
	rts                                     ; 8E73 60                       `

; ----------------------------------------------------------------------------
L8E74:	.byte	$00
L8E75:  .byte	$00
L8E76:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L8E7B:  .byte	$00
L8E7C:  .byte	$00

; ----------------------------------------------------------------------------
sub_8E7D:  
	stack_prolog L8E74, $06
	shladdm8 off_AE, L46F5, L8E74
	ldp16	L8E7B
	add16i	off_AE, L8E7B, $0002
	add16m8	off_AC, L4678, L8E75
	lda     (off_AC),y
	sta     (off_AE),y
	jsr     sub_63DD
	jsr     sub_62D1
	add16i	$A0, L8E7B, $0003
	ldi	$A3, >L8E76
	rdldi	$A4, $0005
	ldy     #<L8E76
	ldxa	$A0
	jsr     blockmove
	jsr     sub_636E
	rts                                     ; 8EF7 60                       `

; ----------------------------------------------------------------------------
L8EF8:	.byte	$00
L8EF9:  .byte	$00
L8EFA:  .byte	$00
L8EFB:  .byte	$00
L8EFC:  .byte	$00

sub_8EFD:
	stack_prolog L8EF8, $02
	shladdm8 off_AE, L46F5, L8EF8
	ldp16	L8EFB
	dmv	off_AE, L8EFB
	lda     L8EF9                           ; 8F31 AD F9 8E                 ...
	sta     ($AE),y                         ; 8F34 91 AE                    ..
	add16i	off_AE, L8EFB, $0001
	lda     L8EFA                           ; 8F45 AD FA 8E                 ...
	sta     ($AE),y                         ; 8F48 91 AE                    ..
	iny                                     ; 8F4A C8                       .
	sty	L4656
	rts                                     ; 8F4E 60                       `

; ----------------------------------------------------------------------------
L8F4F:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00

; ----------------------------------------------------------------------------
sub_8F55:  
	stack_prolog L8F4F, $05
	ldi	$A3, >L8F4F
	rdldi	$A4, $0006
	ldy     #<L8F4F
	ldxai	L46EF
	jsr     blockmove
	jsr     sub_63DD
	jsr     sub_62D1
	rts                                     ; 8F79 60                       `

; ----------------------------------------------------------------------------
L8F7A:  .byte	$00
L8F7B:  .byte	$00
L8F7C:  .byte	$00


; ----------------------------------------------------------------------------
sub_8F7D:  
	prolog
	ldi	$A3, $00
	ldy     #$50                            ; 8F84 A0 50                    .P
	ldxai	L46F9
	jsr     sub_45F6
	rdldi	L8F7B, L46F9
	yldi	L8F7A, $00
L8F9C:  lda     #$09                            ; 8F9C A9 09                    ..
	cmp     L8F7A                           ; 8F9E CD 7A 8F                 .z.
	lbcc	L8FDE
	shladdm8 off_AE, L46F5, L8F7A
	lda     L8F7C                           ; 8FBA AD 7C 8F                 .|.
	ldy     #$01                            ; 8FBD A0 01                    ..
	sta     ($AE),y                         ; 8FBF 91 AE                    ..
	lda     L8F7B                           ; 8FC1 AD 7B 8F                 .{.
	dey                                     ; 8FC4 88                       .
	sta     ($AE),y                         ; 8FC5 91 AE                    ..
	add16i	L8F7B, L8F7B, $0008
	inc     L8F7A                           ; 8FD8 EE 7A 8F                 .z.
	jmp     L8F9C                           ; 8FDB 4C 9C 8F                 L..

; ----------------------------------------------------------------------------
L8FDE:	dmv	off_AE, L4678
	lda     #$E0                            ; 8FE8 A9 E0                    ..
	ldy     #$00                            ; 8FEA A0 00                    ..
	sta     ($AE),y                         ; 8FEC 91 AE                    ..
	add16i	off_AE, L4678, $0001
	ldi	$84, $08
	ld2xa	L4674
	jsr     sub_43E0
	ldy     #$00                            ; 900B A0 00                    ..
	sta     ($AE),y                         ; 900D 91 AE                    ..
	lda     #>LE000
	sta     $A3                             ; 9011 85 A3                    ..
	rdldi	$A4, $0400
	ldy     #<LE000
	ldxa	L4674
	jsr     blockmove
	ldy     #$18                            ; 9026 A0 18                    ..
	ldx     #$02                            ; 9028 A2 02                    ..
	lda     #$00                            ; 902A A9 00                    ..
	jsr     sub_8EFD
	lda     #$CA                            ; 902F A9 CA                    ..
	sta     $A3                             ; 9031 85 A3                    ..
	lda     #$94                            ; 9033 A9 94                    ..
	sta     $A4                             ; 9035 85 A4                    ..
	lda     #$46                            ; 9037 A9 46                    .F
	sta     $A5                             ; 9039 85 A5                    ..
	lda     #$88                            ; 903B A9 88                    ..
	sta     $A6                             ; 903D 85 A6                    ..
	ldy     #$28                            ; 903F A0 28                    .(
	ldx     #$00                            ; 9041 A2 00                    ..
	lda     #$00                            ; 9043 A9 00                    ..
	jsr     sub_8E7D
	ldx     #$0F                            ; 9048 A2 0F                    ..
	lda     #$00                            ; 904A A9 00                    ..
	jsr     sub_8F55
	rts                                     ; 904F 60                       `

; ----------------------------------------------------------------------------
L9050:  .byte	$00
L9051:  .byte	$00
L9052:  .byte	$00
L9053:  .byte	$00
L9054:  .byte	$00
P9055:  .addr	$0000
L9057:  .byte	$00
L9058:  .byte	$00
L9059:  .byte	$00
L905A:  .byte	$00
L905B:  .byte	$00
L905C:  .byte	$00
L905D:  .byte	$00
L905E:  .byte	$00
L905F:	.byte	$01                             ; 905F 01                       .
L9060:  .byte	$00
L9061:  .byte	$00
	.byte	$00
	.byte	$00
L9064:  .byte	$00
L9065:  .byte	$00
L9066:  .byte	$00
L9067:  .byte	$00
L9068:  .byte	$00
	.byte   $9F                             ; 9069 9F                       .
	tya                                     ; 906A 98                       .
	sta     $B39B,x                         ; 906B 9D 9B B3                 ...
	lda     $B0,x                           ; 906E B5 B0                    ..
	.byte   $B2                             ; 9070 B2                       .
	ldx     $27                             ; 9071 A6 27                    .'
	.byte   $67                             ; 9073 67                       g
	.byte   $A7                             ; 9074 A7                       .
	.byte   $E7                             ; 9075 E7                       .
	ora     ($51),y                         ; 9076 11 51                    .Q
	sta     ($D1),y                         ; 9078 91 D1                    ..
	.byte   $FF                             ; 907A FF                       .
L907B:	.byte	$69                             ; 907B 69                       i
L907C:	.byte	$90                             ; 907C 90                       .

; ----------------------------------------------------------------------------
sub_907D:
	prolog
	sta     L9066                           ; 9080 8D 66 90                 .f.
	ldy     #$00                            ; 9083 A0 00                    ..
	sty     L9067                           ; 9085 8C 67 90                 .g.
L9088:	add16m8	off_AE, L907B, L9067
	ldy     #$00                            ; 9098 A0 00                    ..
	lda     ($AE),y                         ; 909A B1 AE                    ..
	sta     L9068                           ; 909C 8D 68 90                 .h.
	lda     L9066                           ; 909F AD 66 90                 .f.
	eor     L9068                           ; 90A2 4D 68 90                 Mh.
	beq     L90AA                           ; 90A5 F0 03                    ..
	jmp     L90AF                           ; 90A7 4C AF 90                 L..

; ----------------------------------------------------------------------------
L90AA:  ldi	$A0, $01
	rts                                     ; 90AE 60                       `

; ----------------------------------------------------------------------------
L90AF:  inc     L9067                           ; 90AF EE 67 90                 .g.
	lda     L9068                           ; 90B2 AD 68 90                 .h.
	eor     #$FF                            ; 90B5 49 FF                    I.
	lbne	L9088
	ldi	$A0, $00
	rts                                     ; 90C0 60                       `

; ----------------------------------------------------------------------------
L90C1:  .byte	$00
L90C2:  .byte	$00
L90C3:  .byte	$00
L90C4:  .byte	$00
L90C5:  .byte	$00
L90C6:  .byte	$00
L90C7:  .byte	$00
L90C8:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L90CC:  .byte	$00
L90CD:  .byte	$00

; ----------------------------------------------------------------------------
sub_90CE:
	prolog
	stx     L90C2                           ; 90D1 8E C2 90                 ...
	sta     L90C1                           ; 90D4 8D C1 90                 ...
	lda     L90C1                           ; 90D7 AD C1 90                 ...
	jsr     sub_65B0
	rdmv	L90C3, $A0
	lda     L90C4                           ; 90E7 AD C4 90                 ...
	sta     $A3                             ; 90EA 85 A3                    ..
	lda     #$00                            ; 90EC A9 00                    ..
	sta     $A5                             ; 90EE 85 A5                    ..
	lda     #$06                            ; 90F0 A9 06                    ..
	sta     $A4                             ; 90F2 85 A4                    ..
	ldy     L90C3                           ; 90F4 AC C3 90                 ...
	ldxai	L90C8
	jsr     blockmove
	shladdm8 off_AE, L90CC, L90C2
	ldy     #$01
	lda     ($AE),y                         ; 9114 B1 AE                    ..
	sta     L90C6                           ; 9116 8D C6 90                 ...
	dey                                     ; 9119 88                       .
	lda     ($AE),y                         ; 911A B1 AE                    ..
	sta     L90C5                           ; 911C 8D C5 90                 ...
	ldy     L90C8                           ; 911F AC C8 90                 ...
	ldxa	L90C5
	jsr     sub_4B97
	lda     $A0                             ; 912B A5 A0                    ..
	sta     L90C7                           ; 912D 8D C7 90                 ...
	lda     L90C7                           ; 9130 AD C7 90                 ...
	sta     $A0                             ; 9133 85 A0                    ..
	rts                                     ; 9135 60                       `

; ----------------------------------------------------------------------------
L9136:  .byte	$00
L9137:  .byte	$00
L9138:  .byte	$00,$00
L913A:  .byte	$00
L913B:  .byte	$00
L913C:  .byte	$00
L913D:  .byte	$00
L913E:  .byte	$00
L913F:  .byte	$00
L9140:  .byte	$00
L9141:	.byte	$00
	.byte	$00
	.byte	$00
L9144:  .byte	$00
L9145:  .byte	$00

; ----------------------------------------------------------------------------
sub_9146:  
	prolog
	stxa	L9136
	func16_8 sub_65B0, L9138, L9136
	mv	$A3, L9138+1
	rdldi	$A4, $0006
	ldy     L9138                           ; 916C AC 38 91                 .8.
	ldxai	L9140
	jsr     blockmove
	lda     L9137                           ; 9176 AD 37 91                 .7.
	cmp     L9141                           ; 9179 CD 41 91                 .A.
	lbcs	L9242
	shladdm8 off_AE, L9144, L9137
	ldp16	L913C
	add8i	off_AE, L9137, $01
	shladdm8 off_AC, L9144, off_AE
	iny                                     ; 91BD C8                       .
	lda     ($AC),y                         ; 91BE B1 AC                    ..
	sta     L913B                           ; 91C0 8D 3B 91                 .;.
	dey                                     ; 91C3 88                       .
	lda     ($AC),y                         ; 91C4 B1 AC                    ..
	sta     L913A                           ; 91C6 8D 3A 91                 .:.
	sec                                     ; 91C9 38                       8
	lda     L9141                           ; 91CA AD 41 91                 .A.
	sbc     L9137                           ; 91CD ED 37 91                 .7.
	sta     $AE                             ; 91D0 85 AE                    ..
	sec                                     ; 91D2 38                       8
	lda     $AE                             ; 91D3 A5 AE                    ..
	sbc     #$01                            ; 91D5 E9 01                    ..
	sta     $AC                             ; 91D7 85 AC                    ..
	lda     #$00                            ; 91D9 A9 00                    ..
	sta     $85                             ; 91DB 85 85                    ..
	lda     L9140                           ; 91DD AD 40 91                 .@.
	sta     $84                             ; 91E0 85 84                    ..
	lda     $AC                             ; 91E2 A5 AC                    ..
	ldx     #$00                            ; 91E4 A2 00                    ..
	jsr     sub_444A
	st2xa	L913E
	lda     L913B                           ; 91F0 AD 3B 91                 .;.
	sta     $A3                             ; 91F3 85 A3                    ..
	lda     L913F                           ; 91F5 AD 3F 91                 .?.
	sta     $A5                             ; 91F8 85 A5                    ..
	lda     L913E                           ; 91FA AD 3E 91                 .>.
	sta     $A4                             ; 91FD 85 A4                    ..
	ldy     L913A                           ; 91FF AC 3A 91                 .:.
	ldx     L913D                           ; 9202 AE 3D 91                 .=.
	lda     L913C                           ; 9205 AD 3C 91                 .<.
	jsr     blockmove
	sub8i	off_AE, L9141, $01
	shladdm8 off_AC, L9144, off_AE
	ldy     #$01                            ; 9226 A0 01                    ..
	lda     ($AC),y                         ; 9228 B1 AC                    ..
	sta     $A1                             ; 922A 85 A1                    ..
	dey                                     ; 922C 88                       .
	lda     ($AC),y                         ; 922D B1 AC                    ..
	sta     $A0                             ; 922F 85 A0                    ..
	lda     #$00                            ; 9231 A9 00                    ..
	sta     $A3                             ; 9233 85 A3                    ..
	ldy     L9140                           ; 9235 AC 40 91                 .@.
	ldxa	$A0
	jsr     sub_45F6
	jmp     L9247                           ; 923F 4C 47 92                 LG.

; ----------------------------------------------------------------------------
L9242:	ldi	$A0, $00
	rts                                     ; 9246 60                       `

; ----------------------------------------------------------------------------
L9247:	ldi	$A0, $01
	rts                                     ; 924B 60                       `

; ----------------------------------------------------------------------------
L924C:  .byte	$00
L924D:  .byte	$00
L924E:  .byte	$00
L924F:  .byte	$00
L9250:  .byte	$00
L9251:  .byte	$00
L9252:  .byte	$00
L9253:  .byte	$00
L9254:  .byte	$00
L9255:  .byte	$00
L9256:  .byte	$00
L9257:  .byte	$00
L9258:  .byte	$00
	.byte	$00
	.byte	$00
L925B:  .byte	$00
L925C:  .byte	$00

; ----------------------------------------------------------------------------
sub_925D:  
	prolog
	stx     L924D                           ; 9260 8E 4D 92                 .M.
	sta     L924C                           ; 9263 8D 4C 92                 .L.
	func16_8 sub_65B0, L924E, L924C
	mv	$A3, L924E+1
	rdldi	$A4, $0006
	ldy     L924E                           ; 9283 AC 4E 92                 .N.
	ldxai	L9257
	jsr     blockmove
	sec                                     ; 928D 38                       8
	lda     L9258                           ; 928E AD 58 92                 .X.
	sbc     #$01                            ; 9291 E9 01                    ..
	sta     $A1                             ; 9293 85 A1                    ..
	ldx     $A1                             ; 9295 A6 A1                    ..
	lda     L924C                           ; 9297 AD 4C 92                 .L.
	jsr     sub_90CE
	lda     $A0                             ; 929D A5 A0                    ..
	sta     L9250                           ; 929F 8D 50 92                 .P.
	lda     L924D                           ; 92A2 AD 4D 92                 .M.
	cmp     L9258                           ; 92A5 CD 58 92                 .X.
	lbcs	L9352
	lda     L9250                           ; 92AD AD 50 92                 .P.
	lbne	L9352
	add8i	off_AE, L924D, $01
	shladdm8 off_AC, L925B, off_AE
	ldy     #$01                            ; 92D0 A0 01                    ..
	lda     ($AC),y                         ; 92D2 B1 AC                    ..
	sta     L9254                           ; 92D4 8D 54 92                 .T.
	dey                                     ; 92D7 88                       .
	lda     ($AC),y                         ; 92D8 B1 AC                    ..
	sta     L9253                           ; 92DA 8D 53 92                 .S.
	shladdm8 off_AE, L925B, L924D
	iny                                     ; 92F1 C8                       .
	lda     ($AE),y                         ; 92F2 B1 AE                    ..
	sta     L9252                           ; 92F4 8D 52 92                 .R.
	dey                                     ; 92F7 88                       .
	lda     ($AE),y                         ; 92F8 B1 AE                    ..
	sta     L9251                           ; 92FA 8D 51 92                 .Q.
	sec                                     ; 92FD 38                       8
	lda     L9258                           ; 92FE AD 58 92                 .X.
	sbc     L924D                           ; 9301 ED 4D 92                 .M.
	sta     $AE                             ; 9304 85 AE                    ..
	sec                                     ; 9306 38                       8
	lda     $AE                             ; 9307 A5 AE                    ..
	sbc     #$01                            ; 9309 E9 01                    ..
	sta     $AC                             ; 930B 85 AC                    ..
	lda     #$00                            ; 930D A9 00                    ..
	sta     $85                             ; 930F 85 85                    ..
	lda     L9257                           ; 9311 AD 57 92                 .W.
	sta     $84                             ; 9314 85 84                    ..
	lda     $AC                             ; 9316 A5 AC                    ..
	ldx     #$00                            ; 9318 A2 00                    ..
	jsr     sub_444A
	st2xa	L9255
	lda     L9252                           ; 9324 AD 52 92                 .R.
	sta     $A3                             ; 9327 85 A3                    ..
	rdmv	$A4, L9255
	ldy     L9251                           ; 9333 AC 51 92                 .Q.
	ldxa	L9253
	jsr     sub_4EB1
	lda     #$00                            ; 933F A9 00                    ..
	sta     $A3                             ; 9341 85 A3                    ..
	ldy     L9257                           ; 9343 AC 57 92                 .W.
	ldxa	L9251
	jsr     sub_45F6
	jmp     L935C                           ; 934F 4C 5C 93                 L\.

; ----------------------------------------------------------------------------
L9352:  lda     #$11                            ; 9352 A9 11                    ..
	jsr     sub_4BA7
	ldi	$A0, $00
	rts                                     ; 935B 60                       `

; ----------------------------------------------------------------------------
L935C:	ldi	$A0, $01
	rts                                     ; 9360 60                       `

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
	ldy     #$00                            ; 936D A0 00                    ..
	sty     L9363                           ; 936F 8C 63 93                 .c.
	add16i	off_AE, P9055, $0002
	lda     ($AE),y                         ; 9381 B1 AE                    ..
	sta     L9361                           ; 9383 8D 61 93                 .a.
	add16i	$A2, P9055, $0016
	lda     #$00                            ; 9395 A9 00                    ..
	sta     $A5                             ; 9397 85 A5                    ..
	lda     #$04                            ; 9399 A9 04                    ..
	sta     $A4                             ; 939B 85 A4                    ..
	ldy     $A2                             ; 939D A4 A2                    ..
	ldxai	$9366
	jsr     blockmove
	lda     L905E                           ; 93A6 AD 5E 90                 .^.
	sta     L9362                           ; 93A9 8D 62 93                 .b.
	add8m	L9366, L9366, L9361
	add8m	L9368, L9368, L9361
	ldx     L9366                           ; 93C0 AE 66 93                 .f.
	lda     L9362                           ; 93C3 AD 62 93                 .b.
	jsr     sub_4955
	lda     $A0                             ; 93C9 A5 A0                    ..
	sta     L9364                           ; 93CB 8D 64 93                 .d.
	lda     L9364                           ; 93CE AD 64 93                 .d.
	eor     #$01                            ; 93D1 49 01                    I.
	lbne	L93E5
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

; ----------------------------------------------------------------------------
L9416:  .byte	$00
	.byte	$00
	.byte	$00
L9419:  .byte	$00
L941A:  .byte	$00
	.byte	$00
L941C:	.byte	$00
	.byte	$00
L941E:  .byte	$00
L941F:  .byte	$00
	.byte	$00
	.byte	$00
L9422:  .byte	$00
	.byte	$00
L9424:  .byte	$00
	.byte	$00
L9426:  .byte	$00

; ----------------------------------------------------------------------------
sub_9427:  
	prolog
	sta     L9416                           ; 942A 8D 16 94                 ...
	add16m8	off_AE, L9059, L905E
	ldy     #$00                            ; 943D A0 00                    ..
	lda     (off_AE),y                      ; 943F B1 AE                    ..
	eor     #$80                            ; 9441 49 80                    I.
	sta     (off_AE),y                      ; 9443 91 AE                    ..
	lda     L9050                           ; 9445 AD 50 90                 .P.
	eor     #$01                            ; 9448 49 01                    I.
	lbne	L9499
	mv	$A3, L905A
	ldi	$A5, $00
	mv	$A4, L9060
	ldy     L9059                           ; 945D AC 59 90                 .Y.
	ldxa	L905B
	jsr     blockmove
	jmp     L9470                           ; 9469 4C 70 94                 Lp.

; ----------------------------------------------------------------------------
L946C:	.byte	$03,"CBS"

; ----------------------------------------------------------------------------
L9470:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldi	$A7, $00
	mv	$A6, L905D
	rdmv	$A8, L9059
	ldy     #$4C                            ; 9490 A0 4C                    .L
	ldxai	L946C
	jsr     sub_55A0
L9499:  lda     #$7F                            ; 9499 A9 7F                    ..
	cmp     L9416                           ; 949B CD 16 94                 ...
	lbcs	L94AB
	yldi	L9416, $00
	jmp     L94E8                           ; 94A8 4C E8 94                 L..

; ----------------------------------------------------------------------------
L94AB:	sub8i	off_AE, L9061, $01
	lda     $AE                             ; 94B3 A5 AE                    ..
	cmp     L9416                           ; 94B5 CD 16 94                 ...
	lbcs	L94E8
	lda     L905F                           ; 94BD AD 5F 90                 ._.
	eor     #$01                            ; 94C0 49 01                    I.
	lbne	L94DF
	sec                                     ; 94C7 38                       8
	lda     L9416                           ; 94C8 AD 16 94                 ...
	sbc     L9061                           ; 94CB ED 61 90                 .a.
	sta     $AE                             ; 94CE 85 AE                    ..
	add8i	$A1, off_AE, $01
	ldx     $A1                             ; 94D7 A6 A1                    ..
	lda     L9053                           ; 94D9 AD 53 90                 .S.
	jsr     sub_66FC
L94DF:  sec                                     ; 94DF 38                       8
	lda     L9061                           ; 94E0 AD 61 90                 .a.
	sbc     #$01                            ; 94E3 E9 01                    ..
	sta     L9416                           ; 94E5 8D 16 94                 ...
L94E8:	shladdm8 off_AE, L9064, L9416
	ldp16	L905B
	mv	$A3, L905B+1
	ldi	$A5, $00
	mv	$A4, L9060
	ldy     L905B                           ; 9517 AC 5B 90                 .[.
	ldxa	L9059
	jsr     blockmove
	ldx     L9416                           ; 9523 AE 16 94                 ...
	lda     L9053                           ; 9526 AD 53 90                 .S.
	jsr     sub_90CE
	mv	L905D, $A0
	mv	L9051, L9416
	mv	$A3, P9055+1
	ldi	$A5, $00
	ldi	$A4, $07
	ldy     P9055
	ldxai	L941C
	jsr     blockmove
	add16i	$A2, P9055, $001E
	lda     #$00                            ; 955D A9 00                    ..
	sta     $A5                             ; 955F 85 A5                    ..
	lda     #$04                            ; 9561 A9 04                    ..
	sta     $A4                             ; 9563 85 A4                    ..
	ldy     $A2                             ; 9565 A4 A2                    ..
	ldx     #$94                            ; 9567 A2 94                    ..
	lda     #$23                            ; 9569 A9 23                    .#
	jsr     blockmove
	clc                                     ; 956E 18                       .
	lda     L941F                           ; 956F AD 1F 94                 ...
	adc     L9424                           ; 9572 6D 24 94                 m$.
	sta     L9419                           ; 9575 8D 19 94                 ...
	clc                                     ; 9578 18                       .
	lda     L941F                           ; 9579 AD 1F 94                 ...
	adc     L9426                           ; 957C 6D 26 94                 m&.
	sta     $AE                             ; 957F 85 AE                    ..
	clc                                     ; 9581 18                       .
	lda     $AE                             ; 9582 A5 AE                    ..
	adc     L9422                           ; 9584 6D 22 94                 m".
	sta     $AC                             ; 9587 85 AC                    ..
	sec                                     ; 9589 38                       8
	lda     $AC                             ; 958A A5 AC                    ..
	sbc     #$01                            ; 958C E9 01                    ..
	sta     L941A                           ; 958E 8D 1A 94                 ...
	ldx     L9419                           ; 9591 AE 19 94                 ...
	lda     L9051                           ; 9594 AD 51 90                 .Q.
	jsr     sub_4955
	lda     $A0                             ; 959A A5 A0                    ..
	eor     #$01                            ; 959C 49 01                    I.
	lbne	L95B8
	sub8m	off_AE, L9419, L9051
	sub8m	L941F, L941F, off_AE
	jmp     L95DC                           ; 95B5 4C DC 95                 L..

; ----------------------------------------------------------------------------
L95B8:  ldx     L941A                           ; 95B8 AE 1A 94                 ...
	lda     L9051                           ; 95BB AD 51 90                 .Q.
	jsr     sub_496E
	lda     $A0                             ; 95C1 A5 A0                    ..
	eor     #$01                            ; 95C3 49 01                    I.
	lbne	L95DC
L95CA:  clc                                     ; 95CA 18                       .
	lda     L941F                           ; 95CB AD 1F 94                 ...
	adc     L9051                           ; 95CE 6D 51 90                 mQ.
	sta     $AE                             ; 95D1 85 AE                    ..
	sec                                     ; 95D3 38                       8
	lda     $AE                             ; 95D4 A5 AE                    ..
	sbc     L941A                           ; 95D6 ED 1A 94                 ...
	sta     L941F                           ; 95D9 8D 1F 94                 ...
L95DC:  lda     L9051                           ; 95DC AD 51 90                 .Q.
	sta     $A3                             ; 95DF 85 A3                    ..
	ldy     #$80                            ; 95E1 A0 80                    ..
	ldxa	L9053
	jsr     cmd_uv
	lda     L941F                           ; 95EC AD 1F 94                 ...
	sta     $A3                             ; 95EF 85 A3                    ..
	ldy     L941E                           ; 95F1 AC 1E 94                 ...
	ldx     L9053                           ; 95F4 AE 53 90                 .S.
	lda     L9052                           ; 95F7 AD 52 90                 .R.
	jsr     cmd_lm
	ldy     #$00                            ; 95FD A0 00                    ..
	sty     L9050                           ; 95FF 8C 50 90                 .P.
	clc                                     ; 9602 18                       .
	lda     L9059                           ; 9603 AD 59 90                 .Y.
	adc     L905E                           ; 9606 6D 5E 90                 m^.
	sta     $AE                             ; 9609 85 AE                    ..
	lda     L905A                           ; 960B AD 5A 90                 .Z.
	adc     #$00                            ; 960E 69 00                    i.
	sta     $AF                             ; 9610 85 AF                    ..
	lda     ($AE),y                         ; 9612 B1 AE                    ..
	eor     #$80                            ; 9614 49 80                    I.
	sta     ($AE),y                         ; 9616 91 AE                    ..
	iny                                     ; 9618 C8                       .
	sty     L4656                           ; 9619 8C 56 46                 .VF
	rts                                     ; 961C 60                       `

; ----------------------------------------------------------------------------
L961D:  .byte	$00

; ----------------------------------------------------------------------------
sub_961E:  
	prolog
	sta     L961D                           ; 9621 8D 1D 96                 ...
	clc                                     ; 9624 18                       .
	lda     L9059                           ; 9625 AD 59 90                 .Y.
	adc     L905E                           ; 9628 6D 5E 90                 m^.
	sta     $AE                             ; 962B 85 AE                    ..
	lda     L905A                           ; 962D AD 5A 90                 .Z.
	adc     #$00                            ; 9630 69 00                    i.
	sta     $AF                             ; 9632 85 AF                    ..
	ldy     #$00                            ; 9634 A0 00                    ..
	lda     ($AE),y                         ; 9636 B1 AE                    ..
	eor     #$80                            ; 9638 49 80                    I.
	sta     ($AE),y                         ; 963A 91 AE                    ..
	lda     #$7F                            ; 963C A9 7F                    ..
	cmp     L961D                           ; 963E CD 1D 96                 ...
	bcc     L9646                           ; 9641 90 03                    ..
	jmp     L964C                           ; 9643 4C 4C 96                 LL.

; ----------------------------------------------------------------------------
L9646:  sty     L961D                           ; 9646 8C 1D 96                 ...
	jmp     L9660                           ; 9649 4C 60 96                 L`.

; ----------------------------------------------------------------------------
L964C:  lda     L961D                           ; 964C AD 1D 96                 ...
	cmp     L9060                           ; 964F CD 60 90                 .`.
	bcs     L9657                           ; 9652 B0 03                    ..
	jmp     L9660                           ; 9654 4C 60 96                 L`.

; ----------------------------------------------------------------------------
L9657:  sec                                     ; 9657 38                       8
	lda     L9060                           ; 9658 AD 60 90                 .`.
	sbc     #$01                            ; 965B E9 01                    ..
	sta     L961D                           ; 965D 8D 1D 96                 ...
L9660:  lda     L961D                           ; 9660 AD 1D 96                 ...
	sta     L905E                           ; 9663 8D 5E 90                 .^.
	add16m8	off_AE, L9059, L905E
	ldy     #$00                            ; 9676 A0 00                    ..
	lda     ($AE),y                         ; 9678 B1 AE                    ..
	eor     #$80                            ; 967A 49 80                    I.
	sta     ($AE),y                         ; 967C 91 AE                    ..
	jsr     sub_936A
	rts                                     ; 9681 60                       `

; ----------------------------------------------------------------------------
L9682:  .byte	$00
L9683:  .byte	$00
L9684:  .byte	$00
	.byte	$00
	.byte	$00
L9687:  .byte	$00
L9688:  .byte	$00
L9689:  .byte	$00
L968A:  .byte	$00
L968B:  .byte	$00
L968C:  .byte	$00
L968D:  .byte	$00

; ----------------------------------------------------------------------------
sub_968E:
	stack_prolog L9682, $02
	mv	L9052, L9682
	func16_8 sub_7035, P9055, L9052
	add16i	off_AE, P9055, $0004
	ldy     #$00                            ; 96BC A0 00                    ..
	lda     ($AE),y                         ; 96BE B1 AE                    ..
	sta     L9053                           ; 96C0 8D 53 90                 .S.
	func16_8 sub_65B0, L9057, L9053
	mv	$A3, L9057+1
	rdldi	$A4, $0006
	ldy     L9057                           ; 96E0 AC 57 90                 .W.
	ldxai	L9060
	jsr     blockmove
	mv	L9054, L9683
	ldxa	L9053
	jsr     sub_799B
	lda     $A1                             ; 96F9 A5 A1                    ..
	sta     L968D                           ; 96FB 8D 8D 96                 ...
	lda     $A0                             ; 96FE A5 A0                    ..
	sta     L968C                           ; 9700 8D 8C 96                 ...
	ldy     #$00                            ; 9703 A0 00                    ..
	sty     L905E                           ; 9705 8C 5E 90                 .^.
	lda     #$00                            ; 9708 A9 00                    ..
	sta     $A3                             ; 970A 85 A3                    ..
	ldy     #$00                            ; 970C A0 00                    ..
	ldxa	L9053
	jsr     cmd_uv
	dmv	off_AE, L968C
	ldy     #$00                            ; 9721 A0 00                    ..
	lda     (off_AE),y                      ; 9723 B1 AE                    ..
	sta     L968B                           ; 9725 8D 8B 96                 ...
	func16_8 sub_65B0, L9687, L968B
	dmv	off_AE, L9687
	ldy     #$00                            ; 9742 A0 00                    ..
	lda     (off_AE),y
	sta     $A1                             ; 9746 85 A1                    ..
	ldx     $A1                             ; 9748 A6 A1                    ..
	lda     L9060                           ; 974A AD 60 90                 .`.
	jsr     sub_4990
	lda     $A0                             ; 9750 A5 A0                    ..
	sta     L9060                           ; 9752 8D 60 90                 .`.
	clc                                     ; 9755 18                       .
	lda     L9687                           ; 9756 AD 87 96                 ...
	adc     #$04                            ; 9759 69 04                    i.
	sta     $AE                             ; 975B 85 AE                    ..
	lda     L9688                           ; 975D AD 88 96                 ...
	adc     #$00                            ; 9760 69 00                    i.
	sta     $AF                             ; 9762 85 AF                    ..
	ldy     #$01                            ; 9764 A0 01                    ..
	lda     ($AE),y                         ; 9766 B1 AE                    ..
	sta     L968A                           ; 9768 8D 8A 96                 ...
	dey                                     ; 976B 88                       .
	lda     ($AE),y                         ; 976C B1 AE                    ..
	sta     L9689                           ; 976E 8D 89 96                 ...
	dmv	off_AE, L9689
	iny                                     ; 977B C8                       .
	lda     ($AE),y                         ; 977C B1 AE                    ..
	sta     L905A                           ; 977E 8D 5A 90                 .Z.
	dey                                     ; 9781 88                       .
	lda     ($AE),y                         ; 9782 B1 AE                    ..
	sta     L9059                           ; 9784 8D 59 90                 .Y.
	sty     L9051                           ; 9787 8C 51 90                 .Q.
	sty     L9050                           ; 978A 8C 50 90                 .P.
	lda     #$00                            ; 978D A9 00                    ..
	jsr     sub_9427
	jsr     sub_936A
	mv	L905F, L9684
	rts                                     ; 979B 60                       `

; ----------------------------------------------------------------------------
L979C:  .byte	$00
L979D:  .byte	$00
L979E:  .byte	$00
L979F:  .byte	$00
L97A0:  .byte	$00

; ----------------------------------------------------------------------------
sub_97A1:  
	prolog
	sta     L979C                           ; 97A4 8D 9C 97                 ...
	lda     L464E                           ; 97A7 AD 4E 46                 .NF
	eor     #$02                            ; 97AA 49 02                    I.
	lbeq	L97B2
	rts                                     ; 97B1 60                       `

; ----------------------------------------------------------------------------
L97B2:	yldi	L979D, $00
	and8i	off_AE, L979C, $C0
	lda     $AE                             ; 97BE A5 AE                    ..
	eor     #$C0                            ; 97C0 49 C0                    I.
	lbeq	L97E7
	lda	CH
	eor     #$FF                            ; 97CA 49 FF                    I.
	lbeq	L97DE
	lda     #$07                            ; 97D1 A9 07                    ..
	jsr     sub_45A3
	mv	L979D, $A0
	jmp     L97E4                           ; 97DB 4C E4 97                 L..

; ----------------------------------------------------------------------------
L97DE:	mv	L979D, L979C
L97E4:  jmp     L97EC                           ; 97E4 4C EC 97                 L..

; ----------------------------------------------------------------------------
L97E7:	ldi	CH, $FF
L97EC:  lda     L979D                           ; 97EC AD 9D 97                 ...
	eor     #$9B                            ; 97EF 49 9B                    I.
	lbne	L9810
	add8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	lda     #$00                            ; 9803 A9 00                    ..
	jsr     sub_961E
	lda     #$15                            ; 9808 A9 15                    ..
	jsr     sub_4BA7
	jmp     L9BCE                           ; 980D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9810:  lda     L979C                           ; 9810 AD 9C 97                 ...
	eor     #$2C                            ; tab
	lbne	L9830
	add8i	off_AE, L905E, $04
	and8i	$A0, off_AE, $FC
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 982D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9830:  lda     L979C                           ; 9830 AD 9C 97                 ...
	eor     #$87                            ; ctrl-*
	lbne	L984A
	add8i	$A0, L905E, $01
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 9847 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L984A:  lda     L979C                           ; 984A AD 9C 97                 ...
	eor     #$86                            ; ctrl-+
	lbne	L9864
	sub8i	$A0, L905E, $01
	proc8	sub_961E, $A0
	jmp     L9BCE                           ; 9861 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9864:  lda     L979C                           ; 9864 AD 9C 97                 ...
	eor     #$F6                            ; ctrl-shift-<
	lbne	L9876
	lda     #$00                            ; 986E A9 00                    ..
	jsr     sub_961E
	jmp     L9BCE                           ; 9873 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9876:  lda     L979C                           ; 9876 AD 9C 97                 ...
	eor     #$F7                            ; ctrl-shift->
	lbne	L9889
	proc8	sub_961E, L905D
	jmp     L9BCE                           ; 9886 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9889:  lda     L979C                           ; 9889 AD 9C 97                 ...
	eor     #$8E                            ; ctrl--
	lbne	L98A9
	sub8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	proc8	sub_961E, L905E
	jmp     L9BCE                           ; 98A6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98A9:  lda     L979C                           ; 98A9 AD 9C 97                 ...
	eor     #$8F                            ; ctrl-=
	lbne	L98C9
	add8i	$A0, L9051, $01
	proc8	sub_9427, $A0
	proc8	sub_961E, L905E
	jmp     L9BCE                           ; 98C6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98C9:  lda     L979C                           ; 98C9 AD 9C 97                 ...
	eor     #$CE                            ; ctrl-shift--
	lbne	L98E0
	lda     #$00                            ; 98D3 A9 00                    ..
	jsr     sub_9427
	lda     #$00                            ; 98D8 A9 00                    ..
	jsr     sub_961E
	jmp     L9BCE                           ; 98DD 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98E0:  lda     L979C                           ; 98E0 AD 9C 97                 ...
	eor     #$CF                            ; ctrl-shift-=
	lbne	L98FF
	sub8i	$A0, L9061, $01
	proc8	sub_9427, $A0
	lda     #$00                            ; 98F7 A9 00                    ..
	jsr     sub_961E
	jmp     L9BCE                           ; 98FC 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98FF:  lda     L979C                           ; 98FF AD 9C 97                 ...
	eor     #$B4                            ; 9902 49 B4                    I.
	lbne	L999C
	lda     L905E                           ; 9909 AD 5E 90                 .^.
	cmp     L905D                           ; 990C CD 5D 90                 .].
	lbcc	L991A
	jsr     sub_4F5A
	jmp     L9999                           ; 9917 4C 99 99                 L..

; ----------------------------------------------------------------------------
L991A:	add16m8 L979F, L9059, L905E
	add16i	$A2, L979F, $0001
	sub8m	$A4, L9060, L905E
	ldi	$A5, $00
	ldy     $A2                             ; 9948 A4 A2                    ..
	ldxa	L979F
	jsr     blockmove
	sub8i	off_AE, L9060, $01
	add16m8	off_AC, L9059, off_AE
	lda     #$00                            ; 996A A9 00                    ..
	ldy     #$00                            ; 996C A0 00                    ..
	sta     ($AC),y                         ; 996E 91 AC                    ..
	sub8i	L905D, L905D, $01
	add16m8	off_AE, L9059, L905E
	lda     ($AE),y                         ; 9989 B1 AE                    ..
	eor     #$80                            ; 998B 49 80                    I.
	sta     ($AE),y                         ; 998D 91 AE                    ..
	iny                                     ; 998F C8                       .
	sty     L9050                           ; 9990 8C 50 90                 .P.
	lda     L905E                           ; 9993 AD 5E 90                 .^.
	jsr     sub_961E
L9999:  jmp     L9BCE                           ; 9999 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L999C:  lda     L979C                           ; 999C AD 9C 97                 ...
	eor     #$34                            ; 999F 49 34                    I4
	lbne	L9A45
	lda     L905E                           ; 99A6 AD 5E 90                 .^.
	lbne	L99B4
	jsr     sub_4F5A
	jmp     L9A42                           ; 99B1 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L99B4:  lda     L905D                           ; 99B4 AD 5D 90                 .].
	cmp     L905E                           ; 99B7 CD 5E 90                 .^.
	lbcc	L9A35
	add16m8	L979F, L9059, L905E
	sub16i	$A0, L979F, $01
	mv	$A3, L979F+1
	sub8m	$A4, L9060, L905E
	ldi	$A5, $00
	ldy     L979F                           ; 99F2 AC 9F 97                 ...
	ldxa	$A0
	jsr     blockmove
	sub8i	off_AE, L9060, $01
	add16m8	off_AC, L9059, off_AE
	lda     #$00                            ; 9A13 A9 00                    ..
	ldy     #$00                            ; 9A15 A0 00                    ..
	sta     ($AC),y                         ; 9A17 91 AC                    ..
	iny                                     ; 9A19 C8                       .
	sty     L9050                           ; 9A1A 8C 50 90                 .P.
	sub8i	L905D, L905D, $01
	sub8i	L905E, L905E, $01
	jsr     sub_936A
	jmp     L9A42                           ; 9A32 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L9A35:	sub8i	$A0, L905E, $01
	lda     $A0                             ; 9A3D A5 A0                    ..
	jsr     sub_961E
L9A42:  jmp     L9BCE                           ; 9A42 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A45:  lda     L979C                           ; 9A45 AD 9C 97                 ...
	eor     #$74                            ; 9A48 49 74                    It
	lbne	L9A96
	ldx     L9051                           ; 9A4F AE 51 90                 .Q.
	lda     L9053                           ; 9A52 AD 53 90                 .S.
	jsr     sub_9146
	lda     $A0                             ; 9A58 A5 A0                    ..
	sta     L979E                           ; 9A5A 8D 9E 97                 ...
	lda     L979E                           ; 9A5D AD 9E 97                 ...
	eor     #$01                            ; 9A60 49 01                    I.
	lbne	L9A93
	yldi	L9050, $00
	jmp     L9A72                           ; 9A6C 4C 72 9A                 Lr.

; ----------------------------------------------------------------------------
L9A6F:	.byte	$02,"CB"

; ----------------------------------------------------------------------------
L9A72:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldy     #$44                            ; 9A7F A0 44                    .D
	ldxai	$9A6F
	jsr     sub_55A0
	lda     L9051                           ; 9A88 AD 51 90                 .Q.
	jsr     sub_9427
	lda     #$00                            ; 9A8E A9 00                    ..
	jsr     sub_961E
L9A93:  jmp     L9BCE                           ; 9A93 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A96:  lda     L979C                           ; 9A96 AD 9C 97                 ...
	eor     #$77                            ; 9A99 49 77                    Iw
	lbne	L9AE8
L9AA0:  ldx     L9051                           ; 9AA0 AE 51 90                 .Q.
	lda     L9053                           ; 9AA3 AD 53 90                 .S.
	jsr     sub_925D
	lda     $A0                             ; 9AA9 A5 A0                    ..
	sta     L979E                           ; 9AAB 8D 9E 97                 ...
	lda     L979E                           ; 9AAE AD 9E 97                 ...
	eor     #$01                            ; 9AB1 49 01                    I.
	beq     L9AB8                           ; 9AB3 F0 03                    ..
	jmp     L9AE5                           ; 9AB5 4C E5 9A                 L..

; ----------------------------------------------------------------------------
L9AB8:  lda     L9051                           ; 9AB8 AD 51 90                 .Q.
	jsr     sub_9427
	jmp     L9AC4                           ; 9ABE 4C C4 9A                 L..

; ----------------------------------------------------------------------------
L9AC1:	.byte	$02,"CB"

; ----------------------------------------------------------------------------
L9AC4:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9051
	ldy     #$49                            ; 9AD1 A0 49                    .I
	ldxai	L9AC1
	jsr     sub_55A0
	lda     L9051                           ; 9ADA AD 51 90                 .Q.
	jsr     sub_9427
	lda     #$00                            ; 9AE0 A9 00                    ..
	jsr     sub_961E
L9AE5:  jmp     L9BCE                           ; 9AE5 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9AE8:  lda     L979C                           ; 9AE8 AD 9C 97                 ...
	eor     #$76                            ; shift-<
	lbne	L9B05
	yldi	L9050, $00
	lda     L9051                           ; 9AF7 AD 51 90                 .Q.
	jsr     sub_9427 
	lda     #$00                            ; 9AFD A9 00                    ..
	jsr     sub_961E
	jmp     L9BCE                           ; 9B02 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B05:  lda     L979D                           ; 9B05 AD 9D 97                 ...
	jsr     sub_4B7B
	lda     $A0                             ; 9B0B A5 A0                    ..
	and     #$7F                            ; 9B0D 29 7F                    ).
	sta     L979D                           ; 9B0F 8D 9D 97                 ...
	lda     L905D                           ; 9B12 AD 5D 90                 .].
	eor     L9060                           ; 9B15 4D 60 90                 M`.
	lbne	L9B23
L9B1D:  jsr     sub_4F5A
	jmp     L9BCE                           ; 9B20 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B23:  sec                                     ; 9B23 38                       8
	lda     L9060                           ; 9B24 AD 60 90                 .`.
	sbc     #$01                            ; 9B27 E9 01                    ..
	sta     $AE                             ; 9B29 85 AE                    ..
	lda     L905E                           ; 9B2B AD 5E 90                 .^.
	eor     $AE                             ; 9B2E 45 AE                    E.
	lbne	L9B57
	add16m8	off_AE, L9059, L905E
	lda     L979D                           ; 9B45 AD 9D 97                 ...
	eor     #$80                            ; 9B48 49 80                    I.
	ldy     #$00                            ; 9B4A A0 00                    ..
	sta     ($AE),y                         ; 9B4C 91 AE                    ..
	mv	L905D, L9060
	jmp     L9BC6                           ; 9B54 4C C6 9B                 L..

; ----------------------------------------------------------------------------
L9B57:	add16m8	L979F, L9059, L905E
	add16i	$A0, L979F, $0001
	mv	$A3, L97A0
	sub8m	off_AC, L9060, L905E
	sub8i	$A4, $AC, $01
	ldi	$A5, $00
	ldy     L979F                           ; 9B91 AC 9F 97                 ...
	ldxa	$A0
	jsr     sub_4EB1
	add16m8	off_AE, L9059, L905E
	lda     L979D                           ; 9BAB AD 9D 97                 ...
	ldy     #$00                            ; 9BAE A0 00                    ..
	sta     (off_AE),y
	ldxa	L905D
	jsr     sub_4983
	lda     $A0                             ; 9BBB A5 A0                    ..
	sta     L905D                           ; 9BBD 8D 5D 90                 .].
	inc     L905D                           ; 9BC0 EE 5D 90                 .].
	inc     L905E                           ; 9BC3 EE 5E 90                 .^.
L9BC6:  jsr     sub_936A
	yldi	L9050, $01
L9BCE:  rts                                     ; 9BCE 60                       `

; ----------------------------------------------------------------------------
L9BCF:	.byte	$00

; ----------------------------------------------------------------------------
sub_9BD0:  
	prolog
	sta     L9BCF                           ; 9BD3 8D CF 9B                 ...
	mv	L46E7, L9BCF
	rts                                     ; 9BDC 60                       `

; ----------------------------------------------------------------------------
L9BDD:	.byte	$00
L9BDE:	.byte	$00
L9BDF:	.byte	$00

; ----------------------------------------------------------------------------
sub_9BE0:	
	stack_prolog L9BDD, $02
	add16m8 off_AE, L46EB, L9BDD
	lda     L9BDE                           ; 9BF9 AD DE 9B                 ...
	ldy     #$00                            ; 9BFC A0 00                    ..
	sta     ($AE),y                         ; 9BFE 91 AE                    ..
	add16m8 off_AE, L46ED, L9BDD
	lda     L9BDF                           ; 9C10 AD DF 9B                 ...
	sta     ($AE),y                         ; 9C13 91 AE                    ..
	rts                                     ; 9C15 60                       `

; ----------------------------------------------------------------------------
L9C16:  .byte	$00
L9C17:  .byte	$00,$00
L9C19:	.byte	$00,$00,$01,$01,$01,$00,$FF,$FF,$FF
L9C22:	.addr	L9C19
L9C24:	.byte	$00,$FF,$FF,$00,$01,$01,$01,$00,$FF
L9C2D:	.addr	L9C24
L9C2F:	dec     $1E,x                           ; 9C2F D6 1E                    ..
	ora     #$00                            ; 9C31 09 00                    ..
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9C38:	dec     $1E,x                           ; 9C38 D6 1E                    ..
	ora     #$00                            ; 9C3A 09 00                    ..
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00

sub_9C41:  
	stack_prolog L9C16, $02
	mv	L46E6, L9C16
	dmv	L46E9, L9C17
	rdldi	L46EB, L9C2F
	rdldi	L46ED, L9C38
	mv	$A3, L9C22+1
	rdldi	$A4, $0009
	ldy     L9C22                           ; 9C7D AC 22 9C                 .".
	ldxai	L9C2F
	jsr     blockmove
	mv	$A3, L9C2D+1
	rdldi	$A4, $0009
	ldy     L9C2D                           ; 9C94 AC 2D 9C                 .-.
	ldxai	L9C38
	jsr     blockmove
	lda     L46E6                           ; 9C9E AD E6 46                 ..F
	lbne	L9CAB
	lda     #$00                            ; 9CA6 A9 00                    ..
	jsr     sub_9BD0
L9CAB:  rts                                     ; 9CAB 60                       `

; ----------------------------------------------------------------------------
L9CAC:  .byte	$00

; ----------------------------------------------------------------------------
sub_9CAD:	
	prolog
	sta     L9CAC                           ; 9CB0 8D AC 9C                 ...
	mv	L46E8, L9CAC
	rts                                     ; 9CB9 60                       `

; ----------------------------------------------------------------------------
L9CBA:	.byte	$00
L9CBB:	.byte	$00
L9CBC:	.byte	$00
L9CBD:	.byte	$00
L9CBE:	.byte	$0C                             ; 9CBE 0C                       .
L9CBF:	.byte	$0C                             ; 9CBF 0C                       .
L9CC0:	.byte	$02                             ; 9CC0 02                       .
L9CC1:	.byte	$00
L9CC2:	.byte	$00
	.byte   $04                             ; 9CC3 04                       .
	.byte   $02                             ; 9CC4 02                       .
	.byte   $03                             ; 9CC5 03                       .
	.byte   $FF                             ; 9CC6 FF                       .
	asl     $08                             ; 9CC7 06 08                    ..
	.byte   $07                             ; 9CC9 07                       .
	.byte   $FF                             ; 9CCA FF                       .
	ora     $01                             ; 9CCB 05 01                    ..
	.byte	$00
L9CCE:	.byte	$C3,$9C

sub_9CD0:
	stack_prolog L9CBA, $03
	lda     L46E7                           ; 9CD9 AD E7 46                 ..F
	lbne	L9CEB
	ldy     #$00                            ; 9CE1 A0 00                    ..
	sty     L9CC2                           ; 9CE3 8C C2 9C                 ...
	lda     #$00                            ; 9CE6 A9 00                    ..
	sta     $A0                             ; 9CE8 85 A0                    ..
	rts                                     ; 9CEA 60                       `

; ----------------------------------------------------------------------------
L9CEB:  lda     #$00                            ; 9CEB A9 00                    ..
	jsr     sub_45D6
	sec                                     ; 9CF0 38                       8
	lda     $A0                             ; 9CF1 A5 A0                    ..
	sbc     #$05                            ; 9CF3 E9 05                    ..
	sta     L9CC1                           ; 9CF5 8D C1 9C                 ...
	add16m8	off_AE, L9CCE, L9CC1
	ldy     #$00                            ; 9D08 A0 00                    ..
	lda     ($AE),y                         ; 9D0A B1 AE                    ..
	sta     L9CC1                           ; 9D0C 8D C1 9C                 ...
	lda     L9CC1                           ; 9D0F AD C1 9C                 ...
	eor     L9CC2                           ; 9D12 4D C2 9C                 M..
	lbne	L9D40
	lda     #$00                            ; 9D1A A9 00                    ..
	cmp     CDTMF5
	lbcs	L9D29
	ldi	$A0, $00
	rts                                     ; 9D28 60                       `

; ----------------------------------------------------------------------------
L9D29:  lda     L9CC0                           ; 9D29 AD C0 9C                 ...
	cmp     L9CBE                           ; 9D2C CD BE 9C                 ...
	lbcs	L9D3D
	sub8i	L9CBE, L9CBE, $01
L9D3D:  jmp     L9D46                           ; 9D3D 4C 46 9D                 LF.

; ----------------------------------------------------------------------------
L9D40:	mv	L9CBE, L9CBF
L9D46:	mv	L9CC2, L9CC1
	dmv	off_AE, L9CBA
	add16m8	off_AC, L46EB, L9CC1
	ldy     #$00                            ; 9D66 A0 00                    ..
	lda     ($AC),y                         ; 9D68 B1 AC                    ..
	sta     ($AE),y                         ; 9D6A 91 AE                    ..
	dmv	off_AE, L9CBC
	add16m8	off_AC, L46ED, L9CC1
	lda     ($AC),y                         ; 9D86 B1 AC                    ..
	sta     ($AE),y                         ; 9D88 91 AE                    ..
	dmv	off_AE, L9CBA
	lda     ($AE),y                         ; 9D94 B1 AE                    ..
	lbne	L9DB4
	dmv	off_AE, L9CBC
	lda     ($AE),y                         ; 9DA5 B1 AE                    ..
	lbne	L9DB4
	ldi	$A0, $00
	rts                                     ; 9DB0 60                       `

; ----------------------------------------------------------------------------
	jmp     L9DC9                           ; 9DB1 4C C9 9D                 L..

; ----------------------------------------------------------------------------
L9DB4:	yldi	CDTMF5, $01
	ldi	CDTMV5+1, $00
	mv	CDTMV5, L9CBE
	ldi	$A0, $01
	rts                                     ; 9DC8 60                       `

; ----------------------------------------------------------------------------
L9DC9:	.byte	$00
L9DCA:	.byte	$00

; ----------------------------------------------------------------------------
sub_9DCB:  
	prolog
	lda     L46E8                           ; 9DCE AD E8 46                 ..F
	eor     #$01                            ; 9DD1 49 01                    I.
	lbeq	L9DDD
	ldi	$A0, $00
	rts                                     ; 9DDC 60                       `

; ----------------------------------------------------------------------------
L9DDD:  lda     #$00                            ; 9DDD A9 00                    ..
	jsr     read_trig
	mv	L9DCA, $A0
	lda     L9DCA                           ; 9DE7 AD CA 9D                 ...
	eor     L9DC9                           ; 9DEA 4D C9 9D                 M..
	lbne	L9DF7
	ldi	$A0, $00
	rts                                     ; 9DF6 60                       `

; ----------------------------------------------------------------------------
L9DF7:	mv	L9DC9, L9DCA
	lda     L9DCA                           ; 9DFD AD CA 9D                 ...
	lbeq	L9E0A
	ldi	$A0, $00
	rts                                     ; 9E09 60                       `

; ----------------------------------------------------------------------------
L9E0A:	ldi	$A0, $01
	rts                                     ; 9E0E 60                       `

; ----------------------------------------------------------------------------
L9E0F:	.byte	$00
L9E10:  .byte	$00
L9E11:  .byte	$00
L9E12:  .byte	$00
L9E13:  .byte	$00
L9E14:  .byte	$00
L9E15:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9E19:  .byte	$00
L9E1A:  .byte	$00
L9E1B:  .byte	$00
L9E1C:  .byte	$00
L9E1D:  .byte	$00
L9E1E:  .byte	$00
L9E1F:  .byte	$00
L9E20:  .byte	$00
L9E21:  .byte	$00
L9E22:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9E28:	.byte	$00
	.byte	$00
L9E2A:  .byte	$00
L9E2B:  .byte	$00

sub_9E2C:	
	prolog
	sta     L9E0F                           ; 9E2F 8D 0F 9E                 ...
	ldi	L9E19, $80
	ldy     #$00                            ; 9E37 A0 00                    ..
	sty     L9E1A                           ; 9E39 8C 1A 9E                 ...
	sty     L9E1B                           ; 9E3C 8C 1B 9E                 ...
	sty     L9E1C                           ; 9E3F 8C 1C 9E                 ...
	lda     L46E9                           ; 9E42 AD E9 46                 ..F
	jsr     sub_7035
	rdmv	L9E10, $A0
	lda     L9E11                           ; 9E52 AD 11 9E                 ...
	sta     $A3                             ; 9E55 85 A3                    ..
	ldi	$A5, $00
	ldi	$A4, $05
	ldy     L9E10                           ; 9E5F AC 10 9E                 ...
	ldxai	L9E1D                           ; 9E64 A9 1D                    ..
	jsr     blockmove
	lda     L46E6                           ; 9E69 AD E6 46                 ..F
	eor     #$02                            ; 9E6C 49 02                    I.
	lbne	L9E94
L9E73:  sec                                     ; 9E73 38                       8
	lda     #$00                            ; 9E74 A9 00                    ..
	sbc     L9E1D                           ; 9E76 ED 1D 9E                 ...
	sta     L9E19                           ; 9E79 8D 19 9E                 ...
	sec                                     ; 9E7C 38                       8
	lda     #$00                            ; 9E7D A9 00                    ..
	sbc     L9E1E                           ; 9E7F ED 1E 9E                 ...
	sta     L9E1A                           ; 9E82 8D 1A 9E                 ...
	dmv	L9E1B, L9E1F
	jmp     L9F69                           ; 9E91 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9E94:  lda     L46E6                           ; 9E94 AD E6 46                 ..F
	eor     #$04                            ; 9E97 49 04                    I.
	lbne	L9ED7
	ldx     L46EA                           ; 9E9E AE EA 46                 ..F
	lda     L46E9                           ; 9EA1 AD E9 46                 ..F
	jsr     sub_799B
	rdmv	L9E14, $A0
	lda     L9E15                           ; 9EB1 AD 15 9E                 ...
	sta     $A3                             ; 9EB4 85 A3                    ..
	ldi	$A5, $00
	ldi	$A4, $04
	ldy     L9E14                           ; 9EBE AC 14 9E                 ...
	ldxai	L9E28
	jsr     blockmove
	dmv	L9E1B, L9E2A
	jmp     L9F69                           ; 9ED4 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9ED7:  lda     L46E6                           ; 9ED7 AD E6 46                 ..F
	eor     #$03                            ; 9EDA 49 03                    I.
	lbne	L9F69
	dmv	L9E19, L4751
	lda     L474F                           ; 9EED AD 4F 47                 .OG
	eor     #$02                            ; 9EF0 49 02                    I.
	lbne	L9F69
	func16_8 sub_7035, L9E10, L4750	
	mv	$A3, L9E10+1
	rdldi	$A4, $0005
	ldy     L9E10                           ; 9F14 AC 10 9E                 ...
	ldxai	L9E1D
	jsr     blockmove
	func16_8 sub_65B0, L9E12, L9E21
	mv	$A3, L9E12+1
	rdldi	$A4, $0006
	ldy     L9E12                           ; 9F3B AC 12 9E                 ...
	ldxai	L9E22
	jsr     blockmove
	sub8m	off_AE, L9E1D, L9E1F
	sub8m	L9E1B, L4751, off_AE
	sub8m	off_AE, L9E1E, L9E20
	sub8m	L9E1C, L4752, off_AE
L9F69:  lda     L9E19                           ; 9F69 AD 19 9E                 ...
	eor     #$80                            ; 9F6C 49 80                    I.
	bne     L9F74                           ; 9F6E D0 04                    ..
	ora     #$00                            ; 9F70 09 00                    ..
	eor     #$FF                            ; 9F72 49 FF                    I.
L9F74:  beq     L9F79                           ; 9F74 F0 03                    ..
	jmp     L9FB7                           ; 9F76 4C B7 9F                 L..

; ----------------------------------------------------------------------------
L9F79:  jmp     L9F82                           ; 9F79 4C 82 9F                 L..

; ----------------------------------------------------------------------------
L9F7C:	.byte	$05,"cBBBB"

; ----------------------------------------------------------------------------
L9F82:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L9E1B
	ldi	$A7, $00
	mv	$A6, L9E1C
	ldi	$A9, $00
	mv	$A8, L9E19
	ldi	$AB, $00
	mv	$AA, L9E1A
	ldy     L9E0F                           ; 9FAA AC 0F 9E                 ...
	ldxai	L9F7C
	jsr     sub_55A0
	jmp     L9FDE                           ; 9FB4 4C DE 9F                 L..

; ----------------------------------------------------------------------------
L9FB7:  jmp     L9FBE                           ; 9FB7 4C BE 9F                 L..

; ----------------------------------------------------------------------------
L9FBA:	.byte	$03,"cBB"

; ----------------------------------------------------------------------------
L9FBE:  lda     #$00                            ; 9FBE A9 00                    ..
	sta     $A3                             ; 9FC0 85 A3                    ..
	lda     #$00                            ; 9FC2 A9 00                    ..
	sta     $A5                             ; 9FC4 85 A5                    ..
	lda     L9E1B                           ; 9FC6 AD 1B 9E                 ...
	sta     $A4                             ; 9FC9 85 A4                    ..
	lda     #$00                            ; 9FCB A9 00                    ..
	sta     $A7                             ; 9FCD 85 A7                    ..
	lda     L9E1C                           ; 9FCF AD 1C 9E                 ...
	sta     $A6                             ; 9FD2 85 A6                    ..
	ldy     L9E0F                           ; 9FD4 AC 0F 9E                 ...
	ldxai	L9FBA
	jsr     sub_55A0
L9FDE:  ldi	$A0, $01
	rts                                     ; 9FE2 60                       `

; ----------------------------------------------------------------------------
L9FE3:	.byte	$00
L9FE4:	.byte	$00
L9FE5:	.byte	$00
L9FE6:	.byte	$00
L9FE7:	.byte	$00,$00
L9FE9:  .byte	$00
L9FEA:  .byte	$00
	.byte	$00
	.byte	$00
L9FED:  .byte	$00
L9FEE:  .byte	$00
L9FEF:  .byte	$00
L9FF0:  .byte	$00
L9FF1:  .byte	$00
L9FF2:  .byte	$00
L9FF3:  .byte	$00
L9FF4:  .byte	$00
L9FF5:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L9FF9:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA005:  .byte	$00
LA006:  .byte	$00
LA007:  .byte	$00
LA008:  .byte	$00
LA009:  .byte	$00
LA00A:  .byte	$00
LA00B:  .byte	$00
LA00C:  .byte	$00
LA00D:  .byte	$00
LA00E:  .byte	$00
	.byte	$00
LA010:  .byte	$00
LA011:  .byte	$00
	.byte	$00
	.byte	$00
LA014:  .byte	$00
LA015:  .byte	$00
LA016:  .byte	$00
LA017:  .byte	$00
LA018:  .byte	$00
LA019:  .byte	$00
LA01A:  .byte	$00
LA01B:  .byte	$00
	.byte	$00
	.byte	$00
LA01E:  .byte	$00
LA01F:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA023:  .byte	$00
LA024:  .byte	$00
LA025:  .byte	$00
LA026:  .byte	$00

sub_A027:
	stack_prolog L9FE3, $03
	ldxa	L9FE3
	jsr     sub_799B
	rdmv	L9FE7, $A0
	test16	L9FE7
	lbne	LA04F
	rts                                     ; A04E 60                       `

; ----------------------------------------------------------------------------
LA04F:	mv	$A3, L9FE7+1
	rdldi	$A4, $0006
	ldy     L9FE7                           ; A05C AC E7 9F                 ...
	ldxai	LA00E
	jsr     blockmove
	add8m	L9FF3, LA010, L9FE5
	add8m	L9FF4, LA011, L9FE6
	func16_8 sub_65B0, L9FED, LA00E
	mv	$A3, L9FED+1
	rdldi	$A4, $0006
	ldy     L9FED                           ; A097 AC ED 9F                 ...
	ldxai	LA014
	jsr     blockmove
	func16_8 sub_65B0, L9FE9, L9FE3
	mv	$A3, L9FE9+1
	rdldi	$A4, $0009
	ldy     L9FE9                           ; A0BE AC E9 9F                 ...
	ldxai	$A01A
	jsr     blockmove
	yldi	LA006, $01
	ldi	$A3, $00
	sub8i	$A4, LA01A, $01
	sub8i	$A5, LA01B, $01
	ldy     #$00                            ; A0E1 A0 00                    ..
	ldxai	L9FF5
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, LA014, $01
	sub8i	$A5, LA015, $01
	ldy     #$00                            ; A0FE A0 00                    ..
	ldxai	L9FF9
	jsr     sub_4BF2
	ldi	$A3, >L9FF9
	dmv	$A4, L9FF3
	ldy     #<L9FF9
	ldxai	L9FF9
	jsr     sub_4C1D
	blkmv_iii LA023, L9FF9, $0004
	ldi     $A3, >L9FF9
	rdldi	$A4, $A001
	ldy     #<L9FF9
	ldxai	L9FF5
	jsr     sub_4CF5
	mv	LA005, $A0
	lda     LA005                           ; A14D AD 05 A0                 ...
	lbne	LA156
LA155:  rts                                     ; A155 60                       `

; ----------------------------------------------------------------------------
LA156:	ldxai	$A001
	jsr     sub_4E4A
	rdmv	LA007, $A0
	lda     LA007                           ; A167 AD 07 A0                 ...
	eor     LA016                           ; A16A 4D 16 A0                 M..
	bne     LA175                           ; A16D D0 06                    ..
	ora     LA008                           ; A16F 0D 08 A0                 ...
	eor     LA017                           ; A172 4D 17 A0                 M..
LA175:	lbeq	LA17B
	rts                                     ; A17A 60                       `

; ----------------------------------------------------------------------------
LA17B:	dmv	off_AE, LA018
	ldp16	L9FEF
	shladdm8 off_AE, LA01E, LA024
	ld2p16	L9FF1
	sty     LA009                           ; A1B2 8C 09 A0                 ...
	mv	LA00B, LA024
	mv	LA1CC, LA026
LA1C1:  lda     LA1CC                           ; A1C1 AD CC A1                 ...
	cmp     LA00B                           ; A1C4 CD 0B A0                 ...
	bcs     LA1CD                           ; A1C7 B0 04                    ..
	jmp     LA25D                           ; A1C9 4C 5D A2                 L].

; ----------------------------------------------------------------------------
LA1CC:  .byte	$00

; ----------------------------------------------------------------------------
LA1CD:	mv	LA00A, LA023
	mv	LA1E4, LA025
LA1D9:  lda     LA1E4                           ; A1D9 AD E4 A1                 ...
	cmp     LA00A                           ; A1DC CD 0A A0                 ...
	bcs     LA1E5                           ; A1DF B0 04                    ..
	jmp     LA245                           ; A1E1 4C 45 A2                 LE.

; ----------------------------------------------------------------------------
LA1E4:  .byte	$00

; ----------------------------------------------------------------------------
LA1E5:	add16m8	off_AE, L9FEF, LA009
	ldy     #$00                            ; A1F5 A0 00                    ..
	lda     ($AE),y                         ; A1F7 B1 AE                    ..
	sta     LA00C                           ; A1F9 8D 0C A0                 ...
	ldy     LA00C                           ; A1FC AC 0C A0                 ...
	ldx     L9FE3                           ; A1FF AE E3 9F                 ...
	lda     #$04                            ; A202 A9 04                    ..
	jsr     sub_7E24
	lda     $A0                             ; A207 A5 A0                    ..
	lbne	LA23C
	add16m8 off_AE, L9FF1, LA00A
	ldy     #$00                            ; A21E A0 00                    ..
	lda     ($AE),y                         ; A220 B1 AE                    ..
	sta     LA00D                           ; A222 8D 0D A0                 ...
	ldx     LA00D                           ; A225 AE 0D A0                 ...
	lda     L9FE3                           ; A228 AD E3 9F                 ...
	jsr     sub_7F93
	lda     $A0                             ; A22E A5 A0                    ..
	eor     #$01                            ; A230 49 01                    I.
	lbne	LA23C
	ldy     #$00                            ; A237 A0 00                    ..
	sty     LA006                           ; A239 8C 06 A0                 ...
LA23C:  inc     LA009                           ; A23C EE 09 A0                 ...
	inc     LA00A                           ; A23F EE 0A A0                 ...
	jmp     LA1D9                           ; A242 4C D9 A1                 L..

; ----------------------------------------------------------------------------
LA245:	add16m8	L9FF1, L9FF1, LA01A
	inc     LA00B                           ; A257 EE 0B A0                 ...
	jmp     LA1C1                           ; A25A 4C C1 A1                 L..

; ----------------------------------------------------------------------------
LA25D:  lda     LA006                           ; A25D AD 06 A0                 ...
	eor     #$01                            ; A260 49 01                    I.
	lbne	LA28B
	lda     L9FF4                           ; A267 AD F4 9F                 ...
	sta     $A3                             ; A26A 85 A3                    ..
	ldy     L9FF3                           ; A26C AC F3 9F                 ...
	ldxa	L9FE3
	jsr     cmd_uv
	lda     L474F                           ; A278 AD 4F 47                 .OG
	eor     #$03                            ; A27B 49 03                    I.
	lbne	LA28B
	ldxa	L9FE5
	jsr     sub_81F2
LA28B:  rts                                     ; A28B 60                       `

; ----------------------------------------------------------------------------
LA28C:  .byte	$00

; ----------------------------------------------------------------------------
sub_A28D:  
	prolog
	sta     LA28C                           ; A290 8D 8C A2                 ...
	lda     #$00                            ; A293 A9 00                    ..
	ldx     LA28C                           ; A295 AE 8C A2                 ...
	sta     L05C0,x                         ; A298 9D C0 05                 ...
	lda     LA28C                           ; A29B AD 8C A2                 ...
	jsr     sub_4569
	rts                                     ; A2A1 60                       `

; ----------------------------------------------------------------------------
LA2A2:  .byte	$00
LA2A3:  .byte	$00
LA2A4:  .byte	$00
LA2A5:  .byte	$00
LA2A6:  .byte	$00
LA2A7:  .byte	$00

; ----------------------------------------------------------------------------
sub_A2A8:
	stack_prolog LA2A2, $03
	jsr     sub_5E1E
	lda     LA2A2                           ; A2B4 AD A2 A2                 ...
	jsr     sub_A28D
	ldi	LA2A6, $04
	lda     LA2A3                           ; A2BF AD A3 A2                 ...
	eor     #$01                            ; A2C2 49 01                    I.
	lbne	LA2CE
	ldi	LA2A6, $08
LA2CE:  lda     #$00                            ; A2CE A9 00                    ..
	ldx     LA2A2                           ; A2D0 AE A2 A2                 ...
	sta     L05C0,x                         ; A2D3 9D C0 05                 ...
	add16i	off_AE, LA2A4, $0002
	ldp8	LA2A7
	lda     LA2A7                           ; A2EC AD A7 A2                 ...
	eor     #$3F                            ; A2EF 49 3F                    I?
	lbne	LA30A
	add16i	off_AE, LA2A4, $0002
	lda     L464A                           ; A305 AD 4A 46                 .JF
	sta     ($AE),y                         ; A308 91 AE                    ..
LA30A:  lda     LA2A6                           ; A30A AD A6 A2                 ...
	sta     $A3                             ; A30D 85 A3                    ..
	lda     #$00                            ; A30F A9 00                    ..
	sta     $A4                             ; A311 85 A4                    ..
	ldy     LA2A5                           ; A313 AC A5 A2                 ...
	ldx     LA2A4                           ; A316 AE A4 A2                 ...
	lda     LA2A2                           ; A319 AD A2 A2                 ...
	jsr     sub_4539
	lda     LA2A7                           ; A31F AD A7 A2                 ...
	eor     #'?'
	lbne	LA33E
	add16i	off_AE, LA2A4, $0002
	lda     #$3F                            ; A338 A9 3F                    .?
	ldy     #$00                            ; A33A A0 00                    ..
	sta     ($AE),y                         ; A33C 91 AE                    ..
LA33E:  lda     LA2A3                           ; A33E AD A3 A2                 ...
	eor     #$01                            ; A341 49 01                    I.
	lbeq	LA36C
	lda     #$7F                            ; A348 A9 7F                    ..
	cmp	L4AA4
	lbcs	LA36C
	lda     LA2A2                           ; A352 AD A2 A2                 ...
	asl     a                               ; A355 0A                       .
	asl     a                               ; A356 0A                       .
	asl     a                               ; A357 0A                       .
	asl     a                               ; A358 0A                       .
	sta     $A1                             ; A359 85 A1                    ..
	ldx     $A1                             ; A35B A6 A1                    ..
	lda     L4AA4                           ; A35D AD A4 4A                 ..J
	jsr     sub_5E5E                        ; A360 20 5E 5E                  ^^
	lda     LA2A2                           ; A363 AD A2 A2                 ...
	jsr     sub_A28D
	jmp     LA37D                           ; A369 4C 7D A3                 L}.

; ----------------------------------------------------------------------------
LA36C:  lda     LA2A3                           ; A36C AD A3 A2                 ...
	eor     #$02                            ; A36F 49 02                    I.
	lbne	LA37D
	mv	L4652, LA2A2
	rts                                     ; A37C 60                       `

; ----------------------------------------------------------------------------
LA37D:  jsr     sub_5E30
	rts                                     ; A380 60                       `

; ----------------------------------------------------------------------------
LA381:  .byte	$00

; ----------------------------------------------------------------------------
sub_A382:	
	prolog
	sta     LA381                           ; A385 8D 81 A3                 ...
	jsr     sub_5E1E
	lda     LA381                           ; A38B AD 81 A3                 ...
	jsr     sub_A28D
	lda     L4652                           ; A391 AD 52 46                 .RF
	eor     LA381                           ; A394 4D 81 A3                 M..
	lbne	LA3A1
	ldi	L4652, $02
LA3A1:  jsr     sub_5E30
	rts                                     ; A3A4 60                       `

; ----------------------------------------------------------------------------
LA3A5:  .byte	$00
LA3A6:  .byte	$00
LA3A7:  .byte	$00
LA3A8:  .byte	$00
LA3A9:  .byte	$00
LA3AA:  .byte	$00
LA3AB:  .byte	$00
LA3AC:  .byte	$00,$00
LA3AE:  .byte	$00
LA3AF:  .byte	$00
LA3B0:  .byte	$00
LA3B1:  .byte	$00
LA3B2:  .byte	$00
LA3B3:  .byte	$00
LA3B4:  .byte	$00
LA3B5:  .byte	$00
LA3B6:  .byte	$00
LA3B7:  .byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA3BB:  .byte	$00
LA3BC:  .byte	$00

sub_A3BD:
	stack_prolog LA3A5, $04
	func16_8 sub_65B0, LA3AC, LA3A5
	test16	LA3AC
	lbne	LA3E2
	rts                                     ; A3E1 60                       `

; ----------------------------------------------------------------------------
LA3E2:  jsr     sub_5E1E
	blkmv_imi LA3B3, LA3A8, $0004
	sub8m	off_AE, LA3B5, LA3B3
	add8i	LA3AE, off_AE, $01
	blkmv_imi LA3B7, LA3AC, $0006
	shladdm8 off_AE, LA3BB, LA3B4
	clc                                     ; A438 18                       .
	ldy     #$00                            ; A439 A0 00                    ..
	lda     ($AE),y                         ; A43B B1 AE                    ..
	adc     LA3B3                           ; A43D 6D B3 A3                 m..
	sta     $AC                             ; A440 85 AC                    ..
	iny                                     ; A442 C8                       .
	lda     ($AE),y                         ; A443 B1 AE                    ..
	adc     #$00                            ; A445 69 00                    i.
	sta     $AD                             ; A447 85 AD                    ..
	sub16i	LA3AA, off_AC, $0001
	lda     LA3AE                           ; A458 AD AE A3                 ...
	sta     LA3B1                           ; A45B 8D B1 A3                 ...
	lda     LA3B4                           ; A45E AD B4 A3                 ...
	sta     LA3AF                           ; A461 8D AF A3                 ...
	lda     LA3B6                           ; A464 AD B6 A3                 ...
	sta     LA475                           ; A467 8D 75 A4                 .u.
LA46A:  lda     LA475                           ; A46A AD 75 A4                 .u.
	cmp     LA3AF                           ; A46D CD AF A3                 ...
	bcs     LA476                           ; A470 B0 04                    ..
	jmp     LA52A                           ; A472 4C 2A A5                 L*.

; ----------------------------------------------------------------------------
LA475:  .byte	$00

; ----------------------------------------------------------------------------
LA476:  lda     LA3A7                           ; A476 AD A7 A3                 ...
	lbeq	LA49C
	add16i	$A0, LA3AA, $0001
	ldy     LA3AE                           ; A48D AC AE A3                 ...
	ldxa	$A0
	jsr     sub_4B97
	mv	LA3B1, $A0
LA49C:	yldi	LA3B0, $01
	mv	LA4B2, LA3B1
LA4A7:  lda     LA4B2                           ; A4A7 AD B2 A4                 ...
	cmp     LA3B0                           ; A4AA CD B0 A3                 ...
	bcs     LA4B3                           ; A4AD B0 04                    ..
	jmp     LA4F7                           ; A4AF 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4B2:  .byte	$00

; ----------------------------------------------------------------------------
LA4B3:	add16m8	off_AE, LA3AA, LA3B0
	ldy     #$00                            ; A4C3 A0 00                    ..
	lda     ($AE),y                         ; A4C5 B1 AE                    ..
	sta     LA3B2                           ; A4C7 8D B2 A3                 ...
	lda     LA3A7                           ; A4CA AD A7 A3                 ...
	lbeq	LA4DD
	lda     LA3B2                           ; A4D2 AD B2 A3                 ...
	jsr     sub_4BC9
	mv	LA3B2, $A0
LA4DD:  ldx     LA3B2                           ; A4DD AE B2 A3                 ...
	lda     LA3A6                           ; A4E0 AD A6 A3                 ...
	jsr     sub_45C7
	lda     L464D                           ; A4E6 AD 4D 46                 .MF
	lbeq	LA4F1
LA4EE:  jmp     LA4F7                           ; A4EE 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4F1:  inc     LA3B0                           ; A4F1 EE B0 A3                 ...
	jmp     LA4A7                           ; A4F4 4C A7 A4                 L..

; ----------------------------------------------------------------------------
LA4F7:  lda     LA3A7                           ; A4F7 AD A7 A3                 ...
	lbeq	LA507
LA4FF:  ldx     #$9B                            ; A4FF A2 9B                    ..
	lda     LA3A6                           ; A501 AD A6 A3                 ...
	jsr     sub_45C7
LA507:  lda     L464D                           ; A507 AD 4D 46                 .MF
	lbeq	LA512
	jmp     LA52A                           ; A50F 4C 2A A5                 L*.

; ----------------------------------------------------------------------------
LA512:	add16m8 LA3AA, LA3AA, LA3B7
	inc     LA3AF                           ; A524 EE AF A3                 ...
	jmp     LA46A                           ; A527 4C 6A A4                 Lj.

; ----------------------------------------------------------------------------
LA52A:  jsr     sub_5E30
	rts                                     ; A52D 60                       `

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
LA53A:  .byte	$00
LA53B:  .byte	$00
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

sub_A547:
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
	jsr     sub_5E1E
	ldy     #$00                            ; A5E2 A0 00                    ..
	sty     LA53B                           ; A5E4 8C 3B A5                 .;.
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
LA619:  lda     LA52F                           ; A619 AD 2F A5                 ./.
	jsr     sub_45A3
	lda     $A0                             ; A61F A5 A0                    ..
	sta     LA539                           ; A621 8D 39 A5                 .9.
	ldx     LA52F                           ; A624 AE 2F A5                 ./.
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
	lda     LA539                           ; A64D AD 39 A5                 .9.
	ldy     #$00                            ; A650 A0 00                    ..
	sta     ($AE),y                         ; A652 91 AE                    ..
	inc     LA53A                           ; A654 EE 3A A5                 .:.
	bne     LA65C                           ; A657 D0 03                    ..
	inc     LA53B                           ; A659 EE 3B A5                 .;.
LA65C:  inc     LA538                           ; A65C EE 38 A5                 .8.
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
LA68A:  jsr     sub_5E30
	lda     LA53A                           ; A68D AD 3A A5                 .:.
	sta     LA537                           ; A690 8D 37 A5                 .7.
	lda     #$08                            ; A693 A9 08                    ..
	sta     $84                             ; A695 85 84                    ..
	lda     LA53B                           ; A697 AD 3B A5                 .;.
	tax                                     ; A69A AA                       .
	lda     LA53A                           ; A69B AD 3A A5                 .:.
	jsr     sub_43E0
	sta     LA538                           ; A6A1 8D 38 A5                 .8.
	jmp     LA6AB                           ; A6A4 4C AB A6                 L..

; ----------------------------------------------------------------------------
LA6A7:	.byte	$03,"CBB"

; ----------------------------------------------------------------------------
LA6AB:  lda     #$00                            ; A6AB A9 00                    ..
	sta     $A3                             ; A6AD 85 A3                    ..
	lda     #$00                            ; A6AF A9 00                    ..
	sta     $A5                             ; A6B1 85 A5                    ..
	lda     LA538                           ; A6B3 AD 38 A5                 .8.
	sta     $A4                             ; A6B6 85 A4                    ..
	lda     #$00                            ; A6B8 A9 00                    ..
	sta     $A7                             ; A6BA 85 A7                    ..
	lda     LA537                           ; A6BC AD 37 A5                 .7.
	sta     $A6                             ; A6BF 85 A6                    ..
	ldy     #$04                            ; A6C1 A0 04                    ..
	ldxai	LA6A7
	jsr     sub_55A0
	rts                                     ; A6CA 60                       `

; ----------------------------------------------------------------------------
LA6CB:	.byte	$00
	.byte	$00
	.byte	$00

sub_A6CE:
	stack_prolog LA6CB, $02
	rts                                     ; A6D7 60                       `

; ----------------------------------------------------------------------------
LA6D8:  .byte	$00
LA6D9:  .byte	$00
LA6DA:  .byte	$00
LA6DB:  .byte	$00
LA6DC:  .byte	$00,$00
LA6DE:  .byte	$00
LA6DF:  .byte	$00
LA6E0:  .byte	$00
LA6E1:	.byte   $04,"done"
LA6E6:	.addr	LA6E1

sub_A6E8:
	stack_prolog LA6D8, $03
	lda     LA6D8                           ; A6F1 AD D8 A6                 ...
	jsr     sub_65B0
	rdmv	LA6DC, $A0
	test16	LA6DC
	lbne	LA70D
	rts                                     ; A70C 60                       `

; ----------------------------------------------------------------------------
LA70D:	add16i	off_AE, LA6DC, $0001
	ldy     #$00                            ; A71C A0 00                    ..
	lda     (off_AE),y
	sta     LA6DE                           ; A720 8D DE A6                 ...
	jsr     sub_5E1E
	lda     LA6D9                           ; A726 AD D9 A6                 ...
	jsr     sub_A28D
	lda     #$06                            ; A72C A9 06                    ..
	sta     $A3                             ; A72E 85 A3                    ..
	lda     #$00                            ; A730 A9 00                    ..
	sta     $A4                             ; A732 85 A4                    ..
	rdmv	$A5, LA6DA
	ldy     #$03                            ; A73E A0 03                    ..
	ldx     #$00                            ; A740 A2 00                    ..
	lda     LA6D9                           ; A742 AD D9 A6                 ...
	jsr     sub_45D0
LA748:  ldx     LA6D9                           ; A748 AE D9 A6                 ...
	lda     L05C0,x                         ; A74B BD C0 05                 ...
	lbne	LA7B7
	lda     #$5C                            ; A753 A9 5C                    .\
	sta     $A3                             ; A755 85 A3                    ..
	ldy     #>LB224
	ldx     #<LB224
	lda     LA6D9                           ; A75B AD D9 A6                 ...
	jsr     sub_458F
	lda     LB224                           ; A761 AD 24 B2                 .$.
	sta     LA6E0                           ; A764 8D E0 A6                 ...
	ldy     #$01                            ; A767 A0 01                    ..
	sty     LA6DF                           ; A769 8C DF A6                 ...
	lda     LA6E0                           ; A76C AD E0 A6                 ...
	sta     LA77D                           ; A76F 8D 7D A7                 .}.
LA772:  lda     LA77D                           ; A772 AD 7D A7                 .}.
	cmp     LA6DF                           ; A775 CD DF A6                 ...
	bcs     LA77E                           ; A778 B0 04                    ..
	jmp     LA799                           ; A77A 4C 99 A7                 L..

; ----------------------------------------------------------------------------
LA77D:  .byte	$00

; ----------------------------------------------------------------------------
LA77E:  ldx     LA6DF                           ; A77E AE DF A6                 ...
	lda     LB224,x                         ; A781 BD 24 B2                 .$.
	sta     $A0                             ; A784 85 A0                    ..
	lda     $A0                             ; A786 A5 A0                    ..
	jsr     sub_4B7B
	lda     $A0                             ; A78B A5 A0                    ..
	ldx     LA6DF                           ; A78D AE DF A6                 ...
	sta     LB224,x                         ; A790 9D 24 B2                 .$.
	inc     LA6DF                           ; A793 EE DF A6                 ...
	jmp     LA772                           ; A796 4C 72 A7                 Lr.

; ----------------------------------------------------------------------------
LA799:  lda     LA6E0                           ; A799 AD E0 A6                 ...
	sta     $A3                             ; A79C 85 A3                    ..
	rdldi	$A4, $B224
	ldy     LA6DE                           ; A7A6 AC DE A6                 ...
	ldx     #$00                            ; A7A9 A2 00                    ..
	lda     LA6D8                           ; A7AB AD D8 A6                 ...
	jsr     cmd_ud
	jsr     sub_8D01
	jmp     LA748                           ; A7B4 4C 48 A7                 LH.

; ----------------------------------------------------------------------------
LA7B7:  lda     LA6D9                           ; A7B7 AD D9 A6                 ...
	jsr     sub_A28D
	jsr     sub_5E30
	rts                                     ; A7C0 60                       `

; ----------------------------------------------------------------------------
LA7C1:  .byte	$00
LA7C2:  .byte	$00
LA7C3:  .byte	$00

sub_A7C4:
	stack_prolog LA7C1, $02
	jsr     sub_5E1E
	lda     #$00                            ; A7D0 A9 00                    ..
	sta     $A3                             ; A7D2 85 A3                    ..
	lda     #$00                            ; A7D4 A9 00                    ..
	sta     $A4                             ; A7D6 85 A4                    ..
	rdmv	$A5, LA7C2
	ldy     #$20                            ; A7E2 A0 20                    . 
	ldx     #$00                            ; A7E4 A2 00                    ..
	lda     LA7C1                           ; A7E6 AD C1 A7                 ...
	jsr     sub_45D0
	jsr     sub_5E30
	rts                                     ; A7EF 60                       `

; ----------------------------------------------------------------------------
LA7F0:  .byte	$00
LA7F1:  .byte	$00
LA7F2:  .byte	$00

sub_A7F3:
	stack_prolog LA7F0, $02
	jsr     sub_5E1E
	lda     #$00                            ; A7FF A9 00                    ..
	sta     $A3                             ; A801 85 A3                    ..
	lda     #$00                            ; A803 A9 00                    ..
	sta     $A4                             ; A805 85 A4                    ..
	lda     LA7F2                           ; A807 AD F2 A7                 ...
	sta     $A6                             ; A80A 85 A6                    ..
	lda     LA7F1                           ; A80C AD F1 A7                 ...
	sta     $A5                             ; A80F 85 A5                    ..
	ldy     #$21                            ; A811 A0 21                    .!
	ldx     #$00                            ; A813 A2 00                    ..
	lda     LA7F0                           ; A815 AD F0 A7                 ...
	jsr     sub_45D0
	jsr     sub_5E30
	rts                                     ; A81E 60                       `

; ----------------------------------------------------------------------------
sub_A81F:
	sta     $B118,x                         ; A81F 9D 18 B1                 ...
	rts                                     ; A822 60                       `

; ----------------------------------------------------------------------------
sub_A823:	
	clc                                     ; A823 18                       .
	adc     $B118,x                         ; A824 7D 18 B1                 }..
	sta     $B118,x                         ; A827 9D 18 B1                 ...
	rts                                     ; A82A 60                       `

; ----------------------------------------------------------------------------
sub_A82B:	
	lda     $B118,x                         ; A82B BD 18 B1                 ...
	eor     #$FF                            ; A82E 49 FF                    I.
	tay                                     ; A830 A8                       .
	iny                                     ; A831 C8                       .
	tya                                     ; A832 98                       .
	sta     $B118,x                         ; A833 9D 18 B1                 ...
	rts                                     ; A836 60                       `

; ----------------------------------------------------------------------------
sub_A837:
	stx     $A0                             ; A837 86 A0                    ..
	cmp     $A0                             ; A839 C5 A0                    ..
	bne     LA841                           ; A83B D0 04                    ..
	tya                                     ; A83D 98                       .
	jsr     sub_4BA7
LA841:  rts                                     ; A841 60                       `

; ----------------------------------------------------------------------------
sub_A842:	
	rts

; ----------------------------------------------------------------------------
LA843:  .byte	$00
LA844:  .byte	$00
LA845:  .byte	$00

sub_A846:
	stack_prolog LA843, $02
	jmp     LA855                           ; A84F 4C 55 A8                 LU.

; ----------------------------------------------------------------------------
LA852:	.byte   $02,"cS"

; ----------------------------------------------------------------------------
LA855:  lda     #$00                            ; A855 A9 00                    ..
	sta     $A3                             ; A857 85 A3                    ..
	dmv	off_AE, LA844
	lda     #$00                            ; A863 A9 00                    ..
	sta     $A5                             ; A865 85 A5                    ..
	ldy     #$00                            ; A867 A0 00                    ..
	lda     ($AE),y                         ; A869 B1 AE                    ..
	sta     $A4                             ; A86B 85 A4                    ..
	add16i	$A6, LA844, $0001
	ldy     LA843                           ; A87C AC 43 A8                 .C.
	ldxai	$A852
	jsr     sub_55A0
	rts                                     ; A886 60                       `

; ----------------------------------------------------------------------------
LA887:  .byte	$00
LA888:  .byte	$00
LA889:  .byte	$00
	.byte	$00
LA88B:  .byte	$00
LA88C:  .byte	$00
LA88D:  .byte	$00
LA88E:  .byte	$00
LA88F:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA893:  .byte	$00
LA894:  .byte	$00


; ----------------------------------------------------------------------------
sub_A895:  
	prolog
	stxa	LA887
	func16_8 sub_65B0, LA88D, LA887
	blkmv_imi LA88F, LA88D, $0006
	shladdm8 off_AE, LA893, LA888
	ldp16	LA88B
LA8E6:	dmv	off_AE, LA88B
	ldy     #$00                            ; A8F0 A0 00                    ..
	lda     ($AE),y                         ; A8F2 B1 AE                    ..
	eor     #$1E                            ; A8F4 49 1E                    I.
	lbne	LA8FE
	jmp     LA958                           ; A8FB 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA8FE:	yldi	LA889, $00
LA903:  lda     #$5B                            ; A903 A9 5B                    .[
	cmp     LA889                           ; A905 CD 89 A8                 ...
	lbcc	LA936
	add16m8	off_AE, LA88B, LA889
	ldy     #$00                            ; A91D A0 00                    ..
	lda     ($AE),y                         ; A91F B1 AE                    ..
	sta     $A0                             ; A921 85 A0                    ..
	lda     $A0                             ; A923 A5 A0                    ..
	jsr     sub_4BC9
	lda     $A0                             ; A928 A5 A0                    ..
	ldx     LA889                           ; A92A AE 89 A8                 ...
	sta     LB224,x                         ; A92D 9D 24 B2                 .$.
	inc     LA889                           ; A930 EE 89 A8                 ...
	jmp     LA903                           ; A933 4C 03 A9                 L..

; ----------------------------------------------------------------------------
LA936:  jsr     sub_4749
	lda     $A0                             ; A939 A5 A0                    ..
	lbne	LA943
LA940:  jmp     LA958                           ; A940 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA943:	add16m8	LA88B, LA88B, L4654
	jmp     LA8E6                           ; A955 4C E6 A8                 L..

; ----------------------------------------------------------------------------
LA958:  rts                                     ; A958 60                       `

; ----------------------------------------------------------------------------
sub_A959:
	stx     $A0                             ; A959 86 A0                    ..
	tax                                     ; A95B AA                       .
	lda     #$00                            ; A95C A9 00                    ..
	sta     $B800,x                         ; A95E 9D 00 B8                 ...
	lda     $A0                             ; A961 A5 A0                    ..
	sta     $B14A,x                         ; A963 9D 4A B1                 .J.
	tya                                     ; A966 98                       .
	sta     $BC00,x                         ; A967 9D 00 BC                 ...
	rts                                     ; A96A 60                       `

; ----------------------------------------------------------------------------
LA96B:	.byte	$00
LA96C:	.byte	$00
LA96D:	.byte	$D5
LA96E:	.byte	$00

; ----------------------------------------------------------------------------
sub_A96F:
	prolog
	stx     LA96C                           ; A972 8E 6C A9                 .l.
	sta     LA96B                           ; A975 8D 6B A9                 .k.
	add16m8	off_AE, LA96D, LA96B
	lda     LA96C                           ; A988 AD 6C A9                 .l.
	ldy     #$00                            ; A98B A0 00                    ..
	sta     ($AE),y                         ; A98D 91 AE                    ..
	rts                                     ; A98F 60                       `

; ----------------------------------------------------------------------------
LA990:  .byte	$00

; ----------------------------------------------------------------------------
sub_A991:
	prolog
	sta     LA990                           ; A994 8D 90 A9                 ...
	lda     LA990                           ; A997 AD 90 A9                 ...
	jsr     sub_4BC9
	mv	L464A, $A0
	rts                                     ; A9A2 60                       `

; ----------------------------------------------------------------------------
sub_A9A3:
	prolog
	lda     $D8                             ; A9A6 A5 D8                    ..
	eor     #$01                            ; A9A8 49 01                    I.
	lbne	LA9B8
LA9AF:  lda     #$12                            ; A9AF A9 12                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9B4 A0 00                    ..
	sty     $D8                             ; A9B6 84 D8                    ..
LA9B8:  lda     $D9                             ; A9B8 A5 D9                    ..
	eor     #$01                            ; A9BA 49 01                    I.
	lbne	LA9CA
	lda     #$13                            ; A9C1 A9 13                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9C6 A0 00                    ..
	sty     $D9                             ; A9C8 84 D9                    ..
LA9CA:  lda     $DA                             ; A9CA A5 DA                    ..
	eor     #$01                            ; A9CC 49 01                    I.
	lbne	LA9DC
	lda     #$14                            ; A9D3 A9 14                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9D8 A0 00                    ..
	sty     $DA                             ; A9DA 84 DA                    ..
LA9DC:  rts                                     ; A9DC 60                       `

; ----------------------------------------------------------------------------
LA9DD:	.byte	$00

; ----------------------------------------------------------------------------
sub_A9DE:  
	prolog
	sta     LA9DD                           ; A9E1 8D DD A9                 ...
	mv	L4658, LA9DD
	rts                                     ; A9EA 60                       `

; ----------------------------------------------------------------------------
LA9EB:  .byte	$00

; ----------------------------------------------------------------------------
sub_A9EC:  
	prolog
	sta     LA9EB                           ; A9EF 8D EB A9                 ...
	mv	L464E, LA9EB
	rts

; ----------------------------------------------------------------------------
LA9F9:	.word	$0000

; ----------------------------------------------------------------------------
sub_A9FB:
	prolog
	sub16m	LA9F9, MEMTOP, MEMLO
	lda     LA9F9+1
	sta     $A3                             ; AA14 85 A3                    ..
	ldy     LA9F9                           ; AA16 AC F9 A9                 ...
	ldxa	MEMLO
	jsr     sub_619A
	jmp     LAA28                           ; AA22 4C 28 AA                 L(.

; ----------------------------------------------------------------------------
LAA25:	.byte	$02,"CH"

; ----------------------------------------------------------------------------
LAA28:  lda     #$00                            ; AA28 A9 00                    ..
	sta     $A3                             ; AA2A 85 A3                    ..
	rdmv	$A4, LA9F9
	ldy     #$46                            ; AA36 A0 46                    .F
	ldxai	LAA25
	jsr     sub_55A0
	jsr     sub_747D
	jsr     sub_696A
	jsr	sub_8020
	jsr     sub_8F7D
	lda     #$00                            ; AA4B A9 00                    ..
	jsr     sub_9C41
	lda     #$00                            ; AA50 A9 00                    ..
	jsr     sub_A9EC
LAA55:  lda     #$00                            ; AA55 A9 00                    ..
	sta     $A3                             ; AA57 85 A3                    ..
	lda     #$FF                            ; AA59 A9 FF                    ..
	sta     $A4                             ; AA5B 85 A4                    ..
	ldy     #$20                            ; AA5D A0 20                    . 
	ldxai	$B14A
	jsr     sub_45FC
	lda     #$00                            ; AA66 A9 00                    ..
	sta     $A3                             ; AA68 85 A3                    ..
	ldy     #$10                            ; AA6A A0 10                    ..
	ldxai	$B118
	jsr     sub_45F6
	lda     #$FF                            ; AA73 A9 FF                    ..
	sta     L4647                           ; AA75 8D 47 46                 .GF
	lda     #$FF                            ; AA78 A9 FF                    ..
	sta     L4648                           ; AA7A 8D 48 46                 .HF
	lda     #$01                            ; AA7D A9 01                    ..
	jsr     sub_A9DE
	rts                                     ; AA82 60                       `

; ----------------------------------------------------------------------------
	.byte   $4C                             ; AA83 4C                       L
LAA84:  .addr   LAA86
LAA86:	.addr	cmd_uc,cmd_uk,cmd_ud,cmd_uf,cmd_uw,cmd_uy,cmd_ub		; "CKDFWYB"
	.addr	cmd_lx,cmd_ly,cmd_lz,cmd_uu,cmd_uv,cmd_ux,cmd_un		; "xyzUVXN"
	.addr	cmd_lc,cmd_ld,cmd_ls,cmd_lp,cmd_lm,cmd_ll,cmd_la		; "cdspmla"
	.addr	cmd_lb,sub_70E2,sub_9C41,sub_9BE0,sub_9BD0,sub_9CAD,sub_9E2C	; "bfJjST+"
	.addr	sub_7FE9,sub_817C,sub_8003,sub_80BB,sub_8EFD,sub_8E7D,sub_8F55	; "LMPRGAO"
	.addr	sub_8E24,sub_A9DE,sub_A9FB,sub_A9EC,sub_968E,sub_97A1,sub_5E1E	; "ZEiHeI0"
	.addr	sub_5E30,sub_A2A8,sub_A382,sub_A3BD,sub_A547,sub_A6E8,sub_A7C4	; "1234567"
	.addr	sub_A7F3,sub_A6CE,sub_A991,sub_4F5A,sub_A959,sub_4BA7,sub_A837	; "89n.#*="
	.addr	sub_A846,sub_4F9D,sub_8D01,sub_A895,sub_A842,sub_A96F,sub_A81F	; "$%&@.:u"
	.addr	sub_A823,sub_A82B						; "vw"

LAB08:	.byte	$00
	.byte	$00
	.byte	$00

LAB0B:
	stack_prolog LAB08, $02
LAB14:	.byte	"C","K","D","F","W","Y","B"
	.byte	"x","y","z","U","V","X","N"
	.byte	"c","d","s","p","m","l","a"
	.byte	"b","f","J","j","S","T","+"
	.byte	"L","M","P","R","G","A","O"
	.byte	"Z","E","i","H","e","I","0"
	.byte	"1","2","3","4","5","6","7"
	.byte	"8","9","n",$07,"#","*","="
	.byte	"$","%","&","@",".",":","u"
	.byte	"v","w"
	.byte	$00
LAB56:	.addr	LAB14
LAB58:  .addr	$0000
LAB5A:  .byte	$00
LAB5B:  .byte	$00
LAB5C:  .byte	$00
LAB5D:  .byte	$00
	.byte	$00
	.byte	$00
LAB60:	.byte	$D6,$1E,$08,$00,$00,$00,$00,$00
LAB68:  .byte	$00
LAB69:  .byte	$00

sub_AB6A:
	prolog
	rdmv	LAB58, LAA84
	rdmv	LAB5A, L48C1
	yldi	LAB69, $00
	iny                                     ; AB8A C8                       .
	sty     L4651                           ; AB8B 8C 51 46                 .QF
	mv	LAB68, LB224
LAB94:	add16m8 off_AE, LAB56, LAB69
	ldy     #$00                            ; ABA4 A0 00                    ..
	lda     ($AE),y                         ; ABA6 B1 AE                    ..
	eor     LAB68                           ; ABA8 4D 68 AB                 Mh.
	lbeq	LABEE
	add16m8 off_AE, LAB56, LAB69
	lda     ($AE),y                         ; ABC0 B1 AE                    ..
	lbne	LABE8
LABC7:  jmp     LABCD                           ; ABC7 4C CD AB                 L..

; ----------------------------------------------------------------------------
LABCA:	.byte	$02,"CC"

; ----------------------------------------------------------------------------
LABCD:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, LAB68
	ldy     #$3F                            ; ABDA A0 3F                    .?
	ldxai	LABCA
	jsr     sub_55A0
	ldi	$A0, $00
	rts                                     ; ABE7 60                       `

; ----------------------------------------------------------------------------
LABE8:  inc     LAB69                           ; ABE8 EE 69 AB                 .i.
	jmp     LAB94                           ; ABEB 4C 94 AB                 L..

; ----------------------------------------------------------------------------
LABEE:	shladdm8 off_AE, LAB5A, LAB69
	ldp16	LAB5C
	shladdm8 off_AE, LAB58, LAB69
	iny                                     ; AC23 C8                       .
	lda     ($AE),y                         ; AC24 B1 AE                    ..
	sta     LAB0B+2                         ; AC26 8D 0D AB                 ...
	dey                                     ; AC29 88                       .
	lda     ($AE),y                         ; AC2A B1 AE                    ..
	sta     LAB0B+1                         ; AC2C 8D 0C AB                 ...
	dmv	off_AE, LAB5C
	iny                                     ; AC39 C8                       .
	lda     ($AE),y                         ; AC3A B1 AE                    ..
	sta     $A1                             ; AC3C 85 A1                    ..
	dey                                     ; AC3E 88                       .
	lda     ($AE),y                         ; AC3F B1 AE                    ..
	sta     $A0                             ; AC41 85 A0                    ..
	ldi	$A3, >LAB60
	ldy     #<LAB60
	ldxa	$A0
	jsr     sub_58F2                        ; AC4D 20 F2 58                  .X
	yldi	LAB69, $00
LAC55:  lda     #$07                            ; AC55 A9 07                    ..
	cmp     LAB69                           ; AC57 CD 69 AB                 .i.
	lbcc	LAC70
	ldx     LAB69                           ; AC5F AE 69 AB                 .i.
	lda     LAB60,x                         ; AC62 BD 60 AB                 .`.
	ldx     LAB69                           ; AC65 AE 69 AB                 .i.
	sta     $A0,x                           ; AC68 95 A0                    ..
	inc     LAB69                           ; AC6A EE 69 AB                 .i.
	jmp     LAC55                           ; AC6D 4C 55 AC                 LU.

; ----------------------------------------------------------------------------
LAC70:  ldy     $A2                             ; AC70 A4 A2                    ..
	ldxa	$A0
	jsr     LAB0B                           ; AC76 20 0B AB                  ..
	ldi	$A0, $01
	rts                                     ; AC7D 60                       `

; ----------------------------------------------------------------------------

sub_AC7E:
	txa                                     ; AC7E 8A                       .
	pha                                     ; AC7F 48                       H
	lda     KBCODE
	eor     CH1
	bne     LAC8D                           ; AC86 D0 05                    ..
	lda     KEYDEL
	bne     LACA9                           ; AC8B D0 1C                    ..
LAC8D:  lda     $D209                           ; AC8D AD 09 D2                 ...
	sta     CH1
	ldx     L474D                           ; AC93 AE 4D 47                 .MG
	sta     $B138,x                         ; AC96 9D 38 B1                 .8.
	inc     L474D                           ; AC99 EE 4D 47                 .MG
	and8i	L474D, L474D, $0F
	ldx     #$01                            ; ACA4 A2 01                    ..
	stx     KEYDEL
LACA9:	ldi	SRTIMR, $11
	pla                                     ; ACAE 68                       h
	tax                                     ; ACAF AA                       .
	pla                                     ; ACB0 68                       h
	rti                                     ; ACB1 40                       @

; ----------------------------------------------------------------------------
sub_ACB2:  
	lda     $A0                             ; ACB2 A5 A0                    ..
	pha                                     ; ACB4 48                       H
	lda	$AE
	pha
	lda	$AF
	pha
	ldx     $E8                             ; ACBB A6 E8                    ..
	lda     L46EF,x                         ; ACBD BD EF 46                 ..F
	sta     $A0                             ; ACC0 85 A0                    ..
	shladdm8 off_AE, L46F5, $A0
	ldy     #$01                            ; ACD5 A0 01                    ..
	lda     ($AE),y                         ; ACD7 B1 AE                    ..
	sta     $E1                             ; ACD9 85 E1                    ..
	dey                                     ; ACDB 88                       .
	lda     ($AE),y                         ; ACDC B1 AE                    ..
	sta     $E0                             ; ACDE 85 E0                    ..
	inc     $E8                             ; ACE0 E6 E8                    ..
	ldy     #$02                            ; ACE2 A0 02                    ..
	lda     ($E0),y                         ; ACE4 B1 E0                    ..
	sta     $E2                             ; ACE6 85 E2                    ..
	iny                                     ; ACE8 C8                       .
	lda     ($E0),y                         ; ACE9 B1 E0                    ..
	sta     $E3                             ; ACEB 85 E3                    ..
	iny                                     ; ACED C8                       .
	lda     ($E0),y                         ; ACEE B1 E0                    ..
	sta     $E4                             ; ACF0 85 E4                    ..
	iny                                     ; ACF2 C8                       .
	lda     ($E0),y                         ; ACF3 B1 E0                    ..
	sta     $E5                             ; ACF5 85 E5                    ..
	iny                                     ; ACF7 C8                       .
	iny                                     ; ACF8 C8                       .
	lda     ($E0),y                         ; ACF9 B1 E0                    ..
	sta     $E7                             ; ACFB 85 E7                    ..
	pla                                     ; ACFD 68                       h
	sta     $AF                             ; ACFE 85 AF                    ..
	pla                                     ; AD00 68                       h
	sta	$AE
	pla                                     ; AD03 68                       h
	sta	$A0
	rts                                     ; AD06 60                       `

; ----------------------------------------------------------------------------
LAD07:	pha                                     ; AD07 48                       H
	txa                                     ; AD08 8A                       .
	pha                                     ; AD09 48                       H
	tya                                     ; AD0A 98                       .
	pha                                     ; AD0B 48                       H
	lda     $E3                             ; AD0C A5 E3                    ..
	sta     $D016                           ; AD0E 8D 16 D0                 ...
	lda     $E2                             ; AD11 A5 E2                    ..
	sta     $D409                           ; AD13 8D 09 D4                 ...
	lda     $E7                             ; AD16 A5 E7                    ..
LAD18:  sta     $D01A                           ; AD18 8D 1A D0                 ...
	lda     $E5                             ; AD1B A5 E5                    ..
	sta     $D018                           ; AD1D 8D 18 D0                 ...
	lda     $E4                             ; AD20 A5 E4                    ..
	sta     $D017                           ; AD22 8D 17 D0                 ...
	jsr     sub_ACB2
	pla                                     ; AD28 68                       h
	tay                                     ; AD29 A8                       .
	pla                                     ; AD2A 68                       h
	tax                                     ; AD2B AA                       .
	pla                                     ; AD2C 68                       h
	rti                                     ; AD2D 40                       @

; ----------------------------------------------------------------------------

LAD2E:
	yldi	$021B, $00
	ldi	$021A, $1E
	ldy     #$01                            ; AD38 A0 01                    ..
	lda     $D5                             ; AD3A A5 D5                    ..
	beq     LAD44                           ; AD3C F0 06                    ..
	dec     $D5                             ; AD3E C6 D5                    ..
	bne     LAD44                           ; AD40 D0 02                    ..
	sty     $D8                             ; AD42 84 D8                    ..
LAD44:  lda     $D6                             ; AD44 A5 D6                    ..
	beq     LAD4E                           ; AD46 F0 06                    ..
	dec     $D6                             ; AD48 C6 D6                    ..
	bne     LAD4E                           ; AD4A D0 02                    ..
	sty     $D9                             ; AD4C 84 D9                    ..
LAD4E:  lda     $D7                             ; AD4E A5 D7                    ..
	beq     LAD58                           ; AD50 F0 06                    ..
	dec     $D7                             ; AD52 C6 D7                    ..
	bne     LAD58                           ; AD54 D0 02                    ..
	sty     $DA                             ; AD56 84 DA                    ..
LAD58:  rts                                     ; AD58 60                       `

	.include "sub-ad59.asm"
	.include "sub-ad85.asm"
	.include "sub-adea.asm"
	.include "sub-ae81.asm"
	.include "sub-aee4.asm"
	.include "sub-af3a.asm"
	.include "initad.asm"

