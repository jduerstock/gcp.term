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
CDTMA2		:= $0228
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
ICCOM		:= $0342
L05C0		:= $05C0
L3272           := $3272
L4253           := $4253
LB118		:= $B118
LB14A		:= $B14A
LB16A		:= $B16A
LB16C		:= $B16C
LB1C6		:= $B1C6
LB1C8		:= $B1C8
LB1C9		:= $B1C9
LB223		:= $B223
LB224		:= $B224
LB380		:= $B380
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

.macro addi16m8 a1, a2, a3
	clc
        lda     #<a2
        adc     a3
        sta     a1
        lda     #>a2
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

.macro	proc8i	p1, a1
	lda	#a1
	jsr	p1
.endmacro

.macro	func8_8i f1, a1, i1
	lda     #i1
	jsr     f1
	mv	a1, $A0
.endmacro

.macro	func8_8 f1, a1, a2
	proc8	f1, a2
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

Clos:
	ldx     #$FF                            ; 4349 A2 FF                    ..
	stx     $A6                             ; 434B 86 A6                    ..
	ldy     #$0C                            ; 434D A0 0C                    ..
L434F:  bne     L435B                           ; 434F D0 0A                    ..

Output:  
	sty     $A6                             ; 4351 84 A6                    ..
	ldy     #$0B                            ; 4353 A0 0B                    ..
	bne     L435B                           ; 4355 D0 04                    ..

In:
	sty     $A6                             ; 4357 84 A6                    ..
	ldy     #$05                            ; 4359 A0 05                    ..
L435B:  stx     $A5                             ; 435B 86 A5                    ..
	ldx     #$00                            ; 435D A2 00                    ..
	stx     $A3                             ; 435F 86 A3                    ..

XIOstr:
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
Opn:  
	stx     $A5                             ; 4396 86 A5                    ..
	sty     $A6                             ; 4398 84 A6                    ..
	ldy     #$03                            ; 439A A0 03                    ..
	jmp     XIOstr

; ----------------------------------------------------------------------------
Prt:  
	stx     $A5                             ; 439F 86 A5                    ..
	sty     $A6                             ; 43A1 84 A6                    ..
	ldx     #$00                            ; 43A3 A2 00                    ..
	stx     $A3                             ; 43A5 86 A3                    ..
	ldy     #$09                            ; 43A7 A0 09                    ..
	jsr     XIOstr
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
LShift:
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
RShift:
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
SetSign:  
	ldy     $D3                             ; 43EF A4 D3                    ..
	bpl     L4403                           ; 43F1 10 10                    ..

SS1:
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
SMOps:
	stx     $D3                             ; 4404 86 D3                    ..
	cpx     #$00                            ; 4406 E0 00                    ..
	bpl     L440D                           ; 4408 10 03                    ..
	jsr     SS1
L440D:  sta     $82                             ; 440D 85 82                    ..
	stx     $83                             ; 440F 86 83                    ..
	lda     $85                             ; 4411 A5 85                    ..
	bpl     L4423                           ; 4413 10 0E                    ..
	tax                                     ; 4415 AA                       .
	eor     $D3                             ; 4416 45 D3                    E.
	sta     $D3                             ; 4418 85 D3                    ..
	lda     $84                             ; 441A A5 84                    ..
	jsr     SS1
	sta     $84                             ; 441F 85 84                    ..
	stx     $85                             ; 4421 86 85                    ..
L4423:  ldi	$87, $00
	rts                                     ; 4427 60                       `

; ----------------------------------------------------------------------------
MultB:  
	beq     L4445                           ; 4428 F0 1B                    ..
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
MultI:
	jsr     SMOps
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
	jsr     MultB
	lda     $83                             ; 4475 A5 83                    ..
	ldx     $84                             ; 4477 A6 84                    ..
	jsr     MultB
	jmp     SetSign

; ----------------------------------------------------------------------------
DivI:  
	jsr     SMOps
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
	jmp     SetSign

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
	jmp     SetSign

; ----------------------------------------------------------------------------
	jsr     DivI
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
ChkErr:
	bpl     L452D                           ; 4515 10 16                    ..
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
Open:  
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
	jsr     Opn
	jmp     ChkErr

; ----------------------------------------------------------------------------
	jsr     Prt
	jmp     ChkErr

; ----------------------------------------------------------------------------
Close:
	jsr     Clos
	jmp     ChkErr

; ----------------------------------------------------------------------------
	jsr     Output
	jmp     ChkErr

; ----------------------------------------------------------------------------
sub_4575:  
	jsr     In
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
	jsr     sub_4575
	jmp     ChkErr

; ----------------------------------------------------------------------------
sub_45A3:
	ldx     #$07                            ; get binary record
L45A5:  stx     $A4                             ; 45A5 86 A4                    ..
	asl     a                               ; 45A7 0A                       .
	asl     a                               ; 45A8 0A                       .
	asl     a                               ; 45A9 0A                       .
	asl     a                               ; 45AA 0A                       .
	tax                                     ; 45AB AA                       .
	lda     $A4                             ; 45AC A5 A4                    ..
	sta     ICCOM,x                         ; 45AE 9D 42 03                 .B.
	lda     #$00                            ; 45B1 A9 00                    ..
	sta     $0348,x                         ; 45B3 9D 48 03                 .H.
	sta     $0349,x                         ; 45B6 9D 49 03                 .I.
	tya                                     ; 45B9 98                       .
	jsr     CIOV
	sta     $A0                             ; 45BD 85 A0                    ..
	jmp     ChkErr

; ----------------------------------------------------------------------------
	lda     #$9B                            ; 45C2 A9 9B                    ..

sub_45C4:
	tax                                     ; 45C4 AA                       .
	lda     $B7                             ; 45C5 A5 B7                    ..

sub_45C7:
;--	void sub_45c7(uint8_t iocb,chr)		; put character
	stx     $A1                             ; 45C7 86 A1                    ..
	ldy     $A1                             ; 45C9 A4 A1                    ..
	ldx     #$0B				; put buffer
	jmp     L45A5                           ; 45CD 4C A5 45                 L.E

; ----------------------------------------------------------------------------
XIO:
	jsr     XIOstr
	jmp     ChkErr

; ----------------------------------------------------------------------------
read_stick:
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
bzero:
	pha                                     ; 45F6 48                       H
	ldi	$A4, $00
	pla                                     ; 45FB 68                       h

memset:
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

.macro	blkmv_mm16 s1, d1, c1
	mv	$A3, d1+1
	mv	$A5, c1+1
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
L4678:	.addr	L4676
L467A:	.res	40, $00
L46A2:	.addr	L467A
	.byte	$1F
	.byte	$B1
L46A6:	.res	60, $00
L46E2:	.addr	L46A6
	.byte	$20
	.byte   $20
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
L46F6:	.byte	$B0,$14
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
	.byte	$20,$41,$54
	eor     ($53,x)                         ; 471D 41 53                    AS
	.byte   $43                             ; 471F 43                       C
	eor     #$49                            ; 4720 49 49                    II
	.byte	$20,$63,$6F
	.byte   $64                             ; 4725 64                       d
	adc     $20                             ; 4726 65 20                    e 
	.byte   $74                             ; 4728 74                       t
	.byte   $6F                             ; 4729 6F                       o
	.byte	$20,$41,$53
	.byte   $43                             ; 472D 43                       C
	eor     #$49                            ; 472E 49 49                    II
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
	ldi	$A1, $FF
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
L49C5:	.byte	$D6,$1E
	.byte   $0C                             ; 49C7 0C                       .
	.byte	$00
	.byte	$41,$54
	.byte	$41,$52
	.byte	$49,$D8
	.byte	$30,$0C
L49D1:	.byte	$0F,$00

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
	lda     #<LB380
	adc     L499F                           ; 49F1 6D 9F 49                 m.I
	sta     L49D1                           ; 49F4 8D D1 49                 ..I
	lda     #>LB380
	adc     #$00                            ; 49F9 69 00                    i.
	sta     L49D1+1
	blkmv_imi L49C5, L49D1, $000C
	ldi	$A3, $00
	ldy     #$0C                            ; 4A19 A0 0C                    ..
	ldxa	L49D1
	jsr     bzero
	lda     L49C4                           ; 4A24 AD C4 49                 ..I
	sta     L499F                           ; 4A27 8D 9F 49                 ..I
	clc                                     ; 4A2A 18                       .
	lda     #<LB380
	adc     L499F                           ; 4A2D 6D 9F 49                 m.I
	sta     $A0                             ; 4A30 85 A0                    ..
	lda     #>LB380
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
sub_4A53:
	stack_prolog L4A4E, $04
	ldi	$A3, $00
	ldy     #$80                            ; 4A60 A0 80                    ..
	ldxai	LB380
	jsr     bzero
	clc                                     ; 4A69 18                       .
	lda     #<LB380
	adc     L4A52                           ; 4A6C 6D 52 4A                 mRJ
	sta     $A0                             ; 4A6F 85 A0                    ..
	lda     #>LB380
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

sub_4AA5:  
	and     #$0F                            ; 4AA5 29 0F                    ).
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
	ldi	$A3, $00
	ldi	$A4, $00
	rdldi	$A5, L4AA1
	ldy     #$0D                            ; 4AFC A0 0D                    ..
	ldx     #$00                            ; 4AFE A2 00                    ..
	lda     L4AE5                           ; 4B00 AD E5 4A                 ..J
	jsr     XIO
	rts                                     ; 4B06 60                       `

; ----------------------------------------------------------------------------
modem_status:  
	prolog
	lda     #$02                            ; 4B0A A9 02                    ..
	jsr     sub_4AE6
	mv	$A0, DVSTAT+1
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
	lda     LB118,x                         ; 4B5C BD 18 B1                 ...
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
	sta     off_AE
	stx     off_AE+1
L4B9B:  dey                                     ; 4B9B 88                       .
	bmi     L4BA2                           ; 4B9C 30 04                    0.
	lda     (off_AE),y
	beq     L4B9B                           ; 4BA0 F0 F9                    ..
L4BA2:  iny                                     ; 4BA2 C8                       .
	tya                                     ; 4BA3 98                       .
	sta     $A0                             ; 4BA4 85 A0                    ..
	rts                                     ; 4BA6 60                       `

; ----------------------------------------------------------------------------
cmd_2a:  
	tax                                     ; 4BA7 AA                       .
	lda     LB14A,x                         ; 4BA8 BD 4A B1                 .J.
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
	sta     $A0
	and     #$60
	sta     $A2				; $A2 = $A0 & $60
	asl     a
	sta     $A3                             ; $A3 = $A2 << 1
	eor     $A2				; $A1 = ($A3 ^ $A2 ^ $FF & $40) >> 1
	eor     #$FF
	and     #$40
	lsr     a
	sta     $A1				; $A1 = ($A3 ^ $A2 ^ $FF & $40) >> 1
	lda     $A0
	and     #$9F
	sta     $A0				; $A0 = $A0 & $9F
	lda     $A3
	and     #$40
	ora     $A0
	ora     $A1
	sta     $A0				; $A0 = $A3 & $40 | $A0 | $A1
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

	.include "sub-4cf5.asm"
	.include "sub-4e4a.asm"
	.include "sub-4eb1.asm"
	.include "cmd-07.asm"
	.include "sub-4f6d.asm"
	.include "cmd-25.asm"
	.include "sub-4fc5.asm"
	.include "sub-5197.asm"
	.include "sub-51f7.asm"
	.include "sub-5274.asm"
	.include "sub-52e1.asm"
	.include "sub-532a.asm"
	.include "sub-5373.asm"
	.include "sub-537f.asm"
	.include "sub-5388.asm"
	.include "sub-5394.asm"
	.include "sub-5461.asm"
	.include "sub-54ff.asm"
	.include "sub-55a0.asm"
	.include "sub-58f2.asm"
	.include "sub-5cfc.asm"
	.include "sub-5d64.asm"
	.include "sub-5d67.asm"
	.include "cmd-d0.asm"
	.include "cmd-d1.asm"
	.include "sub-5e5e.asm"
	.include "sub-5ec4.asm"
	.include "sub-5edf.asm"
	.include "sub-5efe.asm"
	.include "sub-5f16.asm"
	.include "sub-5ff5.asm"
	.include "sub-6026.asm"
	.include "sub-606e.asm"
	.include "sub-619a.asm"
	.include "sub-6203.asm"
	.include "sub-62d1.asm"
	.include "sub-636e.asm"
	.include "sub-63dd.asm"
	.include "sub-65b0.asm"
	.include "cmd-uw.asm"
	.include "cmd-uf.asm"
	.include "sub-66fc.asm"
	.include "cmd-ud.asm"
	.include "sub-696a.asm"
	.include "cmd-uk.asm"
	.include "cmd-uc.asm"
	.include "cmd-ub.asm"
	.include "cmd-lx.asm"
	.include "cmd-ly.asm"
	.include "cmd-lz.asm"
	.include "sub-7035.asm"
	.include "cmd-la.asm"
	.include "cmd-lb.asm"
	.include "cmd-lf.asm"
	.include "sub-71b5.asm"
	.include "sub-72b1.asm"
	.include "sub-7368.asm"
	.include "sub-73da.asm"
	.include "sub-747d.asm"
	.include "cmd-lc.asm"
	.include "cmd-lm.asm"
	.include "cmd-ll.asm"
	.include "sub-768a.asm"
	.include "cmd-ls.asm"
	.include "cmd-lp.asm"
	.include "cmd-ld.asm"
	.include "sub-799b.asm"
	.include "cmd-uu.asm"
	.include "cmd-un.asm"
	.include "cmd-uv.asm"
	.include "cmd-ux.asm"
	.include "cmd-uy.asm"
	.include "sub-7e24.asm"
	.include "sub-7f80.asm"
	.include "sub-7f93.asm"
	.include "cmd-ul.asm"
	.include "cmd-up.asm"
	.include "sub-8020.asm"
	.include "sub-8047.asm"
	.include "cmd-ur.asm"
	.include "cmd-um.asm"
	.include "sub-81f2.asm"
	.include "sub-840c.asm"
	.include "sub-8521.asm"
	.include "sub-8573.asm"
	.include "sub-884c.asm"
	.include "sub-89ae.asm"
	.include "cmd-26.asm"
	.include "cmd-uz.asm"
	.include "cmd-ua.asm"
	.include "cmd-ug.asm"
	.include "cmd-uo.asm"
	.include "sub-8f7d.asm"
	.include "global2.asm"
	.include "sub-907d.asm"
	.include "sub-90ce.asm"
	.include "sub-9146.asm"
	.include "sub-925d.asm"
	.include "sub-936a.asm"
	.include "sub-9427.asm"
	.include "sub-961e.asm"
	.include "cmd-le.asm"
	.include "cmd-ui.asm"
	.include "cmd-us.asm"
	.include "cmd-lj.asm"
	.include "cmd-uj.asm"
	.include "cmd-ut.asm"
	.include "sub-9cd0.asm"
	.include "sub-9dcb.asm"
	.include "cmd-2b.asm"
	.include "sub-a027.asm"
	.include "sub-a28d.asm"
	.include "cmd-d2.asm"
	.include "cmd-d3.asm"
	.include "cmd-d4.asm"
	.include "cmd-d5.asm"
	.include "cmd-d9.asm"
	.include "cmd-d6.asm"
	.include "cmd-d7.asm"
	.include "cmd-d8.asm"
	.include "cmd-lu.asm"
	.include "cmd-lv.asm"
	.include "cmd-lw.asm"
	.include "cmd-3d.asm"
	.include "cmd-2e.asm"
	.include "cmd-24.asm"
	.include "cmd-40.asm"
	.include "cmd-23.asm"
	.include "cmd-3a.asm"
	.include "cmd-ln.asm"
	.include "sub-a9a3.asm"
	.include "cmd-ue.asm"
	.include "cmd-uh.asm"
	.include "cmd-li.asm"
	.include "sub-ab6a.asm"
	.include "sub-ac7e.asm"
	.include "sub-acb2.asm"
	.include "sub-ad07.asm"
	.include "sub-ad2e.asm"
	.include "sub-ad59.asm"
	.include "sub-ad85.asm"
	.include "sub-adea.asm"
	.include "sub-ae81.asm"
	.include "sub-aee4.asm"
	.include "sub-af3a.asm"
	.include "initad.asm"

