; da65 V2.15 - Git b3d84d5
; Created:    2016-05-13 20:47:02
; Input file: ../TERM.COM
; Page:       1


	.setcpu "6502"

; ----------------------------------------------------------------------------
L000C           := $000C
L0069           := $0069
L0078           := $0078
off_82		:= $0082
off_84		:= $0084
off_AC		:= $00AC
off_AE		:= $00AE
VDSLST		:= $0200
CDTMF3		:= $022A
DVSTAT		:= $02EA
CRSINH		:= $02F0
L0A94           := $0A94
L0ABB           := $0ABB
L0AC1           := $0AC1
L0AC7           := $0AC7
L3272           := $3272
L3C20           := $3C20
L4253           := $4253
HPOSP3		:= $D003
CIOV            := $E456
SETVBV          := $E45C
LE45F           := $E45F
LE59E           := $E59E
LE975           := $E975
LEAB3           := $EAB3
LEB43           := $EB43
LEBC0           := $EBC0
LEC43           := $EC43
LEC53           := $EC53
LED93           := $ED93
LEF40           := $EF40
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

.macro add8i a1, a2, a3
	clc
        lda     a2
        adc     #a3
        sta     a1
.endmacro

.macro ldxa a1
	ldx	a1+1
	lda	a1
.endmacro

.macro ldxai a1
	ldx	#>a1
	lda	#<a1
.endmacro

	.segment "HDR00"

	.word	$FFFF
	.addr	L4327
	.addr	$B0BF

	.segment "SEG00"

L4327:
	.byte	$60
	.byte	" (c)1983 Action Computer Services"

L4349:  ldx     #$FF                            ; 4349 A2 FF                    ..
	stx     $A6                             ; 434B 86 A6                    ..
	ldy     #$0C                            ; 434D A0 0C                    ..
L434F:  bne     L435B                           ; 434F D0 0A                    ..
L4351:  sty     $A6                             ; 4351 84 A6                    ..
	ldy     #$0B                            ; 4353 A0 0B                    ..
	bne     L435B                           ; 4355 D0 04                    ..
L4357:  sty     $A6                             ; 4357 84 A6                    ..
	ldy     #$05                            ; 4359 A0 05                    ..
L435B:  stx     $A5                             ; 435B 86 A5                    ..
	ldx     #$00                            ; 435D A2 00                    ..
	stx     $A3                             ; 435F 86 A3                    ..
L4361:  asl     a                               ; 4361 0A                       .
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
L4396:  stx     $A5                             ; 4396 86 A5                    ..
	sty     $A6                             ; 4398 84 A6                    ..
	ldy     #$03                            ; 439A A0 03                    ..
	jmp     L4361                           ; 439C 4C 61 43                 LaC

; ----------------------------------------------------------------------------
L439F:  stx     $A5                             ; 439F 86 A5                    ..
	sty     $A6                             ; 43A1 84 A6                    ..
	ldx     #$00                            ; 43A3 A2 00                    ..
	stx     $A3                             ; 43A5 86 A3                    ..
	ldy     #$09                            ; 43A7 A0 09                    ..
	jsr     L4361                           ; 43A9 20 61 43                  aC
	bne     L43B8                           ; 43AC D0 0A                    ..
	lda     #$0B                            ; 43AE A9 0B                    ..
	sta     $0342,x                         ; 43B0 9D 42 03                 .B.
	lda     #$9B                            ; 43B3 A9 9B                    ..
	jmp     CIOV

; ----------------------------------------------------------------------------
L43B8:  rts                                     ; 43B8 60                       `

; ----------------------------------------------------------------------------
L43B9:	.byte   $46                             ; 43B9 46                       F
L43BA:	.byte   $4C                             ; 43BA 4C                       L
L43BB:  .word   L43BD                           ; 43BB BD                       .

L43BD:	sta     L43B9                           ; 43BD 8D B9 43                 ..C
	jmp     ($0A)                           ; 43C0 6C 0A 00                 l..

; ----------------------------------------------------------------------------
	.byte   $13                             ; 43C3 13                       .
	ora     ($01),y                         ; 43C4 11 01                    ..

	.byte   $83                             ; 43C6 83                       .
L43C7:  tsx                                     ; 43C7 BA                       .
	stx     $04C1                           ; 43C8 8E C1 04                 ...
	ldy     #$80                            ; 43CB A0 80                    ..
	tya                                     ; 43CD 98                       .
	jmp     L43BA                           ; 43CE 4C BA 43                 L.C

; ----------------------------------------------------------------------------
L43D1:  ldy     $84                             ; 43D1 A4 84                    ..
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
L43F3:  sta     $86                             ; 43F3 85 86                    ..
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
	jsr     L43F3                           ; 440A 20 F3 43                  .C
L440D:  sta     $82                             ; 440D 85 82                    ..
	stx     $83                             ; 440F 86 83                    ..
	lda     $85                             ; 4411 A5 85                    ..
	bpl     L4423                           ; 4413 10 0E                    ..
	tax                                     ; 4415 AA                       .
	eor     $D3                             ; 4416 45 D3                    E.
	sta     $D3                             ; 4418 85 D3                    ..
	lda     $84                             ; 441A A5 84                    ..
	jsr     L43F3                           ; 441C 20 F3 43                  .C
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
	.byte   $90                             ; 4439 90                       .
L443A:  .byte   $02                             ; 443A 02                       .
L443B:  adc     $C7                             ; 443B 65 C7                    e.
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
L447F:  jsr     sub_4404
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
	jsr     L447F                           ; 44CD 20 7F 44                  .D
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
	jmp     L43C7                           ; 4507 4C C7 43                 L.C

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
	beq     L4531                           ; 451E F0 11                    ..
	jmp     L43BA                           ; 4520 4C BA 43                 L.C

; ----------------------------------------------------------------------------
L4523:  txa                                     ; 4523 8A                       .
	lsr     a                               ; 4524 4A                       J
	lsr     a                               ; 4525 4A                       J
	lsr     a                               ; 4526 4A                       J
	lsr     a                               ; 4527 4A                       J
	tax                                     ; 4528 AA                       .
	tya                                     ; 4529 98                       .
	sta     $05C0,x                         ; 452A 9D C0 05                 ...
L452D:  rts                                     ; 452D 60                       `

; ----------------------------------------------------------------------------
	ldx     #$01                            ; 452E A2 01                    ..
	.byte   $86                             ; 4530 86                       .
L4531:  ora     ($48),y                         ; 4531 11 48                    .H
	jsr     L43C7                           ; 4533 20 C7 43                  .C
	pla                                     ; 4536 68                       h
	tay                                     ; 4537 A8                       .
	rts                                     ; 4538 60                       `

; ----------------------------------------------------------------------------
L4539:  pha                                     ; 4539 48                       H
	stx     $A1                             ; 453A 86 A1                    ..
	sty     $A2                             ; 453C 84 A2                    ..
	tay                                     ; 453E A8                       .
	lda     #$00                            ; 453F A9 00                    ..
	sta     $05C0,y                         ; 4541 99 C0 05                 ...
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
	jsr     L4396                           ; 455D 20 96 43                  .C
	jmp     L4515                           ; 4560 4C 15 45                 L.E

; ----------------------------------------------------------------------------
	jsr     L439F                           ; 4563 20 9F 43                  .C
	jmp     L4515                           ; 4566 4C 15 45                 L.E

; ----------------------------------------------------------------------------
L4569:  jsr     L4349                           ; 4569 20 49 43                  IC
	jmp     L4515                           ; 456C 4C 15 45                 L.E

; ----------------------------------------------------------------------------
	jsr     L4351                           ; 456F 20 51 43                  QC
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
	lda     #$FF                            ; 458A A9 FF                    ..
	sta     $A3                             ; 458C 85 A3                    ..
	pla                                     ; 458E 68                       h
L458F:  pha                                     ; 458F 48                       H
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
L45A3:  ldx     #$07                            ; 45A3 A2 07                    ..
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
L45C4:  tax                                     ; 45C4 AA                       .
	lda     $B7                             ; 45C5 A5 B7                    ..
L45C7:  stx     $A1                             ; 45C7 86 A1                    ..
	ldy     $A1                             ; 45C9 A4 A1                    ..
	ldx     #$0B                            ; 45CB A2 0B                    ..
	jmp     L45A5                           ; 45CD 4C A5 45                 L.E

; ----------------------------------------------------------------------------
sub_45D0:  	
	jsr     L4361                           ; 45D0 20 61 43                  aC
	jmp     L4515                           ; 45D3 4C 15 45                 L.E

; ----------------------------------------------------------------------------
L45D6:  ldx     #$00                            ; 45D6 A2 00                    ..
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
L45EF:  tax                                     ; 45EF AA                       .
	lda     $D010,x                         ; 45F0 BD 10 D0                 ...
	sta     $A0                             ; 45F3 85 A0                    ..
	rts                                     ; 45F5 60                       `

; ----------------------------------------------------------------------------
L45F6:  pha                                     ; 45F6 48                       H
	ldi	$A4, $00
	pla                                     ; 45FB 68                       h
L45FC:  sta     $A0                             ; 45FC 85 A0                    ..
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
sub_461F:  
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
	.byte   $2D                             ; 4646 2D                       -
L4647:  .byte   $FF                             ; 4647 FF                       .
L4648:  .byte   $FF                             ; 4648 FF                       .
L4649:  brk                                     ; 4649 00                       .
L464A:  .byte   $31                             ; 464A 31                       1
L464B:  brk                                     ; 464B 00                       .
	brk                                     ; 464C 00                       .
L464D:  brk                                     ; 464D 00                       .
L464E:  brk                                     ; 464E 00                       .
L464F:  brk                                     ; 464F 00                       .
L4650:  brk                                     ; 4650 00                       .
L4651:  .byte   $01                             ; 4651 01                       .
L4652:  .byte   $02                             ; 4652 02                       .
L4653:  brk                                     ; 4653 00                       .
L4654:  brk                                     ; 4654 00                       .
	brk                                     ; 4655 00                       .
L4656:  brk                                     ; 4656 00                       .
L4657:  brk                                     ; 4657 00                       .
L4658:  brk                                     ; 4658 00                       .
L4659:  brk                                     ; 4659 00                       .
	brk                                     ; 465A 00                       .
	.byte   $14                             ; 465B 14                       .
	brk                                     ; 465C 00                       .
	.byte	">>> UTIL.ACT  <<"
L466D:  .byte   $3C                             ; 466D 3C                       <
L466E:  .byte   $3C                             ; 466E 3C                       <
L466F:	.addr	$B0D4
	.byte	$04
	brk                                     ; 4672 00                       .
L4673:  brk                                     ; 4673 00                       .
L4674:  brk                                     ; 4674 00                       .
L4675:  .byte   $B4                             ; 4675 B4                       .
L4676:  cpx     #$E0                            ; 4676 E0 E0                    ..
L4678:  .byte   $76                             ; 4678 76                       v
L4679:  lsr     $00                             ; 4679 46 00                    F.
	brk                                     ; 467B 00                       .
	brk                                     ; 467C 00                       .
	brk                                     ; 467D 00                       .
	brk                                     ; 467E 00                       .
	brk                                     ; 467F 00                       .
	brk                                     ; 4680 00                       .
	brk                                     ; 4681 00                       .
	brk                                     ; 4682 00                       .
	brk                                     ; 4683 00                       .
	brk                                     ; 4684 00                       .
	brk                                     ; 4685 00                       .
	brk                                     ; 4686 00                       .
	brk                                     ; 4687 00                       .
	brk                                     ; 4688 00                       .
	brk                                     ; 4689 00                       .
	brk                                     ; 468A 00                       .
	brk                                     ; 468B 00                       .
	brk                                     ; 468C 00                       .
	brk                                     ; 468D 00                       .
	brk                                     ; 468E 00                       .
	brk                                     ; 468F 00                       .
	brk                                     ; 4690 00                       .
	brk                                     ; 4691 00                       .
	brk                                     ; 4692 00                       .
	brk                                     ; 4693 00                       .
	brk                                     ; 4694 00                       .
	brk                                     ; 4695 00                       .
	brk                                     ; 4696 00                       .
	brk                                     ; 4697 00                       .
	brk                                     ; 4698 00                       .
	brk                                     ; 4699 00                       .
	brk                                     ; 469A 00                       .
	brk                                     ; 469B 00                       .
	brk                                     ; 469C 00                       .
	brk                                     ; 469D 00                       .
	brk                                     ; 469E 00                       .
	brk                                     ; 469F 00                       .
	brk                                     ; 46A0 00                       .
	brk                                     ; 46A1 00                       .
L46A2:  .byte   $7A                             ; 46A2 7A                       z
L46A3:  lsr     $1F                             ; 46A3 46 1F                    F.
	lda     ($00),y                         ; 46A5 B1 00                    ..
	brk                                     ; 46A7 00                       .
	brk                                     ; 46A8 00                       .
	brk                                     ; 46A9 00                       .
	brk                                     ; 46AA 00                       .
	brk                                     ; 46AB 00                       .
	brk                                     ; 46AC 00                       .
	brk                                     ; 46AD 00                       .
	brk                                     ; 46AE 00                       .
	brk                                     ; 46AF 00                       .
	brk                                     ; 46B0 00                       .
	brk                                     ; 46B1 00                       .
	brk                                     ; 46B2 00                       .
	brk                                     ; 46B3 00                       .
	brk                                     ; 46B4 00                       .
	brk                                     ; 46B5 00                       .
	brk                                     ; 46B6 00                       .
	brk                                     ; 46B7 00                       .
	brk                                     ; 46B8 00                       .
	brk                                     ; 46B9 00                       .
	brk                                     ; 46BA 00                       .
	brk                                     ; 46BB 00                       .
	brk                                     ; 46BC 00                       .
	brk                                     ; 46BD 00                       .
	brk                                     ; 46BE 00                       .
	brk                                     ; 46BF 00                       .
	brk                                     ; 46C0 00                       .
	brk                                     ; 46C1 00                       .
	brk                                     ; 46C2 00                       .
	brk                                     ; 46C3 00                       .
	brk                                     ; 46C4 00                       .
	brk                                     ; 46C5 00                       .
	brk                                     ; 46C6 00                       .
	brk                                     ; 46C7 00                       .
	brk                                     ; 46C8 00                       .
	brk                                     ; 46C9 00                       .
	brk                                     ; 46CA 00                       .
	brk                                     ; 46CB 00                       .
	brk                                     ; 46CC 00                       .
	brk                                     ; 46CD 00                       .
	brk                                     ; 46CE 00                       .
	brk                                     ; 46CF 00                       .
	brk                                     ; 46D0 00                       .
	brk                                     ; 46D1 00                       .
	brk                                     ; 46D2 00                       .
	brk                                     ; 46D3 00                       .
	brk                                     ; 46D4 00                       .
	brk                                     ; 46D5 00                       .
	brk                                     ; 46D6 00                       .
	brk                                     ; 46D7 00                       .
	brk                                     ; 46D8 00                       .
	brk                                     ; 46D9 00                       .
	brk                                     ; 46DA 00                       .
	brk                                     ; 46DB 00                       .
	brk                                     ; 46DC 00                       .
	brk                                     ; 46DD 00                       .
	brk                                     ; 46DE 00                       .
	brk                                     ; 46DF 00                       .
	brk                                     ; 46E0 00                       .
	brk                                     ; 46E1 00                       .
L46E2:  .byte   $A6                             ; 46E2 A6                       .
L46E3:  lsr     $20                             ; 46E3 46 20                    F 
	.byte   $20                             ; 46E5 20                        
L46E6:  .byte   $50                             ; 46E6 50                       P
L46E7:  .byte   $1E                             ; 46E7 1E                       .
L46E8:  .byte   $10                             ; 46E8 10                       .
L46E9:  brk                                     ; 46E9 00                       .
L46EA:  .byte   $54                             ; 46EA 54                       T
L46EB:  .word   $416F                             ; 46EB 6F                       o
L46ED:  .byte   $73                             ; 46ED 73                       s
L46EE:  .byte   $63                             ; 46EE 63                       c
L46EF:  brk                                     ; 46EF 00                       .
	brk                                     ; 46F0 00                       .
	brk                                     ; 46F1 00                       .
	brk                                     ; 46F2 00                       .
	brk                                     ; 46F3 00                       .
	brk                                     ; 46F4 00                       .
L46F5:  .byte   $C0                             ; 46F5 C0                       .
L46F6:  ;bcs     $470C                           ; 46F6 B0 14                    ..
	.byte	$B0,$14
	brk                                     ; 46F8 00                       .
	dec     $1E,x                           ; 46F9 D6 1E                    ..
	;bvc     L46FD                           ; 46FB 50 00                    P.
	.byte	$50,$00
	.byte   $52                             ; 46FD 52                       R
	eor     #$50                            ; 46FE 49 50                    IP
	.byte   $54                             ; 4700 54                       T
	eor     #$4F                            ; 4701 49 4F                    IO
	lsr     L443A                           ; 4703 4E 3A 44                 N:D
	.byte   $1F                             ; 4706 1F                       .
	.byte   $4F                             ; 4707 4F                       O
	brk                                     ; 4708 00                       .
	brk                                     ; 4709 00                       .
	brk                                     ; 470A 00                       .
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
	jsr     L6F63                           ; 4722 20 63 6F                  co
	.byte   $64                             ; 4725 64                       d
	adc     $20                             ; 4726 65 20                    e 
	.byte   $74                             ; 4728 74                       t
	.byte   $6F                             ; 4729 6F                       o
	;jsr     L5341                           ; 472A 20 41 53                  AS
	.byte	$20,$41,$53
	.byte   $43                             ; 472D 43                       C
	eor     #$49                            ; 472E 49 49                    II
	jsr     L6176                           ; 4730 20 76 61                  va
	;jmp     (L6575)                         ; 4733 6C 75 65                 lue
	.byte	$6C,$75,$65

; ----------------------------------------------------------------------------
	.byte   $44                             ; 4736 44                       D
	.byte   $1F                             ; 4737 1F                       .
	asl     a:$00,x                         ; 4738 1E 00 00                 ...
	brk                                     ; 473B 00                       .
	brk                                     ; 473C 00                       .
	brk                                     ; 473D 00                       .
	brk                                     ; 473E 00                       .
	brk                                     ; 473F 00                       .
	brk                                     ; 4740 00                       .
	.byte   $44                             ; 4741 44                       D
	.byte   $1F                             ; 4742 1F                       .
	.byte   $13                             ; 4743 13                       .
	brk                                     ; 4744 00                       .
	.byte   $A7                             ; 4745 A7                       .
	.byte   $04                             ; 4746 04                       .
	.byte   $0C                             ; 4747 0C                       .
	.byte   $20                             ; 4748 20                        

sub_4749:  
	jmp	L474C

L474C:  brk                                     ; 474C 00                       .
L474D:  brk                                     ; 474D 00                       .
L474E:  brk                                     ; 474E 00                       .
L474F:  brk                                     ; 474F 00                       .
L4750:  brk                                     ; 4750 00                       .
L4751:  plp                                     ; 4751 28                       (
L4752:  .byte   $32                             ; 4752 32                       2
L4753:  .byte   $29                             ; 4753 29                       )
L4754:  cld                                     ; 4754 D8                       .
	dec     $1E,x                           ; 4755 D6 1E                    ..
	ora     a:$00                           ; 4757 0D 00 00                 ...
	ora     (L000C,x)                       ; 475A 01 0C                    ..
	cld                                     ; 475C D8                       .
	bmi     L47DD                           ; 475D 30 7E                    0~
	ora     ($00),y                         ; 475F 11 00                    ..
	brk                                     ; 4761 00                       .
L4762:  brk                                     ; 4762 00                       .
L4763:  cld                                     ; 4763 D8                       .
L4764:  brk                                     ; 4764 00                       .
	ora     $42                             ; 4765 05 42                    .B
	.byte   $42                             ; 4767 42                       B
	.byte   $42                             ; 4768 42                       B
	.byte   $42                             ; 4769 42                       B
	.byte   $53                             ; 476A 53                       S
	adc     $47                             ; 476B 65 47                    eG
	ora     ($42,x)                         ; 476D 01 42                    .B
	adc     $0547                           ; 476F 6D 47 05                 mG.
	.byte   $42                             ; 4772 42                       B
	.byte   $42                             ; 4773 42                       B
	.byte   $42                             ; 4774 42                       B
	.byte   $42                             ; 4775 42                       B
	.byte   $53                             ; 4776 53                       S
	adc     ($47),y                         ; 4777 71 47                    qG
	.byte   $02                             ; 4779 02                       .
	.byte   $42                             ; 477A 42                       B
	.byte   $43                             ; 477B 43                       C
	adc     $0647,y                         ; 477C 79 47 06                 yG.
	.byte   $42                             ; 477F 42                       B
	.byte   $42                             ; 4780 42                       B
	.byte   $42                             ; 4781 42                       B
	.byte   $42                             ; 4782 42                       B
	.byte   $42                             ; 4783 42                       B
	.byte   $43                             ; 4784 43                       C
	ror     $0447,x                         ; 4785 7E 47 04                 ~G.
	.byte   $42                             ; 4788 42                       B
	.byte   $44                             ; 4789 44                       D
	.byte   $42                             ; 478A 42                       B
	.byte   $42                             ; 478B 42                       B
	.byte   $87                             ; 478C 87                       .
	.byte   $47                             ; 478D 47                       G
	asl     $42                             ; 478E 06 42                    .B
	.byte   $44                             ; 4790 44                       D
	.byte   $42                             ; 4791 42                       B
	.byte   $42                             ; 4792 42                       B
	.byte   $42                             ; 4793 42                       B
	.byte   $42                             ; 4794 42                       B
	stx     $0247                           ; 4795 8E 47 02                 .G.
	.byte   $44                             ; 4798 44                       D
	.byte   $42                             ; 4799 42                       B
	.byte   $97                             ; 479A 97                       .
	.byte   $47                             ; 479B 47                       G
	ora     ($58,x)                         ; 479C 01 58                    .X
	.byte   $9C                             ; 479E 9C                       .
	.byte   $47                             ; 479F 47                       G
	ora     ($58,x)                         ; 47A0 01 58                    .X
	.byte   $A0                             ; 47A2 A0                       .
L47A3:  .byte   $47                             ; 47A3 47                       G
	.byte   $03                             ; 47A4 03                       .
	.byte   $42                             ; 47A5 42                       B
	.byte   $42                             ; 47A6 42                       B
	.byte   $42                             ; 47A7 42                       B
	ldy     $47                             ; 47A8 A4 47                    .G
	.byte   $04                             ; 47AA 04                       .
	.byte   $42                             ; 47AB 42                       B
	.byte   $42                             ; 47AC 42                       B
	.byte   $42                             ; 47AD 42                       B
	.byte   $42                             ; 47AE 42                       B
	tax                                     ; 47AF AA                       .
	.byte   $47                             ; 47B0 47                       G
	.byte   $03                             ; 47B1 03                       .
	.byte   $42                             ; 47B2 42                       B
	.byte   $42                             ; 47B3 42                       B
	.byte   $42                             ; 47B4 42                       B
	lda     ($47),y                         ; 47B5 B1 47                    .G
	.byte   $03                             ; 47B7 03                       .
	.byte   $42                             ; 47B8 42                       B
	.byte   $42                             ; 47B9 42                       B
	.byte   $42                             ; 47BA 42                       B
	.byte   $B7                             ; 47BB B7                       .
	.byte   $47                             ; 47BC 47                       G
	.byte   $03                             ; 47BD 03                       .
	.byte   $42                             ; 47BE 42                       B
	.byte   $43                             ; 47BF 43                       C
	.byte   $53                             ; 47C0 53                       S
	lda     $0147,x                         ; 47C1 BD 47 01                 .G.
	.byte   $42                             ; 47C4 42                       B
	.byte   $C3                             ; 47C5 C3                       .
	.byte   $47                             ; 47C6 47                       G
	.byte   $03                             ; 47C7 03                       .
	.byte   $42                             ; 47C8 42                       B
	.byte   $42                             ; 47C9 42                       B
	.byte   $42                             ; 47CA 42                       B
	.byte   $C7                             ; 47CB C7                       .
	.byte   $47                             ; 47CC 47                       G
	.byte   $02                             ; 47CD 02                       .
	.byte   $42                             ; 47CE 42                       B
	.byte   $42                             ; 47CF 42                       B
	cmp     $0447                           ; 47D0 CD 47 04                 .G.
	.byte   $42                             ; 47D3 42                       B
	.byte   $42                             ; 47D4 42                       B
	.byte   $42                             ; 47D5 42                       B
	.byte   $42                             ; 47D6 42                       B
	.byte   $D2                             ; 47D7 D2                       .
	.byte   $47                             ; 47D8 47                       G
	.byte   $03                             ; 47D9 03                       .
	.byte   $42                             ; 47DA 42                       B
	.byte   $42                             ; 47DB 42                       B
	.byte   $42                             ; 47DC 42                       B
L47DD:  cmp     $0247,y                         ; 47DD D9 47 02                 .G.
	.byte   $42                             ; 47E0 42                       B
	.byte   $44                             ; 47E1 44                       D
	.byte   $DF                             ; 47E2 DF                       .
	.byte   $47                             ; 47E3 47                       G
	.byte   $02                             ; 47E4 02                       .
	.byte   $42                             ; 47E5 42                       B
	.byte   $52                             ; 47E6 52                       R
	cpx     $47                             ; 47E7 E4 47                    .G
	.byte   $02                             ; 47E9 02                       .
	.byte   $42                             ; 47EA 42                       B
	.byte   $52                             ; 47EB 52                       R
	sbc     #$47                            ; 47EC E9 47                    .G
	.byte   $03                             ; 47EE 03                       .
	.byte   $44                             ; 47EF 44                       D
	.byte   $42                             ; 47F0 42                       B
	.byte   $42                             ; 47F1 42                       B
	inc     $0347                           ; 47F2 EE 47 03                 .G.
	.byte   $44                             ; 47F5 44                       D
	.byte   $42                             ; 47F6 42                       B
	.byte   $42                             ; 47F7 42                       B
	.byte   $F4                             ; 47F8 F4                       .
	.byte   $47                             ; 47F9 47                       G
	ora     ($44,x)                         ; 47FA 01 44                    .D
	.byte   $FA                             ; 47FC FA                       .
	.byte   $47                             ; 47FD 47                       G
	ora     ($44,x)                         ; 47FE 01 44                    .D
	inc     $0147,x                         ; 4800 FE 47 01                 .G.
	.byte   $43                             ; 4803 43                       C
	.byte   $02                             ; 4804 02                       .
	pha                                     ; 4805 48                       H
	ora     ($42,x)                         ; 4806 01 42                    .B
	asl     $48                             ; 4808 06 48                    .H
	.byte   $04                             ; 480A 04                       .
	.byte   $42                             ; 480B 42                       B
	.byte   $42                             ; 480C 42                       B
	eor     ($30,x)                         ; 480D 41 30                    A0
	asl     a                               ; 480F 0A                       .
	pha                                     ; 4810 48                       H
	.byte   $02                             ; 4811 02                       .
	.byte   $44                             ; 4812 44                       D
	.byte   $42                             ; 4813 42                       B
	ora     ($48),y                         ; 4814 11 48                    .H
	.byte   $02                             ; 4816 02                       .
	.byte   $42                             ; 4817 42                       B
	.byte   $42                             ; 4818 42                       B
	asl     $48,x                           ; 4819 16 48                    .H
	.byte   $03                             ; 481B 03                       .
	.byte   $44                             ; 481C 44                       D
	.byte   $44                             ; 481D 44                       D
	.byte   $42                             ; 481E 42                       B
	.byte   $1B                             ; 481F 1B                       .
	pha                                     ; 4820 48                       H
	.byte   $07                             ; 4821 07                       .
	.byte   $44                             ; 4822 44                       D
	.byte   $44                             ; 4823 44                       D
	.byte   $42                             ; 4824 42                       B
	.byte   $42                             ; 4825 42                       B
	.byte   $42                             ; 4826 42                       B
	.byte   $42                             ; 4827 42                       B
	.byte   $42                             ; 4828 42                       B
	and     ($48,x)                         ; 4829 21 48                    !H
	asl     $44                             ; 482B 06 44                    .D
	.byte   $44                             ; 482D 44                       D
	.byte   $44                             ; 482E 44                       D
	.byte   $44                             ; 482F 44                       D
	.byte   $44                             ; 4830 44                       D
	.byte   $44                             ; 4831 44                       D
	.byte   $2B                             ; 4832 2B                       +
	pha                                     ; 4833 48                       H
	.byte   $03                             ; 4834 03                       .
	.byte   $43                             ; 4835 43                       C
	eor     ($38,x)                         ; 4836 41 38                    A8
	.byte   $34                             ; 4838 34                       4
	pha                                     ; 4839 48                       H
	ora     ($44,x)                         ; 483A 01 44                    .D
	.byte   $3A                             ; 483C 3A                       :
	pha                                     ; 483D 48                       H
	brk                                     ; 483E 00                       .
	rol     $0148,x                         ; 483F 3E 48 01                 >H.
	.byte   $44                             ; 4842 44                       D
	eor     ($48,x)                         ; 4843 41 48                    AH
	.byte   $03                             ; 4845 03                       .
	.byte   $42                             ; 4846 42                       B
	.byte   $42                             ; 4847 42                       B
	.byte   $44                             ; 4848 44                       D
	eor     $48                             ; 4849 45 48                    EH
	ora     ($42,x)                         ; 484B 01 42                    .B
	.byte   $4B                             ; 484D 4B                       K
	pha                                     ; 484E 48                       H
L484F:	.byte	$00
	.addr	L484F
L4852:	.byte	$00
	.addr	L4852
L4855:	.byte   $03,"DDs"
L4859:	.addr	L4855
L485B:	.byte	$01,"D"
L485D:	.addr	L485B
L485F:	.byte	$04,"BDDR"
L4864:	.addr	L485F
L4866:	.byte	$03,"BDR"
L486A:	.addr	L4866
L486C:	.byte	$03,"BDs"
L4870:	.addr	L486C
L4872:	.byte	$02,"Ds"
	.addr	L4872
	.byte   $02                             ; 4877 02                       .
	.byte   $44                             ; 4878 44                       D
	.byte   $73                             ; 4879 73                       s
	.byte   $77                             ; 487A 77                       w
	pha                                     ; 487B 48                       H
	.byte   $03                             ; 487C 03                       .
	.byte   $42                             ; 487D 42                       B
	.byte   $44                             ; 487E 44                       D
	.byte   $42                             ; 487F 42                       B
	.byte   $7C                             ; 4880 7C                       |
	pha                                     ; 4881 48                       H
	ora     ($43,x)                         ; 4882 01 43                    .C
	.byte   $82                             ; 4884 82                       .
	pha                                     ; 4885 48                       H
	brk                                     ; 4886 00                       .
	stx     $48                             ; 4887 86 48                    .H
L4889:	.byte	$03,"BBB"
L488D:	.addr	L4889
L488F:	.byte	$01,"B"
L4891:	.addr	L488F
L4893:	.byte	$03,"BBB"
L4897:	.addr	L4893
L4899:	.byte	$02,"CS"
L489C:	.addr	L4899
L489E:	.byte	$01,"B"
L48A0:	.addr	L489E
L48A2:	.byte	$00
L48A3:	.addr	L48A2
	.byte   $02                             ; 48A5 02                       .
	.byte   $42                             ; 48A6 42                       B
	.byte   $42                             ; 48A7 42                       B
	lda     $48                             ; 48A8 A5 48                    .H
	brk                                     ; 48AA 00                       .
	tax                                     ; 48AB AA                       .
	pha                                     ; 48AC 48                       H
L48AD:	.byte   $02                             ; 48AD 02                       .
	.byte   $44                             ; 48AE 44                       D
	.byte   $42                             ; 48AF 42                       B
	.addr	L48AD
L48B2:	.byte	$02,"BB"
	.addr	L48B2
L48B7:	.byte   $02,"BB"
L48BA:	.addr	L48B7
L48BC:	.byte	$01,"B"
L48BE:	.addr	L48BC
	.byte	$4C
L48C1:  .addr	L48C3
L48C3:	.byte	$6B                             ; 48C3 6B                       k
	.byte   $47                             ; 48C4 47                       G
	.byte   $6F                             ; 48C5 6F                       o
	.byte   $47                             ; 48C6 47                       G
	.byte   $77                             ; 48C7 77                       w
	.byte   $47                             ; 48C8 47                       G
	.byte   $7C                             ; 48C9 7C                       |
	.byte   $47                             ; 48CA 47                       G
	sta     $47                             ; 48CB 85 47                    .G
	sty     L9547                           ; 48CD 8C 47 95                 .G.
	.byte   $47                             ; 48D0 47                       G
	txs                                     ; 48D1 9A                       .
	.byte   $47                             ; 48D2 47                       G
	.byte   $9E                             ; 48D3 9E                       .
	.byte   $47                             ; 48D4 47                       G
	ldx     #$47                            ; 48D5 A2 47                    .G
	tay                                     ; 48D7 A8                       .
	.byte   $47                             ; 48D8 47                       G
	.byte   $AF                             ; 48D9 AF                       .
	.byte   $47                             ; 48DA 47                       G
	lda     $47,x                           ; 48DB B5 47                    .G
	.byte   $BB                             ; 48DD BB                       .
	.byte   $47                             ; 48DE 47                       G
	cmp     ($47,x)                         ; 48DF C1 47                    .G
	cmp     $47                             ; 48E1 C5 47                    .G
	.byte   $CB                             ; 48E3 CB                       .
	.byte   $47                             ; 48E4 47                       G
	bne     L492E                           ; 48E5 D0 47                    .G
	.byte   $D7                             ; 48E7 D7                       .
	.byte   $47                             ; 48E8 47                       G
	cmp     $E247,x                         ; 48E9 DD 47 E2                 .G.
	.byte   $47                             ; 48EC 47                       G
	.byte   $E7                             ; 48ED E7                       .
	.byte   $47                             ; 48EE 47                       G
	cpx     $F247                           ; 48EF EC 47 F2                 .G.
	.byte   $47                             ; 48F2 47                       G
	sed                                     ; 48F3 F8                       .
	.byte   $47                             ; 48F4 47                       G
	.byte   $FC                             ; 48F5 FC                       .
	.byte   $47                             ; 48F6 47                       G
	brk                                     ; 48F7 00                       .
	pha                                     ; 48F8 48                       H
	.byte   $04                             ; 48F9 04                       .
	pha                                     ; 48FA 48                       H
	php                                     ; 48FB 08                       .
	pha                                     ; 48FC 48                       H
	.byte   $0F                             ; 48FD 0F                       .
	pha                                     ; 48FE 48                       H
	.byte   $14                             ; 48FF 14                       .
	pha                                     ; 4900 48                       H
	ora     $1F48,y                         ; 4901 19 48 1F                 .H.
	pha                                     ; 4904 48                       H
	and     #$48                            ; 4905 29 48                    )H
	.byte   $32                             ; 4907 32                       2
	pha                                     ; 4908 48                       H
	sec                                     ; 4909 38                       8
	pha                                     ; 490A 48                       H
	.byte   $3C                             ; 490B 3C                       <
	pha                                     ; 490C 48                       H
	.byte   $3F                             ; 490D 3F                       ?
	pha                                     ; 490E 48                       H
	.byte   $43                             ; 490F 43                       C
	pha                                     ; 4910 48                       H
	eor     #$48                            ; 4911 49 48                    IH
	.byte	$4D,$48,$50
	pha                                     ; 4916 48                       H
	.byte   $53                             ; 4917 53                       S
	pha                                     ; 4918 48                       H
	.addr	L4859
	.addr	L485D
	.byte   $64                             ; 491D 64                       d
	pha                                     ; 491E 48                       H
	ror     a                               ; 491F 6A                       j
	pha                                     ; 4920 48                       H
	bvs     L496B                           ; 4921 70 48                    pH
	adc     $48,x                           ; 4923 75 48                    uH
	.byte   $7A                             ; 4925 7A                       z
	pha                                     ; 4926 48                       H
	.byte   $80                             ; 4927 80                       .
	pha                                     ; 4928 48                       H
	sty     $48                             ; 4929 84 48                    .H
	.byte   $87                             ; 492B 87                       .
	pha                                     ; 492C 48                       H
	.byte   $8D                             ; 492D 8D                       .
L492E:  pha                                     ; 492E 48                       H
	sta     ($48),y                         ; 492F 91 48                    .H
	.byte   $97                             ; 4931 97                       .
	pha                                     ; 4932 48                       H
	.byte   $9C                             ; 4933 9C                       .
	pha                                     ; 4934 48                       H
	ldy     #$48                            ; 4935 A0 48                    .H
	.byte   $A3                             ; 4937 A3                       .
	pha                                     ; 4938 48                       H
	tay                                     ; 4939 A8                       .
	pha                                     ; 493A 48                       H
	.byte   $AB                             ; 493B AB                       .
	pha                                     ; 493C 48                       H
	bcs     L4987                           ; 493D B0 48                    .H
	lda     $48,x                           ; 493F B5 48                    .H
	.addr	L48BA
	.byte   $BE                             ; 4943 BE                       .
	pha                                     ; 4944 48                       H
L4945:  sta     $A0                             ; 4945 85 A0                    ..
	tax                                     ; 4947 AA                       .
	lda     #$00                            ; 4948 A9 00                    ..
	sta     $A1                             ; 494A 85 A1                    ..
	cpx     #$80                            ; 494C E0 80                    ..
	bmi     L4954                           ; 494E 30 04                    0.
	lda     #$FF                            ; 4950 A9 FF                    ..
	sta     $A1                             ; 4952 85 A1                    ..
L4954:  rts                                     ; 4954 60                       `

; ----------------------------------------------------------------------------
L4955:  sta     $A0                             ; 4955 85 A0                    ..
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
L496E:  stx     $A0                             ; 496E 86 A0                    ..
	cmp     $A0                             ; 4970 C5 A0                    ..
	bne     L4979                           ; 4972 D0 05                    ..
	lda     #$00                            ; 4974 A9 00                    ..
	sta     $A0                             ; 4976 85 A0                    ..
	rts                                     ; 4978 60                       `

; ----------------------------------------------------------------------------
L4979:  jsr     L4955                           ; 4979 20 55 49                  UI
	lda     $A0                             ; 497C A5 A0                    ..
	eor     #$01                            ; 497E 49 01                    I.
	sta     $A0                             ; 4980 85 A0                    ..
	rts                                     ; 4982 60                       `

; ----------------------------------------------------------------------------
L4983:  tay                                     ; 4983 A8                       .
	jsr     L496E                           ; 4984 20 6E 49                  nI
L4987:  lda     $A0                             ; 4987 A5 A0                    ..
	beq     L498D                           ; 4989 F0 02                    ..
	tya                                     ; 498B 98                       .
	tax                                     ; 498C AA                       .
L498D:  stx     $A0                             ; 498D 86 A0                    ..
	rts                                     ; 498F 60                       `

; ----------------------------------------------------------------------------
L4990:  stx     $A3                             ; 4990 86 A3                    ..
	tay                                     ; 4992 A8                       .
	jsr     L496E                           ; 4993 20 6E 49                  nI
	lda     $A0                             ; 4996 A5 A0                    ..
	beq     L499C                           ; 4998 F0 02                    ..
	ldy     $A3                             ; 499A A4 A3                    ..
L499C:  sty     $A0                             ; 499C 84 A0                    ..
	rts                                     ; 499E 60                       `

; ----------------------------------------------------------------------------
L499F:  brk                                     ; 499F 00                       .
L49A0:  brk                                     ; 49A0 00                       .
L49A1:  brk                                     ; 49A1 00                       .
L49A2:  jmp     L49A5                           ; 49A2 4C A5 49                 L.I

; ----------------------------------------------------------------------------
L49A5:  stx     L49A1                           ; 49A5 8E A1 49                 ..I
	sta     L49A0                           ; 49A8 8D A0 49                 ..I
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
	sta     $02C3                           ; 49BF 8D C3 02                 ...
	rts                                     ; 49C2 60                       `

; ----------------------------------------------------------------------------
L49C3:  .byte   $20                             ; 49C3 20                        
L49C4:  .byte   $53                             ; 49C4 53                       S
	dec     $1E,x                           ; 49C5 D6 1E                    ..
	.byte   $0C                             ; 49C7 0C                       .
	brk                                     ; 49C8 00                       .
	eor     ($54,x)                         ; 49C9 41 54                    AT
	eor     ($52,x)                         ; 49CB 41 52                    AR
	eor     #$D8                            ; 49CD 49 D8                    I.
	;bmi     L49DD                           ; 49CF 30 0C                    0.
	.byte	$30,$0C
L49D1:  .byte   $0F                             ; 49D1 0F                       .
L49D2:  brk                                     ; 49D2 00                       .

L49D3:  prolog
	stx     L49C4                           ; 49D6 8E C4 49                 ..I
	sta     L49C3                           ; 49D9 8D C3 49                 ..I
	lda	L49C3
	sta	HPOSP3
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
	lda     L49D2                           ; 49FE AD D2 49                 ..I
	sta     $A3                             ; 4A01 85 A3                    ..
	lda     #$00                            ; 4A03 A9 00                    ..
	sta     $A5                             ; 4A05 85 A5                    ..
	lda     #$0C                            ; 4A07 A9 0C                    ..
	sta     $A4                             ; 4A09 85 A4                    ..
	ldy     L49D1                           ; 4A0B AC D1 49                 ..I
	ldx     #$49                            ; 4A0E A2 49                    .I
	lda     #$C5                            ; 4A10 A9 C5                    ..
	jsr     sub_461F
	lda     #$00                            ; 4A15 A9 00                    ..
	sta     $A3                             ; 4A17 85 A3                    ..
	ldy     #$0C                            ; 4A19 A0 0C                    ..
	ldx     L49D2                           ; 4A1B AE D2 49                 ..I
	lda     L49D1                           ; 4A1E AD D1 49                 ..I
	jsr     L45F6                           ; 4A21 20 F6 45                  .E
	lda     L49C4                           ; 4A24 AD C4 49                 ..I
	sta     L499F                           ; 4A27 8D 9F 49                 ..I
	clc                                     ; 4A2A 18                       .
	lda     #$80                            ; 4A2B A9 80                    ..
	adc     L499F                           ; 4A2D 6D 9F 49                 m.I
	sta     $A0                             ; 4A30 85 A0                    ..
	lda     #$B3                            ; 4A32 A9 B3                    ..
	adc     #$00                            ; 4A34 69 00                    i.
	sta     $A1                             ; 4A36 85 A1                    ..
	lda     #$49                            ; 4A38 A9 49                    .I
	sta     $A3                             ; 4A3A 85 A3                    ..
	lda     #$00                            ; 4A3C A9 00                    ..
	sta     $A5                             ; 4A3E 85 A5                    ..
	lda     #$0C                            ; 4A40 A9 0C                    ..
	sta     $A4                             ; 4A42 85 A4                    ..
	ldy     #$C5                            ; 4A44 A0 C5                    ..
	ldx     $A1                             ; 4A46 A6 A1                    ..
	lda     $A0                             ; 4A48 A5 A0                    ..
	jsr     sub_461F
	rts                                     ; 4A4D 60                       `

; ----------------------------------------------------------------------------
L4A4E:  .byte   $43                             ; 4A4E 43                       C
L4A4F:  .byte   $52                             ; 4A4F 52                       R
L4A50:  .byte   $49                             ; 4A50 49                       I
L4A51:  .byte   $50                             ; 4A51 50                       P
L4A52:  .byte   $54                             ; 4A52 54                       T

L4A53:	stack_prolog L4A4E, $04
	lda     #$00                            ; 4A5C A9 00                    ..
	sta     $A3                             ; 4A5E 85 A3                    ..
	ldy     #$80                            ; 4A60 A0 80                    ..
	ldx     #$B3                            ; 4A62 A2 B3                    ..
	lda     #$80                            ; 4A64 A9 80                    ..
	jsr     L45F6                           ; 4A66 20 F6 45                  .E
	clc                                     ; 4A69 18                       .
	lda     #$80                            ; 4A6A A9 80                    ..
	adc     L4A52                           ; 4A6C 6D 52 4A                 mRJ
	sta     $A0                             ; 4A6F 85 A0                    ..
	lda     #$B3                            ; 4A71 A9 B3                    ..
	adc     #$00                            ; 4A73 69 00                    i.
	sta     $A1                             ; 4A75 85 A1                    ..
	lda     L4A4F                           ; 4A77 AD 4F 4A                 .OJ
	sta     $A3                             ; 4A7A 85 A3                    ..
	lda     #$00                            ; 4A7C A9 00                    ..
	sta     $A5                             ; 4A7E 85 A5                    ..
	lda     L4A50                           ; 4A80 AD 50 4A                 .PJ
	sta     $A4                             ; 4A83 85 A4                    ..
	ldy     L4A4E                           ; 4A85 AC 4E 4A                 .NJ
	ldx     $A1                             ; 4A88 A6 A1                    ..
	lda     $A0                             ; 4A8A A5 A0                    ..
	jsr     sub_461F
	lda     #$02                            ; 4A8F A9 02                    ..
	sta     $D00B                           ; 4A91 8D 0B D0                 ...
	lda     L4A51                           ; 4A94 AD 51 4A                 .QJ
	sta     HPOSP3
	lda     L4A52                           ; 4A9A AD 52 4A                 .RJ
	sta     L499F                           ; 4A9D 8D 9F 49                 ..I
	rts                                     ; 4AA0 60                       `

; ----------------------------------------------------------------------------
L4AA1:	.byte   $02,"R:"

L4AA4:  brk                                     ; 4AA4 00                       .

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
	sta     $05C0,y                         ; 4AE1 99 C0 05                 ...
L4AE4:  rts                                     ; 4AE4 60                       `

; ----------------------------------------------------------------------------
L4AE5:  .byte   $0D                             ; 4AE5 0D                       .

sub_4AE6:  
	prolog
	sta     L4AE5                           ; 4AE9 8D E5 4A                 ..J
	lda     #$00                            ; 4AEC A9 00                    ..
	sta     $A3                             ; 4AEE 85 A3                    ..
	lda     #$00                            ; 4AF0 A9 00                    ..
	sta     $A4                             ; 4AF2 85 A4                    ..
	lda     #>L4AA1                         ; 4AF4 A9 4A                    .J
	sta     $A6                             ; 4AF6 85 A6                    ..
	lda     #<L4AA1                         ; 4AF8 A9 A1                    ..
	sta     $A5                             ; 4AFA 85 A5                    ..
	ldy     #$0D                            ; 4AFC A0 0D                    ..
	ldx     #$00                            ; 4AFE A2 00                    ..
	lda     L4AE5                           ; 4B00 AD E5 4A                 ..J
	jsr     sub_45D0
	rts                                     ; 4B06 60                       `

; ----------------------------------------------------------------------------
sub_4B07:  
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
L4B39:  sec                                     ; 4B39 38                       8
	sbc     #$30                            ; 4B3A E9 30                    .0
	and     #$1F                            ; 4B3C 29 1F                    ).
	cmp     #$0A                            ; 4B3E C9 0A                    ..
	bcc     L4B44                           ; 4B40 90 02                    ..
	sbc     #$07                            ; 4B42 E9 07                    ..
L4B44:  sta     $A0                             ; 4B44 85 A0                    ..
	rts                                     ; 4B46 60                       `

; ----------------------------------------------------------------------------
L4B47:  stx     $AF                             ; 4B47 86 AF                    ..
	sta     $AE                             ; 4B49 85 AE                    ..
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
L4B62:  jsr     L4B39                           ; 4B62 20 39 4B                  9K
	lda     $A0                             ; 4B65 A5 A0                    ..
	asl     a                               ; 4B67 0A                       .
	asl     a                               ; 4B68 0A                       .
	asl     a                               ; 4B69 0A                       .
	asl     a                               ; 4B6A 0A                       .
	sta     $A1                             ; 4B6B 85 A1                    ..
	iny                                     ; 4B6D C8                       .
	lda     ($AE),y                         ; 4B6E B1 AE                    ..
	jsr     L4B39                           ; 4B70 20 39 4B                  9K
	lda     $A0                             ; 4B73 A5 A0                    ..
	clc                                     ; 4B75 18                       .
	adc     $A1                             ; 4B76 65 A1                    e.
	sta     $A0                             ; 4B78 85 A0                    ..
	rts                                     ; 4B7A 60                       `

; ----------------------------------------------------------------------------
L4B7B:  tay                                     ; 4B7B A8                       .
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
L4BC9:  sta     $A0                             ; 4BC9 85 A0                    ..
	and     #$60                            ; 4BCB 29 60                    )`
	sta     $A2                             ; 4BCD 85 A2                    ..
	asl     a                               ; 4BCF 0A                       .
	sta     $A3                             ; 4BD0 85 A3                    ..
	eor     $A2                             ; 4BD2 45 A2                    E.
	eor     #$FF                            ; 4BD4 49 FF                    I.
	and     #$40                            ; 4BD6 29 40                    )@
	lsr     a                               ; 4BD8 4A                       J
	sta     $A1                             ; 4BD9 85 A1                    ..
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
L4BEE:	.byte	$32
	.byte	$20
	.byte	$53
	.byte	$42

sub_4BF2:  
	stack_prolog L4BEC, $05
	lda     #>L4BEE
	sta     $A3                             ; 4BFD 85 A3                    ..
	lda     #$00                            ; 4BFF A9 00                    ..
	sta     $A5                             ; 4C01 85 A5                    ..
	lda     #$04                            ; 4C03 A9 04                    ..
	sta     $A4                             ; 4C05 85 A4                    ..
	ldy     #<L4BEE                         ; 4C07 A0 EE                    ..
	ldxa	L4BEC
	jsr     sub_461F
	rts                                     ; 4C12 60                       `

; ----------------------------------------------------------------------------
L4C13:  .byte   $79                             ; 4C13 79                       y
L4C14:  .byte   $31                             ; 4C14 31                       1
L4C15:  .byte   $2C                             ; 4C15 2C                       ,
L4C16:  .byte   $20                             ; 4C16 20                        
L4C17:  .byte   $72                             ; 4C17 72                       r
L4C18:  .byte   $32                             ; 4C18 32                       2
L4C19:  sei                                     ; 4C19 78                       x
L4C1A:	.byte   $32                             ; 4C1A 32                       2
L4C1B:  .byte   $2C                             ; 4C1B 2C                       ,
L4C1C:  .byte   $20                             ; 4C1C 20                        

L4C1D:  stack_prolog L4C13, $05
	lda	L4C16
	sta     $A3                             ; 4C29 85 A3                    ..
	lda	#$00
	sta     $A5                             ; 4C2D 85 A5                    ..
	lda     #$04                            ; 4C2F A9 04                    ..
	sta     $A4                             ; 4C31 85 A4                    ..
	ldy     L4C15                           ; 4C33 AC 15 4C                 ..L
	ldxai	L4C19
	jsr     sub_461F
	add8m	$A2, L4C19, L4C17
	add8m	$A3, L4C1A, L4C18
	add8m	$A4, L4C1B, L4C17
	add8m	$A5, L4C1C, L4C18
	ldy     $A2                             ; 4C61 A4 A2                    ..
	ldx     L4C14                           ; 4C63 AE 14 4C                 ..L
	lda     L4C13                           ; 4C66 AD 13 4C                 ..L
	jsr     sub_4BF2
	rts                                     ; 4C6C 60                       `

; ----------------------------------------------------------------------------
L4C6D:  .byte   $20                             ; 4C6D 20                        
L4C6E:  .byte   $72                             ; 4C6E 72                       r
L4C6F:  .byte   $32                             ; 4C6F 32                       2
L4C70:  .byte   $79                             ; 4C70 79                       y
L4C71:  .byte   $31                             ; 4C71 31                       1
L4C72:  .byte   $2B                             ; 4C72 2B                       +
L4C73:  .byte   $79                             ; 4C73 79                       y
L4C74:  .byte   $2C                             ; 4C74 2C                       ,

L4C75:  stack_prolog L4C6D, $03
	lda     L4C70                           ; 4C7E AD 70 4C                 .pL
	sta     $A3                             ; 4C81 85 A3                    ..
	lda     #$00                            ; 4C83 A9 00                    ..
	sta     $A5                             ; 4C85 85 A5                    ..
	lda     #$04                            ; 4C87 A9 04                    ..
	sta     $A4                             ; 4C89 85 A4                    ..
	ldy     L4C6F                           ; 4C8B AC 6F 4C                 .oL
	ldx     #$4C                            ; 4C8E A2 4C                    .L
	lda     #$71                            ; 4C90 A9 71                    .q
	jsr     sub_461F
	ldx     L4C71                           ; 4C95 AE 71 4C                 .qL
	lda     L4C6D                           ; 4C98 AD 6D 4C                 .mL
	jsr     L4955                           ; 4C9B 20 55 49                  UI
	lda     $A0                             ; 4C9E A5 A0                    ..
	lbeq	L4CAA
	lda     #$00                            ; 4CA5 A9 00                    ..
	sta     $A0                             ; 4CA7 85 A0                    ..
	rts                                     ; 4CA9 60                       `

; ----------------------------------------------------------------------------
L4CAA:  ldx     L4C73                           ; 4CAA AE 73 4C                 .sL
	lda     L4C6D                           ; 4CAD AD 6D 4C                 .mL
	jsr     L496E                           ; 4CB0 20 6E 49                  nI
	lda     $A0                             ; 4CB3 A5 A0                    ..
	lbeq	L4CBF
	ldi	$A0, $00
	rts                                     ; 4CBE 60                       `

; ----------------------------------------------------------------------------
L4CBF:  ldx     L4C72                           ; 4CBF AE 72 4C                 .rL
	lda     L4C6E                           ; 4CC2 AD 6E 4C                 .nL
	jsr     L4955                           ; 4CC5 20 55 49                  UI
	lda     $A0                             ; 4CC8 A5 A0                    ..
	lbeq	L4CD4
	ldi	$A0, $00
	rts                                     ; 4CD3 60                       `

; ----------------------------------------------------------------------------
L4CD4:  ldx     L4C74                           ; 4CD4 AE 74 4C                 .tL
	lda     L4C6E                           ; 4CD7 AD 6E 4C                 .nL
	jsr     L496E                           ; 4CDA 20 6E 49                  nI
	lda     $A0                             ; 4CDD A5 A0                    ..
	lbeq	L4CE9
	ldi	$A0, $00
	rts                                     ; 4CE8 60                       `

; ----------------------------------------------------------------------------
L4CE9:	ldi	$A0, $01
	rts                                     ; 4CED 60                       `

; ----------------------------------------------------------------------------
L4CEE:  brk                                     ; 4CEE 00                       .
L4CEF:  .byte   $0D                             ; 4CEF 0D                       .
L4CF0:  .byte   $3B                             ; 4CF0 3B                       ;
L4CF1:  .byte   $44                             ; 4CF1 44                       D
L4CF2:  .byte   $45                             ; 4CF2 45                       E
L4CF3:  .byte   $53                             ; 4CF3 53                       S
L4CF4:  .byte   $43                             ; 4CF4 43                       C

L4CF5:  stack_prolog L4CEE, $05
	ldy     #$00                            ; 4CFE A0 00                    ..
	sty     L4CF4                           ; 4D00 8C F4 4C                 ..L
L4D03:  lda     #$01                            ; 4D03 A9 01                    ..
	cmp     L4CF4                           ; 4D05 CD F4 4C                 ..L
	lbcc	L4DCC
	clc                                     ; 4D0D 18                       .
	lda     L4CF2                           ; 4D0E AD F2 4C                 ..L
	adc     L4CF4                           ; 4D11 6D F4 4C                 m.L
	sta     $AE                             ; 4D14 85 AE                    ..
	lda     L4CF3                           ; 4D16 AD F3 4C                 ..L
	adc     #$00                            ; 4D19 69 00                    i.
	sta     $AF                             ; 4D1B 85 AF                    ..
	lda     $AF                             ; 4D1D A5 AF                    ..
	pha                                     ; 4D1F 48                       H
	lda     $AE                             ; 4D20 A5 AE                    ..
	pha                                     ; 4D22 48                       H
	clc                                     ; 4D23 18                       .
	lda     L4CEE                           ; 4D24 AD EE 4C                 ..L
	adc     L4CF4                           ; 4D27 6D F4 4C                 m.L
	sta     $AE                             ; 4D2A 85 AE                    ..
	lda     L4CEF                           ; 4D2C AD EF 4C                 ..L
	adc     #$00                            ; 4D2F 69 00                    i.
	sta     $AF                             ; 4D31 85 AF                    ..
	ldy     #$00                            ; 4D33 A0 00                    ..
	lda     ($AE),y                         ; 4D35 B1 AE                    ..
	sta     $A0                             ; 4D37 85 A0                    ..
	clc                                     ; 4D39 18                       .
	lda     L4CF0                           ; 4D3A AD F0 4C                 ..L
	adc     L4CF4                           ; 4D3D 6D F4 4C                 m.L
	sta     $AE                             ; 4D40 85 AE                    ..
	lda     L4CF1                           ; 4D42 AD F1 4C                 ..L
	adc     #$00                            ; 4D45 69 00                    i.
	sta     $AF                             ; 4D47 85 AF                    ..
	lda     ($AE),y                         ; 4D49 B1 AE                    ..
	sta     $A1                             ; 4D4B 85 A1                    ..
	ldx     $A1                             ; 4D4D A6 A1                    ..
	lda     $A0                             ; 4D4F A5 A0                    ..
	jsr     L4983                           ; 4D51 20 83 49                  .I
	pla                                     ; 4D54 68                       h
	sta     $AE                             ; 4D55 85 AE                    ..
	pla                                     ; 4D57 68                       h
	sta     $AF                             ; 4D58 85 AF                    ..
	lda     $A0                             ; 4D5A A5 A0                    ..
	ldy     #$00                            ; 4D5C A0 00                    ..
	sta     ($AE),y                         ; 4D5E 91 AE                    ..
	clc                                     ; 4D60 18                       .
	lda     L4CF4                           ; 4D61 AD F4 4C                 ..L
	adc     #$02                            ; 4D64 69 02                    i.
	sta     $AE                             ; 4D66 85 AE                    ..
	clc                                     ; 4D68 18                       .
	lda     L4CF2                           ; 4D69 AD F2 4C                 ..L
	adc     $AE                             ; 4D6C 65 AE                    e.
	sta     $AC                             ; 4D6E 85 AC                    ..
	lda     L4CF3                           ; 4D70 AD F3 4C                 ..L
	adc     #$00                            ; 4D73 69 00                    i.
	sta     $AD                             ; 4D75 85 AD                    ..
	lda     $AD                             ; 4D77 A5 AD                    ..
	pha                                     ; 4D79 48                       H
	lda     $AC                             ; 4D7A A5 AC                    ..
	pha                                     ; 4D7C 48                       H
	clc                                     ; 4D7D 18                       .
	lda     L4CF4                           ; 4D7E AD F4 4C                 ..L
	adc     #$02                            ; 4D81 69 02                    i.
	sta     $AE                             ; 4D83 85 AE                    ..
	clc                                     ; 4D85 18                       .
	lda     L4CEE                           ; 4D86 AD EE 4C                 ..L
	adc     $AE                             ; 4D89 65 AE                    e.
	sta     $AC                             ; 4D8B 85 AC                    ..
	lda     L4CEF                           ; 4D8D AD EF 4C                 ..L
	adc     #$00                            ; 4D90 69 00                    i.
	sta     $AD                             ; 4D92 85 AD                    ..
	lda     ($AC),y                         ; 4D94 B1 AC                    ..
	sta     $A0                             ; 4D96 85 A0                    ..
	clc                                     ; 4D98 18                       .
	lda     L4CF4                           ; 4D99 AD F4 4C                 ..L
	adc     #$02                            ; 4D9C 69 02                    i.
	sta     $AE                             ; 4D9E 85 AE                    ..
	clc                                     ; 4DA0 18                       .
	lda     L4CF0                           ; 4DA1 AD F0 4C                 ..L
	adc     $AE                             ; 4DA4 65 AE                    e.
	sta     $AC                             ; 4DA6 85 AC                    ..
	lda     L4CF1                           ; 4DA8 AD F1 4C                 ..L
	adc     #$00                            ; 4DAB 69 00                    i.
	sta     $AD                             ; 4DAD 85 AD                    ..
	lda     ($AC),y                         ; 4DAF B1 AC                    ..
	sta     $A1                             ; 4DB1 85 A1                    ..
	ldx     $A1                             ; 4DB3 A6 A1                    ..
	lda     $A0                             ; 4DB5 A5 A0                    ..
	jsr     L4990                           ; 4DB7 20 90 49                  .I
	pla                                     ; 4DBA 68                       h
	sta     $AC                             ; 4DBB 85 AC                    ..
	pla                                     ; 4DBD 68                       h
	sta     $AD                             ; 4DBE 85 AD                    ..
	lda     $A0                             ; 4DC0 A5 A0                    ..
	ldy     #$00                            ; 4DC2 A0 00                    ..
	sta     ($AC),y                         ; 4DC4 91 AC                    ..
	inc     L4CF4                           ; 4DC6 EE F4 4C                 ..L
	jmp     L4D03                           ; 4DC9 4C 03 4D                 L.M

; ----------------------------------------------------------------------------
L4DCC:  dmv	$AE, L4CF2
	ldy     #$00                            ; 4DD6 A0 00                    ..
	lda     ($AE),y                         ; 4DD8 B1 AE                    ..
	sta     $A0                             ; 4DDA 85 A0                    ..
	clc                                     ; 4DDC 18                       .
	lda     L4CF2                           ; 4DDD AD F2 4C                 ..L
	adc     #$02                            ; 4DE0 69 02                    i.
	sta     $AE                             ; 4DE2 85 AE                    ..
	lda     L4CF3                           ; 4DE4 AD F3 4C                 ..L
	adc     #$00                            ; 4DE7 69 00                    i.
	sta     $AF                             ; 4DE9 85 AF                    ..
	lda     ($AE),y                         ; 4DEB B1 AE                    ..
	sta     $A1                             ; 4DED 85 A1                    ..
	ldx     $A1                             ; 4DEF A6 A1                    ..
	lda     $A0                             ; 4DF1 A5 A0                    ..
	jsr     L496E                           ; 4DF3 20 6E 49                  nI
	lda     $A0                             ; 4DF6 A5 A0                    ..
	bne     L4DFD                           ; 4DF8 D0 03                    ..
	jmp     L4E02                           ; 4DFA 4C 02 4E                 L.N

; ----------------------------------------------------------------------------
L4DFD:  ldi	$A0, $00
	rts                                     ; 4E01 60                       `

; ----------------------------------------------------------------------------
L4E02:  clc                                     ; 4E02 18                       .
	lda     L4CF2                           ; 4E03 AD F2 4C                 ..L
	adc     #$01                            ; 4E06 69 01                    i.
	sta     $AE                             ; 4E08 85 AE                    ..
	lda     L4CF3                           ; 4E0A AD F3 4C                 ..L
	adc     #$00                            ; 4E0D 69 00                    i.
	sta     $AF                             ; 4E0F 85 AF                    ..
	ldy     #$00                            ; 4E11 A0 00                    ..
	lda     ($AE),y                         ; 4E13 B1 AE                    ..
	sta     $A0                             ; 4E15 85 A0                    ..
	clc                                     ; 4E17 18                       .
	lda     L4CF2                           ; 4E18 AD F2 4C                 ..L
	adc     #$03                            ; 4E1B 69 03                    i.
	sta     $AE                             ; 4E1D 85 AE                    ..
	lda     L4CF3                           ; 4E1F AD F3 4C                 ..L
	adc     #$00                            ; 4E22 69 00                    i.
	sta     $AF                             ; 4E24 85 AF                    ..
	lda     ($AE),y                         ; 4E26 B1 AE                    ..
	sta     $A1                             ; 4E28 85 A1                    ..
	ldx     $A1                             ; 4E2A A6 A1                    ..
	lda     $A0                             ; 4E2C A5 A0                    ..
	jsr     L496E                           ; 4E2E 20 6E 49                  nI
	lda     $A0                             ; 4E31 A5 A0                    ..
	lbeq	L4E3D
	ldi	$A0, $00
	rts                                     ; 4E3C 60                       `

; ----------------------------------------------------------------------------
L4E3D:  ldi	$A0, $01
	rts                                     ; 4E41 60                       `

; ----------------------------------------------------------------------------
L4E42:  .byte   $20                             ; 4E42 20                        
L4E43:  .byte   $20                             ; 4E43 20                        
L4E44:  .byte   $20                             ; 4E44 20                        
L4E45:  .byte   $20                             ; 4E45 20                        
L4E46:  .byte   $20                             ; 4E46 20                        
L4E47:  .byte   $52                             ; 4E47 52                       R
L4E48:  .byte   $45                             ; 4E48 45                       E
L4E49:  .byte   $54                             ; 4E49 54                       T
L4E4A:  jmp     L4E4D                           ; 4E4A 4C 4D 4E                 LMN

; ----------------------------------------------------------------------------
L4E4D:  stx     L4E43                           ; 4E4D 8E 43 4E                 .CN
	sta     L4E42                           ; 4E50 8D 42 4E                 .BN
	lda     L4E43                           ; 4E53 AD 43 4E                 .CN
	sta     $A3                             ; 4E56 85 A3                    ..
	lda     #$00                            ; 4E58 A9 00                    ..
	sta     $A5                             ; 4E5A 85 A5                    ..
	lda     #$04                            ; 4E5C A9 04                    ..
	sta     $A4                             ; 4E5E 85 A4                    ..
	ldy     L4E42                           ; 4E60 AC 42 4E                 .BN
	ldxai	L4E46
	jsr     sub_461F
	sec                                     ; 4E6A 38                       8
	lda     L4E48                           ; 4E6B AD 48 4E                 .HN
	sbc     L4E46                           ; 4E6E ED 46 4E                 .FN
	sta     $AE                             ; 4E71 85 AE                    ..
	clc                                     ; 4E73 18                       .
	lda     $AE                             ; 4E74 A5 AE                    ..
	adc     #$01                            ; 4E76 69 01                    i.
	sta     $AC                             ; 4E78 85 AC                    ..
	sec                                     ; 4E7A 38                       8
	lda     L4E49                           ; 4E7B AD 49 4E                 .IN
	sbc     L4E47                           ; 4E7E ED 47 4E                 .GN
	sta     $AE                             ; 4E81 85 AE                    ..
	clc                                     ; 4E83 18                       .
	lda     $AE                             ; 4E84 A5 AE                    ..
	adc     #$01                            ; 4E86 69 01                    i.
	sta     $AA                             ; 4E88 85 AA                    ..
	lda     #$00                            ; 4E8A A9 00                    ..
	sta     $85                             ; 4E8C 85 85                    ..
	lda     $AA                             ; 4E8E A5 AA                    ..
	sta     $84                             ; 4E90 85 84                    ..
	lda     $AC                             ; 4E92 A5 AC                    ..
	ldx     #$00                            ; 4E94 A2 00                    ..
	jsr     sub_444A
	sta     L4E44                           ; 4E99 8D 44 4E                 .DN
	txa                                     ; 4E9C 8A                       .
	sta     L4E45                           ; 4E9D 8D 45 4E                 .EN
	lda     L4E45                           ; 4EA0 AD 45 4E                 .EN
	sta     $A1                             ; 4EA3 85 A1                    ..
	lda     L4E44                           ; 4EA5 AD 44 4E                 .DN
	sta     $A0                             ; 4EA8 85 A0                    ..
	rts                                     ; 4EAA 60                       `

; ----------------------------------------------------------------------------
L4EAB:  .byte   $49                             ; 4EAB 49                       I
L4EAC:  .byte   $46                             ; 4EAC 46                       F
L4EAD:  .byte   $20                             ; 4EAD 20                        
L4EAE:  .byte   $67                             ; 4EAE 67                       g
L4EAF:  .byte   $74                             ; 4EAF 74                       t
L4EB0:  plp                                     ; 4EB0 28                       (

L4EB1:  stack_prolog L4EAB, $05
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
	ldy     #$00                            ; 4F2E A0 00                    ..
	lda     (off_AC),y
	sta     (off_AE),y
	sub16i	L4EAB, L4EAB, $0001
	sub16i	L4EAD, L4EAD, $0001
	jmp     L4EFA                           ; 4F56 4C FA 4E                 L.N

; ----------------------------------------------------------------------------
L4F59:  rts                                     ; 4F59 60                       `

; ----------------------------------------------------------------------------
L4F5A:  ldy     #$01                            ; 4F5A A0 01                    ..
	sty     CRSINH
	lda     #$FD                            ; 4F5F A9 FD                    ..
	jsr     L45C4                           ; 4F61 20 C4 45                  .E
	ldy     #$01                            ; 4F64 A0 01                    ..
	sty     L4656                           ; 4F66 8C 56 46                 .VF
	rts                                     ; 4F69 60                       `

; ----------------------------------------------------------------------------
L4F6A:  brk                                     ; 4F6A 00                       .
L4F6B:  brk                                     ; 4F6B 00                       .
L4F6C:  .byte   $2E                             ; 4F6C 2E                       .

L4F6D:  prolog
	sta     L4F6A                           ; 4F70 8D 6A 4F                 .jO
	jsr     sub_4BB8
	clc                                     ; 4F76 18                       .
	lda     L4F6A                           ; 4F77 AD 6A 4F                 .jO
	adc     $A0                             ; 4F7A 65 A0                    e.
	sta     L4F6B                           ; 4F7C 8D 6B 4F                 .kO
	lda     #$00                            ; 4F7F A9 00                    ..
	adc     $A1                             ; 4F81 65 A1                    e.
	sta     L4F6C                           ; 4F83 8D 6C 4F                 .lO
L4F86:  jsr     sub_4BB8
	lda     $A0                             ; 4F89 A5 A0                    ..
	cmp     L4F6B                           ; 4F8B CD 6B 4F                 .kO
	lda     $A1                             ; 4F8E A5 A1                    ..
	sbc     L4F6C                           ; 4F90 ED 6C 4F                 .lO
	lbcs	L4F9B
L4F98:  jmp     L4F86                           ; 4F98 4C 86 4F                 L.O

; ----------------------------------------------------------------------------
L4F9B:  rts                                     ; 4F9B 60                       `

; ----------------------------------------------------------------------------
L4F9C:  .byte	$30

sub_4F9D:	
	prolog
	sta     L4F9C                           ; 4FA0 8D 9C 4F                 ..O
	ldi	$85, $00
	ldi	$84, $06
	lda     L4F9C                           ; 4FAB AD 9C 4F                 ..O
	ldx     #$00                            ; 4FAE A2 00                    ..
	jsr     sub_444A
	sta     $A0                             ; 4FB3 85 A0                    ..
	lda     $A0                             ; 4FB5 A5 A0                    ..
	jsr     L4F6D                           ; 4FB7 20 6D 4F                  mO
	rts                                     ; 4FBA 60                       `

; ----------------------------------------------------------------------------
L4FBB:  .byte   $53                             ; 4FBB 53                       S
L4FBC:  .byte   $20                             ; 4FBC 20                        
L4FBD:  .byte   $69                             ; 4FBD 69                       i
L4FBE:  brk                                     ; 4FBE 00                       .
L4FBF:  brk                                     ; 4FBF 00                       .
L4FC0:  .byte   $74                             ; 4FC0 74                       t
L4FC1:  pla                                     ; 4FC1 68                       h
L4FC2:  .byte   $65                             ; 4FC2 65                       e
L4FC3:  .byte   $20                             ; 4FC3 20                        
	.byte   $72                             ; 4FC4 72                       r

sub_4FC5:  
	prolog
	jsr     sub_4B07
	ldi	L4FBD, $00
	lda     $A0                             ; 4FD0 A5 A0                    ..
	sta     L4FBC                           ; 4FD2 8D BC 4F                 ..O
	lda     L4FBC                           ; 4FD5 AD BC 4F                 ..O
	ora     L4FBD                           ; 4FD8 0D BD 4F                 ..O
	lbne	L4FE5
	ldi	$A0, $00
	rts                                     ; 4FE4 60                       `

; ----------------------------------------------------------------------------
L4FE5:  lda     #$02                            ; 4FE5 A9 02                    ..
	jsr     L45A3                           ; 4FE7 20 A3 45                  .E
L4FEA:  lda     $A0                             ; 4FEA A5 A0                    ..
	sta     L4FBB                           ; 4FEC 8D BB 4F                 ..O
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
	lda     L4FBB                           ; 501D AD BB 4F                 ..O
	and     #$7F                            ; 5020 29 7F                    ).
	sta     L4FC0                           ; 5022 8D C0 4F                 ..O
	lda     L4FC0                           ; 5025 AD C0 4F                 ..O
	cmp     #$05                            ; 5028 C9 05                    ..
	bcc     L5036                           ; 502A 90 0A                    ..
	lda     #$5F                            ; 502C A9 5F                    ._
	cmp     L4FC0                           ; 502E CD C0 4F                 ..O
	lbcs	L5040
L5036:	ldy     #$01                            ; 5036 A0 01                    ..
	sty     L4FBE                           ; 5038 8C BE 4F                 ..O
	ldi	$A0, $00
	rts                                     ; 503F 60                       `

; ----------------------------------------------------------------------------
L5040:  lda     L4FBB                           ; 5040 AD BB 4F                 ..O
	sta     $B1C6                           ; 5043 8D C6 B1                 ...
	lda     #$B1                            ; 5046 A9 B1                    ..
L5048:  sta     L4FC3                           ; 5048 8D C3 4F                 ..O
	lda     #$C6                            ; 504B A9 C6                    ..
L504D:  sta     L4FC2                           ; 504D 8D C2 4F                 ..O
	lda     L4FBB                           ; 5050 AD BB 4F                 ..O
	sta     L4FC1                           ; 5053 8D C1 4F                 ..O
	ldy     #$01                            ; 5056 A0 01                    ..
	sty     L464F                           ; 5058 8C 4F 46                 .OF
	sty     L4FBF                           ; 505B 8C BF 4F                 ..O
	jsr     sub_4B07
	lda     #$00                            ; 5061 A9 00                    ..
	sta     L4FBD                           ; 5063 8D BD 4F                 ..O
	lda     $A0                             ; 5066 A5 A0                    ..
	sta     L4FBC                           ; 5068 8D BC 4F                 ..O
	lda     L4FBC                           ; 506B AD BC 4F                 ..O
	ora     L4FBD                           ; 506E 0D BD 4F                 ..O
	lbne	L507B
	lda     #$00                            ; 5076 A9 00                    ..
	sta     $A0                             ; 5078 85 A0                    ..
	rts                                     ; 507A 60                       `

; ----------------------------------------------------------------------------
L507B:  lda     #$02                            ; 507B A9 02                    ..
	jsr     L45A3                           ; 507D 20 A3 45                  .E
	lda     $A0                             ; 5080 A5 A0                    ..
	sta     L4FBB                           ; 5082 8D BB 4F                 ..O
L5085:  lda     L4FBF                           ; 5085 AD BF 4F                 ..O
	eor     #$01                            ; 5088 49 01                    I.
	lbne	L50EF
	lda     L4FBB                           ; 508F AD BB 4F                 ..O
	eor     $B1C6                           ; 5092 4D C6 B1                 M..
	lbeq	L50A4
	ldy     #$01                            ; 509A A0 01                    ..
	sty     L4FBE                           ; 509C 8C BE 4F                 ..O
	ldi	$A0, $00
	rts                                     ; 50A3 60                       `

; ----------------------------------------------------------------------------
L50A4:	add16m8 off_AE, L4FC2, L4FBF
	lda     L4FBB                           ; 50B4 AD BB 4F                 ..O
	ldy     #$00                            ; 50B7 A0 00                    ..
	sta     ($AE),y                         ; 50B9 91 AE                    ..
	add8m	L4FC1, L4FC1, L4FBB
	inc     L4FBF                           ; 50C5 EE BF 4F                 ..O
	jsr     sub_4B07
	lda     #$00                            ; 50CB A9 00                    ..
	sta     L4FBD                           ; 50CD 8D BD 4F                 ..O
	lda     $A0                             ; 50D0 A5 A0                    ..
	sta     L4FBC                           ; 50D2 8D BC 4F                 ..O
	lda     L4FBC                           ; 50D5 AD BC 4F                 ..O
	ora     L4FBD                           ; 50D8 0D BD 4F                 ..O
	lbne	L50E5
	ldi	$A0, $00
	rts                                     ; 50E4 60                       `

; ----------------------------------------------------------------------------
L50E5:  lda     #$02                            ; 50E5 A9 02                    ..
	jsr     L45A3                           ; 50E7 20 A3 45                  .E
	lda     $A0                             ; 50EA A5 A0                    ..
	sta     L4FBB                           ; 50EC 8D BB 4F                 ..O
L50EF:  clc                                     ; 50EF 18                       .
	lda     L4FC2                           ; 50F0 AD C2 4F                 ..O
	adc     L4FBF                           ; 50F3 6D BF 4F                 m.O
	sta     $AE                             ; 50F6 85 AE                    ..
	lda     L4FC3                           ; 50F8 AD C3 4F                 ..O
	adc     #$00                            ; 50FB 69 00                    i.
	sta     $AF                             ; 50FD 85 AF                    ..
	lda     L4FBB                           ; 50FF AD BB 4F                 ..O
	ldy     #$00                            ; 5102 A0 00                    ..
	sta     ($AE),y                         ; 5104 91 AE                    ..
	clc                                     ; 5106 18                       .
	lda     L4FC1                           ; 5107 AD C1 4F                 ..O
	adc     L4FBB                           ; 510A 6D BB 4F                 m.O
	sta     L4FC1                           ; 510D 8D C1 4F                 ..O
	inc     L4FBF                           ; 5110 EE BF 4F                 ..O
	lda     L4FBB                           ; 5113 AD BB 4F                 ..O
	eor     #$0A                            ; 5116 49 0A                    I.
	beq     L511D                           ; 5118 F0 03                    ..
	jmp     L514C                           ; 511A 4C 4C 51                 LLQ

; ----------------------------------------------------------------------------
L511D:  clc                                     ; 511D 18                       .
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
	lda     #$01                            ; 513F A9 01                    ..
	sta     $A0                             ; 5141 85 A0                    ..
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
	ldy     #$01                            ; 5157 A0 01                    ..
	sty     L4FBE                           ; 5159 8C BE 4F                 ..O
	ldi	$A0, $00
	rts                                     ; 5160 60                       `

; ----------------------------------------------------------------------------
L5161:  jsr     sub_4B07
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
	jsr     L45A3                           ; 517F 20 A3 45                  .E
	lda     $A0                             ; 5182 A5 A0                    ..
	sta     L4FBB                           ; 5184 8D BB 4F                 ..O
L5187:  lda     L4FBC                           ; 5187 AD BC 4F                 ..O
	ora     L4FBD                           ; 518A 0D BD 4F                 ..O
	lbne	L50EF
	lda     #$00                            ; 5192 A9 00                    ..
	sta     $A0                             ; 5194 85 A0                    ..
	rts                                     ; 5196 60                       `

; ----------------------------------------------------------------------------
L5197:  sta     $A0                             ; 5197 85 A0                    ..
	stx     $A1                             ; 5199 86 A1                    ..
	sty     $A2                             ; 519B 84 A2                    ..
	ldy     #$00                            ; 519D A0 00                    ..
	sty     $A6                             ; 519F 84 A6                    ..
	sty     $A5                             ; 51A1 84 A5                    ..
L51A3:  lda     $A5                             ; 51A3 A5 A5                    ..
	cmp     $A4                             ; 51A5 C5 A4                    ..
	lbcs	L51F1
	ldy     $A5                             ; 51AC A4 A5                    ..
	lda     ($A2),y                         ; 51AE B1 A2                    ..
	sta     $A7                             ; 51B0 85 A7                    ..
	lda     $A7                             ; 51B2 A5 A7                    ..
	and     #$7F                            ; 51B4 29 7F                    ).
	sta     $A8                             ; 51B6 85 A8                    ..
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
L51F7:  sta     $A9                             ; 51F7 85 A9                    ..
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
	sta     $A1                             ; 5253 85 A1                    ..
	lda     $A3                             ; 5255 A5 A3                    ..
	sta     $A3                             ; 5257 85 A3                    ..
	lda     #$00                            ; 5259 A9 00                    ..
	sta     $A5                             ; 525B 85 A5                    ..
	lda     $A9                             ; 525D A5 A9                    ..
	sta     $A4                             ; 525F 85 A4                    ..
	ldy     $A2                             ; 5261 A4 A2                    ..
	ldx     $A1                             ; 5263 A6 A1                    ..
	lda     $A0                             ; 5265 A5 A0                    ..
	jsr     sub_461F
	rts                                     ; 526A 60                       `

; ----------------------------------------------------------------------------
L526B:  cld                                     ; 526B D8                       .
L526C:  .byte   $30                             ; 526C 30                       0
L526D:  .byte   $6F                             ; 526D 6F                       o
L526E:  asl     $00                             ; 526E 06 00                    ..
	brk                                     ; 5270 00                       .
L5271:  brk                                     ; 5271 00                       .
L5272:  cld                                     ; 5272 D8                       .
L5273:  .byte   $30                             ; 5273 30                       0
L5274:  jmp     L5277                           ; 5274 4C 77 52                 LwR

; ----------------------------------------------------------------------------
L5277:	stx     L526C                           ; 5277 8E 6C 52                 .lR
	sta     L526B                           ; 527A 8D 6B 52                 .kR
	dmv	off_AE, L526B
	ldy     #$00                            ; 5287 A0 00                    ..
	lda     ($AE),y                         ; 5289 B1 AE                    ..
	sta     L526D                           ; 528B 8D 6D 52                 .mR
	lda     L526C                           ; 528E AD 6C 52                 .lR
	sta     L5273                           ; 5291 8D 73 52                 .sR
	lda     L526B                           ; 5294 AD 6B 52                 .kR
	sta     L5272                           ; 5297 8D 72 52                 .rR
	sty     L526E                           ; 529A 8C 6E 52                 .nR
	sec                                     ; 529D 38                       8
	lda     L526D                           ; 529E AD 6D 52                 .mR
	sbc     #$01                            ; 52A1 E9 01                    ..
	sta     L52B1                           ; 52A3 8D B1 52                 ..R
L52A6:  lda     L52B1                           ; 52A6 AD B1 52                 ..R
	cmp     L526E                           ; 52A9 CD 6E 52                 .nR
	bcs     L52B2                           ; 52AC B0 04                    ..
	jmp     L52D7                           ; 52AE 4C D7 52                 L.R

; ----------------------------------------------------------------------------
L52B1:  .byte   $65                             ; 52B1 65                       e

; ----------------------------------------------------------------------------
L52B2:  clc                                     ; 52B2 18                       .
	lda     L5272                           ; 52B3 AD 72 52                 .rR
	adc     L526E                           ; 52B6 6D 6E 52                 mnR
	sta     $AE                             ; 52B9 85 AE                    ..
	lda     L5273                           ; 52BB AD 73 52                 .sR
	adc     #$00                            ; 52BE 69 00                    i.
	sta	$AF
	ldy     #$00                            ; 52C2 A0 00                    ..
	lda     ($AE),y                         ; 52C4 B1 AE                    ..
	sta     L5271                           ; 52C6 8D 71 52                 .qR
	ldx     L5271                           ; 52C9 AE 71 52                 .qR
	lda     #$02                            ; 52CC A9 02                    ..
	jsr     L45C7                           ; 52CE 20 C7 45                  .E
	inc     L526E                           ; 52D1 EE 6E 52                 .nR
	jmp     L52A6                           ; 52D4 4C A6 52                 L.R

; ----------------------------------------------------------------------------
L52D7:  ldx     #$0A                            ; 52D7 A2 0A                    ..
	lda     #$02                            ; 52D9 A9 02                    ..
	jsr	L45C7				; 52DB 20 C7 45
	rts					; 52DE 60

L52DF:  .byte   $32                             ; 52DF 32                       2
L52E0:  cld                                     ; 52E0 D8                       .

sub_52E1:  
	prolog
	stx     L52E0                           ; 52E4 8E E0 52                 ..R
	sta     L52DF                           ; 52E7 8D DF 52                 ..R
	inc     L464B                           ; 52EA EE 4B 46                 .KF
	lda     #$0A                            ; 52ED A9 0A                    ..
	cmp     L464B                           ; 52EF CD 4B 46                 .KF
	lbcs	L5304
	ldy     #$00                            ; 52F7 A0 00                    ..
	sty     L464B                           ; 52F9 8C 4B 46                 .KF
	lda     #$11                            ; 52FC A9 11                    ..
	jsr     sub_4BA7
	jsr     sub_4749
L5304:  ldx     L52E0                           ; 5304 AE E0 52                 ..R
	lda     L52DF                           ; 5307 AD DF 52                 ..R
	jsr     L5274                           ; 530A 20 74 52                  tR
	ldy     #$01                            ; 530D A0 01                    ..
	sty     CDTMF3
	ldi	$021D, $02
	ldi	$021C, $58
	rts                                     ; 531C 60                       `

; ----------------------------------------------------------------------------
L531D:  .byte   $FF                             ; 531D FF                       .
L531E:  brk                                     ; 531E 00                       .
L531F:  adc     $00                             ; 531F 65 00                    e.
	brk                                     ; 5321 00                       .
L5322:  .byte   $20                             ; 5322 20                        
L5323:  .byte   $53                             ; 5323 53                       S
	dec     $1E,x                           ; 5324 D6 1E                    ..
	ora     $00                             ; 5326 05 00                    ..
	sei                                     ; 5328 78                       x
L5329:  .byte   $32                             ; 5329 32                       2

L532A:  prolog
	stx     L531F                           ; 532D 8E 1F 53                 ..S
	sta     L531E                           ; 5330 8D 1E 53                 ..S
	lda     L5322                           ; 5333 AD 22 53                 ."S
	sta     $AE                             ; 5336 85 AE                    ..
	lda     L5323                           ; 5338 AD 23 53                 .#S
	sta     $AF                             ; 533B 85 AF                    ..
	lda     L531F                           ; 533D AD 1F 53                 ..S
	ldy	#$00
	sta     ($AE),y                         ; 5342 91 AE                    ..
	lda     L474C                           ; 5344 AD 4C 47                 .LG
	sta     L5329                           ; 5347 8D 29 53                 .)S
	sty     L464B                           ; 534A 8C 4B 46                 .KF
	lda     L5323                           ; 534D AD 23 53                 .#S
	sta     $A3                             ; 5350 85 A3                    ..
	lda     #$53                            ; 5352 A9 53                    .S
	sta     $A5                             ; 5354 85 A5                    ..
	lda     #$24                            ; 5356 A9 24                    .$
	sta     $A4                             ; 5358 85 A4                    ..
	ldy     L5322                           ; 535A AC 22 53                 ."S
	ldx     L531E                           ; 535D AE 1E 53                 ..S
	lda     #$01                            ; 5360 A9 01                    ..
	jsr     L51F7                           ; 5362 20 F7 51                  .Q
	lda     L5329                           ; 5365 AD 29 53                 .)S
	sta     L474C                           ; 5368 8D 4C 47                 .LG
	ldx     #$53                            ; 536B A2 53                    .S
	lda     #$24                            ; 536D A9 24                    .$
	jsr     L5274                           ; 536F 20 74 52                  tR
	rts                                     ; 5372 60                       `

; ----------------------------------------------------------------------------
L5373:  jmp     L5376                           ; 5373 4C 76 53                 LvS

; ----------------------------------------------------------------------------
L5376:  ldx     #$1B                            ; 5376 A2 1B                    ..
	lda     L531D                           ; 5378 AD 1D 53                 ..S
	jsr     L532A                           ; 537B 20 2A 53                  *S
	rts                                     ; 537E 60                       `

; ----------------------------------------------------------------------------
L537F:  ldx     #$06                            ; 537F A2 06                    ..
	lda     $B1C8                           ; 5381 AD C8 B1                 ...
	jsr     L532A                           ; 5384 20 2A 53                  *S
	rts                                     ; 5387 60                       `

; ----------------------------------------------------------------------------
L5388:  ldx     #$15                            ; 5388 A2 15                    ..
	lda     $B1C8                           ; 538A AD C8 B1                 ...
	jsr     L532A                           ; 538D 20 2A 53                  *S
	rts                                     ; 5390 60                       `

; ----------------------------------------------------------------------------
L5391:  .byte   $2D                             ; 5391 2D                       -
L5392:  .byte   $2D                             ; 5392 2D                       -
L5393:  .byte   $2D                             ; 5393 2D                       -

L5394:  prolog
	ldy     #$00                            ; 5397 A0 00                    ..
	sty     L5392                           ; 5399 8C 92 53                 ..S
	jsr     sub_4FC5
	lda     $A0                             ; 539F A5 A0                    ..
	sta     L5391                           ; 53A1 8D 91 53                 ..S
	lda     $B1C9                           ; 53A4 AD C9 B1                 ...
	sta     L5393                           ; 53A7 8D 93 53                 ..S
	lda     L5391                           ; 53AA AD 91 53                 ..S
	eor     #$04                            ; 53AD 49 04                    I.
	lbne	L53BE
	ldxai	$B16A
	jsr     sub_52E1
	jmp     L545B                           ; 53BB 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L53BE:  lda     L5391                           ; 53BE AD 91 53                 ..S
	eor     #$01                            ; 53C1 49 01                    I.
	beq     L53C8                           ; 53C3 F0 03                    ..
	jmp     L544C                           ; 53C5 4C 4C 54                 LLT

; ----------------------------------------------------------------------------
L53C8:  lda     L5393                           ; 53C8 AD 93 53                 ..S
	eor     #$06                            ; 53CB 49 06                    I.
	beq     L53D2                           ; 53CD F0 03                    ..
	jmp     L5421                           ; 53CF 4C 21 54                 L!T

; ----------------------------------------------------------------------------
L53D2:  lda     $B1C8                           ; 53D2 AD C8 B1                 ...
	eor     $B16C                           ; 53D5 4D 6C B1                 Ml.
	beq     L53DD                           ; 53D8 F0 03                    ..
	jmp     L541E                           ; 53DA 4C 1E 54                 L.T

; ----------------------------------------------------------------------------
L53DD:  sec                                     ; 53DD 38                       8
	lda     L4650                           ; 53DE AD 50 46                 .PF
	sbc     $B16A                           ; 53E1 ED 6A B1                 .j.
	sta     L4650                           ; 53E4 8D 50 46                 .PF
	clc                                     ; 53E7 18                       .
	lda     #$6A                            ; 53E8 A9 6A                    .j
	adc     $B16A                           ; 53EA 6D 6A B1                 mj.
	sta     $A2                             ; 53ED 85 A2                    ..
	lda     #$B1                            ; 53EF A9 B1                    ..
	adc     #$00                            ; 53F1 69 00                    i.
	sta     $A3                             ; 53F3 85 A3                    ..
	sec                                     ; 53F5 38                       8
	lda     #$5A                            ; 53F6 A9 5A                    .Z
	sbc     $B16A                           ; 53F8 ED 6A B1                 .j.
	sta     $A4                             ; 53FB 85 A4                    ..
	lda     #$00                            ; 53FD A9 00                    ..
	sta     $A5                             ; 53FF 85 A5                    ..
	ldy     $A2                             ; 5401 A4 A2                    ..
	ldxai	$B16A
	jsr     sub_461F
	lda     L4650                           ; 540A AD 50 46                 .PF
	bne     L5412                           ; 540D D0 03                    ..
	jmp     L541E                           ; 540F 4C 1E 54                 L.T

; ----------------------------------------------------------------------------
L5412:  ldy     #$00                            ; 5412 A0 00                    ..
	sty     L464B                           ; 5414 8C 4B 46                 .KF
	ldxai	$B16A
	jsr     sub_52E1
L541E:  jmp     L5449                           ; 541E 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5421:  lda     L5393                           ; 5421 AD 93 53                 ..S
	eor     #$15                            ; 5424 49 15                    I.
	lbne	L5435
	ldxai	$B16A
	jsr     sub_52E1
	jmp     L5449                           ; 5432 4C 49 54                 LIT

; ----------------------------------------------------------------------------
L5435:  ldy     #$01                            ; 5435 A0 01                    ..
	sty     L5392                           ; 5437 8C 92 53                 ..S
	lda     CDTMF3
	lbne	L5449
	ldxai	$B16A
	jsr     sub_52E1
L5449:  jmp     L545B                           ; 5449 4C 5B 54                 L[T

; ----------------------------------------------------------------------------
L544C:  lda     CDTMF3
	lbne	L545B
	ldxai	$B16A
	jsr     sub_52E1
L545B:  lda     L5392                           ; 545B AD 92 53                 ..S
	sta     $A0                             ; 545E 85 A0                    ..
	rts                                     ; 5460 60                       `

; ----------------------------------------------------------------------------
L5461:  prolog
	lda     L4651                           ; 5464 AD 51 46                 .QF
	lbne	L5471
	ldi	$A0, $00
	rts                                     ; 5470 60                       `

; ----------------------------------------------------------------------------
L5471:	add8i	$AE, L531D, $01
	lda     $B1C8                           ; 5479 AD C8 B1                 ...
	eor     $AE                             ; 547C 45 AE                    E.
	lbne	L54E3
	jsr     L537F                           ; 5483 20 7F 53                  .S
	lda     $B1C8                           ; 5486 AD C8 B1                 ...
	sta     L531D                           ; 5489 8D 1D 53                 ..S
	lda     $B1C6                           ; 548C AD C6 B1                 ...
	and     #$80                            ; 548F 29 80                    ).
	sta     $AE                             ; 5491 85 AE                    ..
	lda     $AE                             ; 5493 A5 AE                    ..
	lbeq	L54B8
	lda     #$B1                            ; 549A A9 B1                    ..
	sta     $A3                             ; 549C 85 A3                    ..
	lda     $B1C6                           ; 549E AD C6 B1                 ...
	and     #$7F                            ; 54A1 29 7F                    ).
	sta     $AE                             ; 54A3 85 AE                    ..
	sec                                     ; 54A5 38                       8
	lda     $AE                             ; 54A6 A5 AE                    ..
	sbc     #$04                            ; 54A8 E9 04                    ..
	sta     $A4                             ; 54AA 85 A4                    ..
	ldy     #$C9                            ; 54AC A0 C9                    ..
	ldxai	$B224
	jsr     L5197                           ; 54B2 20 97 51                  .Q
	jmp     L54D6                           ; 54B5 4C D6 54                 L.T

; ----------------------------------------------------------------------------
L54B8:  sec                                     ; 54B8 38                       8
	lda     $B1C6                           ; 54B9 AD C6 B1                 ...
	sbc     #$04                            ; 54BC E9 04                    ..
	sta     L4649                           ; 54BE 8D 49 46                 .IF
	lda     #$B1                            ; 54C1 A9 B1                    ..
	sta     $A3                             ; 54C3 85 A3                    ..
	lda     #$00                            ; 54C5 A9 00                    ..
	sta     $A5                             ; 54C7 85 A5                    ..
	lda     #$5C                            ; 54C9 A9 5C                    .\
	sta     $A4                             ; 54CB 85 A4                    ..
	ldy     #$C9                            ; 54CD A0 C9                    ..
	ldxai	$B224
	jsr     sub_461F
L54D6:  ldy     #$00                            ; 54D6 A0 00                    ..
	sty     L4651                           ; 54D8 8C 51 46                 .QF
	lda     #$01                            ; 54DB A9 01                    ..
	sta     $A0                             ; 54DD 85 A0                    ..
	rts                                     ; 54DF 60                       `

; ----------------------------------------------------------------------------
	jmp     L54F7                           ; 54E0 4C F7 54                 L.T

; ----------------------------------------------------------------------------
L54E3:  lda     $B1C8                           ; 54E3 AD C8 B1                 ...
	eor     L531D                           ; 54E6 4D 1D 53                 M.S
	beq     L54EE                           ; 54E9 F0 03                    ..
	jmp     L54F4                           ; 54EB 4C F4 54                 L.T

; ----------------------------------------------------------------------------
L54EE:  jsr     L537F                           ; 54EE 20 7F 53                  .S
	jmp     L54F7                           ; 54F1 4C F7 54                 L.T

; ----------------------------------------------------------------------------
L54F4:  jsr     L5373                           ; 54F4 20 73 53                  sS
L54F7:  lda     #$00                            ; 54F7 A9 00                    ..
	sta     $A0                             ; 54F9 85 A0                    ..
	rts                                     ; 54FB 60                       `

; ----------------------------------------------------------------------------
L54FC:  .byte   $44                             ; 54FC 44                       D
L54FD:  .byte   $6F                             ; 54FD 6F                       o
	.byte   $77                             ; 54FE 77                       w

L54FF:  prolog
	lda     L4650                           ; 5502 AD 50 46                 .PF
	lbeq	L552F
	jsr     L5394                           ; 550A 20 94 53                  .S
	lda     $A0                             ; 550D A5 A0                    ..
	sta     L54FC                           ; 550F 8D FC 54                 ..T
	lda     L54FC                           ; 5512 AD FC 54                 ..T
	eor     #$01                            ; 5515 49 01                    I.
	lbne	L552A
	jsr     L5461                           ; 551C 20 61 54                  aT
	lda     $A0                             ; 551F A5 A0                    ..
	sta     L54FC                           ; 5521 8D FC 54                 ..T
	lda     L54FC                           ; 5524 AD FC 54                 ..T
	sta     $A0                             ; 5527 85 A0                    ..
	rts                                     ; 5529 60                       `

; ----------------------------------------------------------------------------
L552A:  lda     #$00                            ; 552A A9 00                    ..
	sta     $A0                             ; 552C 85 A0                    ..
	rts                                     ; 552E 60                       `

; ----------------------------------------------------------------------------
L552F:  jsr     sub_4FC5
	lda     $A0                             ; 5532 A5 A0                    ..
	sta     L54FC                           ; 5534 8D FC 54                 ..T
	lda     L54FC                           ; 5537 AD FC 54                 ..T
	eor     #$04                            ; 553A 49 04                    I.
	beq     L5541                           ; 553C F0 03                    ..
	jmp     L5547                           ; 553E 4C 47 55                 LGU

; ----------------------------------------------------------------------------
L5541:  jsr     L5388                           ; 5541 20 88 53                  .S
	jmp     L5579                           ; 5544 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L5547:  lda     L54FC                           ; 5547 AD FC 54                 ..T
	eor     #$01                            ; 554A 49 01                    I.
	beq     L5551                           ; 554C F0 03                    ..
	jmp     L5579                           ; 554E 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L5551:  lda     $B1C9                           ; 5551 AD C9 B1                 ...
	sta     L54FD                           ; 5554 8D FD 54                 ..T
	lda     L54FD                           ; 5557 AD FD 54                 ..T
	eor     #$06                            ; 555A 49 06                    I.
	bne     L5561                           ; 555C D0 03                    ..
	jmp     L5579                           ; 555E 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L5561:  lda     L54FD                           ; 5561 AD FD 54                 ..T
	eor     #$15                            ; 5564 49 15                    I.
	bne     L556B                           ; 5566 D0 03                    ..
	jmp     L5579                           ; 5568 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L556B:  jsr     L5461                           ; 556B 20 61 54                  aT
	lda     $A0                             ; 556E A5 A0                    ..
	sta     L54FC                           ; 5570 8D FC 54                 ..T
	lda     L54FC                           ; 5573 AD FC 54                 ..T
	sta     $A0                             ; 5576 85 A0                    ..
	rts                                     ; 5578 60                       `

; ----------------------------------------------------------------------------
L5579:  .byte   $A9                             ; 5579 A9                       .
L557A:  brk                                     ; 557A 00                       .
	sta     $A0                             ; 557B 85 A0                    ..
	rts                                     ; 557D 60                       `

; ----------------------------------------------------------------------------
L557E:  .byte   $30                             ; 557E 30                       0
L557F:  eor     a:$03,x                         ; 557F 5D 03 00                 ]..
	brk                                     ; 5582 00                       .
	ora     ($20),y                         ; 5583 11 20                    .
	.byte	"   " 
	.byte	"   " 
	.byte   $73                             ; 558B 73                       s
L558C:  .byte   $69                             ; 558C 69                       i
L558D:  .byte   $7A                             ; 558D 7A                       z
L558E:  .byte   $65                             ; 558E 65                       e
L558F:  .byte   $20                             ; 558F 20                        
L5590:  .byte   $3D                             ; 5590 3D                       =
L5591:  .byte   $3D                             ; 5591 3D                       =
L5592:  .byte   $2D                             ; 5592 2D                       -
L5593:  .byte   $20                             ; 5593 20                        
L5594:  .byte   $31                             ; 5594 31                       1
L5595:  cld                                     ; 5595 D8                       .
	dec     $1E,x                           ; 5596 D6 1E                    ..
L5598:  .byte   $02                             ; 5598 02                       .
L5599:  brk                                     ; 5599 00                       .
L559A:  brk                                     ; 559A 00                       .
L559B:  .byte   $12                             ; 559B 12                       .
L559C:  .byte   $20                             ; 559C 20                        
L559D:  .byte   $20                             ; 559D 20                        
L559E:  .byte   $20                             ; 559E 20                        
L559F:  .byte   $20                             ; 559F 20                       

sub_55A0:
	stack_prolog L557E, $0D
	lda     #$00                            ; 55A9 A9 00                    ..
	cmp     L4653                           ; 55AB CD 53 46                 .SF
	lbcs	L55B4
L55B3:  rts                                     ; 55B3 60                       `

; ----------------------------------------------------------------------------
L55B4:  lda     #$01                            ; 55B4 A9 01                    ..
	asl     a                               ; 55B6 0A                       .
	php                                     ; 55B7 08                       .
	clc                                     ; 55B8 18                       .
	adc     L466F                           ; 55B9 6D 6F 46                 moF
	sta     $AE                             ; 55BC 85 AE                    ..
	lda     #$00                            ; 55BE A9 00                    ..
	rol     a                               ; 55C0 2A                       *
	plp                                     ; 55C1 28                       (
	adc     L466F+1
	sta     $AF                             ; 55C5 85 AF                    ..
	ldy     #$01                            ; 55C7 A0 01                    ..
	lda     ($AE),y                         ; 55C9 B1 AE                    ..
	sta     L559B                           ; 55CB 8D 9B 55                 ..U
	dey                                     ; 55CE 88                       .
	lda     ($AE),y                         ; 55CF B1 AE                    ..
	sta     L559A                           ; 55D1 8D 9A 55                 ..U
	clc                                     ; 55D4 18                       .
	lda     L559A                           ; 55D5 AD 9A 55                 ..U
	adc     #$04                            ; 55D8 69 04                    i.
	sta     L559C                           ; 55DA 8D 9C 55                 ..U
L55DD:  lda     L559B                           ; 55DD AD 9B 55                 ..U
	adc     #$00                            ; 55E0 69 00                    i.
	sta     L559D                           ; 55E2 8D 9D 55                 ..U
	lda     L559C                           ; 55E5 AD 9C 55                 ..U
	sta     $AE                             ; 55E8 85 AE                    ..
	lda     L559D                           ; 55EA AD 9D 55                 ..U
	sta     $AF                             ; 55ED 85 AF                    ..
	clc                                     ; 55EF 18                       .
	lda     ($AE),y                         ; 55F0 B1 AE                    ..
	adc     #$02                            ; 55F2 69 02                    i.
	sta     L559E                           ; 55F4 8D 9E 55                 ..U
	iny                                     ; 55F7 C8                       .
	lda     ($AE),y                         ; 55F8 B1 AE                    ..
	adc     #$00                            ; 55FA 69 00                    i.
	sta     L559F                           ; 55FC 8D 9F 55                 ..U
	lda     #$55                            ; 55FF A9 55                    .U
	sta     L558D                           ; 5601 8D 8D 55                 ..U
	lda     #$80                            ; 5604 A9 80                    ..
	sta     L558C                           ; 5606 8D 8C 55                 ..U
	dey                                     ; 5609 88                       .
	sty     L5594                           ; 560A 8C 94 55                 ..U
	iny                                     ; 560D C8                       .
	sty     L5590                           ; 560E 8C 90 55                 ..U
	lda     L557E                           ; 5611 AD 7E 55                 .~U
	sta     $AE                             ; 5614 85 AE                    ..
	lda     L557F                           ; 5616 AD 7F 55                 ..U
	sta     $AF                             ; 5619 85 AF                    ..
	dey                                     ; 561B 88                       .
	lda     ($AE),y                         ; 561C B1 AE                    ..
	sta     L562C                           ; 561E 8D 2C 56                 .,V
L5621:  lda     L562C                           ; 5621 AD 2C 56                 .,V
	cmp     L5590                           ; 5624 CD 90 55                 ..U
	bcs     L562D                           ; 5627 B0 04                    ..
	jmp     L587E                           ; 5629 4C 7E 58                 L~X

; ----------------------------------------------------------------------------
L562C:  .byte   $30                             ; 562C 30                       0
L562D:  clc                                     ; 562D 18                       .
	lda     L557E                           ; 562E AD 7E 55                 .~U
	adc     L5590                           ; 5631 6D 90 55                 m.U
	sta     $AE                             ; 5634 85 AE                    ..
	lda     L557F                           ; 5636 AD 7F 55                 ..U
	adc     #$00                            ; 5639 69 00                    i.
	sta     $AF                             ; 563B 85 AF                    ..
	ldy     #$00                            ; 563D A0 00                    ..
	lda     ($AE),y                         ; 563F B1 AE                    ..
	sta     L5591                           ; 5641 8D 91 55                 ..U
	lda     L5591                           ; 5644 AD 91 55                 ..U
	eor     #$43                            ; 5647 49 43                    IC
	beq     L564E                           ; 5649 F0 03                    ..
	jmp     L5672                           ; 564B 4C 72 56                 LrV

; ----------------------------------------------------------------------------
L564E:  clc                                     ; 564E 18                       .
	lda     L559E                           ; 564F AD 9E 55                 ..U
	adc     L5594                           ; 5652 6D 94 55                 m.U
	sta     $AE                             ; 5655 85 AE                    ..
	lda     L559F                           ; 5657 AD 9F 55                 ..U
	adc     #$00                            ; 565A 69 00                    i.
	sta     $AF                             ; 565C 85 AF                    ..
	lda     L558C                           ; 565E AD 8C 55                 ..U
	sta     $AC                             ; 5661 85 AC                    ..
	lda     L558D                           ; 5663 AD 8D 55                 ..U
	sta     $AD                             ; 5666 85 AD                    ..
	lda     ($AC),y                         ; 5668 B1 AC                    ..
	sta     ($AE),y                         ; 566A 91 AE                    ..
	inc     L5594                           ; 566C EE 94 55                 ..U
	jmp     L5867                           ; 566F 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L5672:  lda     L5591                           ; 5672 AD 91 55                 ..U
	eor     #$63                            ; 5675 49 63                    Ic
	beq     L567C                           ; 5677 F0 03                    ..
	jmp     L56B7                           ; 5679 4C B7 56                 L.V

; ----------------------------------------------------------------------------
L567C:  clc                                     ; 567C 18                       .
	lda     L559E                           ; 567D AD 9E 55                 ..U
	adc     L5594                           ; 5680 6D 94 55                 m.U
	sta     $AE                             ; 5683 85 AE                    ..
	lda     L559F                           ; 5685 AD 9F 55                 ..U
	adc     #$00                            ; 5688 69 00                    i.
	sta     $AF                             ; 568A 85 AF                    ..
	lda     $AF                             ; 568C A5 AF                    ..
	pha                                     ; 568E 48                       H
	lda     $AE                             ; 568F A5 AE                    ..
	pha                                     ; 5691 48                       H
	lda     L558C                           ; 5692 AD 8C 55                 ..U
	sta     $AE                             ; 5695 85 AE                    ..
	lda     L558D                           ; 5697 AD 8D 55                 ..U
	sta     $AF                             ; 569A 85 AF                    ..
	lda     (off_AE),y
	sta     $A0                             ; 569E 85 A0                    ..
	lda     $A0                             ; 56A0 A5 A0                    ..
	jsr     L4BC9                           ; 56A2 20 C9 4B                  .K
	pla                                     ; 56A5 68                       h
	sta     $AE                             ; 56A6 85 AE                    ..
	pla                                     ; 56A8 68                       h
	sta     $AF                             ; 56A9 85 AF                    ..
	lda     $A0                             ; 56AB A5 A0                    ..
	ldy     #$00                            ; 56AD A0 00                    ..
	sta     ($AE),y                         ; 56AF 91 AE                    ..
	inc     L5594                           ; 56B1 EE 94 55                 ..U
	jmp     L5867                           ; 56B4 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L56B7:  lda     L5591                           ; 56B7 AD 91 55                 ..U
	eor     #$42                            ; 56BA 49 42                    IB
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
	eor     #$48                            ; 56F7 49 48                    IH
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
	eor     #$53                            ; 5777 49 53                    IS
	beq     L5785                           ; 5779 F0 0A                    ..
	lda     L5591                           ; 577B AD 91 55                 ..U
	eor     #$73                            ; 577E 49 73                    Is
	beq     L5785                           ; 5780 F0 03                    ..
	jmp     L5867                           ; 5782 4C 67 58                 LgX

; ----------------------------------------------------------------------------
L5785:	dmv	off_AE, L558C
	ldy     #$00                            ; 578F A0 00                    ..
	lda     ($AE),y                         ; 5791 B1 AE                    ..
	sta     L5592                           ; 5793 8D 92 55                 ..U
	dmv	off_AE, L558C
	lda     ($AE),y                         ; 57A0 B1 AE                    ..
	sta     $A0                             ; 57A2 85 A0                    ..
	clc                                     ; 57A4 18                       .
	lda     L559E                           ; 57A5 AD 9E 55                 ..U
	adc     L5594                           ; 57A8 6D 94 55                 m.U
	sta     $A1                             ; 57AB 85 A1                    ..
	lda     L559F                           ; 57AD AD 9F 55                 ..U
	adc     #$00                            ; 57B0 69 00                    i.
	sta     $A2                             ; 57B2 85 A2                    ..
	ldy     $A2                             ; 57B4 A4 A2                    ..
	ldx     $A1                             ; 57B6 A6 A1                    ..
	lda     $A0                             ; 57B8 A5 A0                    ..
	jsr     tohex
	clc                                     ; 57BD 18                       .
	lda     L5594                           ; 57BE AD 94 55                 ..U
	adc     #$02                            ; 57C1 69 02                    i.
	sta     L5594                           ; 57C3 8D 94 55                 ..U
	clc                                     ; 57C6 18                       .
	lda     L558C                           ; 57C7 AD 8C 55                 ..U
	adc     #$02                            ; 57CA 69 02                    i.
	sta     L558C                           ; 57CC 8D 8C 55                 ..U
	lda     L558D                           ; 57CF AD 8D 55                 ..U
	adc     #$00                            ; 57D2 69 00                    i.
	sta     L558D                           ; 57D4 8D 8D 55                 ..U
	lda     L5592                           ; 57D7 AD 92 55                 ..U
	beq     L57DF                           ; 57DA F0 03                    ..
	jmp     L57E2                           ; 57DC 4C E2 57                 L.W

; ----------------------------------------------------------------------------
L57DF:  jmp     L587E                           ; 57DF 4C 7E 58                 L~X

; ----------------------------------------------------------------------------
L57E2:	dmv	off_AE, L558C
	ldy     #$01                            ; 57EC A0 01                    ..
	lda     ($AE),y                         ; 57EE B1 AE                    ..
	sta     L5599                           ; 57F0 8D 99 55                 ..U
	dey                                     ; 57F3 88                       .
	lda     ($AE),y                         ; 57F4 B1 AE                    ..
	sta     L5598                           ; 57F6 8D 98 55                 ..U
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
L581A:  .byte   $73                             ; 581A 73                       s

; ----------------------------------------------------------------------------
L581B:	add16m8 off_AE, L5598, L5593
	ldy     #$00                            ; 582B A0 00                    ..
	lda     ($AE),y                         ; 582D B1 AE                    ..
	sta     L5595                           ; 582F 8D 95 55                 ..U
	lda     L5591                           ; 5832 AD 91 55                 ..U
	eor     #$53                            ; 5835 49 53                    IS
	lbne	L5847
	lda     L5595                           ; 583C AD 95 55                 ..U
	jsr     L4BC9                           ; 583F 20 C9 4B                  .K
	lda     $A0                             ; 5842 A5 A0                    ..
	sta     L5595                           ; 5844 8D 95 55                 ..U
L5847:  clc                                     ; 5847 18                       .
	lda     L559E                           ; 5848 AD 9E 55                 ..U
	adc     L5594                           ; 584B 6D 94 55                 m.U
	sta     $AE                             ; 584E 85 AE                    ..
	lda     L559F                           ; 5850 AD 9F 55                 ..U
	adc     #$00                            ; 5853 69 00                    i.
	sta     $AF                             ; 5855 85 AF                    ..
	lda     L5595                           ; 5857 AD 95 55                 ..U
	ldy     #$00                            ; 585A A0 00                    ..
	sta     ($AE),y                         ; 585C 91 AE                    ..
	inc     L5594                           ; 585E EE 94 55                 ..U
	inc     L5593                           ; 5861 EE 93 55                 ..U
	jmp     L580F                           ; 5864 4C 0F 58                 L.X

; ----------------------------------------------------------------------------
L5867:  clc                                     ; 5867 18                       .
	lda     L558C                           ; 5868 AD 8C 55                 ..U
	adc     #$02                            ; 586B 69 02                    i.
	sta     L558C                           ; 586D 8D 8C 55                 ..U
	lda     L558D                           ; 5870 AD 8D 55                 ..U
	adc     #$00                            ; 5873 69 00                    i.
	sta     L558D                           ; 5875 8D 8D 55                 ..U
	inc     L5590                           ; 5878 EE 90 55                 ..U
	jmp     L5621                           ; 587B 4C 21 56                 L!V

; ----------------------------------------------------------------------------
L587E:  clc                                     ; 587E 18                       .
	lda     L5594                           ; 587F AD 94 55                 ..U
	adc     #$04                            ; 5882 69 04                    i.
	sta     L5592                           ; 5884 8D 92 55                 ..U
L5887:  sec                                     ; 5887 38                       8
	lda     #$59                            ; 5888 A9 59                    .Y
	sbc     L4650                           ; 588A ED 50 46                 .PF
	sta     $AE                             ; 588D 85 AE                    ..
	lda     $AE                             ; 588F A5 AE                    ..
	cmp     L5592                           ; 5891 CD 92 55                 ..U
	bcc     L5899                           ; 5894 90 03                    ..
	jmp     L589F                           ; 5896 4C 9F 58                 L.X

; ----------------------------------------------------------------------------
L5899:  jsr     L5394                           ; 5899 20 94 53                  .S
	jmp     L5887                           ; 589C 4C 87 58                 L.X

; ----------------------------------------------------------------------------
L589F:  clc                                     ; 589F 18                       .
	lda     L474C                           ; 58A0 AD 4C 47                 .LG
	adc     #$01                            ; 58A3 69 01                    i.
	sta     $A1                             ; 58A5 85 A1                    ..
	lda     L559F                           ; 58A7 AD 9F 55                 ..U
	sta     $A3                             ; 58AA 85 A3                    ..
	clc                                     ; 58AC 18                       .
	lda     #$6A                            ; 58AD A9 6A                    .j
	adc     L4650                           ; 58AF 6D 50 46                 mPF
	sta     $A4                             ; 58B2 85 A4                    ..
	lda     #$B1                            ; 58B4 A9 B1                    ..
	adc     #$00                            ; 58B6 69 00                    i.
	sta     $A5                             ; 58B8 85 A5                    ..
	ldy     L559E                           ; 58BA AC 9E 55                 ..U
	ldx     $A1                             ; 58BD A6 A1                    ..
	lda     L5594                           ; 58BF AD 94 55                 ..U
	jsr     L51F7                           ; 58C2 20 F7 51                  .Q
	clc                                     ; 58C5 18                       .
	lda     L4650                           ; 58C6 AD 50 46                 .PF
	adc     L5592                           ; 58C9 6D 92 55                 m.U
	sta     L4650                           ; 58CC 8D 50 46                 .PF
	lda     L4650                           ; 58CF AD 50 46                 .PF
	eor     L5592                           ; 58D2 4D 92 55                 M.U
	beq     L58DA                           ; 58D5 F0 03                    ..
	jmp     L58E6                           ; 58D7 4C E6 58                 L.X

; ----------------------------------------------------------------------------
L58DA:  ldy     #$00                            ; 58DA A0 00                    ..
	sty     L464B                           ; 58DC 8C 4B 46                 .KF
	ldxai	$B16A
	jsr     sub_52E1
L58E6:  rts                                     ; 58E6 60                       `

; ----------------------------------------------------------------------------
L58E7:  .byte   $44                             ; 58E7 44                       D
L58E8:  plp                                     ; 58E8 28                       (
L58E9:  .byte   $20                             ; 58E9 20                        
L58EA:  .byte   $24                             ; 58EA 24                       $
L58EB:  .byte   $46                             ; 58EB 46                       F
L58EC:  .byte   $44                             ; 58EC 44                       D
L58ED:  .byte   $29                             ; 58ED 29                       )
L58EE:  .byte   $20                             ; 58EE 20                        
L58EF:  .byte   $20                             ; 58EF 20                        
L58F0:  .byte   $20                             ; 58F0 20                        
L58F1:  .byte   $20                             ; 58F1 20                        

sub_58F2:
	prolog
	jsr     sub_44D5                           ; 58F5 20 D5 44                  .D
	.addr	L58E7
	.byte	$03
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
L591E:  .byte   $20                             ; 591E 20                        

; ----------------------------------------------------------------------------
L591F:	add16m8	off_AE, L58E7, L58ED
	ldy     #$00                            ; 592F A0 00                    ..
	lda     ($AE),y                         ; 5931 B1 AE                    ..
	sta     L58EE                           ; 5933 8D EE 58                 ..X
	lda     L58EE                           ; 5936 AD EE 58                 ..X
	eor     #$41                            ; 5939 49 41                    IA
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
	clc                                     ; 5966 18                       .
	lda     L58E9                           ; 5967 AD E9 58                 ..X
	adc     #$02                            ; 596A 69 02                    i.
	sta     L58E9                           ; 596C 8D E9 58                 ..X
	lda     L58EA                           ; 596F AD EA 58                 ..X
	adc     #$00                            ; 5972 69 00                    i.
	sta     L58EA                           ; 5974 8D EA 58                 ..X
	inc     L58ED                           ; 5977 EE ED 58                 ..X
	clc                                     ; 597A 18                       .
	lda     L58E7                           ; 597B AD E7 58                 ..X
	adc     L58ED                           ; 597E 6D ED 58                 m.X
	sta     $AE                             ; 5981 85 AE                    ..
	lda     L58E8                           ; 5983 AD E8 58                 ..X
	adc     #$00                            ; 5986 69 00                    i.
	sta     $AF                             ; 5988 85 AF                    ..
	lda     ($AE),y                         ; 598A B1 AE                    ..
	sta     $A0                             ; 598C 85 A0                    ..
	lda     $A0                             ; 598E A5 A0                    ..
	jsr     L4B39                           ; 5990 20 39 4B                  9K
	lda     $A0                             ; 5993 A5 A0                    ..
	sta     L58EE                           ; 5995 8D EE 58                 ..X
	ldy     #$00                            ; 5998 A0 00                    ..
	sty     L58EF                           ; 599A 8C EF 58                 ..X
	lda     L58EE                           ; 599D AD EE 58                 ..X
	beq     L59A5                           ; 59A0 F0 03                    ..
	jmp     L59E1                           ; 59A2 4C E1 59                 L.Y

; ----------------------------------------------------------------------------
L59A5:  clc                                     ; 59A5 18                       .
	lda     #$24                            ; 59A6 A9 24                    .$
	adc     L4654                           ; 59A8 6D 54 46                 mTF
	sta     $A0                             ; 59AB 85 A0                    ..
	lda     #$B2                            ; 59AD A9 B2                    ..
	adc     #$00                            ; 59AF 69 00                    i.
	sta     $A1                             ; 59B1 85 A1                    ..
	ldx     $A1                             ; 59B3 A6 A1                    ..
	lda     $A0                             ; 59B5 A5 A0                    ..
	jsr     L4B47                           ; 59B7 20 47 4B                  GK
	lda     $A0                             ; 59BA A5 A0                    ..
	sta     L58EE                           ; 59BC 8D EE 58                 ..X
	clc                                     ; 59BF 18                       .
	lda     L4654                           ; 59C0 AD 54 46                 .TF
	adc     #$02                            ; 59C3 69 02                    i.
	sta     L4654                           ; 59C5 8D 54 46                 .TF
	lda     L58EB                           ; 59C8 AD EB 58                 ..X
	sta     $AE                             ; 59CB 85 AE                    ..
	lda     L58EC                           ; 59CD AD EC 58                 ..X
	sta     $AF                             ; 59D0 85 AF                    ..
	lda     L58EE                           ; 59D2 AD EE 58                 ..X
	ldy     #$00                            ; 59D5 A0 00                    ..
	sta     ($AE),y                         ; 59D7 91 AE                    ..
	inc     L58EB                           ; 59D9 EE EB 58                 ..X
	bne     L59E1                           ; 59DC D0 03                    ..
	inc     L58EC                           ; 59DE EE EC 58                 ..X
L59E1:  lda     L58EF                           ; 59E1 AD EF 58                 ..X
	cmp     L58EE                           ; 59E4 CD EE 58                 ..X
	lbcs	L5A32
	clc                                     ; 59EC 18                       .
	lda     L58EB                           ; 59ED AD EB 58                 ..X
	adc     L58EF                           ; 59F0 6D EF 58                 m.X
	sta     $AE                             ; 59F3 85 AE                    ..
	lda     L58EC                           ; 59F5 AD EC 58                 ..X
	adc     #$00                            ; 59F8 69 00                    i.
	sta     $AF                             ; 59FA 85 AF                    ..
	lda     $AF                             ; 59FC A5 AF                    ..
	pha                                     ; 59FE 48                       H
	lda     $AE                             ; 59FF A5 AE                    ..
	pha                                     ; 5A01 48                       H
	clc                                     ; 5A02 18                       .
	lda     #$24                            ; 5A03 A9 24                    .$
	adc     L4654                           ; 5A05 6D 54 46                 mTF
	sta     $A0                             ; 5A08 85 A0                    ..
	lda     #$B2                            ; 5A0A A9 B2                    ..
	adc     #$00                            ; 5A0C 69 00                    i.
	sta     $A1                             ; 5A0E 85 A1                    ..
	ldx     $A1                             ; 5A10 A6 A1                    ..
	lda     $A0                             ; 5A12 A5 A0                    ..
	jsr     L4B47                           ; 5A14 20 47 4B                  GK
	pla                                     ; 5A17 68                       h
	sta     $AE                             ; 5A18 85 AE                    ..
	pla                                     ; 5A1A 68                       h
	sta     $AF                             ; 5A1B 85 AF                    ..
	lda     $A0                             ; 5A1D A5 A0                    ..
	ldy     #$00                            ; 5A1F A0 00                    ..
	sta     ($AE),y                         ; 5A21 91 AE                    ..
	clc                                     ; 5A23 18                       .
	lda     L4654                           ; 5A24 AD 54 46                 .TF
	adc     #$02                            ; 5A27 69 02                    i.
	sta     L4654                           ; 5A29 8D 54 46                 .TF
	inc     L58EF                           ; 5A2C EE EF 58                 ..X
	jmp     L59E1                           ; 5A2F 4C E1 59                 L.Y

; ----------------------------------------------------------------------------
L5A32:  jmp     L5CF5                           ; 5A32 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A35:  lda     L58EE                           ; 5A35 AD EE 58                 ..X
	eor     #$42                            ; 5A38 49 42                    IB
	lbne	L5A90
	lda     L58EA                           ; 5A3F AD EA 58                 ..X
	sta     L58EC                           ; 5A42 8D EC 58                 ..X
	lda     L58E9                           ; 5A45 AD E9 58                 ..X
	sta     L58EB                           ; 5A48 8D EB 58                 ..X
	inc     L58E9                           ; 5A4B EE E9 58                 ..X
	bne     L5A53                           ; 5A4E D0 03                    ..
	inc     L58EA                           ; 5A50 EE EA 58                 ..X
L5A53:  lda     L58EB                           ; 5A53 AD EB 58                 ..X
	sta     $AE                             ; 5A56 85 AE                    ..
	lda     L58EC                           ; 5A58 AD EC 58                 ..X
	sta     $AF                             ; 5A5B 85 AF                    ..
	lda     $AF                             ; 5A5D A5 AF                    ..
	pha                                     ; 5A5F 48                       H
	lda     $AE                             ; 5A60 A5 AE                    ..
	pha                                     ; 5A62 48                       H
	clc                                     ; 5A63 18                       .
	lda     #$24                            ; 5A64 A9 24                    .$
	adc     L4654                           ; 5A66 6D 54 46                 mTF
	sta     $A0                             ; 5A69 85 A0                    ..
	lda     #$B2                            ; 5A6B A9 B2                    ..
	adc     #$00                            ; 5A6D 69 00                    i.
	sta     $A1                             ; 5A6F 85 A1                    ..
	ldx     $A1                             ; 5A71 A6 A1                    ..
	lda     $A0                             ; 5A73 A5 A0                    ..
	jsr     L4B47                           ; 5A75 20 47 4B                  GK
	pla                                     ; 5A78 68                       h
	sta     $AE                             ; 5A79 85 AE                    ..
	pla                                     ; 5A7B 68                       h
	sta     $AF                             ; 5A7C 85 AF                    ..
	lda     $A0                             ; 5A7E A5 A0                    ..
	ldy     #$00                            ; 5A80 A0 00                    ..
	sta     ($AE),y                         ; 5A82 91 AE                    ..
	add8i	L4654, L4654, $02
	jmp     L5CF5                           ; 5A8D 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5A90:  lda     L58EE                           ; 5A90 AD EE 58                 ..X
	eor     #$43                            ; 5A93 49 43                    IC
	lbne	L5ADD
	rdmv	L58EB, L58E9
	inc     L58E9                           ; 5AA6 EE E9 58                 ..X
	bne     L5AAE                           ; 5AA9 D0 03                    ..
	inc     L58EA                           ; 5AAB EE EA 58                 ..X
L5AAE:	dmv	off_AE, L58EB
	lda     $AF                             ; 5AB8 A5 AF                    ..
	pha                                     ; 5ABA 48                       H
	lda     $AE                             ; 5ABB A5 AE                    ..
	pha                                     ; 5ABD 48                       H
	ldx     L4654                           ; 5ABE AE 54 46                 .TF
	lda     $B224,x                         ; 5AC1 BD 24 B2                 .$.
	sta     $A0                             ; 5AC4 85 A0                    ..
	lda     $A0                             ; 5AC6 A5 A0                    ..
	jsr     L4B7B                           ; 5AC8 20 7B 4B                  {K
	pla                                     ; 5ACB 68                       h
	sta     $AE                             ; 5ACC 85 AE                    ..
	pla                                     ; 5ACE 68                       h
	sta     $AF                             ; 5ACF 85 AF                    ..
	lda     $A0                             ; 5AD1 A5 A0                    ..
	ldy     #$00                            ; 5AD3 A0 00                    ..
	sta     ($AE),y                         ; 5AD5 91 AE                    ..
	inc     L4654                           ; 5AD7 EE 54 46                 .TF
	jmp     L5CF5                           ; 5ADA 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5ADD:  lda     L58EE                           ; 5ADD AD EE 58                 ..X
	eor     #$44                            ; 5AE0 49 44                    ID
	lbne	L5B2A
	lda     L58EA                           ; 5AE7 AD EA 58                 ..X
	sta     L58EC                           ; 5AEA 8D EC 58                 ..X
	lda     L58E9                           ; 5AED AD E9 58                 ..X
	sta     L58EB                           ; 5AF0 8D EB 58                 ..X
	inc     L58E9                           ; 5AF3 EE E9 58                 ..X
	bne     L5AFB                           ; 5AF6 D0 03                    ..
	inc     L58EA                           ; 5AF8 EE EA 58                 ..X
L5AFB:  lda     L58EB                           ; 5AFB AD EB 58                 ..X
	sta     $AE                             ; 5AFE 85 AE                    ..
	lda     L58EC                           ; 5B00 AD EC 58                 ..X
	sta     $AF                             ; 5B03 85 AF                    ..
	lda     $AF                             ; 5B05 A5 AF                    ..
	pha                                     ; 5B07 48                       H
	lda     $AE                             ; 5B08 A5 AE                    ..
	pha                                     ; 5B0A 48                       H
	ldx     L4654                           ; 5B0B AE 54 46                 .TF
	lda     $B224,x                         ; 5B0E BD 24 B2                 .$.
	sta     $A0                             ; 5B11 85 A0                    ..
	lda     $A0                             ; 5B13 A5 A0                    ..
	jsr     L4B39                           ; 5B15 20 39 4B                  9K
	pla                                     ; 5B18 68                       h
	sta     $AE                             ; 5B19 85 AE                    ..
	pla                                     ; 5B1B 68                       h
	sta     $AF                             ; 5B1C 85 AF                    ..
	lda     $A0                             ; 5B1E A5 A0                    ..
	ldy     #$00                            ; 5B20 A0 00                    ..
	sta     ($AE),y                         ; 5B22 91 AE                    ..
	inc     L4654                           ; 5B24 EE 54 46                 .TF
	jmp     L5CF5                           ; 5B27 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5B2A:  lda     L58EE                           ; 5B2A AD EE 58                 ..X
	eor     #$52                            ; 5B2D 49 52                    IR
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
	clc                                     ; 5B5A 18                       .
	lda     L58E9                           ; 5B5B AD E9 58                 ..X
	adc     #$02                            ; 5B5E 69 02                    i.
	sta     L58E9                           ; 5B60 8D E9 58                 ..X
	lda     L58EA                           ; 5B63 AD EA 58                 ..X
	adc     #$00                            ; 5B66 69 00                    i.
	sta     L58EA                           ; 5B68 8D EA 58                 ..X
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
	lda     $AF                             ; 5B88 A5 AF                    ..
	pha                                     ; 5B8A 48                       H
	lda     $AE                             ; 5B8B A5 AE                    ..
	pha                                     ; 5B8D 48                       H
	clc                                     ; 5B8E 18                       .
	lda     #$24                            ; 5B8F A9 24                    .$
	adc     L4654                           ; 5B91 6D 54 46                 mTF
	sta     $A0                             ; 5B94 85 A0                    ..
	lda     #$B2                            ; 5B96 A9 B2                    ..
	adc     #$00                            ; 5B98 69 00                    i.
	sta     $A1                             ; 5B9A 85 A1                    ..
	ldx     $A1                             ; 5B9C A6 A1                    ..
	lda     $A0                             ; 5B9E A5 A0                    ..
	jsr     L4B47                           ; 5BA0 20 47 4B                  GK
	pla                                     ; 5BA3 68                       h
	sta     $AE                             ; 5BA4 85 AE                    ..
	pla                                     ; 5BA6 68                       h
	sta     $AF                             ; 5BA7 85 AF                    ..
	lda     $A0                             ; 5BA9 A5 A0                    ..
	ldy     #$00                            ; 5BAB A0 00                    ..
	sta     ($AE),y                         ; 5BAD 91 AE                    ..
	clc                                     ; 5BAF 18                       .
	lda     L4654                           ; 5BB0 AD 54 46                 .TF
	adc     #$02                            ; 5BB3 69 02                    i.
	sta     L4654                           ; 5BB5 8D 54 46                 .TF
	inc     L58EF                           ; 5BB8 EE EF 58                 ..X
	jmp     L5B6E                           ; 5BBB 4C 6E 5B                 Ln[

; ----------------------------------------------------------------------------
L5BBE:  jmp     L5CF5                           ; 5BBE 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5BC1:  lda     L58EE                           ; 5BC1 AD EE 58                 ..X
	eor     #$53                            ; 5BC4 49 53                    IS
	beq     L5BD2                           ; 5BC6 F0 0A                    ..
	lda     L58EE                           ; 5BC8 AD EE 58                 ..X
	eor     #$73                            ; 5BCB 49 73                    Is
	beq     L5BD2                           ; 5BCD F0 03                    ..
	jmp     L5C95                           ; 5BCF 4C 95 5C                 L.\

; ----------------------------------------------------------------------------
L5BD2:  clc                                     ; 5BD2 18                       .
	lda     #$24                            ; 5BD3 A9 24                    .$
	adc     L4654                           ; 5BD5 6D 54 46                 mTF
	sta     L58EB                           ; 5BD8 8D EB 58                 ..X
	lda     #$B2                            ; 5BDB A9 B2                    ..
	adc     #$00                            ; 5BDD 69 00                    i.
	sta     L58EC                           ; 5BDF 8D EC 58                 ..X
	lda     L58E9                           ; 5BE2 AD E9 58                 ..X
	sta     $AE                             ; 5BE5 85 AE                    ..
	lda     L58EA                           ; 5BE7 AD EA 58                 ..X
	sta     $AF                             ; 5BEA 85 AF                    ..
	lda     L58EC                           ; 5BEC AD EC 58                 ..X
	ldy     #$01                            ; 5BEF A0 01                    ..
	sta     ($AE),y                         ; 5BF1 91 AE                    ..
	lda     L58EB                           ; 5BF3 AD EB 58                 ..X
	dey                                     ; 5BF6 88                       .
	sta     ($AE),y                         ; 5BF7 91 AE                    ..
	clc                                     ; 5BF9 18                       .
	lda     L58E9                           ; 5BFA AD E9 58                 ..X
	adc     #$02                            ; 5BFD 69 02                    i.
	sta     L58E9                           ; 5BFF 8D E9 58                 ..X
	lda     L58EA                           ; 5C02 AD EA 58                 ..X
	adc     #$00                            ; 5C05 69 00                    i.
	sta     L58EA                           ; 5C07 8D EA 58                 ..X
	clc                                     ; 5C0A 18                       .
	lda     #$24                            ; 5C0B A9 24                    .$
	adc     L4654                           ; 5C0D 6D 54 46                 mTF
	sta     $A0                             ; 5C10 85 A0                    ..
	lda     #$B2                            ; 5C12 A9 B2                    ..
	adc     #$00                            ; 5C14 69 00                    i.
	sta     $A1                             ; 5C16 85 A1                    ..
	ldx     $A1                             ; 5C18 A6 A1                    ..
	lda     $A0                             ; 5C1A A5 A0                    ..
	jsr     L4B47                           ; 5C1C 20 47 4B                  GK
	lda     $A0                             ; 5C1F A5 A0                    ..
	sta     L58F0                           ; 5C21 8D F0 58                 ..X
	lda     L58EB                           ; 5C24 AD EB 58                 ..X
	sta     $AE                             ; 5C27 85 AE                    ..
	lda     L58EC                           ; 5C29 AD EC 58                 ..X
	sta     $AF                             ; 5C2C 85 AF                    ..
	lda     L58F0                           ; 5C2E AD F0 58                 ..X
	ldy     #$00                            ; 5C31 A0 00                    ..
	sta     ($AE),y                         ; 5C33 91 AE                    ..
	clc                                     ; 5C35 18                       .
	lda     L4654                           ; 5C36 AD 54 46                 .TF
	adc     #$02                            ; 5C39 69 02                    i.
	sta     L4654                           ; 5C3B 8D 54 46                 .TF
	iny                                     ; 5C3E C8                       .
	sty     L58EF                           ; 5C3F 8C EF 58                 ..X
	lda     L58F0                           ; 5C42 AD F0 58                 ..X
	sta     L5C53                           ; 5C45 8D 53 5C                 .S\
L5C48:  lda     L5C53                           ; 5C48 AD 53 5C                 .S\
	cmp     L58EF                           ; 5C4B CD EF 58                 ..X
	bcs     L5C54                           ; 5C4E B0 04                    ..
	jmp     L5C92                           ; 5C50 4C 92 5C                 L.\

; ----------------------------------------------------------------------------
L5C53:  brk                                     ; 5C53 00                       .
L5C54:  ldx     L4654                           ; 5C54 AE 54 46                 .TF
	lda     $B224,x                         ; 5C57 BD 24 B2                 .$.
	sta     L58F1                           ; 5C5A 8D F1 58                 ..X
	lda     L58EE                           ; 5C5D AD EE 58                 ..X
	eor     #$53                            ; 5C60 49 53                    IS
	beq     L5C67                           ; 5C62 F0 03                    ..
	jmp     L5C72                           ; 5C64 4C 72 5C                 Lr\

; ----------------------------------------------------------------------------
L5C67:  lda     L58F1                           ; 5C67 AD F1 58                 ..X
	jsr     L4B7B                           ; 5C6A 20 7B 4B                  {K
	lda     $A0                             ; 5C6D A5 A0                    ..
	sta     L58F1                           ; 5C6F 8D F1 58                 ..X
L5C72:  clc                                     ; 5C72 18                       .
	lda     L58EB                           ; 5C73 AD EB 58                 ..X
	adc     L58EF                           ; 5C76 6D EF 58                 m.X
	sta     $AE                             ; 5C79 85 AE                    ..
	lda     L58EC                           ; 5C7B AD EC 58                 ..X
	adc     #$00                            ; 5C7E 69 00                    i.
	sta     $AF                             ; 5C80 85 AF                    ..
	lda     L58F1                           ; 5C82 AD F1 58                 ..X
	ldy     #$00                            ; 5C85 A0 00                    ..
	sta     ($AE),y                         ; 5C87 91 AE                    ..
	inc     L4654                           ; 5C89 EE 54 46                 .TF
	inc     L58EF                           ; 5C8C EE EF 58                 ..X
	jmp     L5C48                           ; 5C8F 4C 48 5C                 LH\

; ----------------------------------------------------------------------------
L5C92:  jmp     L5CF5                           ; 5C92 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5C95:  lda     L58EE                           ; 5C95 AD EE 58                 ..X
	eor     #$58                            ; 5C98 49 58                    IX
	beq     L5C9F                           ; 5C9A F0 03                    ..
	jmp     L5CF5                           ; 5C9C 4C F5 5C                 L.\

; ----------------------------------------------------------------------------
L5C9F:  sec                                     ; 5C9F 38                       8
	lda     L4649                           ; 5CA0 AD 49 46                 .IF
	sbc     #$01                            ; 5CA3 E9 01                    ..
	sta     L58F0                           ; 5CA5 8D F0 58                 ..X
	lda     L58F0                           ; 5CA8 AD F0 58                 ..X
	sta     $B224                           ; 5CAB 8D 24 B2                 .$.
	ldy     #$01                            ; 5CAE A0 01                    ..
	sty     L58EF                           ; 5CB0 8C EF 58                 ..X
	lda     L58F0                           ; 5CB3 AD F0 58                 ..X
	sta     L5CC4                           ; 5CB6 8D C4 5C                 ..\
L5CB9:  lda     L5CC4                           ; 5CB9 AD C4 5C                 ..\
	cmp     L58EF                           ; 5CBC CD EF 58                 ..X
	bcs     L5CC5                           ; 5CBF B0 04                    ..
	jmp     L5CE0                           ; 5CC1 4C E0 5C                 L.\

; ----------------------------------------------------------------------------
L5CC4:  brk                                     ; 5CC4 00                       .
L5CC5:  ldx     L58EF                           ; 5CC5 AE EF 58                 ..X
	lda     $B224,x                         ; 5CC8 BD 24 B2                 .$.
	sta     $A0                             ; 5CCB 85 A0                    ..
	lda     $A0                             ; 5CCD A5 A0                    ..
	jsr     L4B7B                           ; 5CCF 20 7B 4B                  {K
	lda     $A0                             ; 5CD2 A5 A0                    ..
	ldx     L58EF                           ; 5CD4 AE EF 58                 ..X
	sta     $B224,x                         ; 5CD7 9D 24 B2                 .$.
	inc     L58EF                           ; 5CDA EE EF 58                 ..X
	jmp     L5CB9                           ; 5CDD 4C B9 5C                 L.\

; ----------------------------------------------------------------------------
L5CE0:  lda     L58E9                           ; 5CE0 AD E9 58                 ..X
	sta     $AE                             ; 5CE3 85 AE                    ..
	lda     L58EA                           ; 5CE5 AD EA 58                 ..X
	sta     $AF                             ; 5CE8 85 AF                    ..
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
L5D04:  lda     #$00                            ; 5D04 A9 00                    ..
	sta     $A3                             ; 5D06 85 A3                    ..
	ldy     #$13                            ; 5D08 A0 13                    ..
	ldxai	L5D02
	jsr     sub_55A0
L5D11:  lda     L4650                           ; 5D11 AD 50 46                 .PF
	lbeq	L5D1F
	jsr     L5394                           ; 5D19 20 94 53                  .S
	jmp     L5D11                           ; 5D1C 4C 11 5D                 L.]

; ----------------------------------------------------------------------------
L5D1F:  ldy     #$00                            ; 5D1F A0 00                    ..
	sty     $022F                           ; 5D21 8C 2F 02                 ./.
	lda     #$03                            ; 5D24 A9 03                    ..
	jsr     L4F6D                           ; 5D26 20 6D 4F                  mO
L5D29:  jsr     sub_4B07
	lda     $A0                             ; 5D2C A5 A0                    ..
	lbeq	L5D40
L5D33:  lda     #$02                            ; 5D33 A9 02                    ..
	jsr     L45A3                           ; 5D35 20 A3 45                  .E
	lda     #$02                            ; 5D38 A9 02                    ..
	jsr     L4F6D                           ; 5D3A 20 6D 4F                  mO
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
	jsr     L4569                           ; 5D60 20 69 45                  iE
L5D63:  rts                                     ; 5D63 60                       `

; ----------------------------------------------------------------------------
L5D64:  .byte   $4C                             ; 5D64 4C                       L
L5D65:  .byte   $67                             ; 5D65 67                       g
L5D66:  .byte   $5D                             ; 5D66 5D                       ]
L5D67:  prolog
	lda     $B148                           ; 5D6A AD 48 B1                 .H.
	eor     #$01                            ; 5D6D 49 01                    I.
	lbeq	L5DB9
	lda     #$02                            ; 5D74 A9 02                    ..
	jsr     L4569                           ; 5D76 20 69 45                  iE
	lda     #$0D                            ; 5D79 A9 0D                    ..
	sta     $A3                             ; 5D7B 85 A3                    ..
	ldy     #$4A                            ; 5D7D A0 4A                    .J
	ldx     #$A1                            ; 5D7F A2 A1                    ..
	lda     #$02                            ; 5D81 A9 02                    ..
	jsr     L4539                           ; 5D83 20 39 45                  9E
	lda     $B149                           ; 5D86 AD 49 B1                 .I.
	sta     $A3                             ; 5D89 85 A3                    ..
	lda     #$00                            ; 5D8B A9 00                    ..
	sta     $A4                             ; 5D8D 85 A4                    ..
	lda     #$4A                            ; 5D8F A9 4A                    .J
	sta     $A6                             ; 5D91 85 A6                    ..
	lda     #$A1                            ; 5D93 A9 A1                    ..
	sta     $A5                             ; 5D95 85 A5                    ..
	ldy     #$24                            ; 5D97 A0 24                    .$
	ldx     #$00                            ; 5D99 A2 00                    ..
	lda     #$02                            ; 5D9B A9 02                    ..
	jsr     sub_45D0
	lda     #$20                            ; 5DA0 A9 20                    . 
	sta     $A3                             ; 5DA2 85 A3                    ..
	lda     #$00                            ; 5DA4 A9 00                    ..
	sta     $A4                             ; 5DA6 85 A4                    ..
	lda     #$4A                            ; 5DA8 A9 4A                    .J
	sta     $A6                             ; 5DAA 85 A6                    ..
	lda     #$A1                            ; 5DAC A9 A1                    ..
	sta     $A5                             ; 5DAE 85 A5                    ..
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
	lda     #$4A                            ; 5DE8 A9 4A                    .J
	sta     $A6                             ; 5DEA 85 A6                    ..
	lda     #$A1                            ; 5DEC A9 A1                    ..
	sta     $A5                             ; 5DEE 85 A5                    ..
	ldy     #$59                            ; 5DF0 A0 59                    .Y
	ldx     #$00                            ; 5DF2 A2 00                    ..
	lda     #$02                            ; 5DF4 A9 02                    ..
	jsr     sub_45D0
L5DF9:  jsr     L5D64                           ; 5DF9 20 64 5D                  d]
	lda     #$2A                            ; 5DFC A9 2A                    .*
	sta     $022F                           ; 5DFE 8D 2F 02                 ./.
	lda     #$02                            ; 5E01 A9 02                    ..
	jsr     L4F6D                           ; 5E03 20 6D 4F                  mO
	ldy     #$00                            ; 5E06 A0 00                    ..
	sty     L4653                           ; 5E08 8C 53 46                 .SF
	jmp     L5E10                           ; 5E0B 4C 10 5E                 L.^

; ----------------------------------------------------------------------------
	ora     ($43,x)                         ; 5E0E 01 43                    .C
L5E10:  lda     #$00                            ; 5E10 A9 00                    ..
	sta     $A3                             ; 5E12 85 A3                    ..
	ldy     #$11                            ; 5E14 A0 11                    ..
	ldx     #$5E                            ; 5E16 A2 5E                    .^
	lda     #$0E                            ; 5E18 A9 0E                    ..
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
L5E30:
	prolog
	lda     L4653                           ; 5E33 AD 53 46                 .SF
	lbne	L5E3C
L5E3B:  rts                                     ; 5E3B 60                       `

; ----------------------------------------------------------------------------
L5E3C:  lda     L4653                           ; 5E3C AD 53 46                 .SF
	eor     #$01                            ; 5E3F 49 01                    I.
	lbne	L5E51
	jsr     L5D67                           ; 5E46 20 67 5D                  g]
	ldy     #$01                            ; 5E49 A0 01                    ..
	sty     L4656                           ; 5E4B 8C 56 46                 .VF
	jmp     L5E5A                           ; 5E4E 4C 5A 5E                 LZ^

; ----------------------------------------------------------------------------
L5E51:  sec                                     ; 5E51 38                       8
	lda     L4653                           ; 5E52 AD 53 46                 .SF
	sbc     #$01                            ; 5E55 E9 01                    ..
	sta     L4653                           ; 5E57 8D 53 46                 .SF
L5E5A:  rts                                     ; 5E5A 60                       `

; ----------------------------------------------------------------------------
L5E5B:  brk                                     ; 5E5B 00                       .
L5E5C:  brk                                     ; 5E5C 00                       .
L5E5D:  brk                                     ; 5E5D 00                       .

sub_5E5E:  
	prolog
	stx     L5E5C                           ; 5E61 8E 5C 5E                 .\^
	sta     L5E5B                           ; 5E64 8D 5B 5E                 .[^
	ldy     #$00                            ; 5E67 A0 00                    ..
	sty     L4AA4                           ; 5E69 8C A4 4A                 ..J
	lda     L4653                           ; 5E6C AD 53 46                 .SF
	sta     L5E5D                           ; 5E6F 8D 5D 5E                 .]^
L5E72:  lda     #$00                            ; 5E72 A9 00                    ..
	cmp     L4653                           ; 5E74 CD 53 46                 .SF
	lbcs	L5E82
	jsr     L5E30                           ; 5E7C 20 30 5E                  0^
	jmp     L5E72                           ; 5E7F 4C 72 5E                 Lr^

; ----------------------------------------------------------------------------
L5E82:  lda     L5E5B                           ; 5E82 AD 5B 5E                 .[^
	sta     L464D                           ; 5E85 8D 4D 46                 .MF
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
	ldx     #$5E                            ; 5EAB A2 5E                    .^
	lda     #$8B                            ; 5EAD A9 8B                    ..
	jsr     sub_55A0
L5EB2:  lda     L4653                           ; 5EB2 AD 53 46                 .SF
	cmp     L5E5D                           ; 5EB5 CD 5D 5E                 .]^
	bcc     L5EBD                           ; 5EB8 90 03                    ..
	jmp     L5EC3                           ; 5EBA 4C C3 5E                 L.^

; ----------------------------------------------------------------------------
L5EBD:  jsr     sub_5E1E
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
L5EFA:	brk                                     ; 5EFA 00                       .
	brk                                     ; 5EFB 00                       .
	brk                                     ; 5EFC 00                       .
	brk                                     ; 5EFD 00                       .

L5EFE:  prolog
	jsr     sub_44D5                        ; 5F01 20 D5 44                  .D
	.addr	L5EFA
	.byte	$03
L5F07:  brk                                     ; 5F07 00                       .
L5F08:  brk                                     ; 5F08 00                       .
L5F09:  brk                                     ; 5F09 00                       .
L5F0A:  brk                                     ; 5F0A 00                       .
L5F0B:  brk                                     ; 5F0B 00                       .
L5F0C:  brk                                     ; 5F0C 00                       .
L5F0D:  brk                                     ; 5F0D 00                       .
L5F0E:  brk                                     ; 5F0E 00                       .
L5F0F:  brk                                     ; 5F0F 00                       .
L5F10:  brk                                     ; 5F10 00                       .
L5F11:  brk                                     ; 5F11 00                       .
L5F12:  brk                                     ; 5F12 00                       .
L5F13:  brk                                     ; 5F13 00                       .
L5F14:  .byte   $80                             ; 5F14 80                       .
L5F15:  .byte   $B2                             ; 5F15 B2                       .

L5F16:  
	stack_prolog L5F07, $04
	ldi	$84, $03
	lda     L5F08                           ; 5F23 AD 08 5F                 .._
	tax                                     ; 5F26 AA                       .
	lda     L5F07                           ; 5F27 AD 07 5F                 .._
	jsr     sub_43E0
	sta     L5F0C                           ; 5F2D 8D 0C 5F                 .._
	txa                                     ; 5F30 8A                       .
	sta     L5F0D                           ; 5F31 8D 0D 5F                 .._
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
L5F86:  brk                                     ; 5F86 00                       .
L5F87:  brk                                     ; 5F87 00                       .

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
L5FF0:  brk                                     ; 5FF0 00                       .
L5FF1:  brk                                     ; 5FF1 00                       .
L5FF2:  brk                                     ; 5FF2 00                       .
L5FF3:  brk                                     ; 5FF3 00                       .
L5FF4:  brk                                     ; 5FF4 00                       .

L5FF5:  stack_prolog L5FF0, $04
	lda     #>sub_5EC4
	sta     L5EFE+2                         ; 6000 8D 00 5F                 .._
	lda     #<sub_5EC4
	sta     L5EFE+1                         ; 6005 8D FF 5E                 ..^
	lda     L5FF3                           ; 6008 AD F3 5F                 .._
	sta     $A3                             ; 600B 85 A3                    ..
	lda     L5FF4                           ; 600D AD F4 5F                 .._
	sta     $A4                             ; 6010 85 A4                    ..
	ldy     L5FF2                           ; 6012 AC F2 5F                 .._
	ldx     L5FF1                           ; 6015 AE F1 5F                 .._
	lda     L5FF0                           ; 6018 AD F0 5F                 .._
	jsr     L5F16                           ; 601B 20 16 5F                  ._
	rts                                     ; 601E 60                       `

; ----------------------------------------------------------------------------
L601F:  brk                                     ; 601F 00                       .
L6020:  brk                                     ; 6020 00                       .
L6021:  brk                                     ; 6021 00                       .
L6022:  brk                                     ; 6022 00                       .
L6023:  brk                                     ; 6023 00                       .
L6024:  brk                                     ; 6024 00                       .
L6025:  brk                                     ; 6025 00                       .

L6026:  prolog
	jsr     sub_44D5                        ; 6029 20 D5 44                  .D
	.addr	L601F
	.byte	$04
	lda     #$5E                            ; 602F A9 5E                    .^
	sta     L5EFE+2                         ; 6031 8D 00 5F                 .._
	lda     #$DF                            ; 6034 A9 DF                    ..
	sta     L5EFE+1                         ; 6036 8D FF 5E                 ..^
	lda     L6022                           ; 6039 AD 22 60                 ."`
	sta     $A3                             ; 603C 85 A3                    ..
	lda     L6023                           ; 603E AD 23 60                 .#`
	sta     $A4                             ; 6041 85 A4                    ..
	ldy     L6021                           ; 6043 AC 21 60                 .!`
L6046:  ldx     L6020                           ; 6046 AE 20 60                 . `
	lda     L601F                           ; 6049 AD 1F 60                 ..`
	jsr     L5F16                           ; 604C 20 16 5F                  ._
	lda     $A1                             ; 604F A5 A1                    ..
	sta     L6025                           ; 6051 8D 25 60                 .%`
	lda     $A0                             ; 6054 A5 A0                    ..
	sta     L6024                           ; 6056 8D 24 60                 .$`
	lda     L6025                           ; 6059 AD 25 60                 .%`
L605C:  sta     $A1                             ; 605C 85 A1                    ..
	lda     L6024                           ; 605E AD 24 60                 .$`
	sta     $A0                             ; 6061 85 A0                    ..
	rts                                     ; 6063 60                       `

; ----------------------------------------------------------------------------
L6064:  .word	$0000
L6066:  brk                                     ; 6066 00                       .
L6067:  brk                                     ; 6067 00                       .
L6068:  brk                                     ; 6068 00                       .
L6069:  brk                                     ; 6069 00                       .
L606A:  brk                                     ; 606A 00                       .
L606B:  brk                                     ; 606B 00                       .
L606C:  brk                                     ; 606C 00                       .
L606D:  brk                                     ; 606D 00                       .

sub_606E:
	prolog
	stx     L6064+1                         ; 6071 8E 65 60                 .e`
	sta     L6064                           ; 6074 8D 64 60                 .d`
	clc                                     ; 6077 18                       .
	lda     L6064                           ; 6078 AD 64 60                 .d`
	adc     #$03                            ; 607B 69 03                    i.
	sta     off_AE
	lda     L6064+1                         ; 607F AD 65 60                 .e`
	adc     #$00                            ; 6082 69 00                    i.
	sta     off_AE+1
	lda     #$02                            ; 6086 A9 02                    ..
	sta     $84                             ; 6088 85 84                    ..
	lda     off_AE+1
	tax                                     ; 608C AA                       .
	lda     off_AE
	jsr     sub_43E0
	sta     L6066                           ; 6092 8D 66 60                 .f`
	txa                                     ; 6095 8A                       .
	sta     L6067                           ; 6096 8D 67 60                 .g`
	ldy     #$00                            ; 6099 A0 00                    ..
	sty     L6069                           ; 609B 8C 69 60                 .i`
	sty     L6068                           ; 609E 8C 68 60                 .h`
	lda     L6068                           ; 60A1 AD 68 60                 .h`
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
	ldx     L6069                           ; 60C1 AE 69 60                 .i`
	lda     L6068                           ; 60C4 AD 68 60                 .h`
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
	bcs     L60EA                           ; 60E5 B0 03                    ..
	jmp     L60ED                           ; 60E7 4C ED 60                 L.`

; ----------------------------------------------------------------------------
L60EA:  jmp     L6177                           ; 60EA 4C 77 61                 Lwa

; ----------------------------------------------------------------------------
L60ED:  lda     L6067                           ; 60ED AD 67 60                 .g`
	sta     $A3                             ; 60F0 85 A3                    ..
	lda     #$01                            ; 60F2 A9 01                    ..
	sta     $A4                             ; 60F4 85 A4                    ..
	ldy     L6066                           ; 60F6 AC 66 60                 .f`
	ldx     L6069                           ; 60F9 AE 69 60                 .i`
	lda     L6068                           ; 60FC AD 68 60                 .h`
	jsr     L6026                           ; 60FF 20 26 60                  &`
	lda     $A1                             ; 6102 A5 A1                    ..
	sta     L606B                           ; 6104 8D 6B 60                 .k`
	lda     $A0                             ; 6107 A5 A0                    ..
	sta     L606A                           ; 6109 8D 6A 60                 .j`
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
	ldx     L6069                           ; 6129 AE 69 60                 .i`
	lda     L6068                           ; 612C AD 68 60                 .h`
	jsr     L5FF5                           ; 612F 20 F5 5F                  ._
	lda     #$02                            ; 6132 A9 02                    ..
	sta     $84                             ; 6134 85 84                    ..
	lda     L6069                           ; 6136 AD 69 60                 .i`
	tax                                     ; 6139 AA                       .
	lda     L6068                           ; 613A AD 68 60                 .h`
	jsr     L43D1                           ; 613D 20 D1 43                  .C
	sta     $AE                             ; 6140 85 AE                    ..
	txa                                     ; 6142 8A                       .
	sta     $AF                             ; 6143 85 AF                    ..
	clc                                     ; 6145 18                       .
	lda     $02E7                           ; 6146 AD E7 02                 ...
	adc     $AE                             ; 6149 65 AE                    e.
	sta     L606C                           ; 614B 8D 6C 60                 .l`
	lda     $02E8                           ; 614E AD E8 02                 ...
	adc     $AF                             ; 6151 65 AF                    e.
	sta     L606D                           ; 6153 8D 6D 60                 .m`
	lda     L606D                           ; 6156 AD 6D 60                 .m`
	sta     $A1                             ; 6159 85 A1                    ..
	lda     L606C                           ; 615B AD 6C 60                 .l`
	sta     $A0                             ; 615E 85 A0                    ..
	rts                                     ; 6160 60                       `

; ----------------------------------------------------------------------------
L6161:  clc                                     ; 6161 18                       .
	lda     L6068                           ; 6162 AD 68 60                 .h`
	adc     L606A                           ; 6165 6D 6A 60                 mj`
	sta     L6068                           ; 6168 8D 68 60                 .h`
	lda     L6069                           ; 616B AD 69 60                 .i`
	adc     L606B                           ; 616E 6D 6B 60                 mk`
	sta     L6069                           ; 6171 8D 69 60                 .i`
	.byte   $4C                             ; 6174 4C                       L
	.byte   $A1                             ; 6175 A1                       .
L6176:  rts                                     ; 6176 60                       `

; ----------------------------------------------------------------------------
L6177:  jmp     L617C                           ; 6177 4C 7C 61                 L|a

; ----------------------------------------------------------------------------
	ora     ($43,x)                         ; 617A 01 43                    .C
L617C:  lda     #$00                            ; 617C A9 00                    ..
	sta     $A3                             ; 617E 85 A3                    ..
	ldy     #$26                            ; 6180 A0 26                    .&
	ldx     #$61                            ; 6182 A2 61                    .a
	lda     #$7A                            ; 6184 A9 7A                    .z
	jsr     sub_55A0
	ldi	$A1, $00
	ldi	$A0, $00
	rts                                     ; 6191 60                       `

; ----------------------------------------------------------------------------
L6192:  brk                                     ; 6192 00                       .
L6193:  brk                                     ; 6193 00                       .
L6194:  brk                                     ; 6194 00                       .
L6195:  brk                                     ; 6195 00                       .
L6196:  brk                                     ; 6196 00                       .
L6197:  brk                                     ; 6197 00                       .
L6198:  brk                                     ; 6198 00                       .
L6199:  brk                                     ; 6199 00                       .

L619A:  prolog
	jsr     sub_44D5                        ; 619D 20 D5 44                  .D
	.addr	L6192
	.byte	$03
	clc                                     ; 61A3 18                       .
	lda     L6194                           ; 61A4 AD 94 61                 ..a
	adc     #$03                            ; 61A7 69 03                    i.
	sta     $AE                             ; 61A9 85 AE                    ..
	lda     L6195                           ; 61AB AD 95 61                 ..a
	adc     #$00                            ; 61AE 69 00                    i.
	sta     $AF                             ; 61B0 85 AF                    ..
	lda     #$02                            ; 61B2 A9 02                    ..
	sta     $84                             ; 61B4 85 84                    ..
	lda     $AF                             ; 61B6 A5 AF                    ..
	tax                                     ; 61B8 AA                       .
	lda     $AE                             ; 61B9 A5 AE                    ..
	jsr     sub_43E0
	sta     L6196                           ; 61BE 8D 96 61                 ..a
	txa                                     ; 61C1 8A                       .
	sta     L6197                           ; 61C2 8D 97 61                 ..a
	sec                                     ; 61C5 38                       8
	lda     L6192                           ; 61C6 AD 92 61                 ..a
	sbc     $02E7                           ; 61C9 ED E7 02                 ...
	sta     $AE                             ; 61CC 85 AE                    ..
	lda     L6193                           ; 61CE AD 93 61                 ..a
	sbc     $02E8                           ; 61D1 ED E8 02                 ...
	sta     $AF                             ; 61D4 85 AF                    ..
	lda     #$02                            ; 61D6 A9 02                    ..
	sta     $84                             ; 61D8 85 84                    ..
	lda     $AF                             ; 61DA A5 AF                    ..
	tax                                     ; 61DC AA                       .
	lda     $AE                             ; 61DD A5 AE                    ..
	jsr     sub_43E0
	sta     L6198                           ; 61E2 8D 98 61                 ..a
	txa                                     ; 61E5 8A                       .
	sta     L6199                           ; 61E6 8D 99 61                 ..a
	lda     L6197                           ; 61E9 AD 97 61                 ..a
	sta     $A3                             ; 61EC 85 A3                    ..
	lda     #$01                            ; 61EE A9 01                    ..
	sta     $A4                             ; 61F0 85 A4                    ..
	ldy     L6196                           ; 61F2 AC 96 61                 ..a
	ldx     L6199                           ; 61F5 AE 99 61                 ..a
	lda     L6198                           ; 61F8 AD 98 61                 ..a
	jsr     L5FF5                           ; 61FB 20 F5 5F                  ._
	rts                                     ; 61FE 60                       `

; ----------------------------------------------------------------------------
L61FF:  brk                                     ; 61FF 00                       .
L6200:  brk                                     ; 6200 00                       .
L6201:  brk                                     ; 6201 00                       .
L6202:  brk                                     ; 6202 00                       .

L6203:  prolog
	lda     #$01                            ; 6206 A9 01                    ..
	asl     a                               ; 6208 0A                       .
	php                                     ; 6209 08                       .
	clc                                     ; 620A 18                       .
	adc     L466F                           ; 620B 6D 6F 46                 moF
	sta     $AE                             ; 620E 85 AE                    ..
	lda     #$00                            ; 6210 A9 00                    ..
	rol     a                               ; 6212 2A                       *
	plp                                     ; 6213 28                       (
	adc     L466F+1
	sta     $AF                             ; 6217 85 AF                    ..
	ldy     #$01                            ; 6219 A0 01                    ..
	lda     ($AE),y                         ; 621B B1 AE                    ..
	sta     $A1                             ; 621D 85 A1                    ..
	dey                                     ; 621F 88                       .
	lda     ($AE),y                         ; 6220 B1 AE                    ..
	sta     $A0                             ; 6222 85 A0                    ..
	lda     L466F                           ; 6224 AD 6F 46                 .oF
	sta     $AC                             ; 6227 85 AC                    ..
	lda     L466F+1
	sta     $AD                             ; 622C 85 AD                    ..
	iny                                     ; 622E C8                       .
	lda     ($AC),y                         ; 622F B1 AC                    ..
	sta     $A3                             ; 6231 85 A3                    ..
	dey                                     ; 6233 88                       .
	lda     ($AC),y                         ; 6234 B1 AC                    ..
	sta     $A2                             ; 6236 85 A2                    ..
	lda     #$00                            ; 6238 A9 00                    ..
	sta     $A5                             ; 623A 85 A5                    ..
	lda     #$20                            ; 623C A9 20                    . 
	sta     $A4                             ; 623E 85 A4                    ..
	ldy     $A2                             ; 6240 A4 A2                    ..
	ldxa	$A0
	jsr     sub_461F
	lda     #$01                            ; 6249 A9 01                    ..
	asl     a                               ; 624B 0A                       .
	php                                     ; 624C 08                       .
	clc                                     ; 624D 18                       .
	adc     L466F                           ; 624E 6D 6F 46                 moF
	sta     $AE                             ; 6251 85 AE                    ..
	lda     #$00                            ; 6253 A9 00                    ..
	rol     a                               ; 6255 2A                       *
	plp                                     ; 6256 28                       (
	adc     L466F+1
	sta     $AF                             ; 625A 85 AF                    ..
	ldy     #$01                            ; 625C A0 01                    ..
	lda     ($AE),y                         ; 625E B1 AE                    ..
	sta	L6200
	dey                                     ; 6263 88                       .
	lda     ($AE),y                         ; 6264 B1 AE                    ..
	sta     L61FF                           ; 6266 8D FF 61                 ..a
	clc                                     ; 6269 18                       .
	lda     L61FF                           ; 626A AD FF 61                 ..a
	adc     #$03                            ; 626D 69 03                    i.
	sta     $AE                             ; 626F 85 AE                    ..
	lda     L6200                           ; 6271 AD 00 62                 ..b
	adc     #$00                            ; 6274 69 00                    i.
	sta     $AF                             ; 6276 85 AF                    ..
	clc                                     ; 6278 18                       .
	lda     $AE                             ; 6279 A5 AE                    ..
	adc     #$01                            ; 627B 69 01                    i.
	sta     L6201                           ; 627D 8D 01 62                 ..b
	lda     $AF                             ; 6280 A5 AF                    ..
	adc     #$00                            ; 6282 69 00                    i.
	sta     L6202                           ; 6284 8D 02 62                 ..b
	lda     L6201                           ; 6287 AD 01 62                 ..b
	sta     $AE                             ; 628A 85 AE                    ..
	lda     L6202                           ; 628C AD 02 62                 ..b
	sta     $AF                             ; 628F 85 AF                    ..
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
	clc                                     ; 62A6 18                       .
	lda     L61FF                           ; 62A7 AD FF 61                 ..a
	adc     #$1E                            ; 62AA 69 1E                    i.
	sta     L6201                           ; 62AC 8D 01 62                 ..b
	lda     L6200                           ; 62AF AD 00 62                 ..b
	adc     #$00                            ; 62B2 69 00                    i.
	sta     L6202                           ; 62B4 8D 02 62                 ..b
	lda     L6201                           ; 62B7 AD 01 62                 ..b
	sta     $AE                             ; 62BA 85 AE                    ..
	lda     L6202                           ; 62BC AD 02 62                 ..b
	sta     $AF                             ; 62BF 85 AF                    ..
	lda     L6200                           ; 62C1 AD 00 62                 ..b
	iny                                     ; 62C4 C8                       .
	sta     ($AE),y                         ; 62C5 91 AE                    ..
	lda     L61FF                           ; 62C7 AD FF 61                 ..a
	dey                                     ; 62CA 88                       .
	sta     ($AE),y                         ; 62CB 91 AE                    ..
	rts                                     ; 62CD 60                       `

; ----------------------------------------------------------------------------
L62CE:  brk                                     ; 62CE 00                       .
L62CF:  brk                                     ; 62CF 00                       .
L62D0:  brk                                     ; 62D0 00                       .
L62D1:  jmp     L62D4                           ; 62D1 4C D4 62                 L.b

; ----------------------------------------------------------------------------
L62D4:  lda     $022F                           ; 62D4 AD 2F 02                 ./.
	sta     L62D0                           ; 62D7 8D D0 62                 ..b
	ldy     #$00                            ; 62DA A0 00                    ..
	sty     $022F                           ; 62DC 8C 2F 02                 ./.
	lda     #$01                            ; 62DF A9 01                    ..
	asl     a                               ; 62E1 0A                       .
	php                                     ; 62E2 08                       .
	clc                                     ; 62E3 18                       .
	adc     L466F                           ; 62E4 6D 6F 46                 moF
	sta     $AE                             ; 62E7 85 AE                    ..
	lda     #$00                            ; 62E9 A9 00                    ..
	rol     a                               ; 62EB 2A                       *
	plp                                     ; 62EC 28                       (
	adc     L466F+1
	sta     $AF                             ; 62F0 85 AF                    ..
	iny                                     ; 62F2 C8                       .
	lda     ($AE),y                         ; 62F3 B1 AE                    ..
	sta     $0231                           ; 62F5 8D 31 02                 .1.
	dey                                     ; 62F8 88                       .
	lda     ($AE),y                         ; 62F9 B1 AE                    ..
	sta     $0230                           ; 62FB 8D 30 02                 .0.
	lda     L62D0                           ; 62FE AD D0 62                 ..b
	sta     $022F                           ; 6301 8D 2F 02                 ./.
	lda     #$C0                            ; 6304 A9 C0                    ..
	sta     $D40E                           ; 6306 8D 0E D4                 ...
	lda     #$01                            ; 6309 A9 01                    ..
	asl     a                               ; 630B 0A                       .
	php                                     ; 630C 08                       .
	clc                                     ; 630D 18                       .
	adc     L466F                           ; 630E 6D 6F 46                 moF
	sta     $AE                             ; 6311 85 AE                    ..
	lda     #$00                            ; 6313 A9 00                    ..
	rol     a                               ; 6315 2A                       *
	plp                                     ; 6316 28                       (
	adc     L466F+1
	sta     $AF                             ; 631A 85 AF                    ..
	iny                                     ; 631C C8                       .
	lda     ($AE),y                         ; 631D B1 AE                    ..
	sta     L62CF                           ; 631F 8D CF 62                 ..b
	dey                                     ; 6322 88                       .
	lda     ($AE),y                         ; 6323 B1 AE                    ..
	sta     L62CE                           ; 6325 8D CE 62                 ..b
L6328:  lda     #$01                            ; 6328 A9 01                    ..
	asl     a                               ; 632A 0A                       .
	php                                     ; 632B 08                       .
	clc                                     ; 632C 18                       .
	adc     L466F                           ; 632D 6D 6F 46                 moF
	sta     $AE                             ; 6330 85 AE                    ..
	lda     #$00                            ; 6332 A9 00                    ..
	rol     a                               ; 6334 2A                       *
	plp                                     ; 6335 28                       (
	adc     L466F+1
	sta     $AF                             ; 6339 85 AF                    ..
	lda     L466F                           ; 633B AD 6F 46                 .oF
	sta     $AC                             ; 633E 85 AC                    ..
	.byte   $AD                             ; 6340 AD                       .
L6341:  bvs     L6389                           ; 6341 70 46                    pF
	sta     $AD                             ; 6343 85 AD                    ..
	iny                                     ; 6345 C8                       .
	lda     ($AC),y                         ; 6346 B1 AC                    ..
	sta     ($AE),y                         ; 6348 91 AE                    ..
	dey                                     ; 634A 88                       .
	lda     ($AC),y                         ; 634B B1 AC                    ..
	sta     ($AE),y                         ; 634D 91 AE                    ..
	lda     L466F                           ; 634F AD 6F 46                 .oF
	sta     $AE                             ; 6352 85 AE                    ..
	lda     L466F+1
	sta     $AF                             ; 6357 85 AF                    ..
	lda     L62CF                           ; 6359 AD CF 62                 ..b
	iny                                     ; 635C C8                       .
	sta     ($AE),y                         ; 635D 91 AE                    ..
	lda     L62CE                           ; 635F AD CE 62                 ..b
	dey                                     ; 6362 88                       .
	sta     ($AE),y                         ; 6363 91 AE                    ..
	jsr     L6203                           ; 6365 20 03 62                  .b
	rts                                     ; 6368 60                       `

; ----------------------------------------------------------------------------
L6369:  brk                                     ; 6369 00                       .
L636A:  .byte   $C4                             ; 636A C4                       .
L636B:  .byte   $02                             ; 636B 02                       .
L636C:  brk                                     ; 636C 00                       .
L636D:  brk                                     ; 636D 00                       .
L636E:  jmp     L6371                           ; 636E 4C 71 63                 Lqc

; ----------------------------------------------------------------------------
L6371:  lda     L46EF                           ; 6371 AD EF 46                 ..F
	sta     L6369                           ; 6374 8D 69 63                 .ic
	lda     L6369                           ; 6377 AD 69 63                 .ic
	asl     a                               ; 637A 0A                       .
	php                                     ; 637B 08                       .
	clc                                     ; 637C 18                       .
	adc     L46F5                           ; 637D 6D F5 46                 m.F
	sta     $AE                             ; 6380 85 AE                    ..
	lda     #$00                            ; 6382 A9 00                    ..
	rol     a                               ; 6384 2A                       *
	plp                                     ; 6385 28                       (
	adc     L46F6                           ; 6386 6D F6 46                 m.F
L6389:  sta     $AF                             ; 6389 85 AF                    ..
	ldy     #$01                            ; 638B A0 01                    ..
	lda     ($AE),y                         ; 638D B1 AE                    ..
	sta     L636D                           ; 638F 8D 6D 63                 .mc
	dey                                     ; 6392 88                       .
	lda     ($AE),y                         ; 6393 B1 AE                    ..
	sta     L636C                           ; 6395 8D 6C 63                 .lc
	clc                                     ; 6398 18                       .
	lda     L636C                           ; 6399 AD 6C 63                 .lc
	adc     #$03                            ; 639C 69 03                    i.
	sta     $A2                             ; 639E 85 A2                    ..
	lda     L636D                           ; 63A0 AD 6D 63                 .mc
	adc     #$00                            ; 63A3 69 00                    i.
	sta     $A3                             ; 63A5 85 A3                    ..
	lda     #$00                            ; 63A7 A9 00                    ..
	sta     $A5                             ; 63A9 85 A5                    ..
	lda     #$05                            ; 63AB A9 05                    ..
	sta     $A4                             ; 63AD 85 A4                    ..
	ldy     $A2                             ; 63AF A4 A2                    ..
	ldx     L636B                           ; 63B1 AE 6B 63                 .kc
	lda     L636A                           ; 63B4 AD 6A 63                 .jc
	jsr     sub_461F
	clc                                     ; 63BA 18                       .
	lda     L636C                           ; 63BB AD 6C 63                 .lc
	adc     #$02                            ; 63BE 69 02                    i.
	sta     $AE                             ; 63C0 85 AE                    ..
	lda     L636D                           ; 63C2 AD 6D 63                 .mc
	adc     #$00                            ; 63C5 69 00                    i.
	sta     $AF                             ; 63C7 85 AF                    ..
	ldy     #$00                            ; 63C9 A0 00                    ..
	lda     ($AE),y                         ; 63CB B1 AE                    ..
	sta     $02F4                           ; 63CD 8D F4 02                 ...
	rts                                     ; 63D0 60                       `

; ----------------------------------------------------------------------------
L63D1:  brk                                     ; 63D1 00                       .
L63D2:  brk                                     ; 63D2 00                       .
L63D3:  brk                                     ; 63D3 00                       .
L63D4:  brk                                     ; 63D4 00                       .
L63D5:  brk                                     ; 63D5 00                       .
L63D6:  brk                                     ; 63D6 00                       .
L63D7:  brk                                     ; 63D7 00                       .
L63D8:  brk                                     ; 63D8 00                       .
L63D9:  brk                                     ; 63D9 00                       .
L63DA:  brk                                     ; 63DA 00                       .
L63DB:  brk                                     ; 63DB 00                       .
L63DC:  brk                                     ; 63DC 00                       .

sub_63DD:  
	prolog
	lda     L46EF                           ; 63E0 AD EF 46                 ..F
	sta     L63DB                           ; 63E3 8D DB 63                 ..c
	lda     L63DB                           ; 63E6 AD DB 63                 ..c
	asl     a                               ; 63E9 0A                       .
	php                                     ; 63EA 08                       .
	clc                                     ; 63EB 18                       .
	adc     L46F5                           ; 63EC 6D F5 46                 m.F
	sta     $AE                             ; 63EF 85 AE                    ..
	lda     #$00                            ; 63F1 A9 00                    ..
	rol     a                               ; 63F3 2A                       *
	plp                                     ; 63F4 28                       (
	adc     L46F6                           ; 63F5 6D F6 46                 m.F
	sta     $AF                             ; 63F8 85 AF                    ..
	ldy     #$01                            ; 63FA A0 01                    ..
	lda     ($AE),y                         ; 63FC B1 AE                    ..
	sta     L63DA                           ; 63FE 8D DA 63                 ..c
	dey                                     ; 6401 88                       .
	lda     ($AE),y                         ; 6402 B1 AE                    ..
	sta     L63D9                           ; 6404 8D D9 63                 ..c
	lda     #$01                            ; 6407 A9 01                    ..
	asl     a                               ; 6409 0A                       .
	php                                     ; 640A 08                       .
	clc                                     ; 640B 18                       .
	adc     L466F                           ; 640C 6D 6F 46                 moF
	sta     $AE                             ; 640F 85 AE                    ..
	lda     #$00                            ; 6411 A9 00                    ..
	rol     a                               ; 6413 2A                       *
	plp                                     ; 6414 28                       (
	adc     L466F+1
	sta     $AF                             ; 6418 85 AF                    ..
	iny                                     ; 641A C8                       .
	lda     ($AE),y                         ; 641B B1 AE                    ..
	sta     L63D4                           ; 641D 8D D4 63                 ..c
	dey                                     ; 6420 88                       .
	lda     ($AE),y                         ; 6421 B1 AE                    ..
	sta     L63D3                           ; 6423 8D D3 63                 ..c
	clc                                     ; 6426 18                       .
	lda     L63D3                           ; 6427 AD D3 63                 ..c
	adc     #$06                            ; 642A 69 06                    i.
	sta     L63D7                           ; 642C 8D D7 63                 ..c
	lda     L63D4                           ; 642F AD D4 63                 ..c
	adc     #$00                            ; 6432 69 00                    i.
	sta     L63D8                           ; 6434 8D D8 63                 ..c
	lda     L63D9                           ; 6437 AD D9 63                 ..c
	sta     $AE                             ; 643A 85 AE                    ..
	lda     L63DA                           ; 643C AD DA 63                 ..c
	sta     $AF                             ; 643F 85 AF                    ..
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
	clc                                     ; 645B 18                       .
	lda     $AC                             ; 645C A5 AC                    ..
	adc     L63DC                           ; 645E 6D DC 63                 m.c
	.byte   $8D                             ; 6461 8D                       .
	.byte   $D6                             ; 6462 D6                       .
L6463:  .byte   $63                             ; 6463 63                       c
	clc                                     ; 6464 18                       .
	lda     L63D3                           ; 6465 AD D3 63                 ..c
	adc     #$03                            ; 6468 69 03                    i.
	sta     $AE                             ; 646A 85 AE                    ..
	lda     L63D4                           ; 646C AD D4 63                 ..c
	adc     #$00                            ; 646F 69 00                    i.
	sta     $AF                             ; 6471 85 AF                    ..
	lda     L63D6                           ; 6473 AD D6 63                 ..c
	sta     ($AE),y                         ; 6476 91 AE                    ..
	lda     #$00                            ; 6478 A9 00                    ..
	sta     $A3                             ; 647A 85 A3                    ..
	lda     L63DC                           ; 647C AD DC 63                 ..c
	sta     $A4                             ; 647F 85 A4                    ..
	ldy     #$17                            ; 6481 A0 17                    ..
	ldx     L63D8                           ; 6483 AE D8 63                 ..c
	lda     L63D7                           ; 6486 AD D7 63                 ..c
	jsr     L45FC                           ; 6489 20 FC 45                  .E
	clc                                     ; 648C 18                       .
	lda     L63D9                           ; 648D AD D9 63                 ..c
	adc     #$01                            ; 6490 69 01                    i.
	sta     $AE                             ; 6492 85 AE                    ..
	lda     L63DA                           ; 6494 AD DA 63                 ..c
	adc     #$00                            ; 6497 69 00                    i.
	sta     $AF                             ; 6499 85 AF                    ..
	ldy     #$00                            ; 649B A0 00                    ..
	lda     ($AE),y                         ; 649D B1 AE                    ..
	sta     $A0                             ; 649F 85 A0                    ..
	ldx     #$02                            ; 64A1 A2 02                    ..
	lda     $A0                             ; 64A3 A5 A0                    ..
	jsr     L4983                           ; 64A5 20 83 49                  .I
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
	bcs     L64D2                           ; 64CD B0 03                    ..
	jmp     L64D5                           ; 64CF 4C D5 64                 L.d

; ----------------------------------------------------------------------------
L64D2:  jmp     L6580                           ; 64D2 4C 80 65                 L.e

; ----------------------------------------------------------------------------
L64D5:  lda     L63DB                           ; 64D5 AD DB 63                 ..c
	asl     a                               ; 64D8 0A                       .
	php                                     ; 64D9 08                       .
	clc                                     ; 64DA 18                       .
	adc     L46F5                           ; 64DB 6D F5 46                 m.F
	sta     $AE                             ; 64DE 85 AE                    ..
	lda     #$00                            ; 64E0 A9 00                    ..
	rol     a                               ; 64E2 2A                       *
	plp                                     ; 64E3 28                       (
	adc     L46F6                           ; 64E4 6D F6 46                 m.F
	sta     $AF                             ; 64E7 85 AF                    ..
	ldy     #$01                            ; 64E9 A0 01                    ..
	lda     ($AE),y                         ; 64EB B1 AE                    ..
	sta     L63DA                           ; 64ED 8D DA 63                 ..c
	dey                                     ; 64F0 88                       .
	lda     ($AE),y                         ; 64F1 B1 AE                    ..
	sta     L63D9                           ; 64F3 8D D9 63                 ..c
	lda     L63D9                           ; 64F6 AD D9 63                 ..c
	sta     $AE                             ; 64F9 85 AE                    ..
	lda     L63DA                           ; 64FB AD DA 63                 ..c
	sta     $AF                             ; 64FE 85 AF                    ..
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
	jsr     L45FC                           ; 6550 20 FC 45                  .E
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
	jsr     L636E                           ; 65A8 20 6E 63                  nc
	jsr     L62D1                           ; 65AB 20 D1 62                  .b
	rts                                     ; 65AE 60                       `

; ----------------------------------------------------------------------------
L65AF:  brk                                     ; 65AF 00                       .
L65B0:  jmp     L65B3                           ; 65B0 4C B3 65                 L.e

; ----------------------------------------------------------------------------
L65B3:  sta     L65AF                           ; 65B3 8D AF 65                 ..e
	lda     L65AF                           ; 65B6 AD AF 65                 ..e
	asl     a                               ; 65B9 0A                       .
	php                                     ; 65BA 08                       .
	clc                                     ; 65BB 18                       .
	adc     L46E2                           ; 65BC 6D E2 46                 m.F
	sta     $AE                             ; 65BF 85 AE                    ..
	lda     #$00                            ; 65C1 A9 00                    ..
	rol     a                               ; 65C3 2A                       *
	plp                                     ; 65C4 28                       (
	adc     L46E3                           ; 65C5 6D E3 46                 m.F
	sta     $AF                             ; 65C8 85 AF                    ..
	ldy     #$01                            ; 65CA A0 01                    ..
	lda     ($AE),y                         ; 65CC B1 AE                    ..
	sta     $A1                             ; 65CE 85 A1                    ..
	dey                                     ; 65D0 88                       .
	lda     ($AE),y                         ; 65D1 B1 AE                    ..
	sta     $A0                             ; 65D3 85 A0                    ..
	rts                                     ; 65D5 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 65D6 00                       .
L65D7:  brk                                     ; 65D7 00                       .
L65D8:  brk                                     ; 65D8 00                       .
L65D9:  brk                                     ; 65D9 00                       .
L65DA:  brk                                     ; 65DA 00                       .
L65DB:  brk                                     ; 65DB 00                       .
L65DC:  brk                                     ; 65DC 00                       .
L65DD:  brk                                     ; 65DD 00                       .
L65DE:  brk                                     ; 65DE 00                       .
L65DF:  brk                                     ; 65DF 00                       .
L65E0:  brk                                     ; 65E0 00                       .
L65E1:  brk                                     ; 65E1 00                       .

L65E2:  prolog
	jsr     sub_44D5                        ; 65E5 20 D5 44                  .D
	dec     $65,x                           ; 65E8 D6 65                    .e
	ora     $AD                             ; 65EA 05 AD                    ..
	dec     $65,x                           ; 65EC D6 65                    .e
	jsr     L65B0                           ; 65EE 20 B0 65                  .e
	lda     $A1                             ; 65F1 A5 A1                    ..
	sta     L65E1                           ; 65F3 8D E1 65                 ..e
	lda     $A0                             ; 65F6 A5 A0                    ..
	sta     L65E0                           ; 65F8 8D E0 65                 ..e
	lda     L65E0                           ; 65FB AD E0 65                 ..e
	ora     L65E1                           ; 65FE 0D E1 65                 ..e
	beq     L6606                           ; 6601 F0 03                    ..
	jmp     L6607                           ; 6603 4C 07 66                 L.f

; ----------------------------------------------------------------------------
L6606:  rts                                     ; 6606 60                       `

; ----------------------------------------------------------------------------
L6607:  clc                                     ; 6607 18                       .
	lda     L65E0                           ; 6608 AD E0 65                 ..e
	adc     #$04                            ; 660B 69 04                    i.
	sta     $AE                             ; 660D 85 AE                    ..
	lda     L65E1                           ; 660F AD E1 65                 ..e
	adc     #$00                            ; 6612 69 00                    i.
	sta     $AF                             ; 6614 85 AF                    ..
	ldy     #$01                            ; 6616 A0 01                    ..
	lda     ($AE),y                         ; 6618 B1 AE                    ..
	sta     L65DF                           ; 661A 8D DF 65                 ..e
	dey                                     ; 661D 88                       .
	lda     ($AE),y                         ; 661E B1 AE                    ..
	sta     L65DE                           ; 6620 8D DE 65                 ..e
	sec                                     ; 6623 38                       8
	lda     L65D9                           ; 6624 AD D9 65                 ..e
	sbc     L65D7                           ; 6627 ED D7 65                 ..e
	sta     $AE                             ; 662A 85 AE                    ..
	clc                                     ; 662C 18                       .
	lda     $AE                             ; 662D A5 AE                    ..
	adc     #$01                            ; 662F 69 01                    i.
	sta     L65DC                           ; 6631 8D DC 65                 ..e
	lda     L65D8                           ; 6634 AD D8 65                 ..e
	sta     L65DD                           ; 6637 8D DD 65                 ..e
	lda     L65DA                           ; 663A AD DA 65                 ..e
	sta     L664B                           ; 663D 8D 4B 66                 .Kf
L6640:  lda     L664B                           ; 6640 AD 4B 66                 .Kf
	cmp     L65DD                           ; 6643 CD DD 65                 ..e
	bcs     L664C                           ; 6646 B0 04                    ..
	jmp     L668A                           ; 6648 4C 8A 66                 L.f

; ----------------------------------------------------------------------------
L664B:  brk                                     ; 664B 00                       .
L664C:  lda     L65DD                           ; 664C AD DD 65                 ..e
	asl     a                               ; 664F 0A                       .
	php                                     ; 6650 08                       .
	clc                                     ; 6651 18                       .
	adc     L65DE                           ; 6652 6D DE 65                 m.e
	sta     $AE                             ; 6655 85 AE                    ..
	lda     #$00                            ; 6657 A9 00                    ..
	rol     a                               ; 6659 2A                       *
	plp                                     ; 665A 28                       (
	adc     L65DF                           ; 665B 6D DF 65                 m.e
	sta     $AF                             ; 665E 85 AF                    ..
	clc                                     ; 6660 18                       .
	ldy     #$00                            ; 6661 A0 00                    ..
	lda     ($AE),y                         ; 6663 B1 AE                    ..
	.byte   $6D                             ; 6665 6D                       m
L6666:  .byte   $D7                             ; 6666 D7                       .
	adc     $85                             ; 6667 65 85                    e.
	ldy     #$C8                            ; 6669 A0 C8                    ..
	lda     ($AE),y                         ; 666B B1 AE                    ..
	adc     #$00                            ; 666D 69 00                    i.
	sta     $A1                             ; 666F 85 A1                    ..
	lda     #$00                            ; 6671 A9 00                    ..
	sta     $A3                             ; 6673 85 A3                    ..
	lda     L65DB                           ; 6675 AD DB 65                 ..e
	sta     $A4                             ; 6678 85 A4                    ..
	ldy     L65DC                           ; 667A AC DC 65                 ..e
	ldx     $A1                             ; 667D A6 A1                    ..
	lda     $A0                             ; 667F A5 A0                    ..
	jsr     L45FC                           ; 6681 20 FC 45                  .E
	inc     L65DD                           ; 6684 EE DD 65                 ..e
	jmp     L6640                           ; 6687 4C 40 66                 L@f

; ----------------------------------------------------------------------------
L668A:  ldy     #$01                            ; 668A A0 01                    ..
	sty     L4656                           ; 668C 8C 56 46                 .VF
	rts                                     ; 668F 60                       `

; ----------------------------------------------------------------------------
L6690:  brk                                     ; 6690 00                       .
L6691:  brk                                     ; 6691 00                       .
L6692:  brk                                     ; 6692 00                       .
L6693:  brk                                     ; 6693 00                       .
L6694:  brk                                     ; 6694 00                       .
L6695:  brk                                     ; 6695 00                       .
L6696:  jmp     L6699                           ; 6696 4C 99 66                 L.f

; ----------------------------------------------------------------------------
L6699:  stx     L6691                           ; 6699 8E 91 66                 ..f
	sta     L6690                           ; 669C 8D 90 66                 ..f
	lda     L6690                           ; 669F AD 90 66                 ..f
	jsr     L65B0                           ; 66A2 20 B0 65                  .e
	lda     $A1                             ; 66A5 A5 A1                    ..
	sta     L6693                           ; 66A7 8D 93 66                 ..f
	lda     $A0                             ; 66AA A5 A0                    ..
	sta     L6692                           ; 66AC 8D 92 66                 ..f
	lda     L6692                           ; 66AF AD 92 66                 ..f
	ora     L6693                           ; 66B2 0D 93 66                 ..f
	beq     L66BA                           ; 66B5 F0 03                    ..
	jmp     L66BB                           ; 66B7 4C BB 66                 L.f

; ----------------------------------------------------------------------------
L66BA:  rts                                     ; 66BA 60                       `

; ----------------------------------------------------------------------------
L66BB:  lda     L6693                           ; 66BB AD 93 66                 ..f
	sta     $A3                             ; 66BE 85 A3                    ..
	lda     #$00                            ; 66C0 A9 00                    ..
	sta     $A5                             ; 66C2 85 A5                    ..
	lda     #$02                            ; 66C4 A9 02                    ..
	sta     $A4                             ; 66C6 85 A4                    ..
	ldy     L6692                           ; 66C8 AC 92 66                 ..f
	ldx     #$66                            ; 66CB A2 66                    .f
	lda     #$94                            ; 66CD A9 94                    ..
	jsr     sub_461F
	sec                                     ; 66D2 38                       8
	lda     L6694                           ; 66D3 AD 94 66                 ..f
	sbc     #$01                            ; 66D6 E9 01                    ..
	sta     $A3                             ; 66D8 85 A3                    ..
	sec                                     ; 66DA 38                       8
	lda     L6695                           ; 66DB AD 95 66                 ..f
	sbc     #$01                            ; 66DE E9 01                    ..
	sta     $A4                             ; 66E0 85 A4                    ..
	lda     L6691                           ; 66E2 AD 91 66                 ..f
	sta     $A5                             ; 66E5 85 A5                    ..
	ldy     #$00                            ; 66E7 A0 00                    ..
	ldx     #$00                            ; 66E9 A2 00                    ..
	lda     L6690                           ; 66EB AD 90 66                 ..f
	jsr     L65E2                           ; 66EE 20 E2 65                  .e
	rts                                     ; 66F1 60                       `

; ----------------------------------------------------------------------------
L66F2:  brk                                     ; 66F2 00                       .
L66F3:  brk                                     ; 66F3 00                       .
L66F4:  brk                                     ; 66F4 00                       .
L66F5:  brk                                     ; 66F5 00                       .
L66F6:  brk                                     ; 66F6 00                       .
L66F7:  brk                                     ; 66F7 00                       .
	brk                                     ; 66F8 00                       .
	brk                                     ; 66F9 00                       .
L66FA:  brk                                     ; 66FA 00                       .
L66FB:  brk                                     ; 66FB 00                       .
L66FC:  jmp     L66FF                           ; 66FC 4C FF 66                 L.f

; ----------------------------------------------------------------------------
L66FF:  stx     L66F3                           ; 66FF 8E F3 66                 ..f
	sta     L66F2                           ; 6702 8D F2 66                 ..f
	lda     L66F2                           ; 6705 AD F2 66                 ..f
	jsr     L65B0                           ; 6708 20 B0 65                  .e
	lda     $A1                             ; 670B A5 A1                    ..
	sta     L66F5                           ; 670D 8D F5 66                 ..f
	lda     $A0                             ; 6710 A5 A0                    ..
	sta     L66F4                           ; 6712 8D F4 66                 ..f
	lda     L66F5                           ; 6715 AD F5 66                 ..f
	sta     $A3                             ; 6718 85 A3                    ..
	lda     #$00                            ; 671A A9 00                    ..
	sta     $A5                             ; 671C 85 A5                    ..
	lda     #$06                            ; 671E A9 06                    ..
	sta     $A4                             ; 6720 85 A4                    ..
	ldy     L66F4                           ; 6722 AC F4 66                 ..f
	ldx     #$66                            ; 6725 A2 66                    .f
	lda     #$F6                            ; 6727 A9 F6                    ..
	jsr     sub_461F
	lda     L66F3                           ; 672C AD F3 66                 ..f
	cmp     L66F7                           ; 672F CD F7 66                 ..f
	bcs     L6737                           ; 6732 B0 03                    ..
	jmp     L6742                           ; 6734 4C 42 67                 LBg

; ----------------------------------------------------------------------------
L6737:  ldx     #$00                            ; 6737 A2 00                    ..
	lda     L66F2                           ; 6739 AD F2 66                 ..f
	jsr     L6696                           ; 673C 20 96 66                  .f
	jmp     L67C3                           ; 673F 4C C3 67                 L.g

; ----------------------------------------------------------------------------
L6742:  lda     L66FA                           ; 6742 AD FA 66                 ..f
	sta     $AE                             ; 6745 85 AE                    ..
	lda     L66FB                           ; 6747 AD FB 66                 ..f
	sta     $AF                             ; 674A 85 AF                    ..
	ldy     #$01                            ; 674C A0 01                    ..
	lda     ($AE),y                         ; 674E B1 AE                    ..
	sta     $A1                             ; 6750 85 A1                    ..
	dey                                     ; 6752 88                       .
	lda     ($AE),y                         ; 6753 B1 AE                    ..
	sta     $A0                             ; 6755 85 A0                    ..
	lda     L66F3                           ; 6757 AD F3 66                 ..f
	asl     a                               ; 675A 0A                       .
	php                                     ; 675B 08                       .
	clc                                     ; 675C 18                       .
	adc     L66FA                           ; 675D 6D FA 66                 m.f
	sta     $AC                             ; 6760 85 AC                    ..
	lda     #$00                            ; 6762 A9 00                    ..
	rol     a                               ; 6764 2A                       *
	plp                                     ; 6765 28                       (
	adc     L66FB                           ; 6766 6D FB 66                 m.f
	sta     $AD                             ; 6769 85 AD                    ..
	iny                                     ; 676B C8                       .
	lda     ($AC),y                         ; 676C B1 AC                    ..
	sta     $A3                             ; 676E 85 A3                    ..
	dey                                     ; 6770 88                       .
	lda     ($AC),y                         ; 6771 B1 AC                    ..
	sta     $A2                             ; 6773 85 A2                    ..
	sec                                     ; 6775 38                       8
	lda     L66F7                           ; 6776 AD F7 66                 ..f
	sbc     L66F3                           ; 6779 ED F3 66                 ..f
	sta     $AA                             ; 677C 85 AA                    ..
	lda     #$00                            ; 677E A9 00                    ..
	sta     $85                             ; 6780 85 85                    ..
	lda     L66F6                           ; 6782 AD F6 66                 ..f
	sta     $84                             ; 6785 85 84                    ..
	lda     $AA                             ; 6787 A5 AA                    ..
	ldx     #$00                            ; 6789 A2 00                    ..
	jsr     sub_444A
	sta     $A4                             ; 678E 85 A4                    ..
	txa                                     ; 6790 8A                       .
	sta     $A5                             ; 6791 85 A5                    ..
	ldy     $A2                             ; 6793 A4 A2                    ..
	ldx     $A1                             ; 6795 A6 A1                    ..
	lda     $A0                             ; 6797 A5 A0                    ..
	jsr     sub_461F
	sec                                     ; 679C 38                       8
	lda     L66F7                           ; 679D AD F7 66                 ..f
	sbc     L66F3                           ; 67A0 ED F3 66                 ..f
	sta     $A2                             ; 67A3 85 A2                    ..
	sec                                     ; 67A5 38                       8
	lda     L66F6                           ; 67A6 AD F6 66                 ..f
	sbc     #$01                            ; 67A9 E9 01                    ..
	sta     $A3                             ; 67AB 85 A3                    ..
	sec                                     ; 67AD 38                       8
	lda     L66F7                           ; 67AE AD F7 66                 ..f
	sbc     #$01                            ; 67B1 E9 01                    ..
	sta     $A4                             ; 67B3 85 A4                    ..
	lda     #$00                            ; 67B5 A9 00                    ..
	sta     $A5                             ; 67B7 85 A5                    ..
	ldy     $A2                             ; 67B9 A4 A2                    ..
	ldx     #$00                            ; 67BB A2 00                    ..
	lda     L66F2                           ; 67BD AD F2 66                 ..f
	jsr     L65E2                           ; 67C0 20 E2 65                  .e
L67C3:  rts                                     ; 67C3 60                       `

; ----------------------------------------------------------------------------
L67C4:  .byte   $6C                             ; 67C4 6C                       l
L67C5:  .byte   $0C                             ; 67C5 0C                       .
L67C6:  brk                                     ; 67C6 00                       .
L67C7:  .byte   $AD                             ; 67C7 AD                       .
L67C8:  .byte   $01                             ; 67C8 01                       .
L67C9:  .byte   $D3                             ; 67C9 D3                       .
L67CA:  .byte   $29                             ; 67CA 29                       )
L67CB:  .byte   $FE                             ; 67CB FE                       .
L67CC:  .byte   $8D                             ; 67CC 8D                       .
L67CD:  .byte   $01                             ; 67CD 01                       .
L67CE:  .byte   $D3                             ; 67CE D3                       .
L67CF:  rts                                     ; 67CF 60                       `

; ----------------------------------------------------------------------------
L67D0:  .byte   $AD                             ; 67D0 AD                       .
L67D1:  .byte   $01                             ; 67D1 01                       .
L67D2:  .byte   $D3                             ; 67D2 D3                       .
L67D3:  .byte   $09                             ; 67D3 09                       .
L67D4:  .byte   $01                             ; 67D4 01                       .
L67D5:  .byte   $8D                             ; 67D5 8D                       .
L67D6:  .byte   $01                             ; 67D6 01                       .
L67D7:  .byte   $D3                             ; 67D7 D3                       .

L67D8:  prolog
	jsr     sub_44D5                        ; 67DB 20 D5 44                  .D
	.addr	L67C4
	.byte	$05
	lda	L67C4
	jsr     L65B0                           ; 67E4 20 B0 65                  .e
	lda     $A1                             ; 67E7 A5 A1                    ..
	sta     L67D0                           ; 67E9 8D D0 67                 ..g
	lda     $A0                             ; 67EC A5 A0                    ..
	sta     L67CF                           ; 67EE 8D CF 67                 ..g
	lda     L67CF                           ; 67F1 AD CF 67                 ..g
	ora     L67D0                           ; 67F4 0D D0 67                 ..g
	lbne	L67FD
	rts                                     ; 67FC 60                       `

; ----------------------------------------------------------------------------
L67FD:  lda     L67D0                           ; 67FD AD D0 67                 ..g
	sta     $A3                             ; 6800 85 A3                    ..
	lda     #$00                            ; 6802 A9 00                    ..
	sta     $A5                             ; 6804 85 A5                    ..
	lda     #$06                            ; 6806 A9 06                    ..
	sta     $A4                             ; 6808 85 A4                    ..
	ldy     L67CF                           ; 680A AC CF 67                 ..g
	ldx     #$67                            ; 680D A2 67                    .g
	lda     #$D2                            ; 680F A9 D2                    ..
	jsr     sub_461F
	lda     L67C8                           ; 6814 AD C8 67                 ..g
	sta     $AE                             ; 6817 85 AE                    ..
	lda     L67C9                           ; 6819 AD C9 67                 ..g
	sta     $AF                             ; 681C 85 AF                    ..
	ldy     #$00                            ; 681E A0 00                    ..
	lda     ($AE),y                         ; 6820 B1 AE                    ..
	sta     L67CA                           ; 6822 8D CA 67                 ..g
	lda     L67D4                           ; 6825 AD D4 67                 ..g
	cmp     L67CA                           ; 6828 CD CA 67                 ..g
	lda     L67D5                           ; 682B AD D5 67                 ..g
	sbc     #$00                            ; 682E E9 00                    ..
	bcc     L6835                           ; 6830 90 03                    ..
	jmp     L683B                           ; 6832 4C 3B 68                 L;h

; ----------------------------------------------------------------------------
L6835:  lda     L67D4                           ; 6835 AD D4 67                 ..g
	sta     L67CA                           ; 6838 8D CA 67                 ..g
L683B:  lda     L67CA                           ; 683B AD CA 67                 ..g
	beq     L6843                           ; 683E F0 03                    ..
	jmp     L6844                           ; 6840 4C 44 68                 LDh

; ----------------------------------------------------------------------------
L6843:  rts                                     ; 6843 60                       `

; ----------------------------------------------------------------------------
L6844:  sec                                     ; 6844 38                       8
	lda     L67D2                           ; 6845 AD D2 67                 ..g
	sbc     L67C5                           ; 6848 ED C5 67                 ..g
	sta     $AE                             ; 684B 85 AE                    ..
	lda     $AE                             ; 684D A5 AE                    ..
	cmp     L67C7                           ; 684F CD C7 67                 ..g
	bcc     L6857                           ; 6852 90 03                    ..
	jmp     L6861                           ; 6854 4C 61 68                 Lah

; ----------------------------------------------------------------------------
L6857:  sec                                     ; 6857 38                       8
	lda     L67D2                           ; 6858 AD D2 67                 ..g
	sbc     L67C5                           ; 685B ED C5 67                 ..g
	sta     L67C7                           ; 685E 8D C7 67                 ..g
L6861:  sec                                     ; 6861 38                       8
	lda     L67CA                           ; 6862 AD CA 67                 ..g
	sbc     #$01                            ; 6865 E9 01                    ..
	sta     $AE                             ; 6867 85 AE                    ..
	lda     #$00                            ; 6869 A9 00                    ..
	sta     $85                             ; 686B 85 85                    ..
	lda     L67C7                           ; 686D AD C7 67                 ..g
	sta     $84                             ; 6870 85 84                    ..
	lda     $AE                             ; 6872 A5 AE                    ..
	ldx     #$00                            ; 6874 A2 00                    ..
	jsr     L447F                           ; 6876 20 7F 44                  .D
	sta     L67CC                           ; 6879 8D CC 67                 ..g
	clc                                     ; 687C 18                       .
	lda     L67C6                           ; 687D AD C6 67                 ..g
	adc     L67CC                           ; 6880 6D CC 67                 m.g
	sta     $AE                             ; 6883 85 AE                    ..
	sec                                     ; 6885 38                       8
	lda     L67D3                           ; 6886 AD D3 67                 ..g
	sbc     #$01                            ; 6889 E9 01                    ..
	sta     $AC                             ; 688B 85 AC                    ..
	sec                                     ; 688D 38                       8
	lda     $AE                             ; 688E A5 AE                    ..
	sbc     $AC                             ; 6890 E5 AC                    ..
	sta     L67CD                           ; 6892 8D CD 67                 ..g
	ldx     #$00                            ; 6895 A2 00                    ..
	lda     L67CD                           ; 6897 AD CD 67                 ..g
	jsr     L496E                           ; 689A 20 6E 49                  nI
	lda     $A0                             ; 689D A5 A0                    ..
	sta     L67CE                           ; 689F 8D CE 67                 ..g
	lda     L67CE                           ; 68A2 AD CE 67                 ..g
	eor     #$01                            ; 68A5 49 01                    I.
	beq     L68AC                           ; 68A7 F0 03                    ..
	jmp     L68C6                           ; 68A9 4C C6 68                 L.h

; ----------------------------------------------------------------------------
L68AC:  ldx     L67CD                           ; 68AC AE CD 67                 ..g
	lda     L67C4                           ; 68AF AD C4 67                 ..g
	jsr     L66FC                           ; 68B2 20 FC 66                  .f
	sec                                     ; 68B5 38                       8
	lda     L67D3                           ; 68B6 AD D3 67                 ..g
	sbc     #$01                            ; 68B9 E9 01                    ..
	sta     $AE                             ; 68BB 85 AE                    ..
	sec                                     ; 68BD 38                       8
	lda     $AE                             ; 68BE A5 AE                    ..
	sbc     L67CC                           ; 68C0 ED CC 67                 ..g
	sta     L67C6                           ; 68C3 8D C6 67                 ..g
L68C6:  lda     L67C7                           ; 68C6 AD C7 67                 ..g
	sta     L67CB                           ; 68C9 8D CB 67                 ..g
	ldy     #$00                            ; 68CC A0 00                    ..
	sty     L67D1                           ; 68CE 8C D1 67                 ..g
	lda     L67CC                           ; 68D1 AD CC 67                 ..g
	sta     L68E2                           ; 68D4 8D E2 68                 ..h
L68D7:  lda     L68E2                           ; 68D7 AD E2 68                 ..h
	cmp     L67D1                           ; 68DA CD D1 67                 ..g
	bcs     L68E3                           ; 68DD B0 04                    ..
	jmp     L6964                           ; 68DF 4C 64 69                 Ldi

; ----------------------------------------------------------------------------
L68E2:  .byte   $07                             ; 68E2 07                       .
L68E3:  lda     L67CA                           ; 68E3 AD CA 67                 ..g
	cmp     L67C7                           ; 68E6 CD C7 67                 ..g
	bcc     L68EE                           ; 68E9 90 03                    ..
	jmp     L68F4                           ; 68EB 4C F4 68                 L.h

; ----------------------------------------------------------------------------
L68EE:  lda     L67CA                           ; 68EE AD CA 67                 ..g
	sta     L67CB                           ; 68F1 8D CB 67                 ..g
L68F4:  clc                                     ; 68F4 18                       .
	lda     L67D1                           ; 68F5 AD D1 67                 ..g
	adc     L67C6                           ; 68F8 6D C6 67                 m.g
	sta     $AE                             ; 68FB 85 AE                    ..
	lda     $AE                             ; 68FD A5 AE                    ..
	asl     a                               ; 68FF 0A                       .
	php                                     ; 6900 08                       .
	clc                                     ; 6901 18                       .
	adc     L67D6                           ; 6902 6D D6 67                 m.g
	sta     $AC                             ; 6905 85 AC                    ..
	lda     #$00                            ; 6907 A9 00                    ..
	rol     a                               ; 6909 2A                       *
	plp                                     ; 690A 28                       (
	adc     L67D7                           ; 690B 6D D7 67                 m.g
	sta     $AD                             ; 690E 85 AD                    ..
	clc                                     ; 6910 18                       .
	ldy     #$00                            ; 6911 A0 00                    ..
	lda     ($AC),y                         ; 6913 B1 AC                    ..
	adc     L67C5                           ; 6915 6D C5 67                 m.g
	sta     $A0                             ; 6918 85 A0                    ..
	iny                                     ; 691A C8                       .
	lda     ($AC),y                         ; 691B B1 AC                    ..
	adc     #$00                            ; 691D 69 00                    i.
	sta     $A1                             ; 691F 85 A1                    ..
	clc                                     ; 6921 18                       .
	lda     L67C8                           ; 6922 AD C8 67                 ..g
	adc     #$01                            ; 6925 69 01                    i.
	sta     $A2                             ; 6927 85 A2                    ..
	lda     L67C9                           ; 6929 AD C9 67                 ..g
	adc     #$00                            ; 692C 69 00                    i.
	sta     $A3                             ; 692E 85 A3                    ..
	lda     #$00                            ; 6930 A9 00                    ..
	sta     $A5                             ; 6932 85 A5                    ..
	lda     L67CB                           ; 6934 AD CB 67                 ..g
	sta     $A4                             ; 6937 85 A4                    ..
	ldy     $A2                             ; 6939 A4 A2                    ..
	ldx     $A1                             ; 693B A6 A1                    ..
	lda     $A0                             ; 693D A5 A0                    ..
	jsr     sub_461F
	clc                                     ; 6942 18                       .
	lda     L67C8                           ; 6943 AD C8 67                 ..g
	adc     L67CB                           ; 6946 6D CB 67                 m.g
	sta     L67C8                           ; 6949 8D C8 67                 ..g
	lda     L67C9                           ; 694C AD C9 67                 ..g
	adc     #$00                            ; 694F 69 00                    i.
	sta     L67C9                           ; 6951 8D C9 67                 ..g
	sec                                     ; 6954 38                       8
	lda     L67CA                           ; 6955 AD CA 67                 ..g
	sbc     L67CB                           ; 6958 ED CB 67                 ..g
	sta     L67CA                           ; 695B 8D CA 67                 ..g
	inc     L67D1                           ; 695E EE D1 67                 ..g
	jmp     L68D7                           ; 6961 4C D7 68                 L.h

; ----------------------------------------------------------------------------
L6964:  ldy     #$01                            ; 6964 A0 01                    ..
	sty     L4656                           ; 6966 8C 56 46                 .VF
	rts                                     ; 6969 60                       `

; ----------------------------------------------------------------------------
L696A:  jmp     L696D                           ; 696A 4C 6D 69                 Lmi

; ----------------------------------------------------------------------------
L696D:  lda     #$1E                            ; 696D A9 1E                    ..
	asl     a                               ; 696F 0A                       .
	sta     $A2                             ; 6970 85 A2                    ..
	lda     #$00                            ; 6972 A9 00                    ..
	sta     $A3                             ; 6974 85 A3                    ..
	ldy     $A2                             ; 6976 A4 A2                    ..
	ldx     L46E3                           ; 6978 AE E3 46                 ..F
	lda     L46E2                           ; 697B AD E2 46                 ..F
	jsr     L45F6                           ; 697E 20 F6 45                  .E
	rts                                     ; 6981 60                       `

; ----------------------------------------------------------------------------
L6982:  brk                                     ; 6982 00                       .
L6983:  brk                                     ; 6983 00                       .
L6984:  brk                                     ; 6984 00                       .
L6985:  brk                                     ; 6985 00                       .
L6986:  brk                                     ; 6986 00                       .
L6987:  brk                                     ; 6987 00                       .
L6988:  brk                                     ; 6988 00                       .
L6989:  brk                                     ; 6989 00                       .
	brk                                     ; 698A 00                       .
L698B:  brk                                     ; 698B 00                       .
L698C:  brk                                     ; 698C 00                       .
L698D:  brk                                     ; 698D 00                       .
L698E:  brk                                     ; 698E 00                       .
L698F:  brk                                     ; 698F 00                       .
L6990:  brk                                     ; 6990 00                       .
L6991:  brk                                     ; 6991 00                       .
L6992:  brk                                     ; 6992 00                       .
L6993:  brk                                     ; 6993 00                       .
L6994:  brk                                     ; 6994 00                       .
L6995:  jmp     L6998                           ; 6995 4C 98 69                 L.i

; ----------------------------------------------------------------------------
L6998:  sta     L6982                           ; 6998 8D 82 69                 ..i
	lda     L6982                           ; 699B AD 82 69                 ..i
	jsr     L65B0                           ; 699E 20 B0 65                  .e
	lda     $A1                             ; 69A1 A5 A1                    ..
	sta     L6984                           ; 69A3 8D 84 69                 ..i
	lda     $A0                             ; 69A6 A5 A0                    ..
	sta     L6983                           ; 69A8 8D 83 69                 ..i
	lda     L6983                           ; 69AB AD 83 69                 ..i
	ora     L6984                           ; 69AE 0D 84 69                 ..i
	beq     L69B6                           ; 69B1 F0 03                    ..
	jmp     L69B7                           ; 69B3 4C B7 69                 L.i

; ----------------------------------------------------------------------------
L69B6:  rts                                     ; 69B6 60                       `

; ----------------------------------------------------------------------------
L69B7:  lda     L6984                           ; 69B7 AD 84 69                 ..i
	sta     $A3                             ; 69BA 85 A3                    ..
	lda     #$00                            ; 69BC A9 00                    ..
	sta     $A5                             ; 69BE 85 A5                    ..
	lda     #$0B                            ; 69C0 A9 0B                    ..
	sta     $A4                             ; 69C2 85 A4                    ..
	ldy     L6983                           ; 69C4 AC 83 69                 ..i
	ldx     #$69                            ; 69C7 A2 69                    .i
	lda     #$8A                            ; 69C9 A9 8A                    ..
	jsr     sub_461F
	lda     L698E                           ; 69CE AD 8E 69                 ..i
	sta     $AE                             ; 69D1 85 AE                    ..
	lda     L698F                           ; 69D3 AD 8F 69                 ..i
	sta     $AF                             ; 69D6 85 AF                    ..
	ldy     #$01                            ; 69D8 A0 01                    ..
	lda     ($AE),y                         ; 69DA B1 AE                    ..
	sta     L6986                           ; 69DC 8D 86 69                 ..i
	dey                                     ; 69DF 88                       .
	lda     ($AE),y                         ; 69E0 B1 AE                    ..
	sta     L6985                           ; 69E2 8D 85 69                 ..i
	lda     L698B                           ; 69E5 AD 8B 69                 ..i
	asl     a                               ; 69E8 0A                       .
	sta     $A2                             ; 69E9 85 A2                    ..
	lda     #$00                            ; 69EB A9 00                    ..
	sta     $A3                             ; 69ED 85 A3                    ..
	ldy     $A2                             ; 69EF A4 A2                    ..
	ldx     L698F                           ; 69F1 AE 8F 69                 ..i
	lda     L698E                           ; 69F4 AD 8E 69                 ..i
	jsr     L619A                           ; 69F7 20 9A 61                  .a
	lda     L698D                           ; 69FA AD 8D 69                 ..i
	sta     $A3                             ; 69FD 85 A3                    ..
	ldy     L698C                           ; 69FF AC 8C 69                 ..i
	ldx     L6986                           ; 6A02 AE 86 69                 ..i
	lda     L6985                           ; 6A05 AD 85 69                 ..i
	jsr     L619A                           ; 6A08 20 9A 61                  .a
	lda     #$00                            ; 6A0B A9 00                    ..
	sta     $A3                             ; 6A0D 85 A3                    ..
	ldy     #$0B                            ; 6A0F A0 0B                    ..
	ldx     L6984                           ; 6A11 AE 84 69                 ..i
	lda     L6983                           ; 6A14 AD 83 69                 ..i
	jsr     L619A                           ; 6A17 20 9A 61                  .a
	lda     L6982                           ; 6A1A AD 82 69                 ..i
	asl     a                               ; 6A1D 0A                       .
	php                                     ; 6A1E 08                       .
	clc                                     ; 6A1F 18                       .
	adc     L46E2                           ; 6A20 6D E2 46                 m.F
	sta     $AE                             ; 6A23 85 AE                    ..
	lda     #$00                            ; 6A25 A9 00                    ..
	rol     a                               ; 6A27 2A                       *
	plp                                     ; 6A28 28                       (
	adc     L46E3                           ; 6A29 6D E3 46                 m.F
	sta     $AF                             ; 6A2C 85 AF                    ..
	lda     #$00                            ; 6A2E A9 00                    ..
	ldy     #$01                            ; 6A30 A0 01                    ..
	sta     ($AE),y                         ; 6A32 91 AE                    ..
	lda     #$00                            ; 6A34 A9 00                    ..
	dey                                     ; 6A36 88                       .
	sta     ($AE),y                         ; 6A37 91 AE                    ..
	lda     L6991                           ; 6A39 AD 91 69                 ..i
	ora     L6992                           ; 6A3C 0D 92 69                 ..i
	bne     L6A44                           ; 6A3F D0 03                    ..
	jmp     L6A67                           ; 6A41 4C 67 6A                 Lgj

; ----------------------------------------------------------------------------
L6A44:  lda     #$00                            ; 6A44 A9 00                    ..
	sta     $85                             ; 6A46 85 85                    ..
	lda     #$06                            ; 6A48 A9 06                    ..
	sta     $84                             ; 6A4A 85 84                    ..
	lda     L6990                           ; 6A4C AD 90 69                 ..i
	ldx     #$00                            ; 6A4F A2 00                    ..
	jsr     sub_444A
	sta     L6987                           ; 6A54 8D 87 69                 ..i
	lda     #$00                            ; 6A57 A9 00                    ..
	sta     $A3                             ; 6A59 85 A3                    ..
	ldy     L6987                           ; 6A5B AC 87 69                 ..i
	ldx     L6992                           ; 6A5E AE 92 69                 ..i
	lda     L6991                           ; 6A61 AD 91 69                 ..i
	jsr     L619A                           ; 6A64 20 9A 61                  .a
L6A67:  lda     L6993                           ; 6A67 AD 93 69                 ..i
	ora     L6994                           ; 6A6A 0D 94 69                 ..i
	bne     L6A72                           ; 6A6D D0 03                    ..
	jmp     L6AB8                           ; 6A6F 4C B8 6A                 L.j

; ----------------------------------------------------------------------------
L6A72:  lda     L6993                           ; 6A72 AD 93 69                 ..i
	sta     $AE                             ; 6A75 85 AE                    ..
	lda     L6994                           ; 6A77 AD 94 69                 ..i
	sta     $AF                             ; 6A7A 85 AF                    ..
	ldy     #$01                            ; 6A7C A0 01                    ..
	lda     ($AE),y                         ; 6A7E B1 AE                    ..
	sta     L6989                           ; 6A80 8D 89 69                 ..i
	dey                                     ; 6A83 88                       .
	lda     ($AE),y                         ; 6A84 B1 AE                    ..
	sta     L6988                           ; 6A86 8D 88 69                 ..i
	lda     L6988                           ; 6A89 AD 88 69                 ..i
	sta     $AE                             ; 6A8C 85 AE                    ..
	lda     L6989                           ; 6A8E AD 89 69                 ..i
	sta     $AF                             ; 6A91 85 AF                    ..
	clc                                     ; 6A93 18                       .
	lda     ($AE),y                         ; 6A94 B1 AE                    ..
	adc     #$01                            ; 6A96 69 01                    i.
	sta     $A2                             ; 6A98 85 A2                    ..
	lda     #$00                            ; 6A9A A9 00                    ..
	sta     $A3                             ; 6A9C 85 A3                    ..
	ldy     $A2                             ; 6A9E A4 A2                    ..
	ldx     L6989                           ; 6AA0 AE 89 69                 ..i
	lda     L6988                           ; 6AA3 AD 88 69                 ..i
	jsr     L619A                           ; 6AA6 20 9A 61                  .a
	.byte   $A9                             ; 6AA9 A9                       .
L6AAA:  brk                                     ; 6AAA 00                       .
	sta     $A3                             ; 6AAB 85 A3                    ..
	ldy     #$1A                            ; 6AAD A0 1A                    ..
	ldx     L6994                           ; 6AAF AE 94 69                 ..i
	lda     L6993                           ; 6AB2 AD 93 69                 ..i
	jsr     L619A                           ; 6AB5 20 9A 61                  .a
L6AB8:  ldy     #$01                            ; 6AB8 A0 01                    ..
	sty     L4656                           ; 6ABA 8C 56 46                 .VF
	rts                                     ; 6ABD 60                       `

; ----------------------------------------------------------------------------
L6ABE:  .byte   $90                             ; 6ABE 90                       .
L6ABF:  nop                                     ; 6ABF EA                       .
L6AC0:  tya                                     ; 6AC0 98                       .
L6AC1:  pha                                     ; 6AC1 48                       H
L6AC2:  .byte   $4C                             ; 6AC2 4C                       L
L6AC3:  .byte   $B3                             ; 6AC3 B3                       .
L6AC4:  nop                                     ; 6AC4 EA                       .
L6AC5:  .byte   $4C                             ; 6AC5 4C                       L
L6AC6:  .byte   $94                             ; 6AC6 94                       .
L6AC7:  asl     a                               ; 6AC7 0A                       .
L6AC8:  .byte   $4C                             ; 6AC8 4C                       L
L6AC9:  .byte   $BB                             ; 6AC9 BB                       .
L6ACA:  asl     a                               ; 6ACA 0A                       .
L6ACB:  .byte   $4C                             ; 6ACB 4C                       L
L6ACC:  .byte   $C1                             ; 6ACC C1                       .
L6ACD:  asl     a                               ; 6ACD 0A                       .
L6ACE:  .byte   $4C                             ; 6ACE 4C                       L
L6ACF:  .byte   $C7                             ; 6ACF C7                       .
L6AD0:  asl     a                               ; 6AD0 0A                       .
L6AD1:  .byte   $9B                             ; 6AD1 9B                       .
L6AD2:  .byte   $44                             ; 6AD2 44                       D
L6AD3:  .byte   $31                             ; 6AD3 31                       1
L6AD4:  .byte   $3A                             ; 6AD4 3A                       :

L6AD5:	prolog
	jsr     sub_44D5                        ; 6AD8 20 D5 44                  .D
	.addr	L6ABE
	.byte	$05
	lda     L6ABE                           ; 6ADE AD BE 6A                 ..j
	jsr     L65B0                           ; 6AE1 20 B0 65                  .e
	lda     $A1                             ; 6AE4 A5 A1                    ..
	sta     L6AC5                           ; 6AE6 8D C5 6A                 ..j
	lda     $A0                             ; 6AE9 A5 A0                    ..
	sta     L6AC4                           ; 6AEB 8D C4 6A                 ..j
	lda     L6AC4                           ; 6AEE AD C4 6A                 ..j
	ora     L6AC5                           ; 6AF1 0D C5 6A                 ..j
	bne     L6AF9                           ; 6AF4 D0 03                    ..
	jmp     L6AFF                           ; 6AF6 4C FF 6A                 L.j

; ----------------------------------------------------------------------------
L6AF9:  lda     L6ABE                           ; 6AF9 AD BE 6A                 ..j
	jsr     L6995                           ; 6AFC 20 95 69                  .i
L6AFF:  ldx     #$00                            ; 6AFF A2 00                    ..
	lda     #$0B                            ; 6B01 A9 0B                    ..
	jsr     sub_606E
	lda     $A1                             ; 6B06 A5 A1                    ..
	sta     L6AC5                           ; 6B08 8D C5 6A                 ..j
	lda     $A0                             ; 6B0B A5 A0                    ..
	sta     L6AC4                           ; 6B0D 8D C4 6A                 ..j
	lda     L6ABE                           ; 6B10 AD BE 6A                 ..j
	asl     a                               ; 6B13 0A                       .
	php                                     ; 6B14 08                       .
	clc                                     ; 6B15 18                       .
	adc     L46E2                           ; 6B16 6D E2 46                 m.F
	sta     $AE                             ; 6B19 85 AE                    ..
	lda     #$00                            ; 6B1B A9 00                    ..
	rol     a                               ; 6B1D 2A                       *
	plp                                     ; 6B1E 28                       (
	adc     L46E3                           ; 6B1F 6D E3 46                 m.F
	sta     $AF                             ; 6B22 85 AF                    ..
	lda     L6AC5                           ; 6B24 AD C5 6A                 ..j
	ldy     #$01                            ; 6B27 A0 01                    ..
	sta     ($AE),y                         ; 6B29 91 AE                    ..
	lda     L6AC4                           ; 6B2B AD C4 6A                 ..j
	dey                                     ; 6B2E 88                       .
	sta     ($AE),y                         ; 6B2F 91 AE                    ..
	lda     #$00                            ; 6B31 A9 00                    ..
	sta     $A3                             ; 6B33 85 A3                    ..
	ldy     #$0B                            ; 6B35 A0 0B                    ..
	ldx     L6AC5                           ; 6B37 AE C5 6A                 ..j
	lda     L6AC4                           ; 6B3A AD C4 6A                 ..j
	jsr     L45F6                           ; 6B3D 20 F6 45                  .E
	lda     L6AC4                           ; 6B40 AD C4 6A                 ..j
	sta     $AE                             ; 6B43 85 AE                    ..
	lda     L6AC5                           ; 6B45 AD C5 6A                 ..j
	sta     $AF                             ; 6B48 85 AF                    ..
	.byte   $AD                             ; 6B4A AD                       .
	.byte   $BF                             ; 6B4B BF                       .
L6B4C:  ror     a                               ; 6B4C 6A                       j
	ldy     #$00                            ; 6B4D A0 00                    ..
	sta     ($AE),y                         ; 6B4F 91 AE                    ..
	clc                                     ; 6B51 18                       .
	lda     L6AC4                           ; 6B52 AD C4 6A                 ..j
	adc     #$01                            ; 6B55 69 01                    i.
	sta     $AE                             ; 6B57 85 AE                    ..
	lda     L6AC5                           ; 6B59 AD C5 6A                 ..j
	adc     #$00                            ; 6B5C 69 00                    i.
	sta     $AF                             ; 6B5E 85 AF                    ..
	lda     L6AC0                           ; 6B60 AD C0 6A                 ..j
	sta     ($AE),y                         ; 6B63 91 AE                    ..
	lda     #$00                            ; 6B65 A9 00                    ..
	sta     $85                             ; 6B67 85 85                    ..
	lda     L6AC0                           ; 6B69 AD C0 6A                 ..j
	sta     $84                             ; 6B6C 85 84                    ..
	lda     L6ABF                           ; 6B6E AD BF 6A                 ..j
	ldx     #$00                            ; 6B71 A2 00                    ..
	jsr     sub_444A
	sta     L6ACE                           ; 6B76 8D CE 6A                 ..j
	txa                                     ; 6B79 8A                       .
	sta     L6ACF                           ; 6B7A 8D CF 6A                 ..j
	clc                                     ; 6B7D 18                       .
	lda     L6AC4                           ; 6B7E AD C4 6A                 ..j
	adc     #$02                            ; 6B81 69 02                    i.
	sta     $AE                             ; 6B83 85 AE                    ..
	lda     L6AC5                           ; 6B85 AD C5 6A                 ..j
	adc     #$00                            ; 6B88 69 00                    i.
	sta     $AF                             ; 6B8A 85 AF                    ..
	lda     L6ACF                           ; 6B8C AD CF 6A                 ..j
	ldy     #$01                            ; 6B8F A0 01                    ..
	sta     ($AE),y                         ; 6B91 91 AE                    ..
	lda     L6ACE                           ; 6B93 AD CE 6A                 ..j
	dey                                     ; 6B96 88                       .
	sta     ($AE),y                         ; 6B97 91 AE                    ..
	clc                                     ; 6B99 18                       .
	lda     L6AC4                           ; 6B9A AD C4 6A                 ..j
	adc     #$04                            ; 6B9D 69 04                    i.
	sta     $AE                             ; 6B9F 85 AE                    ..
	lda     L6AC5                           ; 6BA1 AD C5 6A                 ..j
	adc     #$00                            ; 6BA4 69 00                    i.
	sta     $AF                             ; 6BA6 85 AF                    ..
	lda     $AF                             ; 6BA8 A5 AF                    ..
	pha                                     ; 6BAA 48                       H
	lda     $AE                             ; 6BAB A5 AE                    ..
	pha                                     ; 6BAD 48                       H
	lda     L6AC0                           ; 6BAE AD C0 6A                 ..j
	asl     a                               ; 6BB1 0A                       .
	sta     $A0                             ; 6BB2 85 A0                    ..
	lda     #$00                            ; 6BB4 A9 00                    ..
	sta     $A1                             ; 6BB6 85 A1                    ..
	ldx     $A1                             ; 6BB8 A6 A1                    ..
	lda     $A0                             ; 6BBA A5 A0                    ..
	jsr     sub_606E
	pla                                     ; 6BBF 68                       h
	sta     $AE                             ; 6BC0 85 AE                    ..
	pla                                     ; 6BC2 68                       h
	sta     $AF                             ; 6BC3 85 AF                    ..
	lda     $A1                             ; 6BC5 A5 A1                    ..
	ldy     #$01                            ; 6BC7 A0 01                    ..
	sta     ($AE),y                         ; 6BC9 91 AE                    ..
	lda     $A0                             ; 6BCB A5 A0                    ..
	dey                                     ; 6BCD 88                       .
	sta     ($AE),y                         ; 6BCE 91 AE                    ..
	ldx     L6ACF                           ; 6BD0 AE CF 6A                 ..j
	lda     L6ACE                           ; 6BD3 AD CE 6A                 ..j
	jsr     sub_606E
	lda     $A1                             ; 6BD9 A5 A1                    ..
	sta     L6AC7                           ; 6BDB 8D C7 6A                 ..j
	lda     $A0                             ; 6BDE A5 A0                    ..
	sta     L6AC6                           ; 6BE0 8D C6 6A                 ..j
	clc                                     ; 6BE3 18                       .
	lda     L6AC4                           ; 6BE4 AD C4 6A                 ..j
	adc     #$04                            ; 6BE7 69 04                    i.
	sta     $AE                             ; 6BE9 85 AE                    ..
	lda     L6AC5                           ; 6BEB AD C5 6A                 ..j
	adc     #$00                            ; 6BEE 69 00                    i.
	sta     $AF                             ; 6BF0 85 AF                    ..
	ldy     #$01                            ; 6BF2 A0 01                    ..
	lda     ($AE),y                         ; 6BF4 B1 AE                    ..
	sta     L6AD2                           ; 6BF6 8D D2 6A                 ..j
	dey                                     ; 6BF9 88                       .
	lda     ($AE),y                         ; 6BFA B1 AE                    ..
	sta     L6AD1                           ; 6BFC 8D D1 6A                 ..j
	lda     L6AC7                           ; 6BFF AD C7 6A                 ..j
	sta     L6ACB                           ; 6C02 8D CB 6A                 ..j
	lda     L6AC6                           ; 6C05 AD C6 6A                 ..j
	sta     L6ACA                           ; 6C08 8D CA 6A                 ..j
	sty     L6AD0                           ; 6C0B 8C D0 6A                 ..j
	sec                                     ; 6C0E 38                       8
	lda     L6AC0                           ; 6C0F AD C0 6A                 ..j
	sbc     #$01                            ; 6C12 E9 01                    ..
	sta     L6C22                           ; 6C14 8D 22 6C                 ."l
L6C17:  lda     L6C22                           ; 6C17 AD 22 6C                 ."l
	cmp     L6AD0                           ; 6C1A CD D0 6A                 ..j
	bcs     L6C23                           ; 6C1D B0 04                    ..
	jmp     L6C5C                           ; 6C1F 4C 5C 6C                 L\l

; ----------------------------------------------------------------------------
L6C22:  txa                                     ; 6C22 8A                       .
L6C23:  lda     L6AD0                           ; 6C23 AD D0 6A                 ..j
	asl     a                               ; 6C26 0A                       .
	php                                     ; 6C27 08                       .
	clc                                     ; 6C28 18                       .
	adc     L6AD1                           ; 6C29 6D D1 6A                 m.j
	sta     $AE                             ; 6C2C 85 AE                    ..
	lda     #$00                            ; 6C2E A9 00                    ..
	rol     a                               ; 6C30 2A                       *
	plp                                     ; 6C31 28                       (
	adc     L6AD2                           ; 6C32 6D D2 6A                 m.j
	sta     $AF                             ; 6C35 85 AF                    ..
	lda     L6ACB                           ; 6C37 AD CB 6A                 ..j
	ldy     #$01                            ; 6C3A A0 01                    ..
	sta     ($AE),y                         ; 6C3C 91 AE                    ..
	lda     L6ACA                           ; 6C3E AD CA 6A                 ..j
	dey                                     ; 6C41 88                       .
	.byte   $91                             ; 6C42 91                       .
L6C43:  ldx     LAD18                           ; 6C43 AE 18 AD                 ...
	dex                                     ; 6C46 CA                       .
	ror     a                               ; 6C47 6A                       j
	adc     L6ABF                           ; 6C48 6D BF 6A                 m.j
	sta     L6ACA                           ; 6C4B 8D CA 6A                 ..j
	lda     L6ACB                           ; 6C4E AD CB 6A                 ..j
	adc     #$00                            ; 6C51 69 00                    i.
	sta     L6ACB                           ; 6C53 8D CB 6A                 ..j
	inc     L6AD0                           ; 6C56 EE D0 6A                 ..j
	jmp     L6C17                           ; 6C59 4C 17 6C                 L.l

; ----------------------------------------------------------------------------
L6C5C:  lda     L6ACF                           ; 6C5C AD CF 6A                 ..j
	sta     $A3                             ; 6C5F 85 A3                    ..
	lda     #$00                            ; 6C61 A9 00                    ..
	sta     $A4                             ; 6C63 85 A4                    ..
	ldy     L6ACE                           ; 6C65 AC CE 6A                 ..j
	ldx     L6AC7                           ; 6C68 AE C7 6A                 ..j
	lda     L6AC6                           ; 6C6B AD C6 6A                 ..j
	jsr     L45FC                           ; 6C6E 20 FC 45                  .E
	clc                                     ; 6C71 18                       .
	lda     L6AC4                           ; 6C72 AD C4 6A                 ..j
	adc     #$06                            ; 6C75 69 06                    i.
	sta     $AE                             ; 6C77 85 AE                    ..
	lda     L6AC5                           ; 6C79 AD C5 6A                 ..j
	adc     #$00                            ; 6C7C 69 00                    i.
	sta     $AF                             ; 6C7E 85 AF                    ..
	lda     L6AC1                           ; 6C80 AD C1 6A                 ..j
	ldy     #$00                            ; 6C83 A0 00                    ..
	sta     ($AE),y                         ; 6C85 91 AE                    ..
	clc                                     ; 6C87 18                       .
	lda     L6AC4                           ; 6C88 AD C4 6A                 ..j
	adc     #$07                            ; 6C8B 69 07                    i.
	sta     $AE                             ; 6C8D 85 AE                    ..
	lda     L6AC5                           ; 6C8F AD C5 6A                 ..j
	adc     #$00                            ; 6C92 69 00                    i.
	sta     $AF                             ; 6C94 85 AF                    ..
	lda     #$00                            ; 6C96 A9 00                    ..
	iny                                     ; 6C98 C8                       .
	sta     ($AE),y                         ; 6C99 91 AE                    ..
	lda     #$00                            ; 6C9B A9 00                    ..
	dey                                     ; 6C9D 88                       .
	sta     ($AE),y                         ; 6C9E 91 AE                    ..
	lda     #$00                            ; 6CA0 A9 00                    ..
	cmp     L6AC1                           ; 6CA2 CD C1 6A                 ..j
	bcc     L6CAA                           ; 6CA5 90 03                    ..
	jmp     L6D5B                           ; 6CA7 4C 5B 6D                 L[m

; ----------------------------------------------------------------------------
L6CAA:  lda     #$00                            ; 6CAA A9 00                    ..
	sta     $85                             ; 6CAC 85 85                    ..
	lda     #$06                            ; 6CAE A9 06                    ..
	sta     $84                             ; 6CB0 85 84                    ..
	lda     L6AC1                           ; 6CB2 AD C1 6A                 ..j
	ldx     #$00                            ; 6CB5 A2 00                    ..
	jsr     sub_444A
	sta     L6ACC                           ; 6CBA 8D CC 6A                 ..j
	txa                                     ; 6CBD 8A                       .
	sta     L6ACD                           ; 6CBE 8D CD 6A                 ..j
	ldxa	L6ACC
	jsr     sub_606E
	rdmv	L6AD3, $A0
	clc                                     ; 6CD4 18                       .
	lda     L6AC4                           ; 6CD5 AD C4 6A                 ..j
	adc     #$07                            ; 6CD8 69 07                    i.
	sta     $AE                             ; 6CDA 85 AE                    ..
	lda     L6AC5                           ; 6CDC AD C5 6A                 ..j
	adc     #$00                            ; 6CDF 69 00                    i.
	sta     $AF                             ; 6CE1 85 AF                    ..
	lda     L6AD4                           ; 6CE3 AD D4 6A                 ..j
	ldy     #$01                            ; 6CE6 A0 01                    ..
	sta     ($AE),y                         ; 6CE8 91 AE                    ..
	lda     L6AD3                           ; 6CEA AD D3 6A                 ..j
	dey                                     ; 6CED 88                       .
	sta     ($AE),y                         ; 6CEE 91 AE                    ..
	lda     L6ACD                           ; 6CF0 AD CD 6A                 ..j
	sta     $A3                             ; 6CF3 85 A3                    ..
	ldy     L6ACC                           ; 6CF5 AC CC 6A                 ..j
	ldx     L6AD4                           ; 6CF8 AE D4 6A                 ..j
	lda     L6AD3                           ; 6CFB AD D3 6A                 ..j
	jsr     L45F6                           ; 6CFE 20 F6 45                  .E
	ldy     #$00                            ; 6D01 A0 00                    ..
	sty     L6AD0                           ; 6D03 8C D0 6A                 ..j
	sec                                     ; 6D06 38                       8
	lda     L6AC1                           ; 6D07 AD C1 6A                 ..j
	sbc     #$01                            ; 6D0A E9 01                    ..
	sta     L6D1A                           ; 6D0C 8D 1A 6D                 ..m
L6D0F:  lda     L6D1A                           ; 6D0F AD 1A 6D                 ..m
	cmp     L6AD0                           ; 6D12 CD D0 6A                 ..j
	bcs     L6D1B                           ; 6D15 B0 04                    ..
	jmp     L6D5B                           ; 6D17 4C 5B 6D                 L[m

; ----------------------------------------------------------------------------
L6D1A:  .byte   $4C                             ; 6D1A 4C                       L
L6D1B:  clc                                     ; 6D1B 18                       .
	lda     L6AD3                           ; 6D1C AD D3 6A                 ..j
	adc     #$04                            ; 6D1F 69 04                    i.
	sta     $AE                             ; 6D21 85 AE                    ..
	lda     L6AD4                           ; 6D23 AD D4 6A                 ..j
	adc     #$00                            ; 6D26 69 00                    i.
	sta     $AF                             ; 6D28 85 AF                    ..
	lda     #$FF                            ; 6D2A A9 FF                    ..
	ldy     #$00                            ; 6D2C A0 00                    ..
	sta     ($AE),y                         ; 6D2E 91 AE                    ..
	clc                                     ; 6D30 18                       .
	lda     L6AD3                           ; 6D31 AD D3 6A                 ..j
	adc     #$01                            ; 6D34 69 01                    i.
	sta     $AE                             ; 6D36 85 AE                    ..
	lda     L6AD4                           ; 6D38 AD D4 6A                 ..j
	adc     #$00                            ; 6D3B 69 00                    i.
	sta     $AF                             ; 6D3D 85 AF                    ..
	lda     L6AD0                           ; 6D3F AD D0 6A                 ..j
	sta     ($AE),y                         ; 6D42 91 AE                    ..
	clc                                     ; 6D44 18                       .
	lda     L6AD3                           ; 6D45 AD D3 6A                 ..j
	adc     #$06                            ; 6D48 69 06                    i.
	sta     L6AD3                           ; 6D4A 8D D3 6A                 ..j
	lda     L6AD4                           ; 6D4D AD D4 6A                 ..j
	adc     #$00                            ; 6D50 69 00                    i.
	sta     L6AD4                           ; 6D52 8D D4 6A                 ..j
	inc     L6AD0                           ; 6D55 EE D0 6A                 ..j
	jmp     L6D0F                           ; 6D58 4C 0F 6D                 L.m

; ----------------------------------------------------------------------------
L6D5B:  clc                                     ; 6D5B 18                       .
	lda     L6AC4                           ; 6D5C AD C4 6A                 ..j
	adc     #$09                            ; 6D5F 69 09                    i.
	sta     $AE                             ; 6D61 85 AE                    ..
	lda     L6AC5                           ; 6D63 AD C5 6A                 ..j
	adc     #$00                            ; 6D66 69 00                    i.
	sta     $AF                             ; 6D68 85 AF                    ..
	lda     #$00                            ; 6D6A A9 00                    ..
	ldy     #$01                            ; 6D6C A0 01                    ..
	sta     ($AE),y                         ; 6D6E 91 AE                    ..
	lda     #$00                            ; 6D70 A9 00                    ..
	dey                                     ; 6D72 88                       .
	sta     ($AE),y                         ; 6D73 91 AE                    ..
	lda     L6AC2                           ; 6D75 AD C2 6A                 ..j
	sta     $AE                             ; 6D78 85 AE                    ..
	lda     L6AC3                           ; 6D7A AD C3 6A                 ..j
	sta     $AF                             ; 6D7D 85 AF                    ..
	lda     #$00                            ; 6D7F A9 00                    ..
	cmp     ($AE),y                         ; 6D81 D1 AE                    ..
	lbcs	L6E3B
L6D88:  ldx     #$00                            ; 6D88 A2 00                    ..
	lda     #$1A                            ; 6D8A A9 1A                    ..
	jsr     sub_606E
	rdmv	L6AC8, $A0
	clc                                     ; 6D99 18                       .
	lda     L6AC4                           ; 6D9A AD C4 6A                 ..j
	adc     #$09                            ; 6D9D 69 09                    i.
	sta     $AE                             ; 6D9F 85 AE                    ..
	lda     L6AC5                           ; 6DA1 AD C5 6A                 ..j
	adc     #$00                            ; 6DA4 69 00                    i.
	sta     $AF                             ; 6DA6 85 AF                    ..
	lda     L6AC9                           ; 6DA8 AD C9 6A                 ..j
	ldy     #$01                            ; 6DAB A0 01                    ..
	sta     ($AE),y                         ; 6DAD 91 AE                    ..
	lda     L6AC8                           ; 6DAF AD C8 6A                 ..j
	dey                                     ; 6DB2 88                       .
	sta     ($AE),y                         ; 6DB3 91 AE                    ..
	lda     #$00                            ; 6DB5 A9 00                    ..
	sta     $A3                             ; 6DB7 85 A3                    ..
	ldy     #$1A                            ; 6DB9 A0 1A                    ..
	ldx     L6AC9                           ; 6DBB AE C9 6A                 ..j
	lda     L6AC8                           ; 6DBE AD C8 6A                 ..j
	jsr     L45F6                           ; 6DC1 20 F6 45                  .E
	lda     L6AC8                           ; 6DC4 AD C8 6A                 ..j
	sta     $AE                             ; 6DC7 85 AE                    ..
	lda     L6AC9                           ; 6DC9 AD C9 6A                 ..j
	sta     $AF                             ; 6DCC 85 AF                    ..
	lda     $AF                             ; 6DCE A5 AF                    ..
	pha                                     ; 6DD0 48                       H
	lda     $AE                             ; 6DD1 A5 AE                    ..
	pha                                     ; 6DD3 48                       H
	lda     L6AC2                           ; 6DD4 AD C2 6A                 ..j
	sta     $AE                             ; 6DD7 85 AE                    ..
	lda     L6AC3                           ; 6DD9 AD C3 6A                 ..j
	sta     $AF                             ; 6DDC 85 AF                    ..
	clc                                     ; 6DDE 18                       .
	ldy     #$00                            ; 6DDF A0 00                    ..
	lda     ($AE),y                         ; 6DE1 B1 AE                    ..
	adc     #$01                            ; 6DE3 69 01                    i.
	sta     $A0                             ; 6DE5 85 A0                    ..
	lda     #$00                            ; 6DE7 A9 00                    ..
	sta     $A1                             ; 6DE9 85 A1                    ..
	ldx     $A1                             ; 6DEB A6 A1                    ..
	lda     $A0                             ; 6DED A5 A0                    ..
	jsr     sub_606E                        ; 6DEF 20 6E 60                  n`
	pla                                     ; 6DF2 68                       h
	sta     $AE                             ; 6DF3 85 AE                    ..
	pla                                     ; 6DF5 68                       h
	sta     $AF                             ; 6DF6 85 AF                    ..
	lda     $A1                             ; 6DF8 A5 A1                    ..
	ldy     #$01                            ; 6DFA A0 01                    ..
	sta     ($AE),y                         ; 6DFC 91 AE                    ..
	lda     $A0                             ; 6DFE A5 A0                    ..
	dey                                     ; 6E00 88                       .
	sta     ($AE),y                         ; 6E01 91 AE                    ..
	lda     L6AC8                           ; 6E03 AD C8 6A                 ..j
	sta     $AE                             ; 6E06 85 AE                    ..
	lda     L6AC9                           ; 6E08 AD C9 6A                 ..j
	sta     $AF                             ; 6E0B 85 AF                    ..
	iny                                     ; 6E0D C8                       .
	lda     ($AE),y                         ; 6E0E B1 AE                    ..
	sta     $A1                             ; 6E10 85 A1                    ..
	dey                                     ; 6E12 88                       .
	lda     ($AE),y                         ; 6E13 B1 AE                    ..
	sta     $A0                             ; 6E15 85 A0                    ..
	lda     L6AC3                           ; 6E17 AD C3 6A                 ..j
	sta     $A3                             ; 6E1A 85 A3                    ..
	lda     L6AC2                           ; 6E1C AD C2 6A                 ..j
	sta     $AC                             ; 6E1F 85 AC                    ..
	lda     L6AC3                           ; 6E21 AD C3 6A                 ..j
	sta     $AD                             ; 6E24 85 AD                    ..
	clc                                     ; 6E26 18                       .
	lda     ($AC),y                         ; 6E27 B1 AC                    ..
	adc     #$01                            ; 6E29 69 01                    i.
	sta     $A4                             ; 6E2B 85 A4                    ..
	lda     #$00                            ; 6E2D A9 00                    ..
	sta     $A5                             ; 6E2F 85 A5                    ..
	ldy     L6AC2                           ; 6E31 AC C2 6A                 ..j
	ldx     $A1                             ; 6E34 A6 A1                    ..
	lda     $A0                             ; 6E36 A5 A0                    ..
	jsr     sub_461F
L6E3B:  ldi	$A0, $01
	rts                                     ; 6E3F 60                       `

; ----------------------------------------------------------------------------
L6E40:	sei                                     ; 6E40 78                       x
L6E41:  .byte   $6C                             ; 6E41 6C                       l
L6E42:  .byte   $66                             ; 6E42 66                       f
L6E43:  .byte   $66                             ; 6E43 66                       f
L6E44:  .byte   $6C                             ; 6E44 6C                       l
L6E45:  sei                                     ; 6E45 78                       x
L6E46:  brk                                     ; 6E46 00                       .
L6E47:  brk                                     ; 6E47 00                       .
L6E48:  .byte   $7E                             ; 6E48 7E                       ~
L6E49:  rts                                     ; 6E49 60                       `

; ----------------------------------------------------------------------------
L6E4A:  .byte   $7C                             ; 6E4A 7C                       |
L6E4B:  rts                                     ; 6E4B 60                       `

; ----------------------------------------------------------------------------
L6E4C:  rts                                     ; 6E4C 60                       `

; ----------------------------------------------------------------------------
L6E4D:  .byte   $7E                             ; 6E4D 7E                       ~
L6E4E:  brk                                     ; 6E4E 00                       .
	.byte   $03                             ; 6E4F 03                       .
	.byte   $43                             ; 6E50 43                       C
	.byte   $42                             ; 6E51 42                       B
	.byte   $53                             ; 6E52 53                       S
L6E53:  .byte   $4F                             ; 6E53 4F                       O
L6E54:  ror     $4303                           ; 6E54 6E 03 43                 n.C
	.byte   $42                             ; 6E57 42                       B
	.byte   $73                             ; 6E58 73                       s
L6E59:  .byte   $55                             ; 6E59 55                       U
L6E5A:  .byte   $6E                             ; 6E5A 6E                       n
L6E5B:  ror     $3E66                           ; 6E5B 6E 66 3E                 nf>
	brk                                     ; 6E5E 00                       .
L6E5F:  brk                                     ; 6E5F 00                       .
L6E60:  .byte   $66                             ; 6E60 66                       f

L6E61:  prolog
	jsr     sub_44D5
	.addr	L6E40
	.byte	$05
	.byte	$AD
	rti                                     ; 6E6B 40                       @

; ----------------------------------------------------------------------------
	ror     LB020                           ; 6E6C 6E 20 B0                 n .
	adc     $A5                             ; 6E6F 65 A5                    e.
	lda     ($8D,x)                         ; 6E71 A1 8D                    ..
	.byte   $47                             ; 6E73 47                       G
	ror     LA0A5                           ; 6E74 6E A5 A0                 n..
	sta     L6E46                           ; 6E77 8D 46 6E                 .Fn
	lda     L6E47                           ; 6E7A AD 47 6E                 .Gn
	sta     $A3                             ; 6E7D 85 A3                    ..
	lda     #$00                            ; 6E7F A9 00                    ..
	sta     $A5                             ; 6E81 85 A5                    ..
	lda     #$06                            ; 6E83 A9 06                    ..
	sta     $A4                             ; 6E85 85 A4                    ..
	ldy     L6E46                           ; 6E87 AC 46 6E                 .Fn
	ldx     #$6E                            ; 6E8A A2 6E                    .n
	lda     #$5B                            ; 6E8C A9 5B                    .[
	jsr     sub_461F
	sec                                     ; 6E91 38                       8
	lda     L6E44                           ; 6E92 AD 44 6E                 .Dn
	sbc     L6E42                           ; 6E95 ED 42 6E                 .Bn
	sta     $AE                             ; 6E98 85 AE                    ..
	clc                                     ; 6E9A 18                       .
	lda     $AE                             ; 6E9B A5 AE                    ..
	adc     #$01                            ; 6E9D 69 01                    i.
	sta     L6E49                           ; 6E9F 8D 49 6E                 .In
	lda     L6E49                           ; 6EA2 AD 49 6E                 .In
	sta     L6E4A                           ; 6EA5 8D 4A 6E                 .Jn
	lda     L6E43                           ; 6EA8 AD 43 6E                 .Cn
	asl     a                               ; 6EAB 0A                       .
	php                                     ; 6EAC 08                       .
	clc                                     ; 6EAD 18                       .
	adc     L6E5F                           ; 6EAE 6D 5F 6E                 m_n
	sta     $AE                             ; 6EB1 85 AE                    ..
	lda     #$00                            ; 6EB3 A9 00                    ..
	rol     a                               ; 6EB5 2A                       *
	plp                                     ; 6EB6 28                       (
	adc     L6E60                           ; 6EB7 6D 60 6E                 m`n
	sta     $AF                             ; 6EBA 85 AF                    ..
	clc                                     ; 6EBC 18                       .
	ldy     #$00                            ; 6EBD A0 00                    ..
	lda     ($AE),y                         ; 6EBF B1 AE                    ..
	adc     L6E42                           ; 6EC1 6D 42 6E                 mBn
	sta     L6E4B                           ; 6EC4 8D 4B 6E                 .Kn
	iny                                     ; 6EC7 C8                       .
	lda     ($AE),y                         ; 6EC8 B1 AE                    ..
	adc     #$00                            ; 6ECA 69 00                    i.
	sta     L6E4C                           ; 6ECC 8D 4C 6E                 .Ln
	lda     L6E54                           ; 6ECF AD 54 6E                 .Tn
	sta     L6E4E                           ; 6ED2 8D 4E 6E                 .Nn
	lda     L6E53                           ; 6ED5 AD 53 6E                 .Sn
	sta     L6E4D                           ; 6ED8 8D 4D 6E                 .Mn
	lda     L6E41                           ; 6EDB AD 41 6E                 .An
	lbne	L6EEF
	lda     L6E5A                           ; 6EE3 AD 5A 6E                 .Zn
	sta     L6E4E                           ; 6EE6 8D 4E 6E                 .Nn
	lda     L6E59                           ; 6EE9 AD 59 6E                 .Yn
	sta     L6E4D                           ; 6EEC 8D 4D 6E                 .Mn
L6EEF:  lda     L6E43                           ; 6EEF AD 43 6E                 .Cn
	sta     L6E48                           ; 6EF2 8D 48 6E                 .Hn
	lda     L6E45                           ; 6EF5 AD 45 6E                 .En
	sta     L6F06                           ; 6EF8 8D 06 6F                 ..o
	lda     L6F06                           ; 6EFB AD 06 6F                 ..o
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
	ldx     L6E4C                           ; 6F14 AE 4C 6E                 .Ln
	lda     L6E4B                           ; 6F17 AD 4B 6E                 .Kn
	jsr     sub_4B97
	lda     $A0                             ; 6F1D A5 A0                    ..
	sta     L6E4A                           ; 6F1F 8D 4A 6E                 .Jn
L6F22:  lda     #$00                            ; 6F22 A9 00                    ..
	sta     $A3                             ; 6F24 85 A3                    ..
	lda     #$00                            ; 6F26 A9 00                    ..
	sta     $A5                             ; 6F28 85 A5                    ..
	lda     L6E48                           ; 6F2A AD 48 6E                 .Hn
	sta     $A4                             ; 6F2D 85 A4                    ..
	lda     #$00                            ; 6F2F A9 00                    ..
	sta     $A7                             ; 6F31 85 A7                    ..
	lda     L6E4A                           ; 6F33 AD 4A 6E                 .Jn
	sta     $A6                             ; 6F36 85 A6                    ..
	lda     L6E4C                           ; 6F38 AD 4C 6E                 .Ln
	sta     $A9                             ; 6F3B 85 A9                    ..
	lda     L6E4B                           ; 6F3D AD 4B 6E                 .Kn
	sta     $A8                             ; 6F40 85 A8                    ..
	ldy     #$55                            ; 6F42 A0 55                    .U
	ldx     L6E4E                           ; 6F44 AE 4E 6E                 .Nn
	lda     L6E4D                           ; 6F47 AD 4D 6E                 .Mn
	jsr     sub_55A0
	clc                                     ; 6F4D 18                       .
	lda     L6E4B                           ; 6F4E AD 4B 6E                 .Kn
	adc     L6E5B                           ; 6F51 6D 5B 6E                 m[n
	sta     L6E4B                           ; 6F54 8D 4B 6E                 .Kn
	lda     L6E4C                           ; 6F57 AD 4C 6E                 .Ln
	adc     #$00                            ; 6F5A 69 00                    i.
	sta     L6E4C                           ; 6F5C 8D 4C 6E                 .Ln
	inc     L6E48                           ; 6F5F EE 48 6E                 .Hn
	.byte   $4C                             ; 6F62 4C                       L
L6F63:  .byte   $FB                             ; 6F63 FB                       .
	.byte   $6E                             ; 6F64 6E                       n
L6F65:  rts                                     ; 6F65 60                       `

; ----------------------------------------------------------------------------
L6F66:  .byte   $FF                             ; 6F66 FF                       .
L6F67:  brk                                     ; 6F67 00                       .

L6F68:	prolog
	stx     L6F67                           ; 6F6B 8E 67 6F                 .go
	sta     L6F66                           ; 6F6E 8D 66 6F                 .fo
	lda     L6F66                           ; 6F71 AD 66 6F                 .fo
	eor     #$01                            ; 6F74 49 01                    I.
	beq     L6F7B                           ; 6F76 F0 03                    ..
	jmp     L6F84                           ; 6F78 4C 84 6F                 L.o

; ----------------------------------------------------------------------------
L6F7B:  lda     L6F67                           ; 6F7B AD 67 6F                 .go
	sta     L4647                           ; 6F7E 8D 47 46                 .GF
	jmp     L6F8A                           ; 6F81 4C 8A 6F                 L.o

; ----------------------------------------------------------------------------
L6F84:  lda     L6F67                           ; 6F84 AD 67 6F                 .go
	sta     L4648                           ; 6F87 8D 48 46                 .HF
L6F8A:  rts                                     ; 6F8A 60                       `

; ----------------------------------------------------------------------------
L6F8B:  brk                                     ; 6F8B 00                       .
L6F8C:  brk                                     ; 6F8C 00                       .
L6F8D:  brk                                     ; 6F8D 00                       .
L6F8E:  brk                                     ; 6F8E 00                       .
L6F8F:  brk                                     ; 6F8F 00                       .
L6F90:  brk                                     ; 6F90 00                       .

L6F91:	prolog
	stx     L6F8C                           ; 6F94 8E 8C 6F                 ..o
	sta     L6F8B                           ; 6F97 8D 8B 6F                 ..o
	lda     L4647                           ; 6F9A AD 47 46                 .GF
	eor     #$FF                            ; 6F9D 49 FF                    I.
	beq     L6FA4                           ; 6F9F F0 03                    ..
	jmp     L6FA5                           ; 6FA1 4C A5 6F                 L.o

; ----------------------------------------------------------------------------
L6FA4:  rts                                     ; 6FA4 60                       `

; ----------------------------------------------------------------------------
L6FA5:  lda     L4647                           ; 6FA5 AD 47 46                 .GF
	jsr     L65B0                           ; 6FA8 20 B0 65                  .e
	lda     $A1                             ; 6FAB A5 A1                    ..
	sta     L6F8E                           ; 6FAD 8D 8E 6F                 ..o
	lda     $A0                             ; 6FB0 A5 A0                    ..
	sta     L6F8D                           ; 6FB2 8D 8D 6F                 ..o
	lda     L6F8E                           ; 6FB5 AD 8E 6F                 ..o
	sta     $A3                             ; 6FB8 85 A3                    ..
	lda     #$00                            ; 6FBA A9 00                    ..
	sta     $A5                             ; 6FBC 85 A5                    ..
	lda     #$02                            ; 6FBE A9 02                    ..
	sta     $A4                             ; 6FC0 85 A4                    ..
	ldy     L6F8D                           ; 6FC2 AC 8D 6F                 ..o
	.byte   $A2                             ; 6FC5 A2                       .
L6FC6:  .byte   $6F                             ; 6FC6 6F                       o
	lda     #$8F                            ; 6FC7 A9 8F                    ..
	jsr     sub_461F
	lda     L6F8F                           ; 6FCC AD 8F 6F                 ..o
	sta     $A3                             ; 6FCF 85 A3                    ..
	lda     L6F8C                           ; 6FD1 AD 8C 6F                 ..o
	sta     $A5                             ; 6FD4 85 A5                    ..
	lda     L6F8B                           ; 6FD6 AD 8B 6F                 ..o
L6FD9:  sta     $A4                             ; 6FD9 85 A4                    ..
L6FDB:  ldy     L6F90                           ; 6FDB AC 90 6F                 ..o
	ldx     #$00                            ; 6FDE A2 00                    ..
	lda     L4647                           ; 6FE0 AD 47 46                 .GF
	jsr     L67D8                           ; 6FE3 20 D8 67                  .g
	rts                                     ; 6FE6 60                       `

; ----------------------------------------------------------------------------
L6FE7:  .byte   $F0                             ; 6FE7 F0                       .
L6FE8:  .byte   $F0                             ; 6FE8 F0                       .
L6FE9:  .byte   $F0                             ; 6FE9 F0                       .
L6FEA:  ;beq     $7038                           ; 6FEA F0 4C                    .L
	.byte	$F0

L6FEB:	prolog
	stx     L6FE8                           ; 6FEE 8E E8 6F                 ..o
	sta     L6FE7                           ; 6FF1 8D E7 6F                 ..o
	lda     L4648                           ; 6FF4 AD 48 46                 .HF
	eor     #$FF                            ; 6FF7 49 FF                    I.
	lbne	L6FFF
L6FFE:  rts                                     ; 6FFE 60                       `

; ----------------------------------------------------------------------------
L6FFF:  lda     L4648                           ; 6FFF AD 48 46                 .HF
	jsr     L65B0                           ; 7002 20 B0 65                  .e
	lda     $A1                             ; 7005 A5 A1                    ..
	sta     L6FEA                           ; 7007 8D EA 6F                 ..o
	lda     $A0                             ; 700A A5 A0                    ..
	sta     L6FE9                           ; 700C 8D E9 6F                 ..o
	lda     L6FE9                           ; 700F AD E9 6F                 ..o
	sta     $AE                             ; 7012 85 AE                    ..
	lda     L6FEA                           ; 7014 AD EA 6F                 ..o
	sta     $AF                             ; 7017 85 AF                    ..
	ldy     #$00                            ; 7019 A0 00                    ..
	lda     ($AE),y                         ; 701B B1 AE                    ..
	sta     $A3                             ; 701D 85 A3                    ..
	lda     L6FE8                           ; 701F AD E8 6F                 ..o
	sta     $A5                             ; 7022 85 A5                    ..
	lda     L6FE7                           ; 7024 AD E7 6F                 ..o
	sta     $A4                             ; 7027 85 A4                    ..
	ldy     #$00                            ; 7029 A0 00                    ..
	ldx     #$00                            ; 702B A2 00                    ..
	lda     L4648                           ; 702D AD 48 46                 .HF
	jsr     L67D8                           ; 7030 20 D8 67                  .g
	rts                                     ; 7033 60                       `

; ----------------------------------------------------------------------------
	.byte   $66                             ; 7034 66                       f

sub_7035:  
	prolog
	sta     $7034                           ; 7038 8D 34 70                 .4p
	lda     $7034                           ; 703B AD 34 70                 .4p
	asl     a                               ; 703E 0A                       .
	php                                     ; 703F 08                       .
	clc                                     ; 7040 18                       .
	adc     L46A2                           ; 7041 6D A2 46                 m.F
	sta     $AE                             ; 7044 85 AE                    ..
	lda     #$00                            ; 7046 A9 00                    ..
	rol     a                               ; 7048 2A                       *
L7049:  plp                                     ; 7049 28                       (
	.byte   $6D                             ; 704A 6D                       m
	.byte   $A3                             ; 704B A3                       .
L704C:  lsr     $85                             ; 704C 46 85                    F.
	.byte   $AF                             ; 704E AF                       .
	ldy     #$01                            ; 704F A0 01                    ..
	lda     ($AE),y                         ; 7051 B1 AE                    ..
	sta     $A1                             ; 7053 85 A1                    ..
	dey                                     ; 7055 88                       .
	lda     ($AE),y                         ; 7056 B1 AE                    ..
	sta     $A0                             ; 7058 85 A0                    ..
	rts                                     ; 705A 60                       `

; ----------------------------------------------------------------------------
L705B:  .byte   $66                             ; 705B 66                       f
L705C:  .byte   $3E                             ; 705C 3E                       >
L705D:  .byte   $06                             ; 705D 06                       .
L705E:  .byte   $7C                             ; 705E 7C                       |

L705F:	prolog
	stx     L705C                           ; 7062 8E 5C 70                 .\p
	sta     L705B                           ; 7065 8D 5B 70                 .[p
	lda     L705B                           ; 7068 AD 5B 70                 .[p
	jsr     sub_7035
	lda     $A1                             ; 706E A5 A1                    ..
	sta     L705E                           ; 7070 8D 5E 70                 .^p
	lda     $A0                             ; 7073 A5 A0                    ..
	sta     L705D                           ; 7075 8D 5D 70                 .]p
	clc                                     ; 7078 18                       .
	lda     L705D                           ; 7079 AD 5D 70                 .]p
	adc     #$09                            ; 707C 69 09                    i.
	sta     $AE                             ; 707E 85 AE                    ..
	lda     L705E                           ; 7080 AD 5E 70                 .^p
	adc     #$00                            ; 7083 69 00                    i.
	sta     $AF                             ; 7085 85 AF                    ..
	lda     L705C                           ; 7087 AD 5C 70                 .\p
	ldy     #$00                            ; 708A A0 00                    ..
	sta     ($AE),y                         ; 708C 91 AE                    ..
	rts                                     ; 708E 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 708F 00                       .
L7090:  brk                                     ; 7090 00                       .
L7091:  .byte   $7C                             ; 7091 7C                       |
L7092:  .byte   $66                             ; 7092 66                       f
L7093:  ror     $66                             ; 7093 66 66                    ff
	.byte	$66

L7096:	prolog
	jsr     sub_44D5                           ; 7099 20 D5 44                  .D
	.byte   $8F                             ; 709C 8F                       .
	bvs     L70A1                           ; 709D 70 02                    p.
	.byte   $AD                             ; 709F AD                       .
	.byte   $8F                             ; 70A0 8F                       .
L70A1:  bvs     L70C3                           ; 70A1 70 20                    p 
	and     $70,x                           ; 70A3 35 70                    5p
	lda     $A1                             ; 70A5 A5 A1                    ..
	sta     L7093                           ; 70A7 8D 93 70                 ..p
	lda     $A0                             ; 70AA A5 A0                    ..
	sta     L7092                           ; 70AC 8D 92 70                 ..p
	clc                                     ; 70AF 18                       .
	lda     L7092                           ; 70B0 AD 92 70                 ..p
	adc     #$1E                            ; 70B3 69 1E                    i.
	sta     $A0                             ; 70B5 85 A0                    ..
	lda     L7093                           ; 70B7 AD 93 70                 ..p
	adc     #$00                            ; 70BA 69 00                    i.
	sta     $A1                             ; 70BC 85 A1                    ..
	lda     L7091                           ; 70BE AD 91 70                 ..p
	sta     $A3                             ; 70C1 85 A3                    ..
L70C3:  lda     #$00                            ; 70C3 A9 00                    ..
	sta     $A5                             ; 70C5 85 A5                    ..
	lda     #$04                            ; 70C7 A9 04                    ..
	sta     $A4                             ; 70C9 85 A4                    ..
	ldy     L7090                           ; 70CB AC 90 70                 ..p
	ldx     $A1                             ; 70CE A6 A1                    ..
	lda     $A0                             ; 70D0 A5 A0                    ..
	jsr     sub_461F
	rts                                     ; 70D5 60                       `

; ----------------------------------------------------------------------------
L70D6:  brk                                     ; 70D6 00                       .
L70D7:  brk                                     ; 70D7 00                       .
L70D8:  brk                                     ; 70D8 00                       .
L70D9:  .byte   $63                             ; 70D9 63                       c
L70DA:  .byte   $6B                             ; 70DA 6B                       k
L70DB:  .byte   $7F                             ; 70DB 7F                       .
L70DC:  .byte   $3E                             ; 70DC 3E                       >
L70DD:  .byte   $36                             ; 70DD 36                       6
L70DE:  brk                                     ; 70DE 00                       .
L70DF:  brk                                     ; 70DF 00                       .
L70E0:  brk                                     ; 70E0 00                       .
L70E1:	.byte	$66

L70E2:	prolog
	jsr     sub_44D5                           ; 70E5 20 D5 44                  .D
	dec     $70,x                           ; 70E8 D6 70                    .p
	.byte   $02                             ; 70EA 02                       .
	lda     L70D6                           ; 70EB AD D6 70                 ..p
	jsr     sub_7035
	lda     $A1                             ; 70F1 A5 A1                    ..
	sta     L70DA                           ; 70F3 8D DA 70                 ..p
	lda     $A0                             ; 70F6 A5 A0                    ..
	sta     L70D9                           ; 70F8 8D D9 70                 ..p
	clc                                     ; 70FB 18                       .
	lda     L70D9                           ; 70FC AD D9 70                 ..p
	adc     #$22                            ; 70FF 69 22                    i"
	sta     L70DD                           ; 7101 8D DD 70                 ..p
	lda     L70DA                           ; 7104 AD DA 70                 ..p
	adc     #$00                            ; 7107 69 00                    i.
	sta     L70DE                           ; 7109 8D DE 70                 ..p
	lda     L70D8                           ; 710C AD D8 70                 ..p
	sta     $A3                             ; 710F 85 A3                    ..
	lda     #$00                            ; 7111 A9 00                    ..
	sta     $A5                             ; 7113 85 A5                    ..
	lda     #$04                            ; 7115 A9 04                    ..
	sta     $A4                             ; 7117 85 A4                    ..
	ldy     L70D7                           ; 7119 AC D7 70                 ..p
	ldx     L70DE                           ; 711C AE DE 70                 ..p
	lda     L70DD                           ; 711F AD DD 70                 ..p
	jsr     sub_461F
	clc                                     ; 7125 18                       .
	lda     L70D9                           ; 7126 AD D9 70                 ..p
	adc     #$04                            ; 7129 69 04                    i.
	sta     $AE                             ; 712B 85 AE                    ..
	lda     L70DA                           ; 712D AD DA 70                 ..p
	adc     #$00                            ; 7130 69 00                    i.
	sta     $AF                             ; 7132 85 AF                    ..
	ldy     #$00                            ; 7134 A0 00                    ..
	lda     ($AE),y                         ; 7136 B1 AE                    ..
	sta     L70DF                           ; 7138 8D DF 70                 ..p
	lda     L70DF                           ; 713B AD DF 70                 ..p
	jsr     L65B0                           ; 713E 20 B0 65                  .e
	lda     $A1                             ; 7141 A5 A1                    ..
	sta     L70DC                           ; 7143 8D DC 70                 ..p
	lda     $A0                             ; 7146 A5 A0                    ..
	sta     L70DB                           ; 7148 8D DB 70                 ..p
	lda     L70DC                           ; 714B AD DC 70                 ..p
	sta     $A3                             ; 714E 85 A3                    ..
	lda     #$00                            ; 7150 A9 00                    ..
	sta     $A5                             ; 7152 85 A5                    ..
	lda     #$02                            ; 7154 A9 02                    ..
	sta     $A4                             ; 7156 85 A4                    ..
	ldy     L70DB                           ; 7158 AC DB 70                 ..p
	ldx     #$70                            ; 715B A2 70                    .p
	lda     #$E0                            ; 715D A9 E0                    ..
	jsr     sub_461F
	clc                                     ; 7162 18                       .
	lda     L70DD                           ; 7163 AD DD 70                 ..p
	adc     #$02                            ; 7166 69 02                    i.
	sta     $AE                             ; 7168 85 AE                    ..
	lda     L70DE                           ; 716A AD DE 70                 ..p
	adc     #$00                            ; 716D 69 00                    i.
	sta     $AF                             ; 716F 85 AF                    ..
	clc                                     ; 7171 18                       .
	ldy     #$00                            ; 7172 A0 00                    ..
	lda     ($AE),y                         ; 7174 B1 AE                    ..
	adc     L70E0                           ; 7176 6D E0 70                 m.p
	sta     $AC                             ; 7179 85 AC                    ..
	sec                                     ; 717B 38                       8
	lda     $AC                             ; 717C A5 AC                    ..
	sbc     #$01                            ; 717E E9 01                    ..
	sta     ($AE),y                         ; 7180 91 AE                    ..
	clc                                     ; 7182 18                       .
	lda     L70DD                           ; 7183 AD DD 70                 ..p
	adc     #$03                            ; 7186 69 03                    i.
	sta     $AE                             ; 7188 85 AE                    ..
	lda     L70DE                           ; 718A AD DE 70                 ..p
	adc     #$00                            ; 718D 69 00                    i.
	sta     $AF                             ; 718F 85 AF                    ..
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
L71A1:  brk                                     ; 71A1 00                       .
L71A2:  brk                                     ; 71A2 00                       .
L71A3:  brk                                     ; 71A3 00                       .
	brk                                     ; 71A4 00                       .
	brk                                     ; 71A5 00                       .
	brk                                     ; 71A6 00                       .
	brk                                     ; 71A7 00                       .
	brk                                     ; 71A8 00                       .
	brk                                     ; 71A9 00                       .
	brk                                     ; 71AA 00                       .
	brk                                     ; 71AB 00                       .
L71AC:  brk                                     ; 71AC 00                       .
L71AD:  brk                                     ; 71AD 00                       .
	brk                                     ; 71AE 00                       .
	brk                                     ; 71AF 00                       .
L71B0:  brk                                     ; 71B0 00                       .
L71B1:  brk                                     ; 71B1 00                       .
L71B2:  brk                                     ; 71B2 00                       .
L71B3:  brk                                     ; 71B3 00                       .
L71B4:  brk                                     ; 71B4 00                       .
L71B5:  jmp     L71B8                           ; 71B5 4C B8 71                 L.q

; ----------------------------------------------------------------------------
L71B8:  sta     L71A1                           ; 71B8 8D A1 71                 ..q
	lda     L71A1                           ; 71BB AD A1 71                 ..q
	jsr     sub_7035
	lda     $A1                             ; 71C1 A5 A1                    ..
	sta     L71A3                           ; 71C3 8D A3 71                 ..q
	lda     $A0                             ; 71C6 A5 A0                    ..
	sta     L71A2                           ; 71C8 8D A2 71                 ..q
	lda     L71A3                           ; 71CB AD A3 71                 ..q
	sta     $A3                             ; 71CE 85 A3                    ..
	lda     #$00                            ; 71D0 A9 00                    ..
	sta     $A5                             ; 71D2 85 A5                    ..
	lda     #$07                            ; 71D4 A9 07                    ..
	sta     $A4                             ; 71D6 85 A4                    ..
	ldy     L71A2                           ; 71D8 AC A2 71                 ..q
	ldx     #$71                            ; 71DB A2 71                    .q
	lda     #$AE                            ; 71DD A9 AE                    ..
	jsr     sub_461F
	lda     L71B2                           ; 71E2 AD B2 71                 ..q
	jsr     L65B0                           ; 71E5 20 B0 65                  .e
	lda     $A1                             ; 71E8 A5 A1                    ..
	sta     L71AD                           ; 71EA 8D AD 71                 ..q
	lda     $A0                             ; 71ED A5 A0                    ..
	sta     L71AC                           ; 71EF 8D AC 71                 ..q
	sec                                     ; 71F2 38                       8
	lda     #$00                            ; 71F3 A9 00                    ..
	sbc     L71B0                           ; 71F5 ED B0 71                 ..q
	sta     $A2                             ; 71F8 85 A2                    ..
	sec                                     ; 71FA 38                       8
	lda     #$00                            ; 71FB A9 00                    ..
	sbc     L71B1                           ; 71FD ED B1 71                 ..q
	sta     $A3                             ; 7200 85 A3                    ..
	lda     L71AC                           ; 7202 AD AC 71                 ..q
	sta     $AA                             ; 7205 85 AA                    ..
	lda     L71AD                           ; 7207 AD AD 71                 ..q
	sta     $AB                             ; 720A 85 AB                    ..
	sec                                     ; 720C 38                       8
	ldy     #$00                            ; 720D A0 00                    ..
	lda     ($AA),y                         ; 720F B1 AA                    ..
	sbc     #$01                            ; 7211 E9 01                    ..
	sta     $A8                             ; 7213 85 A8                    ..
	sec                                     ; 7215 38                       8
	lda     $A8                             ; 7216 A5 A8                    ..
	sbc     L71B0                           ; 7218 ED B0 71                 ..q
	sta     $A4                             ; 721B 85 A4                    ..
	clc                                     ; 721D 18                       .
	lda     L71AC                           ; 721E AD AC 71                 ..q
	adc     #$01                            ; 7221 69 01                    i.
	sta     $A8                             ; 7223 85 A8                    ..
	lda     L71AD                           ; 7225 AD AD 71                 ..q
	adc     #$00                            ; 7228 69 00                    i.
	sta     $A9                             ; 722A 85 A9                    ..
	sec                                     ; 722C 38                       8
	lda     ($A8),y                         ; 722D B1 A8                    ..
	sbc     #$01                            ; 722F E9 01                    ..
	sta     $A6                             ; 7231 85 A6                    ..
	sec                                     ; 7233 38                       8
	lda     $A6                             ; 7234 A5 A6                    ..
	sbc     L71B1                           ; 7236 ED B1 71                 ..q
	sta     $A5                             ; 7239 85 A5                    ..
	ldy     $A2                             ; 723B A4 A2                    ..
	ldx     #$71                            ; 723D A2 71                    .q
	lda     #$A4                            ; 723F A9 A4                    ..
	jsr     sub_4BF2
	lda     #$00                            ; 7244 A9 00                    ..
	sta     $A3                             ; 7246 85 A3                    ..
	sec                                     ; 7248 38                       8
	lda     L71B3                           ; 7249 AD B3 71                 ..q
	sbc     #$01                            ; 724C E9 01                    ..
	sta     $A4                             ; 724E 85 A4                    ..
	sec                                     ; 7250 38                       8
	lda     L71B4                           ; 7251 AD B4 71                 ..q
	sbc     #$01                            ; 7254 E9 01                    ..
	sta     $A5                             ; 7256 85 A5                    ..
	ldy     #$00                            ; 7258 A0 00                    ..
	ldx     #$71                            ; 725A A2 71                    .q
	lda     #$A8                            ; 725C A9 A8                    ..
	jsr     sub_4BF2
	clc                                     ; 7261 18                       .
	lda     L71A2                           ; 7262 AD A2 71                 ..q
	adc     #$0B                            ; 7265 69 0B                    i.
	sta     $AE                             ; 7267 85 AE                    ..
	lda     L71A3                           ; 7269 AD A3 71                 ..q
	adc     #$00                            ; 726C 69 00                    i.
	sta     $AF                             ; 726E 85 AF                    ..
	.byte   $A5                             ; 7270 A5                       .
L7271:  .byte   $AF                             ; 7271 AF                       .
	pha                                     ; 7272 48                       H
	lda     $AE                             ; 7273 A5 AE                    ..
	pha                                     ; 7275 48                       H
	lda     #$71                            ; 7276 A9 71                    .q
	sta     $A3                             ; 7278 85 A3                    ..
	clc                                     ; 727A 18                       .
	lda     L71A2                           ; 727B AD A2 71                 ..q
	adc     #$16                            ; 727E 69 16                    i.
	sta     $A4                             ; 7280 85 A4                    ..
	lda     L71A3                           ; 7282 AD A3 71                 ..q
	adc     #$00                            ; 7285 69 00                    i.
	sta     $A5                             ; 7287 85 A5                    ..
	ldy     #$A8                            ; 7289 A0 A8                    ..
	ldx     #$71                            ; 728B A2 71                    .q
	lda     #$A4                            ; 728D A9 A4                    ..
	jsr     L4CF5                           ; 728F 20 F5 4C                  .L
	pla                                     ; 7292 68                       h
	sta     $AE                             ; 7293 85 AE                    ..
	pla                                     ; 7295 68                       h
	sta     $AF                             ; 7296 85 AF                    ..
	lda     $A0                             ; 7298 A5 A0                    ..
	ldy     #$00                            ; 729A A0 00                    ..
	sta     ($AE),y                         ; 729C 91 AE                    ..
	rts                                     ; 729E 60                       `

; ----------------------------------------------------------------------------
L729F:  .byte   $10                             ; 729F 10                       .
L72A0:  .byte   $D0                             ; 72A0 D0                       .
L72A1:  .byte   $E2                             ; 72A1 E2                       .
	lda     $43                             ; 72A2 A5 43                    .C
	sta     $02E7                           ; 72A4 8D E7 02                 ...
	lda     $44                             ; 72A7 A5 44                    .D
	.byte   $8D                             ; 72A9 8D                       .
L72AA:  inx                                     ; 72AA E8                       .
L72AB:  .byte   $02                             ; 72AB 02                       .
	jmp     LE59E                           ; 72AC 4C 9E E5                 L..

; ----------------------------------------------------------------------------
L72AF:  clc                                     ; 72AF 18                       .
L72B0:  .byte   $A5                             ; 72B0 A5                       .
L72B1:  jmp     $72B4                           ; 72B1 4C B4 72                 L.r

; ----------------------------------------------------------------------------
	sta     L729F                           ; 72B4 8D 9F 72                 ..r
	lda     L729F                           ; 72B7 AD 9F 72                 ..r
	jsr     sub_7035
	lda     $A1                             ; 72BD A5 A1                    ..
	sta     L72A1                           ; 72BF 8D A1 72                 ..r
	lda     $A0                             ; 72C2 A5 A0                    ..
	sta     L72A0                           ; 72C4 8D A0 72                 ..r
	lda     L72A1                           ; 72C7 AD A1 72                 ..r
	sta     $A3                             ; 72CA 85 A3                    ..
	lda     #$00                            ; 72CC A9 00                    ..
	sta     $A5                             ; 72CE 85 A5                    ..
	lda     #$07                            ; 72D0 A9 07                    ..
	sta     $A4                             ; 72D2 85 A4                    ..
	ldy     L72A0                           ; 72D4 AC A0 72                 ..r
	ldx     #$72                            ; 72D7 A2 72                    .r
	lda     #$AA                            ; 72D9 A9 AA                    ..
	jsr     sub_461F
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
	ldx     #$72                            ; 7300 A2 72                    .r
	lda     #$A2                            ; 7302 A9 A2                    ..
	jsr     sub_4BF2
	lda     #$00                            ; 7307 A9 00                    ..
	sta     $A3                             ; 7309 85 A3                    ..
	sec                                     ; 730B 38                       8
	lda     L72AF                           ; 730C AD AF 72                 ..r
	sbc     #$01                            ; 730F E9 01                    ..
	sta     $A4                             ; 7311 85 A4                    ..
	sec                                     ; 7313 38                       8
	lda     L72B0                           ; 7314 AD B0 72                 ..r
	sbc     #$01                            ; 7317 E9 01                    ..
	sta     $A5                             ; 7319 85 A5                    ..
	ldy     #$00                            ; 731B A0 00                    ..
	ldx     #$72                            ; 731D A2 72                    .r
	lda     #$A6                            ; 731F A9 A6                    ..
	jsr     sub_4BF2
	clc                                     ; 7324 18                       .
	lda     L72A0                           ; 7325 AD A0 72                 ..r
	adc     #$0A                            ; 7328 69 0A                    i.
	sta     $AE                             ; 732A 85 AE                    ..
	lda     L72A1                           ; 732C AD A1 72                 ..r
	adc     #$00                            ; 732F 69 00                    i.
	sta     $AF                             ; 7331 85 AF                    ..
	lda     $AF                             ; 7333 A5 AF                    ..
	pha                                     ; 7335 48                       H
	lda     $AE                             ; 7336 A5 AE                    ..
	pha                                     ; 7338 48                       H
	lda     #$72                            ; 7339 A9 72                    .r
	sta     $A3                             ; 733B 85 A3                    ..
	clc                                     ; 733D 18                       .
	lda     L72A0                           ; 733E AD A0 72                 ..r
	adc     #$1A                            ; 7341 69 1A                    i.
	sta     $A4                             ; 7343 85 A4                    ..
	lda     L72A1                           ; 7345 AD A1 72                 ..r
	adc     #$00                            ; 7348 69 00                    i.
	sta     $A5                             ; 734A 85 A5                    ..
	ldy     #$A6                            ; 734C A0 A6                    ..
	ldx     #$72                            ; 734E A2 72                    .r
	lda     #$A2                            ; 7350 A9 A2                    ..
	.byte   $20                             ; 7352 20                        
	.byte   $F5                             ; 7353 F5                       .
L7354:  jmp     $8568                           ; 7354 4C 68 85                 Lh.

; ----------------------------------------------------------------------------
	ldx     $8568                           ; 7357 AE 68 85                 .h.
	.byte   $AF                             ; 735A AF                       .
	lda     $A0                             ; 735B A5 A0                    ..
	ldy     #$00                            ; 735D A0 00                    ..
	sta     ($AE),y                         ; 735F 91 AE                    ..
	rts                                     ; 7361 60                       `

; ----------------------------------------------------------------------------
L7362:  .byte   $2D                             ; 7362 2D                       -
L7363:  .byte   $F0                             ; 7363 F0                       .
L7364:  .byte   $20                             ; 7364 20                        
L7365:  .byte   $90                             ; 7365 90                       .
L7366:  .byte   $ED                             ; 7366 ED                       .
L7367:  .byte   $AD                             ; 7367 AD                       .
L7368:  jmp     $736B                           ; 7368 4C 6B 73                 Lks

; ----------------------------------------------------------------------------
	jsr     sub_44D5                        ; 736B 20 D5 44                  .D
	.byte   $62                             ; 736E 62                       b
	.byte   $73                             ; 736F 73                       s
	.byte   $02                             ; 7370 02                       .
	lda     L7364                           ; 7371 AD 64 73                 .ds
	jsr     sub_7035
	lda     $A1                             ; 7377 A5 A1                    ..
	sta     L7366                           ; 7379 8D 66 73                 .fs
	lda     $A0                             ; 737C A5 A0                    ..
	sta     L7365                           ; 737E 8D 65 73                 .es
	lda     L7365                           ; 7381 AD 65 73                 .es
	sta     $AE                             ; 7384 85 AE                    ..
	lda     L7366                           ; 7386 AD 66 73                 .fs
	sta     $AF                             ; 7389 85 AF                    ..
	sec                                     ; 738B 38                       8
	lda     L7362                           ; 738C AD 62 73                 .bs
	ldy     #$00                            ; 738F A0 00                    ..
	sbc     ($AE),y                         ; 7391 F1 AE                    ..
	sta     L7362                           ; 7393 8D 62 73                 .bs
	clc                                     ; 7396 18                       .
	lda     L7365                           ; 7397 AD 65 73                 .es
	adc     #$01                            ; 739A 69 01                    i.
	sta     $AE                             ; 739C 85 AE                    ..
	lda     L7366                           ; 739E AD 66 73                 .fs
	adc     #$00                            ; 73A1 69 00                    i.
	sta     $AF                             ; 73A3 85 AF                    ..
	sec                                     ; 73A5 38                       8
	lda     L7363                           ; 73A6 AD 63 73                 .cs
	sbc     ($AE),y                         ; 73A9 F1 AE                    ..
	sta     L7363                           ; 73AB 8D 63 73                 .cs
	clc                                     ; 73AE 18                       .
	lda     L7365                           ; 73AF AD 65 73                 .es
	adc     #$1A                            ; 73B2 69 1A                    i.
	sta     $A2                             ; 73B4 85 A2                    ..
	lda     L7366                           ; 73B6 AD 66 73                 .fs
	adc     #$00                            ; 73B9 69 00                    i.
	sta     $A3                             ; 73BB 85 A3                    ..
	ldy     $A2                             ; 73BD A4 A2                    ..
	ldx     L7363                           ; 73BF AE 63 73                 .cs
	lda     L7362                           ; 73C2 AD 62 73                 .bs
	jsr     L4C75                           ; 73C5 20 75 4C                  uL
	lda     $A0                             ; 73C8 A5 A0                    ..
	sta     L7367                           ; 73CA 8D 67 73                 .gs
	lda     L7367                           ; 73CD AD 67 73                 .gs
	sta     $A0                             ; 73D0 85 A0                    ..
	rts                                     ; 73D2 60                       `

; ----------------------------------------------------------------------------
L73D3:  .byte   $EF                             ; 73D3 EF                       .
L73D4:  .byte   $A9                             ; 73D4 A9                       .
L73D5:  brk                                     ; 73D5 00                       .
L73D6:  .byte   $9D                             ; 73D6 9D                       .
L73D7:  .byte   $9D                             ; 73D7 9D                       .
L73D8:  .byte   $F0                             ; 73D8 F0                       .
L73D9:  .byte   $AD                             ; 73D9 AD                       .
L73DA:  jmp     $73DD                           ; 73DA 4C DD 73                 L.s

; ----------------------------------------------------------------------------
	stx     L73D4                           ; 73DD 8E D4 73                 ..s
	sta     L73D3                           ; 73E0 8D D3 73                 ..s
	ldy     #$00                            ; 73E3 A0 00                    ..
	sty     L73D7                           ; 73E5 8C D7 73                 ..s
	iny                                     ; 73E8 C8                       .
	sty     L73D6                           ; 73E9 8C D6 73                 ..s
	lda     L4673                           ; 73EC AD 73 46                 .sF
	sta     L73FD                           ; 73EF 8D FD 73                 ..s
L73F2:  lda     L73FD                           ; 73F2 AD FD 73                 ..s
	cmp     L73D6                           ; 73F5 CD D6 73                 ..s
	bcs     L73FE                           ; 73F8 B0 04                    ..
	jmp     L7467                           ; 73FA 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L73FD:  php                                     ; 73FD 08                       .
L73FE:  sec                                     ; 73FE 38                       8
	lda     L4673                           ; 73FF AD 73 46                 .sF
	sbc     L73D6                           ; 7402 ED D6 73                 ..s
	sta     $AE                             ; 7405 85 AE                    ..
	ldx     $AE                             ; 7407 A6 AE                    ..
	lda     L4659,x                         ; 7409 BD 59 46                 .YF
	sta     L73D5                           ; 740C 8D D5 73                 ..s
	lda     L73D5                           ; 740F AD D5 73                 ..s
	jsr     sub_7035
	lda     $A1                             ; 7415 A5 A1                    ..
	sta     L73D9                           ; 7417 8D D9 73                 ..s
	lda     $A0                             ; 741A A5 A0                    ..
	sta     L73D8                           ; 741C 8D D8 73                 ..s
	clc                                     ; 741F 18                       .
	lda     L73D8                           ; 7420 AD D8 73                 ..s
	adc     #$0C                            ; 7423 69 0C                    i.
	sta     $AE                             ; 7425 85 AE                    ..
	lda     L73D9                           ; 7427 AD D9 73                 ..s
	adc     #$00                            ; 742A 69 00                    i.
	sta     $AF                             ; 742C 85 AF                    ..
	ldy     #$00                            ; 742E A0 00                    ..
	lda     ($AE),y                         ; 7430 B1 AE                    ..
	cmp     #$FF                            ; 7432 C9 FF                    ..
	bcc     L7439                           ; 7434 90 03                    ..
	jmp     L7461                           ; 7436 4C 61 74                 Lat

; ----------------------------------------------------------------------------
L7439:  lda     L73D8                           ; 7439 AD D8 73                 ..s
	ora     L73D9                           ; 743C 0D D9 73                 ..s
	bne     L7444                           ; 743F D0 03                    ..
	jmp     L7461                           ; 7441 4C 61 74                 Lat

; ----------------------------------------------------------------------------
L7444:  ldy     L73D5                           ; 7444 AC D5 73                 ..s
	ldx     L73D4                           ; 7447 AE D4 73                 ..s
	.byte   $AD                             ; 744A AD                       .
	.byte   $D3                             ; 744B D3                       .
L744C:  .byte   $73                             ; 744C 73                       s
	jsr     L7368                           ; 744D 20 68 73                  hs
	lda     $A0                             ; 7450 A5 A0                    ..
	eor     #$01                            ; 7452 49 01                    I.
	beq     L7459                           ; 7454 F0 03                    ..
	jmp     L7461                           ; 7456 4C 61 74                 Lat

; ----------------------------------------------------------------------------
L7459:  ldy     #$01                            ; 7459 A0 01                    ..
	sty     L73D7                           ; 745B 8C D7 73                 ..s
	jmp     L7467                           ; 745E 4C 67 74                 Lgt

; ----------------------------------------------------------------------------
L7461:  inc     L73D6                           ; 7461 EE D6 73                 ..s
	jmp     L73F2                           ; 7464 4C F2 73                 L.s

; ----------------------------------------------------------------------------
L7467:  lda     L73D7                           ; 7467 AD D7 73                 ..s
	eor     #$01                            ; 746A 49 01                    I.
	beq     L7471                           ; 746C F0 03                    ..
	jmp     L7477                           ; 746E 4C 77 74                 Lwt

; ----------------------------------------------------------------------------
L7471:  lda     L73D5                           ; 7471 AD D5 73                 ..s
	sta     $A0                             ; 7474 85 A0                    ..
	rts                                     ; 7476 60                       `

; ----------------------------------------------------------------------------
L7477:  lda     #$FF                            ; 7477 A9 FF                    ..
	sta     $A0                             ; 7479 85 A0                    ..
	rts                                     ; 747B 60                       `

; ----------------------------------------------------------------------------
L747C:  .byte   $25                             ; 747C 25                       %
L747D:  jmp     $7480                           ; 747D 4C 80 74                 L.t

; ----------------------------------------------------------------------------
	lda     #$14                            ; 7480 A9 14                    ..
	asl     a                               ; 7482 0A                       .
	sta     $A2                             ; 7483 85 A2                    ..
	lda     #$00                            ; 7485 A9 00                    ..
	sta     $A3                             ; 7487 85 A3                    ..
	ldy     $A2                             ; 7489 A4 A2                    ..
	ldx     L46A3                           ; 748B AE A3 46                 ..F
	lda     L46A2                           ; 748E AD A2 46                 ..F
	jsr     L45F6                           ; 7491 20 F6 45                  .E
	ldy     #$00                            ; 7494 A0 00                    ..
	sty     L747C                           ; 7496 8C 7C 74                 .|t
L7499:  lda     #$13                            ; 7499 A9 13                    ..
	cmp     L747C                           ; 749B CD 7C 74                 .|t
	bcs     L74A3                           ; 749E B0 03                    ..
	jmp     L74B2                           ; 74A0 4C B2 74                 L.t

; ----------------------------------------------------------------------------
L74A3:  lda     L747C                           ; 74A3 AD 7C 74                 .|t
	ldx     L747C                           ; 74A6 AE 7C 74                 .|t
	sta     L4659,x                         ; 74A9 9D 59 46                 .YF
	inc     L747C                           ; 74AC EE 7C 74                 .|t
	jmp     L7499                           ; 74AF 4C 99 74                 L.t

; ----------------------------------------------------------------------------
L74B2:  ldy     #$00                            ; 74B2 A0 00                    ..
	sty     L4673                           ; 74B4 8C 73 46                 .sF
	rts                                     ; 74B7 60                       `

; ----------------------------------------------------------------------------
L74B8:  .byte   $32                             ; 74B8 32                       2
L74B9:  .byte   $F0                             ; 74B9 F0                       .
L74BA:  .byte   $91                             ; 74BA 91                       .
L74BB:  .byte   $47                             ; 74BB 47                       G
L74BC:  clc                                     ; 74BC 18                       .
L74BD:  .byte	$A5

L74BE:	
	stack_prolog L74B8, $03
	lda     L74B8                           ; 74C7 AD B8 74                 ..t
	jsr     sub_7035
	rdmv	L74BC, $A0
	lda     L74BC                           ; 74D7 AD BC 74                 ..t
	ora     L74BD                           ; 74DA 0D BD 74                 ..t
	lbne	L753B
	ldx     #$00                            ; 74E2 A2 00                    ..
	lda     #$26                            ; 74E4 A9 26                    .&
	jsr     sub_606E
	rdmv	L74BC, $A0
	lda     L74B8                           ; 74F3 AD B8 74                 ..t
	asl     a                               ; 74F6 0A                       .
	php                                     ; 74F7 08                       .
	clc                                     ; 74F8 18                       .
	adc     L46A2                           ; 74F9 6D A2 46                 m.F
	sta     $AE                             ; 74FC 85 AE                    ..
	lda     #$00                            ; 74FE A9 00                    ..
	rol     a                               ; 7500 2A                       *
	plp                                     ; 7501 28                       (
	adc     L46A3                           ; 7502 6D A3 46                 m.F
	sta     $AF                             ; 7505 85 AF                    ..
	lda     L74BD                           ; 7507 AD BD 74                 ..t
	ldy     #$01                            ; 750A A0 01                    ..
	sta     ($AE),y                         ; 750C 91 AE                    ..
	lda     L74BC                           ; 750E AD BC 74                 ..t
	dey                                     ; 7511 88                       .
	sta     ($AE),y                         ; 7512 91 AE                    ..
	ldi	$A3, $00
	ldy     #$26                            ; 7518 A0 26                    .&
	ldxa	L74BC
	jsr     L45F6                           ; 7520 20 F6 45                  .E
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
	ldi	$A5, $00
	ldi	$A4, $08
	ldy     $A2                             ; 7577 A4 A2                    ..
	ldx     $A1                             ; 7579 A6 A1                    ..
	lda     $A0                             ; 757B A5 A0                    ..
	jsr     sub_461F
	ldy     #$01                            ; 7580 A0 01                    ..
	sty     L4656                           ; 7582 8C 56 46                 .VF
	rts                                     ; 7585 60                       `

; ----------------------------------------------------------------------------
L7586:  .byte   $A3                             ; 7586 A3                       .
L7587:  .byte   $F0                             ; 7587 F0                       .
L7588:  .byte   $BD                             ; 7588 BD                       .
L7589:  .byte   $A6                             ; 7589 A6                       .
L758A:  .byte   $F0                             ; 758A F0                       .
L758B:  .byte   $9D                             ; 758B 9D                       .

sub_758C:  
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
L75F9:  lda     L7586                           ; 75F9 AD 86 75                 ..u
	jsr     L71B5                           ; 75FC 20 B5 71                  .q
	ldy     #$01                            ; 75FF A0 01                    ..
	sty     L4656                           ; 7601 8C 56 46                 .VF
	rts                                     ; 7604 60                       `

; ----------------------------------------------------------------------------
L7605:  .byte	$60
L7606:  .byte	$E8
L7607:  .byte   $FA                             ; 7607 FA                       .
L7608:  .byte   $E9                             ; 7608 E9                       .
L7609:  .byte   $53                             ; 7609 53                       S

sub_760A:	
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
L765E:  lda     L7605                           ; 765E AD 05 76                 ..v
	jsr     L72B1                           ; 7661 20 B1 72                  .r
	ldy     #$01                            ; 7664 A0 01                    ..
	sty     L4656                           ; 7666 8C 56 46                 .VF
	rts                                     ; 7669 60                       `

; ----------------------------------------------------------------------------
L766A:  .byte   $EB                             ; 766A EB                       .
L766B:  .byte   $20                             ; 766B 20                        
L766C:  .byte   $53                             ; 766C 53                       S
L766D:  .byte   $EC                             ; 766D EC                       .
L766E:  .byte   $90                             ; 766E 90                       .
L766F:  clv                                     ; 766F B8                       .
L7670:  .byte   $4C                             ; 7670 4C                       L
L7671:  .byte   $11                             ; 7671 11                       .
L7672:  ;beq     $7694                           ; 7672 F0 20                    . 
	.byte	$F0,$20
	cpy     #$EB                            ; 7674 C0 EB                    ..
	.byte	$20
L7677:	.byte	$43,$EC
	bcs     L76BA                           ; 7679 B0 3F                    .?
L767B:  .byte   $20                             ; 767B 20                        
L767C:  .byte   $75                             ; 767C 75                       u
L767D:  .byte	$E9
L767E:	.byte	$20
	.byte   $C2                             ; 767F C2                       .
L7680:  .byte   $EF                             ; 7680 EF                       .
L7681:  .byte   $D0                             ; 7681 D0                       .
L7682:  .byte   $03                             ; 7682 03                       .
	jsr     LEF40                           ; 7683 20 40 EF                  @.
L7686:  .byte   $20                             ; 7686 20                        
L7687:  .byte   $93                             ; 7687 93                       .
L7688:  .byte   $ED                             ; 7688 ED                       .
	.byte   $20                             ; 7689 20                        

sub_768A:  
	stack_prolog L766A, $02
	lda     L766A                           ; 7693 AD 6A 76                 .jv
	jsr     sub_7035
	rdmv	L766D, $A0
	lda     L766E                           ; 76A3 AD 6E 76                 .nv
	sta     $A3                             ; 76A6 85 A3                    ..
	ldi	$A5, $00
	ldi	$A4, $0C
	ldy     L766D                           ; 76B0 AC 6D 76                 .mv
	ldx     #>L767E                         ; 76B3 A2 76                    .v
	lda     #<L767E                         ; 76B5 A9 7E                    .~
	jsr     sub_461F
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
	lda     L767C                           ; 76F1 AD 7C 76                 .|v
	sta     $A3                             ; 76F4 85 A3                    ..
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
	jsr     L4C1D                           ; 770D 20 1D 4C                  .L
	add16i	$A2, L766D, $001A
	lda     #$76                            ; 771F A9 76                    .v
	sta     $A5                             ; 7721 85 A5                    ..
	lda     #$73                            ; 7723 A9 73                    .s
	sta     $A4                             ; 7725 85 A4                    ..
	ldy     $A2                             ; 7727 A4 A2                    ..
	ldx     #$76                            ; 7729 A2 76                    .v
	lda     #$77                            ; 772B A9 77                    .w
	jsr     L4CF5                           ; 772D 20 F5 4C                  .L
	lda     $A0                             ; 7730 A5 A0                    ..
	sta     L767D                           ; 7732 8D 7D 76                 .}v
	lda     L767D                           ; 7735 AD 7D 76                 .}v
	eor     #$01                            ; 7738 49 01                    I.
	lbne	L7799
	add16i	$A0, L766D, $001A
	ldx     $A1                             ; 774E A6 A1                    ..
	lda     $A0                             ; 7750 A5 A0                    ..
	jsr     L4E4A                           ; 7752 20 4A 4E                  JN
	lda     $A1                             ; 7755 A5 A1                    ..
	sta     L7670                           ; 7757 8D 70 76                 .pv
	lda     $A0                             ; 775A A5 A0                    ..
	sta     L766F                           ; 775C 8D 6F 76                 .ov
	ldx     #$76                            ; 775F A2 76                    .v
	lda     #$73                            ; 7761 A9 73                    .s
	jsr     L4E4A                           ; 7763 20 4A 4E                  JN
	rdmv	L7671, $A0
	lda     L7671                           ; 7770 AD 71 76                 .qv
	eor     L766F                           ; 7773 4D 6F 76                 Mov
	bne     L777E                           ; 7776 D0 06                    ..
	ora     L7672                           ; 7778 0D 72 76                 .rv
	eor     L7670                           ; 777B 4D 70 76                 Mpv
L777E:	lbne	L7799
	lda     L766C                           ; 7783 AD 6C 76                 .lv
	sta     $A3                             ; 7786 85 A3                    ..
	ldy     L766B                           ; 7788 AC 6B 76                 .kv
	ldx     L7682                           ; 778B AE 82 76                 ..v
	lda     L766A                           ; 778E AD 6A 76                 .jv
	jsr     sub_758C
	ldi	$A0, $01
	rts                                     ; 7798 60                       `

; ----------------------------------------------------------------------------
L7799:	ldi	$A0, $00
	rts                                     ; 779D 60                       `

; ----------------------------------------------------------------------------
L779E:  .byte   $02                             ; 779E 02                       .
L779F:  .byte   $91                             ; 779F 91                       .
L77A0:  .byte   $45                             ; 77A0 45                       E
L77A1:  iny                                     ; 77A1 C8                       .
L77A2:  .byte   $A9                             ; 77A2 A9                       .

sub_77A3:  
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
	lda     L77A0                           ; 77F9 AD A0 77                 ..w
	sta     $84                             ; 77FC 85 84                    ..
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
	lda     L779E                           ; 7812 AD 9E 77                 ..w
	jsr     L71B5                           ; 7815 20 B5 71                  .q
	lda     L779E                           ; 7818 AD 9E 77                 ..w
	jsr     L72B1                           ; 781B 20 B1 72                  .r
	ldy     #$01                            ; 781E A0 01                    ..
	sty     L4656                           ; 7820 8C 56 46                 .VF
	rts                                     ; 7823 60                       `

; ----------------------------------------------------------------------------
L7824:  .byte   $20                             ; 7824 20                        
L7825:  .byte   $43                             ; 7825 43                       C
L7826:  .byte   $EB                             ; 7826 EB                       .
L7827:  .byte   $4C                             ; 7827 4C                       L
L7828:  .byte   $17                             ; 7828 17                       .
L7829:  .byte   $F0                             ; 7829 F0                       .

sub_782A:  
	prolog
	stx     L7825                           ; 782D 8E 25 78                 .%x
	sta     L7824                           ; 7830 8D 24 78                 .$x
	lda     L7824                           ; 7833 AD 24 78                 .$x
	jsr     sub_7035
	rdmv	L7828, $A0
	lda     L7828                           ; 7843 AD 28 78                 .(x
	ora     L7829                           ; 7846 0D 29 78                 .)x
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
	clc                                     ; 7893 18                       .
	lda     L7826                           ; 7894 AD 26 78                 .&x
	adc     #$01                            ; 7897 69 01                    i.
	sta     $AE                             ; 7899 85 AE                    ..
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
	lda     $A0                             ; 78C3 A5 A0                    ..
	jsr     sub_7035
	lda     $A1                             ; 78C8 A5 A1                    ..
	sta     L7829                           ; 78CA 8D 29 78                 .)x
	lda     $A0                             ; 78CD A5 A0                    ..
	sta     L7828                           ; 78CF 8D 28 78                 .(x
	lda     L7828                           ; 78D2 AD 28 78                 .(x
	ora     L7829                           ; 78D5 0D 29 78                 .)x
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
L78FE:  ldy     #$01                            ; 78FE A0 01                    ..
	sty     L7827                           ; 7900 8C 27 78                 .'x
	sec                                     ; 7903 38                       8
	lda     #$13                            ; 7904 A9 13                    ..
	sbc     L7826                           ; 7906 ED 26 78                 .&x
	sta     L7917                           ; 7909 8D 17 79                 ..y
L790C:  lda     L7917                           ; 790C AD 17 79                 ..y
	cmp     L7827                           ; 790F CD 27 78                 .'x
	bcs     L7918                           ; 7912 B0 04                    ..
	jmp     L793F                           ; 7914 4C 3F 79                 L?y

; ----------------------------------------------------------------------------
L7917:  .byte   $2E                             ; 7917 2E                       .

; ----------------------------------------------------------------------------
L7918:  sec                                     ; 7918 38                       8
	lda     #$14                            ; 7919 A9 14                    ..
	sbc     L7827                           ; 791B ED 27 78                 .'x
	sta     $AE                             ; 791E 85 AE                    ..
	sec                                     ; 7920 38                       8
	lda     #$14                            ; 7921 A9 14                    ..
	sbc     L7827                           ; 7923 ED 27 78                 .'x
	sta     $AC                             ; 7926 85 AC                    ..
	sec                                     ; 7928 38                       8
	lda     $AC                             ; 7929 A5 AC                    ..
	sbc     #$01                            ; 792B E9 01                    ..
	sta     $AA                             ; 792D 85 AA                    ..
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
	ldy     #$01                            ; 7948 A0 01                    ..
	sty     L4656                           ; 794A 8C 56 46                 .VF
	rts                                     ; 794D 60                       `

; ----------------------------------------------------------------------------
L794E:  .byte   $0D                             ; 794E 0D                       .
L794F:  .byte   $F0                             ; 794F F0                       .
L7950:  .byte	$05

sub_7951:	
	prolog
	sta     L794E                           ; 7954 8D 4E 79                 .Ny
	lda     L794E                           ; 7957 AD 4E 79                 .Ny
	jsr     sub_7035
	rdmv	L794F, $A0
	lda     L794F                           ; 7967 AD 4F 79                 .Oy
	ora     L7950                           ; 796A 0D 50 79                 .Py
	lbne	L7973
L7972:  rts                                     ; 7972 60                       `

; ----------------------------------------------------------------------------
L7973:  ldx     #$FF                            ; 7973 A2 FF                    ..
	lda     L794E                           ; 7975 AD 4E 79                 .Ny
	jsr     sub_782A
	ldi	$A3, $00
	ldy     #$26                            ; 797F A0 26                    .&
	ldx     L7950                           ; 7981 AE 50 79                 .Py
	lda     L794F                           ; 7984 AD 4F 79                 .Oy
	jsr     L619A                           ; 7987 20 9A 61                  .a
	sec                                     ; 798A 38                       8
	lda     L4673                           ; 798B AD 73 46                 .sF
	sbc     #$01                            ; 798E E9 01                    ..
	sta     L4673                           ; 7990 8D 73 46                 .sF
	rts                                     ; 7993 60                       `

; ----------------------------------------------------------------------------
L7994:  .byte	$08
L7995:  .byte   $F0                             ; 7995 F0                       .
L7996:  .byte   $1D                             ; 7996 1D                       .
L7997:  .byte   $30                             ; 7997 30                       0
L7998:  .byte   $1B                             ; 7998 1B                       .
L7999:  .byte   $29                             ; 7999 29                       )
L799A:  .byte   $01                             ; 799A 01                       .

sub_799B:  
	prolog
	stx     L7995                           ; 799E 8E 95 79                 ..y
	sta     L7994                           ; 79A1 8D 94 79                 ..y
	lda     L7994                           ; 79A4 AD 94 79                 ..y
	jsr     L65B0                           ; 79A7 20 B0 65                  .e
	rdmv	L7996, $A0
L79B4:  add16i	off_AE, L7996, $0007
	ldy     #$01                            ; 79C3 A0 01                    ..
	lda     (off_AE),y
	sta     L7999                           ; 79C7 8D 99 79                 ..y
	dey                                     ; 79CA 88                       .
	lda     (off_AE),y
	sta     L7998                           ; 79CD 8D 98 79                 ..y
	lda     L7998                           ; 79D0 AD 98 79                 ..y
	ora     L7999                           ; 79D3 0D 99 79                 ..y
	lbne	L79E4
	ldi	$A1, $00
	ldi	$A0, $00
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
L7A0B:  .byte   $03                             ; 7A0B 03                       .

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
L7A49:	ldi	$A1, $00
	ldi	$A0, $00
	rts                                     ; 7A51 60                       `

; ----------------------------------------------------------------------------
L7A52:  .byte   $F0                             ; 7A52 F0                       .
L7A53:  .byte   $F0                             ; 7A53 F0                       .
L7A54:  .byte   $03                             ; 7A54 03                       .
L7A55:  .byte   $4C                             ; 7A55 4C                       L
L7A56:	.byte	$B6

sub_7A57:	
	stack_prolog L7A52, $02
	ldx     L7A53                           ; 7A60 AE 53 7A                 .Sz
	lda     L7A52                           ; 7A63 AD 52 7A                 .Rz
	jsr     sub_799B
	rdmv	L7A55, $A0
	lda     L7A55                           ; 7A73 AD 55 7A                 .Uz
	ora     L7A56                           ; 7A76 0D 56 7A                 .Vz
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
L7A95:  pla                                     ; 7A95 68                       h
L7A96:  .byte   $9D                             ; 7A96 9D                       .
L7A97:  .byte   $9E                             ; 7A97 9E                       .
L7A98:  .byte   $F0                             ; 7A98 F0                       .
L7A99:  .byte	$A9

sub_7A9A:	
	stack_prolog L7A95, $02
	ldx     L7A96                           ; 7AA3 AE 96 7A                 ..z
	lda     L7A95                           ; 7AA6 AD 95 7A                 ..z
	jsr     sub_799B
	rdmv	L7A98, $A0
	lda     L7A98                           ; 7AB6 AD 98 7A                 ..z
	ora     L7A99                           ; 7AB9 0D 99 7A                 ..z
	lbne	L7AC2
	rts                                     ; 7AC1 60                       `

; ----------------------------------------------------------------------------
L7AC2:	add16i	off_AE, L7A98, $0005
	lda     L7A97                           ; 7AD1 AD 97 7A                 ..z
	ldy     #$00                            ; 7AD4 A0 00                    ..
	sta     (off_AE),y                      ; 7AD6 91 AE                    ..
	rts                                     ; 7AD8 60                       `

; ----------------------------------------------------------------------------
L7AD9:  brk                                     ; 7AD9 00                       .
L7ADA:  .byte   $91                             ; 7ADA 91                       .
L7ADB:  .byte   $45                             ; 7ADB 45                       E
L7ADC:  sec                                     ; 7ADC 38                       8
L7ADD:  php                                     ; 7ADD 08                       .
L7ADE:  .byte   $A5                             ; 7ADE A5                       .

sub_7ADF:  
	stack_prolog L7AD9, $03
	ldx     L7ADA                           ; 7AE8 AE DA 7A                 ..z
	lda     L7AD9                           ; 7AEB AD D9 7A                 ..z
	jsr     sub_799B
	lda     $A1                             ; 7AF1 A5 A1                    ..
	sta     L7ADE                           ; 7AF3 8D DE 7A                 ..z
	lda     $A0                             ; 7AF6 A5 A0                    ..
	sta     L7ADD                           ; 7AF8 8D DD 7A                 ..z
	lda     L7ADD                           ; 7AFB AD DD 7A                 ..z
	ora     L7ADE                           ; 7AFE 0D DE 7A                 ..z
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
L7B49:  sty     L4656                           ; 7B49 8C 56 46                 .VF
	rts                                     ; 7B4C 60                       `

; ----------------------------------------------------------------------------
L7B4D:  .byte   $54                             ; 7B4D 54                       T
L7B4E:  .byte   $B1                             ; 7B4E B1                       .
L7B4F:  .byte   $45                             ; 7B4F 45                       E
L7B50:  .byte   $F0                             ; 7B50 F0                       .
L7B51:  .byte   $F7                             ; 7B51 F7                       .
L7B52:  .byte   $8C                             ; 7B52 8C                       .
L7B53:  .byte   $33                             ; 7B53 33                       3
L7B54:  .byte   $F0                             ; 7B54 F0                       .
L7B55:  pha                                     ; 7B55 48                       H
L7B56:  sec                                     ; 7B56 38                       8
L7B57:  ldy     #$03                            ; 7B57 A0 03                    ..
	lda     ($45),y                         ; 7B59 B1 45                    .E
	.byte   $E9                             ; 7B5B E9                       .
L7B5C:  ora     ($91,x)                         ; 7B5C 01 91                    ..
	eor     $C8                             ; 7B5E 45 C8                    E.
	lda     ($45),y                         ; 7B60 B1 45                    .E
	sbc     #$00                            ; 7B62 E9 00                    ..
L7B64:  sta     ($45),y                         ; 7B64 91 45                    .E
	iny                                     ; 7B66 C8                       .
L7B67:  .byte	$A9

sub_7B68:	
	stack_prolog L7B4D, $02
	sec                                     ; 7B71 38                       8
	lda     L7B4F                           ; 7B72 AD 4F 7B                 .O{
	sbc     #$01                            ; 7B75 E9 01                    ..
	sta     L7B4F                           ; 7B77 8D 4F 7B                 .O{
	lda     L7B4D                           ; 7B7A AD 4D 7B                 .M{
	jsr     L65B0                           ; 7B7D 20 B0 65                  .e
	lda     $A1                             ; 7B80 A5 A1                    ..
	sta     L7B51                           ; 7B82 8D 51 7B                 .Q{
	lda     $A0                             ; 7B85 A5 A0                    ..
	sta     L7B50                           ; 7B87 8D 50 7B                 .P{
	add16i	off_AE, L7B50, $0007
	ldy     #$01                            ; 7B99 A0 01                    ..
	lda     ($AE),y                         ; 7B9B B1 AE                    ..
	sta     L7B53                           ; 7B9D 8D 53 7B                 .S{
	dey                                     ; 7BA0 88                       .
	lda     ($AE),y                         ; 7BA1 B1 AE                    ..
	sta     L7B52                           ; 7BA3 8D 52 7B                 .R{
	lda     L7B52                           ; 7BA6 AD 52 7B                 .R{
	ora     L7B53                           ; 7BA9 0D 53 7B                 .S{
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
L7BEB:  .byte   $F6                             ; 7BEB F6                       .

; ----------------------------------------------------------------------------
L7BEC:  lda     L7B53                           ; 7BEC AD 53 7B                 .S{
	sta     $A3                             ; 7BEF 85 A3                    ..
	lda     #$00                            ; 7BF1 A9 00                    ..
	sta     $A5                             ; 7BF3 85 A5                    ..
	lda     #$05                            ; 7BF5 A9 05                    ..
	sta     $A4                             ; 7BF7 85 A4                    ..
	ldy     L7B52                           ; 7BF9 AC 52 7B                 .R{
	ldx     #$7B                            ; 7BFC A2 7B                    .{
	lda     #$63                            ; 7BFE A9 63                    .c
	jsr     sub_461F
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
L7C36:  lda     L7B53                           ; 7C36 AD 53 7B                 .S{
	sta     L7B55                           ; 7C39 8D 55 7B                 .U{
	lda     L7B52                           ; 7C3C AD 52 7B                 .R{
	sta     L7B54                           ; 7C3F 8D 54 7B                 .T{
L7C42:  clc                                     ; 7C42 18                       .
	lda     L7B52                           ; 7C43 AD 52 7B                 .R{
	adc     #$06                            ; 7C46 69 06                    i.
	sta     L7B52                           ; 7C48 8D 52 7B                 .R{
	lda     L7B53                           ; 7C4B AD 53 7B                 .S{
	adc     #$00                            ; 7C4E 69 00                    i.
	sta     L7B53                           ; 7C50 8D 53 7B                 .S{
	inc     L7B5C                           ; 7C53 EE 5C 7B                 .\{
	jmp     L7BE0                           ; 7C56 4C E0 7B                 L.{

; ----------------------------------------------------------------------------
L7C59:  lda     L7B54                           ; 7C59 AD 54 7B                 .T{
	ora     L7B55                           ; 7C5C 0D 55 7B                 .U{
	.byte   $F0                             ; 7C5F F0                       .
L7C60:  .byte   $03                             ; 7C60 03                       .
	jmp     L7C65                           ; 7C61 4C 65 7C                 Le|

; ----------------------------------------------------------------------------
	rts                                     ; 7C64 60                       `

; ----------------------------------------------------------------------------
L7C65:  lda     L7B55                           ; 7C65 AD 55 7B                 .U{
	sta     $A3                             ; 7C68 85 A3                    ..
	lda     #$00                            ; 7C6A A9 00                    ..
	sta     $A5                             ; 7C6C 85 A5                    ..
	lda     #$06                            ; 7C6E A9 06                    ..
	sta     $A4                             ; 7C70 85 A4                    ..
	ldy     L7B54                           ; 7C72 AC 54 7B                 .T{
	ldx     #$7B                            ; 7C75 A2 7B                    .{
	lda     #$5D                            ; 7C77 A9 5D                    .]
	jsr     sub_461F
	clc                                     ; 7C7C 18                       .
	lda     L7B54                           ; 7C7D AD 54 7B                 .T{
	adc     #$06                            ; 7C80 69 06                    i.
	sta     $A2                             ; 7C82 85 A2                    ..
	lda     L7B55                           ; 7C84 AD 55 7B                 .U{
	adc     #$00                            ; 7C87 69 00                    i.
	sta     $A3                             ; 7C89 85 A3                    ..
	sec                                     ; 7C8B 38                       8
	lda     L7B52                           ; 7C8C AD 52 7B                 .R{
	sbc     L7B54                           ; 7C8F ED 54 7B                 .T{
	sta     $AC                             ; 7C92 85 AC                    ..
	lda     L7B53                           ; 7C94 AD 53 7B                 .S{
	sbc     L7B55                           ; 7C97 ED 55 7B                 .U{
	sta     $AD                             ; 7C9A 85 AD                    ..
	sec                                     ; 7C9C 38                       8
	lda     $AC                             ; 7C9D A5 AC                    ..
	sbc     #$06                            ; 7C9F E9 06                    ..
	sta     $A4                             ; 7CA1 85 A4                    ..
	lda     $AD                             ; 7CA3 A5 AD                    ..
	sbc     #$00                            ; 7CA5 E9 00                    ..
	sta     $A5                             ; 7CA7 85 A5                    ..
	ldy     $A2                             ; 7CA9 A4 A2                    ..
	ldx     L7B55                           ; 7CAB AE 55 7B                 .U{
	lda     L7B54                           ; 7CAE AD 54 7B                 .T{
	jsr     sub_461F
	lda     L7B54                           ; 7CB4 AD 54 7B                 .T{
	cmp     L7B56                           ; 7CB7 CD 56 7B                 .V{
	lda     L7B55                           ; 7CBA AD 55 7B                 .U{
	sbc     L7B57                           ; 7CBD ED 57 7B                 .W{
	lbcs	L7CD6
	sec                                     ; 7CC5 38                       8
	lda     L7B56                           ; 7CC6 AD 56 7B                 .V{
	sbc     #$06                            ; 7CC9 E9 06                    ..
	sta     L7B56                           ; 7CCB 8D 56 7B                 .V{
	lda     L7B57                           ; 7CCE AD 57 7B                 .W{
	sbc     #$00                            ; 7CD1 E9 00                    ..
	sta     L7B57                           ; 7CD3 8D 57 7B                 .W{
L7CD6:	add16i	$A0, L7B56, $0006
	mv	$A3, L7B57
	sub16m	off_AC, L7B52, L7B56
	sub16i	$A4, off_AC, $0006
	ldy     L7B56                           ; 7D08 AC 56 7B                 .V{
	ldxa	$A0
	jsr     L4EB1                           ; 7D0F 20 B1 4E                  .N
	lda     #$7B                            ; 7D12 A9 7B                    .{
	sta     $A3                             ; 7D14 85 A3                    ..
	lda     #$00                            ; 7D16 A9 00                    ..
	sta     $A5                             ; 7D18 85 A5                    ..
	lda     #$06                            ; 7D1A A9 06                    ..
	sta     $A4                             ; 7D1C 85 A4                    ..
	ldy     #$5D                            ; 7D1E A0 5D                    .]
	ldxa	L7B56
	jsr     sub_461F
	add16i	off_AE, L7B56, $0004
	lda     L7B4F                           ; 7D38 AD 4F 7B                 .O{
	ldy     #$00                            ; 7D3B A0 00                    ..
	sta     ($AE),y                         ; 7D3D 91 AE                    ..
	iny                                     ; 7D3F C8                       .
	sty     L4656                           ; 7D40 8C 56 46                 .VF
	rts                                     ; 7D43 60                       `

; ----------------------------------------------------------------------------
L7D44:	brk                                     ; 7D44 00                       .
L7D45:  brk                                     ; 7D45 00                       .
L7D46:  brk                                     ; 7D46 00                       .
L7D47:  brk                                     ; 7D47 00                       .
L7D48:  brk                                     ; 7D48 00                       .
L7D49:  brk                                     ; 7D49 00                       .
L7D4A:  brk                                     ; 7D4A 00                       .
L7D4B:  brk                                     ; 7D4B 00                       .
L7D4C:  brk                                     ; 7D4C 00                       .

L7D4D:	prolog
	jsr     sub_44D5                        ; 7D50 20 D5 44                  .D
	.addr	L7D44
	.byte	$03
	lda	L7D44
	jsr	L65B0
	rdmv	L7D48, $A0
	lda     L7D45                           ; 7D66 AD 45 7D                 .E}
	and     #$01                            ; 7D69 29 01                    ).
	sta     L7D4A                           ; 7D6B 8D 4A 7D                 .J}
	lsr     L7D45                           ; 7D6E 4E 45 7D                 NE}
	lda     L7D48                           ; 7D71 AD 48 7D                 .H}
	ora     L7D49                           ; 7D74 0D 49 7D                 .I}
	lbne	L7D7D
	rts                                     ; 7D7C 60                       `

; ----------------------------------------------------------------------------
L7D7D:	add16i	off_AE, L7D48, $0009
	ldy     #$01                            ; 7D8C A0 01                    ..
L7D8E:  lda     ($AE),y                         ; 7D8E B1 AE                    ..
	sta     L7D4C                           ; 7D90 8D 4C 7D                 .L}
	dey                                     ; 7D93 88                       .
	lda     ($AE),y                         ; 7D94 B1 AE                    ..
	sta     L7D4B                           ; 7D96 8D 4B 7D                 .K}
	lda     L7D4B                           ; 7D99 AD 4B 7D                 .K}
	ora     L7D4C                           ; 7D9C 0D 4C 7D                 .L}
	lbne	L7DA5
	rts                                     ; 7DA4 60                       `

; ----------------------------------------------------------------------------
L7DA5:	add16i	off_AE, L7D4B, $0002
	lda     #$00                            ; 7DB4 A9 00                    ..
	sta     $85                             ; 7DB6 85 85                    ..
	lda     #$03                            ; 7DB8 A9 03                    ..
	sta     $84                             ; 7DBA 85 84                    ..
	lda     L7D45                           ; 7DBC AD 45 7D                 .E}
	ldx     #$00                            ; 7DBF A2 00                    ..
	jsr     sub_444A
	sta     $AC                             ; 7DC4 85 AC                    ..
	txa                                     ; 7DC6 8A                       .
	sta     $AD                             ; 7DC7 85 AD                    ..
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
L7E12:  brk                                     ; 7E12 00                       .
L7E13:	brk                                     ; 7E13 00                       .
L7E14:  brk                                     ; 7E14 00                       .
L7E15:  brk                                     ; 7E15 00                       .
L7E16:  brk                                     ; 7E16 00                       .
L7E17:  brk                                     ; 7E17 00                       .
L7E18:  brk                                     ; 7E18 00                       .
L7E19:  brk                                     ; 7E19 00                       .
L7E1A:  brk                                     ; 7E1A 00                       .
L7E1B:  brk                                     ; 7E1B 00                       .
L7E1C:  brk                                     ; 7E1C 00                       .
L7E1D:  brk                                     ; 7E1D 00                       .
L7E1E:  brk                                     ; 7E1E 00                       .
L7E1F:  brk                                     ; 7E1F 00                       .
L7E20:  brk                                     ; 7E20 00                       .
L7E21:  brk                                     ; 7E21 00                       .
L7E22:  brk                                     ; 7E22 00                       .
L7E23:  brk                                     ; 7E23 00                       .

L7E24:
	stack_prolog L7E12, $02
	lda	L7E13
	jsr	L65B0
	rdmv	L7E15, $A0
	add16i	off_AE, L7E15, $0009
	ldy     #$01                            ; 7E4C A0 01                    ..
	lda     ($AE),y                         ; 7E4E B1 AE                    ..
	sta     L7E1F                           ; 7E50 8D 1F 7E                 ..~
	dey                                     ; 7E53 88                       .
	lda     ($AE),y                         ; 7E54 B1 AE                    ..
	sta     L7E1E                           ; 7E56 8D 1E 7E                 ..~
	lda     L7E1E                           ; 7E59 AD 1E 7E                 ..~
	ora     L7E1F                           ; 7E5C 0D 1F 7E                 ..~
	lbne	L7E69
L7E64:	ldi	$A0, $00
	rts                                     ; 7E68 60                       `

; ----------------------------------------------------------------------------
L7E69:	dmv	off_AE, L7E1E
	ldy     #$01                            ; 7E73 A0 01                    ..
	lda     ($AE),y                         ; 7E75 B1 AE                    ..
	sta     L7E1D                           ; 7E77 8D 1D 7E                 ..~
	dey                                     ; 7E7A 88                       .
	lda     ($AE),y                         ; 7E7B B1 AE                    ..
	sta     L7E1C                           ; 7E7D 8D 1C 7E                 ..~
	add16i	off_AE, L7E1E, $0002
	lda     #$00                            ; 7E8F A9 00                    ..
	sta     $85                             ; 7E91 85 85                    ..
	lda     #$03                            ; 7E93 A9 03                    ..
	sta     $84                             ; 7E95 85 84                    ..
	lda     L7E12                           ; 7E97 AD 12 7E                 ..~
	ldx     #$00                            ; 7E9A A2 00                    ..
	jsr     sub_444A
	sta     $AC                             ; 7E9F 85 AC                    ..
	txa                                     ; 7EA1 8A                       .
	sta     $AD                             ; 7EA2 85 AD                    ..
	add16m	L7E1A, $AE, $AC
	lda     L7E1B                           ; 7EB3 AD 1B 7E                 ..~
	sta     $A3                             ; 7EB6 85 A3                    ..
	lda     #$00                            ; 7EB8 A9 00                    ..
	sta     $A5                             ; 7EBA 85 A5                    ..
	lda     #$03                            ; 7EBC A9 03                    ..
	sta     $A4                             ; 7EBE 85 A4                    ..
	ldy     L7E1A                           ; 7EC0 AC 1A 7E                 ..~
	ldxai	L7E21
	jsr     sub_461F
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
L7EFA:  clc                                     ; 7EFA 18                       .
	lda     L7E1C                           ; 7EFB AD 1C 7E                 ..~
	adc     L7E23                           ; 7EFE 6D 23 7E                 m#~
	sta     $AE                             ; 7F01 85 AE                    ..
	lda     L7E1D                           ; 7F03 AD 1D 7E                 ..~
	adc     #$00                            ; 7F06 69 00                    i.
	sta     $AF                             ; 7F08 85 AF                    ..
	clc                                     ; 7F0A 18                       .
	lda     $AE                             ; 7F0B A5 AE                    ..
	adc     #$01                            ; 7F0D 69 01                    i.
	sta     L7E17                           ; 7F0F 8D 17 7E                 ..~
	lda     $AF                             ; 7F12 A5 AF                    ..
	adc     #$00                            ; 7F14 69 00                    i.
	sta     L7E18                           ; 7F16 8D 18 7E                 ..~
	ldy     #$00                            ; 7F19 A0 00                    ..
	sty     L7E19                           ; 7F1B 8C 19 7E                 ..~
	sty     L7E20                           ; 7F1E 8C 20 7E                 . ~
	sec                                     ; 7F21 38                       8
	lda     L7E22                           ; 7F22 AD 22 7E                 ."~
	sbc     #$01                            ; 7F25 E9 01                    ..
	.byte   $8D                             ; 7F27 8D                       .
	.byte   $35                             ; 7F28 35                       5
L7F29:  .byte   $7F                             ; 7F29 7F                       .
L7F2A:  lda     L7F35                           ; 7F2A AD 35 7F                 .5.
	cmp     L7E20                           ; 7F2D CD 20 7E                 . ~
	bcs     L7F36                           ; 7F30 B0 04                    ..
	jmp     L7F5F                           ; 7F32 4C 5F 7F                 L_.

; ----------------------------------------------------------------------------
L7F35:  brk                                     ; 7F35 00                       .
L7F36:  clc                                     ; 7F36 18                       .
	lda     L7E17                           ; 7F37 AD 17 7E                 ..~
	adc     L7E20                           ; 7F3A 6D 20 7E                 m ~
	sta     $AE                             ; 7F3D 85 AE                    ..
	lda     L7E18                           ; 7F3F AD 18 7E                 ..~
	adc     #$00                            ; 7F42 69 00                    i.
	sta     $AF                             ; 7F44 85 AF                    ..
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
L7F74:  brk                                     ; 7F74 00                       .
L7F75:  brk                                     ; 7F75 00                       .
	brk                                     ; 7F76 00                       .
	brk                                     ; 7F77 00                       .
	brk                                     ; 7F78 00                       .
	brk                                     ; 7F79 00                       .
	brk                                     ; 7F7A 00                       .
	brk                                     ; 7F7B 00                       .
	brk                                     ; 7F7C 00                       .
	brk                                     ; 7F7D 00                       .
	brk                                     ; 7F7E 00                       .
	brk                                     ; 7F7F 00                       .

L7F80:	prolog
	stx     L7F75                           ; 7F83 8E 75 7F                 .u.
	sta     L7F74                           ; 7F86 8D 74 7F                 .t.
	lda     #$00                            ; 7F89 A9 00                    ..
	sta     $A0                             ; 7F8B 85 A0                    ..
	rts                                     ; 7F8D 60                       `

; ----------------------------------------------------------------------------
L7F8E:  brk                                     ; 7F8E 00                       .
L7F8F:  brk                                     ; 7F8F 00                       .
L7F90:  brk                                     ; 7F90 00                       .
L7F91:  brk                                     ; 7F91 00                       .
L7F92:  brk                                     ; 7F92 00                       .

sub_7F93:
	prolog
	stx     L7F8F                           ; 7F96 8E 8F 7F                 ...
	sta     L7F8E                           ; 7F99 8D 8E 7F                 ...
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
	jsr     L7E24                           ; 7FBD 20 24 7E                  $~
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
L7FE8:  brk                                     ; 7FE8 00                       .

; ----------------------------------------------------------------------------
L7FE9:	prolog
	sta     L7FE8                           ; 7FEC 8D E8 7F                 ...
	lda     L7FE8                           ; 7FEF AD E8 7F                 ...
	lsr     a                               ; 7FF2 4A                       J
	lsr     a                               ; 7FF3 4A                       J
	lsr     a                               ; 7FF4 4A                       J
	lsr     a                               ; 7FF5 4A                       J
	sta     $A0                             ; 7FF6 85 A0                    ..
	ldx     L7FE8                           ; 7FF8 AE E8 7F                 ...
	lda     $A0                             ; 7FFB A5 A0                    ..
	jsr     L49A2                           ; 7FFD 20 A2 49                  .I
	rts                                     ; 8000 60                       `

; ----------------------------------------------------------------------------
L8001:  brk                                     ; 8001 00                       .
L8002:  brk                                     ; 8002 00                       .

; ----------------------------------------------------------------------------
L8003:  prolog
	stx     L8002                           ; 8006 8E 02 80                 ...
	sta     L8001                           ; 8009 8D 01 80                 ...
	lda     L8001                           ; 800C AD 01 80                 ...
	sta     L474F                           ; 800F 8D 4F 47                 .OG
	lda     L8002                           ; 8012 AD 02 80                 ...
	sta     L4750                           ; 8015 8D 50 47                 .PG
	ldx     #$00                            ; 8018 A2 00                    ..
	lda     #$00                            ; 801A A9 00                    ..
	jsr     L49D3                           ; 801C 20 D3 49                  .I
	rts                                     ; 801F 60                       `

; ----------------------------------------------------------------------------
L8020:  prolog
	lda     #$00                            ; 8023 A9 00                    ..
	sta     $A3                             ; 8025 85 A3                    ..
	ldy     #$04                            ; 8027 A0 04                    ..
	ldx     #$47                            ; 8029 A2 47                    .G
	lda     #$4F                            ; 802B A9 4F                    .O
	jsr     L45F6                           ; 802D 20 F6 45                  .E
	lda     #$00                            ; 8030 A9 00                    ..
	sta     $A3                             ; 8032 85 A3                    ..
	ldy     #$0E                            ; 8034 A0 0E                    ..
	ldx     #$47                            ; 8036 A2 47                    .G
	lda     #$53                            ; 8038 A9 53                    .S
	jsr     L45F6                           ; 803A 20 F6 45                  .E
	ldx     #$00                            ; 803D A2 00                    ..
	lda     #$00                            ; 803F A9 00                    ..
	jsr     L8003                           ; 8041 20 03 80                  ..
	rts                                     ; 8044 60                       `

; ----------------------------------------------------------------------------
L8045:  .byte   $A9                             ; 8045 A9                       .
L8046:  pha                                     ; 8046 48                       H

L8047:  prolog
	stx     L8046                           ; 804A 8E 46 80                 .F.
	sta     L8045                           ; 804D 8D 45 80                 .E.
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
	clc                                     ; 8090 18                       .
	lda     $AE                             ; 8091 A5 AE                    ..
	adc     L4754                           ; 8093 6D 54 47                 mTG
	sta     $AC                             ; 8096 85 AC                    ..
	clc                                     ; 8098 18                       .
	lda     $AC                             ; 8099 A5 AC                    ..
	adc     #$10                            ; 809B 69 10                    i.
	sta     L8046                           ; 809D 8D 46 80                 .F.
	ldx     L8046                           ; 80A0 AE 46 80                 .F.
	lda     L8045                           ; 80A3 AD 45 80                 .E.
	jsr     L49D3                           ; 80A6 20 D3 49                  .I
	rts                                     ; 80A9 60                       `

; ----------------------------------------------------------------------------
L80AA:  .byte   $4F                             ; 80AA 4F                       O
L80AB:  .byte   $4D                             ; 80AB 4D                       M
L80AC:  .byte   $9B                             ; 80AC 9B                       .
L80AD:  .byte   $A2                             ; 80AD A2                       .
L80AE:  brk                                     ; 80AE 00                       .
L80AF:  .byte   $B9                             ; 80AF B9                       .
L80B0:  .byte   $5B                             ; 80B0 5B                       [
L80B1:  .byte   $F3                             ; 80B1 F3                       .
L80B2:  .byte   $9D                             ; 80B2 9D                       .
L80B3:  .byte   $CD                             ; 80B3 CD                       .
L80B4:  .byte   $09                             ; 80B4 09                       .
L80B5:  .byte   $9D                             ; 80B5 9D                       .
L80B6:  clv                                     ; 80B6 B8                       .
	ora     #$C8                            ; 80B7 09 C8                    ..
L80B9:  inx                                     ; 80B9 E8                       .
L80BA:  .byte	$C9

L80BB:	prolog	
	.byte	$8E
	.byte   $AB                             ; 80BF AB                       .
	.byte   $80                             ; 80C0 80                       .
	sta     L80AA                           ; 80C1 8D AA 80                 ...
	lda     L474F                           ; 80C4 AD 4F 47                 .OG
	eor     #$01                            ; 80C7 49 01                    I.
	beq     L80CE                           ; 80C9 F0 03                    ..
	jmp     L80DA                           ; 80CB 4C DA 80                 L..

; ----------------------------------------------------------------------------
L80CE:  ldx     L80AB                           ; 80CE AE AB 80                 ...
	lda     L80AA                           ; 80D1 AD AA 80                 ...
	jsr     L8047                           ; 80D4 20 47 80                  G.
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
	ldx     #$80                            ; 80FA A2 80                    ..
	lda     #$B2                            ; 80FC A9 B2                    ..
	jsr     sub_461F
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
	jsr     sub_461F
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
L816D:  ldx     L80B1                           ; 816D AE B1 80                 ...
	lda     L80B0                           ; 8170 AD B0 80                 ...
	jsr     L8047                           ; 8173 20 47 80                  G.
L8176:  rts                                     ; 8176 60                       `

; ----------------------------------------------------------------------------
	.byte   $F4                             ; 8177 F4                       .
	pha                                     ; 8178 48                       H
L8179:  rts                                     ; 8179 60                       `

; ----------------------------------------------------------------------------
L817A:  .byte   $AD                             ; 817A AD                       .
L817B:  .byte   $6F                             ; 817B 6F                       o

L817C:	prolog
	jsr     sub_44D5                        ; 817F 20 D5 44                  .D
	.byte   $77                             ; 8182 77                       w
	sta     ($03,x)                         ; 8183 81 03                    ..
	lda     #$81                            ; 8185 A9 81                    ..
	sta     $A3                             ; 8187 85 A3                    ..
	lda     #$00                            ; 8189 A9 00                    ..
	sta     $A5                             ; 818B 85 A5                    ..
	lda     #$02                            ; 818D A9 02                    ..
	sta     $A4                             ; 818F 85 A4                    ..
	ldy     #$77                            ; 8191 A0 77                    .w
	ldx     #$47                            ; 8193 A2 47                    .G
	lda     #$53                            ; 8195 A9 53                    .S
	jsr     sub_461F
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
	ldx     #$47                            ; 81BF A2 47                    .G
	lda     #$55                            ; 81C1 A9 55                    .U
	jsr     sub_461F
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
L81D9:  .byte   $01                             ; 81D9 01                       .
L81DA:  tya                                     ; 81DA 98                       .
L81DB:  .byte   $8D                             ; 81DB 8D                       .
L81DC:  .byte   $B4                             ; 81DC B4                       .
L81DD:  ora     #$4C                            ; 81DD 09 4C                    .L
	.byte   $DD                             ; 81DF DD                       .
L81E0:  .byte   $F3                             ; 81E0 F3                       .
L81E1:  .byte   $4D                             ; 81E1 4D                       M
L81E2:  .byte   $4F                             ; 81E2 4F                       O
L81E3:  .byte   $43                             ; 81E3 43                       C
L81E4:  .byte   $2E                             ; 81E4 2E                       .
L81E5:  .byte   $52                             ; 81E5 52                       R
L81E6:  .byte   $45                             ; 81E6 45                       E
L81E7:  .byte   $4D                             ; 81E7 4D                       M
L81E8:  .byte   $43                             ; 81E8 43                       C
L81E9:  eor     ($52,x)                         ; 81E9 41 52                    AR
	eor     $4E                             ; 81EB 45 4E                    EN
L81ED:  .byte   $44                             ; 81ED 44                       D
L81EE:  .byte   $53                             ; 81EE 53                       S
	.byte   $43                             ; 81EF 43                       C
	.byte   $52                             ; 81F0 52                       R
L81F1:  .byte   $4E                             ; 81F1 4E                       N

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
	jsr     sub_461F
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
	ldx     L81E5                           ; 8251 AE E5 81                 ...
	lda     L81E4                           ; 8254 AD E4 81                 ...
	jsr     L4C75                           ; 8257 20 75 4C                  uL
	lda     $A0                             ; 825A A5 A0                    ..
	sta     L81DB                           ; 825C 8D DB 81                 ...
	lda     L81DB                           ; 825F AD DB 81                 ...
	eor     #$01                            ; 8262 49 01                    I.
	lbne	L827A
	ldy     L4750                           ; 8269 AC 50 47                 .PG
	ldxa	L81E4
	jsr     L7368                           ; 8272 20 68 73                  hs
	lda     $A0                             ; 8275 A5 A0                    ..
	sta     L81DB                           ; 8277 8D DB 81                 ...
L827A:  lda     L81DB                           ; 827A AD DB 81                 ...
	eor     #$01                            ; 827D 49 01                    I.
	lbne	L832F
	ldxa	L81E4
	jsr     L73DA                           ; 828A 20 DA 73                  .s
	lda     $A0                             ; 828D A5 A0                    ..
	sta     L81E3                           ; 828F 8D E3 81                 ...
	lda     L4750                           ; 8292 AD 50 47                 .PG
	eor     L81E3                           ; 8295 4D E3 81                 M..
	bne     L829D                           ; 8298 D0 03                    ..
	jmp     L82A5                           ; 829A 4C A5 82                 L..

; ----------------------------------------------------------------------------
L829D:  lda     #$02                            ; 829D A9 02                    ..
	sta     L81E2                           ; 829F 8D E2 81                 ...
	jmp     L832C                           ; 82A2 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L82A5:  lda     L81F1                           ; 82A5 AD F1 81                 ...
	beq     L82AD                           ; 82A8 F0 03                    ..
	jmp     L82B5                           ; 82AA 4C B5 82                 L..

; ----------------------------------------------------------------------------
L82AD:  ldy     #$01                            ; 82AD A0 01                    ..
	sty     L81E2                           ; 82AF 8C E2 81                 ...
	jmp     L832C                           ; 82B2 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L82B5:  clc                                     ; 82B5 18                       .
	lda     L81DC                           ; 82B6 AD DC 81                 ...
	adc     #$1E                            ; 82B9 69 1E                    i.
	sta     L81E6                           ; 82BB 8D E6 81                 ...
	lda     L81DD                           ; 82BE AD DD 81                 ...
	adc     #$00                            ; 82C1 69 00                    i.
	sta     L81E7                           ; 82C3 8D E7 81                 ...
	lda     L81E7                           ; 82C6 AD E7 81                 ...
	sta     $A3                             ; 82C9 85 A3                    ..
	lda     L81E8                           ; 82CB AD E8 81                 ...
	sta     $A4                             ; 82CE 85 A4                    ..
	lda     L81E9                           ; 82D0 AD E9 81                 ...
	sta     $A5                             ; 82D3 85 A5                    ..
	ldy     L81E6                           ; 82D5 AC E6 81                 ...
	ldx     #$81                            ; 82D8 A2 81                    ..
	lda     #$DE                            ; 82DA A9 DE                    ..
	jsr     L4C1D                           ; 82DC 20 1D 4C                  .L
	clc                                     ; 82DF 18                       .
	lda     L81E0                           ; 82E0 AD E0 81                 ...
	adc     L81ED                           ; 82E3 6D ED 81                 m..
	sta     $AE                             ; 82E6 85 AE                    ..
	sec                                     ; 82E8 38                       8
	lda     $AE                             ; 82E9 A5 AE                    ..
	sbc     #$01                            ; 82EB E9 01                    ..
	sta     L81E0                           ; 82ED 8D E0 81                 ...
	clc                                     ; 82F0 18                       .
	lda     L81E1                           ; 82F1 AD E1 81                 ...
	adc     L81EE                           ; 82F4 6D EE 81                 m..
	sta     $AE                             ; 82F7 85 AE                    ..
	sec                                     ; 82F9 38                       8
	lda     $AE                             ; 82FA A5 AE                    ..
	sbc     #$01                            ; 82FC E9 01                    ..
	sta     L81E1                           ; 82FE 8D E1 81                 ...
	lda     #$81                            ; 8301 A9 81                    ..
	sta     $A3                             ; 8303 85 A3                    ..
	ldy     #$DE                            ; 8305 A0 DE                    ..
	ldx     L81E5                           ; 8307 AE E5 81                 ...
	lda     L81E4                           ; 830A AD E4 81                 ...
	jsr     L4C75                           ; 830D 20 75 4C                  uL
	lda     $A0                             ; 8310 A5 A0                    ..
	sta     L81DB                           ; 8312 8D DB 81                 ...
	lda     L81DB                           ; 8315 AD DB 81                 ...
	eor     #$01                            ; 8318 49 01                    I.
	beq     L831F                           ; 831A F0 03                    ..
	jmp     L8327                           ; 831C 4C 27 83                 L'.

; ----------------------------------------------------------------------------
L831F:  ldy     #$01                            ; 831F A0 01                    ..
	sty     L81E2                           ; 8321 8C E2 81                 ...
	jmp     L832C                           ; 8324 4C 2C 83                 L,.

; ----------------------------------------------------------------------------
L8327:  lda     #$02                            ; 8327 A9 02                    ..
	sta     L81E2                           ; 8329 8D E2 81                 ...
L832C:  jmp     L8334                           ; 832C 4C 34 83                 L4.

; ----------------------------------------------------------------------------
L832F:  lda     #$02                            ; 832F A9 02                    ..
	sta     L81E2                           ; 8331 8D E2 81                 ...
L8334:  lda     L81E2                           ; 8334 AD E2 81                 ...
	eor     #$02                            ; 8337 49 02                    I.
	beq     L833E                           ; 8339 F0 03                    ..
	jmp     L83D1                           ; 833B 4C D1 83                 L..

; ----------------------------------------------------------------------------
L833E:  ldy     #$00                            ; 833E A0 00                    ..
	ldx     L81D9                           ; 8340 AE D9 81                 ...
	lda     L4750                           ; 8343 AD 50 47                 .PG
	jsr     sub_768A
	lda     $A0                             ; 8349 A5 A0                    ..
	beq     L8350                           ; 834B F0 03                    ..
	jmp     L8386                           ; 834D 4C 86 83                 L..

; ----------------------------------------------------------------------------
L8350:  lda     #$80                            ; 8350 A9 80                    ..
	cmp     L81D9                           ; 8352 CD D9 81                 ...
	bcc     L835A                           ; 8355 90 03                    ..
	jmp     L8362                           ; 8357 4C 62 83                 Lb.

; ----------------------------------------------------------------------------
L835A:  lda     #$0D                            ; 835A A9 0D                    ..
	jsr     sub_4BA7
	jmp     L8371                           ; 835F 4C 71 83                 Lq.

; ----------------------------------------------------------------------------
L8362:  lda     #$00                            ; 8362 A9 00                    ..
	cmp     L81D9                           ; 8364 CD D9 81                 ...
	bcc     L836C                           ; 8367 90 03                    ..
	jmp     L8371                           ; 8369 4C 71 83                 Lq.

; ----------------------------------------------------------------------------
L836C:  lda     #$0F                            ; 836C A9 0F                    ..
	jsr     sub_4BA7
L8371:  lda     L474F                           ; 8371 AD 4F 47                 .OG
	eor     #$03                            ; 8374 49 03                    I.
	lbne	L8386
	mv	L4762, L81E4
	ldy     #$01                            ; 8381 A0 01                    ..
	sty     L4657                           ; 8383 8C 57 46                 .WF
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
	bcc     L83B4                           ; 83AF 90 03                    ..
	jmp     L83B9                           ; 83B1 4C B9 83                 L..

; ----------------------------------------------------------------------------
L83B4:  lda     #$10                            ; 83B4 A9 10                    ..
	jsr     sub_4BA7
L83B9:  lda     L474F                           ; 83B9 AD 4F 47                 .OG
	eor     #$03                            ; 83BC 49 03                    I.
	beq     L83C3                           ; 83BE F0 03                    ..
	jmp     L83CE                           ; 83C0 4C CE 83                 L..

; ----------------------------------------------------------------------------
L83C3:  lda     L81E5                           ; 83C3 AD E5 81                 ...
	sta     L4763                           ; 83C6 8D 63 47                 .cG
	ldy     #$01                            ; 83C9 A0 01                    ..
	sty     L4657                           ; 83CB 8C 57 46                 .WF
L83CE:  jmp     L83E2                           ; 83CE 4C E2 83                 L..

; ----------------------------------------------------------------------------
L83D1:  lda     L81E4                           ; 83D1 AD E4 81                 ...
	sta     L4762                           ; 83D4 8D 62 47                 .bG
	lda     L81E5                           ; 83D7 AD E5 81                 ...
	sta     L4763                           ; 83DA 8D 63 47                 .cG
	ldy     #$01                            ; 83DD A0 01                    ..
	sty     L4657                           ; 83DF 8C 57 46
L83E2:  rts                                     ; 83E2 60                       `

; ----------------------------------------------------------------------------
L83E3:  .byte   $09                             ; 83E3 09                       .
L83E4:  .byte   $4C                             ; 83E4 4C                       L
L83E5:  .byte   $DD                             ; 83E5 DD                       .
L83E6:  .byte   $F3                             ; 83E6 F3                       .
L83E7:  .byte   $A9                             ; 83E7 A9                       .
L83E8:  brk                                     ; 83E8 00                       .
L83E9:  .byte   $F0                             ; 83E9 F0                       .
L83EA:  inc     $AD,x                           ; 83EA F6 AD                    ..
	.byte   $B7                             ; 83EC B7                       .
L83ED:  .byte   $09                             ; 83ED 09                       .
L83EE:  and     #$BF                            ; 83EE 29 BF                    ).
	;jmp     LF6C2                           ; 83F0 4C C2 F6                 L..
	.byte	$4C,$C2,$F6

; ----------------------------------------------------------------------------
	.byte   $AD                             ; 83F3 AD                       .
L83F4:  .byte   $B7                             ; 83F4 B7                       .
L83F5:  .byte   $09                             ; 83F5 09                       .
L83F6:  .byte   $09                             ; 83F6 09                       .
L83F7:  rti                                     ; 83F7 40                       @

; ----------------------------------------------------------------------------
L83F8:  ;bne     L83E1                           ; 83F8 D0 E7                    ..
	.byte	$D0,$E7
	.byte   $A9                             ; 83FA A9                       .
L83FB:  .byte   $09                             ; 83FB 09                       .
L83FC:  ldy     #$B8                            ; 83FC A0 B8                    ..
	ldx     #$20                            ; 83FE A2 20                    . 
	;jsr     LF823                           ; 8400 20 23 F8                  #.
	.byte	$20,$23,$F8
	lda     #$04                            ; 8403 A9 04                    ..
	.byte   $9D                             ; 8405 9D                       .
	lsr     a                               ; 8406 4A                       J
L8407:  .byte   $03                             ; 8407 03                       .
L8408:  .byte   $A9                             ; 8408 A9                       .
L8409:  .byte   $03                             ; 8409 03                       .
L840A:  .byte   $20                             ; 840A 20                        
L840B:  clv                                     ; 840B B8                       .
L840C:  prolog
	stx     L83E4                           ; 840F 8E E4 83                 ...
	sta     L83E3                           ; 8412 8D E3 83                 ...
	clc                                     ; 8415 18                       .
	lda     L4751                           ; 8416 AD 51 47                 .QG
	adc     L83E3                           ; 8419 6D E3 83                 m..
	sta     L83E5                           ; 841C 8D E5 83                 ...
	clc                                     ; 841F 18                       .
	lda     L4752                           ; 8420 AD 52 47                 .RG
	adc     L83E4                           ; 8423 6D E4 83                 m..
	sta     L83E6                           ; 8426 8D E6 83                 ...
	lda     L474F                           ; 8429 AD 4F 47                 .OG
	eor     #$01                            ; 842C 49 01                    I.
	lbne	L843C
	ldx     L83E6                           ; 8433 AE E6 83                 ...
	lda     L83E5                           ; 8436 AD E5 83                 ...
	jsr     L8047                           ; 8439 20 47 80                  G.
L843C:  lda     L474F                           ; 843C AD 4F 47                 .OG
	eor     #$02                            ; 843F 49 02                    I.
	lbeq	L8447
	rts                                     ; 8446 60                       `

; ----------------------------------------------------------------------------
L8447:  lda     L4750                           ; 8447 AD 50 47                 .PG
	jsr     sub_7035
	lda     $A1                             ; 844D A5 A1                    ..
	sta     L83E8                           ; 844F 8D E8 83                 ...
	lda     $A0                             ; 8452 A5 A0                    ..
	sta     L83E7                           ; 8454 8D E7 83                 ...
	lda     L83E8                           ; 8457 AD E8 83                 ...
	sta     $A3                             ; 845A 85 A3                    ..
	lda     #$00                            ; 845C A9 00                    ..
	sta     $A5                             ; 845E 85 A5                    ..
	lda     #$05                            ; 8460 A9 05                    ..
	sta     $A4                             ; 8462 85 A4                    ..
	ldy     L83E7                           ; 8464 AC E7 83                 ...
	ldx     #$84                            ; 8467 A2 84                    ..
	lda     #$07                            ; 8469 A9 07                    ..
	jsr     sub_461F
	lda     L840B                           ; 846E AD 0B 84                 ...
	jsr     L65B0                           ; 8471 20 B0 65                  .e
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
	jsr     sub_461F
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
	bcs     L84CC                           ; 84C7 B0 03                    ..
	jmp     L84CD                           ; 84C9 4C CD 84                 L..

; ----------------------------------------------------------------------------
L84CC:  rts                                     ; 84CC 60                       `

; ----------------------------------------------------------------------------
L84CD:  lda     L83EE                           ; 84CD AD EE 83                 ...
	asl     a                               ; 84D0 0A                       .
	php                                     ; 84D1 08                       .
	clc                                     ; 84D2 18                       .
	adc     L83FB                           ; 84D3 6D FB 83                 m..
	sta     $AE                             ; 84D6 85 AE                    ..
	lda     #$00                            ; 84D8 A9 00                    ..
	rol     a                               ; 84DA 2A                       *
	plp                                     ; 84DB 28                       (
	adc     L83FC                           ; 84DC 6D FC 83                 m..
	sta     $AF                             ; 84DF 85 AF                    ..
	ldy     #$01                            ; 84E1 A0 01                    ..
	lda     ($AE),y                         ; 84E3 B1 AE                    ..
	sta     L83F5                           ; 84E5 8D F5 83                 ...
	dey                                     ; 84E8 88                       .
	lda     ($AE),y                         ; 84E9 B1 AE                    ..
	sta     L83F4                           ; 84EB 8D F4 83                 ...
	clc                                     ; 84EE 18                       .
	lda     L83F4                           ; 84EF AD F4 83                 ...
	adc     L83ED                           ; 84F2 6D ED 83                 m..
	sta     $AE                             ; 84F5 85 AE                    ..
	lda     L83F5                           ; 84F7 AD F5 83                 ...
	adc     #$00                            ; 84FA 69 00                    i.
	sta     $AF                             ; 84FC 85 AF                    ..
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
L851D:  sed                                     ; 851D F8                       .
L851E:  .byte   $4C                             ; 851E 4C                       L
L851F:  .byte   $DD                             ; 851F DD                       .
L8520:  .byte   $F3                             ; 8520 F3                       .
L8521:  jmp     L8524                           ; 8521 4C 24 85                 L$.

; ----------------------------------------------------------------------------
L8524:  jsr     sub_44D5                           ; 8524 20 D5 44                  .D
	ora     $0385,x                         ; 8527 1D 85 03                 ...
	lda     L851D                           ; 852A AD 1D 85                 ...
	eor     #$01                            ; 852D 49 01                    I.
	beq     L8534                           ; 852F F0 03                    ..
	jmp     L854F                           ; 8531 4C 4F 85                 LO.

; ----------------------------------------------------------------------------
L8534:  lda     L851E                           ; 8534 AD 1E 85                 ...
	eor     #$41                            ; 8537 49 41                    IA
	bne     L853E                           ; 8539 D0 03                    ..
	jmp     L854F                           ; 853B 4C 4F 85                 LO.

; ----------------------------------------------------------------------------
L853E:  lda     L851F                           ; 853E AD 1F 85                 ...
	sta     $AE                             ; 8541 85 AE                    ..
	lda     L8520                           ; 8543 AD 20 85                 . .
	sta     $AF                             ; 8546 85 AF                    ..
L8548:  lda     L851E                           ; 8548 AD 1E 85                 ...
	ldy     #$00                            ; 854B A0 00                    ..
	sta     ($AE),y                         ; 854D 91 AE                    ..
L854F:  rts                                     ; 854F 60                       `

; ----------------------------------------------------------------------------
L8550:  .byte   $9D                             ; 8550 9D                       .
L8551:  pha                                     ; 8551 48                       H
L8552:  .byte   $03                             ; 8552 03                       .
L8553:  rts                                     ; 8553 60                       `

; ----------------------------------------------------------------------------
L8554:  .byte   $A0                             ; 8554 A0                       .
L8555:  .byte   $EB                             ; 8555 EB                       .
L8556:  .byte   $A9                             ; 8556 A9                       .
L8557:  .byte   $09                             ; 8557 09                       .
L8558:  .byte   $20                             ; 8558 20                        
L8559:  .byte   $23                             ; 8559 23                       #
L855A:  sed                                     ; 855A F8                       .
L855B:  .byte   $A0                             ; 855B A0                       .
L855C:  .byte   $80                             ; 855C 80                       .
L855D:  .byte   $4C                             ; 855D 4C                       L
L855E:  .byte   $2B                             ; 855E 2B                       +
L855F:  sed                                     ; 855F F8                       .
L8560:  .byte   $A9                             ; 8560 A9                       .
L8561:  .byte   $07                             ; 8561 07                       .
L8562:  .byte   $8D                             ; 8562 8D                       .
L8563:  .byte   $6C                             ; 8563 6C                       l
L8564:  asl     a                               ; 8564 0A                       .
L8565:  lda     $0A6C                           ; 8565 AD 6C 0A                 .l.
	asl     a                               ; 8568 0A                       .
L8569:  asl     a                               ; 8569 0A                       .
L856A:  asl     a                               ; 856A 0A                       .
L856B:  asl     a                               ; 856B 0A                       .
L856C:  tax                                     ; 856C AA                       .
L856D:  .byte   $20                             ; 856D 20                        
L856E:  .byte   $B6                             ; 856E B6                       .
L856F:  .byte   $F7                             ; 856F F7                       .
L8570:  .byte   $CE                             ; 8570 CE                       .
L8571:  .byte   $6C                             ; 8571 6C                       l
L8572:  asl     a                               ; 8572 0A                       .

L8573:  stack_prolog L8550, $02
	lda     L8550                           ; 857C AD 50 85                 .P.
	jsr     sub_7035
	lda     $A1                             ; 8582 A5 A1                    ..
	sta     L8557                           ; 8584 8D 57 85                 .W.
	lda     $A0                             ; 8587 A5 A0                    ..
	sta     L8556                           ; 8589 8D 56 85                 .V.
	lda     L8557                           ; 858C AD 57 85                 .W.
	sta     $A3                             ; 858F 85 A3                    ..
	lda     #$00                            ; 8591 A9 00                    ..
	sta     $A5                             ; 8593 85 A5                    ..
	lda     #$07                            ; 8595 A9 07                    ..
	sta     $A4                             ; 8597 85 A4                    ..
	ldy     L8556                           ; 8599 AC 56 85                 .V.
	ldx     #$85                            ; 859C A2 85                    ..
	lda     #$64                            ; 859E A9 64                    .d
	jsr     sub_461F
	lda     L8552                           ; 85A3 AD 52 85                 .R.
	sta     $A3                             ; 85A6 85 A3                    ..
	lda     #$00                            ; 85A8 A9 00                    ..
	sta     $A5                             ; 85AA 85 A5                    ..
	lda     #$04                            ; 85AC A9 04                    ..
	sta     $A4                             ; 85AE 85 A4                    ..
	ldy     L8551                           ; 85B0 AC 51 85                 .Q.
	ldx     #$85                            ; 85B3 A2 85                    ..
	lda     #$60                            ; 85B5 A9 60                    .`
	jsr     sub_461F
	clc                                     ; 85BA 18                       .
	lda     L8556                           ; 85BB AD 56 85                 .V.
	adc     #$0E                            ; 85BE 69 0E                    i.
	sta     $A2                             ; 85C0 85 A2                    ..
	lda     L8557                           ; 85C2 AD 57 85                 .W.
	adc     #$00                            ; 85C5 69 00                    i.
	sta     $A3                             ; 85C7 85 A3                    ..
	lda     #$00                            ; 85C9 A9 00                    ..
	sta     $A5                             ; 85CB 85 A5                    ..
	lda     #$08                            ; 85CD A9 08                    ..
	sta     $A4                             ; 85CF 85 A4                    ..
	ldy     $A2                             ; 85D1 A4 A2                    ..
	ldx     #$85                            ; 85D3 A2 85                    ..
	lda     #$6B                            ; 85D5 A9 6B                    .k
	jsr     sub_461F
	ldy     #$01                            ; 85DA A0 01                    ..
	sty     $A0                             ; 85DC 84 A0                    ..
	lda     #$85                            ; 85DE A9 85                    ..
	sta     $A2                             ; 85E0 85 A2                    ..
	lda     #$6B                            ; 85E2 A9 6B                    .k
	sta     $A1                             ; 85E4 85 A1                    ..
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
	beq     L860D                           ; 8608 F0 03                    ..
	jmp     L8611                           ; 860A 4C 11 86                 L..

; ----------------------------------------------------------------------------
L860D:  iny                                     ; 860D C8                       .
	sty     L855D                           ; 860E 8C 5D 85                 .].
L8611:  ldy     #$00                            ; 8611 A0 00                    ..
	sty     L855E                           ; 8613 8C 5E 85                 .^.
	clc                                     ; 8616 18                       .
	lda     L8564                           ; 8617 AD 64 85                 .d.
	adc     L8569                           ; 861A 6D 69 85                 mi.
	sta     $AE                             ; 861D 85 AE                    ..
	sec                                     ; 861F 38                       8
	lda     $AE                             ; 8620 A5 AE                    ..
	sbc     #$01                            ; 8622 E9 01                    ..
	sta     $AC                             ; 8624 85 AC                    ..
	lda     $AC                             ; 8626 A5 AC                    ..
	eor     L8562                           ; 8628 4D 62 85                 Mb.
	beq     L8630                           ; 862B F0 03                    ..
	jmp     L8634                           ; 862D 4C 34 86                 L4.

; ----------------------------------------------------------------------------
L8630:  iny                                     ; 8630 C8                       .
	sty     L855E                           ; 8631 8C 5E 85                 .^.
L8634:  ldy     #$00                            ; 8634 A0 00                    ..
	sty     L855F                           ; 8636 8C 5F 85                 ._.
	lda     L8565                           ; 8639 AD 65 85                 .e.
	eor     L8561                           ; 863C 4D 61 85                 Ma.
	beq     L8644                           ; 863F F0 03                    ..
	jmp     L8648                           ; 8641 4C 48 86                 LH.

; ----------------------------------------------------------------------------
L8644:  iny                                     ; 8644 C8                       .
	sty     L855F                           ; 8645 8C 5F 85                 ._.
L8648:  lda     #$00                            ; 8648 A9 00                    ..
	sta     $85                             ; 864A 85 85                    ..
	lda     #$28                            ; 864C A9 28                    .(
	sta     $84                             ; 864E 85 84                    ..
	lda     L8561                           ; 8650 AD 61 85                 .a.
	ldx     #$00                            ; 8653 A2 00                    ..
	jsr     sub_444A
	sta     $AE                             ; 8658 85 AE                    ..
	txa                                     ; 865A 8A                       .
	sta     $AF                             ; 865B 85 AF                    ..
	clc                                     ; 865D 18                       .
	lda     L466D                           ; 865E AD 6D 46                 .mF
	adc     $AE                             ; 8661 65 AE                    e.
	sta     L8558                           ; 8663 8D 58 85                 .X.
	lda     L466E                           ; 8666 AD 6E 46                 .nF
	adc     $AF                             ; 8669 65 AF                    e.
	sta     L8559                           ; 866B 8D 59 85                 .Y.
	sec                                     ; 866E 38                       8
	lda     L8562                           ; 866F AD 62 85                 .b.
	sbc     L8560                           ; 8672 ED 60 85                 .`.
	sta     L8555                           ; 8675 8D 55 85                 .U.
	lda     L8560                           ; 8678 AD 60 85                 .`.
	sta     L8553                           ; 867B 8D 53 85                 .S.
	clc                                     ; 867E 18                       .
	lda     L8553                           ; 867F AD 53 85                 .S.
	adc     L8555                           ; 8682 6D 55 85                 mU.
	sta     L8554                           ; 8685 8D 54 85                 .T.
	lda     L855F                           ; 8688 AD 5F 85                 ._.
	bne     L8690                           ; 868B D0 03                    ..
	jmp     L870F                           ; 868D 4C 0F 87                 L..

; ----------------------------------------------------------------------------
L8690:  lda     L856C                           ; 8690 AD 6C 85                 .l.
	eor     #$41                            ; 8693 49 41                    IA
	bne     L869A                           ; 8695 D0 03                    ..
	jmp     L86C8                           ; 8697 4C C8 86                 L..

; ----------------------------------------------------------------------------
L869A:  clc                                     ; 869A 18                       .
	lda     L8558                           ; 869B AD 58 85                 .X.
	adc     L8553                           ; 869E 6D 53 85                 mS.
	sta     L855A                           ; 86A1 8D 5A 85                 .Z.
	lda     L8559                           ; 86A4 AD 59 85                 .Y.
	adc     #$00                            ; 86A7 69 00                    i.
	sta     L855B                           ; 86A9 8D 5B 85                 .[.
	clc                                     ; 86AC 18                       .
	lda     L8555                           ; 86AD AD 55 85                 .U.
	adc     #$01                            ; 86B0 69 01                    i.
	sta     $A2                             ; 86B2 85 A2                    ..
	lda     #$00                            ; 86B4 A9 00                    ..
	sta     $A3                             ; 86B6 85 A3                    ..
	lda     L856C                           ; 86B8 AD 6C 85                 .l.
	sta     $A4                             ; 86BB 85 A4                    ..
	ldy     $A2                             ; 86BD A4 A2                    ..
	ldx     L855B                           ; 86BF AE 5B 85                 .[.
	lda     L855A                           ; 86C2 AD 5A 85                 .Z.
	jsr     L45FC                           ; 86C5 20 FC 45                  .E
L86C8:  clc                                     ; 86C8 18                       .
	lda     L8558                           ; 86C9 AD 58 85                 .X.
	adc     L8553                           ; 86CC 6D 53 85                 mS.
	sta     $A2                             ; 86CF 85 A2                    ..
	lda     L8559                           ; 86D1 AD 59 85                 .Y.
	adc     #$00                            ; 86D4 69 00                    i.
	sta     $A3                             ; 86D6 85 A3                    ..
	ldy     $A2                             ; 86D8 A4 A2                    ..
	ldx     L856B                           ; 86DA AE 6B 85                 .k.
	lda     L855D                           ; 86DD AD 5D 85                 .].
	jsr     L8521                           ; 86E0 20 21 85                  !.
	clc                                     ; 86E3 18                       .
	lda     L8558                           ; 86E4 AD 58 85                 .X.
	adc     L8554                           ; 86E7 6D 54 85                 mT.
	sta     $A2                             ; 86EA 85 A2                    ..
	lda     L8559                           ; 86EC AD 59 85                 .Y.
	adc     #$00                            ; 86EF 69 00                    i.
	sta     $A3                             ; 86F1 85 A3                    ..
	ldy     $A2                             ; 86F3 A4 A2                    ..
	ldx     L856D                           ; 86F5 AE 6D 85                 .m.
	lda     L855E                           ; 86F8 AD 5E 85                 .^.
	jsr     L8521                           ; 86FB 20 21 85                  !.
	clc                                     ; 86FE 18                       .
	lda     L8558                           ; 86FF AD 58 85                 .X.
	adc     #$28                            ; 8702 69 28                    i(
	sta     L8558                           ; 8704 8D 58 85                 .X.
	lda     L8559                           ; 8707 AD 59 85                 .Y.
	adc     #$00                            ; 870A 69 00                    i.
	sta     L8559                           ; 870C 8D 59 85                 .Y.
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
L8729:  brk                                     ; 8729 00                       .
L872A:  clc                                     ; 872A 18                       .
	lda     L8558                           ; 872B AD 58 85                 .X.
	adc     L8553                           ; 872E 6D 53 85                 mS.
	sta     $A2                             ; 8731 85 A2                    ..
	lda     L8559                           ; 8733 AD 59 85                 .Y.
	adc     #$00                            ; 8736 69 00                    i.
	sta     $A3                             ; 8738 85 A3                    ..
	ldy     $A2                             ; 873A A4 A2                    ..
	ldx     L8572                           ; 873C AE 72 85                 .r.
	lda     L855D                           ; 873F AD 5D 85                 .].
	jsr     L8521                           ; 8742 20 21 85                  !.
	clc                                     ; 8745 18                       .
	lda     L8558                           ; 8746 AD 58 85                 .X.
	adc     L8554                           ; 8749 6D 54 85                 mT.
	sta     $A2                             ; 874C 85 A2                    ..
	lda     L8559                           ; 874E AD 59 85                 .Y.
	adc     #$00                            ; 8751 69 00                    i.
	sta     $A3                             ; 8753 85 A3                    ..
	ldy     $A2                             ; 8755 A4 A2                    ..
	ldx     L856E                           ; 8757 AE 6E 85                 .n.
	lda     L855E                           ; 875A AD 5E 85                 .^.
	jsr     L8521                           ; 875D 20 21 85                  !.
	clc                                     ; 8760 18                       .
	lda     L8558                           ; 8761 AD 58 85                 .X.
	adc     #$28                            ; 8764 69 28                    i(
	sta     L8558                           ; 8766 8D 58 85                 .X.
	lda     L8559                           ; 8769 AD 59 85                 .Y.
	adc     #$00                            ; 876C 69 00                    i.
	sta     L8559                           ; 876E 8D 59 85                 .Y.
	inc     L855C                           ; 8771 EE 5C 85                 .\.
	jmp     L871E                           ; 8774 4C 1E 87                 L..

; ----------------------------------------------------------------------------
L8777:  clc                                     ; 8777 18                       .
	lda     L8565                           ; 8778 AD 65 85                 .e.
	adc     L856A                           ; 877B 6D 6A 85                 mj.
	sta     $AE                             ; 877E 85 AE                    ..
	sec                                     ; 8780 38                       8
	lda     $AE                             ; 8781 A5 AE                    ..
	sbc     #$01                            ; 8783 E9 01                    ..
	sta     $AC                             ; 8785 85 AC                    ..
	lda     $AC                             ; 8787 A5 AC                    ..
	eor     L8563                           ; 8789 4D 63 85                 Mc.
	beq     L8791                           ; 878C F0 03                    ..
	jmp     L8818                           ; 878E 4C 18 88                 L..

; ----------------------------------------------------------------------------
L8791:  lda     L855F                           ; 8791 AD 5F 85                 ._.
	bne     L8799                           ; 8794 D0 03                    ..
	jmp     L87AA                           ; 8796 4C AA 87                 L..

; ----------------------------------------------------------------------------
L8799:  sec                                     ; 8799 38                       8
	lda     L8558                           ; 879A AD 58 85                 .X.
	sbc     #$28                            ; 879D E9 28                    .(
	sta     L8558                           ; 879F 8D 58 85                 .X.
	lda     L8559                           ; 87A2 AD 59 85                 .Y.
	sbc     #$00                            ; 87A5 E9 00                    ..
	sta     L8559                           ; 87A7 8D 59 85                 .Y.
L87AA:  lda     L8570                           ; 87AA AD 70 85                 .p.
	eor     #$41                            ; 87AD 49 41                    IA
	bne     L87B4                           ; 87AF D0 03                    ..
	jmp     L87E2                           ; 87B1 4C E2 87                 L..

; ----------------------------------------------------------------------------
L87B4:  clc                                     ; 87B4 18                       .
	lda     L8558                           ; 87B5 AD 58 85                 .X.
	adc     L8553                           ; 87B8 6D 53 85                 mS.
	sta     L855A                           ; 87BB 8D 5A 85                 .Z.
	lda     L8559                           ; 87BE AD 59 85                 .Y.
	adc     #$00                            ; 87C1 69 00                    i.
	sta     L855B                           ; 87C3 8D 5B 85                 .[.
	clc                                     ; 87C6 18                       .
	lda     L8555                           ; 87C7 AD 55 85                 .U.
	adc     #$01                            ; 87CA 69 01                    i.
	sta     $A2                             ; 87CC 85 A2                    ..
	lda     #$00                            ; 87CE A9 00                    ..
	sta     $A3                             ; 87D0 85 A3                    ..
	lda     L8570                           ; 87D2 AD 70 85                 .p.
	sta     $A4                             ; 87D5 85 A4                    ..
	ldy     $A2                             ; 87D7 A4 A2                    ..
	ldx     L855B                           ; 87D9 AE 5B 85                 .[.
	lda     L855A                           ; 87DC AD 5A 85                 .Z.
	jsr     L45FC                           ; 87DF 20 FC 45                  .E
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
	jsr     L8521                           ; 87FA 20 21 85                  !.
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
	jsr     L8521                           ; 8815 20 21 85                  !.
L8818:  rts                                     ; 8818 60                       `

; ----------------------------------------------------------------------------
L8819:  sta     $A0                             ; 8819 85 A0                    ..
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
L8831:  brk                                     ; 8831 00                       .
L8832:  brk                                     ; 8832 00                       .
L8833:  brk                                     ; 8833 00                       .
	brk                                     ; 8834 00                       .
L8835:  brk                                     ; 8835 00                       .
L8836:  brk                                     ; 8836 00                       .
L8837:  brk                                     ; 8837 00                       .
L8838:  brk                                     ; 8838 00                       .
L8839:  brk                                     ; 8839 00                       .
L883A:  brk                                     ; 883A 00                       .
L883B:  brk                                     ; 883B 00                       .
L883C:  brk                                     ; 883C 00                       .
	brk                                     ; 883D 00                       .
L883E:  brk                                     ; 883E 00                       .
L883F:  brk                                     ; 883F 00                       .
L8840:  brk                                     ; 8840 00                       .
L8841:  brk                                     ; 8841 00                       .
L8842:  brk                                     ; 8842 00                       .
L8843:  brk                                     ; 8843 00                       .
	brk                                     ; 8844 00                       .
	brk                                     ; 8845 00                       .
L8846:  brk                                     ; 8846 00                       .
	brk                                     ; 8847 00                       .
	brk                                     ; 8848 00                       .
	brk                                     ; 8849 00                       .
L884A:  brk                                     ; 884A 00                       .
L884B:  brk                                     ; 884B 00                       .
L884C:  jmp     L884F                           ; 884C 4C 4F 88                 LO.

; ----------------------------------------------------------------------------
L884F:  jsr     sub_44D5                           ; 884F 20 D5 44                  .D
	and     ($88),y                         ; 8852 31 88                    1.
	ora     $AD                             ; 8854 05 AD                    ..
	.byte   $34                             ; 8856 34                       4
	dey                                     ; 8857 88                       .
	sta     $A3                             ; 8858 85 A3                    ..
	lda     #$00                            ; 885A A9 00                    ..
	sta     $A5                             ; 885C 85 A5                    ..
	lda     #$04                            ; 885E A9 04                    ..
	sta     $A4                             ; 8860 85 A4                    ..
	ldy     L8833                           ; 8862 AC 33 88                 .3.
	ldx     #$88                            ; 8865 A2 88                    ..
	lda     #$3E                            ; 8867 A9 3E                    .>
	jsr     sub_461F
	lda     L8836                           ; 886C AD 36 88                 .6.
	sta     $A3                             ; 886F 85 A3                    ..
	lda     #$00                            ; 8871 A9 00                    ..
	sta     $A5                             ; 8873 85 A5                    ..
	lda     #$04                            ; 8875 A9 04                    ..
	sta     $A4                             ; 8877 85 A4                    ..
	ldy     L8835                           ; 8879 AC 35 88                 .5.
	ldx     #$88                            ; 887C A2 88                    ..
	lda     #$42                            ; 887E A9 42                    .B
	jsr     sub_461F
	lda     L8832                           ; 8883 AD 32 88                 .2.
	sta     $A3                             ; 8886 85 A3                    ..
	lda     #$00                            ; 8888 A9 00                    ..
	sta     $A5                             ; 888A 85 A5                    ..
	lda     #$06                            ; 888C A9 06                    ..
	sta     $A4                             ; 888E 85 A4                    ..
	ldy     L8831                           ; 8890 AC 31 88                 .1.
	ldx     #$88                            ; 8893 A2 88                    ..
	lda     #$46                            ; 8895 A9 46                    .F
	jsr     sub_461F
	lda     L883F                           ; 889A AD 3F 88                 .?.
	asl     a                               ; 889D 0A                       .
	php                                     ; 889E 08                       .
	clc                                     ; 889F 18                       .
	adc     L884A                           ; 88A0 6D 4A 88                 mJ.
	sta     $AE                             ; 88A3 85 AE                    ..
	lda     #$00                            ; 88A5 A9 00                    ..
	rol     a                               ; 88A7 2A                       *
	plp                                     ; 88A8 28                       (
	adc     L884B                           ; 88A9 6D 4B 88                 mK.
	sta     $AF                             ; 88AC 85 AF                    ..
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
	sta     $AE                             ; 88D1 85 AE                    ..
	txa                                     ; 88D3 8A                       .
	sta     $AF                             ; 88D4 85 AF                    ..
	clc                                     ; 88D6 18                       .
	lda     L466D                           ; 88D7 AD 6D 46                 .mF
	adc     $AE                             ; 88DA 65 AE                    e.
	sta     $AC                             ; 88DC 85 AC                    ..
	lda     L466E                           ; 88DE AD 6E 46                 .nF
	adc     $AF                             ; 88E1 65 AF                    e.
	sta     $AD                             ; 88E3 85 AD                    ..
	clc                                     ; 88E5 18                       .
	lda     $AC                             ; 88E6 A5 AC                    ..
	adc     L8842                           ; 88E8 6D 42 88                 mB.
	sta     L8837                           ; 88EB 8D 37 88                 .7.
	lda     $AD                             ; 88EE A5 AD                    ..
	adc     #$00                            ; 88F0 69 00                    i.
	sta     L8838                           ; 88F2 8D 38 88                 .8.
	sec                                     ; 88F5 38                       8
	lda     L8840                           ; 88F6 AD 40 88                 .@.
	sbc     L883E                           ; 88F9 ED 3E 88                 .>.
	sta     $AE                             ; 88FC 85 AE                    ..
	clc                                     ; 88FE 18                       .
	lda     $AE                             ; 88FF A5 AE                    ..
	adc     #$01                            ; 8901 69 01                    i.
	sta     L883C                           ; 8903 8D 3C 88                 .<.
	ldy     #$00                            ; 8906 A0 00                    ..
	sty     L883B                           ; 8908 8C 3B 88                 .;.
	sec                                     ; 890B 38                       8
	lda     L8841                           ; 890C AD 41 88                 .A.
	sbc     L883F                           ; 890F ED 3F 88                 .?.
	sta     L8920                           ; 8912 8D 20 89                 . .
L8915:  lda     L8920                           ; 8915 AD 20 89                 . .
	cmp     L883B                           ; 8918 CD 3B 88                 .;.
	bcs     L8921                           ; 891B B0 04                    ..
	jmp     L8960                           ; 891D 4C 60 89                 L`.

; ----------------------------------------------------------------------------
L8920:  brk                                     ; 8920 00                       .
L8921:  lda     L883A                           ; 8921 AD 3A 88                 .:.
	sta     $A3                             ; 8924 85 A3                    ..
	lda     L883C                           ; 8926 AD 3C 88                 .<.
	sta     $A4                             ; 8929 85 A4                    ..
	ldy     L8839                           ; 892B AC 39 88                 .9.
	ldx     L8838                           ; 892E AE 38 88                 .8.
	lda     L8837                           ; 8931 AD 37 88                 .7.
	jsr     L8819                           ; 8934 20 19 88                  ..
	clc                                     ; 8937 18                       .
	lda     L8837                           ; 8938 AD 37 88                 .7.
	adc     #$28                            ; 893B 69 28                    i(
	sta     L8837                           ; 893D 8D 37 88                 .7.
	lda     L8838                           ; 8940 AD 38 88                 .8.
	adc     #$00                            ; 8943 69 00                    i.
	sta     L8838                           ; 8945 8D 38 88                 .8.
	clc                                     ; 8948 18                       .
	lda     L8839                           ; 8949 AD 39 88                 .9.
	adc     L8846                           ; 894C 6D 46 88                 mF.
	sta     L8839                           ; 894F 8D 39 88                 .9.
	lda     L883A                           ; 8952 AD 3A 88                 .:.
	adc     #$00                            ; 8955 69 00                    i.
	sta     L883A                           ; 8957 8D 3A 88                 .:.
	inc     L883B                           ; 895A EE 3B 88                 .;.
	jmp     L8915                           ; 895D 4C 15 89                 L..

; ----------------------------------------------------------------------------
L8960:  rts                                     ; 8960 60                       `

; ----------------------------------------------------------------------------
L8961:  brk                                     ; 8961 00                       .
L8962:  brk                                     ; 8962 00                       .
L8963:  brk                                     ; 8963 00                       .
L8964:  brk                                     ; 8964 00                       .
L8965:  brk                                     ; 8965 00                       .
	brk                                     ; 8966 00                       .
	brk                                     ; 8967 00                       .
L8968:  brk                                     ; 8968 00                       .
L8969:  brk                                     ; 8969 00                       .
L896A:  brk                                     ; 896A 00                       .
L896B:  brk                                     ; 896B 00                       .
L896C:  brk                                     ; 896C 00                       .
	brk                                     ; 896D 00                       .
	brk                                     ; 896E 00                       .
	brk                                     ; 896F 00                       .
	brk                                     ; 8970 00                       .
	brk                                     ; 8971 00                       .
	brk                                     ; 8972 00                       .
	brk                                     ; 8973 00                       .
	brk                                     ; 8974 00                       .
	brk                                     ; 8975 00                       .
	brk                                     ; 8976 00                       .
	brk                                     ; 8977 00                       .
	brk                                     ; 8978 00                       .
	brk                                     ; 8979 00                       .
	brk                                     ; 897A 00                       .
	brk                                     ; 897B 00                       .
	brk                                     ; 897C 00                       .
	brk                                     ; 897D 00                       .
	brk                                     ; 897E 00                       .
	brk                                     ; 897F 00                       .
	brk                                     ; 8980 00                       .
	brk                                     ; 8981 00                       .
	brk                                     ; 8982 00                       .
	brk                                     ; 8983 00                       .
	brk                                     ; 8984 00                       .
	brk                                     ; 8985 00                       .
	brk                                     ; 8986 00                       .
	brk                                     ; 8987 00                       .
	brk                                     ; 8988 00                       .
	brk                                     ; 8989 00                       .
	brk                                     ; 898A 00                       .
	brk                                     ; 898B 00                       .
	brk                                     ; 898C 00                       .
L898D:  brk                                     ; 898D 00                       .
L898E:  brk                                     ; 898E 00                       .
L898F:  brk                                     ; 898F 00                       .
L8990:  brk                                     ; 8990 00                       .
L8991:  brk                                     ; 8991 00                       .
L8992:  brk                                     ; 8992 00                       .
L8993:  brk                                     ; 8993 00                       .
L8994:  brk                                     ; 8994 00                       .
L8995:  brk                                     ; 8995 00                       .
L8996:  brk                                     ; 8996 00                       .
L8997:  brk                                     ; 8997 00                       .
L8998:  brk                                     ; 8998 00                       .
L8999:  brk                                     ; 8999 00                       .
L899A:  brk                                     ; 899A 00                       .
L899B:  brk                                     ; 899B 00                       .
L899C:  brk                                     ; 899C 00                       .
L899D:  brk                                     ; 899D 00                       .
	brk                                     ; 899E 00                       .
	brk                                     ; 899F 00                       .
	brk                                     ; 89A0 00                       .
	brk                                     ; 89A1 00                       .
L89A2:  brk                                     ; 89A2 00                       .
	brk                                     ; 89A3 00                       .
L89A4:  brk                                     ; 89A4 00                       .
L89A5:  brk                                     ; 89A5 00                       .
L89A6:  brk                                     ; 89A6 00                       .
L89A7:  brk                                     ; 89A7 00                       .
L89A8:  brk                                     ; 89A8 00                       .
L89A9:  brk                                     ; 89A9 00                       .
	brk                                     ; 89AA 00                       .
L89AB:  brk                                     ; 89AB 00                       .
L89AC:  brk                                     ; 89AC 00                       .
L89AD:  brk                                     ; 89AD 00                       .

L89AE:  prolog
	sta     L8961                           ; 89B1 8D 61 89                 .a.
	lda     L8961                           ; 89B4 AD 61 89                 .a.
	jsr     sub_7035
	lda     $A1                             ; 89BA A5 A1                    ..
	sta     L8963                           ; 89BC 8D 63 89                 .c.
	lda     $A0                             ; 89BF A5 A0                    ..
	sta     L8962                           ; 89C1 8D 62 89                 .b.
	lda     L8963                           ; 89C4 AD 63 89                 .c.
	sta     $A3                             ; 89C7 85 A3                    ..
	lda     #$00                            ; 89C9 A9 00                    ..
	sta     $A5                             ; 89CB 85 A5                    ..
	lda     #$0E                            ; 89CD A9 0E                    ..
	sta     $A4                             ; 89CF 85 A4                    ..
	ldy     L8962                           ; 89D1 AC 62 89                 .b.
	ldx     #$89                            ; 89D4 A2 89                    ..
	lda     #$97                            ; 89D6 A9 97                    ..
	jsr     sub_461F
	lda     L899B                           ; 89DB AD 9B 89                 ...
	jsr     L65B0                           ; 89DE 20 B0 65                  .e
	lda     $A1                             ; 89E1 A5 A1                    ..
	sta     L898E                           ; 89E3 8D 8E 89                 ...
	lda     $A0                             ; 89E6 A5 A0                    ..
	sta     L898D                           ; 89E8 8D 8D 89                 ...
	clc                                     ; 89EB 18                       .
	lda     L8962                           ; 89EC AD 62 89                 .b.
	adc     #$1A                            ; 89EF 69 1A                    i.
	sta     L896B                           ; 89F1 8D 6B 89                 .k.
	lda     L8963                           ; 89F4 AD 63 89                 .c.
	adc     #$00                            ; 89F7 69 00                    i.
	sta     L896C                           ; 89F9 8D 6C 89                 .l.
	lda     L896C                           ; 89FC AD 6C 89                 .l.
	sta     $A3                             ; 89FF 85 A3                    ..
	lda     #$00                            ; 8A01 A9 00                    ..
	sta     $A5                             ; 8A03 85 A5                    ..
	lda     #$04                            ; 8A05 A9 04                    ..
	sta     $A4                             ; 8A07 85 A4                    ..
	ldy     L896B                           ; 8A09 AC 6B 89                 .k.
	ldx     #$89                            ; 8A0C A2 89                    ..
	lda     #$A5                            ; 8A0E A9 A5                    ..
	jsr     sub_461F
	sec                                     ; 8A13 38                       8
	lda     L89A7                           ; 8A14 AD A7 89                 ...
	sbc     L89A5                           ; 8A17 ED A5 89                 ...
	sta     $AE                             ; 8A1A 85 AE                    ..
	clc                                     ; 8A1C 18                       .
	lda     $AE                             ; 8A1D A5 AE                    ..
	adc     #$01                            ; 8A1F 69 01                    i.
	sta     L8968                           ; 8A21 8D 68 89                 .h.
	clc                                     ; 8A24 18                       .
	lda     L89A6                           ; 8A25 AD A6 89                 ...
	adc     L8998                           ; 8A28 6D 98 89                 m..
	sta     $A0                             ; 8A2B 85 A0                    ..
	lda     $A0                             ; 8A2D A5 A0                    ..
	jsr     L4945                           ; 8A2F 20 45 49                  EI
	lda     $A1                             ; 8A32 A5 A1                    ..
	sta     L8996                           ; 8A34 8D 96 89                 ...
	lda     $A0                             ; 8A37 A5 A0                    ..
	sta     L8995                           ; 8A39 8D 95 89                 ...
	clc                                     ; 8A3C 18                       .
	lda     L89A5                           ; 8A3D AD A5 89                 ...
	adc     L8997                           ; 8A40 6D 97 89                 m..
	sta     $A0                             ; 8A43 85 A0                    ..
	lda     $A0                             ; 8A45 A5 A0                    ..
	jsr     L4945                           ; 8A47 20 45 49                  EI
	lda     $A1                             ; 8A4A A5 A1                    ..
	sta     L8994                           ; 8A4C 8D 94 89                 ...
	lda     $A0                             ; 8A4F A5 A0                    ..
	sta     L8993                           ; 8A51 8D 93 89                 ...
	lda     #$00                            ; 8A54 A9 00                    ..
	sta     $85                             ; 8A56 85 85                    ..
	lda     #$28                            ; 8A58 A9 28                    .(
	sta     $84                             ; 8A5A 85 84                    ..
	lda     L8996                           ; 8A5C AD 96 89                 ...
	tax                                     ; 8A5F AA                       .
	lda     L8995                           ; 8A60 AD 95 89                 ...
	jsr     sub_444A
	sta     $AE                             ; 8A66 85 AE                    ..
	txa                                     ; 8A68 8A                       .
	sta     $AF                             ; 8A69 85 AF                    ..
	clc                                     ; 8A6B 18                       .
	lda     L466D                           ; 8A6C AD 6D 46                 .mF
	adc     $AE                             ; 8A6F 65 AE                    e.
	sta     $AC                             ; 8A71 85 AC                    ..
	lda     L466E                           ; 8A73 AD 6E 46                 .nF
	adc     $AF                             ; 8A76 65 AF                    e.
	sta     $AD                             ; 8A78 85 AD                    ..
	clc                                     ; 8A7A 18                       .
	lda     $AC                             ; 8A7B A5 AC                    ..
	adc     L8993                           ; 8A7D 6D 93 89                 m..
	sta     L8964                           ; 8A80 8D 64 89                 .d.
	lda     $AD                             ; 8A83 A5 AD                    ..
	adc     L8994                           ; 8A85 6D 94 89                 m..
	sta     L8965                           ; 8A88 8D 65 89                 .e.
	ldy     #$01                            ; 8A8B A0 01                    ..
	sty     L8992                           ; 8A8D 8C 92 89                 ...
	sec                                     ; 8A90 38                       8
	lda     L89A8                           ; 8A91 AD A8 89                 ...
	sbc     L89A6                           ; 8A94 ED A6 89                 ...
	sta     $AE                             ; 8A97 85 AE                    ..
	clc                                     ; 8A99 18                       .
	lda     $AE                             ; 8A9A A5 AE                    ..
	adc     #$01                            ; 8A9C 69 01                    i.
	sta     L8AAC                           ; 8A9E 8D AC 8A                 ...
L8AA1:  lda     L8AAC                           ; 8AA1 AD AC 8A                 ...
	cmp     L8992                           ; 8AA4 CD 92 89                 ...
	bcs     L8AAD                           ; 8AA7 B0 04                    ..
	jmp     L8AD9                           ; 8AA9 4C D9 8A                 L..

; ----------------------------------------------------------------------------
L8AAC:  brk                                     ; 8AAC 00                       .
L8AAD:  lda     #$00                            ; 8AAD A9 00                    ..
	sta     $A3                             ; 8AAF 85 A3                    ..
	lda     L89A4                           ; 8AB1 AD A4 89                 ...
	sta     $A4                             ; 8AB4 85 A4                    ..
	ldy     L8968                           ; 8AB6 AC 68 89                 .h.
	ldx     L8965                           ; 8AB9 AE 65 89                 .e.
	lda     L8964                           ; 8ABC AD 64 89                 .d.
	jsr     L45FC                           ; 8ABF 20 FC 45                  .E
	clc                                     ; 8AC2 18                       .
	lda     L8964                           ; 8AC3 AD 64 89                 .d.
	adc     #$28                            ; 8AC6 69 28                    i(
	sta     L8964                           ; 8AC8 8D 64 89                 .d.
	lda     L8965                           ; 8ACB AD 65 89                 .e.
	adc     #$00                            ; 8ACE 69 00                    i.
	sta     L8965                           ; 8AD0 8D 65 89                 .e.
	inc     L8992                           ; 8AD3 EE 92 89                 ...
	jmp     L8AA1                           ; 8AD6 4C A1 8A                 L..

; ----------------------------------------------------------------------------
L8AD9:  lda     L89A2                           ; 8AD9 AD A2 89                 ...
	eor     #$01                            ; 8ADC 49 01                    I.
	beq     L8AE3                           ; 8ADE F0 03                    ..
	jmp     L8B4E                           ; 8AE0 4C 4E 8B                 LN.

; ----------------------------------------------------------------------------
L8AE3:  clc                                     ; 8AE3 18                       .
	lda     L8962                           ; 8AE4 AD 62 89                 .b.
	adc     #$16                            ; 8AE7 69 16                    i.
	sta     $A0                             ; 8AE9 85 A0                    ..
	lda     L8963                           ; 8AEB AD 63 89                 .c.
	adc     #$00                            ; 8AEE 69 00                    i.
	sta     $A1                             ; 8AF0 85 A1                    ..
	lda     L896C                           ; 8AF2 AD 6C 89                 .l.
	sta     $A3                             ; 8AF5 85 A3                    ..
	lda     #$89                            ; 8AF7 A9 89                    ..
	sta     $A5                             ; 8AF9 85 A5                    ..
	lda     #$6D                            ; 8AFB A9 6D                    .m
	sta     $A4                             ; 8AFD 85 A4                    ..
	ldy     L896B                           ; 8AFF AC 6B 89                 .k.
	ldx     $A1                             ; 8B02 A6 A1                    ..
	lda     $A0                             ; 8B04 A5 A0                    ..
	jsr     L4CF5                           ; 8B06 20 F5 4C                  .L
	lda     #$89                            ; 8B09 A9 89                    ..
	sta     $A3                             ; 8B0B 85 A3                    ..
	lda     L8997                           ; 8B0D AD 97 89                 ...
	sta     $A4                             ; 8B10 85 A4                    ..
	lda     L8998                           ; 8B12 AD 98 89                 ...
	sta     $A5                             ; 8B15 85 A5                    ..
	ldy     #$6D                            ; 8B17 A0 6D                    .m
	ldx     #$89                            ; 8B19 A2 89                    ..
	lda     #$7D                            ; 8B1B A9 7D                    .}
	jsr     L4C1D                           ; 8B1D 20 1D 4C                  .L
	lda     #$89                            ; 8B20 A9 89                    ..
	sta     $A3                             ; 8B22 85 A3                    ..
	lda     L8999                           ; 8B24 AD 99 89                 ...
	sta     $A4                             ; 8B27 85 A4                    ..
	lda     L899A                           ; 8B29 AD 9A 89                 ...
	sta     $A5                             ; 8B2C 85 A5                    ..
	ldy     #$6D                            ; 8B2E A0 6D                    .m
	ldx     #$89                            ; 8B30 A2 89                    ..
	lda     #$79                            ; 8B32 A9 79                    .y
	jsr     L4C1D                           ; 8B34 20 1D 4C                  .L
	lda     #$89                            ; 8B37 A9 89                    ..
	sta     $A3                             ; 8B39 85 A3                    ..
	lda     #$89                            ; 8B3B A9 89                    ..
	sta     $A5                             ; 8B3D 85 A5                    ..
	lda     #$7D                            ; 8B3F A9 7D                    .}
	sta     $A4                             ; 8B41 85 A4                    ..
	ldy     #$79                            ; 8B43 A0 79                    .y
	ldx     L898E                           ; 8B45 AE 8E 89                 ...
	lda     L898D                           ; 8B48 AD 8D 89                 ...
	jsr     L884C                           ; 8B4B 20 4C 88                  L.
L8B4E:  clc                                     ; 8B4E 18                       .
	lda     L898D                           ; 8B4F AD 8D 89                 ...
	adc     #$07                            ; 8B52 69 07                    i.
	sta     $AE                             ; 8B54 85 AE                    ..
	lda     L898E                           ; 8B56 AD 8E 89                 ...
	adc     #$00                            ; 8B59 69 00                    i.
	sta     $AF                             ; 8B5B 85 AF                    ..
	ldy     #$01                            ; 8B5D A0 01                    ..
	lda     ($AE),y                         ; 8B5F B1 AE                    ..
	sta     L896A                           ; 8B61 8D 6A 89                 .j.
	dey                                     ; 8B64 88                       .
	lda     ($AE),y                         ; 8B65 B1 AE                    ..
	sta     L8969                           ; 8B67 8D 69 89                 .i.
	lda     #$00                            ; 8B6A A9 00                    ..
	sta     $A3                             ; 8B6C 85 A3                    ..
	sec                                     ; 8B6E 38                       8
	lda     L899C                           ; 8B6F AD 9C 89                 ...
	sbc     #$01                            ; 8B72 E9 01                    ..
	sta     $A4                             ; 8B74 85 A4                    ..
	sec                                     ; 8B76 38                       8
	lda     L899D                           ; 8B77 AD 9D 89                 ...
	sbc     #$01                            ; 8B7A E9 01                    ..
	sta     $A5                             ; 8B7C 85 A5                    ..
	ldy     #$00                            ; 8B7E A0 00                    ..
	ldx     #$89                            ; 8B80 A2 89                    ..
	lda     #$71                            ; 8B82 A9 71                    .q
	jsr     sub_4BF2
	ldy     #$01                            ; 8B87 A0 01                    ..
	sty     L8992                           ; 8B89 8C 92 89                 ...
	clc                                     ; 8B8C 18                       .
	lda     L898D                           ; 8B8D AD 8D 89                 ...
	adc     #$06                            ; 8B90 69 06                    i.
	sta     $AE                             ; 8B92 85 AE                    ..
	lda     L898E                           ; 8B94 AD 8E 89                 ...
	adc     #$00                            ; 8B97 69 00                    i.
	sta     $AF                             ; 8B99 85 AF                    ..
	dey                                     ; 8B9B 88                       .
	lda     ($AE),y                         ; 8B9C B1 AE                    ..
	sta     L8BAC                           ; 8B9E 8D AC 8B                 ...
L8BA1:  lda     L8BAC                           ; 8BA1 AD AC 8B                 ...
	cmp     L8992                           ; 8BA4 CD 92 89                 ...
	bcs     L8BAD                           ; 8BA7 B0 04                    ..
	jmp     L8CD5                           ; 8BA9 4C D5 8C                 L..

; ----------------------------------------------------------------------------
L8BAC:  brk                                     ; 8BAC 00                       .
L8BAD:  lda     L896A                           ; 8BAD AD 6A 89                 .j.
	sta     $A3                             ; 8BB0 85 A3                    ..
	lda     #$00                            ; 8BB2 A9 00                    ..
	sta     $A5                             ; 8BB4 85 A5                    ..
	lda     #$05                            ; 8BB6 A9 05                    ..
	sta     $A4                             ; 8BB8 85 A4                    ..
	ldy     L8969                           ; 8BBA AC 69 89                 .i.
	ldx     #$89                            ; 8BBD A2 89                    ..
	lda     #$A9                            ; 8BBF A9 A9                    ..
	jsr     sub_461F
	lda     L89AD                           ; 8BC4 AD AD 89                 ...
	eor     #$FF                            ; 8BC7 49 FF                    I.
	beq     L8BCE                           ; 8BC9 F0 03                    ..
	jmp     L8BD1                           ; 8BCB 4C D1 8B                 L..

; ----------------------------------------------------------------------------
L8BCE:  jmp     L8CD5                           ; 8BCE 4C D5 8C                 L..

; ----------------------------------------------------------------------------
L8BD1:  lda     L89A9                           ; 8BD1 AD A9 89                 ...
	jsr     L65B0                           ; 8BD4 20 B0 65                  .e
	lda     $A1                             ; 8BD7 A5 A1                    ..
	sta     L8990                           ; 8BD9 8D 90 89                 ...
	lda     $A0                             ; 8BDC A5 A0                    ..
	sta     L898F                           ; 8BDE 8D 8F 89                 ...
	lda     #$00                            ; 8BE1 A9 00                    ..
	sta     $A3                             ; 8BE3 85 A3                    ..
	lda     L898F                           ; 8BE5 AD 8F 89                 ...
	sta     $AE                             ; 8BE8 85 AE                    ..
	lda     L8990                           ; 8BEA AD 90 89                 ...
	sta     $AF                             ; 8BED 85 AF                    ..
	sec                                     ; 8BEF 38                       8
	ldy     #$00                            ; 8BF0 A0 00                    ..
	lda     ($AE),y                         ; 8BF2 B1 AE                    ..
	sbc     #$01                            ; 8BF4 E9 01                    ..
	sta     $A4                             ; 8BF6 85 A4                    ..
	clc                                     ; 8BF8 18                       .
	lda     L898F                           ; 8BF9 AD 8F 89                 ...
	adc     #$01                            ; 8BFC 69 01                    i.
	sta     $AE                             ; 8BFE 85 AE                    ..
	lda     L8990                           ; 8C00 AD 90 89                 ...
	adc     #$00                            ; 8C03 69 00                    i.
	sta     $AF                             ; 8C05 85 AF                    ..
	sec                                     ; 8C07 38                       8
	lda     ($AE),y                         ; 8C08 B1 AE                    ..
	sbc     #$01                            ; 8C0A E9 01                    ..
	sta     $A5                             ; 8C0C 85 A5                    ..
	ldy     #$00                            ; 8C0E A0 00                    ..
	ldx     #$89                            ; 8C10 A2 89                    ..
	lda     #$85                            ; 8C12 A9 85                    ..
	jsr     sub_4BF2
	lda     #$89                            ; 8C17 A9 89                    ..
	sta     $A3                             ; 8C19 85 A3                    ..
	sec                                     ; 8C1B 38                       8
	lda     L89AB                           ; 8C1C AD AB 89                 ...
	sbc     L8999                           ; 8C1F ED 99 89                 ...
	sta     $A4                             ; 8C22 85 A4                    ..
	sec                                     ; 8C24 38                       8
	lda     L89AC                           ; 8C25 AD AC 89                 ...
	sbc     L899A                           ; 8C28 ED 9A 89                 ...
	sta     $A5                             ; 8C2B 85 A5                    ..
	ldy     #$85                            ; 8C2D A0 85                    ..
	ldx     #$89                            ; 8C2F A2 89                    ..
	lda     #$85                            ; 8C31 A9 85                    ..
	jsr     L4C1D                           ; 8C33 20 1D 4C                  .L
	lda     #$89                            ; 8C36 A9 89                    ..
	sta     $A3                             ; 8C38 85 A3                    ..
	lda     #$89                            ; 8C3A A9 89                    ..
	sta     $A5                             ; 8C3C 85 A5                    ..
	lda     #$89                            ; 8C3E A9 89                    ..
	sta     $A4                             ; 8C40 85 A4                    ..
	ldy     #$71                            ; 8C42 A0 71                    .q
	ldx     #$89                            ; 8C44 A2 89                    ..
	lda     #$85                            ; 8C46 A9 85                    ..
	jsr     L4CF5                           ; 8C48 20 F5 4C                  .L
	lda     $A0                             ; 8C4B A5 A0                    ..
	sta     L8991                           ; 8C4D 8D 91 89                 ...
	lda     L8991                           ; 8C50 AD 91 89                 ...
	eor     #$01                            ; 8C53 49 01                    I.
	beq     L8C5A                           ; 8C55 F0 03                    ..
	jmp     L8CBE                           ; 8C57 4C BE 8C                 L..

; ----------------------------------------------------------------------------
L8C5A:  lda     L896C                           ; 8C5A AD 6C 89                 .l.
	sta     $A3                             ; 8C5D 85 A3                    ..
	lda     #$89                            ; 8C5F A9 89                    ..
	sta     $A5                             ; 8C61 85 A5                    ..
	lda     #$79                            ; 8C63 A9 79                    .y
	sta     $A4                             ; 8C65 85 A4                    ..
	ldy     L896B                           ; 8C67 AC 6B 89                 .k.
	ldx     #$89                            ; 8C6A A2 89                    ..
	lda     #$89                            ; 8C6C A9 89                    ..
	jsr     L4CF5                           ; 8C6E 20 F5 4C                  .L
	lda     #$89                            ; 8C71 A9 89                    ..
	sta     $A3                             ; 8C73 85 A3                    ..
	lda     L8997                           ; 8C75 AD 97 89                 ...
	sta     $A4                             ; 8C78 85 A4                    ..
	lda     L8998                           ; 8C7A AD 98 89                 ...
	sta     $A5                             ; 8C7D 85 A5                    ..
	ldy     #$79                            ; 8C7F A0 79                    .y
	ldx     #$89                            ; 8C81 A2 89                    ..
	lda     #$75                            ; 8C83 A9 75                    .u
	jsr     L4C1D                           ; 8C85 20 1D 4C                  .L
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
	ldx     #$89                            ; 8CA0 A2 89                    ..
	lda     #$81                            ; 8CA2 A9 81                    ..
	jsr     L4C1D                           ; 8CA4 20 1D 4C                  .L
	lda     #$89                            ; 8CA7 A9 89                    ..
	sta     $A3                             ; 8CA9 85 A3                    ..
	lda     #$89                            ; 8CAB A9 89                    ..
	sta     $A5                             ; 8CAD 85 A5                    ..
	lda     #$75                            ; 8CAF A9 75                    .u
	sta     $A4                             ; 8CB1 85 A4                    ..
	ldy     #$81                            ; 8CB3 A0 81                    ..
	ldx     L8990                           ; 8CB5 AE 90 89                 ...
	lda     L898F                           ; 8CB8 AD 8F 89                 ...
	jsr     L884C                           ; 8CBB 20 4C 88                  L.
L8CBE:  clc                                     ; 8CBE 18                       .
	lda     L8969                           ; 8CBF AD 69 89                 .i.
	adc     #$06                            ; 8CC2 69 06                    i.
	sta     L8969                           ; 8CC4 8D 69 89                 .i.
	lda     L896A                           ; 8CC7 AD 6A 89                 .j.
	adc     #$00                            ; 8CCA 69 00                    i.
	sta     L896A                           ; 8CCC 8D 6A 89                 .j.
	inc     L8992                           ; 8CCF EE 92 89                 ...
	jmp     L8BA1                           ; 8CD2 4C A1 8B                 L..

; ----------------------------------------------------------------------------
L8CD5:  lda     L896C                           ; 8CD5 AD 6C 89                 .l.
	sta     $A3                             ; 8CD8 85 A3                    ..
	lda     L8997                           ; 8CDA AD 97 89                 ...
	sta     $A4                             ; 8CDD 85 A4                    ..
	lda     L8998                           ; 8CDF AD 98 89                 ...
	sta     $A5                             ; 8CE2 85 A5                    ..
	ldy     L896B                           ; 8CE4 AC 6B 89                 .k.
	ldx     #$89                            ; 8CE7 A2 89                    ..
	lda     #$7D                            ; 8CE9 A9 7D                    .}
	jsr     L4C1D                           ; 8CEB 20 1D 4C                  .L
	ldy     #$89                            ; 8CEE A0 89                    ..
	ldx     #$7D                            ; 8CF0 A2 7D                    .}
	lda     L8961                           ; 8CF2 AD 61 89                 .a.
	jsr     L8573                           ; 8CF5 20 73 85                  s.
	rts                                     ; 8CF8 60                       `

; ----------------------------------------------------------------------------
L8CF9:  brk                                     ; 8CF9 00                       .
L8CFA:  brk                                     ; 8CFA 00                       .
L8CFB:  brk                                     ; 8CFB 00                       .
L8CFC:  brk                                     ; 8CFC 00                       .
L8CFD:  brk                                     ; 8CFD 00                       .
L8CFE:  brk                                     ; 8CFE 00                       .
L8CFF:  brk                                     ; 8CFF 00                       .
L8D00:  brk                                     ; 8D00 00                       .
L8D01:  .byte   $4C                             ; 8D01 4C                       L
L8D02:  .byte   $04                             ; 8D02 04                       .
L8D03:  .byte   $8D                             ; 8D03 8D                       .
L8D04:  lda     L4658                           ; 8D04 AD 58 46                 .XF
	beq     L8D0C                           ; 8D07 F0 03                    ..
	jmp     L8D0D                           ; 8D09 4C 0D 8D                 L..

; ----------------------------------------------------------------------------
L8D0C:  rts                                     ; 8D0C 60                       `

; ----------------------------------------------------------------------------
L8D0D:  lda     L4656                           ; 8D0D AD 56 46                 .VF
	eor     #$01                            ; 8D10 49 01                    I.
	lbne	L8E06
	lda     #$01                            ; 8D17 A9 01                    ..
	asl     a                               ; 8D19 0A                       .
	php                                     ; 8D1A 08                       .
	clc                                     ; 8D1B 18                       .
	adc     L466F                           ; 8D1C 6D 6F 46                 moF
	sta     $AE                             ; 8D1F 85 AE                    ..
	lda     #$00                            ; 8D21 A9 00                    ..
	rol     a                               ; 8D23 2A                       *
	plp                                     ; 8D24 28                       (
	adc     L466F+1
	sta     $AF                             ; 8D28 85 AF                    ..
	ldy     #$01                            ; 8D2A A0 01                    ..
	lda     ($AE),y                         ; 8D2C B1 AE                    ..
	sta     L8D00                           ; 8D2E 8D 00 8D                 ...
	dey                                     ; 8D31 88                       .
	lda     ($AE),y                         ; 8D32 B1 AE                    ..
	sta     L8CFF                           ; 8D34 8D FF 8C                 ...
	clc                                     ; 8D37 18                       .
	lda     L8CFF                           ; 8D38 AD FF 8C                 ...
	adc     #$03                            ; 8D3B 69 03                    i.
	sta     $AE                             ; 8D3D 85 AE                    ..
	lda     L8D00                           ; 8D3F AD 00 8D                 ...
	.byte   $69                             ; 8D42 69                       i
L8D43:  brk                                     ; 8D43 00                       .
	sta     $AF                             ; 8D44 85 AF                    ..
	clc                                     ; 8D46 18                       .
	lda     $AE                             ; 8D47 A5 AE                    ..
	adc     #$01                            ; 8D49 69 01                    i.
	.byte   $8D                             ; 8D4B 8D                       .
L8D4C:  sbc     LA58C,x                         ; 8D4C FD 8C A5                 ...
	.byte   $AF                             ; 8D4F AF                       .
	adc     #$00                            ; 8D50 69 00                    i.
	sta     L8CFE                           ; 8D52 8D FE 8C                 ...
	lda     L8CFD                           ; 8D55 AD FD 8C                 ...
	sta     $AE                             ; 8D58 85 AE                    ..
	lda     L8CFE                           ; 8D5A AD FE 8C                 ...
	sta     $AF                             ; 8D5D 85 AF                    ..
	iny                                     ; 8D5F C8                       .
	lda     ($AE),y                         ; 8D60 B1 AE                    ..
	sta     L466E                           ; 8D62 8D 6E 46                 .nF
	dey                                     ; 8D65 88                       .
	lda     ($AE),y                         ; 8D66 B1 AE                    ..
	sta     L466D                           ; 8D68 8D 6D 46                 .mF
	lda     #$03                            ; 8D6B A9 03                    ..
	sta     $A3                             ; 8D6D 85 A3                    ..
	lda     #$00                            ; 8D6F A9 00                    ..
	sta     $A4                             ; 8D71 85 A4                    ..
	ldy     #$C0                            ; 8D73 A0 C0                    ..
	ldx     L466E                           ; 8D75 AE 6E 46                 .nF
	lda     L466D                           ; 8D78 AD 6D 46                 .mF
	jsr     L45FC                           ; 8D7B 20 FC 45                  .E
	ldy     #$01                            ; 8D7E A0 01                    ..
	sty     L8CF9                           ; 8D80 8C F9 8C                 ...
	lda     L4673                           ; 8D83 AD 73 46                 .sF
	sta     L8D94                           ; 8D86 8D 94 8D                 ...
L8D89:  lda     L8D94                           ; 8D89 AD 94 8D                 ...
	cmp     L8CF9                           ; 8D8C CD F9 8C                 ...
	bcs     L8D95                           ; 8D8F B0 04                    ..
	jmp     L8DFE                           ; 8D91 4C FE 8D                 L..

; ----------------------------------------------------------------------------
L8D94:  brk                                     ; 8D94 00                       .
L8D95:  sec                                     ; 8D95 38                       8
	lda     L8CF9                           ; 8D96 AD F9 8C                 ...
	sbc     #$01                            ; 8D99 E9 01                    ..
	sta     $AE                             ; 8D9B 85 AE                    ..
	ldx     $AE                             ; 8D9D A6 AE                    ..
	lda     L4659,x                         ; 8D9F BD 59 46                 .YF
	sta     L8CFA                           ; 8DA2 8D FA 8C                 ...
	lda     L8CFA                           ; 8DA5 AD FA 8C                 ...
	jsr     sub_7035
	lda     $A1                             ; 8DAB A5 A1                    ..
	sta     L8CFC                           ; 8DAD 8D FC 8C                 ...
	lda     $A0                             ; 8DB0 A5 A0                    ..
	sta     L8CFB                           ; 8DB2 8D FB 8C                 ...
	clc                                     ; 8DB5 18                       .
	lda     L8CFB                           ; 8DB6 AD FB 8C                 ...
	adc     #$0C                            ; 8DB9 69 0C                    i.
	sta     $AE                             ; 8DBB 85 AE                    ..
	lda     L8CFC                           ; 8DBD AD FC 8C                 ...
	adc     #$00                            ; 8DC0 69 00                    i.
	sta     $AF                             ; 8DC2 85 AF                    ..
	ldy     #$00                            ; 8DC4 A0 00                    ..
	lda     ($AE),y                         ; 8DC6 B1 AE                    ..
	eor     #$FF                            ; 8DC8 49 FF                    I.
	bne     L8DCF                           ; 8DCA D0 03                    ..
	jmp     L8DF8                           ; 8DCC 4C F8 8D                 L..

; ----------------------------------------------------------------------------
L8DCF:  clc                                     ; 8DCF 18                       .
	lda     L8CFB                           ; 8DD0 AD FB 8C                 ...
	adc     #$0A                            ; 8DD3 69 0A                    i.
	sta     $AE                             ; 8DD5 85 AE                    ..
	lda     L8CFC                           ; 8DD7 AD FC 8C                 ...
	adc     #$00                            ; 8DDA 69 00                    i.
	sta     $AF                             ; 8DDC 85 AF                    ..
	lda     ($AE),y                         ; 8DDE B1 AE                    ..
	eor     #$01                            ; 8DE0 49 01                    I.
	beq     L8DE7                           ; 8DE2 F0 03                    ..
	jmp     L8DF8                           ; 8DE4 4C F8 8D                 L..

; ----------------------------------------------------------------------------
L8DE7:  lda     L8CFB                           ; 8DE7 AD FB 8C                 ...
	ora     L8CFC                           ; 8DEA 0D FC 8C                 ...
	bne     L8DF2                           ; 8DED D0 03                    ..
	jmp     L8DF8                           ; 8DEF 4C F8 8D                 L..

; ----------------------------------------------------------------------------
L8DF2:  lda     L8CFA                           ; 8DF2 AD FA 8C                 ...
	jsr     L89AE                           ; 8DF5 20 AE 89                  ..
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
L8E10:  ldx     L4763                           ; 8E10 AE 63 47                 .cG
	lda     L4762                           ; 8E13 AD 62 47                 .bG
	jsr     L8047                           ; 8E16 20 47 80                  G.
	ldy     #$00                            ; 8E19 A0 00                    ..
	sty     L4657                           ; 8E1B 8C 57 46                 .WF
L8E1E:  rts                                     ; 8E1E 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 8E1F 00                       .
L8E20:  brk                                     ; 8E20 00                       .
L8E21:  brk                                     ; 8E21 00                       .
L8E22:  brk                                     ; 8E22 00                       .
L8E23:  brk                                     ; 8E23 00                       .

L8E24:	prolog
	jsr     sub_44D5                        ; 8E27 20 D5 44                  .D
	.byte   $1F                             ; 8E2A 1F                       .
	stx     LAD02                           ; 8E2B 8E 02 AD                 ...
	.byte   $1F                             ; 8E2E 1F                       .
	stx     L7F29                           ; 8E2F 8E 29 7F                 .).
	sta     L8E22                           ; 8E32 8D 22 8E                 .".
	lda     #$00                            ; 8E35 A9 00                    ..
	sta     L8E23                           ; 8E37 8D 23 8E                 .#.
	lda     #$03                            ; 8E3A A9 03                    ..
	sta     $84                             ; 8E3C 85 84                    ..
	lda     L8E23                           ; 8E3E AD 23 8E                 .#.
	tax                                     ; 8E41 AA                       .
	lda     L8E22                           ; 8E42 AD 22 8E                 .".
	jsr     L43D1                           ; 8E45 20 D1 43                  .C
	sta     $AE                             ; 8E48 85 AE                    ..
	txa                                     ; 8E4A 8A                       .
	sta     $AF                             ; 8E4B 85 AF                    ..
	clc                                     ; 8E4D 18                       .
	lda     L4674                           ; 8E4E AD 74 46                 .tF
	adc     $AE                             ; 8E51 65 AE                    e.
	sta     $A0                             ; 8E53 85 A0                    ..
	lda     L4675                           ; 8E55 AD 75 46                 .uF
	adc     $AF                             ; 8E58 65 AF                    e.
	sta     $A1                             ; 8E5A 85 A1                    ..
	lda     L8E21                           ; 8E5C AD 21 8E                 .!.
	sta     $A3                             ; 8E5F 85 A3                    ..
	lda     #$00                            ; 8E61 A9 00                    ..
	sta     $A5                             ; 8E63 85 A5                    ..
	lda     #$08                            ; 8E65 A9 08                    ..
	sta     $A4                             ; 8E67 85 A4                    ..
	ldy     L8E20                           ; 8E69 AC 20 8E                 . .
	ldxa	$A0
	jsr     sub_461F
	rts                                     ; 8E73 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 8E74 00                       .
L8E75:  brk                                     ; 8E75 00                       .
	brk                                     ; 8E76 00                       .
	brk                                     ; 8E77 00                       .
	brk                                     ; 8E78 00                       .
	brk                                     ; 8E79 00                       .
	brk                                     ; 8E7A 00                       .
L8E7B:  brk                                     ; 8E7B 00                       .
L8E7C:  brk                                     ; 8E7C 00                       .
L8E7D:  jmp     L8E80                           ; 8E7D 4C 80 8E                 L..

; ----------------------------------------------------------------------------
L8E80:  jsr     sub_44D5                           ; 8E80 20 D5 44                  .D
	.byte   $74                             ; 8E83 74                       t
	stx     LAD06                           ; 8E84 8E 06 AD                 ...
	.byte   $74                             ; 8E87 74                       t
	stx     $080A                           ; 8E88 8E 0A 08                 ...
	clc                                     ; 8E8B 18                       .
	adc     L46F5                           ; 8E8C 6D F5 46                 m.F
	sta     $AE                             ; 8E8F 85 AE                    ..
	lda     #$00                            ; 8E91 A9 00                    ..
	rol     a                               ; 8E93 2A                       *
	plp                                     ; 8E94 28                       (
	adc     L46F6                           ; 8E95 6D F6 46                 m.F
	sta     $AF                             ; 8E98 85 AF                    ..
	ldy     #$01                            ; 8E9A A0 01                    ..
	lda     ($AE),y                         ; 8E9C B1 AE                    ..
	sta     L8E7C                           ; 8E9E 8D 7C 8E                 .|.
	dey                                     ; 8EA1 88                       .
	lda     ($AE),y                         ; 8EA2 B1 AE                    ..
	sta     L8E7B                           ; 8EA4 8D 7B 8E                 .{.
	clc                                     ; 8EA7 18                       .
	lda     L8E7B                           ; 8EA8 AD 7B 8E                 .{.
	adc     #$02                            ; 8EAB 69 02                    i.
	sta     $AE                             ; 8EAD 85 AE                    ..
	lda     L8E7C                           ; 8EAF AD 7C 8E                 .|.
	adc     #$00                            ; 8EB2 69 00                    i.
	sta     $AF                             ; 8EB4 85 AF                    ..
	clc                                     ; 8EB6 18                       .
	lda     L4678                           ; 8EB7 AD 78 46                 .xF
	adc     L8E75                           ; 8EBA 6D 75 8E                 mu.
	sta     $AC                             ; 8EBD 85 AC                    ..
	lda     L4679                           ; 8EBF AD 79 46                 .yF
	adc     #$00                            ; 8EC2 69 00                    i.
	sta     $AD                             ; 8EC4 85 AD                    ..
	lda     ($AC),y                         ; 8EC6 B1 AC                    ..
	sta     ($AE),y                         ; 8EC8 91 AE                    ..
	jsr     sub_63DD
	jsr     L62D1                           ; 8ECD 20 D1 62                  .b
	clc                                     ; 8ED0 18                       .
	lda     L8E7B                           ; 8ED1 AD 7B 8E                 .{.
	adc     #$03                            ; 8ED4 69 03                    i.
	sta     $A0                             ; 8ED6 85 A0                    ..
	lda     L8E7C                           ; 8ED8 AD 7C 8E                 .|.
	adc     #$00                            ; 8EDB 69 00                    i.
	sta     $A1                             ; 8EDD 85 A1                    ..
	lda     #$8E                            ; 8EDF A9 8E                    ..
	sta     $A3                             ; 8EE1 85 A3                    ..
	lda     #$00                            ; 8EE3 A9 00                    ..
	sta     $A5                             ; 8EE5 85 A5                    ..
	lda     #$05                            ; 8EE7 A9 05                    ..
	sta     $A4                             ; 8EE9 85 A4                    ..
	ldy     #$76                            ; 8EEB A0 76                    .v
	ldxa	$A0
	jsr     sub_461F
	jsr     L636E                           ; 8EF4 20 6E 63                  nc
	rts                                     ; 8EF7 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 8EF8 00                       .
L8EF9:  brk                                     ; 8EF9 00                       .
L8EFA:  brk                                     ; 8EFA 00                       .
L8EFB:  brk                                     ; 8EFB 00                       .
L8EFC:  brk                                     ; 8EFC 00                       .
L8EFD:  jmp     L8F00                           ; 8EFD 4C 00 8F                 L..

; ----------------------------------------------------------------------------
L8F00:  jsr     sub_44D5                           ; 8F00 20 D5 44                  .D
	sed                                     ; 8F03 F8                       .
	stx     LAD02                           ; 8F04 8E 02 AD                 ...
	sed                                     ; 8F07 F8                       .
	stx     $080A                           ; 8F08 8E 0A 08                 ...
	clc                                     ; 8F0B 18                       .
	adc     L46F5                           ; 8F0C 6D F5 46                 m.F
	sta     $AE                             ; 8F0F 85 AE                    ..
	lda     #$00                            ; 8F11 A9 00                    ..
	rol     a                               ; 8F13 2A                       *
	plp                                     ; 8F14 28                       (
	adc     L46F6                           ; 8F15 6D F6 46                 m.F
	sta     $AF                             ; 8F18 85 AF                    ..
	ldy     #$01                            ; 8F1A A0 01                    ..
	lda     ($AE),y                         ; 8F1C B1 AE                    ..
	sta     L8EFC                           ; 8F1E 8D FC 8E                 ...
	dey                                     ; 8F21 88                       .
	lda     ($AE),y                         ; 8F22 B1 AE                    ..
	sta     L8EFB                           ; 8F24 8D FB 8E                 ...
	lda     L8EFB                           ; 8F27 AD FB 8E                 ...
	sta     $AE                             ; 8F2A 85 AE                    ..
	lda     L8EFC                           ; 8F2C AD FC 8E                 ...
	sta     $AF                             ; 8F2F 85 AF                    ..
	lda     L8EF9                           ; 8F31 AD F9 8E                 ...
	sta     ($AE),y                         ; 8F34 91 AE                    ..
	clc                                     ; 8F36 18                       .
	lda     L8EFB                           ; 8F37 AD FB 8E                 ...
	adc     #$01                            ; 8F3A 69 01                    i.
	sta     $AE                             ; 8F3C 85 AE                    ..
	lda     L8EFC                           ; 8F3E AD FC 8E                 ...
	adc     #$00                            ; 8F41 69 00                    i.
	sta     $AF                             ; 8F43 85 AF                    ..
	lda     L8EFA                           ; 8F45 AD FA 8E                 ...
	sta     ($AE),y                         ; 8F48 91 AE                    ..
	iny                                     ; 8F4A C8                       .
	.byte   $8C                             ; 8F4B 8C                       .
L8F4C:  lsr     $46,x                           ; 8F4C 56 46                    VF
	rts                                     ; 8F4E 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; 8F4F 00                       .
	brk                                     ; 8F50 00                       .
	brk                                     ; 8F51 00                       .
	brk                                     ; 8F52 00                       .
	brk                                     ; 8F53 00                       .
	brk                                     ; 8F54 00                       .
L8F55:  jmp     L8F58                           ; 8F55 4C 58 8F                 LX.

; ----------------------------------------------------------------------------
L8F58:  jsr     sub_44D5                           ; 8F58 20 D5 44                  .D
	.byte   $4F                             ; 8F5B 4F                       O
	.byte   $8F                             ; 8F5C 8F                       .
	ora     $A9                             ; 8F5D 05 A9                    ..
	.byte   $8F                             ; 8F5F 8F                       .
	sta     $A3                             ; 8F60 85 A3                    ..
	lda     #$00                            ; 8F62 A9 00                    ..
	sta     $A5                             ; 8F64 85 A5                    ..
	lda     #$06                            ; 8F66 A9 06                    ..
	sta     $A4                             ; 8F68 85 A4                    ..
	ldy     #$4F                            ; 8F6A A0 4F                    .O
	ldx     #$46                            ; 8F6C A2 46                    .F
	lda     #$EF                            ; 8F6E A9 EF                    ..
	jsr     sub_461F
	jsr     sub_63DD
	jsr     L62D1                           ; 8F76 20 D1 62                  .b
	rts                                     ; 8F79 60                       `

; ----------------------------------------------------------------------------
L8F7A:  brk                                     ; 8F7A 00                       .
L8F7B:  brk                                     ; 8F7B 00                       .
L8F7C:  brk                                     ; 8F7C 00                       .
L8F7D:  jmp     L8F80                           ; 8F7D 4C 80 8F                 L..

; ----------------------------------------------------------------------------
L8F80:  lda     #$00                            ; 8F80 A9 00                    ..
	sta     $A3                             ; 8F82 85 A3                    ..
	ldy     #$50                            ; 8F84 A0 50                    .P
	ldx     #$46                            ; 8F86 A2 46                    .F
	lda     #$F9                            ; 8F88 A9 F9                    ..
	jsr     L45F6                           ; 8F8A 20 F6 45                  .E
	lda     #$46                            ; 8F8D A9 46                    .F
	sta     L8F7C                           ; 8F8F 8D 7C 8F                 .|.
	lda     #$F9                            ; 8F92 A9 F9                    ..
	sta     L8F7B                           ; 8F94 8D 7B 8F                 .{.
	ldy     #$00                            ; 8F97 A0 00                    ..
	sty     L8F7A                           ; 8F99 8C 7A 8F                 .z.
L8F9C:  lda     #$09                            ; 8F9C A9 09                    ..
	cmp     L8F7A                           ; 8F9E CD 7A 8F                 .z.
	bcs     L8FA6                           ; 8FA1 B0 03                    ..
	jmp     L8FDE                           ; 8FA3 4C DE 8F                 L..

; ----------------------------------------------------------------------------
L8FA6:  lda     L8F7A                           ; 8FA6 AD 7A 8F                 .z.
	asl     a                               ; 8FA9 0A                       .
	php                                     ; 8FAA 08                       .
	clc                                     ; 8FAB 18                       .
	adc     L46F5                           ; 8FAC 6D F5 46                 m.F
	sta     $AE                             ; 8FAF 85 AE                    ..
	lda     #$00                            ; 8FB1 A9 00                    ..
	rol     a                               ; 8FB3 2A                       *
	plp                                     ; 8FB4 28                       (
	adc     L46F6                           ; 8FB5 6D F6 46                 m.F
	sta     $AF                             ; 8FB8 85 AF                    ..
	lda     L8F7C                           ; 8FBA AD 7C 8F                 .|.
	ldy     #$01                            ; 8FBD A0 01                    ..
	sta     ($AE),y                         ; 8FBF 91 AE                    ..
	lda     L8F7B                           ; 8FC1 AD 7B 8F                 .{.
	dey                                     ; 8FC4 88                       .
	sta     ($AE),y                         ; 8FC5 91 AE                    ..
	clc                                     ; 8FC7 18                       .
	lda     L8F7B                           ; 8FC8 AD 7B 8F                 .{.
	adc     #$08                            ; 8FCB 69 08                    i.
	sta     L8F7B                           ; 8FCD 8D 7B 8F                 .{.
	lda     L8F7C                           ; 8FD0 AD 7C 8F                 .|.
	adc     #$00                            ; 8FD3 69 00                    i.
	sta     L8F7C                           ; 8FD5 8D 7C 8F                 .|.
	inc     L8F7A                           ; 8FD8 EE 7A 8F                 .z.
	jmp     L8F9C                           ; 8FDB 4C 9C 8F                 L..

; ----------------------------------------------------------------------------
L8FDE:  lda     L4678                           ; 8FDE AD 78 46                 .xF
	sta     $AE                             ; 8FE1 85 AE                    ..
	lda     L4679                           ; 8FE3 AD 79 46                 .yF
	sta     $AF                             ; 8FE6 85 AF                    ..
	lda     #$E0                            ; 8FE8 A9 E0                    ..
	ldy     #$00                            ; 8FEA A0 00                    ..
	sta     ($AE),y                         ; 8FEC 91 AE                    ..
	clc                                     ; 8FEE 18                       .
	lda     L4678                           ; 8FEF AD 78 46                 .xF
	adc     #$01                            ; 8FF2 69 01                    i.
	sta     $AE                             ; 8FF4 85 AE                    ..
	lda     L4679                           ; 8FF6 AD 79 46                 .yF
	adc     #$00                            ; 8FF9 69 00                    i.
	sta     $AF                             ; 8FFB 85 AF                    ..
	lda     #$08                            ; 8FFD A9 08                    ..
	sta     $84                             ; 8FFF 85 84                    ..
	lda     L4675                           ; 9001 AD 75 46                 .uF
	tax                                     ; 9004 AA                       .
	lda     L4674                           ; 9005 AD 74 46                 .tF
	jsr     sub_43E0
	ldy     #$00                            ; 900B A0 00                    ..
	sta     ($AE),y                         ; 900D 91 AE                    ..
	lda     #$E0                            ; 900F A9 E0                    ..
	sta     $A3                             ; 9011 85 A3                    ..
	lda     #$04                            ; 9013 A9 04                    ..
	sta     $A5                             ; 9015 85 A5                    ..
	lda     #$00                            ; 9017 A9 00                    ..
	sta     $A4                             ; 9019 85 A4                    ..
	ldy     #$00                            ; 901B A0 00                    ..
	ldx     L4675                           ; 901D AE 75 46                 .uF
	lda     L4674                           ; 9020 AD 74 46                 .tF
	jsr     sub_461F
	ldy     #$18                            ; 9026 A0 18                    ..
	ldx     #$02                            ; 9028 A2 02                    ..
	lda     #$00                            ; 902A A9 00                    ..
	jsr     L8EFD                           ; 902C 20 FD 8E                  ..
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
	jsr     L8E7D                           ; 9045 20 7D 8E                  }.
	ldx     #$0F                            ; 9048 A2 0F                    ..
	lda     #$00                            ; 904A A9 00                    ..
	jsr     L8F55                           ; 904C 20 55 8F                  U.
	rts                                     ; 904F 60                       `

; ----------------------------------------------------------------------------
L9050:  brk                                     ; 9050 00                       .
L9051:  brk                                     ; 9051 00                       .
L9052:  brk                                     ; 9052 00                       .
L9053:  brk                                     ; 9053 00                       .
L9054:  brk                                     ; 9054 00                       .
L9055:  brk                                     ; 9055 00                       .
L9056:  brk                                     ; 9056 00                       .
L9057:  brk                                     ; 9057 00                       .
L9058:  brk                                     ; 9058 00                       .
L9059:  brk                                     ; 9059 00                       .
L905A:  brk                                     ; 905A 00                       .
L905B:  brk                                     ; 905B 00                       .
L905C:  brk                                     ; 905C 00                       .
L905D:  brk                                     ; 905D 00                       .
L905E:  brk                                     ; 905E 00                       .
L905F:  .byte   $01                             ; 905F 01                       .
L9060:  brk                                     ; 9060 00                       .
L9061:  brk                                     ; 9061 00                       .
	brk                                     ; 9062 00                       .
	brk                                     ; 9063 00                       .
L9064:  brk                                     ; 9064 00                       .
L9065:  brk                                     ; 9065 00                       .
L9066:  brk                                     ; 9066 00                       .
L9067:  brk                                     ; 9067 00                       .
L9068:  brk                                     ; 9068 00                       .
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
L907B:  .byte   $69                             ; 907B 69                       i
L907C:  .byte   $90                             ; 907C 90                       .
L907D:  jmp     L9080                           ; 907D 4C 80 90                 L..

; ----------------------------------------------------------------------------
L9080:  sta     L9066                           ; 9080 8D 66 90                 .f.
	ldy     #$00                            ; 9083 A0 00                    ..
	sty     L9067                           ; 9085 8C 67 90                 .g.
L9088:  clc                                     ; 9088 18                       .
	lda     L907B                           ; 9089 AD 7B 90                 .{.
	adc     L9067                           ; 908C 6D 67 90                 mg.
	sta     $AE                             ; 908F 85 AE                    ..
	lda     L907C                           ; 9091 AD 7C 90                 .|.
	adc     #$00                            ; 9094 69 00                    i.
	sta     $AF                             ; 9096 85 AF                    ..
	ldy     #$00                            ; 9098 A0 00                    ..
	lda     ($AE),y                         ; 909A B1 AE                    ..
	sta     L9068                           ; 909C 8D 68 90                 .h.
	lda     L9066                           ; 909F AD 66 90                 .f.
	eor     L9068                           ; 90A2 4D 68 90                 Mh.
	beq     L90AA                           ; 90A5 F0 03                    ..
	jmp     L90AF                           ; 90A7 4C AF 90                 L..

; ----------------------------------------------------------------------------
L90AA:  lda     #$01                            ; 90AA A9 01                    ..
	sta     $A0                             ; 90AC 85 A0                    ..
	rts                                     ; 90AE 60                       `

; ----------------------------------------------------------------------------
L90AF:  inc     L9067                           ; 90AF EE 67 90                 .g.
	lda     L9068                           ; 90B2 AD 68 90                 .h.
	eor     #$FF                            ; 90B5 49 FF                    I.
	beq     L90BC                           ; 90B7 F0 03                    ..
	jmp     L9088                           ; 90B9 4C 88 90                 L..

; ----------------------------------------------------------------------------
L90BC:  lda     #$00                            ; 90BC A9 00                    ..
	sta     $A0                             ; 90BE 85 A0                    ..
	rts                                     ; 90C0 60                       `

; ----------------------------------------------------------------------------
L90C1:  brk                                     ; 90C1 00                       .
L90C2:  brk                                     ; 90C2 00                       .
L90C3:  brk                                     ; 90C3 00                       .
L90C4:  brk                                     ; 90C4 00                       .
L90C5:  brk                                     ; 90C5 00                       .
L90C6:  brk                                     ; 90C6 00                       .
L90C7:  brk                                     ; 90C7 00                       .
L90C8:  brk                                     ; 90C8 00                       .
	brk                                     ; 90C9 00                       .
	brk                                     ; 90CA 00                       .
	brk                                     ; 90CB 00                       .
L90CC:  brk                                     ; 90CC 00                       .
L90CD:  brk                                     ; 90CD 00                       .
L90CE:  jmp     L90D1                           ; 90CE 4C D1 90                 L..

; ----------------------------------------------------------------------------
L90D1:  stx     L90C2                           ; 90D1 8E C2 90                 ...
	sta     L90C1                           ; 90D4 8D C1 90                 ...
	lda     L90C1                           ; 90D7 AD C1 90                 ...
	jsr     L65B0                           ; 90DA 20 B0 65                  .e
	lda     $A1                             ; 90DD A5 A1                    ..
	sta     L90C4                           ; 90DF 8D C4 90                 ...
	lda     $A0                             ; 90E2 A5 A0                    ..
	sta     L90C3                           ; 90E4 8D C3 90                 ...
	lda     L90C4                           ; 90E7 AD C4 90                 ...
	sta     $A3                             ; 90EA 85 A3                    ..
	lda     #$00                            ; 90EC A9 00                    ..
	sta     $A5                             ; 90EE 85 A5                    ..
	lda     #$06                            ; 90F0 A9 06                    ..
	sta     $A4                             ; 90F2 85 A4                    ..
	ldy     L90C3                           ; 90F4 AC C3 90                 ...
	ldx     #$90                            ; 90F7 A2 90                    ..
	lda     #$C8                            ; 90F9 A9 C8                    ..
	jsr     sub_461F
	lda     L90C2                           ; 90FE AD C2 90                 ...
	asl     a                               ; 9101 0A                       .
	php                                     ; 9102 08                       .
	clc                                     ; 9103 18                       .
	adc     L90CC                           ; 9104 6D CC 90                 m..
	sta     $AE                             ; 9107 85 AE                    ..
	lda     #$00                            ; 9109 A9 00                    ..
	rol     a                               ; 910B 2A                       *
	plp                                     ; 910C 28                       (
	adc     L90CD                           ; 910D 6D CD 90                 m..
	sta     $AF                             ; 9110 85 AF                    ..
	ldy     #$01                            ; 9112 A0 01                    ..
	lda     ($AE),y                         ; 9114 B1 AE                    ..
	sta     L90C6                           ; 9116 8D C6 90                 ...
	dey                                     ; 9119 88                       .
	lda     ($AE),y                         ; 911A B1 AE                    ..
	sta     L90C5                           ; 911C 8D C5 90                 ...
	ldy     L90C8                           ; 911F AC C8 90                 ...
	ldx     L90C6                           ; 9122 AE C6 90                 ...
	lda     L90C5                           ; 9125 AD C5 90                 ...
	jsr     sub_4B97
	lda     $A0                             ; 912B A5 A0                    ..
	sta     L90C7                           ; 912D 8D C7 90                 ...
	lda     L90C7                           ; 9130 AD C7 90                 ...
	sta     $A0                             ; 9133 85 A0                    ..
	rts                                     ; 9135 60                       `

; ----------------------------------------------------------------------------
L9136:  brk                                     ; 9136 00                       .
L9137:  brk                                     ; 9137 00                       .
L9138:  brk                                     ; 9138 00                       .
L9139:  brk                                     ; 9139 00                       .
L913A:  brk                                     ; 913A 00                       .
L913B:  brk                                     ; 913B 00                       .
L913C:  brk                                     ; 913C 00                       .
L913D:  brk                                     ; 913D 00                       .
L913E:  brk                                     ; 913E 00                       .
L913F:  brk                                     ; 913F 00                       .
L9140:  brk                                     ; 9140 00                       .
L9141:  brk                                     ; 9141 00                       .
	brk                                     ; 9142 00                       .
	brk                                     ; 9143 00                       .
L9144:  brk                                     ; 9144 00                       .
L9145:  brk                                     ; 9145 00                       .
L9146:  jmp     L9149                           ; 9146 4C 49 91                 LI.

; ----------------------------------------------------------------------------
L9149:  stx     L9137                           ; 9149 8E 37 91                 .7.
	sta     L9136                           ; 914C 8D 36 91                 .6.
	lda     L9136                           ; 914F AD 36 91                 .6.
	jsr     L65B0                           ; 9152 20 B0 65                  .e
	lda     $A1                             ; 9155 A5 A1                    ..
	sta     L9139                           ; 9157 8D 39 91                 .9.
	lda     $A0                             ; 915A A5 A0                    ..
	sta     L9138                           ; 915C 8D 38 91                 .8.
	lda     L9139                           ; 915F AD 39 91                 .9.
	sta     $A3                             ; 9162 85 A3                    ..
	lda     #$00                            ; 9164 A9 00                    ..
	sta     $A5                             ; 9166 85 A5                    ..
	lda     #$06                            ; 9168 A9 06                    ..
	sta     $A4                             ; 916A 85 A4                    ..
	ldy     L9138                           ; 916C AC 38 91                 .8.
	ldx     #$91                            ; 916F A2 91                    ..
	lda     #$40                            ; 9171 A9 40                    .@
	jsr     sub_461F
	lda     L9137                           ; 9176 AD 37 91                 .7.
	cmp     L9141                           ; 9179 CD 41 91                 .A.
	bcc     L9181                           ; 917C 90 03                    ..
	jmp     L9242                           ; 917E 4C 42 92                 LB.

; ----------------------------------------------------------------------------
L9181:  lda     L9137                           ; 9181 AD 37 91                 .7.
	asl     a                               ; 9184 0A                       .
	php                                     ; 9185 08                       .
	clc                                     ; 9186 18                       .
	adc     L9144                           ; 9187 6D 44 91                 mD.
	sta     $AE                             ; 918A 85 AE                    ..
	lda     #$00                            ; 918C A9 00                    ..
	rol     a                               ; 918E 2A                       *
	plp                                     ; 918F 28                       (
	adc     L9145                           ; 9190 6D 45 91                 mE.
	sta     $AF                             ; 9193 85 AF                    ..
	ldy     #$01                            ; 9195 A0 01                    ..
	lda     ($AE),y                         ; 9197 B1 AE                    ..
	sta     L913D                           ; 9199 8D 3D 91                 .=.
	dey                                     ; 919C 88                       .
	lda     ($AE),y                         ; 919D B1 AE                    ..
	sta     L913C                           ; 919F 8D 3C 91                 .<.
	clc                                     ; 91A2 18                       .
	lda     L9137                           ; 91A3 AD 37 91                 .7.
L91A6:  adc     #$01                            ; 91A6 69 01                    i.
	sta     $AE                             ; 91A8 85 AE                    ..
	lda     $AE                             ; 91AA A5 AE                    ..
	asl     a                               ; 91AC 0A                       .
	php                                     ; 91AD 08                       .
	clc                                     ; 91AE 18                       .
	adc     L9144                           ; 91AF 6D 44 91                 mD.
	sta     $AC                             ; 91B2 85 AC                    ..
	lda     #$00                            ; 91B4 A9 00                    ..
	rol     a                               ; 91B6 2A                       *
	plp                                     ; 91B7 28                       (
	adc     L9145                           ; 91B8 6D 45 91                 mE.
	sta     $AD                             ; 91BB 85 AD                    ..
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
	sta     L913E                           ; 91E9 8D 3E 91                 .>.
	txa                                     ; 91EC 8A                       .
	sta     L913F                           ; 91ED 8D 3F 91                 .?.
	lda     L913B                           ; 91F0 AD 3B 91                 .;.
	sta     $A3                             ; 91F3 85 A3                    ..
	lda     L913F                           ; 91F5 AD 3F 91                 .?.
	sta     $A5                             ; 91F8 85 A5                    ..
	lda     L913E                           ; 91FA AD 3E 91                 .>.
	sta     $A4                             ; 91FD 85 A4                    ..
	ldy     L913A                           ; 91FF AC 3A 91                 .:.
	ldx     L913D                           ; 9202 AE 3D 91                 .=.
	lda     L913C                           ; 9205 AD 3C 91                 .<.
	jsr     sub_461F
	sec                                     ; 920B 38                       8
	lda     L9141                           ; 920C AD 41 91                 .A.
	sbc     #$01                            ; 920F E9 01                    ..
	sta     $AE                             ; 9211 85 AE                    ..
	lda     $AE                             ; 9213 A5 AE                    ..
	asl     a                               ; 9215 0A                       .
	php                                     ; 9216 08                       .
	clc                                     ; 9217 18                       .
	adc     L9144                           ; 9218 6D 44 91                 mD.
	sta     $AC                             ; 921B 85 AC                    ..
	lda     #$00                            ; 921D A9 00                    ..
	rol     a                               ; 921F 2A                       *
	plp                                     ; 9220 28                       (
	adc     L9145                           ; 9221 6D 45 91                 mE.
	sta     $AD                             ; 9224 85 AD                    ..
	ldy     #$01                            ; 9226 A0 01                    ..
	lda     ($AC),y                         ; 9228 B1 AC                    ..
	sta     $A1                             ; 922A 85 A1                    ..
	dey                                     ; 922C 88                       .
	lda     ($AC),y                         ; 922D B1 AC                    ..
	sta     $A0                             ; 922F 85 A0                    ..
	lda     #$00                            ; 9231 A9 00                    ..
	sta     $A3                             ; 9233 85 A3                    ..
	ldy     L9140                           ; 9235 AC 40 91                 .@.
	ldx     $A1                             ; 9238 A6 A1                    ..
	lda     $A0                             ; 923A A5 A0                    ..
	jsr     L45F6                           ; 923C 20 F6 45                  .E
	jmp     L9247                           ; 923F 4C 47 92                 LG.

; ----------------------------------------------------------------------------
L9242:  lda     #$00                            ; 9242 A9 00                    ..
	sta     $A0                             ; 9244 85 A0                    ..
	rts                                     ; 9246 60                       `

; ----------------------------------------------------------------------------
L9247:  lda     #$01                            ; 9247 A9 01                    ..
	sta     $A0                             ; 9249 85 A0                    ..
	rts                                     ; 924B 60                       `

; ----------------------------------------------------------------------------
L924C:  brk                                     ; 924C 00                       .
L924D:  brk                                     ; 924D 00                       .
L924E:  brk                                     ; 924E 00                       .
L924F:  brk                                     ; 924F 00                       .
L9250:  brk                                     ; 9250 00                       .
L9251:  brk                                     ; 9251 00                       .
L9252:  brk                                     ; 9252 00                       .
L9253:  brk                                     ; 9253 00                       .
L9254:  brk                                     ; 9254 00                       .
L9255:  brk                                     ; 9255 00                       .
L9256:  brk                                     ; 9256 00                       .
L9257:  brk                                     ; 9257 00                       .
L9258:  brk                                     ; 9258 00                       .
	brk                                     ; 9259 00                       .
	brk                                     ; 925A 00                       .
L925B:  brk                                     ; 925B 00                       .
L925C:  brk                                     ; 925C 00                       .
L925D:  jmp     L9260                           ; 925D 4C 60 92                 L`.

; ----------------------------------------------------------------------------
L9260:  stx     L924D                           ; 9260 8E 4D 92                 .M.
	sta     L924C                           ; 9263 8D 4C 92                 .L.
	lda     L924C                           ; 9266 AD 4C 92                 .L.
	jsr     L65B0                           ; 9269 20 B0 65                  .e
	lda     $A1                             ; 926C A5 A1                    ..
	sta     L924F                           ; 926E 8D 4F 92                 .O.
	lda     $A0                             ; 9271 A5 A0                    ..
	sta     L924E                           ; 9273 8D 4E 92                 .N.
	lda     L924F                           ; 9276 AD 4F 92                 .O.
	sta     $A3                             ; 9279 85 A3                    ..
	lda     #$00                            ; 927B A9 00                    ..
	sta     $A5                             ; 927D 85 A5                    ..
	lda     #$06                            ; 927F A9 06                    ..
	sta     $A4                             ; 9281 85 A4                    ..
	ldy     L924E                           ; 9283 AC 4E 92                 .N.
	ldx     #$92                            ; 9286 A2 92                    ..
	lda     #$57                            ; 9288 A9 57                    .W
	jsr     sub_461F
	sec                                     ; 928D 38                       8
	lda     L9258                           ; 928E AD 58 92                 .X.
	sbc     #$01                            ; 9291 E9 01                    ..
	sta     $A1                             ; 9293 85 A1                    ..
	ldx     $A1                             ; 9295 A6 A1                    ..
	lda     L924C                           ; 9297 AD 4C 92                 .L.
	jsr     L90CE                           ; 929A 20 CE 90                  ..
	lda     $A0                             ; 929D A5 A0                    ..
	sta     L9250                           ; 929F 8D 50 92                 .P.
	lda     L924D                           ; 92A2 AD 4D 92                 .M.
	cmp     L9258                           ; 92A5 CD 58 92                 .X.
	bcc     L92AD                           ; 92A8 90 03                    ..
	jmp     L9352                           ; 92AA 4C 52 93                 LR.

; ----------------------------------------------------------------------------
L92AD:  lda     L9250                           ; 92AD AD 50 92                 .P.
	beq     L92B5                           ; 92B0 F0 03                    ..
	jmp     L9352                           ; 92B2 4C 52 93                 LR.

; ----------------------------------------------------------------------------
L92B5:  clc                                     ; 92B5 18                       .
	lda     L924D                           ; 92B6 AD 4D 92                 .M.
	adc     #$01                            ; 92B9 69 01                    i.
	sta     $AE                             ; 92BB 85 AE                    ..
	lda     $AE                             ; 92BD A5 AE                    ..
	asl     a                               ; 92BF 0A                       .
	php                                     ; 92C0 08                       .
	clc                                     ; 92C1 18                       .
	adc     L925B                           ; 92C2 6D 5B 92                 m[.
	sta     $AC                             ; 92C5 85 AC                    ..
	lda     #$00                            ; 92C7 A9 00                    ..
	rol     a                               ; 92C9 2A                       *
	plp                                     ; 92CA 28                       (
	adc     L925C                           ; 92CB 6D 5C 92                 m\.
	sta     $AD                             ; 92CE 85 AD                    ..
	ldy     #$01                            ; 92D0 A0 01                    ..
	lda     ($AC),y                         ; 92D2 B1 AC                    ..
	sta     L9254                           ; 92D4 8D 54 92                 .T.
	dey                                     ; 92D7 88                       .
	lda     ($AC),y                         ; 92D8 B1 AC                    ..
	sta     L9253                           ; 92DA 8D 53 92                 .S.
	lda     L924D                           ; 92DD AD 4D 92                 .M.
	asl     a                               ; 92E0 0A                       .
	php                                     ; 92E1 08                       .
	clc                                     ; 92E2 18                       .
	adc     L925B                           ; 92E3 6D 5B 92                 m[.
	sta     $AE                             ; 92E6 85 AE                    ..
	lda     #$00                            ; 92E8 A9 00                    ..
	rol     a                               ; 92EA 2A                       *
	plp                                     ; 92EB 28                       (
	adc     L925C                           ; 92EC 6D 5C 92                 m\.
	sta     $AF                             ; 92EF 85 AF                    ..
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
	sta     L9255                           ; 931D 8D 55 92                 .U.
	txa                                     ; 9320 8A                       .
	sta     L9256                           ; 9321 8D 56 92                 .V.
	lda     L9252                           ; 9324 AD 52 92                 .R.
	sta     $A3                             ; 9327 85 A3                    ..
	lda     L9256                           ; 9329 AD 56 92                 .V.
	sta     $A5                             ; 932C 85 A5                    ..
	lda     L9255                           ; 932E AD 55 92                 .U.
	sta     $A4                             ; 9331 85 A4                    ..
	ldy     L9251                           ; 9333 AC 51 92                 .Q.
	ldx     L9254                           ; 9336 AE 54 92                 .T.
	lda     L9253                           ; 9339 AD 53 92                 .S.
	jsr     L4EB1                           ; 933C 20 B1 4E                  .N
	lda     #$00                            ; 933F A9 00                    ..
	sta     $A3                             ; 9341 85 A3                    ..
	ldy     L9257                           ; 9343 AC 57 92                 .W.
	ldx     L9252                           ; 9346 AE 52 92                 .R.
	lda     L9251                           ; 9349 AD 51 92                 .Q.
	jsr     L45F6                           ; 934C 20 F6 45                  .E
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
L9361:  brk                                     ; 9361 00                       .
L9362:  brk                                     ; 9362 00                       .
L9363:  brk                                     ; 9363 00                       .
L9364:  brk                                     ; 9364 00                       .
L9365:  brk                                     ; 9365 00                       .
L9366:  brk                                     ; 9366 00                       .
	brk                                     ; 9367 00                       .
L9368:  brk                                     ; 9368 00                       .
	brk                                     ; 9369 00                       .

L936A:  prolog
	ldy     #$00                            ; 936D A0 00                    ..
	sty     L9363                           ; 936F 8C 63 93                 .c.
	clc                                     ; 9372 18                       .
	lda     L9055                           ; 9373 AD 55 90                 .U.
	adc     #$02                            ; 9376 69 02                    i.
	sta     $AE                             ; 9378 85 AE                    ..
	lda     L9056                           ; 937A AD 56 90                 .V.
	adc     #$00                            ; 937D 69 00                    i.
	sta     $AF                             ; 937F 85 AF                    ..
	lda     ($AE),y                         ; 9381 B1 AE                    ..
	sta     L9361                           ; 9383 8D 61 93                 .a.
	clc                                     ; 9386 18                       .
	lda     L9055                           ; 9387 AD 55 90                 .U.
	adc     #$16                            ; 938A 69 16                    i.
	sta     $A2                             ; 938C 85 A2                    ..
	lda     L9056                           ; 938E AD 56 90                 .V.
	adc     #$00                            ; 9391 69 00                    i.
	sta     $A3                             ; 9393 85 A3                    ..
	lda     #$00                            ; 9395 A9 00                    ..
	sta     $A5                             ; 9397 85 A5                    ..
	lda     #$04                            ; 9399 A9 04                    ..
	sta     $A4                             ; 939B 85 A4                    ..
	ldy     $A2                             ; 939D A4 A2                    ..
	ldx     #$93                            ; 939F A2 93                    ..
	lda     #$66                            ; 93A1 A9 66                    .f
	jsr     sub_461F
	lda     L905E                           ; 93A6 AD 5E 90                 .^.
	sta     L9362                           ; 93A9 8D 62 93                 .b.
	clc                                     ; 93AC 18                       .
	lda     L9366                           ; 93AD AD 66 93                 .f.
	adc     L9361                           ; 93B0 6D 61 93                 ma.
	sta     L9366                           ; 93B3 8D 66 93                 .f.
	clc                                     ; 93B6 18                       .
	lda     L9368                           ; 93B7 AD 68 93                 .h.
	adc     L9361                           ; 93BA 6D 61 93                 ma.
	sta     L9368                           ; 93BD 8D 68 93                 .h.
	ldx     L9366                           ; 93C0 AE 66 93                 .f.
	lda     L9362                           ; 93C3 AD 62 93                 .b.
	jsr     L4955                           ; 93C6 20 55 49                  UI
	lda     $A0                             ; 93C9 A5 A0                    ..
	sta     L9364                           ; 93CB 8D 64 93                 .d.
	lda     L9364                           ; 93CE AD 64 93                 .d.
	eor     #$01                            ; 93D1 49 01                    I.
	beq     L93D8                           ; 93D3 F0 03                    ..
	jmp     L93E5                           ; 93D5 4C E5 93                 L..

; ----------------------------------------------------------------------------
L93D8:  sec                                     ; 93D8 38                       8
	lda     L9366                           ; 93D9 AD 66 93                 .f.
	sbc     L9362                           ; 93DC ED 62 93                 .b.
	sta     L9363                           ; 93DF 8D 63 93                 .c.
	jmp     L9405                           ; 93E2 4C 05 94                 L..

; ----------------------------------------------------------------------------
L93E5:  ldx     L9368                           ; 93E5 AE 68 93                 .h.
	lda     L9362                           ; 93E8 AD 62 93                 .b.
	jsr     L4955                           ; 93EB 20 55 49                  UI
	lda     $A0                             ; 93EE A5 A0                    ..
	sta     L9365                           ; 93F0 8D 65 93                 .e.
	lda     L9365                           ; 93F3 AD 65 93                 .e.
	beq     L93FB                           ; 93F6 F0 03                    ..
	jmp     L9405                           ; 93F8 4C 05 94                 L..

; ----------------------------------------------------------------------------
L93FB:  sec                                     ; 93FB 38                       8
	lda     L9368                           ; 93FC AD 68 93                 .h.
	sbc     L9362                           ; 93FF ED 62 93                 .b.
	sta     L9363                           ; 9402 8D 63 93                 .c.
L9405:  lda     #$80                            ; 9405 A9 80                    ..
	sta     $A3                             ; 9407 85 A3                    ..
	ldy     L9363                           ; 9409 AC 63 93                 .c.
	ldx     L9054                           ; 940C AE 54 90                 .T.
	lda     L9053                           ; 940F AD 53 90                 .S.
	jsr     sub_7ADF                        ; 9412 20 DF 7A                  .z
	rts                                     ; 9415 60                       `

; ----------------------------------------------------------------------------
L9416:  brk                                     ; 9416 00                       .
	brk                                     ; 9417 00                       .
	brk                                     ; 9418 00                       .
L9419:  brk                                     ; 9419 00                       .
L941A:  brk                                     ; 941A 00                       .
	brk                                     ; 941B 00                       .
	brk                                     ; 941C 00                       .
	brk                                     ; 941D 00                       .
L941E:  brk                                     ; 941E 00                       .
L941F:  brk                                     ; 941F 00                       .
	brk                                     ; 9420 00                       .
	brk                                     ; 9421 00                       .
L9422:  brk                                     ; 9422 00                       .
	brk                                     ; 9423 00                       .
L9424:  brk                                     ; 9424 00                       .
	brk                                     ; 9425 00                       .
L9426:  brk                                     ; 9426 00                       .

sub_9427:  
	prolog
	sta     L9416                           ; 942A 8D 16 94                 ...
	clc                                     ; 942D 18                       .
	lda     L9059                           ; 942E AD 59 90                 .Y.
	adc     L905E                           ; 9431 6D 5E 90                 m^.
	sta     $AE                             ; 9434 85 AE                    ..
	lda     L905A                           ; 9436 AD 5A 90                 .Z.
	adc     #$00                            ; 9439 69 00                    i.
	sta     $AF                             ; 943B 85 AF                    ..
	ldy     #$00                            ; 943D A0 00                    ..
	lda     (off_AE),y                      ; 943F B1 AE                    ..
	eor     #$80                            ; 9441 49 80                    I.
	sta     (off_AE),y                      ; 9443 91 AE                    ..
	lda     L9050                           ; 9445 AD 50 90                 .P.
	eor     #$01                            ; 9448 49 01                    I.
	lbne	L9499
	lda     L905A                           ; 944F AD 5A 90                 .Z.
	sta     $A3                             ; 9452 85 A3                    ..
	lda     #$00                            ; 9454 A9 00                    ..
	sta     $A5                             ; 9456 85 A5                    ..
	lda     L9060                           ; 9458 AD 60 90                 .`.
	sta     $A4                             ; 945B 85 A4                    ..
	ldy     L9059                           ; 945D AC 59 90                 .Y.
	ldx     L905C                           ; 9460 AE 5C 90                 .\.
	lda     L905B                           ; 9463 AD 5B 90                 .[.
	jsr     sub_461F
	jmp     L9470                           ; 9469 4C 70 94                 Lp.

; ----------------------------------------------------------------------------
L946C:	.byte	$03,"CBS"

; ----------------------------------------------------------------------------
L9470:  lda     #$00                            ; 9470 A9 00                    ..
	sta     $A3                             ; 9472 85 A3                    ..
	lda     #$00                            ; 9474 A9 00                    ..
	sta     $A5                             ; 9476 85 A5                    ..
	lda     L9051                           ; 9478 AD 51 90                 .Q.
	sta     $A4                             ; 947B 85 A4                    ..
	lda     #$00                            ; 947D A9 00                    ..
	sta     $A7                             ; 947F 85 A7                    ..
	lda     L905D                           ; 9481 AD 5D 90                 .].
	sta     $A6                             ; 9484 85 A6                    ..
	lda     L905A                           ; 9486 AD 5A 90                 .Z.
	sta     $A9                             ; 9489 85 A9                    ..
	lda     L9059                           ; 948B AD 59 90                 .Y.
	sta     $A8                             ; 948E 85 A8                    ..
	ldy     #$4C                            ; 9490 A0 4C                    .L
	ldx     #>L946C                         ; 9492 A2 94                    ..
	lda     #<L946C                         ; 9494 A9 6C                    .l
	jsr     sub_55A0
L9499:  lda     #$7F                            ; 9499 A9 7F                    ..
	cmp     L9416                           ; 949B CD 16 94                 ...
	bcc     L94A3                           ; 949E 90 03                    ..
	jmp     L94AB                           ; 94A0 4C AB 94                 L..

; ----------------------------------------------------------------------------
L94A3:  ldy     #$00                            ; 94A3 A0 00                    ..
	sty     L9416                           ; 94A5 8C 16 94                 ...
	jmp     L94E8                           ; 94A8 4C E8 94                 L..

; ----------------------------------------------------------------------------
L94AB:  sec                                     ; 94AB 38                       8
	lda     L9061                           ; 94AC AD 61 90                 .a.
	sbc     #$01                            ; 94AF E9 01                    ..
	sta     $AE                             ; 94B1 85 AE                    ..
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
	clc                                     ; 94D0 18                       .
	lda     $AE                             ; 94D1 A5 AE                    ..
	adc     #$01                            ; 94D3 69 01                    i.
	sta     $A1                             ; 94D5 85 A1                    ..
	ldx     $A1                             ; 94D7 A6 A1                    ..
	lda     L9053                           ; 94D9 AD 53 90                 .S.
	jsr     L66FC                           ; 94DC 20 FC 66                  .f
L94DF:  sec                                     ; 94DF 38                       8
	lda     L9061                           ; 94E0 AD 61 90                 .a.
	sbc     #$01                            ; 94E3 E9 01                    ..
	sta     L9416                           ; 94E5 8D 16 94                 ...
L94E8:  lda     L9416                           ; 94E8 AD 16 94                 ...
	asl     a                               ; 94EB 0A                       .
	php                                     ; 94EC 08                       .
	clc                                     ; 94ED 18                       .
	adc     L9064                           ; 94EE 6D 64 90                 md.
	sta     $AE                             ; 94F1 85 AE                    ..
	lda     #$00                            ; 94F3 A9 00                    ..
	rol     a                               ; 94F5 2A                       *
	plp                                     ; 94F6 28                       (
	adc     L9065                           ; 94F7 6D 65 90                 me.
	sta     $AF                             ; 94FA 85 AF                    ..
	ldy     #$01                            ; 94FC A0 01                    ..
	lda     ($AE),y                         ; 94FE B1 AE                    ..
	sta     L905C                           ; 9500 8D 5C 90                 .\.
	dey                                     ; 9503 88                       .
	lda     ($AE),y                         ; 9504 B1 AE                    ..
	sta     L905B                           ; 9506 8D 5B 90                 .[.
	lda     L905C                           ; 9509 AD 5C 90                 .\.
	sta     $A3                             ; 950C 85 A3                    ..
	lda     #$00                            ; 950E A9 00                    ..
	sta     $A5                             ; 9510 85 A5                    ..
	lda     L9060                           ; 9512 AD 60 90                 .`.
	sta     $A4                             ; 9515 85 A4                    ..
	ldy     L905B                           ; 9517 AC 5B 90                 .[.
	ldx     L905A                           ; 951A AE 5A 90                 .Z.
	lda     L9059                           ; 951D AD 59 90                 .Y.
	jsr     sub_461F
	ldx     L9416                           ; 9523 AE 16 94                 ...
	lda     L9053                           ; 9526 AD 53 90                 .S.
	jsr     L90CE                           ; 9529 20 CE 90                  ..
	mv	L905D, $A0
	mv	L9051, L9416
	mv	$A3, L9056
	ldi	$A5, $00
	ldi	$A4, $07
	ldy     L9055                           ; 9544 AC 55 90                 .U.
L9547:  ldx     #$94                            ; 9547 A2 94                    ..
	lda     #$1C                            ; 9549 A9 1C                    ..
	jsr     sub_461F
	clc                                     ; 954E 18                       .
	lda     L9055                           ; 954F AD 55 90                 .U.
	adc     #$1E                            ; 9552 69 1E                    i.
	sta     $A2                             ; 9554 85 A2                    ..
	lda     L9056                           ; 9556 AD 56 90                 .V.
	adc     #$00                            ; 9559 69 00                    i.
	sta     $A3                             ; 955B 85 A3                    ..
	lda     #$00                            ; 955D A9 00                    ..
	sta     $A5                             ; 955F 85 A5                    ..
	lda     #$04                            ; 9561 A9 04                    ..
	sta     $A4                             ; 9563 85 A4                    ..
	ldy     $A2                             ; 9565 A4 A2                    ..
	ldx     #$94                            ; 9567 A2 94                    ..
	lda     #$23                            ; 9569 A9 23                    .#
	jsr     sub_461F
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
	jsr     L4955                           ; 9597 20 55 49                  UI
	lda     $A0                             ; 959A A5 A0                    ..
	eor     #$01                            ; 959C 49 01                    I.
	beq     L95A3                           ; 959E F0 03                    ..
	jmp     L95B8                           ; 95A0 4C B8 95                 L..

; ----------------------------------------------------------------------------
L95A3:  sec                                     ; 95A3 38                       8
	lda     L9419                           ; 95A4 AD 19 94                 ...
	sbc     L9051                           ; 95A7 ED 51 90                 .Q.
	sta     $AE                             ; 95AA 85 AE                    ..
	sec                                     ; 95AC 38                       8
	lda     L941F                           ; 95AD AD 1F 94                 ...
	sbc     $AE                             ; 95B0 E5 AE                    ..
	sta     L941F                           ; 95B2 8D 1F 94                 ...
	jmp     L95DC                           ; 95B5 4C DC 95                 L..

; ----------------------------------------------------------------------------
L95B8:  ldx     L941A                           ; 95B8 AE 1A 94                 ...
	lda     L9051                           ; 95BB AD 51 90                 .Q.
	jsr     L496E                           ; 95BE 20 6E 49                  nI
	lda     $A0                             ; 95C1 A5 A0                    ..
	eor     #$01                            ; 95C3 49 01                    I.
	beq     L95CA                           ; 95C5 F0 03                    ..
	jmp     L95DC                           ; 95C7 4C DC 95                 L..

; ----------------------------------------------------------------------------
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
	ldx     L9054                           ; 95E3 AE 54 90                 .T.
	lda     L9053                           ; 95E6 AD 53 90                 .S.
	jsr     sub_7ADF                        ; 95E9 20 DF 7A                  .z
	lda     L941F                           ; 95EC AD 1F 94                 ...
	sta     $A3                             ; 95EF 85 A3                    ..
	ldy     L941E                           ; 95F1 AC 1E 94                 ...
	ldx     L9053                           ; 95F4 AE 53 90                 .S.
	lda     L9052                           ; 95F7 AD 52 90                 .R.
	jsr     sub_758C
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
L961D:  brk                                     ; 961D 00                       .
L961E:  jmp     L9621                           ; 961E 4C 21 96                 L!.

; ----------------------------------------------------------------------------
L9621:  sta     L961D                           ; 9621 8D 1D 96                 ...
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
	clc                                     ; 9666 18                       .
	lda     L9059                           ; 9667 AD 59 90                 .Y.
	adc     L905E                           ; 966A 6D 5E 90                 m^.
	sta     $AE                             ; 966D 85 AE                    ..
	lda     L905A                           ; 966F AD 5A 90                 .Z.
	adc     #$00                            ; 9672 69 00                    i.
	sta     $AF                             ; 9674 85 AF                    ..
	ldy     #$00                            ; 9676 A0 00                    ..
	lda     ($AE),y                         ; 9678 B1 AE                    ..
	eor     #$80                            ; 967A 49 80                    I.
	sta     ($AE),y                         ; 967C 91 AE                    ..
	jsr     L936A                           ; 967E 20 6A 93                  j.
	rts                                     ; 9681 60                       `

; ----------------------------------------------------------------------------
L9682:  brk                                     ; 9682 00                       .
L9683:  brk                                     ; 9683 00                       .
L9684:  brk                                     ; 9684 00                       .
	brk                                     ; 9685 00                       .
	brk                                     ; 9686 00                       .
L9687:  brk                                     ; 9687 00                       .
L9688:  brk                                     ; 9688 00                       .
L9689:  brk                                     ; 9689 00                       .
L968A:  brk                                     ; 968A 00                       .
L968B:  brk                                     ; 968B 00                       .
L968C:  brk                                     ; 968C 00                       .
L968D:  brk                                     ; 968D 00                       .

L968E:	prolog
	jsr     sub_44D5                           ; 9691 20 D5 44                  .D
	.byte   $82                             ; 9694 82                       .
	stx     $02,y                           ; 9695 96 02                    ..
	lda     L9682                           ; 9697 AD 82 96                 ...
	sta     L9052                           ; 969A 8D 52 90                 .R.
	lda     L9052                           ; 969D AD 52 90                 .R.
	jsr     sub_7035
	lda     $A1                             ; 96A3 A5 A1                    ..
	sta     L9056                           ; 96A5 8D 56 90                 .V.
	lda     $A0                             ; 96A8 A5 A0                    ..
	sta     L9055                           ; 96AA 8D 55 90                 .U.
	clc                                     ; 96AD 18                       .
	lda     L9055                           ; 96AE AD 55 90                 .U.
	adc     #$04                            ; 96B1 69 04                    i.
	sta     $AE                             ; 96B3 85 AE                    ..
	lda     L9056                           ; 96B5 AD 56 90                 .V.
	adc     #$00                            ; 96B8 69 00                    i.
	sta     $AF                             ; 96BA 85 AF                    ..
	ldy     #$00                            ; 96BC A0 00                    ..
	lda     ($AE),y                         ; 96BE B1 AE                    ..
	sta     L9053                           ; 96C0 8D 53 90                 .S.
	lda     L9053                           ; 96C3 AD 53 90                 .S.
	jsr     L65B0                           ; 96C6 20 B0 65                  .e
	lda     $A1                             ; 96C9 A5 A1                    ..
	sta     L9058                           ; 96CB 8D 58 90                 .X.
	lda     $A0                             ; 96CE A5 A0                    ..
	sta     L9057                           ; 96D0 8D 57 90                 .W.
	lda     L9058                           ; 96D3 AD 58 90                 .X.
	sta     $A3                             ; 96D6 85 A3                    ..
	lda     #$00                            ; 96D8 A9 00                    ..
	sta     $A5                             ; 96DA 85 A5                    ..
	lda     #$06                            ; 96DC A9 06                    ..
	sta     $A4                             ; 96DE 85 A4                    ..
	ldy     L9057                           ; 96E0 AC 57 90                 .W.
	ldx     #$90                            ; 96E3 A2 90                    ..
	lda     #$60                            ; 96E5 A9 60                    .`
	jsr     sub_461F
	lda     L9683                           ; 96EA AD 83 96                 ...
	sta     L9054                           ; 96ED 8D 54 90                 .T.
	ldx     L9054                           ; 96F0 AE 54 90                 .T.
	lda     L9053                           ; 96F3 AD 53 90                 .S.
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
	ldx     L9054                           ; 970E AE 54 90                 .T.
	lda     L9053                           ; 9711 AD 53 90                 .S.
	jsr     sub_7ADF                        ; 9714 20 DF 7A                  .z
	dmv	off_AE, L968C
	ldy     #$00                            ; 9721 A0 00                    ..
	lda     (off_AE),y                      ; 9723 B1 AE                    ..
	sta     L968B                           ; 9725 8D 8B 96                 ...
	lda     L968B                           ; 9728 AD 8B 96                 ...
	jsr     L65B0                           ; 972B 20 B0 65                  .e
	rdmv	L9687, $A0
	dmv	off_AE, L9687
	ldy     #$00                            ; 9742 A0 00                    ..
	lda     (off_AE),y
	sta     $A1                             ; 9746 85 A1                    ..
	ldx     $A1                             ; 9748 A6 A1                    ..
	lda     L9060                           ; 974A AD 60 90                 .`.
	jsr     L4990                           ; 974D 20 90 49                  .I
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
	lda     L9689                           ; 9771 AD 89 96                 ...
	sta     $AE                             ; 9774 85 AE                    ..
	lda     L968A                           ; 9776 AD 8A 96                 ...
	sta     $AF                             ; 9779 85 AF                    ..
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
	jsr     L936A                           ; 9792 20 6A 93                  j.
	lda     L9684                           ; 9795 AD 84 96                 ...
	sta     L905F                           ; 9798 8D 5F 90                 ._.
	rts                                     ; 979B 60                       `

; ----------------------------------------------------------------------------
L979C:  brk                                     ; 979C 00                       .
L979D:  brk                                     ; 979D 00                       .
L979E:  brk                                     ; 979E 00                       .
L979F:  brk                                     ; 979F 00                       .
L97A0:  brk                                     ; 97A0 00                       .

L97A1:  prolog
	sta     L979C                           ; 97A4 8D 9C 97                 ...
	lda     L464E                           ; 97A7 AD 4E 46                 .NF
	eor     #$02                            ; 97AA 49 02                    I.
	lbeq	L97B2
	rts                                     ; 97B1 60                       `

; ----------------------------------------------------------------------------
L97B2:  ldy     #$00                            ; 97B2 A0 00                    ..
	sty     L979D                           ; 97B4 8C 9D 97                 ...
	lda     L979C                           ; 97B7 AD 9C 97                 ...
	and     #$C0                            ; 97BA 29 C0                    ).
	sta     $AE                             ; 97BC 85 AE                    ..
	lda     $AE                             ; 97BE A5 AE                    ..
	eor     #$C0                            ; 97C0 49 C0                    I.
	lbeq	L97E7
	lda     $02FC                           ; 97C7 AD FC 02                 ...
	eor     #$FF                            ; 97CA 49 FF                    I.
	lbeq	L97DE
	lda     #$07                            ; 97D1 A9 07                    ..
	jsr     L45A3                           ; 97D3 20 A3 45                  .E
	mv	L979D, $A0
	jmp     L97E4                           ; 97DB 4C E4 97                 L..

; ----------------------------------------------------------------------------
L97DE:  lda     L979C                           ; 97DE AD 9C 97                 ...
	sta     L979D                           ; 97E1 8D 9D 97                 ...
L97E4:  jmp     L97EC                           ; 97E4 4C EC 97                 L..

; ----------------------------------------------------------------------------
L97E7:  lda     #$FF                            ; 97E7 A9 FF                    ..
	sta     $02FC                           ; 97E9 8D FC 02                 ...
L97EC:  lda     L979D                           ; 97EC AD 9D 97                 ...
	eor     #$9B                            ; 97EF 49 9B                    I.
	lbne	L9810
	clc                                     ; 97F6 18                       .
	lda     L9051                           ; 97F7 AD 51 90                 .Q.
	adc     #$01                            ; 97FA 69 01                    i.
	sta     $A0                             ; 97FC 85 A0                    ..
	lda     $A0                             ; 97FE A5 A0                    ..
	jsr     sub_9427
	lda     #$00                            ; 9803 A9 00                    ..
	jsr     L961E                           ; 9805 20 1E 96                  ..
	lda     #$15                            ; 9808 A9 15                    ..
	jsr     sub_4BA7                           ; 980A 20 A7 4B                  .K
	jmp     L9BCE                           ; 980D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9810:  lda     L979C                           ; 9810 AD 9C 97                 ...
	eor     #$2C                            ; 9813 49 2C                    I,
	lbne	L9830
	clc                                     ; 981A 18                       .
	lda     L905E                           ; 981B AD 5E 90                 .^.
	adc     #$04                            ; 981E 69 04                    i.
	sta     $AE                             ; 9820 85 AE                    ..
	lda     $AE                             ; 9822 A5 AE                    ..
	and     #$FC                            ; 9824 29 FC                    ).
	sta     $A0                             ; 9826 85 A0                    ..
	lda     $A0                             ; 9828 A5 A0                    ..
	jsr     L961E                           ; 982A 20 1E 96                  ..
	jmp     L9BCE                           ; 982D 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9830:  lda     L979C                           ; 9830 AD 9C 97                 ...
	eor     #$87                            ; 9833 49 87                    I.
	lbne	L984A
	clc                                     ; 983A 18                       .
	lda     L905E                           ; 983B AD 5E 90                 .^.
	adc     #$01                            ; 983E 69 01                    i.
	sta     $A0                             ; 9840 85 A0                    ..
	lda     $A0                             ; 9842 A5 A0                    ..
	jsr     L961E                           ; 9844 20 1E 96                  ..
	jmp     L9BCE                           ; 9847 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L984A:  lda     L979C                           ; 984A AD 9C 97                 ...
	eor     #$86                            ; 984D 49 86                    I.
	lbne	L9864
	sec                                     ; 9854 38                       8
	lda     L905E                           ; 9855 AD 5E 90                 .^.
	sbc     #$01                            ; 9858 E9 01                    ..
	sta     $A0                             ; 985A 85 A0                    ..
	lda     $A0                             ; 985C A5 A0                    ..
	jsr     L961E                           ; 985E 20 1E 96                  ..
	jmp     L9BCE                           ; 9861 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9864:  lda     L979C                           ; 9864 AD 9C 97                 ...
	eor     #$F6                            ; 9867 49 F6                    I.
	lbne	L9876
	lda     #$00                            ; 986E A9 00                    ..
	jsr     L961E                           ; 9870 20 1E 96                  ..
	jmp     L9BCE                           ; 9873 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9876:  lda     L979C                           ; 9876 AD 9C 97                 ...
	eor     #$F7                            ; 9879 49 F7                    I.
	lbne	L9889
	lda     L905D                           ; 9880 AD 5D 90                 .].
	jsr     L961E                           ; 9883 20 1E 96                  ..
	jmp     L9BCE                           ; 9886 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9889:  lda     L979C                           ; 9889 AD 9C 97                 ...
	eor     #$8E                            ; 988C 49 8E                    I.
	lbne	L98A9
	sec                                     ; 9893 38                       8
	lda     L9051                           ; 9894 AD 51 90                 .Q.
	sbc     #$01                            ; 9897 E9 01                    ..
	sta     $A0                             ; 9899 85 A0                    ..
	lda     $A0                             ; 989B A5 A0                    ..
	jsr     sub_9427
	lda     L905E                           ; 98A0 AD 5E 90                 .^.
	jsr     L961E                           ; 98A3 20 1E 96                  ..
	jmp     L9BCE                           ; 98A6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98A9:  lda     L979C                           ; 98A9 AD 9C 97                 ...
	eor     #$8F                            ; 98AC 49 8F                    I.
	beq     L98B3                           ; 98AE F0 03                    ..
	jmp     L98C9                           ; 98B0 4C C9 98                 L..

; ----------------------------------------------------------------------------
L98B3:  clc                                     ; 98B3 18                       .
	lda     L9051                           ; 98B4 AD 51 90                 .Q.
	adc     #$01                            ; 98B7 69 01                    i.
	sta     $A0                             ; 98B9 85 A0                    ..
	lda     $A0                             ; 98BB A5 A0                    ..
	jsr     sub_9427
	lda     L905E                           ; 98C0 AD 5E 90                 .^.
	jsr     L961E                           ; 98C3 20 1E 96                  ..
	jmp     L9BCE                           ; 98C6 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98C9:  lda     L979C                           ; 98C9 AD 9C 97                 ...
	eor     #$CE                            ; 98CC 49 CE                    I.
	beq     L98D3                           ; 98CE F0 03                    ..
	jmp     L98E0                           ; 98D0 4C E0 98                 L..

; ----------------------------------------------------------------------------
L98D3:  lda     #$00                            ; 98D3 A9 00                    ..
	jsr     sub_9427
	lda     #$00                            ; 98D8 A9 00                    ..
	jsr     L961E                           ; 98DA 20 1E 96                  ..
	jmp     L9BCE                           ; 98DD 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98E0:  lda     L979C                           ; 98E0 AD 9C 97                 ...
	eor     #$CF                            ; 98E3 49 CF                    I.
	beq     L98EA                           ; 98E5 F0 03                    ..
	jmp     L98FF                           ; 98E7 4C FF 98                 L..

; ----------------------------------------------------------------------------
L98EA:  sec                                     ; 98EA 38                       8
	lda     L9061                           ; 98EB AD 61 90                 .a.
	sbc     #$01                            ; 98EE E9 01                    ..
	sta     $A0                             ; 98F0 85 A0                    ..
	lda     $A0                             ; 98F2 A5 A0                    ..
	jsr     sub_9427
	lda     #$00                            ; 98F7 A9 00                    ..
	jsr     L961E                           ; 98F9 20 1E 96                  ..
	jmp     L9BCE                           ; 98FC 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L98FF:  lda     L979C                           ; 98FF AD 9C 97                 ...
	eor     #$B4                            ; 9902 49 B4                    I.
	beq     L9909                           ; 9904 F0 03                    ..
	jmp     L999C                           ; 9906 4C 9C 99                 L..

; ----------------------------------------------------------------------------
L9909:  lda     L905E                           ; 9909 AD 5E 90                 .^.
	cmp     L905D                           ; 990C CD 5D 90                 .].
	bcs     L9914                           ; 990F B0 03                    ..
	jmp     L991A                           ; 9911 4C 1A 99                 L..

; ----------------------------------------------------------------------------
L9914:  jsr     L4F5A                           ; 9914 20 5A 4F                  ZO
	jmp     L9999                           ; 9917 4C 99 99                 L..

; ----------------------------------------------------------------------------
L991A:  clc                                     ; 991A 18                       .
	lda     L9059                           ; 991B AD 59 90                 .Y.
	adc     L905E                           ; 991E 6D 5E 90                 m^.
	sta     L979F                           ; 9921 8D 9F 97                 ...
	lda     L905A                           ; 9924 AD 5A 90                 .Z.
	adc     #$00                            ; 9927 69 00                    i.
	sta     L97A0                           ; 9929 8D A0 97                 ...
	clc                                     ; 992C 18                       .
	lda     L979F                           ; 992D AD 9F 97                 ...
	adc     #$01                            ; 9930 69 01                    i.
	sta     $A2                             ; 9932 85 A2                    ..
	lda     L97A0                           ; 9934 AD A0 97                 ...
	adc     #$00                            ; 9937 69 00                    i.
	sta     $A3                             ; 9939 85 A3                    ..
	sec                                     ; 993B 38                       8
	lda     L9060                           ; 993C AD 60 90                 .`.
	sbc     L905E                           ; 993F ED 5E 90                 .^.
	sta     $A4                             ; 9942 85 A4                    ..
	lda     #$00                            ; 9944 A9 00                    ..
	sta     $A5                             ; 9946 85 A5                    ..
	ldy     $A2                             ; 9948 A4 A2                    ..
	ldx     L97A0                           ; 994A AE A0 97                 ...
	lda     L979F                           ; 994D AD 9F 97                 ...
	jsr     sub_461F
	sec                                     ; 9953 38                       8
	lda     L9060                           ; 9954 AD 60 90                 .`.
	sbc     #$01                            ; 9957 E9 01                    ..
	sta     $AE                             ; 9959 85 AE                    ..
	clc                                     ; 995B 18                       .
	lda     L9059                           ; 995C AD 59 90                 .Y.
	adc     $AE                             ; 995F 65 AE                    e.
	sta     $AC                             ; 9961 85 AC                    ..
	lda     L905A                           ; 9963 AD 5A 90                 .Z.
	adc     #$00                            ; 9966 69 00                    i.
	sta     $AD                             ; 9968 85 AD                    ..
	lda     #$00                            ; 996A A9 00                    ..
	ldy     #$00                            ; 996C A0 00                    ..
	sta     ($AC),y                         ; 996E 91 AC                    ..
	sec                                     ; 9970 38                       8
	lda     L905D                           ; 9971 AD 5D 90                 .].
	sbc     #$01                            ; 9974 E9 01                    ..
	sta     L905D                           ; 9976 8D 5D 90                 .].
	clc                                     ; 9979 18                       .
	lda     L9059                           ; 997A AD 59 90                 .Y.
	adc     L905E                           ; 997D 6D 5E 90                 m^.
	sta     $AE                             ; 9980 85 AE                    ..
	lda     L905A                           ; 9982 AD 5A 90                 .Z.
	adc     #$00                            ; 9985 69 00                    i.
	sta     $AF                             ; 9987 85 AF                    ..
	lda     ($AE),y                         ; 9989 B1 AE                    ..
	eor     #$80                            ; 998B 49 80                    I.
	sta     ($AE),y                         ; 998D 91 AE                    ..
	iny                                     ; 998F C8                       .
	sty     L9050                           ; 9990 8C 50 90                 .P.
	lda     L905E                           ; 9993 AD 5E 90                 .^.
	jsr     L961E                           ; 9996 20 1E 96                  ..
L9999:  jmp     L9BCE                           ; 9999 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L999C:  lda     L979C                           ; 999C AD 9C 97                 ...
	eor     #$34                            ; 999F 49 34                    I4
	beq     L99A6                           ; 99A1 F0 03                    ..
	jmp     L9A45                           ; 99A3 4C 45 9A                 LE.

; ----------------------------------------------------------------------------
L99A6:  lda     L905E                           ; 99A6 AD 5E 90                 .^.
	beq     L99AE                           ; 99A9 F0 03                    ..
	jmp     L99B4                           ; 99AB 4C B4 99                 L..

; ----------------------------------------------------------------------------
L99AE:  jsr     L4F5A                           ; 99AE 20 5A 4F                  ZO
	jmp     L9A42                           ; 99B1 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L99B4:  lda     L905D                           ; 99B4 AD 5D 90                 .].
	cmp     L905E                           ; 99B7 CD 5E 90                 .^.
	bcs     L99BF                           ; 99BA B0 03                    ..
	jmp     L9A35                           ; 99BC 4C 35 9A                 L5.

; ----------------------------------------------------------------------------
L99BF:  clc                                     ; 99BF 18                       .
	lda     L9059                           ; 99C0 AD 59 90                 .Y.
	adc     L905E                           ; 99C3 6D 5E 90                 m^.
	sta     L979F                           ; 99C6 8D 9F 97                 ...
	lda     L905A                           ; 99C9 AD 5A 90                 .Z.
	adc     #$00                            ; 99CC 69 00                    i.
	sta     L97A0                           ; 99CE 8D A0 97                 ...
	sec                                     ; 99D1 38                       8
	lda     L979F                           ; 99D2 AD 9F 97                 ...
	sbc     #$01                            ; 99D5 E9 01                    ..
	sta     $A0                             ; 99D7 85 A0                    ..
	lda     L97A0                           ; 99D9 AD A0 97                 ...
	sbc     #$00                            ; 99DC E9 00                    ..
	sta     $A1                             ; 99DE 85 A1                    ..
	lda     L97A0                           ; 99E0 AD A0 97                 ...
	sta     $A3                             ; 99E3 85 A3                    ..
	sec                                     ; 99E5 38                       8
	lda     L9060                           ; 99E6 AD 60 90                 .`.
	sbc     L905E                           ; 99E9 ED 5E 90                 .^.
	sta     $A4                             ; 99EC 85 A4                    ..
	lda     #$00                            ; 99EE A9 00                    ..
	sta     $A5                             ; 99F0 85 A5                    ..
	ldy     L979F                           ; 99F2 AC 9F 97                 ...
	ldxa	$A0
	jsr     sub_461F
	sec                                     ; 99FC 38                       8
	lda     L9060                           ; 99FD AD 60 90                 .`.
	sbc     #$01                            ; 9A00 E9 01                    ..
	sta     $AE                             ; 9A02 85 AE                    ..
	clc                                     ; 9A04 18                       .
	lda     L9059                           ; 9A05 AD 59 90                 .Y.
	adc     $AE                             ; 9A08 65 AE                    e.
	sta     $AC                             ; 9A0A 85 AC                    ..
	lda     L905A                           ; 9A0C AD 5A 90                 .Z.
	adc     #$00                            ; 9A0F 69 00                    i.
	sta     $AD                             ; 9A11 85 AD                    ..
	lda     #$00                            ; 9A13 A9 00                    ..
	ldy     #$00                            ; 9A15 A0 00                    ..
	sta     ($AC),y                         ; 9A17 91 AC                    ..
	iny                                     ; 9A19 C8                       .
	sty     L9050                           ; 9A1A 8C 50 90                 .P.
	sec                                     ; 9A1D 38                       8
	lda     L905D                           ; 9A1E AD 5D 90                 .].
	sbc     #$01                            ; 9A21 E9 01                    ..
	sta     L905D                           ; 9A23 8D 5D 90                 .].
	sec                                     ; 9A26 38                       8
	lda     L905E                           ; 9A27 AD 5E 90                 .^.
	sbc     #$01                            ; 9A2A E9 01                    ..
	sta     L905E                           ; 9A2C 8D 5E 90                 .^.
	jsr     L936A                           ; 9A2F 20 6A 93                  j.
	jmp     L9A42                           ; 9A32 4C 42 9A                 LB.

; ----------------------------------------------------------------------------
L9A35:  sec                                     ; 9A35 38                       8
	lda     L905E                           ; 9A36 AD 5E 90                 .^.
	sbc     #$01                            ; 9A39 E9 01                    ..
	sta     $A0                             ; 9A3B 85 A0                    ..
	lda     $A0                             ; 9A3D A5 A0                    ..
	jsr     L961E                           ; 9A3F 20 1E 96                  ..
L9A42:  jmp     L9BCE                           ; 9A42 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A45:  lda     L979C                           ; 9A45 AD 9C 97                 ...
	eor     #$74                            ; 9A48 49 74                    It
	beq     L9A4F                           ; 9A4A F0 03                    ..
	jmp     L9A96                           ; 9A4C 4C 96 9A                 L..

; ----------------------------------------------------------------------------
L9A4F:  ldx     L9051                           ; 9A4F AE 51 90                 .Q.
	lda     L9053                           ; 9A52 AD 53 90                 .S.
	jsr     L9146                           ; 9A55 20 46 91                  F.
	lda     $A0                             ; 9A58 A5 A0                    ..
	sta     L979E                           ; 9A5A 8D 9E 97                 ...
	lda     L979E                           ; 9A5D AD 9E 97                 ...
	eor     #$01                            ; 9A60 49 01                    I.
	beq     L9A67                           ; 9A62 F0 03                    ..
	jmp     L9A93                           ; 9A64 4C 93 9A                 L..

; ----------------------------------------------------------------------------
L9A67:  ldy     #$00                            ; 9A67 A0 00                    ..
	sty     L9050                           ; 9A69 8C 50 90                 .P.
	jmp     L9A72                           ; 9A6C 4C 72 9A                 Lr.

; ----------------------------------------------------------------------------
	.byte   $02                             ; 9A6F 02                       .
	.byte   $43                             ; 9A70 43                       C
	.byte   $42                             ; 9A71 42                       B
L9A72:  lda     #$00                            ; 9A72 A9 00                    ..
	sta     $A3                             ; 9A74 85 A3                    ..
	lda     #$00                            ; 9A76 A9 00                    ..
	sta     $A5                             ; 9A78 85 A5                    ..
	lda     L9051                           ; 9A7A AD 51 90                 .Q.
	sta     $A4                             ; 9A7D 85 A4                    ..
	ldy     #$44                            ; 9A7F A0 44                    .D
	ldx     #$9A                            ; 9A81 A2 9A                    ..
	lda     #$6F                            ; 9A83 A9 6F                    .o
	jsr     sub_55A0
	lda     L9051                           ; 9A88 AD 51 90                 .Q.
	jsr     sub_9427
	lda     #$00                            ; 9A8E A9 00                    ..
	jsr     L961E                           ; 9A90 20 1E 96                  ..
L9A93:  jmp     L9BCE                           ; 9A93 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9A96:  lda     L979C                           ; 9A96 AD 9C 97                 ...
	eor     #$77                            ; 9A99 49 77                    Iw
	beq     L9AA0                           ; 9A9B F0 03                    ..
	jmp     L9AE8                           ; 9A9D 4C E8 9A                 L..

; ----------------------------------------------------------------------------
L9AA0:  ldx     L9051                           ; 9AA0 AE 51 90                 .Q.
	lda     L9053                           ; 9AA3 AD 53 90                 .S.
	jsr     L925D                           ; 9AA6 20 5D 92                  ].
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
	.byte   $02                             ; 9AC1 02                       .
	.byte   $43                             ; 9AC2 43                       C
	.byte   $42                             ; 9AC3 42                       B
L9AC4:  lda     #$00                            ; 9AC4 A9 00                    ..
	sta     $A3                             ; 9AC6 85 A3                    ..
	lda     #$00                            ; 9AC8 A9 00                    ..
	sta     $A5                             ; 9ACA 85 A5                    ..
	lda     L9051                           ; 9ACC AD 51 90                 .Q.
	sta     $A4                             ; 9ACF 85 A4                    ..
	ldy     #$49                            ; 9AD1 A0 49                    .I
	ldx     #$9A                            ; 9AD3 A2 9A                    ..
	lda     #$C1                            ; 9AD5 A9 C1                    ..
	jsr     sub_55A0
	lda     L9051                           ; 9ADA AD 51 90                 .Q.
	jsr     sub_9427
	lda     #$00                            ; 9AE0 A9 00                    ..
	jsr     L961E                           ; 9AE2 20 1E 96                  ..
L9AE5:  jmp     L9BCE                           ; 9AE5 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9AE8:  lda     L979C                           ; 9AE8 AD 9C 97                 ...
	eor     #$76                            ; 9AEB 49 76                    Iv
	lbne	L9B05
	ldy     #$00                            ; 9AF2 A0 00                    ..
	sty     L9050                           ; 9AF4 8C 50 90                 .P.
	lda     L9051                           ; 9AF7 AD 51 90                 .Q.
	jsr     sub_9427 
	lda     #$00                            ; 9AFD A9 00                    ..
	jsr     L961E                           ; 9AFF 20 1E 96                  ..
	jmp     L9BCE                           ; 9B02 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B05:  lda     L979D                           ; 9B05 AD 9D 97                 ...
	jsr     L4B7B                           ; 9B08 20 7B 4B                  {K
	lda     $A0                             ; 9B0B A5 A0                    ..
	and     #$7F                            ; 9B0D 29 7F                    ).
	sta     L979D                           ; 9B0F 8D 9D 97                 ...
	lda     L905D                           ; 9B12 AD 5D 90                 .].
	eor     L9060                           ; 9B15 4D 60 90                 M`.
	lbne	L9B23
L9B1D:  jsr     L4F5A                           ; 9B1D 20 5A 4F                  ZO
	jmp     L9BCE                           ; 9B20 4C CE 9B                 L..

; ----------------------------------------------------------------------------
L9B23:  sec                                     ; 9B23 38                       8
	lda     L9060                           ; 9B24 AD 60 90                 .`.
	sbc     #$01                            ; 9B27 E9 01                    ..
	sta     $AE                             ; 9B29 85 AE                    ..
	lda     L905E                           ; 9B2B AD 5E 90                 .^.
	eor     $AE                             ; 9B2E 45 AE                    E.
	lbne	L9B57
	clc                                     ; 9B35 18                       .
	lda     L9059                           ; 9B36 AD 59 90                 .Y.
	adc     L905E                           ; 9B39 6D 5E 90                 m^.
	sta     $AE                             ; 9B3C 85 AE                    ..
	lda     L905A                           ; 9B3E AD 5A 90                 .Z.
	adc     #$00                            ; 9B41 69 00                    i.
	sta     $AF                             ; 9B43 85 AF                    ..
	lda     L979D                           ; 9B45 AD 9D 97                 ...
	eor     #$80                            ; 9B48 49 80                    I.
	ldy     #$00                            ; 9B4A A0 00                    ..
	sta     ($AE),y                         ; 9B4C 91 AE                    ..
	lda     L9060                           ; 9B4E AD 60 90                 .`.
	sta     L905D                           ; 9B51 8D 5D 90                 .].
	jmp     L9BC6                           ; 9B54 4C C6 9B                 L..

; ----------------------------------------------------------------------------
L9B57:  clc                                     ; 9B57 18                       .
	lda     L9059                           ; 9B58 AD 59 90                 .Y.
	adc     L905E                           ; 9B5B 6D 5E 90                 m^.
	sta     L979F                           ; 9B5E 8D 9F 97                 ...
	lda     L905A                           ; 9B61 AD 5A 90                 .Z.
	adc     #$00                            ; 9B64 69 00                    i.
	sta     L97A0                           ; 9B66 8D A0 97                 ...
	clc                                     ; 9B69 18                       .
	lda     L979F                           ; 9B6A AD 9F 97                 ...
	adc     #$01                            ; 9B6D 69 01                    i.
	sta     $A0                             ; 9B6F 85 A0                    ..
	lda     L97A0                           ; 9B71 AD A0 97                 ...
	adc     #$00                            ; 9B74 69 00                    i.
	sta     $A1                             ; 9B76 85 A1                    ..
	lda     L97A0                           ; 9B78 AD A0 97                 ...
	sta     $A3                             ; 9B7B 85 A3                    ..
	sec                                     ; 9B7D 38                       8
	lda     L9060                           ; 9B7E AD 60 90                 .`.
	sbc     L905E                           ; 9B81 ED 5E 90                 .^.
	sta     $AC                             ; 9B84 85 AC                    ..
	sec                                     ; 9B86 38                       8
	lda     $AC                             ; 9B87 A5 AC                    ..
	sbc     #$01                            ; 9B89 E9 01                    ..
	sta     $A4                             ; 9B8B 85 A4                    ..
	lda     #$00                            ; 9B8D A9 00                    ..
	sta     $A5                             ; 9B8F 85 A5                    ..
	ldy     L979F                           ; 9B91 AC 9F 97                 ...
	ldx     $A1                             ; 9B94 A6 A1                    ..
	lda     $A0                             ; 9B96 A5 A0                    ..
	jsr     L4EB1                           ; 9B98 20 B1 4E                  .N
	clc                                     ; 9B9B 18                       .
	lda     L9059                           ; 9B9C AD 59 90                 .Y.
	adc     L905E                           ; 9B9F 6D 5E 90                 m^.
	sta     $AE                             ; 9BA2 85 AE                    ..
	lda     L905A                           ; 9BA4 AD 5A 90                 .Z.
	adc     #$00                            ; 9BA7 69 00                    i.
	sta     $AF                             ; 9BA9 85 AF                    ..
	lda     L979D                           ; 9BAB AD 9D 97                 ...
	ldy     #$00                            ; 9BAE A0 00                    ..
	sta     ($AE),y                         ; 9BB0 91 AE                    ..
	ldx     L905E                           ; 9BB2 AE 5E 90                 .^.
	lda     L905D                           ; 9BB5 AD 5D 90                 .].
	jsr     L4983                           ; 9BB8 20 83 49                  .I
	lda     $A0                             ; 9BBB A5 A0                    ..
	sta     L905D                           ; 9BBD 8D 5D 90                 .].
	inc     L905D                           ; 9BC0 EE 5D 90                 .].
	inc     L905E                           ; 9BC3 EE 5E 90                 .^.
L9BC6:  jsr     L936A                           ; 9BC6 20 6A 93                  j.
	ldy     #$01                            ; 9BC9 A0 01                    ..
	sty     L9050                           ; 9BCB 8C 50 90                 .P.
L9BCE:  rts                                     ; 9BCE 60                       `

; ----------------------------------------------------------------------------
L9BCF:  brk                                     ; 9BCF 00                       .

L9BD0:  prolog
	sta     L9BCF                           ; 9BD3 8D CF 9B                 ...
	lda     L9BCF                           ; 9BD6 AD CF 9B                 ...
	sta     L46E7                           ; 9BD9 8D E7 46                 ..F
	rts                                     ; 9BDC 60                       `

; ----------------------------------------------------------------------------
L9BDD:  brk                                     ; 9BDD 00                       .
L9BDE:  brk                                     ; 9BDE 00                       .
L9BDF:  brk                                     ; 9BDF 00                       .

L9BE0:	stack_prolog L9BDD, $02
	clc                                     ; 9BE9 18                       .
	lda     L46EB                           ; 9BEA AD EB 46                 ..F
	adc     L9BDD                           ; 9BED 6D DD 9B                 m..
	sta     $AE                             ; 9BF0 85 AE                    ..
	lda     L46EB+1                         ; 9BF2 AD EC 46                 ..F
	adc     #$00                            ; 9BF5 69 00                    i.
	sta     $AF                             ; 9BF7 85 AF                    ..
	lda     L9BDE                           ; 9BF9 AD DE 9B                 ...
	ldy     #$00                            ; 9BFC A0 00                    ..
	sta     ($AE),y                         ; 9BFE 91 AE                    ..
	clc                                     ; 9C00 18                       .
	lda     L46ED                           ; 9C01 AD ED 46                 ..F
	adc     L9BDD                           ; 9C04 6D DD 9B                 m..
	sta     $AE                             ; 9C07 85 AE                    ..
	lda     L46EE                           ; 9C09 AD EE 46                 ..F
	adc     #$00                            ; 9C0C 69 00                    i.
	sta     $AF                             ; 9C0E 85 AF                    ..
	lda     L9BDF                           ; 9C10 AD DF 9B                 ...
	sta     ($AE),y                         ; 9C13 91 AE                    ..
	rts                                     ; 9C15 60                       `

; ----------------------------------------------------------------------------
L9C16:  brk                                     ; 9C16 00                       .
L9C17:  brk                                     ; 9C17 00                       .
L9C18:  brk                                     ; 9C18 00                       .
	brk                                     ; 9C19 00                       .
	brk                                     ; 9C1A 00                       .
	ora     ($01,x)                         ; 9C1B 01 01                    ..
	ora     ($00,x)                         ; 9C1D 01 00                    ..
	.byte   $FF                             ; 9C1F FF                       .
	.byte   $FF                             ; 9C20 FF                       .
	.byte   $FF                             ; 9C21 FF                       .
L9C22:  .byte   $19                             ; 9C22 19                       .
L9C23:  .byte   $9C                             ; 9C23 9C                       .
	brk                                     ; 9C24 00                       .
	.byte   $FF                             ; 9C25 FF                       .
	.byte   $FF                             ; 9C26 FF                       .
	brk                                     ; 9C27 00                       .
	ora     ($01,x)                         ; 9C28 01 01                    ..
	ora     ($00,x)                         ; 9C2A 01 00                    ..
	.byte   $FF                             ; 9C2C FF                       .
L9C2D:  .byte   $24                             ; 9C2D 24                       $
L9C2E:  .byte   $9C                             ; 9C2E 9C                       .
L9C2F:	dec     $1E,x                           ; 9C2F D6 1E                    ..
	ora     #$00                            ; 9C31 09 00                    ..
	brk                                     ; 9C33 00                       .
	brk                                     ; 9C34 00                       .
	brk                                     ; 9C35 00                       .
	brk                                     ; 9C36 00                       .
	brk                                     ; 9C37 00                       .
	dec     $1E,x                           ; 9C38 D6 1E                    ..
	ora     #$00                            ; 9C3A 09 00                    ..
	brk                                     ; 9C3C 00                       .
	brk                                     ; 9C3D 00                       .
	brk                                     ; 9C3E 00                       .
	brk                                     ; 9C3F 00                       .
	brk                                     ; 9C40 00                       .

L9C41:  stack_prolog L9C16, $02
	lda     L9C16                           ; 9C4A AD 16 9C                 ...
	sta     L46E6                           ; 9C4D 8D E6 46                 ..F
	lda     L9C17                           ; 9C50 AD 17 9C                 ...
	sta     L46E9                           ; 9C53 8D E9 46                 ..F
	lda     L9C18                           ; 9C56 AD 18 9C                 ...
	sta     L46EA                           ; 9C59 8D EA 46                 ..F
	ldi	L46EB+1, >L9C2F
	ldi	L46EB, <L9C2F
	lda     #$9C                            ; 9C66 A9 9C                    ..
	sta     L46EE                           ; 9C68 8D EE 46                 ..F
	lda     #$38                            ; 9C6B A9 38                    .8
	sta     L46ED                           ; 9C6D 8D ED 46                 ..F
	lda     L9C23                           ; 9C70 AD 23 9C                 .#.
	sta     $A3                             ; 9C73 85 A3                    ..
	lda     #$00                            ; 9C75 A9 00                    ..
	sta     $A5                             ; 9C77 85 A5                    ..
	lda     #$09                            ; 9C79 A9 09                    ..
	sta     $A4                             ; 9C7B 85 A4                    ..
	ldy     L9C22                           ; 9C7D AC 22 9C                 .".
	ldx     #$9C                            ; 9C80 A2 9C                    ..
	lda     #$2F                            ; 9C82 A9 2F                    ./
	jsr     sub_461F
	lda     L9C2E                           ; 9C87 AD 2E 9C                 ...
	sta     $A3                             ; 9C8A 85 A3                    ..
	lda     #$00                            ; 9C8C A9 00                    ..
	sta     $A5                             ; 9C8E 85 A5                    ..
	lda     #$09                            ; 9C90 A9 09                    ..
	sta     $A4                             ; 9C92 85 A4                    ..
	ldy     L9C2D                           ; 9C94 AC 2D 9C                 .-.
	ldx     #$9C                            ; 9C97 A2 9C                    ..
	lda     #$38                            ; 9C99 A9 38                    .8
	jsr     sub_461F
	lda     L46E6                           ; 9C9E AD E6 46                 ..F
	beq     L9CA6                           ; 9CA1 F0 03                    ..
	jmp     L9CAB                           ; 9CA3 4C AB 9C                 L..

; ----------------------------------------------------------------------------
L9CA6:  lda     #$00                            ; 9CA6 A9 00                    ..
	jsr     L9BD0                           ; 9CA8 20 D0 9B                  ..
L9CAB:  rts                                     ; 9CAB 60                       `

; ----------------------------------------------------------------------------
L9CAC:  brk                                     ; 9CAC 00                       .

L9CAD:	prolog
	sta     L9CAC                           ; 9CB0 8D AC 9C                 ...
	lda     L9CAC                           ; 9CB3 AD AC 9C                 ...
	sta     L46E8                           ; 9CB6 8D E8 46                 ..F
	rts                                     ; 9CB9 60                       `

; ----------------------------------------------------------------------------
L9CBA:  brk                                     ; 9CBA 00                       .
L9CBB:  brk                                     ; 9CBB 00                       .
L9CBC:  brk                                     ; 9CBC 00                       .
L9CBD:  brk                                     ; 9CBD 00                       .
L9CBE:  .byte   $0C                             ; 9CBE 0C                       .
L9CBF:  .byte   $0C                             ; 9CBF 0C                       .
L9CC0:  .byte   $02                             ; 9CC0 02                       .
L9CC1:  brk                                     ; 9CC1 00                       .
L9CC2:  brk                                     ; 9CC2 00                       .
	.byte   $04                             ; 9CC3 04                       .
	.byte   $02                             ; 9CC4 02                       .
	.byte   $03                             ; 9CC5 03                       .
	.byte   $FF                             ; 9CC6 FF                       .
	asl     $08                             ; 9CC7 06 08                    ..
	.byte   $07                             ; 9CC9 07                       .
	.byte   $FF                             ; 9CCA FF                       .
	ora     $01                             ; 9CCB 05 01                    ..
	brk                                     ; 9CCD 00                       .
L9CCE:  .byte   $C3                             ; 9CCE C3                       .
L9CCF:  .byte   $9C                             ; 9CCF 9C                       .
L9CD0:  jmp     L9CD3                           ; 9CD0 4C D3 9C                 L..

; ----------------------------------------------------------------------------
L9CD3:  jsr     sub_44D5                           ; 9CD3 20 D5 44                  .D
	tsx                                     ; 9CD6 BA                       .
	.byte   $9C                             ; 9CD7 9C                       .
	.byte   $03                             ; 9CD8 03                       .
	lda     L46E7                           ; 9CD9 AD E7 46                 ..F
	beq     L9CE1                           ; 9CDC F0 03                    ..
	jmp     L9CEB                           ; 9CDE 4C EB 9C                 L..

; ----------------------------------------------------------------------------
L9CE1:  ldy     #$00                            ; 9CE1 A0 00                    ..
	sty     L9CC2                           ; 9CE3 8C C2 9C                 ...
	lda     #$00                            ; 9CE6 A9 00                    ..
	sta     $A0                             ; 9CE8 85 A0                    ..
	rts                                     ; 9CEA 60                       `

; ----------------------------------------------------------------------------
L9CEB:  lda     #$00                            ; 9CEB A9 00                    ..
	jsr     L45D6                           ; 9CED 20 D6 45                  .E
	sec                                     ; 9CF0 38                       8
	lda     $A0                             ; 9CF1 A5 A0                    ..
	sbc     #$05                            ; 9CF3 E9 05                    ..
	sta     L9CC1                           ; 9CF5 8D C1 9C                 ...
	clc                                     ; 9CF8 18                       .
	lda     L9CCE                           ; 9CF9 AD CE 9C                 ...
	adc     L9CC1                           ; 9CFC 6D C1 9C                 m..
	sta     $AE                             ; 9CFF 85 AE                    ..
	lda     L9CCF                           ; 9D01 AD CF 9C                 ...
	adc     #$00                            ; 9D04 69 00                    i.
	sta     $AF                             ; 9D06 85 AF                    ..
	ldy     #$00                            ; 9D08 A0 00                    ..
	lda     ($AE),y                         ; 9D0A B1 AE                    ..
	sta     L9CC1                           ; 9D0C 8D C1 9C                 ...
	lda     L9CC1                           ; 9D0F AD C1 9C                 ...
	eor     L9CC2                           ; 9D12 4D C2 9C                 M..
	beq     L9D1A                           ; 9D15 F0 03                    ..
	jmp     L9D40                           ; 9D17 4C 40 9D                 L@.

; ----------------------------------------------------------------------------
L9D1A:  lda     #$00                            ; 9D1A A9 00                    ..
	cmp     $022E                           ; 9D1C CD 2E 02                 ...
	bcc     L9D24                           ; 9D1F 90 03                    ..
	jmp     L9D29                           ; 9D21 4C 29 9D                 L).

; ----------------------------------------------------------------------------
L9D24:  lda     #$00                            ; 9D24 A9 00                    ..
	sta     $A0                             ; 9D26 85 A0                    ..
	rts                                     ; 9D28 60                       `

; ----------------------------------------------------------------------------
L9D29:  lda     L9CC0                           ; 9D29 AD C0 9C                 ...
	cmp     L9CBE                           ; 9D2C CD BE 9C                 ...
	lbcs	L9D3D
	sec                                     ; 9D34 38                       8
	lda     L9CBE                           ; 9D35 AD BE 9C                 ...
	sbc     #$01                            ; 9D38 E9 01                    ..
	sta     L9CBE                           ; 9D3A 8D BE 9C                 ...
L9D3D:  jmp     L9D46                           ; 9D3D 4C 46 9D                 LF.

; ----------------------------------------------------------------------------
L9D40:  lda     L9CBF                           ; 9D40 AD BF 9C                 ...
	sta     L9CBE                           ; 9D43 8D BE 9C                 ...
L9D46:  lda     L9CC1                           ; 9D46 AD C1 9C                 ...
	sta     L9CC2                           ; 9D49 8D C2 9C                 ...
	lda     L9CBA                           ; 9D4C AD BA 9C                 ...
	sta     $AE                             ; 9D4F 85 AE                    ..
	lda     L9CBB                           ; 9D51 AD BB 9C                 ...
	sta     $AF                             ; 9D54 85 AF                    ..
	clc                                     ; 9D56 18                       .
	lda     L46EB                           ; 9D57 AD EB 46                 ..F
	adc     L9CC1                           ; 9D5A 6D C1 9C                 m..
	sta     $AC                             ; 9D5D 85 AC                    ..
	lda     L46EB+1                         ; 9D5F AD EC 46                 ..F
	adc     #$00                            ; 9D62 69 00                    i.
	sta     $AD                             ; 9D64 85 AD                    ..
	ldy     #$00                            ; 9D66 A0 00                    ..
	lda     ($AC),y                         ; 9D68 B1 AC                    ..
	sta     ($AE),y                         ; 9D6A 91 AE                    ..
	lda     L9CBC                           ; 9D6C AD BC 9C                 ...
	sta     $AE                             ; 9D6F 85 AE                    ..
	lda     L9CBD                           ; 9D71 AD BD 9C                 ...
	sta     $AF                             ; 9D74 85 AF                    ..
	clc                                     ; 9D76 18                       .
	lda     L46ED                           ; 9D77 AD ED 46                 ..F
	adc     L9CC1                           ; 9D7A 6D C1 9C                 m..
	sta     $AC                             ; 9D7D 85 AC                    ..
	lda     L46EE                           ; 9D7F AD EE 46                 ..F
	adc     #$00                            ; 9D82 69 00                    i.
	sta     $AD                             ; 9D84 85 AD                    ..
	lda     ($AC),y                         ; 9D86 B1 AC                    ..
	sta     ($AE),y                         ; 9D88 91 AE                    ..
	lda     L9CBA                           ; 9D8A AD BA 9C                 ...
	sta     $AE                             ; 9D8D 85 AE                    ..
	lda     L9CBB                           ; 9D8F AD BB 9C                 ...
	sta     $AF                             ; 9D92 85 AF                    ..
	lda     ($AE),y                         ; 9D94 B1 AE                    ..
	lbne	L9DB4
	lda     L9CBC                           ; 9D9B AD BC 9C                 ...
	sta     $AE                             ; 9D9E 85 AE                    ..
	lda     L9CBD                           ; 9DA0 AD BD 9C                 ...
	sta     $AF                             ; 9DA3 85 AF                    ..
	lda     ($AE),y                         ; 9DA5 B1 AE                    ..
	lbne	L9DB4
	ldi	$A0, $00
	rts                                     ; 9DB0 60                       `

; ----------------------------------------------------------------------------
	jmp     L9DC9                           ; 9DB1 4C C9 9D                 L..

; ----------------------------------------------------------------------------
L9DB4:  ldy     #$01                            ; 9DB4 A0 01                    ..
	sty     $022E                           ; 9DB6 8C 2E 02                 ...
	ldi	$0221, $00
	lda     L9CBE                           ; 9DBE AD BE 9C                 ...
	sta     $0220                           ; 9DC1 8D 20 02                 . .
	ldi	$A0, $01
	rts                                     ; 9DC8 60                       `

; ----------------------------------------------------------------------------
L9DC9:  brk                                     ; 9DC9 00                       .
L9DCA:  brk                                     ; 9DCA 00                       .

L9DCB:  prolog
	lda     L46E8                           ; 9DCE AD E8 46                 ..F
	eor     #$01                            ; 9DD1 49 01                    I.
	lbeq	L9DDD
	ldi	$A0, $00
	rts                                     ; 9DDC 60                       `

; ----------------------------------------------------------------------------
L9DDD:  lda     #$00                            ; 9DDD A9 00                    ..
	jsr     L45EF                           ; 9DDF 20 EF 45                  .E
	lda     $A0                             ; 9DE2 A5 A0                    ..
	sta     L9DCA                           ; 9DE4 8D CA 9D                 ...
	lda     L9DCA                           ; 9DE7 AD CA 9D                 ...
	eor     L9DC9                           ; 9DEA 4D C9 9D                 M..
	lbne	L9DF7
	ldi	$A0, $00
	rts                                     ; 9DF6 60                       `

; ----------------------------------------------------------------------------
L9DF7:  lda     L9DCA                           ; 9DF7 AD CA 9D                 ...
	sta     L9DC9                           ; 9DFA 8D C9 9D                 ...
	lda     L9DCA                           ; 9DFD AD CA 9D                 ...
	lbeq	L9E0A
	ldi	$A0, $00
	rts                                     ; 9E09 60                       `

; ----------------------------------------------------------------------------
L9E0A:	ldi	$A0, $01
	rts                                     ; 9E0E 60                       `

; ----------------------------------------------------------------------------
L9E0F:  brk                                     ; 9E0F 00                       .
L9E10:  brk                                     ; 9E10 00                       .
L9E11:  brk                                     ; 9E11 00                       .
L9E12:  brk                                     ; 9E12 00                       .
L9E13:  brk                                     ; 9E13 00                       .
L9E14:  brk                                     ; 9E14 00                       .
L9E15:  brk                                     ; 9E15 00                       .
	brk                                     ; 9E16 00                       .
	brk                                     ; 9E17 00                       .
	brk                                     ; 9E18 00                       .
L9E19:  brk                                     ; 9E19 00                       .
L9E1A:  brk                                     ; 9E1A 00                       .
L9E1B:  brk                                     ; 9E1B 00                       .
L9E1C:  brk                                     ; 9E1C 00                       .
L9E1D:  brk                                     ; 9E1D 00                       .
L9E1E:  brk                                     ; 9E1E 00                       .
L9E1F:  brk                                     ; 9E1F 00                       .
L9E20:  brk                                     ; 9E20 00                       .
L9E21:  brk                                     ; 9E21 00                       .
	brk                                     ; 9E22 00                       .
	brk                                     ; 9E23 00                       .
	brk                                     ; 9E24 00                       .
	brk                                     ; 9E25 00                       .
	brk                                     ; 9E26 00                       .
	brk                                     ; 9E27 00                       .
L9E28:	brk                                     ; 9E28 00                       .
	brk                                     ; 9E29 00                       .
L9E2A:  brk                                     ; 9E2A 00                       .
L9E2B:  brk                                     ; 9E2B 00                       .

L9E2C:	prolog
	sta     L9E0F                           ; 9E2F 8D 0F 9E                 ...
	ldi	L9E19, $80
	ldy     #$00                            ; 9E37 A0 00                    ..
	sty     L9E1A                           ; 9E39 8C 1A 9E                 ...
	sty     L9E1B                           ; 9E3C 8C 1B 9E                 ...
	sty     L9E1C                           ; 9E3F 8C 1C 9E                 ...
	lda     L46E9                           ; 9E42 AD E9 46                 ..F
	jsr     sub_7035
	lda     $A1                             ; 9E48 A5 A1                    ..
	sta     L9E11                           ; 9E4A 8D 11 9E                 ...
	lda     $A0                             ; 9E4D A5 A0                    ..
	sta     L9E10                           ; 9E4F 8D 10 9E                 ...
	lda     L9E11                           ; 9E52 AD 11 9E                 ...
	sta     $A3                             ; 9E55 85 A3                    ..
	ldi	$A5, $00
	ldi	$A4, $05
	ldy     L9E10                           ; 9E5F AC 10 9E                 ...
	ldxai	L9E1D                           ; 9E64 A9 1D                    ..
	jsr     sub_461F
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
	lda     L9E1F                           ; 9E85 AD 1F 9E                 ...
	sta     L9E1B                           ; 9E88 8D 1B 9E                 ...
	lda     L9E20                           ; 9E8B AD 20 9E                 . .
	sta     L9E1C                           ; 9E8E 8D 1C 9E                 ...
	jmp     L9F69                           ; 9E91 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9E94:  lda     L46E6                           ; 9E94 AD E6 46                 ..F
	eor     #$04                            ; 9E97 49 04                    I.
	lbne	L9ED7
	ldx     L46EA                           ; 9E9E AE EA 46                 ..F
	lda     L46E9                           ; 9EA1 AD E9 46                 ..F
	jsr     sub_799B
	lda     $A1                             ; 9EA7 A5 A1                    ..
	sta     L9E15                           ; 9EA9 8D 15 9E                 ...
	lda     $A0                             ; 9EAC A5 A0                    ..
	sta     L9E14                           ; 9EAE 8D 14 9E                 ...
	lda     L9E15                           ; 9EB1 AD 15 9E                 ...
	sta     $A3                             ; 9EB4 85 A3                    ..
	ldi	$A5, $00
	ldi	$A4, $04
	ldy     L9E14                           ; 9EBE AC 14 9E                 ...
	ldxai	L9E28
	jsr     sub_461F
	lda     L9E2A                           ; 9EC8 AD 2A 9E                 .*.
	sta     L9E1B                           ; 9ECB 8D 1B 9E                 ...
	lda     L9E2B                           ; 9ECE AD 2B 9E                 .+.
	sta     L9E1C                           ; 9ED1 8D 1C 9E                 ...
	jmp     L9F69                           ; 9ED4 4C 69 9F                 Li.

; ----------------------------------------------------------------------------
L9ED7:  lda     L46E6                           ; 9ED7 AD E6 46                 ..F
	eor     #$03                            ; 9EDA 49 03                    I.
	lbne	L9F69
	lda     L4751                           ; 9EE1 AD 51 47                 .QG
	sta     L9E19                           ; 9EE4 8D 19 9E                 ...
	lda     L4752                           ; 9EE7 AD 52 47                 .RG
	sta     L9E1A                           ; 9EEA 8D 1A 9E                 ...
	lda     L474F                           ; 9EED AD 4F 47                 .OG
	eor     #$02                            ; 9EF0 49 02                    I.
	lbne	L9F69
	lda     L4750                           ; 9EF7 AD 50 47                 .PG
	jsr     sub_7035
	lda     $A1                             ; 9EFD A5 A1                    ..
	sta     L9E11                           ; 9EFF 8D 11 9E                 ...
	lda     $A0                             ; 9F02 A5 A0                    ..
	sta     L9E10                           ; 9F04 8D 10 9E                 ...
	lda     L9E11                           ; 9F07 AD 11 9E                 ...
	sta     $A3                             ; 9F0A 85 A3                    ..
	lda     #$00                            ; 9F0C A9 00                    ..
	sta     $A5                             ; 9F0E 85 A5                    ..
	lda     #$05                            ; 9F10 A9 05                    ..
	sta     $A4                             ; 9F12 85 A4                    ..
	ldy     L9E10                           ; 9F14 AC 10 9E                 ...
	ldx     #$9E                            ; 9F17 A2 9E                    ..
	lda     #$1D                            ; 9F19 A9 1D                    ..
	jsr     sub_461F
	lda     L9E21                           ; 9F1E AD 21 9E                 .!.
	jsr     L65B0                           ; 9F21 20 B0 65                  .e
	lda     $A1                             ; 9F24 A5 A1                    ..
	sta     L9E13                           ; 9F26 8D 13 9E                 ...
	lda     $A0                             ; 9F29 A5 A0                    ..
	sta     L9E12                           ; 9F2B 8D 12 9E                 ...
	lda     L9E13                           ; 9F2E AD 13 9E                 ...
	sta     $A3                             ; 9F31 85 A3                    ..
	lda     #$00                            ; 9F33 A9 00                    ..
	sta     $A5                             ; 9F35 85 A5                    ..
	lda     #$06                            ; 9F37 A9 06                    ..
	sta     $A4                             ; 9F39 85 A4                    ..
	ldy     L9E12                           ; 9F3B AC 12 9E                 ...
	ldx     #$9E                            ; 9F3E A2 9E                    ..
	lda     #$22                            ; 9F40 A9 22                    ."
	jsr     sub_461F
	sec                                     ; 9F45 38                       8
	lda     L9E1D                           ; 9F46 AD 1D 9E                 ...
	sbc     L9E1F                           ; 9F49 ED 1F 9E                 ...
	sta     $AE                             ; 9F4C 85 AE                    ..
	sec                                     ; 9F4E 38                       8
	lda     L4751                           ; 9F4F AD 51 47                 .QG
	sbc     $AE                             ; 9F52 E5 AE                    ..
	sta     L9E1B                           ; 9F54 8D 1B 9E                 ...
	sec                                     ; 9F57 38                       8
	lda     L9E1E                           ; 9F58 AD 1E 9E                 ...
	sbc     L9E20                           ; 9F5B ED 20 9E                 . .
	sta     $AE                             ; 9F5E 85 AE                    ..
	sec                                     ; 9F60 38                       8
	lda     L4752                           ; 9F61 AD 52 47                 .RG
	sbc     $AE                             ; 9F64 E5 AE                    ..
	sta     L9E1C                           ; 9F66 8D 1C 9E                 ...
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

L9F82:	ldi	$A3, $00
	ldi	$A5, $00
	lda     L9E1B                           ; 9F8A AD 1B 9E                 ...
	sta     $A4                             ; 9F8D 85 A4                    ..
	ldi	$A7, $00
	lda     L9E1C                           ; 9F93 AD 1C 9E                 ...
	sta     $A6                             ; 9F96 85 A6                    ..
	ldi	$A9, $00
	lda     L9E19                           ; 9F9C AD 19 9E                 ...
	sta     $A8                             ; 9F9F 85 A8                    ..
	ldi	$AB, $00
	lda     L9E1A                           ; 9FA5 AD 1A 9E                 ...
	sta     $AA                             ; 9FA8 85 AA                    ..
	ldy     L9E0F                           ; 9FAA AC 0F 9E                 ...
	ldx     #>L9F7C
	lda     #<L9F7C
	jsr     sub_55A0
	jmp     L9FDE                           ; 9FB4 4C DE 9F                 L..

; ----------------------------------------------------------------------------
L9FB7:  jmp     L9FBE                           ; 9FB7 4C BE 9F                 L..

; ----------------------------------------------------------------------------
	.byte   $03                             ; 9FBA 03                       .
	.byte   $63                             ; 9FBB 63                       c
	.byte   $42                             ; 9FBC 42                       B
	.byte   $42                             ; 9FBD 42                       B
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
	ldx     #$9F                            ; 9FD7 A2 9F                    ..
	lda     #$BA                            ; 9FD9 A9 BA                    ..
	jsr     sub_55A0
L9FDE:  lda     #$01                            ; 9FDE A9 01                    ..
	sta     $A0                             ; 9FE0 85 A0                    ..
	rts                                     ; 9FE2 60                       `

; ----------------------------------------------------------------------------
L9FE3:  brk                                     ; 9FE3 00                       .
L9FE4:  brk                                     ; 9FE4 00                       .
L9FE5:  brk                                     ; 9FE5 00                       .
L9FE6:  brk                                     ; 9FE6 00                       .
L9FE7:  brk                                     ; 9FE7 00                       .
L9FE8:  brk                                     ; 9FE8 00                       .
L9FE9:  brk                                     ; 9FE9 00                       .
L9FEA:  brk                                     ; 9FEA 00                       .
	brk                                     ; 9FEB 00                       .
	brk                                     ; 9FEC 00                       .
L9FED:  brk                                     ; 9FED 00                       .
L9FEE:  brk                                     ; 9FEE 00                       .
L9FEF:  brk                                     ; 9FEF 00                       .
L9FF0:  brk                                     ; 9FF0 00                       .
L9FF1:  brk                                     ; 9FF1 00                       .
L9FF2:  brk                                     ; 9FF2 00                       .
L9FF3:  brk                                     ; 9FF3 00                       .
L9FF4:  brk                                     ; 9FF4 00                       .
	brk                                     ; 9FF5 00                       .
	brk                                     ; 9FF6 00                       .
	brk                                     ; 9FF7 00                       .
	brk                                     ; 9FF8 00                       .
	brk                                     ; 9FF9 00                       .
	brk                                     ; 9FFA 00                       .
	brk                                     ; 9FFB 00                       .
	brk                                     ; 9FFC 00                       .
	brk                                     ; 9FFD 00                       .
	brk                                     ; 9FFE 00                       .
	brk                                     ; 9FFF 00                       .
	brk                                     ; A000 00                       .
	brk                                     ; A001 00                       .
	brk                                     ; A002 00                       .
	brk                                     ; A003 00                       .
	brk                                     ; A004 00                       .
LA005:  brk                                     ; A005 00                       .
LA006:  brk                                     ; A006 00                       .
LA007:  brk                                     ; A007 00                       .
LA008:  brk                                     ; A008 00                       .
LA009:  brk                                     ; A009 00                       .
LA00A:  brk                                     ; A00A 00                       .
LA00B:  brk                                     ; A00B 00                       .
LA00C:  brk                                     ; A00C 00                       .
LA00D:  brk                                     ; A00D 00                       .
LA00E:  brk                                     ; A00E 00                       .
	brk                                     ; A00F 00                       .
LA010:  brk                                     ; A010 00                       .
LA011:  brk                                     ; A011 00                       .
	brk                                     ; A012 00                       .
	brk                                     ; A013 00                       .
LA014:  brk                                     ; A014 00                       .
LA015:  brk                                     ; A015 00                       .
LA016:  brk                                     ; A016 00                       .
LA017:  brk                                     ; A017 00                       .
LA018:  brk                                     ; A018 00                       .
LA019:  brk                                     ; A019 00                       .
LA01A:  brk                                     ; A01A 00                       .
LA01B:  brk                                     ; A01B 00                       .
	brk                                     ; A01C 00                       .
	brk                                     ; A01D 00                       .
LA01E:  brk                                     ; A01E 00                       .
LA01F:  brk                                     ; A01F 00                       .
	brk                                     ; A020 00                       .
	brk                                     ; A021 00                       .
	brk                                     ; A022 00                       .
LA023:  brk                                     ; A023 00                       .
LA024:  brk                                     ; A024 00                       .
LA025:  brk                                     ; A025 00                       .
LA026:  brk                                     ; A026 00                       .
LA027:  jmp     LA02A                           ; A027 4C 2A A0                 L*.

; ----------------------------------------------------------------------------
LA02A:  jsr     sub_44D5                           ; A02A 20 D5 44                  .D
	.byte   $E3                             ; A02D E3                       .
	.byte   $9F                             ; A02E 9F                       .
	.byte   $03                             ; A02F 03                       .
	ldx     L9FE4                           ; A030 AE E4 9F                 ...
	lda     L9FE3                           ; A033 AD E3 9F                 ...
	jsr     sub_799B
	lda     $A1                             ; A039 A5 A1                    ..
	sta     L9FE8                           ; A03B 8D E8 9F                 ...
	lda     $A0                             ; A03E A5 A0                    ..
	sta     L9FE7                           ; A040 8D E7 9F                 ...
	lda     L9FE7                           ; A043 AD E7 9F                 ...
	ora     L9FE8                           ; A046 0D E8 9F                 ...
	beq     LA04E                           ; A049 F0 03                    ..
	jmp     LA04F                           ; A04B 4C 4F A0                 LO.

; ----------------------------------------------------------------------------
LA04E:  rts                                     ; A04E 60                       `

; ----------------------------------------------------------------------------
LA04F:  lda     L9FE8                           ; A04F AD E8 9F                 ...
	sta     $A3                             ; A052 85 A3                    ..
	lda     #$00                            ; A054 A9 00                    ..
	sta     $A5                             ; A056 85 A5                    ..
	lda     #$06                            ; A058 A9 06                    ..
	sta     $A4                             ; A05A 85 A4                    ..
	ldy     L9FE7                           ; A05C AC E7 9F                 ...
	ldx     #$A0                            ; A05F A2 A0                    ..
	lda     #$0E                            ; A061 A9 0E                    ..
	jsr     sub_461F
	clc                                     ; A066 18                       .
	lda     LA010                           ; A067 AD 10 A0                 ...
	adc     L9FE5                           ; A06A 6D E5 9F                 m..
	sta     L9FF3                           ; A06D 8D F3 9F                 ...
	clc                                     ; A070 18                       .
	lda     LA011                           ; A071 AD 11 A0                 ...
	adc     L9FE6                           ; A074 6D E6 9F                 m..
	sta     L9FF4                           ; A077 8D F4 9F                 ...
	lda     LA00E                           ; A07A AD 0E A0                 ...
	jsr     L65B0                           ; A07D 20 B0 65                  .e
	lda     $A1                             ; A080 A5 A1                    ..
	sta     L9FEE                           ; A082 8D EE 9F                 ...
	lda     $A0                             ; A085 A5 A0                    ..
	sta     L9FED                           ; A087 8D ED 9F                 ...
	lda     L9FEE                           ; A08A AD EE 9F                 ...
	sta     $A3                             ; A08D 85 A3                    ..
	lda     #$00                            ; A08F A9 00                    ..
	sta     $A5                             ; A091 85 A5                    ..
	lda     #$06                            ; A093 A9 06                    ..
	sta     $A4                             ; A095 85 A4                    ..
	ldy     L9FED                           ; A097 AC ED 9F                 ...
	ldx     #$A0                            ; A09A A2 A0                    ..
	lda     #$14                            ; A09C A9 14                    ..
	jsr     sub_461F
	lda     L9FE3                           ; A0A1 AD E3 9F                 ...
	.byte   $20                             ; A0A4 20                        
LA0A5:  bcs     LA10C                           ; A0A5 B0 65                    .e
	lda     $A1                             ; A0A7 A5 A1                    ..
	sta     L9FEA                           ; A0A9 8D EA 9F                 ...
	lda     $A0                             ; A0AC A5 A0                    ..
	sta     L9FE9                           ; A0AE 8D E9 9F                 ...
	lda     L9FEA                           ; A0B1 AD EA 9F                 ...
	sta     $A3                             ; A0B4 85 A3                    ..
	lda     #$00                            ; A0B6 A9 00                    ..
	sta     $A5                             ; A0B8 85 A5                    ..
	lda     #$09                            ; A0BA A9 09                    ..
	sta     $A4                             ; A0BC 85 A4                    ..
	ldy     L9FE9                           ; A0BE AC E9 9F                 ...
	ldx     #$A0                            ; A0C1 A2 A0                    ..
	lda     #$1A                            ; A0C3 A9 1A                    ..
	jsr     sub_461F
	ldy     #$01                            ; A0C8 A0 01                    ..
	sty     LA006                           ; A0CA 8C 06 A0                 ...
	lda     #$00                            ; A0CD A9 00                    ..
	sta     $A3                             ; A0CF 85 A3                    ..
	sec                                     ; A0D1 38                       8
	lda     LA01A                           ; A0D2 AD 1A A0                 ...
	sbc     #$01                            ; A0D5 E9 01                    ..
	sta     $A4                             ; A0D7 85 A4                    ..
	sec                                     ; A0D9 38                       8
	lda     LA01B                           ; A0DA AD 1B A0                 ...
	sbc     #$01                            ; A0DD E9 01                    ..
	sta     $A5                             ; A0DF 85 A5                    ..
	ldy     #$00                            ; A0E1 A0 00                    ..
	ldx     #$9F                            ; A0E3 A2 9F                    ..
	lda     #$F5                            ; A0E5 A9 F5                    ..
	jsr     sub_4BF2
	lda     #$00                            ; A0EA A9 00                    ..
	sta     $A3                             ; A0EC 85 A3                    ..
	sec                                     ; A0EE 38                       8
	lda     LA014                           ; A0EF AD 14 A0                 ...
	sbc     #$01                            ; A0F2 E9 01                    ..
	sta     $A4                             ; A0F4 85 A4                    ..
	sec                                     ; A0F6 38                       8
	lda     LA015                           ; A0F7 AD 15 A0                 ...
	sbc     #$01                            ; A0FA E9 01                    ..
	sta     $A5                             ; A0FC 85 A5                    ..
	ldy     #$00                            ; A0FE A0 00                    ..
	ldx     #$9F                            ; A100 A2 9F                    ..
	lda     #$F9                            ; A102 A9 F9                    ..
	jsr     sub_4BF2
	lda     #$9F                            ; A107 A9 9F                    ..
	sta     $A3                             ; A109 85 A3                    ..
	.byte   $AD                             ; A10B AD                       .
LA10C:  .byte   $F3                             ; A10C F3                       .
	.byte   $9F                             ; A10D 9F                       .
	sta     $A4                             ; A10E 85 A4                    ..
	lda     L9FF4                           ; A110 AD F4 9F                 ...
	sta     $A5                             ; A113 85 A5                    ..
	ldy     #$F9                            ; A115 A0 F9                    ..
	ldx     #$9F                            ; A117 A2 9F                    ..
	lda     #$F9                            ; A119 A9 F9                    ..
	jsr     L4C1D                           ; A11B 20 1D 4C                  .L
	lda     #$9F                            ; A11E A9 9F                    ..
	sta     $A3                             ; A120 85 A3                    ..
	lda     #$00                            ; A122 A9 00                    ..
	sta     $A5                             ; A124 85 A5                    ..
	lda     #$04                            ; A126 A9 04                    ..
	sta     $A4                             ; A128 85 A4                    ..
	ldy     #$F9                            ; A12A A0 F9                    ..
	ldx     #$A0                            ; A12C A2 A0                    ..
	lda     #$23                            ; A12E A9 23                    .#
	jsr     sub_461F
	lda     #$9F                            ; A133 A9 9F                    ..
	sta     $A3                             ; A135 85 A3                    ..
	lda     #$A0                            ; A137 A9 A0                    ..
	sta     $A5                             ; A139 85 A5                    ..
	lda     #$01                            ; A13B A9 01                    ..
	sta     $A4                             ; A13D 85 A4                    ..
	ldy     #$F9                            ; A13F A0 F9                    ..
	ldx     #$9F                            ; A141 A2 9F                    ..
	lda     #$F5                            ; A143 A9 F5                    ..
	jsr     L4CF5                           ; A145 20 F5 4C                  .L
	lda     $A0                             ; A148 A5 A0                    ..
	sta     LA005                           ; A14A 8D 05 A0                 ...
	lda     LA005                           ; A14D AD 05 A0                 ...
	beq     LA155                           ; A150 F0 03                    ..
	jmp     LA156                           ; A152 4C 56 A1                 LV.

; ----------------------------------------------------------------------------
LA155:  rts                                     ; A155 60                       `

; ----------------------------------------------------------------------------
LA156:  ldx     #$A0                            ; A156 A2 A0                    ..
	lda     #$01                            ; A158 A9 01                    ..
	jsr     L4E4A                           ; A15A 20 4A 4E                  JN
	lda     $A1                             ; A15D A5 A1                    ..
	sta     LA008                           ; A15F 8D 08 A0                 ...
	lda     $A0                             ; A162 A5 A0                    ..
	sta     LA007                           ; A164 8D 07 A0                 ...
	lda     LA007                           ; A167 AD 07 A0                 ...
	eor     LA016                           ; A16A 4D 16 A0                 M..
	bne     LA175                           ; A16D D0 06                    ..
	ora     LA008                           ; A16F 0D 08 A0                 ...
	eor     LA017                           ; A172 4D 17 A0                 M..
LA175:  bne     LA17A                           ; A175 D0 03                    ..
	jmp     LA17B                           ; A177 4C 7B A1                 L{.

; ----------------------------------------------------------------------------
LA17A:  rts                                     ; A17A 60                       `

; ----------------------------------------------------------------------------
LA17B:  lda     LA018                           ; A17B AD 18 A0                 ...
	sta     $AE                             ; A17E 85 AE                    ..
	lda     LA019                           ; A180 AD 19 A0                 ...
	sta     $AF                             ; A183 85 AF                    ..
	ldy     #$01                            ; A185 A0 01                    ..
	lda     ($AE),y                         ; A187 B1 AE                    ..
	sta     L9FF0                           ; A189 8D F0 9F                 ...
	dey                                     ; A18C 88                       .
	lda     ($AE),y                         ; A18D B1 AE                    ..
	sta     L9FEF                           ; A18F 8D EF 9F                 ...
	lda     LA024                           ; A192 AD 24 A0                 .$.
	asl     a                               ; A195 0A                       .
	php                                     ; A196 08                       .
	clc                                     ; A197 18                       .
	adc     LA01E                           ; A198 6D 1E A0                 m..
	sta     $AE                             ; A19B 85 AE                    ..
	lda     #$00                            ; A19D A9 00                    ..
	rol     a                               ; A19F 2A                       *
	plp                                     ; A1A0 28                       (
	adc     LA01F                           ; A1A1 6D 1F A0                 m..
	sta     $AF                             ; A1A4 85 AF                    ..
	iny                                     ; A1A6 C8                       .
	lda     ($AE),y                         ; A1A7 B1 AE                    ..
	sta     L9FF2                           ; A1A9 8D F2 9F                 ...
	dey                                     ; A1AC 88                       .
	lda     ($AE),y                         ; A1AD B1 AE                    ..
	sta     L9FF1                           ; A1AF 8D F1 9F                 ...
	sty     LA009                           ; A1B2 8C 09 A0                 ...
	lda     LA024                           ; A1B5 AD 24 A0                 .$.
	sta     LA00B                           ; A1B8 8D 0B A0                 ...
	lda     LA026                           ; A1BB AD 26 A0                 .&.
	sta     LA1CC                           ; A1BE 8D CC A1                 ...
LA1C1:  lda     LA1CC                           ; A1C1 AD CC A1                 ...
	cmp     LA00B                           ; A1C4 CD 0B A0                 ...
	bcs     LA1CD                           ; A1C7 B0 04                    ..
	jmp     LA25D                           ; A1C9 4C 5D A2                 L].

; ----------------------------------------------------------------------------
LA1CC:  brk                                     ; A1CC 00                       .
LA1CD:  lda     LA023                           ; A1CD AD 23 A0                 .#.
	sta     LA00A                           ; A1D0 8D 0A A0                 ...
	lda     LA025                           ; A1D3 AD 25 A0                 .%.
	sta     LA1E4                           ; A1D6 8D E4 A1                 ...
LA1D9:  lda     LA1E4                           ; A1D9 AD E4 A1                 ...
	cmp     LA00A                           ; A1DC CD 0A A0                 ...
	bcs     LA1E5                           ; A1DF B0 04                    ..
	jmp     LA245                           ; A1E1 4C 45 A2                 LE.

; ----------------------------------------------------------------------------
LA1E4:  brk                                     ; A1E4 00                       .
LA1E5:  clc                                     ; A1E5 18                       .
	lda     L9FEF                           ; A1E6 AD EF 9F                 ...
	adc     LA009                           ; A1E9 6D 09 A0                 m..
	sta     $AE                             ; A1EC 85 AE                    ..
	lda     L9FF0                           ; A1EE AD F0 9F                 ...
	adc     #$00                            ; A1F1 69 00                    i.
	sta     $AF                             ; A1F3 85 AF                    ..
	ldy     #$00                            ; A1F5 A0 00                    ..
	lda     ($AE),y                         ; A1F7 B1 AE                    ..
	sta     LA00C                           ; A1F9 8D 0C A0                 ...
	ldy     LA00C                           ; A1FC AC 0C A0                 ...
	ldx     L9FE3                           ; A1FF AE E3 9F                 ...
	lda     #$04                            ; A202 A9 04                    ..
	jsr     L7E24                           ; A204 20 24 7E                  $~
	lda     $A0                             ; A207 A5 A0                    ..
	beq     LA20E                           ; A209 F0 03                    ..
	jmp     LA23C                           ; A20B 4C 3C A2                 L<.

; ----------------------------------------------------------------------------
LA20E:  clc                                     ; A20E 18                       .
	lda     L9FF1                           ; A20F AD F1 9F                 ...
	adc     LA00A                           ; A212 6D 0A A0                 m..
	sta     $AE                             ; A215 85 AE                    ..
	lda     L9FF2                           ; A217 AD F2 9F                 ...
	adc     #$00                            ; A21A 69 00                    i.
	sta     $AF                             ; A21C 85 AF                    ..
	ldy     #$00                            ; A21E A0 00                    ..
	lda     ($AE),y                         ; A220 B1 AE                    ..
	sta     LA00D                           ; A222 8D 0D A0                 ...
	ldx     LA00D                           ; A225 AE 0D A0                 ...
	lda     L9FE3                           ; A228 AD E3 9F                 ...
	jsr     sub_7F93
	lda     $A0                             ; A22E A5 A0                    ..
	eor     #$01                            ; A230 49 01                    I.
	beq     LA237                           ; A232 F0 03                    ..
	jmp     LA23C                           ; A234 4C 3C A2                 L<.

; ----------------------------------------------------------------------------
LA237:  ldy     #$00                            ; A237 A0 00                    ..
	sty     LA006                           ; A239 8C 06 A0                 ...
LA23C:  inc     LA009                           ; A23C EE 09 A0                 ...
	inc     LA00A                           ; A23F EE 0A A0                 ...
	jmp     LA1D9                           ; A242 4C D9 A1                 L..

; ----------------------------------------------------------------------------
LA245:  clc                                     ; A245 18                       .
	lda     L9FF1                           ; A246 AD F1 9F                 ...
	adc     LA01A                           ; A249 6D 1A A0                 m..
	sta     L9FF1                           ; A24C 8D F1 9F                 ...
	lda     L9FF2                           ; A24F AD F2 9F                 ...
	adc     #$00                            ; A252 69 00                    i.
	sta     L9FF2                           ; A254 8D F2 9F                 ...
	inc     LA00B                           ; A257 EE 0B A0                 ...
	jmp     LA1C1                           ; A25A 4C C1 A1                 L..

; ----------------------------------------------------------------------------
LA25D:  lda     LA006                           ; A25D AD 06 A0                 ...
	eor     #$01                            ; A260 49 01                    I.
	beq     LA267                           ; A262 F0 03                    ..
	jmp     LA28B                           ; A264 4C 8B A2                 L..

; ----------------------------------------------------------------------------
LA267:  lda     L9FF4                           ; A267 AD F4 9F                 ...
	sta     $A3                             ; A26A 85 A3                    ..
	ldy     L9FF3                           ; A26C AC F3 9F                 ...
	ldx     L9FE4                           ; A26F AE E4 9F                 ...
	lda     L9FE3                           ; A272 AD E3 9F                 ...
	jsr     sub_7ADF                        ; A275 20 DF 7A                  .z
	lda     L474F                           ; A278 AD 4F 47                 .OG
	eor     #$03                            ; A27B 49 03                    I.
	lbne	LA28B
	ldxa	L9FE5
	jsr     sub_81F2
LA28B:  rts                                     ; A28B 60                       `

; ----------------------------------------------------------------------------
LA28C:  .byte	$00

; ----------------------------------------------------------------------------
LA28D:  prolog
	sta     LA28C                           ; A290 8D 8C A2                 ...
	lda     #$00                            ; A293 A9 00                    ..
	ldx     LA28C                           ; A295 AE 8C A2                 ...
	sta     $05C0,x                         ; A298 9D C0 05                 ...
LA29B:  lda     LA28C                           ; A29B AD 8C A2                 ...
	jsr     L4569                           ; A29E 20 69 45                  iE
	rts                                     ; A2A1 60                       `

; ----------------------------------------------------------------------------
LA2A2:  brk                                     ; A2A2 00                       .
LA2A3:  brk                                     ; A2A3 00                       .
LA2A4:  brk                                     ; A2A4 00                       .
LA2A5:  brk                                     ; A2A5 00                       .
LA2A6:  brk                                     ; A2A6 00                       .
LA2A7:  brk                                     ; A2A7 00                       .

; ----------------------------------------------------------------------------
sub_A2A8:	
	stack_prolog LA2A2, $03
	jsr     sub_5E1E
	lda     LA2A2                           ; A2B4 AD A2 A2                 ...
	jsr     LA28D                           ; A2B7 20 8D A2                  ..
	lda     #$04                            ; A2BA A9 04                    ..
	sta     LA2A6                           ; A2BC 8D A6 A2                 ...
	lda     LA2A3                           ; A2BF AD A3 A2                 ...
	eor     #$01                            ; A2C2 49 01                    I.
	lbne	LA2CE
	ldi	LA2A6, $08
LA2CE:  lda     #$00                            ; A2CE A9 00                    ..
	ldx     LA2A2                           ; A2D0 AE A2 A2                 ...
	sta     $05C0,x                         ; A2D3 9D C0 05                 ...
	add16i	off_AE, LA2A4, $0002
	ldy     #$00                            ; A2E5 A0 00                    ..
	lda     ($AE),y                         ; A2E7 B1 AE                    ..
	sta     LA2A7                           ; A2E9 8D A7 A2                 ...
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
	jsr     L4539                           ; A31C 20 39 45                  9E
	lda     LA2A7                           ; A31F AD A7 A2                 ...
	eor     #$3F                            ; A322 49 3F                    I?
	beq     LA329                           ; A324 F0 03                    ..
	jmp     LA33E                           ; A326 4C 3E A3                 L>.

; ----------------------------------------------------------------------------
LA329:  clc                                     ; A329 18                       .
	lda     LA2A4                           ; A32A AD A4 A2                 ...
	adc     #$02                            ; A32D 69 02                    i.
	sta     $AE                             ; A32F 85 AE                    ..
	lda     LA2A5                           ; A331 AD A5 A2                 ...
	adc     #$00                            ; A334 69 00                    i.
	sta     $AF                             ; A336 85 AF                    ..
	lda     #$3F                            ; A338 A9 3F                    .?
	ldy     #$00                            ; A33A A0 00                    ..
	sta     ($AE),y                         ; A33C 91 AE                    ..
LA33E:  lda     LA2A3                           ; A33E AD A3 A2                 ...
	eor     #$01                            ; A341 49 01                    I.
	bne     LA348                           ; A343 D0 03                    ..
	jmp     LA36C                           ; A345 4C 6C A3                 Ll.

; ----------------------------------------------------------------------------
LA348:  lda     #$7F                            ; A348 A9 7F                    ..
	.byte   $CD                             ; A34A CD                       .
	.byte   $A4                             ; A34B A4                       .
LA34C:  lsr     a                               ; A34C 4A                       J
	bcc     LA352                           ; A34D 90 03                    ..
	jmp     LA36C                           ; A34F 4C 6C A3                 Ll.

; ----------------------------------------------------------------------------
LA352:  lda     LA2A2                           ; A352 AD A2 A2                 ...
	asl     a                               ; A355 0A                       .
	asl     a                               ; A356 0A                       .
	asl     a                               ; A357 0A                       .
	asl     a                               ; A358 0A                       .
	sta     $A1                             ; A359 85 A1                    ..
	ldx     $A1                             ; A35B A6 A1                    ..
	lda     L4AA4                           ; A35D AD A4 4A                 ..J
	jsr     sub_5E5E                        ; A360 20 5E 5E                  ^^
	lda     LA2A2                           ; A363 AD A2 A2                 ...
	jsr     LA28D                           ; A366 20 8D A2                  ..
	jmp     LA37D                           ; A369 4C 7D A3                 L}.

; ----------------------------------------------------------------------------
LA36C:  lda     LA2A3                           ; A36C AD A3 A2                 ...
	eor     #$02                            ; A36F 49 02                    I.
	beq     LA376                           ; A371 F0 03                    ..
	jmp     LA37D                           ; A373 4C 7D A3                 L}.

; ----------------------------------------------------------------------------
LA376:  lda     LA2A2                           ; A376 AD A2 A2                 ...
	sta     L4652                           ; A379 8D 52 46                 .RF
	rts                                     ; A37C 60                       `

; ----------------------------------------------------------------------------
LA37D:  jsr     L5E30                           ; A37D 20 30 5E                  0^
	rts                                     ; A380 60                       `

; ----------------------------------------------------------------------------
LA381:  brk                                     ; A381 00                       .

LA382:	prolog
LA385:	sta     LA381                           ; A385 8D 81 A3                 ...
	jsr     sub_5E1E
	lda     LA381                           ; A38B AD 81 A3                 ...
	jsr     LA28D                           ; A38E 20 8D A2                  ..
	lda     L4652                           ; A391 AD 52 46                 .RF
	eor     LA381                           ; A394 4D 81 A3                 M..
	lbne	LA3A1
	lda     #$02                            ; A39C A9 02                    ..
	sta     L4652                           ; A39E 8D 52 46                 .RF
LA3A1:  jsr     L5E30                           ; A3A1 20 30 5E                  0^
	rts                                     ; A3A4 60                       `

; ----------------------------------------------------------------------------
LA3A5:  brk                                     ; A3A5 00                       .
LA3A6:  brk                                     ; A3A6 00                       .
LA3A7:  brk                                     ; A3A7 00                       .
LA3A8:  brk                                     ; A3A8 00                       .
LA3A9:  brk                                     ; A3A9 00                       .
LA3AA:  brk                                     ; A3AA 00                       .
LA3AB:  brk                                     ; A3AB 00                       .
LA3AC:  brk                                     ; A3AC 00                       .
LA3AD:  brk                                     ; A3AD 00                       .
LA3AE:  brk                                     ; A3AE 00                       .
LA3AF:  brk                                     ; A3AF 00                       .
LA3B0:  brk                                     ; A3B0 00                       .
LA3B1:  brk                                     ; A3B1 00                       .
LA3B2:  brk                                     ; A3B2 00                       .
LA3B3:  brk                                     ; A3B3 00                       .
LA3B4:  brk                                     ; A3B4 00                       .
LA3B5:  brk                                     ; A3B5 00                       .
LA3B6:  brk                                     ; A3B6 00                       .
LA3B7:  brk                                     ; A3B7 00                       .
	brk                                     ; A3B8 00                       .
	brk                                     ; A3B9 00                       .
	brk                                     ; A3BA 00                       .
LA3BB:  brk                                     ; A3BB 00                       .
LA3BC:  brk                                     ; A3BC 00                       .

LA3BD:	
	stack_prolog LA3A5, $04
	lda     LA3A5                           ; A3C6 AD A5 A3                 ...
	jsr     L65B0                           ; A3C9 20 B0 65                  .e
	lda     $A1                             ; A3CC A5 A1                    ..
	sta     LA3AD                           ; A3CE 8D AD A3                 ...
	lda     $A0                             ; A3D1 A5 A0                    ..
	sta     LA3AC                           ; A3D3 8D AC A3                 ...
	lda     LA3AC                           ; A3D6 AD AC A3                 ...
	ora     LA3AD                           ; A3D9 0D AD A3                 ...
	lbne	LA3E2
	rts                                     ; A3E1 60                       `

; ----------------------------------------------------------------------------
LA3E2:  jsr     sub_5E1E
	lda     LA3A9                           ; A3E5 AD A9 A3                 ...
	sta     $A3                             ; A3E8 85 A3                    ..
	lda     #$00                            ; A3EA A9 00                    ..
	sta     $A5                             ; A3EC 85 A5                    ..
	lda     #$04                            ; A3EE A9 04                    ..
	sta     $A4                             ; A3F0 85 A4                    ..
	ldy     LA3A8                           ; A3F2 AC A8 A3                 ...
	ldx     #$A3                            ; A3F5 A2 A3                    ..
	lda     #$B3                            ; A3F7 A9 B3                    ..
	jsr     sub_461F
	sec                                     ; A3FC 38                       8
	lda     LA3B5                           ; A3FD AD B5 A3                 ...
	sbc     LA3B3                           ; A400 ED B3 A3                 ...
	sta     $AE                             ; A403 85 AE                    ..
	clc                                     ; A405 18                       .
	lda     $AE                             ; A406 A5 AE                    ..
	adc     #$01                            ; A408 69 01                    i.
	sta     LA3AE                           ; A40A 8D AE A3                 ...
	lda     LA3AD                           ; A40D AD AD A3                 ...
	sta     $A3                             ; A410 85 A3                    ..
	lda     #$00                            ; A412 A9 00                    ..
	sta     $A5                             ; A414 85 A5                    ..
	lda     #$06                            ; A416 A9 06                    ..
	sta     $A4                             ; A418 85 A4                    ..
	ldy     LA3AC                           ; A41A AC AC A3                 ...
	ldx     #$A3                            ; A41D A2 A3                    ..
	lda     #$B7                            ; A41F A9 B7                    ..
	jsr     sub_461F
	lda     LA3B4                           ; A424 AD B4 A3                 ...
	asl     a                               ; A427 0A                       .
	php                                     ; A428 08                       .
	clc                                     ; A429 18                       .
	adc     LA3BB                           ; A42A 6D BB A3                 m..
	sta     $AE                             ; A42D 85 AE                    ..
	lda     #$00                            ; A42F A9 00                    ..
	rol     a                               ; A431 2A                       *
	plp                                     ; A432 28                       (
	adc     LA3BC                           ; A433 6D BC A3                 m..
	sta     $AF                             ; A436 85 AF                    ..
	clc                                     ; A438 18                       .
	ldy     #$00                            ; A439 A0 00                    ..
	lda     ($AE),y                         ; A43B B1 AE                    ..
	adc     LA3B3                           ; A43D 6D B3 A3                 m..
	sta     $AC                             ; A440 85 AC                    ..
	iny                                     ; A442 C8                       .
	lda     ($AE),y                         ; A443 B1 AE                    ..
	adc     #$00                            ; A445 69 00                    i.
	sta     $AD                             ; A447 85 AD                    ..
	sec                                     ; A449 38                       8
	lda     $AC                             ; A44A A5 AC                    ..
	sbc     #$01                            ; A44C E9 01                    ..
	sta     LA3AA                           ; A44E 8D AA A3                 ...
	lda     $AD                             ; A451 A5 AD                    ..
	sbc     #$00                            ; A453 E9 00                    ..
	sta     LA3AB                           ; A455 8D AB A3                 ...
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
LA475:  brk                                     ; A475 00                       .
LA476:  lda     LA3A7                           ; A476 AD A7 A3                 ...
	bne     LA47E                           ; A479 D0 03                    ..
	jmp     LA49C                           ; A47B 4C 9C A4                 L..

; ----------------------------------------------------------------------------
LA47E:  clc                                     ; A47E 18                       .
	lda     LA3AA                           ; A47F AD AA A3                 ...
	adc     #$01                            ; A482 69 01                    i.
	sta     $A0                             ; A484 85 A0                    ..
	lda     LA3AB                           ; A486 AD AB A3                 ...
	adc     #$00                            ; A489 69 00                    i.
	sta     $A1                             ; A48B 85 A1                    ..
	ldy     LA3AE                           ; A48D AC AE A3                 ...
	ldx     $A1                             ; A490 A6 A1                    ..
	lda     $A0                             ; A492 A5 A0                    ..
	jsr     sub_4B97
	lda     $A0                             ; A497 A5 A0                    ..
	sta     LA3B1                           ; A499 8D B1 A3                 ...
LA49C:  ldy     #$01                            ; A49C A0 01                    ..
	sty     LA3B0                           ; A49E 8C B0 A3                 ...
	lda     LA3B1                           ; A4A1 AD B1 A3                 ...
	sta     LA4B2                           ; A4A4 8D B2 A4                 ...
LA4A7:  lda     LA4B2                           ; A4A7 AD B2 A4                 ...
	cmp     LA3B0                           ; A4AA CD B0 A3                 ...
	bcs     LA4B3                           ; A4AD B0 04                    ..
	jmp     LA4F7                           ; A4AF 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4B2:  brk                                     ; A4B2 00                       .
LA4B3:  clc                                     ; A4B3 18                       .
	lda     LA3AA                           ; A4B4 AD AA A3                 ...
	adc     LA3B0                           ; A4B7 6D B0 A3                 m..
	sta     $AE                             ; A4BA 85 AE                    ..
	lda     LA3AB                           ; A4BC AD AB A3                 ...
	adc     #$00                            ; A4BF 69 00                    i.
	sta     $AF                             ; A4C1 85 AF                    ..
	ldy     #$00                            ; A4C3 A0 00                    ..
	lda     ($AE),y                         ; A4C5 B1 AE                    ..
	sta     LA3B2                           ; A4C7 8D B2 A3                 ...
	lda     LA3A7                           ; A4CA AD A7 A3                 ...
	bne     LA4D2                           ; A4CD D0 03                    ..
	jmp     LA4DD                           ; A4CF 4C DD A4                 L..

; ----------------------------------------------------------------------------
LA4D2:  lda     LA3B2                           ; A4D2 AD B2 A3                 ...
	jsr     L4BC9                           ; A4D5 20 C9 4B                  .K
	lda     $A0                             ; A4D8 A5 A0                    ..
	sta     LA3B2                           ; A4DA 8D B2 A3                 ...
LA4DD:  ldx     LA3B2                           ; A4DD AE B2 A3                 ...
	lda     LA3A6                           ; A4E0 AD A6 A3                 ...
	jsr     L45C7                           ; A4E3 20 C7 45                  .E
	lda     L464D                           ; A4E6 AD 4D 46                 .MF
	bne     LA4EE                           ; A4E9 D0 03                    ..
	jmp     LA4F1                           ; A4EB 4C F1 A4                 L..

; ----------------------------------------------------------------------------
LA4EE:  jmp     LA4F7                           ; A4EE 4C F7 A4                 L..

; ----------------------------------------------------------------------------
LA4F1:  inc     LA3B0                           ; A4F1 EE B0 A3                 ...
	jmp     LA4A7                           ; A4F4 4C A7 A4                 L..

; ----------------------------------------------------------------------------
LA4F7:  lda     LA3A7                           ; A4F7 AD A7 A3                 ...
	bne     LA4FF                           ; A4FA D0 03                    ..
	jmp     LA507                           ; A4FC 4C 07 A5                 L..

; ----------------------------------------------------------------------------
LA4FF:  ldx     #$9B                            ; A4FF A2 9B                    ..
	lda     LA3A6                           ; A501 AD A6 A3                 ...
	jsr     L45C7                           ; A504 20 C7 45                  .E
LA507:  lda     L464D                           ; A507 AD 4D 46                 .MF
	bne     LA50F                           ; A50A D0 03                    ..
	jmp     LA512                           ; A50C 4C 12 A5                 L..

; ----------------------------------------------------------------------------
LA50F:  jmp     LA52A                           ; A50F 4C 2A A5                 L*.

; ----------------------------------------------------------------------------
LA512:  clc                                     ; A512 18                       .
	lda     LA3AA                           ; A513 AD AA A3                 ...
	adc     LA3B7                           ; A516 6D B7 A3                 m..
	sta     LA3AA                           ; A519 8D AA A3                 ...
	lda     LA3AB                           ; A51C AD AB A3                 ...
	adc     #$00                            ; A51F 69 00                    i.
	sta     LA3AB                           ; A521 8D AB A3                 ...
	inc     LA3AF                           ; A524 EE AF A3                 ...
	jmp     LA46A                           ; A527 4C 6A A4                 Lj.

; ----------------------------------------------------------------------------
LA52A:  jsr     L5E30                           ; A52A 20 30 5E                  0^
	rts                                     ; A52D 60                       `

; ----------------------------------------------------------------------------
LA52E:  brk                                     ; A52E 00                       .
LA52F:  brk                                     ; A52F 00                       .
LA530:  brk                                     ; A530 00                       .
LA531:  brk                                     ; A531 00                       .
LA532:  brk                                     ; A532 00                       .
LA533:  brk                                     ; A533 00                       .
LA534:  brk                                     ; A534 00                       .
LA535:  brk                                     ; A535 00                       .
LA536:  brk                                     ; A536 00                       .
LA537:  brk                                     ; A537 00                       .
LA538:  brk                                     ; A538 00                       .
LA539:  brk                                     ; A539 00                       .
LA53A:  brk                                     ; A53A 00                       .
LA53B:  brk                                     ; A53B 00                       .
LA53C:  brk                                     ; A53C 00                       .
LA53D:  brk                                     ; A53D 00                       .
LA53E:  brk                                     ; A53E 00                       .
LA53F:  brk                                     ; A53F 00                       .
LA540:  brk                                     ; A540 00                       .
LA541:  brk                                     ; A541 00                       .
	brk                                     ; A542 00                       .
	brk                                     ; A543 00                       .
	brk                                     ; A544 00                       .
LA545:  brk                                     ; A545 00                       .
LA546:  brk                                     ; A546 00                       .

LA547:	
	stack_prolog LA52E, $03
	lda     LA52E                           ; A550 AD 2E A5                 ...
	jsr     L65B0                           ; A553 20 B0 65                  .e
	lda     $A1                             ; A556 A5 A1                    ..
	sta     LA535                           ; A558 8D 35 A5                 .5.
	lda     $A0                             ; A55B A5 A0                    ..
	sta     LA534                           ; A55D 8D 34 A5                 .4.
	lda     LA534                           ; A560 AD 34 A5                 .4.
	ora     LA535                           ; A563 0D 35 A5                 .5.
	lbne	LA56C
LA56B:  rts                                     ; A56B 60                       `

; ----------------------------------------------------------------------------
LA56C:  lda     LA531                           ; A56C AD 31 A5                 .1.
	sta     $A3                             ; A56F 85 A3                    ..
	lda     #$00                            ; A571 A9 00                    ..
	sta     $A5                             ; A573 85 A5                    ..
	lda     #$04                            ; A575 A9 04                    ..
	sta     $A4                             ; A577 85 A4                    ..
	ldy     LA530                           ; A579 AC 30 A5                 .0.
	ldx     #$A5                            ; A57C A2 A5                    ..
	lda     #$3D                            ; A57E A9 3D                    .=
	jsr     sub_461F
	sec                                     ; A583 38                       8
	lda     LA53F                           ; A584 AD 3F A5                 .?.
	sbc     LA53D                           ; A587 ED 3D A5                 .=.
	sta     $AE                             ; A58A 85 AE                    ..
LA58C:  clc                                     ; A58C 18                       .
	lda     $AE                             ; A58D A5 AE                    ..
	adc     #$01                            ; A58F 69 01                    i.
	sta     LA536                           ; A591 8D 36 A5                 .6.
	lda     LA535                           ; A594 AD 35 A5                 .5.
	sta     $A3                             ; A597 85 A3                    ..
	lda     #$00                            ; A599 A9 00                    ..
	sta     $A5                             ; A59B 85 A5                    ..
	lda     #$06                            ; A59D A9 06                    ..
	sta     $A4                             ; A59F 85 A4                    ..
	ldy     LA534                           ; A5A1 AC 34 A5                 .4.
	ldx     #$A5                            ; A5A4 A2 A5                    ..
	lda     #$41                            ; A5A6 A9 41                    .A
	jsr     sub_461F
	lda     LA53E                           ; A5AB AD 3E A5                 .>.
	asl     a                               ; A5AE 0A                       .
	php                                     ; A5AF 08                       .
	clc                                     ; A5B0 18                       .
	adc     LA545                           ; A5B1 6D 45 A5                 mE.
	sta     $AE                             ; A5B4 85 AE                    ..
	lda     #$00                            ; A5B6 A9 00                    ..
	rol     a                               ; A5B8 2A                       *
	plp                                     ; A5B9 28                       (
	adc     LA546                           ; A5BA 6D 46 A5                 mF.
	sta     $AF                             ; A5BD 85 AF                    ..
	clc                                     ; A5BF 18                       .
	ldy     #$00                            ; A5C0 A0 00                    ..
	lda     ($AE),y                         ; A5C2 B1 AE                    ..
	adc     LA53D                           ; A5C4 6D 3D A5                 m=.
	sta     $AC                             ; A5C7 85 AC                    ..
	iny                                     ; A5C9 C8                       .
	lda     ($AE),y                         ; A5CA B1 AE                    ..
	adc     #$00                            ; A5CC 69 00                    i.
	sta     $AD                             ; A5CE 85 AD                    ..
	sec                                     ; A5D0 38                       8
	lda     $AC                             ; A5D1 A5 AC                    ..
	sbc     #$01                            ; A5D3 E9 01                    ..
	sta     LA532                           ; A5D5 8D 32 A5                 .2.
	lda     $AD                             ; A5D8 A5 AD                    ..
	sbc     #$00                            ; A5DA E9 00                    ..
	sta     LA533                           ; A5DC 8D 33 A5                 .3.
	jsr     sub_5E1E
	ldy     #$00                            ; A5E2 A0 00                    ..
	sty     LA53B                           ; A5E4 8C 3B A5                 .;.
	sty     LA53A                           ; A5E7 8C 3A A5                 .:.
	lda     LA53E                           ; A5EA AD 3E A5                 .>.
	sta     LA537                           ; A5ED 8D 37 A5                 .7.
	lda     LA540                           ; A5F0 AD 40 A5                 .@.
	sta     LA601                           ; A5F3 8D 01 A6                 ...
LA5F6:  lda     LA601                           ; A5F6 AD 01 A6                 ...
	cmp     LA537                           ; A5F9 CD 37 A5                 .7.
	bcs     LA602                           ; A5FC B0 04                    ..
	jmp     LA68A                           ; A5FE 4C 8A A6                 L..

; ----------------------------------------------------------------------------
LA601:  brk                                     ; A601 00                       .
LA602:  ldy     #$01                            ; A602 A0 01                    ..
	sty     LA538                           ; A604 8C 38 A5                 .8.
	lda     LA536                           ; A607 AD 36 A5                 .6.
	sta     LA618                           ; A60A 8D 18 A6                 ...
LA60D:  lda     LA618                           ; A60D AD 18 A6                 ...
	cmp     LA538                           ; A610 CD 38 A5                 .8.
	bcs     LA619                           ; A613 B0 04                    ..
	jmp     LA662                           ; A615 4C 62 A6                 Lb.

; ----------------------------------------------------------------------------
LA618:  brk                                     ; A618 00                       .
LA619:  lda     LA52F                           ; A619 AD 2F A5                 ./.
	jsr     L45A3                           ; A61C 20 A3 45                  .E
	lda     $A0                             ; A61F A5 A0                    ..
	sta     LA539                           ; A621 8D 39 A5                 .9.
	ldx     LA52F                           ; A624 AE 2F A5                 ./.
	lda     $05C0,x                         ; A627 BD C0 05                 ...
	sta     LA53C                           ; A62A 8D 3C A5                 .<.
	lda     LA53C                           ; A62D AD 3C A5                 .<.
	bne     LA63A                           ; A630 D0 08                    ..
	lda     L464D                           ; A632 AD 4D 46                 .MF
	bne     LA63A                           ; A635 D0 03                    ..
	jmp     LA63D                           ; A637 4C 3D A6                 L=.

; ----------------------------------------------------------------------------
LA63A:  jmp     LA662                           ; A63A 4C 62 A6                 Lb.

; ----------------------------------------------------------------------------
LA63D:  clc                                     ; A63D 18                       .
	lda     LA532                           ; A63E AD 32 A5                 .2.
	adc     LA538                           ; A641 6D 38 A5                 m8.
	sta     $AE                             ; A644 85 AE                    ..
	lda     LA533                           ; A646 AD 33 A5                 .3.
	adc     #$00                            ; A649 69 00                    i.
	sta     $AF                             ; A64B 85 AF                    ..
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
LA672:  clc                                     ; A672 18                       .
	lda     LA532                           ; A673 AD 32 A5                 .2.
	adc     LA541                           ; A676 6D 41 A5                 mA.
	sta     LA532                           ; A679 8D 32 A5                 .2.
	lda     LA533                           ; A67C AD 33 A5                 .3.
	adc     #$00                            ; A67F 69 00                    i.
	sta     LA533                           ; A681 8D 33 A5                 .3.
	inc     LA537                           ; A684 EE 37 A5                 .7.
	jmp     LA5F6                           ; A687 4C F6 A5                 L..

; ----------------------------------------------------------------------------
LA68A:  jsr     L5E30                           ; A68A 20 30 5E                  0^
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
	.byte   $03                             ; A6A7 03                       .
	.byte   $43                             ; A6A8 43                       C
	.byte   $42                             ; A6A9 42                       B
	.byte   $42                             ; A6AA 42                       B
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
	ldx     #$A6                            ; A6C3 A2 A6                    ..
	lda     #$A7                            ; A6C5 A9 A7                    ..
	jsr     sub_55A0
	rts                                     ; A6CA 60                       `

; ----------------------------------------------------------------------------
LA6CB:	brk                                     ; A6CB 00                       .
	brk                                     ; A6CC 00                       .
	brk                                     ; A6CD 00                       .

LA6CE:	
	stack_prolog LA6CB, $02
	rts                                     ; A6D7 60                       `

; ----------------------------------------------------------------------------
LA6D8:  brk                                     ; A6D8 00                       .
LA6D9:  brk                                     ; A6D9 00                       .
LA6DA:  brk                                     ; A6DA 00                       .
LA6DB:  brk                                     ; A6DB 00                       .
LA6DC:  brk                                     ; A6DC 00                       .
LA6DD:  brk                                     ; A6DD 00                       .
LA6DE:  brk                                     ; A6DE 00                       .
LA6DF:  brk                                     ; A6DF 00                       .
LA6E0:  brk                                     ; A6E0 00                       .
LA6E1:	.byte   $04,"done"
LA6E6:	.addr	LA6E1

LA6E8:	
	stack_prolog LA6D8, $03
	lda     LA6D8                           ; A6F1 AD D8 A6                 ...
	jsr     L65B0                           ; A6F4 20 B0 65                  .e
	lda     $A1                             ; A6F7 A5 A1                    ..
	sta     LA6DD                           ; A6F9 8D DD A6                 ...
	lda     $A0                             ; A6FC A5 A0                    ..
	sta     LA6DC                           ; A6FE 8D DC A6                 ...
	lda     LA6DC                           ; A701 AD DC A6                 ...
	ora     LA6DD                           ; A704 0D DD A6                 ...
	lbne	LA70D
	rts                                     ; A70C 60                       `

; ----------------------------------------------------------------------------
LA70D:	add16i	off_AE, LA6DC, $0001
	ldy     #$00                            ; A71C A0 00                    ..
	lda     (off_AE),y
	sta     LA6DE                           ; A720 8D DE A6                 ...
	jsr     sub_5E1E
	lda     LA6D9                           ; A726 AD D9 A6                 ...
	jsr     LA28D                           ; A729 20 8D A2                  ..
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
	lda     $05C0,x                         ; A74B BD C0 05                 ...
	lbne	LA7B7
	lda     #$5C                            ; A753 A9 5C                    .\
	sta     $A3                             ; A755 85 A3                    ..
	ldy     #$B2                            ; A757 A0 B2                    ..
	ldx     #$24                            ; A759 A2 24                    .$
	lda     LA6D9                           ; A75B AD D9 A6                 ...
	jsr     L458F                           ; A75E 20 8F 45                  .E
	lda     $B224                           ; A761 AD 24 B2                 .$.
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
	lda     $B224,x                         ; A781 BD 24 B2                 .$.
	sta     $A0                             ; A784 85 A0                    ..
	lda     $A0                             ; A786 A5 A0                    ..
	jsr     L4B7B                           ; A788 20 7B 4B                  {K
	lda     $A0                             ; A78B A5 A0                    ..
	ldx     LA6DF                           ; A78D AE DF A6                 ...
	sta     $B224,x                         ; A790 9D 24 B2                 .$.
	inc     LA6DF                           ; A793 EE DF A6                 ...
	jmp     LA772                           ; A796 4C 72 A7                 Lr.

; ----------------------------------------------------------------------------
LA799:  lda     LA6E0                           ; A799 AD E0 A6                 ...
	sta     $A3                             ; A79C 85 A3                    ..
	lda     #$B2                            ; A79E A9 B2                    ..
	sta     $A5                             ; A7A0 85 A5                    ..
LA7A2:  lda     #$24                            ; A7A2 A9 24                    .$
	sta     $A4                             ; A7A4 85 A4                    ..
	ldy     LA6DE                           ; A7A6 AC DE A6                 ...
LA7A9:  ldx     #$00                            ; A7A9 A2 00                    ..
	lda     LA6D8                           ; A7AB AD D8 A6                 ...
	jsr     L67D8                           ; A7AE 20 D8 67                  .g
	jsr     L8D01                           ; A7B1 20 01 8D                  ..
	jmp     LA748                           ; A7B4 4C 48 A7                 LH.

; ----------------------------------------------------------------------------
LA7B7:  lda     LA6D9                           ; A7B7 AD D9 A6                 ...
	jsr     LA28D                           ; A7BA 20 8D A2                  ..
	jsr     L5E30                           ; A7BD 20 30 5E                  0^
	rts                                     ; A7C0 60                       `

; ----------------------------------------------------------------------------
LA7C1:  brk                                     ; A7C1 00                       .
LA7C2:  brk                                     ; A7C2 00                       .
LA7C3:  brk                                     ; A7C3 00                       .

LA7C4:
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
	jsr     L5E30                           ; A7EC 20 30 5E                  0^
	rts                                     ; A7EF 60                       `

; ----------------------------------------------------------------------------
LA7F0:  brk                                     ; A7F0 00                       .
LA7F1:  brk                                     ; A7F1 00                       .
LA7F2:  brk                                     ; A7F2 00                       .

LA7F3:
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
	jsr     L5E30                           ; A81B 20 30 5E                  0^
	rts                                     ; A81E 60                       `

; ----------------------------------------------------------------------------
sub_A81F:	
	sta     $B118,x                         ; A81F 9D 18 B1                 ...
	rts                                     ; A822 60                       `

; ----------------------------------------------------------------------------
LA823:	clc                                     ; A823 18                       .
	adc     $B118,x                         ; A824 7D 18 B1                 }..
	sta     $B118,x                         ; A827 9D 18 B1                 ...
	rts                                     ; A82A 60                       `

; ----------------------------------------------------------------------------
LA82B:	lda     $B118,x                         ; A82B BD 18 B1                 ...
	eor     #$FF                            ; A82E 49 FF                    I.
	tay                                     ; A830 A8                       .
	iny                                     ; A831 C8                       .
	tya                                     ; A832 98                       .
	sta     $B118,x                         ; A833 9D 18 B1                 ...
	rts                                     ; A836 60                       `

; ----------------------------------------------------------------------------
LA837:	stx     $A0                             ; A837 86 A0                    ..
	cmp     $A0                             ; A839 C5 A0                    ..
	bne     LA841                           ; A83B D0 04                    ..
	tya                                     ; A83D 98                       .
	jsr     sub_4BA7
LA841:  rts                                     ; A841 60                       `

; ----------------------------------------------------------------------------
LA842:	rts                                     ; A842 60                       `

; ----------------------------------------------------------------------------
LA843:  brk                                     ; A843 00                       .
LA844:  brk                                     ; A844 00                       .
LA845:  brk                                     ; A845 00                       .

LA846:
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
LA887:  brk                                     ; A887 00                       .
LA888:  brk                                     ; A888 00                       .
LA889:  brk                                     ; A889 00                       .
	brk                                     ; A88A 00                       .
LA88B:  brk                                     ; A88B 00                       .
LA88C:  brk                                     ; A88C 00                       .
LA88D:  brk                                     ; A88D 00                       .
LA88E:  brk                                     ; A88E 00                       .
	brk                                     ; A88F 00                       .
	brk                                     ; A890 00                       .
	brk                                     ; A891 00                       .
	brk                                     ; A892 00                       .
LA893:  brk                                     ; A893 00                       .
LA894:  brk                                     ; A894 00                       .
LA895:  jmp     LA898                           ; A895 4C 98 A8                 L..

; ----------------------------------------------------------------------------
LA898:  stx     LA888                           ; A898 8E 88 A8                 ...
	sta     LA887                           ; A89B 8D 87 A8                 ...
	lda     LA887                           ; A89E AD 87 A8                 ...
	jsr     L65B0                           ; A8A1 20 B0 65                  .e
	lda     $A1                             ; A8A4 A5 A1                    ..
	sta     LA88E                           ; A8A6 8D 8E A8                 ...
	lda     $A0                             ; A8A9 A5 A0                    ..
	sta     LA88D                           ; A8AB 8D 8D A8                 ...
	lda     LA88E                           ; A8AE AD 8E A8                 ...
	sta     $A3                             ; A8B1 85 A3                    ..
	lda     #$00                            ; A8B3 A9 00                    ..
	sta     $A5                             ; A8B5 85 A5                    ..
	lda     #$06                            ; A8B7 A9 06                    ..
	sta     $A4                             ; A8B9 85 A4                    ..
	ldy     LA88D                           ; A8BB AC 8D A8                 ...
	ldx     #$A8                            ; A8BE A2 A8                    ..
	lda     #$8F                            ; A8C0 A9 8F                    ..
	jsr     sub_461F
	lda     LA888                           ; A8C5 AD 88 A8                 ...
	asl     a                               ; A8C8 0A                       .
	php                                     ; A8C9 08                       .
	clc                                     ; A8CA 18                       .
	adc     LA893                           ; A8CB 6D 93 A8                 m..
	sta     $AE                             ; A8CE 85 AE                    ..
	lda     #$00                            ; A8D0 A9 00                    ..
	rol     a                               ; A8D2 2A                       *
	plp                                     ; A8D3 28                       (
	adc     LA894                           ; A8D4 6D 94 A8                 m..
	sta     $AF                             ; A8D7 85 AF                    ..
	ldy     #$01                            ; A8D9 A0 01                    ..
	lda     ($AE),y                         ; A8DB B1 AE                    ..
	sta     LA88C                           ; A8DD 8D 8C A8                 ...
	dey                                     ; A8E0 88                       .
	lda     ($AE),y                         ; A8E1 B1 AE                    ..
	sta     LA88B                           ; A8E3 8D 8B A8                 ...
LA8E6:  lda     LA88B                           ; A8E6 AD 8B A8                 ...
	sta     $AE                             ; A8E9 85 AE                    ..
	lda     LA88C                           ; A8EB AD 8C A8                 ...
	sta     $AF                             ; A8EE 85 AF                    ..
	ldy     #$00                            ; A8F0 A0 00                    ..
	lda     ($AE),y                         ; A8F2 B1 AE                    ..
	eor     #$1E                            ; A8F4 49 1E                    I.
	beq     LA8FB                           ; A8F6 F0 03                    ..
	jmp     LA8FE                           ; A8F8 4C FE A8                 L..

; ----------------------------------------------------------------------------
LA8FB:  jmp     LA958                           ; A8FB 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA8FE:  ldy     #$00                            ; A8FE A0 00                    ..
	sty     LA889                           ; A900 8C 89 A8                 ...
LA903:  lda     #$5B                            ; A903 A9 5B                    .[
	cmp     LA889                           ; A905 CD 89 A8                 ...
	bcs     LA90D                           ; A908 B0 03                    ..
	jmp     LA936                           ; A90A 4C 36 A9                 L6.

; ----------------------------------------------------------------------------
LA90D:  clc                                     ; A90D 18                       .
	lda     LA88B                           ; A90E AD 8B A8                 ...
	adc     LA889                           ; A911 6D 89 A8                 m..
	sta     $AE                             ; A914 85 AE                    ..
	lda     LA88C                           ; A916 AD 8C A8                 ...
	adc     #$00                            ; A919 69 00                    i.
	sta     $AF                             ; A91B 85 AF                    ..
	ldy     #$00                            ; A91D A0 00                    ..
	lda     ($AE),y                         ; A91F B1 AE                    ..
	sta     $A0                             ; A921 85 A0                    ..
	lda     $A0                             ; A923 A5 A0                    ..
	jsr     L4BC9                           ; A925 20 C9 4B                  .K
	lda     $A0                             ; A928 A5 A0                    ..
	ldx     LA889                           ; A92A AE 89 A8                 ...
	sta     $B224,x                         ; A92D 9D 24 B2                 .$.
	inc     LA889                           ; A930 EE 89 A8                 ...
	jmp     LA903                           ; A933 4C 03 A9                 L..

; ----------------------------------------------------------------------------
LA936:  jsr     sub_4749
	lda     $A0                             ; A939 A5 A0                    ..
	beq     LA940                           ; A93B F0 03                    ..
	jmp     LA943                           ; A93D 4C 43 A9                 LC.

; ----------------------------------------------------------------------------
LA940:  jmp     LA958                           ; A940 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA943:  clc                                     ; A943 18                       .
	lda     LA88B                           ; A944 AD 8B A8                 ...
	adc     L4654                           ; A947 6D 54 46                 mTF
	sta     LA88B                           ; A94A 8D 8B A8                 ...
	lda     LA88C                           ; A94D AD 8C A8                 ...
	adc     #$00                            ; A950 69 00                    i.
	sta     LA88C                           ; A952 8D 8C A8                 ...
	jmp     LA8E6                           ; A955 4C E6 A8                 L..

; ----------------------------------------------------------------------------
LA958:  rts                                     ; A958 60                       `

; ----------------------------------------------------------------------------
LA959:	stx     $A0                             ; A959 86 A0                    ..
	tax                                     ; A95B AA                       .
	lda     #$00                            ; A95C A9 00                    ..
	sta     $B800,x                         ; A95E 9D 00 B8                 ...
	lda     $A0                             ; A961 A5 A0                    ..
	sta     $B14A,x                         ; A963 9D 4A B1                 .J.
	tya                                     ; A966 98                       .
	sta     $BC00,x                         ; A967 9D 00 BC                 ...
	rts                                     ; A96A 60                       `

; ----------------------------------------------------------------------------
LA96B:  brk                                     ; A96B 00                       .
LA96C:  brk                                     ; A96C 00                       .
LA96D:  .byte   $D5                             ; A96D D5                       .
LA96E:  brk                                     ; A96E 00                       .

LA96F:	prolog
	stx     LA96C                           ; A972 8E 6C A9                 .l.
	sta     LA96B                           ; A975 8D 6B A9                 .k.
	clc                                     ; A978 18                       .
	lda     LA96D                           ; A979 AD 6D A9                 .m.
	adc     LA96B                           ; A97C 6D 6B A9                 mk.
	sta     $AE                             ; A97F 85 AE                    ..
	lda     LA96E                           ; A981 AD 6E A9                 .n.
	adc     #$00                            ; A984 69 00                    i.
	sta     $AF                             ; A986 85 AF                    ..
	lda     LA96C                           ; A988 AD 6C A9                 .l.
	ldy     #$00                            ; A98B A0 00                    ..
	sta     ($AE),y                         ; A98D 91 AE                    ..
	rts                                     ; A98F 60                       `

; ----------------------------------------------------------------------------
LA990:  brk                                     ; A990 00                       .

LA991:	prolog
	sta     LA990                           ; A994 8D 90 A9                 ...
	lda     LA990                           ; A997 AD 90 A9                 ...
	jsr     L4BC9                           ; A99A 20 C9 4B                  .K
	lda     $A0                             ; A99D A5 A0                    ..
	sta     L464A                           ; A99F 8D 4A 46                 .JF
	rts                                     ; A9A2 60                       `

; ----------------------------------------------------------------------------
LA9A3:  jmp     LA9A6                           ; A9A3 4C A6 A9                 L..

; ----------------------------------------------------------------------------
LA9A6:  lda     $D8                             ; A9A6 A5 D8                    ..
	eor     #$01                            ; A9A8 49 01                    I.
	beq     LA9AF                           ; A9AA F0 03                    ..
	jmp     LA9B8                           ; A9AC 4C B8 A9                 L..

; ----------------------------------------------------------------------------
LA9AF:  lda     #$12                            ; A9AF A9 12                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9B4 A0 00                    ..
	sty     $D8                             ; A9B6 84 D8                    ..
LA9B8:  lda     $D9                             ; A9B8 A5 D9                    ..
	eor     #$01                            ; A9BA 49 01                    I.
	beq     LA9C1                           ; A9BC F0 03                    ..
	jmp     LA9CA                           ; A9BE 4C CA A9                 L..

; ----------------------------------------------------------------------------
LA9C1:  lda     #$13                            ; A9C1 A9 13                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9C6 A0 00                    ..
	sty     $D9                             ; A9C8 84 D9                    ..
LA9CA:  lda     $DA                             ; A9CA A5 DA                    ..
	eor     #$01                            ; A9CC 49 01                    I.
	beq     LA9D3                           ; A9CE F0 03                    ..
	jmp     LA9DC                           ; A9D0 4C DC A9                 L..

; ----------------------------------------------------------------------------
LA9D3:  lda     #$14                            ; A9D3 A9 14                    ..
	jsr     sub_4BA7
	ldy     #$00                            ; A9D8 A0 00                    ..
	sty     $DA                             ; A9DA 84 DA                    ..
LA9DC:  rts                                     ; A9DC 60                       `

; ----------------------------------------------------------------------------
LA9DD:  brk                                     ; A9DD 00                       .
LA9DE:  jmp     LA9E1                           ; A9DE 4C E1 A9                 L..

; ----------------------------------------------------------------------------
LA9E1:  sta     LA9DD                           ; A9E1 8D DD A9                 ...
	lda     LA9DD                           ; A9E4 AD DD A9                 ...
	sta     L4658                           ; A9E7 8D 58 46                 .XF
	rts                                     ; A9EA 60                       `

; ----------------------------------------------------------------------------
LA9EB:  brk                                     ; A9EB 00                       .
LA9EC:  jmp     LA9EF                           ; A9EC 4C EF A9                 L..

; ----------------------------------------------------------------------------
LA9EF:  sta     LA9EB                           ; A9EF 8D EB A9                 ...
	.byte   $AD                             ; A9F2 AD                       .
LA9F3:  .byte   $EB                             ; A9F3 EB                       .
	lda     #$8D                            ; A9F4 A9 8D                    ..
	lsr     L6046                           ; A9F6 4E 46 60                 NF`
LA9F9:  brk                                     ; A9F9 00                       .
LA9FA:  brk                                     ; A9FA 00                       .
LA9FB:  jmp     LA9FE                           ; A9FB 4C FE A9                 L..

; ----------------------------------------------------------------------------
LA9FE:  sec                                     ; A9FE 38                       8
	lda     $02E5                           ; A9FF AD E5 02                 ...
	sbc     $02E7                           ; AA02 ED E7 02                 ...
	sta     LA9F9                           ; AA05 8D F9 A9                 ...
	lda     $02E6                           ; AA08 AD E6 02                 ...
	sbc     $02E8                           ; AA0B ED E8 02                 ...
	sta     LA9FA                           ; AA0E 8D FA A9                 ...
	lda     LA9FA                           ; AA11 AD FA A9                 ...
	sta     $A3                             ; AA14 85 A3                    ..
	ldy     LA9F9                           ; AA16 AC F9 A9                 ...
	ldx     $02E8                           ; AA19 AE E8 02                 ...
	lda     $02E7                           ; AA1C AD E7 02                 ...
	jsr     L619A                           ; AA1F 20 9A 61                  .a
	jmp     LAA28                           ; AA22 4C 28 AA                 L(.

; ----------------------------------------------------------------------------
	.byte   $02                             ; AA25 02                       .
	.byte   $43                             ; AA26 43                       C
	pha                                     ; AA27 48                       H
LAA28:  lda     #$00                            ; AA28 A9 00                    ..
	sta     $A3                             ; AA2A 85 A3                    ..
	lda     LA9FA                           ; AA2C AD FA A9                 ...
	sta     $A5                             ; AA2F 85 A5                    ..
	lda     LA9F9                           ; AA31 AD F9 A9                 ...
	sta     $A4                             ; AA34 85 A4                    ..
	ldy     #$46                            ; AA36 A0 46                    .F
	ldx     #$AA                            ; AA38 A2 AA                    ..
	lda     #$25                            ; AA3A A9 25                    .%
	jsr     sub_55A0                           ; AA3C 20 A0 55                  .U
	jsr     L747D                           ; AA3F 20 7D 74                  }t
	jsr     L696A                           ; AA42 20 6A 69                  ji
	.byte   $20                             ; AA45 20                        
	.byte   $20                             ; AA46 20                        
LAA47:  .byte   $80                             ; AA47 80                       .
	jsr     L8F7D                           ; AA48 20 7D 8F                  }.
	lda     #$00                            ; AA4B A9 00                    ..
	jsr     L9C41                           ; AA4D 20 41 9C                  A.
	lda     #$00                            ; AA50 A9 00                    ..
	jsr     LA9EC                           ; AA52 20 EC A9                  ..
LAA55:  lda     #$00                            ; AA55 A9 00                    ..
	sta     $A3                             ; AA57 85 A3                    ..
	lda     #$FF                            ; AA59 A9 FF                    ..
	sta     $A4                             ; AA5B 85 A4                    ..
	ldy     #$20                            ; AA5D A0 20                    . 
	ldx     #$B1                            ; AA5F A2 B1                    ..
	lda     #$4A                            ; AA61 A9 4A                    .J
	jsr     L45FC                           ; AA63 20 FC 45                  .E
	lda     #$00                            ; AA66 A9 00                    ..
	sta     $A3                             ; AA68 85 A3                    ..
	ldy     #$10                            ; AA6A A0 10                    ..
	ldx     #$B1                            ; AA6C A2 B1                    ..
	lda     #$18                            ; AA6E A9 18                    ..
	jsr     L45F6                           ; AA70 20 F6 45                  .E
	lda     #$FF                            ; AA73 A9 FF                    ..
	sta     L4647                           ; AA75 8D 47 46                 .GF
	lda     #$FF                            ; AA78 A9 FF                    ..
	sta     L4648                           ; AA7A 8D 48 46                 .HF
	lda     #$01                            ; AA7D A9 01                    ..
	jsr     LA9DE                           ; AA7F 20 DE A9                  ..
	rts                                     ; AA82 60                       `

; ----------------------------------------------------------------------------
	.byte   $4C                             ; AA83 4C                       L
LAA84:  .addr   LAA86
LAA86:	.addr	L6AD5
	.addr	L6995
	.addr	L67D8
	.addr	L6696
	.addr	L65E2
	.addr	L7D4D
	.addr	L6E61
	.addr	L6F68
	.addr	L6F91
	.addr	L6FEB
	.addr	sub_7A57
	.addr	sub_7ADF
	.addr	sub_7B68
	.addr	sub_7A9A
	.addr	L74BE
	.addr	sub_7951
	.addr	sub_77A3
	.addr	sub_782A
	.addr	sub_758C
	.addr	sub_760A
	.addr	L705F
	.addr	L7096
	.addr	L70E2
	.addr	L9C41
	.addr	L9BE0
	.addr	L9BD0
	.addr	L9CAD
	.addr	L9E2C
	.addr	L7FE9
	.addr	L817C
	.addr	L8003
	.addr	L80BB
	.addr	L8EFD
	.addr	L8E7D
	.addr	L8F55
	.addr	L8E24
	.addr	LA9DE
	.addr	LA9FB
	.addr	LA9EC
	.addr	L968E
	.addr	L97A1
	.addr	sub_5E1E
	.addr	L5E30
	.addr	sub_A2A8
	.addr	LA382
	.addr	LA3BD
	.addr	LA547
	.addr	LA6E8
	.addr	LA7C4
	.addr	LA7F3
	.addr	LA6CE
	.addr	LA991
	.addr	L4F5A
	.addr	LA959
	.addr	sub_4BA7
	.addr	LA837
	.addr	LA846
	.addr	sub_4F9D
	.addr	L8D01
	.addr	LA895
	.addr	LA842
	.addr	LA96F
	.addr	sub_A81F
	.addr	LA823
	.addr	LA82B

LAB08:	brk                                     ; AB08 00                       .
	brk                                     ; AB09 00                       .
	brk                                     ; AB0A 00                       .

LAB0B:  prolog
	jsr     sub_44D5                           ; AB0E 20 D5 44                  .D
	.addr	LAB08
	.byte	$02
	.byte   $43                             ; AB14 43                       C
	.byte   $4B                             ; AB15 4B                       K
	.byte   $44                             ; AB16 44                       D
	lsr     $57                             ; AB17 46 57                    FW
	;eor     L7842,y                         ; AB19 59 42 78                 YBx
	.byte	$59,$42,$78
	adc     L557A,y                         ; AB1C 79 7A 55                 yzU
	lsr     $58,x                           ; AB1F 56 58                    VX
	lsr     L6463                           ; AB21 4E 63 64                 Ncd
	.byte   $73                             ; AB24 73                       s
	bvs     LAB94                           ; AB25 70 6D                    pm
	;jmp     (L6261)                         ; AB27 6C 61 62                 lab
	.byte	$6C,$61,$62
	ror     $4A                             ; AB2A 66 4A                    fJ
	ror     a                               ; AB2C 6A                       j
	.byte   $53                             ; AB2D 53                       S
	.byte   $54                             ; AB2E 54                       T
	.byte   $2B                             ; AB2F 2B                       +
	jmp     L504D                           ; AB30 4C 4D 50                 LMP

; ----------------------------------------------------------------------------
	.byte   $52                             ; AB33 52                       R
	.byte   $47                             ; AB34 47                       G
	eor     ($4F,x)                         ; AB35 41 4F                    AO
	.byte   $5A                             ; AB37 5A                       Z
	eor     L0069                           ; AB38 45 69                    Ei
	pha                                     ; AB3A 48                       H
	adc     $49                             ; AB3B 65 49                    eI
	;bmi     LAB70                           ; AB3D 30 31                    01
	.byte	$30,$31
	.byte   $32                             ; AB3F 32                       2
	.byte   $33                             ; AB40 33                       3
	.byte   $34                             ; AB41 34                       4
	and     $36,x                           ; AB42 35 36                    56
	.byte   $37                             ; AB44 37                       7
	sec                                     ; AB45 38                       8
	and     $076E,y                         ; AB46 39 6E 07                 9n.
	.byte   $23                             ; AB49 23                       #
	rol     a                               ; AB4A 2A                       *
	and     $2524,x                         ; AB4B 3D 24 25                 =$%
	rol     $40                             ; AB4E 26 40                    &@
	;rol     L753A                           ; AB50 2E 3A 75                 .:u
	.byte	$2E,$3A,$75
	ror     $77,x                           ; AB53 76 77                    vw
	brk                                     ; AB55 00                       .
LAB56:  .byte   $14                             ; AB56 14                       .
LAB57:  .byte   $AB                             ; AB57 AB                       .
LAB58:  .addr	$0000
LAB5A:  brk                                     ; AB5A 00                       .
LAB5B:  brk                                     ; AB5B 00                       .
LAB5C:  brk                                     ; AB5C 00                       .
LAB5D:  brk                                     ; AB5D 00                       .
	brk                                     ; AB5E 00                       .
	brk                                     ; AB5F 00                       .
LAB60:  dec     $1E,x                           ; AB60 D6 1E                    ..
	php                                     ; AB62 08                       .
	brk                                     ; AB63 00                       .
	brk                                     ; AB64 00                       .
	brk                                     ; AB65 00                       .
	brk                                     ; AB66 00                       .
	brk                                     ; AB67 00                       .
LAB68:  brk                                     ; AB68 00                       .
LAB69:  brk                                     ; AB69 00                       .

sub_AB6A:  
	prolog
	rdmv	LAB58, LAA84
	rdmv	LAB5A, L48C1
	ldy     #$00                            ; AB85 A0 00                    ..
	sty     LAB69                           ; AB87 8C 69 AB                 .i.
	iny                                     ; AB8A C8                       .
	sty     L4651                           ; AB8B 8C 51 46                 .QF
	lda     $B224                           ; AB8E AD 24 B2                 .$.
	sta     LAB68                           ; AB91 8D 68 AB                 .h.
LAB94:  clc                                     ; AB94 18                       .
	lda     LAB56                           ; AB95 AD 56 AB                 .V.
	adc     LAB69                           ; AB98 6D 69 AB                 mi.
	sta     $AE                             ; AB9B 85 AE                    ..
	lda     LAB57                           ; AB9D AD 57 AB                 .W.
	adc     #$00                            ; ABA0 69 00                    i.
	sta     $AF                             ; ABA2 85 AF                    ..
	ldy     #$00                            ; ABA4 A0 00                    ..
	lda     ($AE),y                         ; ABA6 B1 AE                    ..
	eor     LAB68                           ; ABA8 4D 68 AB                 Mh.
	bne     LABB0                           ; ABAB D0 03                    ..
	jmp     LABEE                           ; ABAD 4C EE AB                 L..

; ----------------------------------------------------------------------------
LABB0:  clc                                     ; ABB0 18                       .
	lda     LAB56                           ; ABB1 AD 56 AB                 .V.
	adc     LAB69                           ; ABB4 6D 69 AB                 mi.
	sta     $AE                             ; ABB7 85 AE                    ..
	lda     LAB57                           ; ABB9 AD 57 AB                 .W.
	adc     #$00                            ; ABBC 69 00                    i.
	sta     $AF                             ; ABBE 85 AF                    ..
	lda     ($AE),y                         ; ABC0 B1 AE                    ..
	beq     LABC7                           ; ABC2 F0 03                    ..
	jmp     LABE8                           ; ABC4 4C E8 AB                 L..

; ----------------------------------------------------------------------------
LABC7:  jmp     LABCD                           ; ABC7 4C CD AB                 L..

; ----------------------------------------------------------------------------
	.byte   $02                             ; ABCA 02                       .
	.byte   $43                             ; ABCB 43                       C
	.byte   $43                             ; ABCC 43                       C
LABCD:  lda     #$00                            ; ABCD A9 00                    ..
	sta     $A3                             ; ABCF 85 A3                    ..
	lda     #$00                            ; ABD1 A9 00                    ..
	sta     $A5                             ; ABD3 85 A5                    ..
	lda     LAB68                           ; ABD5 AD 68 AB                 .h.
	sta     $A4                             ; ABD8 85 A4                    ..
	ldy     #$3F                            ; ABDA A0 3F                    .?
	ldx     #$AB                            ; ABDC A2 AB                    ..
	lda     #$CA                            ; ABDE A9 CA                    ..
	jsr     sub_55A0
	ldi	$A0, $00
	rts                                     ; ABE7 60                       `

; ----------------------------------------------------------------------------
LABE8:  inc     LAB69                           ; ABE8 EE 69 AB                 .i.
	jmp     LAB94                           ; ABEB 4C 94 AB                 L..

; ----------------------------------------------------------------------------
LABEE:  lda     LAB69                           ; ABEE AD 69 AB                 .i.
	asl     a                               ; ABF1 0A                       .
	php                                     ; ABF2 08                       .
	clc                                     ; ABF3 18                       .
	adc     LAB5A                           ; ABF4 6D 5A AB                 mZ.
	sta     $AE                             ; ABF7 85 AE                    ..
	lda     #$00                            ; ABF9 A9 00                    ..
	rol     a                               ; ABFB 2A                       *
	plp                                     ; ABFC 28                       (
	adc     LAB5B                           ; ABFD 6D 5B AB                 m[.
	sta     $AF                             ; AC00 85 AF                    ..
	ldy     #$01                            ; AC02 A0 01                    ..
	lda     ($AE),y                         ; AC04 B1 AE                    ..
	sta     LAB5D                           ; AC06 8D 5D AB                 .].
	dey                                     ; AC09 88                       .
	lda     ($AE),y                         ; AC0A B1 AE                    ..
	sta     LAB5C                           ; AC0C 8D 5C AB                 .\.
	lda     LAB69                           ; AC0F AD 69 AB                 .i.
	asl     a                               ; AC12 0A                       .
	php                                     ; AC13 08                       .
	clc                                     ; AC14 18                       .
	adc     LAB58                           ; AC15 6D 58 AB                 mX.
	sta     $AE                             ; AC18 85 AE                    ..
	lda     #$00                            ; AC1A A9 00                    ..
	rol     a                               ; AC1C 2A                       *
	plp                                     ; AC1D 28                       (
	adc     LAB58+1                         ; AC1E 6D 59 AB                 mY.
	sta     $AF                             ; AC21 85 AF                    ..
	iny                                     ; AC23 C8                       .
	lda     ($AE),y                         ; AC24 B1 AE                    ..
	sta     LAB0B+2                         ; AC26 8D 0D AB                 ...
	dey                                     ; AC29 88                       .
	lda     ($AE),y                         ; AC2A B1 AE                    ..
	sta     LAB0B+1                         ; AC2C 8D 0C AB                 ...
	lda     LAB5C                           ; AC2F AD 5C AB                 .\.
	sta     $AE                             ; AC32 85 AE                    ..
	lda     LAB5D                           ; AC34 AD 5D AB                 .].
	sta     $AF                             ; AC37 85 AF                    ..
	iny                                     ; AC39 C8                       .
	lda     ($AE),y                         ; AC3A B1 AE                    ..
	sta     $A1                             ; AC3C 85 A1                    ..
	dey                                     ; AC3E 88                       .
	lda     ($AE),y                         ; AC3F B1 AE                    ..
	sta     $A0                             ; AC41 85 A0                    ..
	lda     #$AB                            ; AC43 A9 AB                    ..
	sta     $A3                             ; AC45 85 A3                    ..
	ldy     #$60                            ; AC47 A0 60                    .`
	ldx     $A1                             ; AC49 A6 A1                    ..
	lda     $A0                             ; AC4B A5 A0                    ..
	jsr     sub_58F2                        ; AC4D 20 F2 58                  .X
	ldy     #$00                            ; AC50 A0 00                    ..
	sty     LAB69                           ; AC52 8C 69 AB                 .i.
LAC55:  lda     #$07                            ; AC55 A9 07                    ..
	cmp     LAB69                           ; AC57 CD 69 AB                 .i.
	bcs     LAC5F                           ; AC5A B0 03                    ..
	jmp     LAC70                           ; AC5C 4C 70 AC                 Lp.

; ----------------------------------------------------------------------------
LAC5F:  ldx     LAB69                           ; AC5F AE 69 AB                 .i.
	lda     LAB60,x                         ; AC62 BD 60 AB                 .`.
	ldx     LAB69                           ; AC65 AE 69 AB                 .i.
	sta     $A0,x                           ; AC68 95 A0                    ..
	inc     LAB69                           ; AC6A EE 69 AB                 .i.
	jmp     LAC55                           ; AC6D 4C 55 AC                 LU.

; ----------------------------------------------------------------------------
LAC70:  ldy     $A2                             ; AC70 A4 A2                    ..
	ldx     $A1                             ; AC72 A6 A1                    ..
	lda     $A0                             ; AC74 A5 A0                    ..
	jsr     LAB0B                           ; AC76 20 0B AB                  ..
	lda     #$01                            ; AC79 A9 01                    ..
	sta     $A0                             ; AC7B 85 A0                    ..
	rts                                     ; AC7D 60                       `

; ----------------------------------------------------------------------------

sub_AC7E:
	txa                                     ; AC7E 8A                       .
	pha                                     ; AC7F 48                       H
	lda     $D209                           ; AC80 AD 09 D2                 ...
	eor     $02F2                           ; AC83 4D F2 02                 M..
	bne     LAC8D                           ; AC86 D0 05                    ..
	lda     $02F1                           ; AC88 AD F1 02                 ...
	bne     LACA9                           ; AC8B D0 1C                    ..
LAC8D:  lda     $D209                           ; AC8D AD 09 D2                 ...
	sta     $02F2                           ; AC90 8D F2 02                 ...
	ldx     L474D                           ; AC93 AE 4D 47                 .MG
	sta     $B138,x                         ; AC96 9D 38 B1                 .8.
	inc     L474D                           ; AC99 EE 4D 47                 .MG
	lda     L474D                           ; AC9C AD 4D 47                 .MG
	and     #$0F                            ; AC9F 29 0F                    ).
	sta     L474D                           ; ACA1 8D 4D 47                 .MG
	ldx     #$01                            ; ACA4 A2 01                    ..
	stx     $02F1                           ; ACA6 8E F1 02                 ...
LACA9:  lda     #$11                            ; ACA9 A9 11                    ..
	sta     $022B                           ; ACAB 8D 2B 02                 .+.
	pla                                     ; ACAE 68                       h
	tax                                     ; ACAF AA                       .
	pla                                     ; ACB0 68                       h
	rti                                     ; ACB1 40                       @

; ----------------------------------------------------------------------------
LACB2:  lda     $A0                             ; ACB2 A5 A0                    ..
	pha                                     ; ACB4 48                       H
	lda     $AE                             ; ACB5 A5 AE                    ..
	pha                                     ; ACB7 48                       H
	lda     $AF                             ; ACB8 A5 AF                    ..
	pha                                     ; ACBA 48                       H
	ldx     $E8                             ; ACBB A6 E8                    ..
	lda     L46EF,x                         ; ACBD BD EF 46                 ..F
	sta     $A0                             ; ACC0 85 A0                    ..
	lda     $A0                             ; ACC2 A5 A0                    ..
	asl     a                               ; ACC4 0A                       .
	php                                     ; ACC5 08                       .
	clc                                     ; ACC6 18                       .
	adc     L46F5                           ; ACC7 6D F5 46                 m.F
	sta     $AE                             ; ACCA 85 AE                    ..
	lda     #$00                            ; ACCC A9 00                    ..
	rol     a                               ; ACCE 2A                       *
	plp                                     ; ACCF 28                       (
	adc     L46F6                           ; ACD0 6D F6 46                 m.F
	sta     $AF                             ; ACD3 85 AF                    ..
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
	.byte   $85                             ; AD01 85                       .
LAD02:  .byte   $AE                             ; AD02 AE                       .
LAD03:  pla                                     ; AD03 68                       h
	.byte   $85                             ; AD04 85                       .
LAD05:  .byte   $A0                             ; AD05 A0                       .
LAD06:  rts                                     ; AD06 60                       `

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
	jsr     LACB2                           ; AD25 20 B2 AC                  ..
	pla                                     ; AD28 68                       h
	tay                                     ; AD29 A8                       .
	pla                                     ; AD2A 68                       h
	tax                                     ; AD2B AA                       .
	pla                                     ; AD2C 68                       h
	rti                                     ; AD2D 40                       @

; ----------------------------------------------------------------------------

LAD2E:
	ldy     #$00                            ; AD2E A0 00                    ..
	sty     $021B                           ; AD30 8C 1B 02                 ...
	lda     #$1E                            ; AD33 A9 1E                    ..
	sta     $021A                           ; AD35 8D 1A 02                 ...
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

; ----------------------------------------------------------------------------
	lda     $02C4                           ; AD59 AD C4 02                 ...
	sta     $D016                           ; AD5C 8D 16 D0                 ...
	lda     $02F4                           ; AD5F AD F4 02                 ...
	sta     $D409                           ; AD62 8D 09 D4                 ...
	lda     $02C8                           ; AD65 AD C8 02                 ...
	sta     $D01A                           ; AD68 8D 1A D0                 ...
	lda     $02C6                           ; AD6B AD C6 02                 ...
	sta     $D018                           ; AD6E 8D 18 D0                 ...
	lda     $02C5                           ; AD71 AD C5 02                 ...
	sta     $D017                           ; AD74 8D 17 D0                 ...
	ldy     #$01                            ; AD77 A0 01                    ..
	sty     $E8                             ; AD79 84 E8                    ..
	jsr     LACB2                           ; AD7B 20 B2 AC                  ..
	jmp     LE45F                           ; AD7E 4C 5F E4                 L_.

; ----------------------------------------------------------------------------
LAD81:  brk                                     ; AD81 00                       .
LAD82:  brk                                     ; AD82 00                       .
LAD83:  .byte   $D5                             ; AD83 D5                       .
LAD84:  brk                                     ; AD84 00                       .

; ----------------------------------------------------------------------------
LAD85:  prolog
	lda     #>LAD2E
	sta     $0229                           ; AD8A 8D 29 02                 .).
	lda     #<LAD2E
	sta     $0228                           ; AD8F 8D 28 02                 .(.
	ldy     #$00                            ; AD92 A0 00                    ..
	sty     $021B                           ; AD94 8C 1B 02                 ...
	lda     #$06                            ; AD97 A9 06                    ..
	sta     $021A                           ; AD99 8D 1A 02                 ...
	lda     #$00                            ; AD9C A9 00                    ..
	sta     $A3                             ; AD9E 85 A3                    ..
	ldy     #$06                            ; ADA0 A0 06                    ..
	ldx     LAD84                           ; ADA2 AE 84 AD                 ...
	lda     LAD83                           ; ADA5 AD 83 AD                 ...
	jsr     L45F6                           ; ADA8 20 F6 45                  .E
	lda     #$AC                            ; ADAB A9 AC                    ..
	sta     $0209                           ; ADAD 8D 09 02                 ...
	lda     #$7E                            ; ADB0 A9 7E                    .~
	sta     $0208                           ; ADB2 8D 08 02                 ...
	lda     #$AD                            ; ADB5 A9 AD                    ..
	sta     LAD82                           ; ADB7 8D 82 AD                 ...
	lda     #$59                            ; ADBA A9 59                    .Y
	sta     LAD81                           ; ADBC 8D 81 AD                 ...
	lda     #$08                            ; ADBF A9 08                    ..
	sta     $84                             ; ADC1 85 84                    ..
	lda     LAD82                           ; ADC3 AD 82 AD                 ...
	tax                                     ; ADC6 AA                       .
	lda     LAD81                           ; ADC7 AD 81 AD                 ...
	jsr     sub_43E0
	sta     $A1                             ; ADCD 85 A1                    ..
	lda     LAD81                           ; ADCF AD 81 AD                 ...
	and     #$FF                            ; ADD2 29 FF                    ).
	sta     $A2                             ; ADD4 85 A2                    ..
	ldy     $A2                             ; ADD6 A4 A2                    ..
	ldx     $A1                             ; ADD8 A6 A1                    ..
	lda     #$06                            ; ADDA A9 06                    ..
	jsr     SETVBV
	lda     #>LAD07                         ; ADDF A9 AD                    ..
	sta     VDSLST+1
	lda     #<LAD07
	sta     VDSLST
	rts                                     ; ADE9 60                       `

; ----------------------------------------------------------------------------
sub_ADEA:  
	prolog
	dmv	off_AE, L466F
	lda     $0231                           ; ADF7 AD 31 02                 .1.
	ldy     #$01                            ; ADFA A0 01                    ..
	sta     ($AE),y                         ; ADFC 91 AE                    ..
	lda     $0230                           ; ADFE AD 30 02                 .0.
	dey                                     ; AE01 88                       .
	sta     ($AE),y                         ; AE02 91 AE                    ..
	lda     #$01                            ; AE04 A9 01                    ..
	asl     a                               ; AE06 0A                       .
	php                                     ; AE07 08                       .
	clc                                     ; AE08 18                       .
	adc     L466F                           ; AE09 6D 6F 46                 moF
	sta     $AE                             ; AE0C 85 AE                    ..
	lda     #$00                            ; AE0E A9 00                    ..
	rol     a                               ; AE10 2A                       *
	plp                                     ; AE11 28                       (
	adc     L466F+1
	sta     $AF                             ; AE15 85 AF                    ..
	sec                                     ; AE17 38                       8
	lda     $0230                           ; AE18 AD 30 02                 .0.
	sbc     #$00                            ; AE1B E9 00                    ..
	sta     $AC                             ; AE1D 85 AC                    ..
	lda     $0231                           ; AE1F AD 31 02                 .1.
	sbc     #$04                            ; AE22 E9 04                    ..
	iny                                     ; AE24 C8                       .
	sta     ($AE),y                         ; AE25 91 AE                    ..
	lda     $AC                             ; AE27 A5 AC                    ..
	dey                                     ; AE29 88                       .
	sta     ($AE),y                         ; AE2A 91 AE                    ..
	lda     #>L4327
	sta     $02E6                           ; AE2E 8D E6 02                 ...
	lda     #<L4327
	sta     $02E5                           ; AE33 8D E5 02                 ...
	rdmv	sub_4749+1, sub_AB6A+1
	rdmv	L5D65, L8D02
	jsr     LAD85                           ; AE4E 20 85 AD                  ..
	jsr     L6203                           ; AE51 20 03 62                  .b
	jsr     L5D67                           ; AE54 20 67 5D                  g]
	jsr     LA9FB                           ; AE57 20 FB A9                  ..
	sei                                     ; AE5A 78                       x
	lda     $10                             ; AE5B A5 10                    ..
	and     #$7F                            ; AE5D 29 7F                    ).
	sta     $10                             ; AE5F 85 10                    ..
	cli                                     ; AE61 58                       X
	jmp     LAE70                           ; AE62 4C 70 AE                 Lp.

; ----------------------------------------------------------------------------
LAE65:	.byte	10,"D:INIT.MAC"

LAE70:  lda     #>LAE65
	sta     $A3                             ; AE72 85 A3                    ..
	ldy     #<LAE65
	ldx     #$02                            ; AE76 A2 02                    ..
	lda     #$03                            ; AE78 A9 03                    ..
	jsr     sub_A2A8
	rts                                     ; AE7D 60                       `

; ----------------------------------------------------------------------------
LAE7E:  brk                                     ; AE7E 00                       .
LAE7F:  brk                                     ; AE7F 00                       .
LAE80:  brk                                     ; AE80 00                       .

; ----------------------------------------------------------------------------
sub_AE81:
	prolog
	ldy     #$00                            ; AE84 A0 00                    ..
	sty     L4764                           ; AE86 8C 64 47                 .dG
	sty     LAE7E                           ; AE89 8C 7E AE                 .~.
LAE8C:  lda     #$1F                            ; AE8C A9 1F                    ..
	cmp     LAE7E                           ; AE8E CD 7E AE                 .~.
	lbcc	LAED6
	ldx     LAE7E                           ; AE96 AE 7E AE                 .~.
	lda     $B14A,x                         ; AE99 BD 4A B1                 .J.
	sta     LAE7F                           ; AE9C 8D 7F AE                 ...
	ldx     LAE7E                           ; AE9F AE 7E AE                 .~.
	lda     $B800,x                         ; AEA2 BD 00 B8                 ...
	eor     #$01                            ; AEA5 49 01                    I.
	lbne	LAED0
	lda     LAE7F                           ; AEAC AD 7F AE                 ...
	cmp     #$80                            ; AEAF C9 80                    ..
	lbcs	LAED0
	ldx     LAE7E                           ; AEB6 AE 7E AE                 .~.
	lda     $BC00,x                         ; AEB9 BD 00 BC                 ...
	sta     LAE80                           ; AEBC 8D 80 AE                 ...
	ldx     LAE80                           ; AEBF AE 80 AE                 ...
	lda     LAE7F                           ; AEC2 AD 7F AE                 ...
	jsr     LA895                           ; AEC5 20 95 A8                  ..
	lda     #$00                            ; AEC8 A9 00                    ..
	ldx     LAE7E                           ; AECA AE 7E AE                 .~.
	sta     $B800,x                         ; AECD 9D 00 B8                 ...
LAED0:  inc     LAE7E                           ; AED0 EE 7E AE                 .~.
	jmp     LAE8C                           ; AED3 4C 8C AE                 L..

; ----------------------------------------------------------------------------
LAED6:  rts                                     ; AED6 60                       `

; ----------------------------------------------------------------------------
	brk                                     ; AED7 00                       .
	ora     ($02,x)                         ; AED8 01 02                    ..
	brk                                     ; AEDA 00                       .
	.byte   $03                             ; AEDB 03                       .
	brk                                     ; AEDC 00                       .
	brk                                     ; AEDD 00                       .
	brk                                     ; AEDE 00                       .
LAEDF:  .byte   $D7                             ; AEDF D7                       .
LAEE0:  .byte   $AE                             ; AEE0 AE                       .
LAEE1:  brk                                     ; AEE1 00                       .
LAEE2:  brk                                     ; AEE2 00                       .
LAEE3:  brk                                     ; AEE3 00                       .
LAEE4:  jmp     LAEE7                           ; AEE4 4C E7 AE                 L..

; ----------------------------------------------------------------------------
LAEE7:  lda     $D01F                           ; AEE7 AD 1F D0                 ...
	eor     #$FF                            ; AEEA 49 FF                    I.
	sta     $AE                             ; AEEC 85 AE                    ..
	lda     $AE                             ; AEEE A5 AE                    ..
	and     #$07                            ; AEF0 29 07                    ).
	sta     LAEE2                           ; AEF2 8D E2 AE                 ...
	lda     LAEE3                           ; AEF5 AD E3 AE                 ...
	eor     #$01                            ; AEF8 49 01                    I.
	lbne	LAF08
	lda     LAEE2                           ; AEFF AD E2 AE                 ...
	lbeq	LAF08
	rts                                     ; AF07 60                       `

; ----------------------------------------------------------------------------
LAF08:  ldy     #$00                            ; AF08 A0 00                    ..
	sty     LAEE3                           ; AF0A 8C E3 AE                 ...
	clc                                     ; AF0D 18                       .
	lda     LAEDF                           ; AF0E AD DF AE                 ...
	adc     LAEE2                           ; AF11 6D E2 AE                 m..
	sta     $AE                             ; AF14 85 AE                    ..
	lda     LAEE0                           ; AF16 AD E0 AE                 ...
	adc     #$00                            ; AF19 69 00                    i.
	sta     $AF                             ; AF1B 85 AF                    ..
	lda     ($AE),y                         ; AF1D B1 AE                    ..
	sta     LAEE1                           ; AF1F 8D E1 AE                 ...
	lda     LAEE1                           ; AF22 AD E1 AE                 ...
	lbeq	LAF35
	lda     LAEE1                           ; AF2A AD E1 AE                 ...
	jsr     sub_4BA7
	ldy     #$01                            ; AF30 A0 01                    ..
	sty     LAEE3                           ; AF32 8C E3 AE                 ...
LAF35:  rts                                     ; AF35 60                       `

; ----------------------------------------------------------------------------
LAF36:  brk                                     ; AF36 00                       .
LAF37:  brk                                     ; AF37 00                       .
LAF38:  brk                                     ; AF38 00                       .
LAF39:  brk                                     ; AF39 00                       .

sub_AF3A:
	prolog
	rdmv	L43BB, sub_5E5E+1
	jsr     sub_ADEA
LAF4C:  lda     #$01                            ; AF4C A9 01                    ..
	eor     #$01                            ; AF4E 49 01                    I.
	lbne	LB0BE
	ldy     #$00                            ; AF55 A0 00                    ..
	sty     $4D                             ; AF57 84 4D                    .M
	lda     L4652                           ; AF59 AD 52 46                 .RF
	eor     #$02                            ; AF5C 49 02                    I.
	lbeq	LAFA2
LAF63:	lda     L4652                           ; AF63 AD 52 46                 .RF
	eor     #$02                            ; AF66 49 02                    I.
	lbeq	LAF94
	lda     #$5C                            ; AF6D A9 5C                    .\
	sta     $A3                             ; AF6F 85 A3                    ..
	ldy     #$B2                            ; AF71 A0 B2                    ..
	ldx     #$23                            ; AF73 A2 23                    .#
	lda     L4652                           ; AF75 AD 52 46                 .RF
	jsr     L458F                           ; AF78 20 8F 45                  .E
	ldx     L4652                           ; AF7B AE 52 46                 .RF
	lda     $05C0,x                         ; AF7E BD C0 05                 ...
	bne     LAF8B                           ; AF81 D0 08                    ..
	lda     L464D                           ; AF83 AD 4D 46                 .MF
	bne     LAF8B                           ; AF86 D0 03                    ..
	jmp     LAF8E                           ; AF88 4C 8E AF                 L..

; ----------------------------------------------------------------------------
LAF8B:  jmp     LAF94                           ; AF8B 4C 94 AF                 L..

; ----------------------------------------------------------------------------
LAF8E:  jsr     sub_AB6A
	jmp     LAF63                           ; AF91 4C 63 AF                 Lc.

; ----------------------------------------------------------------------------
LAF94:  lda     L4652                           ; AF94 AD 52 46                 .RF
	jsr     LA28D                           ; AF97 20 8D A2                  ..
	lda     #$02                            ; AF9A A9 02                    ..
	sta     L4652                           ; AF9C 8D 52 46                 .RF
	jsr     L5E30                           ; AF9F 20 30 5E                  0^
LAFA2:  lda     $02FC                           ; AFA2 AD FC 02                 ...
	eor     #$FF                            ; AFA5 49 FF                    I.
	beq     LAFAC                           ; AFA7 F0 03                    ..
	jmp     LAFCB                           ; AFA9 4C CB AF                 L..

; ----------------------------------------------------------------------------
LAFAC:  lda     L474D                           ; AFAC AD 4D 47                 .MG
	eor     L474E                           ; AFAF 4D 4E 47                 MNG
	bne     LAFB7                           ; AFB2 D0 03                    ..
	jmp     LAFCB                           ; AFB4 4C CB AF                 L..

; ----------------------------------------------------------------------------
LAFB7:  ldx     L474E                           ; AFB7 AE 4E 47                 .NG
	lda     $B138,x                         ; AFBA BD 38 B1                 .8.
	sta     $02FC                           ; AFBD 8D FC 02                 ...
	inc     L474E                           ; AFC0 EE 4E 47                 .NG
	lda     L474E                           ; AFC3 AD 4E 47                 .NG
	and     #$0F                            ; AFC6 29 0F                    ).
	sta     L474E                           ; AFC8 8D 4E 47                 .NG
LAFCB:  lda     $02FC                           ; AFCB AD FC 02                 ...
	jsr     L907D                           ; AFCE 20 7D 90                  }.
	lda     $A0                             ; AFD1 A5 A0                    ..
	eor     #$01                            ; AFD3 49 01                    I.
	beq     LAFDA                           ; AFD5 F0 03                    ..
	jmp     LAFE2                           ; AFD7 4C E2 AF                 L..

; ----------------------------------------------------------------------------
LAFDA:  lda     #$FF                            ; AFDA A9 FF                    ..
	sta     $02FC                           ; AFDC 8D FC 02                 ...
	jmp     LB01A                           ; AFDF 4C 1A B0                 L..

; ----------------------------------------------------------------------------
LAFE2:  lda     $02BE                           ; AFE2 AD BE 02                 ...
	and     #$40                            ; AFE5 29 40                    )@
	sta     $02BE                           ; AFE7 8D BE 02                 ...
	ldy     #$00                            ; AFEA A0 00                    ..
	sty     $02B6                           ; AFEC 8C B6 02                 ...
	lda     $02FC                           ; AFEF AD FC 02                 ...
	and     #$3F                            ; AFF2 29 3F                    )?
	sta     LAF39                           ; AFF4 8D 39 AF                 .9.
	lda     LAF39                           ; AFF7 AD 39 AF                 .9.
	eor     #$3C                            ; AFFA 49 3C                    I<
	beq     LB001                           ; AFFC F0 03                    ..
	jmp     LB00C                           ; AFFE 4C 0C B0                 L..

; ----------------------------------------------------------------------------
LB001:  lda     $02FC                           ; B001 AD FC 02                 ...
	and     #$40                            ; B004 29 40                    )@
	sta     $02BE                           ; B006 8D BE 02                 ...
	jmp     LB012                           ; B009 4C 12 B0                 L..

; ----------------------------------------------------------------------------
LB00C:  lda     $02FC                           ; B00C AD FC 02                 ...
	jsr     L97A1                           ; B00F 20 A1 97                  ..
LB012:  lda     #$FF                            ; B012 A9 FF                    ..
	sta     $02FC                           ; B014 8D FC 02                 ...
	jmp     LAFA2                           ; B017 4C A2 AF                 L..

; ----------------------------------------------------------------------------
LB01A:  jsr     LAEE4                           ; B01A 20 E4 AE                  ..
	lda     #$AF                            ; B01D A9 AF                    ..
	.byte   $85                             ; B01F 85                       .
LB020:  .byte   $A3                             ; B020 A3                       .
	ldy     #$38                            ; B021 A0 38                    .8
	ldx     #$AF                            ; B023 A2 AF                    ..
	lda     #$37                            ; B025 A9 37                    .7
	jsr     L9CD0                           ; B027 20 D0 9C                  ..
	lda     $A0                             ; B02A A5 A0                    ..
	eor     #$01                            ; B02C 49 01                    I.
	beq     LB033                           ; B02E F0 03                    ..
	jmp     LB07D                           ; B030 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB033:  lda     L46E6                           ; B033 AD E6 46                 ..F
	eor     #$03                            ; B036 49 03                    I.
	beq     LB03D                           ; B038 F0 03                    ..
	jmp     LB049                           ; B03A 4C 49 B0                 LI.

; ----------------------------------------------------------------------------
LB03D:  ldx     LAF38                           ; B03D AE 38 AF                 .8.
	lda     LAF37                           ; B040 AD 37 AF                 .7.
	jsr     L840C                           ; B043 20 0C 84                  ..
	jmp     LB07D                           ; B046 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB049:  lda     L46E6                           ; B049 AD E6 46                 ..F
	eor     #$02                            ; B04C 49 02                    I.
	beq     LB053                           ; B04E F0 03                    ..
	jmp     LB062                           ; B050 4C 62 B0                 Lb.

; ----------------------------------------------------------------------------
LB053:  ldy     LAF38                           ; B053 AC 38 AF                 .8.
	ldx     LAF37                           ; B056 AE 37 AF                 .7.
	lda     L46E9                           ; B059 AD E9 46                 ..F
	jsr     sub_768A
	jmp     LB07D                           ; B05F 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB062:  lda     L46E6                           ; B062 AD E6 46                 ..F
	eor     #$04                            ; B065 49 04                    I.
	beq     LB06C                           ; B067 F0 03                    ..
	jmp     LB07D                           ; B069 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB06C:  lda     LAF38                           ; B06C AD 38 AF                 .8.
	sta     $A3                             ; B06F 85 A3                    ..
	ldy     LAF37                           ; B071 AC 37 AF                 .7.
	ldx     L46EA                           ; B074 AE EA 46                 ..F
	lda     L46E9                           ; B077 AD E9 46                 ..F
	jsr     LA027                           ; B07A 20 27 A0                  '.
LB07D:  jsr     L54FF                           ; B07D 20 FF 54                  .T
	lda     $A0                             ; B080 A5 A0                    ..
	sta     LAF36                           ; B082 8D 36 AF                 .6.
	lda     LAF36                           ; B085 AD 36 AF                 .6.
	eor     #$01                            ; B088 49 01                    I.
	lbne	LB092
	jsr     sub_AB6A
LB092:  jsr     L9DCB                           ; B092 20 CB 9D                  ..
	lda     $A0                             ; B095 A5 A0                    ..
	eor     #$01                            ; B097 49 01                    I.
	lbne	LB0A3
	lda     #$00                            ; B09E A9 00                    ..
	jsr     sub_4BA7
LB0A3:  jsr     LA9A3                           ; B0A3 20 A3 A9                  ..
	lda     L4764                           ; B0A6 AD 64 47                 .dG
	eor     #$01                            ; B0A9 49 01                    I.
	lbne	LB0B3
	jsr     sub_AE81
LB0B3:  jsr     L8D01                           ; B0B3 20 01 8D                  ..
	ldy     #$00                            ; B0B6 A0 00                    ..
	sty     L464D                           ; B0B8 8C 4D 46                 .MF
	jmp     LAF4C                           ; B0BB 4C 4C AF                 LL.

; ----------------------------------------------------------------------------
LB0BE:  rts                                     ; B0BE 60                       `

; ----------------------------------------------------------------------------
	rts                                     ; B0BF 60                       `

; ----------------------------------------------------------------------------

	.segment "HDR01"

	.addr	$02E2,$02E3

	.segment "SEG01"

	.addr	sub_AF3A

