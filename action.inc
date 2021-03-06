
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

.macro	lbcc	addr, ofs
	.local	untrue
	.if	.paramcount = 2
		bcs	untrue+ofs
	.else
		bcs	untrue
	.endif
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
	jsr	SArgs
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

.macro	func8 f1, a1
	jsr     f1
	mv	a1, $A0
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

.macro	ldp16y0	a1
	iny
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
	lda	(off_AE),y
	sta	a1
.endmacro

.macro	stp8	a1
	lda	a1
	ldy	#$00
	sta	(off_AE),y
.endmacro

.macro	stp8a	a1
	lda	a1
	sta	(off_AE),y
.endmacro

.macro	stp16	a1
	lda     a1+1
	ldy     #$01
	sta     (off_AE),y
	lda     a1
	dey
	sta     (off_AE),y
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
	.if	.strlen(a1) > 0
		.byte	.strlen(a1)
		.byte	a1
	.else
		.byte	$00
	.endif
	.addr	ptr
.endmacro

.ifndef MONOLITH
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
.endif

.macro	ifm8eqi	a1, a2, a3
	lda     a1
	eor     #a2
	lbne	a3
.endmacro

.macro	ifm8eqm	a1, a2, a3
	lda     a1
	eor     a2
	lbne	a3
.endmacro

.macro	ifm8nei	a1, a2, a3
	lda     a1
	eor     #a2
	lbeq	a3
.endmacro

.macro	ifm8nem	a1, a2, a3
	lda     a1
	eor     a2
	lbeq	a3
.endmacro

.macro	ifm8nz	a1, a2
	lda     a1
	lbeq	a2
.endmacro

.macro	ifm8z	a1, a2
	lda     a1
	lbne	a2
.endmacro

.macro	ifm8gei	a1, a2, a3
	lda     a1
	eor     #a2
	lbcs	a3
.endmacro

.macro	ifm8gem	a1, a2, a3
	lda     a1
	eor     a2
	lbcs	a3
.endmacro

