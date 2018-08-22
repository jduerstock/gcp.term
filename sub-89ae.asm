
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
	blkmv_imi L8997, L8962, $000E
	func16_8 sub_65B0, L898D, L899B
	add16i	L896B, L8962, $001A
	blkmv_imi L89A5, L896B, $0004
	sub8m	off_AE, L89A7, L89A5
	add8i	L8968, off_AE, $01
	add8m	$A0, L89A6, L8998
	func16_8 sub_4945, L8995, $A0
	add8m	$A0, L89A5, L8997
	func16_8 sub_4945, L8993, $A0
	rdldi	$84, $0028
	ld2xa	L8995
	jsr     MultI
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
	jsr     SetBlock
	add16i	L8964, L8964, $0028
	inc     L8992                           ; 8AD3 EE 92 89                 ...
	jmp     L8AA1                           ; 8AD6 4C A1 8A                 L..

; ----------------------------------------------------------------------------
L8AD9:	ifm8eqi	L89A2, $01, L8B4E
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
	lda     (off_AE),y
	sta     L8BAC                           ; 8B9E 8D AC 8B                 ...
L8BA1:  lda     L8BAC                           ; 8BA1 AD AC 8B                 ...
	cmp     L8992                           ; 8BA4 CD 92 89                 ...
	bcs     L8BAD                           ; 8BA7 B0 04                    ..
	jmp     L8CD5                           ; 8BA9 4C D5 8C                 L..

; ----------------------------------------------------------------------------
L8BAC:  .byte	$00

; ----------------------------------------------------------------------------
L8BAD:	blkmv_imi L89A9, L8969, $0005
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
	lda     (off_AE),y
	sbc     #$01                            ; 8BF4 E9 01                    ..
	sta     $A4                             ; 8BF6 85 A4                    ..
	add16i	off_AE, L898F, $0001
	;
	sec                                     ; 8C07 38                       8
	lda     (off_AE),y
	sbc     #$01                            ; 8C0A E9 01                    ..
	sta     $A5                             ; 8C0C 85 A5                    ..
	;
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
	dmv	$A4, L8997
	ldy     #<L8979
	ldxai	L8975
	jsr     sub_4C1D
	lda     #$89                            ; 8C88 A9 89                    ..
	sta     $A3                             ; 8C8A 85 A3                    ..
	sec                                     ; 8C8C 38                       8
	lda     L8999                           ; 8C8D AD 99 89                 ...
	sbc     L89AB                           ; 8C90 ED AB 89                 ...
	sta     $A4                             ; 8C93 85 A4                    ..
	;
	sec                                     ; 8C95 38                       8
	lda     L899A                           ; 8C96 AD 9A 89                 ...
	sbc     L89AC                           ; 8C99 ED AC 89                 ...
	sta     $A5                             ; 8C9C 85 A5                    ..
	;
	ldy     #$79                            ; 8C9E A0 79                    .y
	ldxai	L8981
	jsr     sub_4C1D
	;
	ldi	$A3, >L8981
	ldi	$A5, >L8975
	ldi	$A4, <L8975
	ldy     #<L8981
	ldxa	L898F
	jsr     sub_884C
	;
L8CBE:	add16i	L8969, L8969, $0006
	inc     L8992                           ; 8CCF EE 92 89                 ...
	jmp     L8BA1                           ; 8CD2 4C A1 8B                 L..

; ----------------------------------------------------------------------------
L8CD5:	mv	$A3, L896C
	dmv	$A4, L8997
	ldy     L896B                           ; 8CE4 AC 6B 89                 .k.
	ldxai	L897D
	jsr     sub_4C1D
	;
	ldyxi	L897D
	lda     L8961                           ; 8CF2 AD 61 89                 .a.
	jsr     sub_8573
	rts                                     ; 8CF8 60                       `

