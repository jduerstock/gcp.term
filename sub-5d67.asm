
sub_5D67:  
	prolog
	lda     $B148                           ; 5D6A AD 48 B1                 .H.
	eor     #$01                            ; 5D6D 49 01                    I.
	lbeq	L5DB9
	lda     #$02                            ; 5D74 A9 02                    ..
	jsr     Close
	ldi	$A3, $0D
	ldy     #>L4AA1
	ldx     #<L4AA1
	lda     #$02                            ; 5D81 A9 02                    ..
	jsr     Open
	mv	$A3, $B149
	ldi	$A4, $00
	rdldi	$A5, L4AA1
	ldy     #$24                            ; 5D97 A0 24                    .$
	ldx     #$00                            ; 5D99 A2 00                    ..
	lda     #$02                            ; 5D9B A9 02                    ..
	jsr     XIO
	dldi	$A3, $0020
	rdldi	$A5, L4AA1
	ldy     #$26                            ; 5DB0 A0 26                    .&
	ldx     #$00                            ; 5DB2 A2 00                    ..
	lda     #$02                            ; 5DB4 A9 02                    ..
	jsr     XIO
L5DB9:	ldi	$A4, $00
	ldi	$A3, $80
	ldi	$A5, $28
	ldi	$A6, $0D
	ldi	$A7, $00
	ldy     #$06                            ; 5DCD A0 06                    ..
	ldx     #$00                            ; 5DCF A2 00                    ..
	lda     #$02                            ; 5DD1 A9 02                    ..
	jsr     sub_4AA5
	lda     $B148                           ; 5DD6 AD 48 B1                 .H.
	eor     #$01                            ; 5DD9 49 01                    I.
	lbne	L5DF9
L5DE0:  lda     #$00                            ; 5DE0 A9 00                    ..
	sta     $A3                             ; 5DE2 85 A3                    ..
	lda     #$00                            ; 5DE4 A9 00                    ..
	sta     $A4                             ; 5DE6 85 A4                    ..
	rdldi	$A5, L4AA1
	ldy     #$59                            ; 5DF0 A0 59                    .Y
	ldx     #$00                            ; 5DF2 A2 00                    ..
	lda     #$02                            ; 5DF4 A9 02                    ..
	jsr     XIO
L5DF9:  jsr     sub_5D64
	ldi	SDMCTL, $2A
	lda     #$02                            ; 5E01 A9 02                    ..
	jsr     delay
	yldi	L4653, $00
	jmp     L5E10                           ; 5E0B 4C 10 5E                 L.^

; ----------------------------------------------------------------------------
L5E0E:	.byte	$01,"C"

; ----------------------------------------------------------------------------
L5E10:	ldi	$A3, $00
	ldy     #$11                            ; 5E14 A0 11                    ..
	ldxai	L5E0E
	jsr     sub_55A0
	rts                                     ; 5E1D 60                       `

