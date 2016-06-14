
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
L9CC3:	.byte   $04,$02,$03,$FF,$06,$08,$07,$FF,$05,$01,$00
L9CCE:	.addr	L9CC3

sub_9CD0:
	stack_prolog L9CBA, $03
	lda     L46E7                           ; 9CD9 AD E7 46                 ..F
	lbne	L9CEB
	yldi	L9CC2, $00
	ldi	$A0, $00
	rts                                     ; 9CEA 60                       `

; ----------------------------------------------------------------------------
L9CEB:	proc8i	read_stick, $00
	sub8i	L9CC1, $A0, $05
	add16m8	off_AE, L9CCE, L9CC1
	ldp8	L9CC1
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

