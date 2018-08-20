
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
	jsr     Zero
	rdldi	L8F7B, L46F9
	yldi	L8F7A, $00
L8F9C:  lda     #$09                            ; 8F9C A9 09                    ..
	cmp     L8F7A                           ; 8F9E CD 7A 8F                 .z.
	lbcc	L8FDE
	shladdm8 off_AE, L46F5, L8F7A
	stp16	L8F7B
	add16i	L8F7B, L8F7B, $0008
	inc     L8F7A                           ; 8FD8 EE 7A 8F                 .z.
	jmp     L8F9C                           ; 8FDB 4C 9C 8F                 L..

; ----------------------------------------------------------------------------
L8FDE:	dmv	off_AE, L4678
	lda     #$E0                            ; 8FE8 A9 E0                    ..
	ldy     #$00                            ; 8FEA A0 00                    ..
	sta     (off_AE),y
	add16i	off_AE, L4678, $0001
	ldi	$84, $08
	ld2xa	L4674
	jsr     RShift
	ldy     #$00                            ; 900B A0 00                    ..
	sta     (off_AE),y
	blkmv_mii L4674, LE000, $0400
	ldy     #$18                            ; 9026 A0 18                    ..
	ldx     #$02                            ; 9028 A2 02                    ..
	lda     #$00                            ; 902A A9 00                    ..
	jsr     cmd_ug
	ldi	$A3, $CA
	ldi	$A4, $94
	ldi	$A5, <L8846
	ldi	$A6, >L8846
	ldy     #$28                            ; 903F A0 28                    .(
	ldx     #$00                            ; 9041 A2 00                    ..
	lda     #$00                            ; 9043 A9 00                    ..
	jsr     cmd_ua
	ldx     #$0F                            ; 9048 A2 0F                    ..
	lda     #$00                            ; 904A A9 00                    ..
	jsr     cmd_uo
	rts                                     ; 904F 60                       `

