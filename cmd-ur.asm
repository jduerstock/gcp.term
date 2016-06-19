
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
L80B7:	ora     #$C8                            ; 80B7 09 C8                    ..
L80B9:  inx                                     ; 80B9 E8                       .
L80BA:  .byte	$C9

cmd_ur:						; "R" "BB"
	prolog
	stxa	L80AA
	lda     L474F                           ; 80C4 AD 4F 47                 .OG
	eor     #$01                            ; 80C7 49 01                    I.
	lbne	L80DA
L80CE:	ldxa	L80AA
	jsr     sub_8047
	jmp     L8176                           ; 80D7 4C 76 81                 Lv.

; ----------------------------------------------------------------------------
L80DA:  lda     L4750                           ; 80DA AD 50 47                 .PG
	jsr     sub_7035
	rdmv	L80AC, $A0
	blkmv_imi L80B2, L80AC, $0005
	add8m	L80B0, L80AA, L80B2
	add8m	L80B1, L80AB, L80B3
	lda     L474F                           ; 8115 AD 4F 47                 .OG
	eor     #$03                            ; 8118 49 03                    I.
	lbne	L816D
	ldx     L46EA                           ; 811F AE EA 46                 ..F
	lda     L80B6                           ; 8122 AD B6 80                 ...
	jsr     sub_799B
	rdmv	L80AE, $A0
	blkmv_imi L80B7, L80AE, $0004
	add8m	off_AE, L80B0, L80B9
	sub8m	L80B0, off_AE, L80B4
	add8m	off_AE, L80B1, L80BA
	sub8m	L80B1, off_AE, L80B5
L816D:	ldxa	L80B0
	jsr     sub_8047
L8176:  rts                                     ; 8176 60                       `

