
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

