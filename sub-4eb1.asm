
; ----------------------------------------------------------------------------
L4EAB:	.byte	$49,$46
L4EAD:	.byte	$20,$67
L4EAF:	.byte	$74,$28

sub_4EB1:  
	stack_prolog L4EAB, $05
	add16m	off_AE, L4EAB, L4EAF
	sub16i	L4EAB, off_AE, $0001
	add16m	off_AE, L4EAD, L4EAF
	sub16i	L4EAD, off_AE, $0001
L4EFA:  lda     #$00                            ; 4EFA A9 00                    ..
	cmp     L4EAF                           ; 4EFC CD AF 4E                 ..N
	lda     #$00                            ; 4EFF A9 00                    ..
	sbc     L4EAF+1
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

