
; ----------------------------------------------------------------------------
L7362:	.byte	$2D                             ; 7362 2D                       -
L7363:	.byte	$F0                             ; 7363 F0                       .
L7364:	.byte	$20                             ; 7364 20                        
L7365:	.byte	$90                             ; 7365 90                       .
L7366:	.byte	$ED                             ; 7366 ED                       .
L7367:	.byte	$AD                             ; 7367 AD                       .

; ----------------------------------------------------------------------------
sub_7368:
	stack_prolog L7362, $02
	func16_8 sub_7035, L7365, L7364
	dmv	off_AE, L7365
	sec                                     ; 738B 38                       8
	lda     L7362                           ; 738C AD 62 73                 .bs
	ldy     #$00                            ; 738F A0 00                    ..
	sbc     (off_AE),y
	sta     L7362                           ; 7393 8D 62 73                 .bs
	add16i	off_AE, L7365, $0001
	sec                                     ; 73A5 38                       8
	lda     L7363                           ; 73A6 AD 63 73                 .cs
	sbc     (off_AE),y
	sta     L7363                           ; 73AB 8D 63 73                 .cs
	add16i	$A2, L7365, $001A
	ldy     $A2                             ; 73BD A4 A2                    ..
	ldxa	L7362
	jsr     sub_4C75
	mv	L7367, $A0
	mv	$A0, L7367
	rts                                     ; 73D2 60                       `

