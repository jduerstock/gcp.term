
; ----------------------------------------------------------------------------
LA887:  .byte	$00
LA888:  .byte	$00
LA889:  .byte	$00
	.byte	$00
LA88B:  .byte	$00,$00
LA88D:  .byte	$00
LA88E:  .byte	$00
LA88F:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
LA893:  .byte	$00,$00


; ----------------------------------------------------------------------------
cmd_40:						; "@" "BB"
	prolog
;--	void cmd_40(uint8_t a, uint8_t x)
	stxa	LA887
	func16_8 sub_65B0, LA88D, LA887
;--		uint16_t LA88D;
;--
;--		LA88D = sub_65B0(a);
	blkmv_imi LA88F, LA88D, $0006
	shladdm8 off_AE, LA893, LA888
	ldp16	LA88B
LA8E6:	dmv	off_AE, LA88B
	ldy     #$00                            ; A8F0 A0 00                    ..
	lda     (off_AE),y
	eor     #$1E                            ; A8F4 49 1E                    I.
	lbne	LA8FE
	jmp     LA958                           ; A8FB 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA8FE:	yldi	LA889, $00
LA903:  lda     #$5B                            ; A903 A9 5B                    .[
	cmp     LA889                           ; A905 CD 89 A8                 ...
	lbcc	LA936
	add16m8	off_AE, LA88B, LA889
	ldp8	$A0
	lda	$A0
	jsr	sub_4BC9
	lda	$A0
	ldx     LA889                           ; A92A AE 89 A8                 ...
	sta     LB224,x                         ; A92D 9D 24 B2                 .$.
	inc     LA889                           ; A930 EE 89 A8                 ...
	jmp     LA903                           ; A933 4C 03 A9                 L..

; ----------------------------------------------------------------------------
LA936:  jsr     sub_4749
	lda     $A0                             ; A939 A5 A0                    ..
	lbne	LA943
LA940:  jmp     LA958                           ; A940 4C 58 A9                 LX.

; ----------------------------------------------------------------------------
LA943:	add16m8	LA88B, LA88B, L4654
	jmp     LA8E6                           ; A955 4C E6 A8                 L..

; ----------------------------------------------------------------------------
LA958:  rts                                     ; A958 60                       `

