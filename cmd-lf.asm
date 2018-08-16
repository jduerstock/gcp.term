
; ----------------------------------------------------------------------------
L70D6:	.byte	$00
L70D7:	.byte	$00
L70D8:	.byte	$00
L70D9:	.byte	$63,$6B
L70DB:	.byte	$7F
L70DC:	.byte	$3E
L70DD:	.byte	$36
L70DE:  .byte	$00
L70DF:  .byte	$00
L70E0:  .byte	$00
L70E1:	.byte	$66

; ----------------------------------------------------------------------------
cmd_lf:						; "f" "BR"
	stack_prolog L70D6, $02
	func16_8 sub_7035, L70D9, L70D6
	add16i	L70DD, L70D9, $0022
	blkmv_mmi L70DD, L70D7, $0004
	add16i	off_AE, L70D9, $0004
	ldp8	L70DF
	func16_8 sub_65B0, L70DB, L70DF
	blkmv_imi L70E0, L70DB, $0002
	add16i	off_AE, L70DD, $0002
	clc                                     ; 7171 18                       .
	ldy     #$00                            ; 7172 A0 00                    ..
	lda     (off_AE),y
	adc     L70E0                           ; 7176 6D E0 70                 m.p
	sta     $AC                             ; 7179 85 AC                    ..
	sec                                     ; 717B 38                       8
	lda     $AC                             ; 717C A5 AC                    ..
	sbc     #$01                            ; 717E E9 01                    ..
	sta     (off_AE),y
	add16i	off_AE, L70DD, $0003
	clc                                     ; 7191 18                       .
	lda     (off_AE),y
	adc     L70E1                           ; 7194 6D E1 70                 m.p
	sta     $AC                             ; 7197 85 AC                    ..
	sec                                     ; 7199 38                       8
	lda     $AC                             ; 719A A5 AC                    ..
	sbc     #$01                            ; 719C E9 01                    ..
	sta     (off_AE),y
	rts                                     ; 71A0 60                       `

