
; ----------------------------------------------------------------------------
L71A1:	.byte	$00
L71A2:  .byte	$00
L71A3:  .byte	$00
L71A4:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L71A8:	.byte	$00
	.byte	$00
	.byte	$00
	.byte	$00
L71AC:  .byte	$00
L71AD:  .byte	$00
L71AE:	.byte	$00
	.byte	$00
L71B0:  .byte	$00
L71B1:  .byte	$00
L71B2:  .byte	$00
L71B3:  .byte	$00
L71B4:  .byte	$00

; ----------------------------------------------------------------------------
sub_71B5:  
	prolog
	sta     L71A1                           ; 71B8 8D A1 71                 ..q
	func16_8 sub_7035, L71A2, L71A1
	blkmv_imi L71AE, L71A2, $0007
	func16_8 sub_65B0, L71AC, L71B2
	sec                                     ; 71F2 38                       8
	lda     #$00                            ; 71F3 A9 00                    ..
	sbc     L71B0                           ; 71F5 ED B0 71                 ..q
	sta     $A2                             ; 71F8 85 A2                    ..
	sec                                     ; 71FA 38                       8
	lda     #$00                            ; 71FB A9 00                    ..
	sbc     L71B1                           ; 71FD ED B1 71                 ..q
	sta     $A3                             ; 7200 85 A3                    ..
	dmv	$AA, L71AC
	sec                                     ; 720C 38                       8
	ldy     #$00                            ; 720D A0 00                    ..
	lda     ($AA),y                         ; 720F B1 AA                    ..
	sbc     #$01                            ; 7211 E9 01                    ..
	sta     $A8                             ; 7213 85 A8                    ..
	sec                                     ; 7215 38                       8
	lda     $A8                             ; 7216 A5 A8                    ..
	sbc     L71B0                           ; 7218 ED B0 71                 ..q
	sta     $A4                             ; 721B 85 A4                    ..
	add16i	$A8, L71AC, $0001
	sec                                     ; 722C 38                       8
	lda     ($A8),y                         ; 722D B1 A8                    ..
	sbc     #$01                            ; 722F E9 01                    ..
	sta     $A6                             ; 7231 85 A6                    ..
	sub8m	$A5, $A6, L71B1
	ldy     $A2                             ; 723B A4 A2                    ..
	ldxai	L71A4
	jsr     sub_4BF2
	ldi	$A3, $00
	sub8i	$A4, L71B3, $01
	sub8i	$A5, L71B4, $01
	ldy     #$00                            ; 7258 A0 00                    ..
	ldxai	L71A8
	jsr     sub_4BF2
	add16i	off_AE, L71A2, $000B
	push16	off_AE
	ldi	$A3, $71
	add16i	$A4, L71A2, $0016
	ldy     #$A8                            ; 7289 A0 A8                    ..
	ldxai	L71A4
	jsr     sub_4CF5
	pull16	off_AE
	stp8	$A0
	rts                                     ; 729E 60                       `

