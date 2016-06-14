
; ----------------------------------------------------------------------------
LA2A2:  .byte	$00
LA2A3:  .byte	$00
LA2A4:  .byte	$00
LA2A5:  .byte	$00
LA2A6:  .byte	$00
LA2A7:  .byte	$00

; ----------------------------------------------------------------------------
cmd_d2:
	stack_prolog LA2A2, $03
	jsr     cmd_d0
	lda     LA2A2                           ; A2B4 AD A2 A2                 ...
	jsr     sub_A28D
	ldi	LA2A6, $04
	lda     LA2A3                           ; A2BF AD A3 A2                 ...
	eor     #$01                            ; A2C2 49 01                    I.
	lbne	LA2CE
	ldi	LA2A6, $08
LA2CE:  lda     #$00                            ; A2CE A9 00                    ..
	ldx     LA2A2                           ; A2D0 AE A2 A2                 ...
	sta     L05C0,x                         ; A2D3 9D C0 05                 ...
	add16i	off_AE, LA2A4, $0002
	ldp8	LA2A7
	lda     LA2A7                           ; A2EC AD A7 A2                 ...
	eor     #$3F                            ; A2EF 49 3F                    I?
	lbne	LA30A
	add16i	off_AE, LA2A4, $0002
	lda     L464A                           ; A305 AD 4A 46                 .JF
	sta     ($AE),y                         ; A308 91 AE                    ..
LA30A:  lda     LA2A6                           ; A30A AD A6 A2                 ...
	sta     $A3                             ; A30D 85 A3                    ..
	lda     #$00                            ; A30F A9 00                    ..
	sta     $A4                             ; A311 85 A4                    ..
	ldy     LA2A5                           ; A313 AC A5 A2                 ...
	ldx     LA2A4                           ; A316 AE A4 A2                 ...
	lda     LA2A2                           ; A319 AD A2 A2                 ...
	jsr     sub_4539
	lda     LA2A7                           ; A31F AD A7 A2                 ...
	eor     #'?'
	lbne	LA33E
	add16i	off_AE, LA2A4, $0002
	lda     #$3F                            ; A338 A9 3F                    .?
	ldy     #$00                            ; A33A A0 00                    ..
	sta     ($AE),y                         ; A33C 91 AE                    ..
LA33E:  lda     LA2A3                           ; A33E AD A3 A2                 ...
	eor     #$01                            ; A341 49 01                    I.
	lbeq	LA36C
	lda     #$7F                            ; A348 A9 7F                    ..
	cmp	L4AA4
	lbcs	LA36C
	lda     LA2A2                           ; A352 AD A2 A2                 ...
	asl     a                               ; A355 0A                       .
	asl     a                               ; A356 0A                       .
	asl     a                               ; A357 0A                       .
	asl     a                               ; A358 0A                       .
	sta     $A1                             ; A359 85 A1                    ..
	ldx     $A1                             ; A35B A6 A1                    ..
	lda     L4AA4                           ; A35D AD A4 4A                 ..J
	jsr     sub_5E5E                        ; A360 20 5E 5E                  ^^
	lda     LA2A2                           ; A363 AD A2 A2                 ...
	jsr     sub_A28D
	jmp     LA37D                           ; A369 4C 7D A3                 L}.

; ----------------------------------------------------------------------------
LA36C:  lda     LA2A3                           ; A36C AD A3 A2                 ...
	eor     #$02                            ; A36F 49 02                    I.
	lbne	LA37D
	mv	L4652, LA2A2
	rts                                     ; A37C 60                       `

; ----------------------------------------------------------------------------
LA37D:  jsr     cmd_d1
	rts                                     ; A380 60                       `

