
; ----------------------------------------------------------------------------
L8177:	.byte   $F4                             ; 8177 F4                       .
	.byte	$48
L8179:  .byte	$60
L817A:	.byte	$AD                             ; 817A AD                       .
L817B:	.byte	$6F                             ; 817B 6F                       o

; ----------------------------------------------------------------------------
cmd_um:						; "M" "BBA0"
	stack_prolog L8177, $03
	blkmv_iii L4753, L8177, $0002
	dmv	off_AE, L8179
	ldp8	L817B
	mv	$A3, L8179+1
	add8i	$A4, L817B, $01
	ldi	$A5, $00
	ldy     L8179                           ; 81BC AC 79 81                 .y.
	ldxai	L4755
	jsr     blockmove
	lda     #$00                            ; 81C6 A9 00                    ..
	sta     $A3                             ; 81C8 85 A3                    ..
	lda     #$00                            ; 81CA A9 00                    ..
	sta     $A4                             ; 81CC 85 A4                    ..
	ldy     L817B                           ; 81CE AC 7B 81                 .{.
	ldxai	$4756
	jsr     sub_4A53
	rts                                     ; 81D8 60                       `

