
; ----------------------------------------------------------------------------
L6192:  .byte	$00
L6193:  .byte	$00
L6194:	.byte	$00,$00
L6196:  .byte	$00
L6197:  .byte	$00
L6198:  .byte	$00
L6199:  .byte	$00

; ----------------------------------------------------------------------------
sub_619A:  
	stack_prolog L6192, $03
	add16i off_AE, L6194, $0003
	ldi	$84, $02
	ld2xa	off_AE
	jsr     RShift
	st2xa	L6196
	sub16m	off_AE, L6192, MEMLO
	ldi	$84, $02
	ld2xa	off_AE
	jsr     RShift
	st2xa	L6198
	mv	$A3, L6197
	ldi	$A4, $01
	ldy     L6196                           ; 61F2 AC 96 61                 ..a
	ldxa	L6198
	jsr     sub_5FF5
	rts                                     ; 61FE 60                       `

