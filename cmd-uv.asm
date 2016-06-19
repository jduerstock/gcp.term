
; ----------------------------------------------------------------------------
L7AD9:  .byte	$00
L7ADA:	.byte	$91
L7ADB:	.byte	$45
L7ADC:  .byte	$38
L7ADD:  .byte	$08
L7ADE:	.byte	$A5

; ----------------------------------------------------------------------------
cmd_uv:						; "V" "BBBB"
	stack_prolog L7AD9, $03
	ldxa	L7AD9
	jsr     sub_799B
	rdmv	L7ADD, $A0
	test16	L7ADD
	lbne	L7B07
	rts                                     ; 7B06 60                       `

; ----------------------------------------------------------------------------
L7B07:  lda     L7ADB                           ; 7B07 AD DB 7A                 ..z
	eor     #$80                            ; 7B0A 49 80                    I.
	lbeq	L7B27
	add16i	off_AE, L7ADD, $02
	stp8	L7ADB
L7B27:  lda     L7ADC                           ; 7B27 AD DC 7A                 ..z
	eor     #$80                            ; 7B2A 49 80                    I.
	lbeq	L7B47
	add16i	off_AE, L7ADD, $03
	stp8	L7ADC
L7B47:	yldi	L4656, $01
	rts                                     ; 7B4C 60                       `

