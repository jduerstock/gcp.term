
; ----------------------------------------------------------------------------
LA9F9:	.word	$0000

; ----------------------------------------------------------------------------
cmd_li:						; "i" "" -- initialize?
	prolog
	sub16m	LA9F9, MEMTOP, MEMLO
	mv	$A3, LA9F9+1
	ldy     LA9F9                           ; AA16 AC F9 A9                 ...
	ldxa	MEMLO
	jsr     sub_619A
	jmp     LAA28                           ; AA22 4C 28 AA                 L(.

; ----------------------------------------------------------------------------
LAA25:	.byte	$02,"CH"

; ----------------------------------------------------------------------------
LAA28:	ldi	$A3, $00
	rdmv	$A4, LA9F9
	ldy     #$46                            ; AA36 A0 46                    .F
	ldxai	LAA25
	jsr     sub_55A0
	jsr     sub_747D
	jsr     sub_696A
	jsr	sub_8020
	jsr     sub_8F7D
	proc8i	cmd_uj, $00
	proc8i	cmd_uh, $00
	ldi	$A3, $00
	ldi	$A4, $FF
	ldy     #$20                            ; AA5D A0 20                    . 
	ldxai	LB14A
	jsr     memset
	ldi	$A3, $00
	ldy     #$10                            ; AA6A A0 10                    ..
	ldxai	LB118
	jsr	Zero
	ldi	L4647, $FF
	ldi	L4648, $FF
	proc8i	cmd_ue, $01
	rts                                     ; AA82 60                       `

