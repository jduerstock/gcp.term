
.ifndef MONOLITH
	.include "action.inc"

	.import Close
	.import XIO

	.import L4650

	.import modem_status

	.import sub_45A3
	.import sub_4F6D
	.import sub_5394
	.import sub_55A0
.endif

; ----------------------------------------------------------------------------
sub_5CFC:  
	prolog
	jmp     L5D04                           ; 5CFF 4C 04 5D                 L.]

; ----------------------------------------------------------------------------
L5D02:	.byte	$01,"C"

; ----------------------------------------------------------------------------
L5D04:	ldi	$A3, $00
	ldy     #$13                            ; 5D08 A0 13                    ..
	ldxai	L5D02
	jsr     sub_55A0
L5D11:  lda     L4650                           ; 5D11 AD 50 46                 .PF
	lbeq	L5D1F
	jsr     sub_5394
	jmp     L5D11                           ; 5D1C 4C 11 5D                 L.]
;--		while (L4650 != 0) {
;--			sub_5394();
;--		}

; ----------------------------------------------------------------------------
L5D1F:	yldi	SDMCTL, $00
	proc8i	delay, $03
L5D29:  jsr     modem_status
	lda     $A0                             ; 5D2C A5 A0                    ..
	lbeq	L5D40
L5D33:	proc8i	GetD, $02
	proc8i	delay, $02
	jmp     L5D29                           ; 5D3D 4C 29 5D                 L)]

; ----------------------------------------------------------------------------
L5D40:	ifm8eqi $B148, $01, L5D5E
	ldi	$A3, $00
	ldi	$A4, $A1
	ldy     #$5A                            ; 5D52 A0 5A                    .Z
	ldx     #$00                            ; 5D54 A2 00                    ..
	lda     #$02                            ; 5D56 A9 02                    ..
	jsr     XIO
	jmp     L5D63                           ; 5D5B 4C 63 5D                 Lc]

; ----------------------------------------------------------------------------
L5D5E:	proc8i Close, $02
L5D63:  rts                                     ; 5D63 60                       `

