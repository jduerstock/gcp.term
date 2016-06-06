
; ----------------------------------------------------------------------------
LAD81:  .byte	$00
LAD82:  .byte	$00
LAD83:	.byte	$D5
LAD84:  .byte	$00

; ----------------------------------------------------------------------------
sub_AD85:
	prolog
	rdldi	$0228, LAD2E
	yldi	$021B, $00
	ldi	$021A, $06
	ldi	$A3, $00
	ldy     #$06                            ; ADA0 A0 06                    ..
	ldxa	LAD83
	jsr     sub_45F6
	rdldi	$0208, $AC7E
	rdldi	LAD81, sub_AD59
	ldi	$84, $08
	lda     LAD82                           ; ADC3 AD 82 AD                 ...
	tax                                     ; ADC6 AA                       .
	lda     LAD81                           ; ADC7 AD 81 AD                 ...
	jsr     sub_43E0
	sta     $A1                             ; ADCD 85 A1                    ..
	lda     LAD81                           ; ADCF AD 81 AD                 ...
	and     #$FF                            ; ADD2 29 FF                    ).
	sta     $A2                             ; ADD4 85 A2                    ..
	ldy     $A2                             ; ADD6 A4 A2                    ..
	ldx     $A1                             ; ADD8 A6 A1                    ..
	lda     #$06                            ; ADDA A9 06                    ..
	jsr     SETVBV
	rdldi	VDSLST, LAD07
	rts                                     ; ADE9 60                       `

