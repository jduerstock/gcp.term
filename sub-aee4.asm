
; ----------------------------------------------------------------------------
LAED7:	.byte	$00,$01,$02,$00,$03,$00,$00,$00
LAEDF:	.addr	LAED7
LAEE1:  .byte	$00
LAEE2:  .byte	$00
LAEE3:  .byte	$00

; ----------------------------------------------------------------------------
sub_AEE4:
	prolog
	lda	CONSOL
	eor     #$FF                            ; AEEA 49 FF                    I.
	sta     off_AE
	and8i	LAEE2, off_AE, $07
	lda     LAEE3                           ; AEF5 AD E3 AE                 ...
	eor     #$01                            ; AEF8 49 01                    I.
	lbne	LAF08
	lda     LAEE2                           ; AEFF AD E2 AE                 ...
	lbeq	LAF08
	rts                                     ; AF07 60                       `

; ----------------------------------------------------------------------------
LAF08:	yldi	LAEE3, $00
	add16m8	off_AE, LAEDF, LAEE2
	lda     (off_AE),y
	sta     LAEE1                           ; AF1F 8D E1 AE                 ...
	lda     LAEE1                           ; AF22 AD E1 AE                 ...
	lbeq	LAF35
	lda     LAEE1                           ; AF2A AD E1 AE                 ...
	jsr     cmd_2a
	yldi	LAEE3, $01
LAF35:  rts                                     ; AF35 60                       `

