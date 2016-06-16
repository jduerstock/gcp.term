
; ----------------------------------------------------------------------------
L7F8E:  .byte	$00
L7F8F:  .byte	$00
L7F90:  .byte	$00
L7F91:  .byte	$00
L7F92:  .byte	$00

; ----------------------------------------------------------------------------
sub_7F93:
	prolog
	stxa	L7F8E
	ldy     #$00                            ; 7F9C A0 00                    ..
	sty     L7F90                           ; 7F9E 8C 90 7F                 ...
L7FA1:  lda     #$07                            ; 7FA1 A9 07                    ..
	cmp     L7F90                           ; 7FA3 CD 90 7F                 ...
	lbcc	L7FE2
	sec                                     ; 7FAB 38                       8
	lda     #$07                            ; 7FAC A9 07                    ..
	sbc     L7F90                           ; 7FAE ED 90 7F                 ...
	sta     L7F91                           ; 7FB1 8D 91 7F                 ...
	ldy     L7F8F                           ; 7FB4 AC 8F 7F                 ...
	ldx     L7F8E                           ; 7FB7 AE 8E 7F                 ...
	lda     L7F91                           ; 7FBA AD 91 7F                 ...
	jsr     sub_7E24
	lda     $A0                             ; 7FC0 A5 A0                    ..
	sta     L7F92                           ; 7FC2 8D 92 7F                 ...
	lda     L7F92                           ; 7FC5 AD 92 7F                 ...
	eor     #$01                            ; 7FC8 49 01                    I.
	lbne	L7FDC
	clc                                     ; 7FCF 18                       .
	lda     #$05                            ; 7FD0 A9 05                    ..
	adc     L7F91                           ; 7FD2 6D 91 7F                 m..
	sta     $A0                             ; 7FD5 85 A0                    ..
	proc8	cmd_2a, $A0
L7FDC:  inc     L7F90                           ; 7FDC EE 90 7F                 ...
	jmp     L7FA1                           ; 7FDF 4C A1 7F                 L..

; ----------------------------------------------------------------------------
L7FE2:  lda     L7F92                           ; 7FE2 AD 92 7F                 ...
	sta     $A0                             ; 7FE5 85 A0                    ..
	rts                                     ; 7FE7 60                       `

