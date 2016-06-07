
; ----------------------------------------------------------------------------
LAE7E:	.byte	$00
LAE7F:	.byte	$00
LAE80:	.byte	$00

; ----------------------------------------------------------------------------
sub_AE81:
	prolog
	ldy     #$00                            ; AE84 A0 00                    ..
	sty     L4764                           ; AE86 8C 64 47                 .dG
	sty     LAE7E                           ; AE89 8C 7E AE                 .~.
LAE8C:  lda     #$1F                            ; AE8C A9 1F                    ..
	cmp     LAE7E                           ; AE8E CD 7E AE                 .~.
	lbcc	LAED6
	ldx     LAE7E                           ; AE96 AE 7E AE                 .~.
	lda     $B14A,x                         ; AE99 BD 4A B1                 .J.
	sta     LAE7F                           ; AE9C 8D 7F AE                 ...
	ldx     LAE7E                           ; AE9F AE 7E AE                 .~.
	lda     $B800,x                         ; AEA2 BD 00 B8                 ...
	eor     #$01                            ; AEA5 49 01                    I.
	lbne	LAED0
	lda     LAE7F                           ; AEAC AD 7F AE                 ...
	cmp     #$80                            ; AEAF C9 80                    ..
	lbcs	LAED0
	ldx     LAE7E                           ; AEB6 AE 7E AE                 .~.
	lda     $BC00,x                         ; AEB9 BD 00 BC                 ...
	sta     LAE80                           ; AEBC 8D 80 AE                 ...
	ldxa	LAE7F
	jsr     cmd_40
	lda     #$00                            ; AEC8 A9 00                    ..
	ldx     LAE7E                           ; AECA AE 7E AE                 .~.
	sta     $B800,x                         ; AECD 9D 00 B8                 ...
LAED0:  inc     LAE7E                           ; AED0 EE 7E AE                 .~.
	jmp     LAE8C                           ; AED3 4C 8C AE                 L..

; ----------------------------------------------------------------------------
LAED6:  rts                                     ; AED6 60                       `

