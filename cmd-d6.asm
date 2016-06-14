
; ----------------------------------------------------------------------------
LA6D8:  .byte	$00
LA6D9:  .byte	$00
LA6DA:  .byte	$00
LA6DB:  .byte	$00
LA6DC:  .byte	$00,$00
LA6DE:  .byte	$00
LA6DF:  .byte	$00
LA6E0:  .byte	$00
LA6E1:	.byte   $04,"done"
LA6E6:	.addr	LA6E1

cmd_d6:
	stack_prolog LA6D8, $03
	lda     LA6D8                           ; A6F1 AD D8 A6                 ...
	jsr     sub_65B0
	rdmv	LA6DC, $A0
	test16	LA6DC
	lbne	LA70D
	rts                                     ; A70C 60                       `

; ----------------------------------------------------------------------------
LA70D:	add16i	off_AE, LA6DC, $0001
	ldp8	LA6DE
	jsr     cmd_d0
	lda     LA6D9                           ; A726 AD D9 A6                 ...
	jsr     sub_A28D
	lda     #$06                            ; A72C A9 06                    ..
	sta     $A3                             ; A72E 85 A3                    ..
	lda     #$00                            ; A730 A9 00                    ..
	sta     $A4                             ; A732 85 A4                    ..
	rdmv	$A5, LA6DA
	ldy     #$03                            ; A73E A0 03                    ..
	ldx     #$00                            ; A740 A2 00                    ..
	lda     LA6D9                           ; A742 AD D9 A6                 ...
	jsr     XIO
LA748:  ldx     LA6D9                           ; A748 AE D9 A6                 ...
	lda     L05C0,x                         ; A74B BD C0 05                 ...
	lbne	LA7B7
	lda     #$5C                            ; A753 A9 5C                    .\
	sta     $A3                             ; A755 85 A3                    ..
	ldy     #>LB224
	ldx     #<LB224
	lda     LA6D9                           ; A75B AD D9 A6                 ...
	jsr     sub_458F
	lda     LB224                           ; A761 AD 24 B2                 .$.
	sta     LA6E0                           ; A764 8D E0 A6                 ...
	ldy     #$01                            ; A767 A0 01                    ..
	sty     LA6DF                           ; A769 8C DF A6                 ...
	lda     LA6E0                           ; A76C AD E0 A6                 ...
	sta     LA77D                           ; A76F 8D 7D A7                 .}.
LA772:  lda     LA77D                           ; A772 AD 7D A7                 .}.
	cmp     LA6DF                           ; A775 CD DF A6                 ...
	bcs     LA77E                           ; A778 B0 04                    ..
	jmp     LA799                           ; A77A 4C 99 A7                 L..

; ----------------------------------------------------------------------------
LA77D:  .byte	$00

; ----------------------------------------------------------------------------
LA77E:  ldx     LA6DF                           ; A77E AE DF A6                 ...
	lda     LB224,x                         ; A781 BD 24 B2                 .$.
	sta     $A0                             ; A784 85 A0                    ..
	lda     $A0                             ; A786 A5 A0                    ..
	jsr     sub_4B7B
	lda     $A0                             ; A78B A5 A0                    ..
	ldx     LA6DF                           ; A78D AE DF A6                 ...
	sta     LB224,x                         ; A790 9D 24 B2                 .$.
	inc     LA6DF                           ; A793 EE DF A6                 ...
	jmp     LA772                           ; A796 4C 72 A7                 Lr.

; ----------------------------------------------------------------------------
LA799:  lda     LA6E0                           ; A799 AD E0 A6                 ...
	sta     $A3                             ; A79C 85 A3                    ..
	rdldi	$A4, $B224
	ldy     LA6DE                           ; A7A6 AC DE A6                 ...
	ldx     #$00                            ; A7A9 A2 00                    ..
	lda     LA6D8                           ; A7AB AD D8 A6                 ...
	jsr     cmd_ud
	jsr     cmd_26
	jmp     LA748                           ; A7B4 4C 48 A7                 LH.

; ----------------------------------------------------------------------------
LA7B7:  lda     LA6D9                           ; A7B7 AD D9 A6                 ...
	jsr     sub_A28D
	jsr     cmd_d1
	rts                                     ; A7C0 60                       `

