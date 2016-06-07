
; ----------------------------------------------------------------------------
sub_ADEA:
;--	void sub_ADEA()
	prolog
;--	{
	dmv	off_AE, L466F
	stp16	DLIST
	shladdi off_AE, L466F, $01
	sub8i	off_AC, $0230, $00
	lda     $0231                           ; AE1F AD 31 02                 .1.
	sbc     #$04                            ; AE22 E9 04                    ..
	iny                                     ; AE24 C8                       .
	sta     ($AE),y                         ; AE25 91 AE                    ..
	lda     $AC                             ; AE27 A5 AC                    ..
	dey                                     ; AE29 88                       .
	sta     ($AE),y                         ; AE2A 91 AE                    ..
	rdldi	MEMTOP, L4327
	rdmv	sub_4749+1, sub_AB6A+1
	rdmv	sub_5D64+1, cmd_26+1
	jsr     sub_AD85
;--		sub_AD85();
	jsr     sub_6203
;--		sub_6203();
	jsr     sub_5D67
;--		sub_5D67();
	jsr     cmd_li
;--		cmd_li();
	sei                                     ; AE5A 78                       x
	and8i	$10, $10, $7F
	cli                                     ; AE61 58                       X
	jmp     LAE70                           ; AE62 4C 70 AE                 Lp.

; ----------------------------------------------------------------------------
LAE65:	.byte	10,"D:INIT.MAC"

; ----------------------------------------------------------------------------
LAE70:  lda     #>LAE65
	sta     $A3                             ; AE72 85 A3                    ..
	ldy     #<LAE65
	ldx     #$02                            ; AE76 A2 02                    ..
	lda     #$03                            ; AE78 A9 03                    ..
	jsr     cmd_d2
;--		cmd_d2(3,2,"D:INIT.MAC");
	rts
;--	}

