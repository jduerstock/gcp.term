
; ----------------------------------------------------------------------------
L8CF9:  .byte	$00
L8CFA:  .byte	$00
L8CFB:  .byte	$00
L8CFC:  .byte	$00
L8CFD:  .word	$0000
L8CFF:  .word	$0000

; ----------------------------------------------------------------------------
cmd_26:						; "&" ""
	prolog
	lda     L4658                           ; 8D04 AD 58 46                 .XF
	lbne	L8D0D
	rts                                     ; 8D0C 60                       `

; ----------------------------------------------------------------------------
L8D0D:  lda     L4656                           ; 8D0D AD 56 46                 .VF
	eor     #$01                            ; 8D10 49 01                    I.
	lbne	L8E06
	shladdi off_AE, L466F, $01
	ldp16	L8CFF
	add16i	off_AE, L8CFF, $03
	add16i	L8CFD, off_AE, $01
	dmv	off_AE, L8CFD
	ld2p16	L466D
	dldi	$A3, $0003
	ldy     #$C0                            ; 8D73 A0 C0                    ..
	ldxa	L466D
	jsr     memset
	yldi	L8CF9, $01
	mv	L8D94, L4673
L8D89:  lda     L8D94                           ; 8D89 AD 94 8D                 ...
	cmp     L8CF9                           ; 8D8C CD F9 8C                 ...
	bcs     L8D95                           ; 8D8F B0 04                    ..
	jmp     L8DFE                           ; 8D91 4C FE 8D                 L..

; ----------------------------------------------------------------------------
L8D94:  .byte	$00

; ----------------------------------------------------------------------------
L8D95:	sub8i	off_AE, L8CF9, $01
	ldx     $AE                             ; 8D9D A6 AE                    ..
	lda     L4659,x                         ; 8D9F BD 59 46                 .YF
	sta     L8CFA                           ; 8DA2 8D FA 8C                 ...
	func16_8 sub_7035, L8CFB, L8CFA
	add16i	off_AE, L8CFB, $000C
	ldy     #$00                            ; 8DC4 A0 00                    ..
	lda     ($AE),y                         ; 8DC6 B1 AE                    ..
	eor     #$FF                            ; 8DC8 49 FF                    I.
	lbeq	L8DF8
	add16i	off_AE, L8CFB, $000A
	lda     ($AE),y                         ; 8DDE B1 AE                    ..
	eor     #$01                            ; 8DE0 49 01                    I.
	lbne	L8DF8
	test16	L8CFB
	lbeq	L8DF8
	lda     L8CFA                           ; 8DF2 AD FA 8C                 ...
	jsr     sub_89AE
L8DF8:  inc     L8CF9                           ; 8DF8 EE F9 8C                 ...
	jmp     L8D89                           ; 8DFB 4C 89 8D                 L..

; ----------------------------------------------------------------------------
L8DFE:	yldi	L4656, $00
	jsr     sub_63DD
L8E06:  lda     L4657                           ; 8E06 AD 57 46                 .WF
	eor     #$01                            ; 8E09 49 01                    I.
	lbne	L8E1E
	ldxa	L4762
	jsr     sub_8047
	yldi	L4657, $00
L8E1E:  rts                                     ; 8E1E 60                       `

;--	void cmd_26();
;--	{
;--		uint16_t L8CFF;
;--	
;--		if (L4658 == 0) return;
;--		if (L4656 == 1) {
;--			L8CFF = r16(L466F + 2);
;--			L8CFD = L8CFF + 3 + 1;
;--			L466D = r16(L8CFD);
;--			memset(L466D, 0xc0, 3);
;--			L8CF9 = 1;
;--			L8D94 = L4673;
;--			while (L8CF9 < L8D94) {
;--				L8CFA = L4659[L8CF9-1];
;--				L8CFB = sub_7035(L8CFA);
;--				if ((r8(L8CFB+12) == 0xff) && (r8(L8CFB+10) == 0x01) && (L8CFB != 0) {
;--					sub_89AE(L8CFA);
;--				}
;--				L8CF9++;
;--			}
;--			L4646 = 0;
;--			sub_63DD();
;--		}
;--		if (L4657 == 1) {
;--			sub_8047(L4762);
;--			L4657 = 0;
;--		}
;--	}
;--
