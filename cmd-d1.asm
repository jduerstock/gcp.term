
; ----------------------------------------------------------------------------
cmd_d1:						; "1" ""
	prolog
	lda     L4653                           ; 5E33 AD 53 46                 .SF
	lbne	L5E3C
	rts                                     ; 5E3B 60                       `

; ----------------------------------------------------------------------------
L5E3C:	ifm8eqi	L4653, $01, L5E51
	jsr     sub_5D67
	yldi	L4656, $01
	jmp     L5E5A                           ; 5E4E 4C 5A 5E                 LZ^

; ----------------------------------------------------------------------------
L5E51:	sub8i	L4653, L4653, $01
L5E5A:  rts                                     ; 5E5A 60                       `

;--	void cmd_d1()
;--	{
;--		if (L4653 == 0) return;
;--		if (L4653 == 1) {
;--			sub_5D67();
;--			L4656 = 1;
;--		} else {
;--			L4653 = L4653 - 1;
;--		}
;--	}
;--
