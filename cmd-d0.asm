
; ----------------------------------------------------------------------------
cmd_d0:						; "0" ""
	prolog 
	lda     L4653                           ; 5E21 AD 53 46                 .SF
	lbne	L5E2C
	jsr     sub_5CFC
L5E2C:  inc     L4653                           ; 5E2C EE 53 46                 .SF
	rts                                     ; 5E2F 60                       `

;--	void cmd_d0()
;--	{
;--		if (L4653 == 0) {
;--			sub_5CFC();
;--		}
;--		L4653++;
;--	}
;--
