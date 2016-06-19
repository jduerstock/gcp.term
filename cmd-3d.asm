
; ----------------------------------------------------------------------------
cmd_3d:						; "=" "BBB"
	stx     $A0                             ; A837 86 A0                    ..
	cmp     $A0                             ; A839 C5 A0                    ..
	bne     LA841                           ; A83B D0 04                    ..
	tya                                     ; A83D 98                       .
	jsr     cmd_2a
LA841:  rts                                     ; A841 60                       `

;--	void cmd_3d(uint8_t a, uint8_t x, uint8_t y)
;--	{
;--		if (a == x) cmd_2a(y);
;--	}
;--
