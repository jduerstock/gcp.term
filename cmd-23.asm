
; ----------------------------------------------------------------------------
cmd_23:
;--	void cmd_23(uint8_t a, uint8_t x, uint8_t y)
;--	{
	stx     $A0                             ; A959 86 A0                    ..
	tax                                     ; A95B AA                       .
	lda     #$00                            ; A95C A9 00                    ..
	sta     $B800,x                         ; A95E 9D 00 B8                 ...
;--		b800[a] = 0x00;
	lda     $A0                             ; A961 A5 A0                    ..
	sta     LB14A,x                         ; A963 9D 4A B1                 .J.
;--		b14a[x] = x;
	tya                                     ; A966 98                       .
	sta     $BC00,x                         ; A967 9D 00 BC                 ...
;--		bc00[x] = y;
	rts                                     ; A96A 60                       `
;--	}

