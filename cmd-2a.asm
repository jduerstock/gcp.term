
; ----------------------------------------------------------------------------
cmd_2a:						; "*" "B"
	tax                                     ; 4BA7 AA                       .
	lda     LB14A,x                         ; 4BA8 BD 4A B1                 .J.
	cmp     #$80                            ; 4BAB C9 80                    ..
	bcs	:+
	lda     #$01                            ; 4BAF A9 01                    ..
	sta     LB800,x                         ; 4BB1 9D 00 B8                 ...
	sta     L4764                           ; 4BB4 8D 64 47                 .dG
:	rts

;--	void cmd_2a(uint8_t a)
;--	{
;--		if (LB14A[a] < 0x80) {
;--			LB800[a] = 1;
;--			L4764 = 1;
;--		}
;--	}
;--
