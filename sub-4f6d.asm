
; ----------------------------------------------------------------------------
L4F6A:	.byte	$00
L4F6B:	.byte	$00,$2E

; ----------------------------------------------------------------------------
delay:  
	prolog
	sta     L4F6A                           ; 4F70 8D 6A 4F                 .jO
	jsr     read_clock
;
	clc                                     ; 4F76 18                       .
	lda     L4F6A                           ; 4F77 AD 6A 4F                 .jO
	adc     $A0                             ; 4F7A 65 A0                    e.
	sta     L4F6B                           ; 4F7C 8D 6B 4F                 .kO
	lda     #$00                            ; 4F7F A9 00                    ..
	adc     $A0+1
	sta     L4F6B+1
;
L4F86:  jsr     read_clock
	lda     $A0                             ; 4F89 A5 A0                    ..
	cmp     L4F6B                           ; 4F8B CD 6B 4F                 .kO
	lda     $A0+1
	sbc     L4F6B+1
	lbcs	L4F9B
L4F98:  jmp     L4F86                           ; 4F98 4C 86 4F                 L.O

; ----------------------------------------------------------------------------
L4F9B:  rts                                     ; 4F9B 60                       `

; --	void delay(uint8_t a)
; --	{
; --		uint16_t l4f6b = read_clock() + a;
; --
; --		while (read_clock() < l4f6b) {};
; --	}

