
; ----------------------------------------------------------------------------
L4F6A:	.byte	$00
L4F6B:	.byte	$00,$2E

; ----------------------------------------------------------------------------
sub_4F6D:  
	prolog
	sta     L4F6A                           ; 4F70 8D 6A 4F                 .jO
	jsr     sub_4BB8
	clc                                     ; 4F76 18                       .
	lda     L4F6A                           ; 4F77 AD 6A 4F                 .jO
	adc     $A0                             ; 4F7A 65 A0                    e.
	sta     L4F6B                           ; 4F7C 8D 6B 4F                 .kO
	lda     #$00                            ; 4F7F A9 00                    ..
	adc     $A1                             ; 4F81 65 A1                    e.
	sta     L4F6B+1
L4F86:  jsr     sub_4BB8
	lda     $A0                             ; 4F89 A5 A0                    ..
	cmp     L4F6B                           ; 4F8B CD 6B 4F                 .kO
	lda     $A1                             ; 4F8E A5 A1                    ..
	sbc     L4F6B+1
	lbcs	L4F9B
L4F98:  jmp     L4F86                           ; 4F98 4C 86 4F                 L.O

; ----------------------------------------------------------------------------
L4F9B:  rts                                     ; 4F9B 60                       `

