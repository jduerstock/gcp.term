
; ----------------------------------------------------------------------------
L5E5B:  .byte	$00
L5E5C:  .byte	$00
L5E5D:  .byte	$00

sub_5E5E:  
	prolog
	stxa	L5E5B
	yldi	L4AA4, $00
	mv	L5E5D, L4653
L5E72:  lda     #$00                            ; 5E72 A9 00                    ..
	cmp     L4653                           ; 5E74 CD 53 46                 .SF
	lbcs	L5E82
	jsr     cmd_d1
	jmp     L5E72                           ; 5E7F 4C 72 5E                 Lr^

; ----------------------------------------------------------------------------
L5E82:	mv	L464D, L5E5B
	jmp     L5E8F                           ; 5E88 4C 8F 5E                 L.^

; ----------------------------------------------------------------------------
L5E8B:	.byte	$03,"CBB"

; ----------------------------------------------------------------------------
L5E8F:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, L5E5B
	lda     L5E5C                           ; 5E9C AD 5C 5E                 .\^
	lsr     a                               ; 5E9F 4A                       J
	lsr     a                               ; 5EA0 4A                       J
	lsr     a                               ; 5EA1 4A                       J
	lsr     a                               ; 5EA2 4A                       J
	sta     $A6                             ; 5EA3 85 A6                    ..
	ldi	$A7, $00
	ldy     #$2A                            ; 5EA9 A0 2A                    .*
	ldxai	L5E8B
	jsr     sub_55A0
L5EB2:  lda     L4653                           ; 5EB2 AD 53 46                 .SF
	cmp     L5E5D                           ; 5EB5 CD 5D 5E                 .]^
	lbcs	L5EC3
	jsr     cmd_d0
	jmp     L5EB2                           ; 5EC0 4C B2 5E                 L.^

; ----------------------------------------------------------------------------
L5EC3:  rts                                     ; 5EC3 60                       `

