
; ----------------------------------------------------------------------------
sub_4B47:
	stxa	off_AE
	ldy     #$00                            ; 4B4B A0 00                    ..
	lda     (off_AE),y
	cmp     #$24                            ; 4B4F C9 24                    .$
	bne     L4B62                           ; 4B51 D0 0F                    ..
	iny                                     ; 4B53 C8                       .
	lda     (off_AE),y
	sec                                     ; 4B56 38                       8
	sbc     #$30                            ; 4B57 E9 30                    .0
	and     #$3F                            ; 4B59 29 3F                    )?
	tax                                     ; 4B5B AA                       .
	lda     LB118,x                         ; 4B5C BD 18 B1                 ...
	sta     $A0                             ; 4B5F 85 A0                    ..
	rts                                     ; 4B61 60                       `

; ----------------------------------------------------------------------------
L4B62:  jsr     sub_4B39
	lda     $A0                             ; 4B65 A5 A0                    ..
	asl     a                               ; 4B67 0A                       .
	asl     a                               ; 4B68 0A                       .
	asl     a                               ; 4B69 0A                       .
	asl     a                               ; 4B6A 0A                       .
	sta     $A1                             ; 4B6B 85 A1                    ..
	iny                                     ; 4B6D C8                       .
	lda     (off_AE),y
	jsr     sub_4B39
	lda     $A0                             ; 4B73 A5 A0                    ..
	clc                                     ; 4B75 18                       .
	adc     $A1                             ; 4B76 65 A1                    e.
	sta     $A0                             ; 4B78 85 A0                    ..
	rts                                     ; 4B7A 60                       `

