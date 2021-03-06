
.ifndef MONOLITH
	.include "action.inc"

	.import L4650
	.import LB1C9

	.import sub_4FC5
	.import sub_5388
	.import sub_5394
	.import sub_5461
.endif

; ----------------------------------------------------------------------------
L54FC:	.byte	$44                             ; 54FC 44                       D
L54FD:	.byte	$6F                             ; 54FD 6F                       o
	.byte   $77                             ; 54FE 77                       w

; ----------------------------------------------------------------------------
sub_54FF:
;--	uint8_t sub_54ff()  
	prolog
;--	{
	lda     L4650                           ; 5502 AD 50 46                 .PF
	lbeq	L552F
	jsr     sub_5394
	lda     $A0                             ; 550D A5 A0                    ..
	sta     L54FC                           ; 550F 8D FC 54                 ..T
	lda     L54FC                           ; 5512 AD FC 54                 ..T
	eor     #$01                            ; 5515 49 01                    I.
	lbne	L552A
	jsr     sub_5461
	mv	L54FC, $A0
	mv	$A0, L54FC
	rts                                     ; 5529 60                       `

; ----------------------------------------------------------------------------
L552A:  ldi	$A0, $00
	rts                                     ; 552E 60                       `

; ----------------------------------------------------------------------------
L552F:  jsr     sub_4FC5
	mv	L54FC, $A0
	lda     L54FC                           ; 5537 AD FC 54                 ..T
	eor     #$04                            ; 553A 49 04                    I.
	lbne	L5547
	jsr     sub_5388
	jmp     L5579                           ; 5544 4C 79 55                 LyU

; ----------------------------------------------------------------------------
L5547:  lda     L54FC                           ; 5547 AD FC 54                 ..T
	eor     #$01                            ; 554A 49 01                    I.
	lbne	L5579
	mv	L54FD, LB1C9
	lda     L54FD                           ; 5557 AD FD 54                 ..T
	eor     #$06                            ; 555A 49 06                    I.
	lbeq	L5579
	lda     L54FD                           ; 5561 AD FD 54                 ..T
	eor     #$15                            ; 5564 49 15                    I.
	lbeq	L5579
	jsr     sub_5461
	mv	L54FC, $A0
	mv	$A0, L54FC
	rts                                     ; 5578 60                       `

; ----------------------------------------------------------------------------
L5579:  ldi	$A0, 00
	rts                                     ; 557D 60                       `

