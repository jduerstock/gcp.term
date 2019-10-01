
.ifndef MONOLITH
	.include "action.inc"

	.import ATRACT
	.import CH
	.import INVFLG
	.import SHFLOK

	.import	L05C0

	.import	L464D
	.import	L4652
	.import L46E6
	.import L46E9
	.import	L474D
	.import	L474E
	.import	L4764
	.import	LB223

	.import cmd_26
	.import cmd_2a
	.import cmd_d1
	.import cmd_ui

	.import sub_43BA
	.import sub_458F
	.import sub_54FF
	.import sub_5E5E
	.import sub_768A
	.import sub_840C
	.import sub_907D
	.import sub_9CD0
	.import sub_9DCB
	.import sub_A027
	.import sub_A28D
	.import sub_A9A3
	.import sub_AB6A
	.import sub_ADEA
	.import sub_AE81
	.import sub_AEE4
.endif

; ----------------------------------------------------------------------------
LAF36:  .byte	$00
LAF37:  .byte	$00
LAF38:  .byte	$00
LAF39:  .byte	$00

; ----------------------------------------------------------------------------
sub_AF3A:
;--	void sub_AF3A()
	prolog
;--	{
	rdmv	Error+1, sub_5E5E+1
;--		ptr_43BA = &sub_5E5E;
	jsr     sub_ADEA
;--		sub_ADEA();
LAF4C:  lda     #$01                            ; AF4C A9 01                    ..
	eor     #$01                            ; AF4E 49 01                    I.
	lbne	LB0BE
;--		while (1 == 1) {
	yldi	ATRACT, $00
;--			atract = 0;
	ifm8nei	L4652, $02, LAFA2
LAF63:	ifm8nei	L4652, $02, LAF94
	ldi	$A3, $5C
	ldy     #>LB223
	ldx     #<LB223
	lda     L4652                           ; AF75 AD 52 46                 .RF
	jsr     InputMD
	ldx     L4652                           ; AF7B AE 52 46                 .RF
	lda     L05C0,x                         ; AF7E BD C0 05                 ...
	bne     LAF8B                           ; AF81 D0 08                    ..
	lda     L464D                           ; AF83 AD 4D 46                 .MF
	lbeq	LAF8E
LAF8B:	jmp     LAF94                           ; AF8B 4C 94 AF                 L..

; ----------------------------------------------------------------------------
LAF8E:  jsr     sub_AB6A
	jmp     LAF63                           ; AF91 4C 63 AF                 Lc.

; ----------------------------------------------------------------------------
;--		}
LAF94:	proc8	sub_A28D, L4652
	ldi	L4652, $02
	jsr     cmd_d1
LAFA2:	ifm8eqi	CH, $FF, LAFCB
	ifm8nem L474D, L474E, LAFCB
	ldx     L474E                           ; AFB7 AE 4E 47                 .NG
	lda     $B138,x                         ; AFBA BD 38 B1                 .8.
	sta	CH
	inc     L474E                           ; AFC0 EE 4E 47                 .NG
	lda     L474E                           ; AFC3 AD 4E 47                 .NG
	and     #$0F                            ; AFC6 29 0F                    ).
	sta     L474E                           ; AFC8 8D 4E 47                 .NG
LAFCB:	proc8	sub_907D, CH
	ifm8eqi	$A0, $01, LAFE2
	ldi	CH, $FF
	jmp     LB01A                           ; AFDF 4C 1A B0                 L..

; ----------------------------------------------------------------------------
LAFE2:	and8i	SHFLOK, SHFLOK, $40
	yldi	INVFLG, $00
	lda	CH
	and     #$3F                            ; AFF2 29 3F                    )?
	sta     LAF39                           ; AFF4 8D 39 AF                 .9.
	ifm8eqi	LAF39, $3C, LB00C
	lda	CH
	and     #$40                            ; B004 29 40                    )@
	sta     SHFLOK
	jmp     LB012                           ; B009 4C 12 B0                 L..

; ----------------------------------------------------------------------------
LB00C:	proc8	cmd_ui, CH
LB012:  ldi	CH, $FF
	jmp     LAFA2                           ; B017 4C A2 AF                 L..

; ----------------------------------------------------------------------------
LB01A:  jsr     sub_AEE4
	ldi	$A3, >LAF38
	ldy     #<LAF38
	ldxai	LAF37
	jsr     sub_9CD0
	ifm8eqi	$A0, $01, LB07D
	ifm8eqi L46E6, $03, LB049
	ldxa	LAF37
	jsr     sub_840C
	jmp     LB07D                           ; B046 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB049:	ifm8eqi L46E6, $02, LB062
	ldy     LAF38                           ; B053 AC 38 AF                 .8.
	ldx     LAF37                           ; B056 AE 37 AF                 .7.
	lda     L46E9                           ; B059 AD E9 46                 ..F
	jsr     sub_768A
	jmp     LB07D                           ; B05F 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB062:	ifm8eqi	L46E6, $04, LB07D
	mv	$A3, LAF38
	ldy     LAF37                           ; B071 AC 37 AF                 .7.
	ldxa	L46E9
	jsr     sub_A027
LB07D:  func8	sub_54FF, LAF36
	ifm8eqi	LAF36, $01, LB092
	jsr     sub_AB6A
LB092:  jsr     sub_9DCB
	ifm8eqi	$A0, $01, LB0A3
	proc8i	cmd_2a, $00
LB0A3:  jsr     sub_A9A3
	ifm8eqi	L4764, $01, LB0B3
	jsr     sub_AE81
LB0B3:  jsr     cmd_26
	yldi	L464D, $00
	jmp     LAF4C                           ; B0BB 4C 4C AF                 LL.

;--		}
; ----------------------------------------------------------------------------
LB0BE:  rts                                     ; B0BE 60                       `
;--	}

; ----------------------------------------------------------------------------
	rts                                     ; B0BF 60                       `

; ----------------------------------------------------------------------------

