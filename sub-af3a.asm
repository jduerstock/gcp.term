
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
	rdmv	sub_43BA+1, sub_5E5E+1
;--		ptr_43BA = &sub_5E5E;
	jsr     sub_ADEA
;--		sub_ADEA();
LAF4C:  lda     #$01                            ; AF4C A9 01                    ..
	eor     #$01                            ; AF4E 49 01                    I.
	lbne	LB0BE
;--		while (1 == 1) {
	yldi	ATRACT, $00
;--			atract = 0;
	lda     L4652                           ; AF59 AD 52 46                 .RF
	eor     #$02                            ; AF5C 49 02                    I.
	lbeq	LAFA2
LAF63:	lda     L4652                           ; AF63 AD 52 46                 .RF
	eor     #$02                            ; AF66 49 02                    I.
	lbeq	LAF94
	ldi	$A3, $5C
	ldy     #>LB223
	ldx     #<LB223
	lda     L4652                           ; AF75 AD 52 46                 .RF
	jsr     sub_458F
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
LAF94:  lda     L4652                           ; AF94 AD 52 46                 .RF
	jsr     sub_A28D
	ldi	L4652, $02
	jsr     cmd_d1
LAFA2:  lda	CH
	eor     #$FF                            ; AFA5 49 FF                    I.
	beq     LAFAC                           ; AFA7 F0 03                    ..
	jmp     LAFCB                           ; AFA9 4C CB AF                 L..

; ----------------------------------------------------------------------------
LAFAC:  lda     L474D                           ; AFAC AD 4D 47                 .MG
	eor     L474E                           ; AFAF 4D 4E 47                 MNG
	lbeq	LAFCB
	ldx     L474E                           ; AFB7 AE 4E 47                 .NG
	lda     $B138,x                         ; AFBA BD 38 B1                 .8.
	sta	CH
	inc     L474E                           ; AFC0 EE 4E 47                 .NG
	lda     L474E                           ; AFC3 AD 4E 47                 .NG
	and     #$0F                            ; AFC6 29 0F                    ).
	sta     L474E                           ; AFC8 8D 4E 47                 .NG
LAFCB:  lda	CH
	jsr     sub_907D
	lda     $A0                             ; AFD1 A5 A0                    ..
	eor     #$01                            ; AFD3 49 01                    I.
	lbne	LAFE2
	ldi	CH, $FF
	jmp     LB01A                           ; AFDF 4C 1A B0                 L..

; ----------------------------------------------------------------------------
LAFE2:	and8i	SHFLOK, SHFLOK, $40
	yldi	INVFLG, $00
	lda	CH
	and     #$3F                            ; AFF2 29 3F                    )?
	sta     LAF39                           ; AFF4 8D 39 AF                 .9.
	lda     LAF39                           ; AFF7 AD 39 AF                 .9.
	eor     #$3C                            ; AFFA 49 3C                    I<
	lbne	LB00C
	lda	CH
	and     #$40                            ; B004 29 40                    )@
	sta     SHFLOK
	jmp     LB012                           ; B009 4C 12 B0                 L..

; ----------------------------------------------------------------------------
LB00C:  lda	CH
	jsr     cmd_ui
LB012:  ldi	CH, $FF
	jmp     LAFA2                           ; B017 4C A2 AF                 L..

; ----------------------------------------------------------------------------
LB01A:  jsr     sub_AEE4
	ldi	$A3, >LAF38
	ldy     #<LAF38
	ldxai	$AF37
	jsr     sub_9CD0
	lda     $A0                             ; B02A A5 A0                    ..
	eor     #$01                            ; B02C 49 01                    I.
	lbne	LB07D
	lda     L46E6                           ; B033 AD E6 46                 ..F
	eor     #$03                            ; B036 49 03                    I.
	lbne	LB049
	ldxa	LAF37
	jsr     sub_840C
	jmp     LB07D                           ; B046 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB049:  lda     L46E6                           ; B049 AD E6 46                 ..F
	eor     #$02                            ; B04C 49 02                    I.
	lbne	LB062
	ldy     LAF38                           ; B053 AC 38 AF                 .8.
	ldx     LAF37                           ; B056 AE 37 AF                 .7.
	lda     L46E9                           ; B059 AD E9 46                 ..F
	jsr     sub_768A
	jmp     LB07D                           ; B05F 4C 7D B0                 L}.

; ----------------------------------------------------------------------------
LB062:  lda     L46E6                           ; B062 AD E6 46                 ..F
	eor     #$04                            ; B065 49 04                    I.
	lbne	LB07D
	mv	$A3, LAF38
	ldy     LAF37                           ; B071 AC 37 AF                 .7.
	ldxa	L46E9
	jsr     sub_A027
LB07D:  jsr     sub_54FF
	mv	LAF36, $A0
	lda     LAF36                           ; B085 AD 36 AF                 .6.
	eor     #$01                            ; B088 49 01                    I.
	lbne	LB092
	jsr     sub_AB6A
LB092:  jsr     sub_9DCB
	lda     $A0                             ; B095 A5 A0                    ..
	eor     #$01                            ; B097 49 01                    I.
	lbne	LB0A3
	lda     #$00                            ; B09E A9 00                    ..
	jsr     sub_4BA7
LB0A3:  jsr     sub_A9A3
	lda     L4764                           ; B0A6 AD 64 47                 .dG
	eor     #$01                            ; B0A9 49 01                    I.
	lbne	LB0B3
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

