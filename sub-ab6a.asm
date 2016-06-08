
; ----------------------------------------------------------------------------
	.byte   $4C                             ; AA83 4C                       L
LAA84:  .addr   LAA86
LAA86:	.addr	cmd_uc,cmd_uk,cmd_ud,cmd_uf,cmd_uw,cmd_uy,cmd_ub		; "CKDFWYB"
	.addr	cmd_lx,cmd_ly,cmd_lz,cmd_uu,cmd_uv,cmd_ux,cmd_un		; "xyzUVXN"
	.addr	cmd_lc,cmd_ld,cmd_ls,cmd_lp,cmd_lm,cmd_ll,cmd_la		; "cdspmla"
	.addr	cmd_lb,cmd_lf,cmd_uj,cmd_lj,cmd_us,cmd_ut,cmd_pl		; "bfJjST+"
	.addr	cmd_ul,cmd_um,cmd_up,cmd_ur,cmd_ug,cmd_ua,cmd_uo		; "LMPRGAO"
	.addr	cmd_uz,cmd_ue,cmd_li,cmd_uh,cmd_le,cmd_ui,cmd_d0		; "ZEiHeI0"
	.addr	cmd_d1,cmd_d2,cmd_d3,cmd_d4,cmd_d5,cmd_d6,cmd_d7		; "1234567"
	.addr	cmd_d8,cmd_d9,cmd_ln,cmd_07,cmd_23,cmd_2a,cmd_3d		; "89n.#*="
	.addr	cmd_24,cmd_25,cmd_26,cmd_40,cmd_2e,cmd_3a,cmd_lu		; "$%&@.:u"
	.addr	cmd_lv,cmd_lw							; "vw"

LAB08:	.byte	$00
	.byte	$00
	.byte	$00

sub_AB0B:
	stack_prolog LAB08, $02
LAB14:	.byte	"C","K","D","F","W","Y","B"
	.byte	"x","y","z","U","V","X","N"
	.byte	"c","d","s","p","m","l","a"
	.byte	"b","f","J","j","S","T","+"
	.byte	"L","M","P","R","G","A","O"
	.byte	"Z","E","i","H","e","I","0"
	.byte	"1","2","3","4","5","6","7"
	.byte	"8","9","n",$07,"#","*","="
	.byte	"$","%","&","@",".",":","u"
	.byte	"v","w"
	.byte	$00
LAB56:	.addr	LAB14
LAB58:  .addr	$0000
LAB5A:  .addr	$0000
LAB5C:  .byte	$00
LAB5D:  .byte	$00
	.byte	$00
	.byte	$00
LAB60:	.byte	$D6,$1E,$08,$00,$00,$00,$00,$00
LAB68:  .byte	$00
LAB69:  .byte	$00

sub_AB6A:
	prolog
	rdmv	LAB58, LAA84
	rdmv	LAB5A, L48C1
	yldi	LAB69, $00
	iny                                     ; AB8A C8                       .
	sty     L4651                           ; AB8B 8C 51 46                 .QF
	mv	LAB68, LB224
LAB94:	add16m8 off_AE, LAB56, LAB69
	ldy     #$00                            ; ABA4 A0 00                    ..
	lda     ($AE),y                         ; ABA6 B1 AE                    ..
	eor     LAB68                           ; ABA8 4D 68 AB                 Mh.
	lbeq	LABEE
	add16m8 off_AE, LAB56, LAB69
	lda     ($AE),y                         ; ABC0 B1 AE                    ..
	lbne	LABE8
LABC7:  jmp     LABCD                           ; ABC7 4C CD AB                 L..

; ----------------------------------------------------------------------------
LABCA:	.byte	$02,"CC"

; ----------------------------------------------------------------------------
LABCD:	ldi	$A3, $00
	ldi	$A5, $00
	mv	$A4, LAB68
	ldy     #$3F                            ; ABDA A0 3F                    .?
	ldxai	LABCA
	jsr     sub_55A0
	ldi	$A0, $00
	rts                                     ; ABE7 60                       `

; ----------------------------------------------------------------------------
LABE8:  inc     LAB69                           ; ABE8 EE 69 AB                 .i.
	jmp     LAB94                           ; ABEB 4C 94 AB                 L..

; ----------------------------------------------------------------------------
LABEE:	shladdm8 off_AE, LAB5A, LAB69
	ldp16	LAB5C
	shladdm8 off_AE, LAB58, LAB69
	iny                                     ; AC23 C8                       .
	lda     ($AE),y                         ; AC24 B1 AE                    ..
	sta     sub_AB0B+2
	dey                                     ; AC29 88                       .
	lda     ($AE),y                         ; AC2A B1 AE                    ..
	sta     sub_AB0B+1
	dmv	off_AE, LAB5C
	iny                                     ; AC39 C8                       .
	lda     ($AE),y                         ; AC3A B1 AE                    ..
	sta     $A1                             ; AC3C 85 A1                    ..
	dey                                     ; AC3E 88                       .
	lda     ($AE),y                         ; AC3F B1 AE                    ..
	sta     $A0                             ; AC41 85 A0                    ..
	ldi	$A3, >LAB60
	ldy     #<LAB60
	ldxa	$A0
	jsr     sub_58F2                        ; AC4D 20 F2 58                  .X
	yldi	LAB69, $00
LAC55:  lda     #$07                            ; AC55 A9 07                    ..
	cmp     LAB69                           ; AC57 CD 69 AB                 .i.
	lbcc	LAC70
	ldx     LAB69                           ; AC5F AE 69 AB                 .i.
	lda     LAB60,x                         ; AC62 BD 60 AB                 .`.
	ldx     LAB69                           ; AC65 AE 69 AB                 .i.
	sta     $A0,x                           ; AC68 95 A0                    ..
	inc     LAB69                           ; AC6A EE 69 AB                 .i.
	jmp     LAC55                           ; AC6D 4C 55 AC                 LU.

; ----------------------------------------------------------------------------
LAC70:  ldy     $A2                             ; AC70 A4 A2                    ..
	ldxa	$A0
	jsr     sub_AB0B
	ldi	$A0, $01
	rts                                     ; AC7D 60                       `

