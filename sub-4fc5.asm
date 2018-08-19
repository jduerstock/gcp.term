
; ----------------------------------------------------------------------------
L4FBB:	.byte	$53                             ; 4FBB 53                       S
L4FBC:	.byte	$20                             ; 4FBC 20                        
L4FBD:	.byte	$69                             ; 4FBD 69                       i
L4FBE:  .byte	$00
L4FBF:  .byte	$00
L4FC0:	.byte	$74                             ; 4FC0 74                       t
L4FC1:	.byte	$68
L4FC2:	.byte	$65                             ; 4FC2 65                       e
L4FC3:	.byte	$20                             ; 4FC3 20                        
	.byte   $72                             ; 4FC4 72                       r

sub_4FC5:  
	prolog
	jsr     modem_status
	ldi	L4FBD, $00
	mv	L4FBC, $A0
	test16	L4FBC
	lbne	L4FE5
	ldi	$A0, $00
	rts                                     ; 4FE4 60                       `

; ----------------------------------------------------------------------------
L4FE5:	func8_8i GetD, L4FBB, $02
	lda     L4FBE                           ; 4FEF AD BE 4F                 ..O
	eor     #$01                            ; 4FF2 49 01                    I.
	lbne	L5015
	lda     L4FBB                           ; 4FF9 AD BB 4F                 ..O
	eor     #$0A                            ; 4FFC 49 0A                    I.
	lbne	L5010
	ldy     #$00                            ; 5003 A0 00                    ..
	sty     L4FBE                           ; 5005 8C BE 4F                 ..O
	sty     L464F                           ; 5008 8C 4F 46                 .OF
	ldi	$A0, $04
	rts                                     ; 500F 60                       `

; ----------------------------------------------------------------------------
L5010:	ldi	$A0, $00
	rts                                     ; 5014 60                       `

; ----------------------------------------------------------------------------
L5015:  lda     L464F                           ; 5015 AD 4F 46                 .OF
	lbne	L5085
	and8i	L4FC0, L4FBB, $7F
	lda     L4FC0                           ; 5025 AD C0 4F                 ..O
	cmp     #$05                            ; 5028 C9 05                    ..
	bcc     L5036                           ; 502A 90 0A                    ..
	lda     #$5F                            ; 502C A9 5F                    ._
	cmp     L4FC0                           ; 502E CD C0 4F                 ..O
	lbcs	L5040
L5036:	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 503F 60                       `

; ----------------------------------------------------------------------------
L5040:	mv	LB1C6, L4FBB
	rdldi	L4FC2, LB1C6
	lda     L4FBB                           ; 5050 AD BB 4F                 ..O
	sta     L4FC1                           ; 5053 8D C1 4F                 ..O
	ldy     #$01                            ; 5056 A0 01                    ..
	sty     L464F                           ; 5058 8C 4F 46                 .OF
	sty     L4FBF                           ; 505B 8C BF 4F                 ..O
	jsr     modem_status
	ldi	L4FBC+1, $00
	mv	L4FBC, $A0
	test16	L4FBC
	lbne	L507B
	ldi	$A0, $00
	rts                                     ; 507A 60                       `

; ----------------------------------------------------------------------------
L507B:	func8_8i GetD, L4FBB, $02
L5085:  lda     L4FBF                           ; 5085 AD BF 4F                 ..O
	eor     #$01                            ; 5088 49 01                    I.
	lbne	L50EF
	lda     L4FBB                           ; 508F AD BB 4F                 ..O
	eor     LB1C6                           ; 5092 4D C6 B1                 M..
	lbeq	L50A4
	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 50A3 60                       `

; ----------------------------------------------------------------------------
L50A4:	add16m8 off_AE, L4FC2, L4FBF
	stp8	L4FBB
	add8m	L4FC1, L4FC1, L4FBB
	inc     L4FBF                           ; 50C5 EE BF 4F                 ..O
	jsr     modem_status
	ldi	L4FBD, $00
	mv	L4FBC, $A0
	test16	L4FBC
	lbne	L50E5
	ldi	$A0, $00
	rts                                     ; 50E4 60                       `

; ----------------------------------------------------------------------------
L50E5:	func8_8i GetD, L4FBB, $02
L50EF:	add16m8	off_AE, L4FC2, L4FBF
	stp8	L4FBB
	add8m	L4FC1, L4FC1, L4FBB
	inc     L4FBF                           ; 5110 EE BF 4F                 ..O
	lda     L4FBB                           ; 5113 AD BB 4F                 ..O
	eor     #$0A                            ; 5116 49 0A                    I.
	lbne	L514C
	add8i	off_AE, L4FC0, $01
	lda     L4FBF                           ; 5125 AD BF 4F                 ..O
	eor     $AE                             ; 5128 45 AE                    E.
	lbne	L514C
	sty     L464F                           ; 512F 8C 4F 46                 .OF
	sty     L4FBF                           ; 5132 8C BF 4F                 ..O
	lda     L4FC1                           ; 5135 AD C1 4F                 ..O
	eor     #$0A                            ; 5138 49 0A                    I.
	lbne	L5147
	ldi	$A0, $01
	rts                                     ; 5143 60                       `

; ----------------------------------------------------------------------------
	jmp     L514C                           ; 5144 4C 4C 51                 LLQ

; ----------------------------------------------------------------------------
L5147:	ldi	$A0, $04
	rts                                     ; 514B 60                       `

; ----------------------------------------------------------------------------
L514C:  lda     L4FC0                           ; 514C AD C0 4F                 ..O
	cmp     L4FBF                           ; 514F CD BF 4F                 ..O
	lbcs	L5161
	yldi	L4FBE, $01
	ldi	$A0, $00
	rts                                     ; 5160 60                       `

; ----------------------------------------------------------------------------
L5161:  jsr     modem_status
	lda     #$00                            ; 5164 A9 00                    ..
	sta     L4FBD                           ; 5166 8D BD 4F                 ..O
	lda     $A0                             ; 5169 A5 A0                    ..
	sta     L4FBC                           ; 516B 8D BC 4F                 ..O
	lda     #$00                            ; 516E A9 00                    ..
	cmp     L4FBC                           ; 5170 CD BC 4F                 ..O
	lda	#$00
	sbc     L4FBD                           ; 5175 ED BD 4F                 ..O
	lbcs	L5187
	lda     #$02                            ; 517D A9 02                    ..
	jsr     GetD
	lda     $A0                             ; 5182 A5 A0                    ..
	sta     L4FBB                           ; 5184 8D BB 4F                 ..O
L5187:  test16	L4FBC
	lbne	L50EF
	ldi	$A0, $00
	rts                                     ; 5196 60                       `

