
; ----------------------------------------------------------------------------
L4CEE:  .byte	$00,$0D
L4CF0:	.byte	$3B,$44
L4CF2:	.byte	$45,$53
L4CF4:	.byte	$43                             ; 4CF4 43                       C

sub_4CF5:
	stack_prolog L4CEE, $05
	yldi	L4CF4, $00
L4D03:  lda     #$01                            ; 4D03 A9 01                    ..
	cmp     L4CF4                           ; 4D05 CD F4 4C                 ..L
	lbcc	L4DCC
	add16m8 off_AE, L4CF2, L4CF4
	push16	off_AE
	add16m8	off_AE, L4CEE, L4CF4
	ldp8	$A0
	add16m8 off_AE, L4CF0, L4CF4
	lda     (off_AE),y
	sta     $A1                             ; 4D4B 85 A1                    ..
	ldxa	$A0
	jsr     sub_4983
	pull16	off_AE
	stp8	$A0
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CF2, off_AE
	push16	off_AC
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CEE, off_AE
	lda     ($AC),y                         ; 4D94 B1 AC                    ..
	sta     $A0                             ; 4D96 85 A0                    ..
	add8i	off_AE, L4CF4, $02
	add16m8	off_AC, L4CF0, off_AE
	lda     ($AC),y                         ; 4DAF B1 AC                    ..
	sta     $A1                             ; 4DB1 85 A1                    ..
	ldxa	$A0
	jsr     sub_4990
	pull16	off_AC
	lda     $A0                             ; 4DC0 A5 A0                    ..
	ldy     #$00                            ; 4DC2 A0 00                    ..
	sta     ($AC),y                         ; 4DC4 91 AC                    ..
	inc     L4CF4                           ; 4DC6 EE F4 4C                 ..L
	jmp     L4D03                           ; 4DC9 4C 03 4D                 L.M

; ----------------------------------------------------------------------------
L4DCC:  dmv	$AE, L4CF2
	ldp8	$A0
	add16i	off_AE, L4CF2, $0002
	lda     (off_AE),y
	sta     $A1                             ; 4DED 85 A1                    ..
	ldxa	$A0
	jsr     sub_496E
	lda     $A0                             ; 4DF6 A5 A0                    ..
	lbeq	L4E02
	ldi	$A0, $00
	rts                                     ; 4E01 60                       `

; ----------------------------------------------------------------------------
L4E02:	add16i	off_AE, L4CF2, $0001
	ldp8	$A0
	add16i	off_AE, L4CF2, $0003
	lda     (off_AE),y
	sta     $A1                             ; 4E28 85 A1                    ..
	ldxa	$A0
	jsr     sub_496E
	lda     $A0                             ; 4E31 A5 A0                    ..
	lbeq	L4E3D
	ldi	$A0, $00
	rts                                     ; 4E3C 60                       `

; ----------------------------------------------------------------------------
L4E3D:  ldi	$A0, $01
	rts                                     ; 4E41 60                       `

