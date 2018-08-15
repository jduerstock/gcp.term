
; ----------------------------------------------------------------------------
L7B4D:	.byte	$54                             ; 7B4D 54                       T
L7B4E:	.byte	$B1                             ; 7B4E B1                       .
L7B4F:	.byte	$45                             ; 7B4F 45                       E
L7B50:	.byte	$F0                             ; 7B50 F0                       .
L7B51:	.byte	$F7                             ; 7B51 F7                       .
L7B52:	.byte	$8C,$33
L7B54:	.byte	$F0                             ; 7B54 F0                       .
L7B55:  .byte	$48
L7B56:	.byte	$38,$A0
	.byte	$03
	.byte	$B1,$45
	.byte   $E9                             ; 7B5B E9                       .
L7B5C:  .byte	$01
L7B5D:	.byte	$91
	.byte	$45
	.byte	$C8
	.byte	$B1
	.byte	$45
	.byte	$E9
L7B63:	.byte	$00
L7B64:  .byte	$91
	.byte	$45
	.byte	$C8
L7B67:  .byte	$A9

; ----------------------------------------------------------------------------
cmd_ux:						; "X" "BBB"
	stack_prolog L7B4D, $02
	sub8i	L7B4F, L7B4F, $01
	lda     L7B4D                           ; 7B7A AD 4D 7B                 .M{
	jsr     sub_65B0
	lda     $A1                             ; 7B80 A5 A1                    ..
	sta     L7B51                           ; 7B82 8D 51 7B                 .Q{
	lda     $A0                             ; 7B85 A5 A0                    ..
	sta     L7B50                           ; 7B87 8D 50 7B                 .P{
	add16i	off_AE, L7B50, $0007
	ldp16	L7B52
	test16	L7B52
	lbne	L7BB2
	rts                                     ; 7BB1 60                       `

; ----------------------------------------------------------------------------
L7BB2:	rdmv	L7B56, L7B52
	ldy     #$00                            ; 7BBE A0 00                    ..
	sty     L7B55                           ; 7BC0 8C 55 7B                 .U{
	sty     L7B54                           ; 7BC3 8C 54 7B                 .T{
	sty     L7B5C                           ; 7BC6 8C 5C 7B                 .\{
	add16i	off_AE, L7B50, $0006
	sec                                     ; 7BD8 38                       8
	lda     (off_AE),y
	sbc     #$01                            ; 7BDB E9 01                    ..
	sta     L7BEB                           ; 7BDD 8D EB 7B                 ..{
L7BE0:  lda     L7BEB                           ; 7BE0 AD EB 7B                 ..{
	cmp     L7B5C                           ; 7BE3 CD 5C 7B                 .\{
	bcs     L7BEC                           ; 7BE6 B0 04                    ..
	jmp     L7C59                           ; 7BE8 4C 59 7C                 LY|

; ----------------------------------------------------------------------------
L7BEB:	.byte	$F6                             ; 7BEB F6                       .

; ----------------------------------------------------------------------------
L7BEC:	blkmv_imi L7B63, L7B52, $0005
	lda     L7B67                           ; 7C03 AD 67 7B                 .g{
	cmp     L7B4F                           ; 7C06 CD 4F 7B                 .O{
	lbcs	L7C1F
	add16i	L7B56, L7B52, $0006
L7C1F:  lda     L7B64                           ; 7C1F AD 64 7B                 .d{
	eor     L7B4E                           ; 7C22 4D 4E 7B                 MN{
	lbne	L7C42
L7C2A:  lda     L7B67                           ; 7C2A AD 67 7B                 .g{
	eor     L7B4F                           ; 7C2D 4D 4F 7B                 MO{
	lbne	L7C36
	rts                                     ; 7C35 60                       `

; ----------------------------------------------------------------------------
L7C36:	rdmv	L7B54, L7B52
L7C42:	add16i	L7B52, L7B52, $0006
	inc     L7B5C                           ; 7C53 EE 5C 7B                 .\{
	jmp     L7BE0                           ; 7C56 4C E0 7B                 L.{

; ----------------------------------------------------------------------------
L7C59:	test16	L7B54
	lbne	L7C65
	rts                                     ; 7C64 60                       `

; ----------------------------------------------------------------------------
L7C65:	blkmv_imi L7B5D, L7B54, $0006
	add16i	$A2, L7B54, $0006
	sub16m	off_AC, L7B52, L7B54
	sub16i	$A4, off_AC, $0006
	ldy     $A2                             ; 7CA9 A4 A2                    ..
	ldxa	L7B54
	jsr     blockmove
	lda     L7B54                           ; 7CB4 AD 54 7B                 .T{
	cmp     L7B56                           ; 7CB7 CD 56 7B                 .V{
	lda     L7B55                           ; 7CBA AD 55 7B                 .U{
	sbc     L7B56+1
	lbcs	L7CD6
	sub16i	L7B56, L7B56, $0006
L7CD6:	add16i	$A0, L7B56, $0006
	mv	$A3, L7B56+1
	sub16m	off_AC, L7B52, L7B56
	sub16i	$A4, off_AC, $0006
	ldy     L7B56                           ; 7D08 AC 56 7B                 .V{
	ldxa	$A0
	jsr     sub_4EB1
	blkmv_mii L7B56, L7B5D, $0006
	add16i	off_AE, L7B56, $0004
	lda     L7B4F                           ; 7D38 AD 4F 7B                 .O{
	ldy     #$00                            ; 7D3B A0 00                    ..
	sta     (off_AE),y
	iny                                     ; 7D3F C8                       .
	sty     L4656                           ; 7D40 8C 56 46                 .VF
	rts                                     ; 7D43 60                       `

