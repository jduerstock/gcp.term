
; ----------------------------------------------------------------------------
sub_A9A3:
	prolog
	lda     $D8                             ; A9A6 A5 D8                    ..
	eor     #$01                            ; A9A8 49 01                    I.
	lbne	LA9B8
LA9AF:	proc8i	cmd_2a, $12
	yldi	$D8, $00
LA9B8:  lda     $D9                             ; A9B8 A5 D9                    ..
	eor     #$01                            ; A9BA 49 01                    I.
	lbne	LA9CA
	proc8i	cmd_2a, $13
	yldi	$D9, $00
LA9CA:  lda     $DA                             ; A9CA A5 DA                    ..
	eor     #$01                            ; A9CC 49 01                    I.
	lbne	LA9DC
	proc8i	cmd_2a, $14
	yldi	$DA, $00
LA9DC:  rts                                     ; A9DC 60                       `

