
; ----------------------------------------------------------------------------
sub_A9A3:
	prolog
	ifm8eqi	$D8, $01, LA9B8
LA9AF:	proc8i	cmd_2a, $12
	yldi	$D8, $00
LA9B8:	ifm8eqi	$D9, $01, LA9CA
	proc8i	cmd_2a, $13
	yldi	$D9, $00
LA9CA:	ifm8eqi	$DA, $01, LA9DC
	proc8i	cmd_2a, $14
	yldi	$DA, $00
LA9DC:  rts                                     ; A9DC 60                       `

