
; ----------------------------------------------------------------------------
cmd_lv:						; "v" "BB"
	clc                                     ; A823 18                       .
	adc     LB118,x                         ; A824 7D 18 B1                 }..
	sta     LB118,x                         ; A827 9D 18 B1                 ...
	rts                                     ; A82A 60                       `

