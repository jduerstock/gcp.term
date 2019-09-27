
; ----------------------------------------------------------------------------
modem_status:  
	prolog
	lda     #$02                            ; 4B0A A9 02                    ..
	jsr     cio_status
	mv	$A0, DVSTAT+1
	rts                                     ; 4B14 60                       `

