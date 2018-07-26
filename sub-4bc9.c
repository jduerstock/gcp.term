//
//; ----------------------------------------------------------------------------
//sub_4BC9:  
//	sta     $A0
//	and     #$60
//	sta     $A2				; $A2 = $A0 & #$60
//	asl     a
//	sta     $A3                             ; $A3 = $A2 << 1
//	eor     $A2
//	eor     #$FF
//	and     #$40
//	lsr     a
//	sta     $A1				; $A1 = ($A3 ^ $A2 ^ $FF & #$40) >> 1
//	lda     $A0
//	and     #$9F
//	sta     $A0				; $A0 = $A0 & #$9F
//	lda     $A3
//	and     #$40
//	ora     $A0
//	ora     $A1
//	sta     $A0				; $A0 = $A3 & #$40 | $A0 | $A1
//	rts                                     ; 4BEB 60                       `
//

#include <stdio.h>

unsigned char sub_4bc9(unsigned char a)
{
	unsigned char a0, a1, a2, a3;

	a0 = a;
	a2 = a0 & 0x60;
	a3 = a2 << 1;
	a1 = ((a3 ^ a2 ^ 0xff) & 0x40) >> 1;
	a0 = a0 & 0x9f;
	a0 = (a3 & 0x40) | a0 | a1;
	return(a0);
}

int main(int argc, char **argv)
{
	int	i;
	unsigned char x;

	for(i=0;i<=255;i++) {
		x = sub_4bc9(i);
		printf("%d %d 0x%x 0x%x\n", i, x, i, x);
		}
}
