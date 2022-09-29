#ident "$Header: pon_buff_parity.c,v 1.2.1.1 90/07/18 14:29:29 huang Exp $"

/* This test verifies the CPU ability to detect parity errors in the lance buffer */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/vmereg.h"
#include "saio/setjmp.h"
#include "pon.h"

extern char success[], failure[], skipped[];
extern int machine_type;
extern int *nofault;

Pon_Buffer_Parity()
{
	jmp_buf parity_buf;
	register u_char byte_lane,cpu_err,cpu_parity_err;
	register u_short wait;
	register u_int *buf_ptr,junk,addr;
	register u_int *config,sr_save;

	if(machine_type != BRDTYPE_RB3125)
		return(PASS);
	pon_set_leds(PON_LANCE_BUFF_PARITY);
	pon_puts("Lance Buffer Parity Test...");

	if(GetDepend() & (PON_FAULT_MEM | PON_FAULT_LNC_BUFF)){
		pon_puts(skipped);
		return(FAIL);
	}
	SetSR((sr_save = GetSR()) & ~SR_BEV);

	buf_ptr = (u_int*)PHYS_TO_K1(LANCE_BUFFER_RB3125);
        config = (u_int*)PHYS_TO_K1(CPU_CR_M2000);
	*config |= CR_LNCRESETB | CR_CLRBUFERRB;
    	SetIMR((GetIMR() | (IMR_LNCINTB | IMR_BUFERROR)));
	flush_cache();
	cpu_err = 0;
	for(byte_lane=0; byte_lane<4; byte_lane++){
		cpu_parity_err = 0;
		nofault = &parity_buf[0];
		if(setjmp(parity_buf)){
			if(GetCause() & EXC_DBE)
				cpu_parity_err = 1;
			/* read the ISR to verify that a buffer error happened */

			if (( GetISR() & IMR_BUFERROR) == 0) {
				cpu_parity_err = 0;
				printf("ISR register doesn't reflect a BufError, isr %x\n",GetISR());
			}

			/* toggle the clear buffer interrupt signal and reset the lance*/

			*config = (*config & ~CR_CLRBUFERRB) & ~CR_LNCRESETB;
			*config = *config | (CR_CLRBUFERRB | CR_LNCRESETB);

			/* read the ISR to verify that a buffer error is cleared */

			if ((GetISR() & IMR_BUFERROR) != 0) {
				cpu_parity_err = 0;
				printf("Can't clear the BufError, isr %x\n",GetISR());
			}
		}
		else {
			addr = PHYS_TO_K1(LANCE_BUFFER_RB3125 + byte_lane);

		/* write bad parity into the address pointed to */

			SetBadPar_RB3125(addr,byte_lane);	

		/* Cause the parity error */

			junk = *buf_ptr;

		/*Wait a little time */

			wait = 0x100;
			while(wait--);

			if(!cpu_parity_err){
				printf("\naddress %x byte_lane %x\n",(LANCE_BUFFER_RB3125 + byte_lane),byte_lane);
				cpu_err = 1;
				*buf_ptr = 0;
				break;
			}
		} 
	} 
	*buf_ptr = 0;
	nofault = 0;
	SetSR(sr_save);
	if(cpu_err){ 
		pon_puts(failure); 
		return(FAIL); 
	}
	else{
		pon_puts(success);
		return(PASS);
	}
}
