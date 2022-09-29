#ident "$Header: machaddr.c,v 1.13 90/05/23 15:19:37 huang Exp $"
/* $Copyright$ */

/*
 * This file is used to specify all the machine dependent addresses
 * used in the standalone (and probably the kernel).  Any module
 * that needs these address will 'extern' it and cast it to what
 * it needs.
 *
 * By definition all addresses will be expressed as K1.  All #defines
 * are the REAL addresses.
 *
 * NOTE: This file should be the same file as the kernel
 *	master.d/machaddr, which should be included by all
 *
 */

#ifdef STANDALONE
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#endif

#ifdef KERNEL
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#endif

/* IDPROM -- same 'part' on all machines so far -- saio/saio.c */

int IDPROM_ADDR[] = {
    PHYS_TO_K1(IDPROM_R2300),		/* M500 */
    PHYS_TO_K1(IDPROM_R2300),		/* M800 */
    PHYS_TO_K1(IDPROM_R2300),		/* M1000 */
    PHYS_TO_K1(IDPROM_R2400),		/* M120 */
    PHYS_TO_K1(IDPROM_R3200),		/* M2000 */
    PHYS_TO_K1(IDPROM_R6300),		/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    PHYS_TO_K1(IDPROM_R2400),		/* M180 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R3030),	/* M20 */
    PHYS_TO_K1(IDPROM_R3200)		/* Genesis */
};

/* ETHERNET PROM */

int ENETPROM_ADDR[] = {
    0,					/* M500 */
    0,					/* M800 */
    0,					/* M1000 */
    PHYS_TO_K1(ENETPROM_BASE),		/* M120 */
    0,					/* M2000 */
    0,					/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    PHYS_TO_K1(ENETPROM_BASE),		/* M180 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R3030),	/* M20 */
    PHYS_TO_K1(ENETPROM_RB3125)		/* Genesis */
};

/* LED register -- Intrepid has 2 more LED bits -- saio/saio.c */

int LED_REG[] = {
    PHYS_TO_K1(LED_REG_R2300),		/* M500 */
    PHYS_TO_K1(LED_REG_R2300),		/* M800 */
    PHYS_TO_K1(LED_REG_R2300),		/* M1000 */
    PHYS_TO_K1(LED_REG_R2400),		/* M120 */
    PHYS_TO_K1(LED_REG_R3200),		/* M2000 */
    PHYS_TO_K1(LED_REG_R6300),		/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    PHYS_TO_K1(LED_REG_R2400),		/* M180 */
    0,					/* M20 */
    PHYS_TO_K1(LED_REG_R3200)		/* Genesis */
};

#ifdef STANDALONE

/* Duart0 information -- saio/s2681cons.c */

int CN_REG_BASE[] = {
    PHYS_TO_K1(DUART0_STR_R2300),		/* M500 */
    PHYS_TO_K1(DUART0_STR_R2300),		/* M800 */
    PHYS_TO_K1(DUART0_STR_R2300),		/* M1000 */
    PHYS_TO_K1(DUART0_STR_R2400),		/* M120 */
    PHYS_TO_K1(DUART0_STR_R3200),		/* M2000 */
    PHYS_TO_K1(DUART0_STR_R6300),		/* M6000 */
    0,						/* M12 */
    0,						/* M12 Sable */
    PHYS_TO_K1(DUART0_STR_R2400),		/* M180 */
    0,						/* M20 */
    0						/* Genesis */
};

/* SCC information -- saio/scc_cons.c */

int SCC_REG_BASE[] = {
    0,						/* M500 */
    0,						/* M800 */
    0,						/* M1000 */
    0,						/* M120 */
    0,						/* M2000 */
    0,						/* M6000 */
    0,						/* M12 */
    0,						/* M12 Sable */
    0,						/* M180 */
    PHYS_TO_K1(SCC_BASE_R3030),			/* M20 */
    PHYS_TO_K1(SCC_BASE_RB3125)			/* Genesis */
};

#endif

/* LANCE information -- Currently Intrepid, later M2000 -- saio/if_lance.c */

int LANCE_ADDR[] = {
    0,					/* M500 - No Lance */
    0,					/* M800 - No Lance */
    0,					/* M1000 - No Lance */
    PHYS_TO_K1(LANCE_BASE_R2400),	/* M120 */
    PHYS_TO_K1(LANCE_BASE_R3200),	/* M2000 */
    0,					/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    PHYS_TO_K1(LANCE_BASE_R2400),	/* M180 */
    PHYS_TO_K1(LANCE_BASE_R3030),	/* M20 */
    PHYS_TO_K1(LANCE_BASE_RB3125)	/* Genesis */
};

/* Real time clock -- used on M-series only -- saio/timer.c */

int RT_CLOCK_ADDR[] = {
    PHYS_TO_K1(RT_CLOCK_ADDR_R2300),	/* M500 */
    PHYS_TO_K1(RT_CLOCK_ADDR_R2300),	/* M800 */
    PHYS_TO_K1(RT_CLOCK_ADDR_R2300),	/* M1000 */
    0,					/* M120 - uses TODC */
    0,					/* M2000 - uses TODC */
    0,					/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    0,					/* M120 - uses TODC */
    0,					/* M20 */
    0					/* Genesis */
};

/* Time-of-day clock -- used on Intrepid and M2000 -- saio/timer.c */

int TODC_CLOCK_ADDR[] = {
    0,					/* M-series uses RT */
    0,
    0,
    PHYS_TO_K1(TODC_CLOCK_ADDR_R2400),	/* M120 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R3200),	/* M2000 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R6300),	/* M6000 */
    0,					/* M12 */
    0,					/* M12 Sable */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R2400),	/* M180 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R3030),	/* M20 */
    PHYS_TO_K1(TODC_CLOCK_ADDR_R3200)	/* Genesis */
};

/* I8254 clock */

int PT_CLOCK_ADDR[] = {
	PHYS_TO_K1(PT_CLOCK_ADDR_R2300),	/* M500 */
	PHYS_TO_K1(PT_CLOCK_ADDR_R2300),	/* M800 */
	PHYS_TO_K1(PT_CLOCK_ADDR_R2300),	/* M1000 */
	PHYS_TO_K1(PT_CLOCK_ADDR_R2400),	/* M120 */
	PHYS_TO_K1(PT_CLOCK_ADDR_R3200),	/* M2000 */
	0,					/* M6000 */
	0,					/* M12 */
	0,					/* M12 Sable */
	PHYS_TO_K1(PT_CLOCK_ADDR_R2400),	/* M180 */
	0,					/* M20 */
	PHYS_TO_K1(PT_CLOCK_ADDR_R3200)		/* Genesis */
};

/* Timer 0 acknowledge */

int TIM0_ACK_ADDR[] = {
	PHYS_TO_K1(TIM0_ACK_ADDR_R2300),	/* M500 */
	PHYS_TO_K1(TIM0_ACK_ADDR_R2300),	/* M800 */
	PHYS_TO_K1(TIM0_ACK_ADDR_R2300),	/* M1000 */
	PHYS_TO_K1(TIM0_ACK_ADDR_R2400),	/* M120 */
	PHYS_TO_K1(TIM0_ACK_ADDR_R3200),	/* M2000 */
	0,					/* M6000 */
	0,					/* M12 */
	0,					/* M12 Sable */
	PHYS_TO_K1(TIM0_ACK_ADDR_R2400),	/* M180 */
	0,					/* M20 */
	PHYS_TO_K1(TIM0_ACK_ADDR_R3200)		/* Genesis */
};
		
/* Timer 1 acknowledge */

int TIM1_ACK_ADDR[] = {
	PHYS_TO_K1(TIM1_ACK_ADDR_R2300),	/* M500 */
	PHYS_TO_K1(TIM1_ACK_ADDR_R2300),	/* M800 */
	PHYS_TO_K1(TIM1_ACK_ADDR_R2300),	/* M1000 */
	PHYS_TO_K1(TIM1_ACK_ADDR_R2400),	/* M120 */
	PHYS_TO_K1(TIM1_ACK_ADDR_R3200),	/* M2000 */
	0,					/* M6000 */
	0,					/* M12 */
	0,					/* M12 Sable */
	PHYS_TO_K1(TIM1_ACK_ADDR_R2400),	/* M180 */
	0,					/* M20 */
	PHYS_TO_K1(TIM1_ACK_ADDR_R3200)		/* Genesis */
};

/*
 * Duart defines mostly for io/sduart.c in kernel
 */

int DUART0[] = {
	PHYS_TO_K1(DUART0_BASE_R2300),		/* M500 */
	PHYS_TO_K1(DUART0_BASE_R2300),		/* M800 */
	PHYS_TO_K1(DUART0_BASE_R2300),		/* M1000 */
	PHYS_TO_K1(DUART0_BASE_R2400),		/* M120 */
	PHYS_TO_K1(DUART0_BASE_R3200),		/* M2000 */
	PHYS_TO_K1(DUART0_BASE_R6300),		/* M6000 */
	0,					/* M12 */
	0,					/* M12 Sable */
	PHYS_TO_K1(DUART0_BASE_R2400),		/* M180 */
	0,					/* M20 */
	0					/* Genesis */
};

int DUART1[] = {
	PHYS_TO_K1(DUART1_BASE_R2300),		/* M500 */
	PHYS_TO_K1(DUART1_BASE_R2300),		/* M800 */
	PHYS_TO_K1(DUART1_BASE_R2300),		/* M1000 */
	PHYS_TO_K1(DUART1_BASE_R2400),		/* M120 */
	0x0,					/* No second duart on M2000 */
	0x0,					/* No second duard on M6000 */
	0x0,					/* M20 */
	0x0,					/* M20 Sable */
	PHYS_TO_K1(DUART1_BASE_R2400),		/* M180 */
	0,					/* M20 */
	0					/* Genesis */
};

/* SCC information -- saio/scc_cons.c */

int SCC0[] = {
    0,						/* M500 */
    0,						/* M800 */
    0,						/* M1000 */
    0,						/* M120 */
    0,						/* M2000 */
    0,						/* M6000 */
    0,						/* M12 */
    0,						/* M12 Sable */
    0,						/* M180 */
    PHYS_TO_K1(SCC_BASE_R3030),			/* M20 */
    PHYS_TO_K1(SCC_BASE_RB3125)			/* Genesis */
};

/*
 * SR Interrupt Enable bit mask for FP
 */

int SR_IBIT_FP[] = {
	SR_IBIT6,				/* M500 */
	SR_IBIT6,				/* M800 */
	SR_IBIT6,				/* M1000 */
	SR_IBIT6,				/* M120 */
	SR_IBIT6,				/* M2000 */
	SR_IBIT4,				/* M6000 */
	0,	 				/* M12 */
	0,					/* M12 */
	SR_IBIT6,				/* M180 */
	SR_IBIT6,				/* M20 */
	SR_IBIT6				/* Genesis */
};
/* NCR scsi information */

int SCSI_REG_BASE[] = {
    0,                                          /* M500 */
    0,                                          /* M800 */
    0,                                          /* M1000 */
    0,                                          /* M120 */
    0,                                          /* M2000 */
    0,                                          /* M6000 */
    0,                                          /* M12 */
    0,                                          /* M12 Sable */
    0,                                          /* M180 */
    PHYS_TO_K1(SCSI_BASE_R3030),		/* M20 */
    PHYS_TO_K1(SCSI_BASE_RB3125),		/* RB3125 */
};

/* RAMBO dma information */

int RAMBO_REG_BASE[] = {
    0,                                          /* M500 */
    0,                                          /* M800 */
    0,                                          /* M1000 */
    0,                                          /* M120 */
    0,                                          /* M2000 */
    0,                                          /* M6000 */
    0,                                          /* M12 */
    0,                                          /* M12 Sable */
    0,                                          /* M180 */
/*    PHYS_TO_K1(RAMBO_BASE_R3030),		/* M20 */
    PHYS_TO_K1(RAMBO_BASE),			/* M20 */
    0,						/* RB3125 */
};


