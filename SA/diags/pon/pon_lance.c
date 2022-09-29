#ident "$Header: pon_lance.c,v 1.11.1.1 90/07/18 14:32:02 huang Exp $"
/* $Copyright$ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/vmereg.h"
#include "prom/prom.h"
#include "pon.h"

#undef	USED

/******************************************************/
/* lanhdr.h this file defines all data structures to  */
/* be used by lan programs                            */
/******************************************************/

#define gimmeabreak	for(;;)
#define ZEROES		0000
#define hangaround	for
#define MAXSZ		64
#define MINSZ		28

struct ladevice {
	u_int   :16;
	u_short rdp;
	u_int   :16;
	u_short rap;
};

/* AS mapped in INTREPID (big endian) memory system adjacent
   half word registers in IO appears as the less significant
   half of adjacent words situated at increasing addresses
   this mapping is achieved by the following struct
   the blank bit field of 16 bits gives the alignment      */

#define	CSR0		0
#define	CSR1		1
#define CSR2		2
#define	CSR3		3

/* pkt stuff */

#define MAXENTRIES	16		/* presently can go upto 128 */
#define ENTRIES		1
#define LOG2MAXENTRIES	4
#define PKTSZ		64
#define MAXPKTSZ	28		/* 256 */
#define MAXBUFFSZ	28		/* 256 */
#define LPBKPKTSZ	28
#define MAXBUFSIZE	0xff00		/* two'comp of MAXBUFSZ */
#define MAXFLSZ		99999999

/* Time out to get out from polling for certain activities */

#define TIMEOUT		600000
#define TIMEOUT1	60000
#define WAITFORPLY	600000

/* Defines for flags and parameters */

#define EXTLOOPBK	1
#define INTLOOPBK	0
#define PROMISCUOUS	1
#define ADDRECOGNISE	0
#define INTERRUPTMODE	1
#define POLLEDMODE	0

/* CSR0 register bits */
/* This is a 16 bit register whose bit definitions are given below */
/* in a descending order from bit 15 to bit 0                      */

#define ERR		0x8000
#define BABL		0x4000
#define CERR		0x2000
#define MISS		0x1000
#define MERR		0x800
#define RINT		0x400
#define TINT		0x200
#define IDON		0x100
#define INTR		0x80
#define INEA		0x40
#define RXON		0x20
#define TXON		0x10
#define TDMD		0x8
#define STOP		0x4
#define STRT		0x2
#define INIT		0x1

/* TMD3 */

#define RTRY		0x1

/* CSR3 register */

#define BCON		0x1	/* bit 0 */
#define ACON		0x2	/* bit 1 */
#define BSWP		0x4	/* bit 2 */

/* This structure definition is for the initialization block */
/* set up by the cpu and when INIT bit is set in CSR0 reg    */
/* lance will load this block to its internal registers,which*/
/* are not accessible otherwise                              */

struct initblk {
	u_int prom	:1;
	u_int		:8;
	u_int intl	:1;
	u_int drty	:1;
	u_int coll	:1;
	u_int dtcr	:1;
	u_int loop	:1;
	u_int dtx	:1;
	u_int drx	:1;
	u_int char phy_adr[6];
	u_int char log_adr_fltr[8];
	u_int RDRAlo	:16;
	u_int RLEN	:3;
	u_int		:5;
	u_int RDRAhi	:8;
	u_int TDRAlo	:16;
	u_int TLEN	:3;
	u_int		:5;
	u_int TDRAhi	:8;
};

/* This array of structures definition is for the receive ring    */
/* whose elements are descriptor entries                          */

struct rxmsgdes {
	/* RMD0 & RMD1 */

	u_int laddr	:16;
	u_int own	:1;
	u_int err	:1;
	u_int fram	:1;
	u_int oflo	:1;
	u_int crc	:1;
	u_int buff	:1;
	u_int stp	:1;
	u_int enp	:1;
	u_int haddr	:8;

	/* RMD2 & RMD3 */

	u_int ones	:4;
	u_int bcnt	:12;
	u_int		:4;
	u_int mcnt	:12;
};

/* This array of structures definition is for the receive ring    */
/* whose elements are descriptor entries                          */

struct txmsgdes {
	/* TMD0 & TMD1 */

	u_int laddr	:16;
	u_int own	:1;
	u_int err	:1;
	u_int		:1;
	u_int more	:1;
	u_int one	:1;
	u_int def	:1;
	u_int stp	:1;
	u_int enp	:1;
	u_int haddr	:8;

	/* TMD2 & TMD3 */

	u_int ones	:4;
	u_int bcnt	:12;
	u_int buff	:1;
	u_int uflo	:1;
	u_int		:1;
	u_int lcol	:1;
	u_int lcar	:1;
	u_int rtry	:1;
	u_int tdr	:10;
};

/* Global Data  **/

int tmpcnt, tmpcnt1;
char *tmptxptr, *tmprxptr;
char *txbufptr[MAXENTRIES];
char *rxbufptr[MAXENTRIES];
char *txbufaddr, *rxbufaddr;
struct txmsgdes *ptr_txmsgdes;
struct rxmsgdes *ptr_rxmsgdes;

u_char srcaddr[6];
u_char dstnaddr[6];
int over;
u_char *phyadrptr;
struct initblk *blkptr;
u_short init[4];
long tx, rx, ib;

extern char success[], failure[], skipped[];
extern int ENETPROM_ADDR[];
extern int LANCE_ADDR[];


Pon_LanceMaster()

{
	register int j;
    	register volatile u_int *config;

	switch (machine_type) {
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
	case BRDTYPE_R3030:
		break;
	case BRDTYPE_RB3125:
		/* remove Lance from reset */
		config = (u_int*)PHYS_TO_K1(CPU_CR_M2000);
		*config |= CR_LNCRESETB;
		break;
	default:
		return(PASS);
		break;
	}

#ifndef	R3030
	pon_set_leds(PON_LANCEMASTER);
#endif	!R3030
	pon_puts("Lance Master Test...");

#ifdef	SABLE
	pon_puts(success);
	return(PASS);
#endif
#ifdef	R3030
	if (GetDepend() & (PON_FAULT_LANCE | PON_FAULT_MEM)) {
#else	R3030
	if (GetDepend() & (PON_FAULT_IMR | PON_FAULT_LANCE | PON_FAULT_MEM | PON_FAULT_LNC_BUFF)) {
#endif	R3030
		pon_puts(skipped);
		goto norun;
	}

	/* tmpcnt is used for pkt numbering */

	tmpcnt = 0;
	tmpcnt1 = 0;
	if (lan__init()) {
printf("init failed\n");
		goto failed;
	}

	for (j = 0; j < 6; j++) {
		dstnaddr[j] = srcaddr[j];
	}

	dumpkt();
	if (lan_txmt_chk()) {
printf("xmit failed\n");
		goto failed;
	}
	else if (lan_recv_chk()) {
printf("rcv failed\n");
		goto failed;
	}
	else if (over) {
		over = 0;
		pon_puts(success);
		return(PASS);
	}
	else {

failed:
		pon_puts(failure);
		FastFlash(PON_LANCEMASTER);
#ifndef	R3030
		pon_set_leds(PON_LANCEMASTER);
#endif	!R3030

norun:
		SetDepend(PON_FAULT_LANCE);
		return(FAIL);
	}
}


Pon_LanceSlave()

{
    	register volatile u_int *config;

	switch (machine_type) {
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
	case BRDTYPE_R3030:
		break;
	case BRDTYPE_RB3125:
		/* remove Lance from reset */
		config = (u_int*)PHYS_TO_K1(CPU_CR_M2000);
		*config |= CR_LNCRESETB;
		break;

	default:
		return(PASS);
		break;
	}

#ifndef	R3030
	pon_set_leds(PON_LANCESLAVE);
#endif	!R3030
	pon_puts("Lance Slave Register Test...");

#ifdef	SABLE
	pon_puts(success);
	return(PASS);
#endif
	if (!(LanceRegisterTest())) {
		pon_puts(success);
		return(PASS);
	}
	else {
		pon_puts(failure);
		FastFlash(PON_LANCESLAVE);
#ifndef	R3030
		pon_set_leds(PON_LANCESLAVE);
#endif	!R3030
		SetDepend(PON_FAULT_LANCE);
		return(FAIL);
	}
}


static LanceRegisterTest()

{
	register volatile struct ladevice *lan = (struct ladevice *)MACHDEP(LANCE_ADDR);
	register int dscr0, dscr1, dscr2, dscr3, dscr4, dscr5, dscr6;

	lan->rap = CSR0;
	wbflush();
	lan->rdp = STOP;
	wbflush();
	dscr0 = lan->rdp;

	/** CSR1 TEST **/

	lan->rap = CSR1;
	wbflush();
	lan->rdp = 0x5555;
	wbflush();
	dscr1 = lan->rdp;
	lan->rdp = 0xAAAA;
	wbflush();
	dscr2 = lan->rdp;

	/** CSR2 TEST **/

	lan->rap = CSR2;
	wbflush();
	lan->rdp = 0x5555;
	wbflush();
	dscr3 = lan->rdp;
	lan->rdp = 0xAAAA;
	wbflush();
	dscr4 = lan->rdp;

	/** CSR3 TEST **/

	lan->rap = CSR3;
	wbflush();
	lan->rdp = 0x5;
	wbflush();
	dscr5 = lan->rdp;
	lan->rdp = 0x2;
	wbflush();
	dscr6 = lan->rdp;

	if ((dscr1 != 0x5555) || (dscr2 != 0xaaaa) ||
	    (dscr3 != 0x5555) || (dscr4 != 0xaaaa) ||
	    (dscr5 != 0x0005) || (dscr6 != 0x0002) ||
	    (dscr0 != 0x4)) {
		return(1);
	}
	else {
		return(0);
	}
}


static lan_recv_chk()

{
	register volatile struct ladevice *lan = (struct ladevice *)MACHDEP(LANCE_ADDR);
	register int i, j;
#ifdef	DEBUG
	char *rxtimeout = "TIMEOUT ON RECEPTION\n\r";
	char *rxerr = "ERR:RX error\n\r";
	char *dataerr = "ERR: Data Comparison Failure\n\r";
	char *dataok = "Data Comparison OK\n\r";
	char *rcvok = "Reception successful\n\r";
	char *pass = "PASSED LANCE MASTER TEST\n\r";
	char *fail = "FAILED LANCE MASTER TEST\n\r";
#endif	DEBUG

	lan->rap = CSR0;
	wbflush();

	/* Check for RX */

	for (i = 0; ((i < TIMEOUT1) && !(lan->rdp & RINT)); i++)
		;

	if (i == TIMEOUT1) {
		if (!(lan->rdp & RINT)) {
#ifdef	DEBUG
			pon_puts(rxtimeout);
#endif	DEBUG
			lan->rdp = RINT;
			wbflush();
		}
		return(1);
	}
	else {
		/* Check for error in reception */

		if (lan->rdp & ERR) {
#ifdef	DEBUG
			pon_puts(rxerr);
#endif	DEBUG
			lan->rdp = RINT;
			wbflush();
			return(1);
		}
		else {
			for (i = 0; i < ENTRIES; i++, tmpcnt++) {
				/* Check for err in RMD1 */

				if (!ptr_rxmsgdes[i].own) {
					if (ptr_rxmsgdes[i].err) {
#ifdef	DEBUG
						pon_puts(rxerr);
#endif	DEBUG
						return(1);
					} /** end of if err **/
					else {
						/* Good Pkt so do data comparison */
#ifdef	DEBUG
						pon_puts(rcvok);
#endif	DEBUG
						tmptxptr = txbufptr[i];
						tmprxptr = rxbufptr[i];
						for (j = 0; j < MINSZ; j++) {
							if (*txbufptr[i] != *rxbufptr[i]) {
#ifdef	DEBUG
								pon_puts(rxerr);
#endif	DEBUG
								ptr_rxmsgdes[i].own = 1;
								return(1);
							}
							else {
								rxbufptr[i]++;
								txbufptr[i]++;
							}
						}

						txbufptr[i] = tmptxptr;
						rxbufptr[i] = tmprxptr;
#ifdef	DEBUG
						pon_puts(dataok);
#endif	DEBUG
						ptr_rxmsgdes[i].own = 1;
						lan->rdp = RINT;
						wbflush();
						over = 1;
						return(0);
					}
				}
			}
		}
	}
}


static lan__init()

{
	register volatile struct ladevice *lan = (struct ladevice *)MACHDEP(LANCE_ADDR);
	register int i;
	register int repeat, ringentry;
#ifdef	DEBUG
	char *initerr = "ERR: Timeout on waiting for end of initialization \n\r";
	char *initok  = "Initialization is over successfully\n\r";
#endif	DEBUG

	/* Enable interrupts at lance lvl but mask in IMR  */

	if(machine_type == BRDTYPE_RB3125)
    		SetIMR((GetIMR() | (IMR_LNCINTB | IMR_BUFERROR)));
	else
		SetIMR(INT_ENET);
	SetSR(((GetSR() & SR_BEV) & ~(SR_IEC | SR_IMASK)) | SR_IBIT3);

	/* Get the addresses of the arrays to hold the     */
	/* rings and the initialization block              */
	/* I have done malloc's to find out the freebufs   */
	/* available within the 16Meg and given it for     */
	/* LANCE communication rings and initblk           */
	/* this is done so that always the buffers will    */
	/* be within 16Meg to enable testing lance         */
	/* even in a 32Meg machine                         */
	/* rather crude way.. but short of time            */
	/* pl forgive ................................     */

	if(machine_type == BRDTYPE_RB3125){
		tx = (u_long)PHYS_TO_K1(LANCE_BUFFER_RB3125);
		rx = (u_long)PHYS_TO_K1(LANCE_BUFFER_RB3125 + 0x10000);
		ib = (u_long)PHYS_TO_K1(LANCE_BUFFER_RB3125 + 0x20000);
		/* Get the starting address of TX & RX buffers */

		txbufaddr = (char *)PHYS_TO_K1(LANCE_BUFFER_RB3125 + 0x30000);
		rxbufaddr = (char *)PHYS_TO_K1(LANCE_BUFFER_RB3125 + 0x40000);

	}
	else {
		tx = (u_long)PHYS_TO_K1(PON_SCRATCHMEM);
		rx = (u_long)PHYS_TO_K1(PON_SCRATCHMEM + 0x10000);
		ib = (u_long)PHYS_TO_K1(PON_SCRATCHMEM + 0x20000);
		/* Get the starting address of TX & RX buffers */

		txbufaddr = (char *)PHYS_TO_K1(PON_SCRATCHMEM + 0x30000);
		rxbufaddr = (char *)PHYS_TO_K1(PON_SCRATCHMEM + 0x40000);

	}


#ifdef	DEBUG_C
	printf("address = %x\n",tx);
	printf("address = %x\n",rx);
	printf("address = %x\n",ib);
#endif	DEBUG

	/* Align rings in a quadword boundary */

	tx += (tx&7) ? 8 - (tx & 0x7) : 0;
	rx += (rx&7) ? 8 - (rx & 0x7) : 0;

	/* Align initblk in a evenbyte boundary */

	ib += (ib&1) ? 1 : 0;

	/* Make tx & rx msgdescriptor pointers point to */
	/* thirumba pointer a edukanum                  */
	/* okala oli                                    */

	ptr_txmsgdes = (struct txmsgdes *)tx;
	ptr_rxmsgdes = (struct rxmsgdes *)rx;

	/* Make pointer to init blk point to aligned area */

	blkptr = (struct initblk *)ib;

#ifdef	DEBUG_C
	printf("address txbuf = %x\n",txbufaddr);
	printf("address  rxbuf = %x\n",rxbufaddr);
#endif	DEBUG

	/* Load MODE reg */

	blkptr->prom = 1;
	blkptr->intl = 1;
	blkptr->loop = 1;
	blkptr->dtx = 0;

	/* Initialize TX descriptor ring */

	for (i = 0; i < ENTRIES; i++) {
		txbufptr[i] = txbufaddr;
		ptr_txmsgdes[i].laddr = getlow(txbufaddr);
		ptr_txmsgdes[i].haddr = gethigh(txbufaddr);
		txbufaddr = txbufaddr + 29;
		ptr_txmsgdes[i].bcnt = -28;
		ptr_txmsgdes[i].ones = 0xf;
	}

	/* Initialization block */

	if(machine_type == BRDTYPE_RB3125)
		phyadrptr = (u_char *)PHYS_TO_K1(IDPROM_R3200 + ID_EA1_OFF);
	else
		phyadrptr = (u_char *)MACHDEP(ENETPROM_ADDR) + 3;
	for (i = 0; i < 6; i++) {
		srcaddr[i] = *phyadrptr;
		phyadrptr++;
		phyadrptr++;
		phyadrptr++;
		phyadrptr++;
	}

	/* BSWP active since that will swap the  */
	/* address pattern in the packet while   */
	/* reception so swapped address should   */
	/* be compared not with unswapped        */
	/* but with already swapped address      */
	/* programmed in init blk                */
	/* better don't read comments,that might */
	/* SWAP you out of good thinking  ....   */

	blkptr->phy_adr[0] = srcaddr[1];
	blkptr->phy_adr[1] = srcaddr[0];
	blkptr->phy_adr[2] = srcaddr[3];
	blkptr->phy_adr[3] = srcaddr[2];
	blkptr->phy_adr[4] = srcaddr[5];
	blkptr->phy_adr[5] = srcaddr[4];
	blkptr->RLEN = 0;
	blkptr->TLEN = 0;

	/* INITIALIZE RX ring */

	for (i = 0; i < ENTRIES; i++) {
		rxbufptr[i] = rxbufaddr;
		ptr_rxmsgdes[i].laddr = getlow(rxbufaddr);
		ptr_rxmsgdes[i].haddr = gethigh(rxbufaddr);
		rxbufaddr = rxbufaddr + 33;
		ptr_rxmsgdes[i].bcnt = -32;
		ptr_rxmsgdes[i].ones = 0xf;

		/* Set OWN bit to give control to LANCE */

		ptr_rxmsgdes[i].own = 1;
	}

	/* Write ring buffer address into initialization block */

	blkptr->TDRAlo = K1_TO_PHYS(tx) & 0xfff8;
	blkptr->TDRAhi = (K1_TO_PHYS(tx) >> 16) & 0xff;
	blkptr->RDRAlo = K1_TO_PHYS(rx) & 0xfff8;
	blkptr->RDRAhi = (K1_TO_PHYS(rx) >> 16) & 0xff;

	/* Write initialization block address into INIT structure */
	/* this will correspond to CSR1 CSR2 regs                 */

	init[1] = K1_TO_PHYS((u_int)ib) & 0xfffe;        /* CSR1 */
	init[2] = K1_TO_PHYS(((u_int)ib >> 16 )) & 0xff; /* CSR2 */

	/* Write ACON BCON BSWP in CSR3 reg                       */
	/* ACON is 0 since ALE is active high                     */
	/* BCON is 0 since ~BM0 & ~BM1 & HOLD is used in H/W      */
	/* BSWP is 1 since the system is BIGENDIAN                */

	init[3] = BSWP;

	/* Access CSR0 reg and set STOP bit */

	lan->rap = CSR0;
	lan->rdp = STOP;
	wbflush();

	/* Write CSR1 & CSR2 & CSR3 */

	lan->rap = CSR3;
	lan->rdp = init[3];
	wbflush();
	lan->rap = CSR1;
	lan->rdp = init[1];
	wbflush();
	lan->rap = CSR2;
	lan->rdp = init[2];
	wbflush();

	/* Start lance by making  csr0 INIT & STRT  bit set */

	lan->rap = CSR0;
	lan->rdp = STOP;
	wbflush();
	lan->rdp = INIT | STRT;
	wbflush();

	/* Poll on IDON bit to find the end of initialization */

	for (i = 0;(i < TIMEOUT) && !(lan->rdp & IDON); i++)
		;

	if (i == TIMEOUT) {
#ifdef	DEBUG
		pon_puts(initerr);
#endif	DEBUG
		return(1);
	}
	else {
#ifdef	DEBUG
		pon_puts(initok);
#endif	DEBUG
		/* Reset IDON bit enable ints */

		lan->rdp = IDON | INEA;
		wbflush();
		return(0);
	}
}


static dumpkt()

{
	register int i, j;
#ifdef	DEBUG
	char *dumpok = "DUMP PKT OK\n\r";
#endif	DEBUG

	for (i = 0; i < ENTRIES; i++) {
		if (!ptr_txmsgdes[i].own) {
			/* Plug destination address to pkt */

			for (j = 0; j < 6; j++) {
				*txbufptr[i]++ = dstnaddr[j];
			}

			/* Plug source address to pkt */

			for (j = 0; j < 6; j++) {
				*txbufptr[i]++ = srcaddr[j];
			}

			/* Transfer data from tmpbuf to txbuf */

			for (j = 0; j < 6; j++) {
				*txbufptr[i]++ = srcaddr[j];
			}

			for (j = 0; j < 6; j++) {
				*txbufptr[i]++ = srcaddr[j];
			}

			for (j = 0; j < 24; j++) {
				txbufptr[i]--;
			}

			/* Set OWN STP & ENP bits for this     */
			/* pkt in the corresponding descriptor */
			/* entry                               */

			ptr_txmsgdes[i].own = 1;
			ptr_txmsgdes[i].stp = 1;
			ptr_txmsgdes[i].enp = 1;
#ifdef	DEBUG
			pon_puts(dumpok);
#endif	DEBUG
		}
	}
}


static lan_txmt_chk()

{
	register volatile struct ladevice *lan = (struct ladevice *)MACHDEP(LANCE_ADDR);
	register int i;
#ifdef	DEBUG
	char *txtimeout = "Timeout:Lance has not generated Interrupt\n\r";
	char *txerr = "ERR: error on transmission\n\r";
	char *txerr1 = "ERR: error on transmission in buffer\n\r";
	char *txok = "Transmission successful\n\r";
	char *pass = "PASSED LANCE MASTER TEST\n\r";
	char *fail = "FAILED LANCE MASTER TEST\n\r";
#endif	DEBUG

	/* Set up STRT bit to start txmit operation */
	/* reset IDON bit  & keep INIT bit          */

	lan->rap = CSR0;
	wbflush();

	/* Check for end of transmission             */

	for (i = 0; ((i < TIMEOUT) && !(GetCause() & CAUSE_IP3)); i++)
		;

	if (i == TIMEOUT) {
#ifdef	R3030
		if (!(GetIntR() & IR_NET_INT_B) && !(lan->rdp & TINT)) {
#else	R3030
#ifdef	RB3125
		if (!(GetISR() & ISR_LNCINTB) && !(lan->rdp & TINT)) {
#else
		if (!(GetISR() & INT_ENET) && !(lan->rdp & TINT)) {
#endif	RB3125
#endif	R3030
#ifdef	DEBUG
			pon_puts(txtimeout);
#endif	DEBUG
			lan->rdp = TINT;
			wbflush();
		}

		return(1);
	}
	else {
		/* Check for error in transmission */

		if (lan->rdp & ERR) {
#ifdef	DEBUG
			pon_puts(txerr);
#endif	DEBUG
			lan->rdp = MERR;
			wbflush();
			lan->rdp = TINT;
			wbflush();
			return(1);
		}
		else {
			for (i = 0; i < ENTRIES; i++, tmpcnt1++) {
				/* Check for err in TMD1 */

#ifdef	USED
				if (!ptr_txmsgdes[i].own) {
#endif	USED
					if (ptr_txmsgdes[i].err) {
#ifdef	DEBUG
						pon_puts(txerr1);
#endif	DEBUG
						lan->rap = CSR0;
						lan->rdp = TINT;
						wbflush();
						return(1);
					}
					else {
#ifdef	DEBUG
						pon_puts(txok);
#endif	DEBUG
						lan->rap = CSR0;
						lan->rdp = TINT;
						wbflush();
					}
#ifdef	USED
				} /** own **/
#endif	USED
			} /** for **/

			return(0);
		}
	}
}


static getlow(ptr)

register u_long  ptr;

{
	register u_int bot;

	bot = (u_int)(ptr) & 0xffff;
	return(bot);
}


static gethigh(ptr)

register u_long ptr;

{
	register u_int top;

	top = (u_int)(ptr >> 16);
	return(top);
}
