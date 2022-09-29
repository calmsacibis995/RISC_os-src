#ident "$Header: pon_ecc.c,v 1.6.3.1 90/07/18 14:30:47 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/mem_board.h"
#include "machine/cpu_board.h"
#include "machine/r3250reg.h"
#include "mipsvme/vmereg.h"
#include "saio/setjmp.h"
#include "pon_ecc.h"
#include "pon.h"

/*
 *	Local definitions.
 */
#define	NULL		0
/*
 *	External functions.
 */
extern int GetSR(),GetCause(),SetSR(),GetSR(),GetFp(),get_machine_type();
extern int GetMemIoReg(),PutMemIoReg(),RealException(),pon_puts();
extern int pon_set_leds(),pon_puthex();
extern void InvalidateWord(),ClearBlock();
extern int *nofault;
extern char success[], failure[], skipped[];
jmp_buf fault_buf;
extern	char f_slot[8];

/*
 *	Data -> check array (for TERSE tests)
 */
static long BitArray[] = {
	0x010a0804,0x03000000,0x01800804,0x038a0000,
	0x02800804,0x008a0000,0x020a0804,0x00000000,
	0x03880000,0x01820804,0x03020000,0x01080804,
	0x00020000,0x02080804,0x00880000,0x02820804,
	0x00820000,0x02880804,0x00080000,0x02020804,
	0x03080000,0x01020804,0x03820000,0x01880804,
	0x02000804,0x000a0000,0x028a0804,0x00800000,
	0x018a0804,0x03800000,0x01000804,0x030a0000,
	0x01820004,0x03880800,0x01080004,0x03020800,
	0x02080004,0x00020800,0x02820004,0x00880800,
	0x03000800,0x010a0004,0x038a0800,0x01800004,
	0x008a0800,0x02800004,0x00000800,0x020a0004,
	0x000a0800,0x02000004,0x00800800,0x028a0004,
	0x03800800,0x018a0004,0x030a0800,0x01000004,
	0x02880004,0x00820800,0x02020004,0x00080800,
	0x01020004,0x03080800,0x01880004,0x03820800,
	0x00080004,0x02020800,0x00820004,0x02880800,
	0x03820004,0x01880800,0x03080004,0x01020800,
	0x028a0800,0x00800004,0x02000800,0x000a0004,
	0x01000800,0x030a0004,0x018a0800,0x03800004,
	0x01800800,0x038a0004,0x010a0800,0x03000004,
	0x020a0800,0x00000004,0x02800800,0x008a0004,
	0x03020004,0x01080800,0x03880004,0x01820800,
	0x00880004,0x02820800,0x00020004,0x02080800,
	0x00800804,0x028a0000,0x000a0804,0x02000000,
	0x030a0804,0x01000000,0x03800804,0x018a0000,
	0x02020000,0x00080804,0x02880000,0x00820804,
	0x01880000,0x03820804,0x01020000,0x03080804,
	0x01080000,0x03020804,0x01820000,0x03880804,
	0x02820000,0x00880804,0x02080000,0x00020804,
	0x038a0804,0x01800000,0x03000804,0x010a0000,
	0x00000804,0x020a0000,0x008a0804,0x02800000
};
/*
 *	Check bit:
 *
 *	  Generate and return check bits for the passed value.
 */
static int CheckBit(v)
register unsigned long v;
{
/*
 *	Locals.
*/
	register int i,j,c;
	static char PTree[7][16] = {
		{31,29,28,26,21,19,18,17,14,11, 9, 8, 7, 6, 4, 0},
		{28,26,24,22,20,18,17,16,12,10, 8, 6, 4, 2, 1, 0},
		{31,29,26,25,23,20,19,16,15,13,10, 9, 7, 4, 3, 0},
		{29,28,27,23,22,21,17,16,13,12,11, 7, 6, 5, 1, 0},
		{31,30,23,22,21,20,19,18,15,14, 7, 6, 5, 4, 3, 2},
		{31,30,29,28,27,26,25,24,15,14,13,12,11,10, 9, 8},
		{31,30,29,28,27,26,25,24, 7, 6, 5, 4, 3, 2, 1, 0}
	};
	/*
	 *	Separate out the bits, generate check parity.
	*/
	c = 0x03;
	for (i = 7; i--;)
		for (j = 16; j--;)
			c ^= ((v & (1 << PTree[i][j])) != 0) << i;
	return (c);
}
/*
 *	Data bit:
 *
 *	  Creates and returns data compatible with the check bits.
 */
static long DataBit(c)
register int c;
{
/*
 *	Locals.
 */
register int i,j;
register long v;
int s[7];
/*
 *	Separate the check bits, make a data match.
 */
	for (i = 0; i < 7; i += 1, c >>= 1)
		s[i] = !(c & 1);
	s[3] = !s[3];
	s[4] = !s[4];
	s[5] = !s[5];
	s[6] = !s[6];
	v = 0;
/*
 *	Do vomit back equations.
 */
	v |= (s[0] ^ s[2] ^ s[3]) << 25;
	v |= (s[2] ^ s[4] ^ s[6]) << 24;
	v |= (s[1] ^ s[2] ^ s[3] ^ s[4] ^ s[5]) << 23;
	v |= (s[0] ^ s[1] ^ s[2] ^ s[4] ^ s[5]) << 19;
	v |= (s[0] ^ s[1] ^ s[2] ^ s[3] ^ s[6]) << 17;
	v |= (s[0] ^ s[3] ^ s[4] ^ s[5] ^ s[6]) << 11;
	v |= (s[0] ^ s[3] ^ s[4]) << 2;
	return (v);
}
/*
 *	Int wait:
 *
 *	  This polls the CPU's status register until an interrupt is
 *	found or the passed count times out. An interrupt structure is
 *	returned.
 */
static IntWait(cnt)
register int cnt;
{
/*
 *	Locals.
*/
	register TestVar *tv = (TestVar *) TestVarBase;
	register int cause,i;
	static short VmeIrq[] = {
		NULL,       M2K_VmeIrq1,M2K_VmeIrq2,M2K_VmeIrq3,
		M2K_VmeIrq4,M2K_VmeIrq5,M2K_VmeIrq6,M2K_VmeIrq7
	};
	static short *VmeIAck[] = {
		(short *) NULL,
		(short *) PHYS_TO_K1(VME_IACK),
		(short *) PHYS_TO_K1(VME_IACK + 2),
		(short *) PHYS_TO_K1(VME_IACK + 4),
		(short *) PHYS_TO_K1(VME_IACK + 6),
		(short *) PHYS_TO_K1(VME_IACK + 8),
		(short *) PHYS_TO_K1(VME_IACK + 0xa),
		(short *) PHYS_TO_K1(VME_IACK + 0xc)
	};

	/*
	 *	Clear the interrupt structure in case nothing happens.
	*/
	tv->Cause = 0;
	tv->CpuIsr = 0;
	tv->VmeIsr = 0;
	tv->ErrAdr = 0;
	for (i = 8; i--;)
		tv->IAck[i] = 0;
/*
 *	Sit here and spin, looking for an interrupt.
 */
	while (cnt--){
		if ((cause = GetCause()) & CAUSE_IPMASK){
			tv->Cause = cause & 0x0000ffff;
/*
 *	On a CPU error, fetch and clear the fault.
 */
			if (cause & CAUSE_IP8){
				tv->ErrAdr = *(long *) PHYS_TO_K1(SBE_ADDR);
				tv->CpuIsr = *(unsigned char *) PHYS_TO_K1(ESR);
				*((long *) PHYS_TO_K1(SBE_ADDR)) = 0;
			}
/*
 *	On a VME error, figure out who sent it and IAck.
 */
			if (cause & CAUSE_IP3){
				tv->VmeIsr = *(unsigned char *) PHYS_TO_K1(VME_ISR);
				for (i = 8; i--;)
					if (tv->VmeIsr & VmeIrq[i])
						tv->IAck[i] = *VmeIAck[i];
			}
			break;
		}
	}
}
/*
 *	ECC data bit:
 *
 *	  This verifies that all ECC bits within the passed region can be
 *	toggled. Much care is taken to stream writes as much as possible.
 *	The data is verified by doing a partial write which causes the array
 *	to go through a RMW cycle. Any type of error will cause an interrupt.
 */
static EccDataBit(m,slt,cnt)
register long *m;
int cnt,slt;
{
/*
 *	Locals.
 */
	register long *b,*e,*eb,x,y;
	register short *bs,*es,*ebs,xs,ys,data;
	register TestVar *tv = (TestVar *) TestVarBase;
/*
 *	Set up stuff so that we barf on errors.
 */
	data = MCR_EN_SINECC | MCR_EN_DBLECC | MCR_INTLEVEL_0B | MCR_EN_MEMVME;
	PutMemIoReg(MEM_CNTRL,slt,data);

/*
 *	Cycle in blocks of 8 words.
 */
	while ((cnt -= 8) >= 0){
/*
 *	Set up registers to hammer on the span.
 */
		bs = (short *) (b = &m[cnt]);
		es = (short *) (e = &tv->BitArray[cnt & 0x78]);
		ebs = (short *) (eb = &tv->BitArray[(~cnt & 0x78)]);
    /*
     *	Fill a span with true form of the ECC bits. This stupid folding occurs
     *	because the reorganizer is fucked up. Creates a full word stream.
     */
		x = e[0];
		y = e[1];
		b[0] = x;
		x = e[2];
		b[1] = y;
		y = e[3];
		b[2] = x;
		x = e[4];
		b[3] = y;
		y = e[5];
		b[4] = x;
		x = e[6];
		b[5] = y;
		y = e[7];
		b[6] = x;
		b[7] = y;
/*
 *	Verify the true form with a partial write. Create a partial stream.
 */
		xs = ebs[16 - 0];
		ys = ebs[16 - 2];
		bs[0] = xs;
		xs = ebs[16 - 4];
		bs[2] = ys;
		ys = ebs[16 - 6];
		bs[4] = xs;
		xs = ebs[16 - 8];
		bs[6] = ys;
		ys = ebs[16 - 10];
		bs[8] = xs;
		xs = ebs[16 - 12];
		bs[10] = ys;
		ys = ebs[16 - 14];
		bs[12] = xs;
		bs[14] = ys;
/*
 *	Set and verify the false form of the ECC bits. This time the verify
 *	is interleaved with the setting to bang full and partial switching.
 */
		x = eb[8 - 0];
		xs = es[0];
		b[0] = x;
		x = eb[8 - 1];
		bs[0] = xs;
		xs = es[2];
		b[1] = x;
		x = eb[8 - 2];
		bs[2] = xs;
		xs = es[4];
		b[2] = x;
		x = eb[8 - 3];
		bs[4] = xs;
		xs = es[6];
		b[3] = x;
		x = eb[8 - 4];
		bs[6] = xs;
		xs = es[8];
		b[4] = x;
		x = eb[8 - 5];
		bs[8] = xs;
		xs = es[10];
		b[5] = x;
		x = eb[8 - 6];
		bs[10] = xs;
		xs = es[12];
		b[6] = x;
		x = eb[8 - 7];
		bs[12] = xs;
		xs = es[14];
		b[7] = x;
		bs[14] = xs;
	}
}

/*
 *	Single bit error:
 *
 *	  This causes a single bit correction for every bit in the passed
 *	interval. For each word tested, one single bit error interrupt will
 *	will be caused. To prevent nasty behavior contributed by adjacent
 *	but unused rows, the count should be a multiple of 2.
 */
static int SingleBitError(m,slt,c)
register long *m;
int slt,c;
{
  /*
   *	Locals.
   */
	register int i,j;
	register long val,tmp;
	register short tcr;
	register TestVar *tv = (TestVar *) TestVarBase;
	static char Dsyn[32] = {
		0060,0065,0055,0053,0050,0047,0044,0042,
		0134,0132,0131,0126,0125,0123,0116,0113,
		0161,0164,0154,0152,0151,0146,0145,0143,
		0035,0033,0030,0027,0024,0022,0017,0012
	};
	static char Csyn[7] = {
		0176,0175,0173,0167,0157,0137,0077
	};
	/*
	 *	Set the boards interrupt level to 3. Force words to be tested to
	 *	a known state.
	 */
	tcr = MCR_EN_DBLECC | MCR_INTLEVEL_0B | MCR_EN_MEMVME;
	for (i = c; (i -= 2) > 0;){
		m[i + 0] = 0;
		m[i + 1] = 0;
	}
  /*
   *	Run through all words in the region.
   */
	for (i = c; i--;){
		register short sr;
		m[i] = val = tv->BitArray[i & 0x7f];
	/*
	 *	Cycle through all the data bits. Start by clearing the any
	 *	previous errors.
	 */
		for (j = 32; j--;){
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB));
		/*
		 *	Generate an error and see if the value gets corrected.
		 */
			m[i] = val ^ (1 << j);
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));

			/* Read the location */
			tmp = m[i];

			sr = GetMemIoReg(MEM_STAT,slt);
			if (val != tmp || M2K_MsrSynd(sr) != Dsyn[j]){
				goto Failed;
			}
	/*
	 *	Now see if the memory board realized it had an error.
	 */
			if ((long) &m[i] & MEM_ROW){		/* ODD */
				if ((sr & MSR_DBL_BIT_ERR) || !(sr &
				    MSR_SIN_BIT_ERR) || !(sr & MSR_VME_ERR)
				    || !(sr & MSR_BANK_DEC_0) || (M2K_MsrBank(sr) != (~tv->BankNumber & 0x3))){
				goto Failed;
			}
			} else {
				/* EVEN */
				if ((sr & MSR_DBL_BIT_ERR) || !(sr & MSR_SIN_BIT_ERR) ||
				!(sr & MSR_VME_ERR) || (sr & MSR_BANK_DEC_0) ||
				  (M2K_MsrBank(sr) != (~tv->BankNumber & 0x3))){
				  goto Failed;
				}
			}
		}
    /*
     *	Now do the same thing as the previous loop except that we now
     *	move through the check bits.
     */
		for (j = 7; j--;){
			m[i] = val;
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_DAB));
      /*
       *	Generate the error and look for correct data.
       */
			m[i] = tv->BitArray[(i & 0x7f) ^ (1 << j)];
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));

			/* Read the location */
			tmp = m[i];

			sr = GetMemIoReg(MEM_STAT,slt);
			if (val != tmp || M2K_MsrSynd(sr) != Csyn[j]){
				goto Failed;
			}
      /*
       *	And make sure the memory board saw the error.
       */
			if ((long) &m[i] & MEM_ROW){		/* ODD */
				if ((sr & MSR_DBL_BIT_ERR) || !(sr & MSR_SIN_BIT_ERR) ||
				    !(sr & MSR_VME_ERR) || !(sr & MSR_BANK_DEC_0) ||
				    (M2K_MsrBank(sr) != (~tv->BankNumber & 0x3))){
						goto Failed;
					}
			}
			else {			/* EVEN */
				if ((sr & MSR_DBL_BIT_ERR) || !(sr & MSR_SIN_BIT_ERR) ||
				    !(sr & MSR_VME_ERR) || (sr & MSR_BANK_DEC_0) ||
				    (M2K_MsrBank(sr) != (~tv->BankNumber & 0x3))){
					goto Failed;
					}
				}
			}
    /*
     *	Make sure a single bit error causes an interrupt when the option
     *	is enabled.
     */
			M2kIV(slt) = 0xdc;
			m[i] = val;
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB));
			m[i] = val ^ 1;
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN | MCR_EN_SINECC));
			if (m[i] != val){	/* should INT */
				goto Failed;
			}
			IntWait(10);
			PutMemIoReg(MEM_CNTRL,slt,tcr);
			if (!(tv->VmeIsr & M2K_VmeIrq3) || (tv->IAck[3] &0xff)!= 0xdc){
				goto Failed;
			}
			/*
			 *	Make sure the thing got scrubbed.
			 */
			PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
			if (m[i] != val){
				goto Failed;
			}
			sr = GetMemIoReg(MEM_STAT,slt);
			if (sr & MSR_SIN_BIT_ERR){
				goto Failed;
			}
		}
			return (0);

			Failed:

			return (1);
}

/*
 *	D bus error:
 *
 *	  This whacks on the CPU error register during a Data bus error.
 */
DBusError()
{
	/*
	 *	Locals.
	 */
	register TestVar *tv = (TestVar *) TestVarBase;
	register int i;
	/*
	 *	Clear the interrupt structure in case nothing happens.
	 */
	tv->VmeIsr = 0;
	for (i = 8; i--;)
		tv->IAck[i] = 0;
	/*
	 *	Get cause and fiddle with the other registers.
	 */
	tv->Cause = GetCause() & 0x0000ffff;
	tv->ErrAdr = *(long *) PHYS_TO_K1(SBE_ADDR);
	tv->CpuIsr = *(unsigned char *) PHYS_TO_K1(ESR);
	*((long *) PHYS_TO_K1(SBE_ADDR)) = 0;
#ifdef	DEBUG
		pon_puts("Bus Error: Cause ");
		pon_puthex(tv->Cause);
		pon_puts(" ErrAdr ");
		pon_puthex(tv->ErrAdr);
		pon_puts(" \n\r");
#endif	DEBUG
}

/*
 *	Double bit error:
 *
 *	  This does double bit error testing on the block of memory specified.
 *	Since there are so many combinations of possible double bit errors
 *	(528 kinds) that only one is processed per call. A data and check mask
 *	are passed to select the bits to fault. The region should be given in
 *	a multiple of 2 words.
 */
static int DoubleBitError(m,slt,c,dm,cm)
register volatile long *m;
int slt,c;
register long dm;
register unsigned char cm;
{
	/*
	 *	Locals.
	 */
	register int i;
	register long val;
	register short tcr,tsr;
	register TestVar *tv = (TestVar *) TestVarBase;
	/*
	 *	Set up an interrupt handler for looking at the error through the bus
	 *	error mechanism. Also put the board on VME level 3.
	 */
	tcr = MCR_INTLEVEL_1B | MCR_EN_MEMVME;
	/*
	 *	Install the bus error handler, initialize the region.
	 */
	for (i = c; (i -= 2) >= 0;){
		m[i + 0] = 0;
		m[i + 1] = 0;
	}
  /*
   *	Run through the region, create a double bit error and make sure that
   *	it is caught by a bus error.
   */
	for (i = c; i--;){
		m[i] = val = tv->BitArray[i & 0x7f];
		/*
		 *	Add error on data side.
		 */
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB));
		m[i] = val ^ dm;
		/*
		 *	Add error on check side.
		 */
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN | MCR_INWR_DAB));
		m[i] = tv->BitArray[(i ^ cm) & 0x7f];
		PutMemIoReg(MEM_CNTRL,slt,tcr);
	/*
	 *Read the data, and cross your fingers the int will get there before
	 *the check code.
	 */
		nofault = &fault_buf[0];

		if (setjmp(fault_buf)){
			DBusError();
		}
		else{
			m[i]++;
			wbflush();
		}
		if (!(tv->CpuIsr & M2K_IsrRdTimOt) || (tv->CpuIsr &
		    M2K_IsrRdBusEr) || !(tv->CpuIsr & M2K_IsrWrTimOt) || !(
		    tv->CpuIsr & M2K_IsrWrBusEr) || !(tv->CpuIsr & M2K_IsrRdProEr)){
			goto Failed;
		}
	}
	/*
	 *Do the same trick as the previous loop, just look for the VME int.
	*/
	for (i = c; i--;){
		m[i] = val = tv->BitArray[i & 0x7f];
		/*
		 *	Add error on data side.
		 */
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB | MCR_EN_SYN));
		m[i] = val ^ dm;
		/*
		 *	Add error on check side.
		 */
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_DAB));
		m[i] = tv->BitArray[(i ^ cm) & 0x7f];
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN | MCR_EN_DBLECC));
		/*
		 *Read the data, and cross your fingers the int will get there
		 * before the check code.
		*/
		nofault = &fault_buf[0];

		if (setjmp(fault_buf)){
			DBusError();
		}
		else{
			m[i]++;
		}
		IntWait(10);
		if (!(tv->VmeIsr & M2K_VmeIrq1)){ /* check for the int */
			goto Failed;
		}
	}
/*
 *	Create a double bit error, cause the error to be detected on a partial
 *	write on the private side.
 */
	for (i = c; i--;){
		register volatile long *dmy = &m[i ^ 1];
		m[i] = val = tv->BitArray[i & 0x7f];
		/*
		 *	Put in the double bit error.
		*/
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN | MCR_INWR_CKB));
		m[i] = val ^ dm;
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_DAB));
		m[i] = tv->BitArray[(i ^ cm) & 0x7f];
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
		/*
		 *	Verify the error (in a stream).
		 */
		wbflush();
		*dmy = 0;
		*((char *) &m[i]) = 0;
		*dmy = 0;
		wbflush();
		IntWait(10);
		if (!(tv->Cause & CAUSE_IP8) && (tv->Cause & CAUSE_IP3)){
			goto Failed;
		}
		if (tv->ErrAdr != K1_TO_PHYS(&m[i])){
			goto Failed;
		}
		if (!(tv->CpuIsr & M2K_IsrRdTimOt) || !(tv->CpuIsr & M2K_IsrRdBusEr) ||
		    !(tv->CpuIsr & M2K_IsrWrTimOt) || (tv->CpuIsr & M2K_IsrWrBusEr) ||
		    !(tv->CpuIsr & M2K_IsrRdProEr)){
			goto Failed;
		}
	}
/*
 *	Disable the bus error int. Look for the memory int.
 */
	SetSR(GetSR() & ~SR_IBIT8);
/*
 *	Do the same double bit silliness but look for an INT response.
 */
	for (i = c; i--;){
		register volatile long *dmy = &m[i ^ 1];
		m[i] = val = tv->BitArray[i & 0x7f];
		/*
		 *	Patch in the error.
		 */
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB | MCR_EN_SYN));
		m[i] = val ^ dm;
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_DAB));
		m[i] = tv->BitArray[(i ^ cm) & 0x7f];
		PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN | MCR_EN_DBLECC));
		/*
		 *Read the data, and cross your fingers the int will get there
		 *before the check code.
		 */
		wbflush();
		*dmy = 0;
		*((char *) &m[i]) = 0;
		*dmy = 0;
		wbflush();
		IntWait(10);
		if (tv->ErrAdr != K1_TO_PHYS(&m[i])){
			goto Failed;
		}
		if (!(tv->VmeIsr & M2K_VmeIrq1)) { /* check for the int */
			goto Failed;
		}
	}
	/*
	 *	Clean up and go home.
	 */
	SetSR(GetSR() | SR_IBIT8);
	nofault = 0;
	return (0);
	Failed:
	nofault = 0;
	return (1);
}

/*
 *	Block ECC:
 *
 *	  This fills a data and instruction block with single bit errors.
 *	It makes sure that the data gets properly loaded, corrected and
 *	scrubbed.
 */
static int BlockEcc(m,slt)
register long *m;
int slt;
{
	/*
	 *	Locals.
	 */
	register int i;
	register short tcr,tsr;
	/*
	 *	Initialize the memory registers.
	 */
	tcr = MCR_EN_DBLECC | MCR_INTLEVEL_0B | MCR_EN_MEMVME;
	/*
	 *	Fill a VME block with data (every word has a single bit error).
	 */
	PutMemIoReg(MEM_CNTRL,slt,tcr);
	m = (long *) K0_TO_K1(m);
	for (i = 2 * DBLOCKSIZE; i--;)
		m[i] = i;
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB));
	for (i = 2 * DBLOCKSIZE; i--;)
		m[i] = 0x8000 ^ i;
	/*
	 *	Write bytes through the VME to do RWM scrubs.
	 */
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
	for (i = 2 * DBLOCKSIZE; i--;){
		register char *b = (char *) PHYS_TO_VME(&m[i]);
		b[0] = 0;
	}
	tsr = GetMemIoReg(MEM_STAT,slt);
	if ((tsr & MSR_DBL_BIT_ERR) || !(tsr & MSR_SIN_BIT_ERR) || (tsr & MSR_VME_ERR))
		goto Failed;
	/*
	 *	Validate on the private side.
	 */
	PutMemIoReg(MEM_CNTRL,slt,tcr);
	m = (long *) K1_TO_K0(m);
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
	for (i = 2 * DBLOCKSIZE; i--;)
		if (m[i] != i)
			goto Failed;
	tsr = GetMemIoReg(MEM_STAT,slt);
	if ((tsr & MSR_DBL_BIT_ERR) || (tsr & MSR_SIN_BIT_ERR))
		goto Failed;
	/*
	 *	Fill the block with data (everybody has a single bit error).
	 */
	PutMemIoReg(MEM_CNTRL,slt,tcr);
	m = (long *) K0_TO_K1(m);
	for (i = DBLOCKSIZE; i--;)
		m[i] = (1 << i);
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_INWR_CKB));
	for (i = DBLOCKSIZE; i--;)
		m[i] ^= (1 << (DBLOCKSIZE - 1 - i));
	/*
	 *	In validate the first word, confirm the data got corrected.
	 */
	m = (long *) K1_TO_K0(m);
	InvalidateWord(&m[DBLOCKSIZE - 1]);
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
	for (i = DBLOCKSIZE; i--;)
		if (m[i] != (1 << i))
			goto Failed;
	tsr = GetMemIoReg(MEM_STAT,slt);
	if ((tsr & MSR_DBL_BIT_ERR) || !(tsr & MSR_SIN_BIT_ERR) || !(tsr & MSR_VME_ERR))
	 goto Failed;
	/*
	 *	See that the data got scrubbed.
	 */
	PutMemIoReg(MEM_CNTRL,slt,tcr);
	m = (long *) K0_TO_K1(m);
	PutMemIoReg(MEM_CNTRL,slt,(tcr | MCR_EN_SYN));
	for (i = DBLOCKSIZE; i--;)
		if (m[i] != (1 << i))
			goto Failed;
	tsr = GetMemIoReg(MEM_STAT,slt);
	if ((tsr & MSR_DBL_BIT_ERR) || (tsr & MSR_SIN_BIT_ERR))
		goto Failed;
	return (0);
	Failed:
	return (1);
}

/*
 *	ECC PON:
 *
 *	  Well here we are in the cold, cruel world. Look at the each slot
 *	and if there is a memory board there set it up and run the ECC tests.
 */
ECCPon()
{

	register TestVar *tv;
	register int i,k,a,b;
	register volatile char *imr,brd_count;
	register volatile u_short ar,memboard_size;
	register volatile u_short data;
	unsigned long m,sr;

	/*
	 * If machine_type is not M2000, exit without executing the test.
	 */
	if ((machine_type != BRDTYPE_R3200) && (machine_type != BRDTYPE_RB3125))
	{
		return(PASS);
	}

	pon_set_leds(PON_ECC);
	pon_puts("ECC Test...");

	if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)) {
		pon_puts(skipped);
		goto norun;
	}

	/*
	 *	Clear the test variable structure.
	 */

	tv = (TestVar *) TestVarBase;
	ClearBlock(tv,sizeof(TestVar));

	/*
	 *	Build the bit array in the test structure.
	*/

	for (i = 128; i--;)
	  tv->BitArray[i] = BitArray[i];

	/*
	 *	Save the status register, disable all INTs (we poll).
	 */

	sr = GetSR();
	SetSR((sr & ~SR_IMASK));

	/*
	 *	Save the old VME interrupt mask register. Enable useful ints.
	 */

	imr = (char*)PHYS_TO_K1(VME_IMR);
	*imr |= 0x1a;	/* 7,3,1 */

	/* Initialize the slot array */

	for(i=0; i<8; i++)
		f_slot[i] = 0xff;

	/*
	 *	determine board count
	 *
	 */

	brd_count = dmemconfig();

	/*
	 *	Fun times, save the address register and make sure the private
	 *	and VME bases are identical except for the bus selector bit.
	 */

	for(i=0; i < brd_count; i++){
		if(f_slot[i] == 0xff || f_slot[i] == 0)
			continue;

	/* Save the present Memory Board address register */
		ar = GetMemIoReg(MEM_ADDR,f_slot[i]);

	/* create new value for the address register */
		data = (ar & 0xff00) | ((ar >> 8) ^ 0x10);

	/* write new value */
		PutMemIoReg(MEM_ADDR,f_slot[i],data);

	/*
	 *	Form a loop to hammer on each of the banks in the current
	 *	board.
	 */

	/* Read Memory board ID PROM */
		memboard_size = GetMemIoReg(MEM_PROMID,f_slot[i]);

/* Assumming a 32 Meg board temporarily */
		k = ((memboard_size & SIZE_MASK) == SIZE_32MEG) ? 4 : 2;
		while (k--){
			m = ((long) (ar & 0xff00) << 16) | WorkSpace;

      /*
       * This wiggles all bits of ECC in the current bank. The
       * way it works requires that it be run twice, the starting
       * point offset by a word so that the high/low rows get
       * get properly exercised. It can only fail if we get an
       * unexpected exception.
       */
		      EccDataBit(m,f_slot[i],sizeof(BitArray) / sizeof(long));
		      EccDataBit(m + sizeof(long),f_slot[i],sizeof(BitArray) / sizeof(long));
      /*
       * The single bit error can be run over as many words as
       * is desired by 2 is the minimum to get both the high/low
       * rows tested.
       */
		      if (SingleBitError(m,f_slot[i],2)){
			goto Failed;
		      }
      /*
       * There are lots of clases of double bit errors. The following
       * loops should check every type. Again the check span must be
       * at least 2 for high/low row coverage.
       */
		      for (a = 32; a--;)
			for (b = a; b--;){
			  if (DoubleBitError(m,f_slot[i],2,(1L << a) | (1L << b),0)){
			    goto Failed;
			  }
			}
		      for (a = 32; a--;)
			for (b = 7; b--;){
			  if (DoubleBitError(m,f_slot[i],2,1L << a,1L << b)){
			    goto Failed;
			  }
			}
		      for (a = 7; a--;)
			for (b = a; b--;){
			  if (DoubleBitError(m,f_slot[i],2,0,(1 << a) | (1 << b))){
			    goto Failed;
			  }
			}
		}
	/*
	* Restore the stuff we trashed.
	*/
		PutMemIoReg(MEM_ADDR,f_slot[i],ar);
	}
  /*
   *	Pass back the return frame.
   */
	  SetSR(sr);
	  *imr &= ~0x1a;
	  pon_puts(success);
	  return(PASS);

	Failed:
	  PutMemIoReg(MEM_ADDR,f_slot[i],ar);
	  SetSR(sr);
	  *imr &= ~0x1a;
	  pon_puts(failure);
	  FastFlash(PON_ECC);
	  pon_set_leds(PON_ECC);
norun:
	  SetDepend(PON_FAULT_ECC);
	  return(FAIL);
}
