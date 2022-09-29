/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */

#ident	"$Header: mem_r3030.c,v 1.3.1.7 90/05/09 15:04:58 wje Exp $"

/*
 * the memory system on the RX3030 is different to all other machines
 * currently in the MIPS lineup. We have to firstly determine whether
 * we have 4MB simms of 1MB simms, and then we can size memory to see
 * how much has been inserted. If we have as a baseline, 4MB simms we
 * will also check to see if a set of 1MB simms are inserted at the
 * beginning of the next memory slice. We use an Intel 8042 controller
 * to hold the size of the simms.
 */

#include "sys/param.h"
#include "sys/cmn_err.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"

#define chk_pattern(pt, p) \
	{ \
	if (*p != pt) \
		cmn_err(CE_WARN, \
		"Error on writing pattern 0x%x to location 0x%x\n", \
		pt, p); \
	}
/*
 * the basic strategy we follow is this:
 *
 * Assume we have 4MB simms. If we do then we have atleast 32MB of
 * memory. If we don't then we have atleast 8MB of memory. So, if
 * we have 1MB simms, but we assume we have 4MB simms then the 8MB
 * is replicated 4 times, once at 0MB, once at 8MB, once at 16, and
 * once at 24MB.
 *
 * So, set up for 4MB simms, and see if memory is replicated. If it
 * is then we have 1MB simms, if not we have 4MB simms.
 *
 * After setting up whether we have 1 or 4 MB simms we then step through
 * memory to see how many simms we have. If we are checking 4MB simms we
 * also check to see if we have one set of 1MB simms inserted after the
 * 4MB simms.
 * This allows The following valid memory configurations:
 *
 * Size of Memory	# 4MB simms	# 1 MB simms
 *	  8 MB			-		8
 *	 16 MB			-		16
 *	 24 MB			-		24
 *	 32 MB			-		32
 *	 32 MB			8		-
 *	 40 MB			8		8
 *	 64 MB			16		-
 *	 72 MB			16		8
 *	 96 MB			24		-
 *	104 MB			24		8
 *	128 MB			32		-
 *
 */

#define	pattern1	0x55aaff00	/* patterns for memory testing */
#define	pattern2	0xc33ca55a
#define	pattern3	0x11111111
#define	pattern4	0x44444444
#define	pattern5	0xff00cc33

int		simms_type;
static	int	rambo_parity;
extern	int	showconfig;

size_r3030_memory(max_bytes)
long	max_bytes;
{
	register int			mb;	/* size of memory currently */
	register unsigned long		t0, t1, t2, t3, t4;	/* temps */
	register int			step;		/* increment */
	register volatile unsigned long *p1, *p2;	/* pointers for steps */
	int				s;		/* spl holder */
	register volatile struct rambo *rambo = ((struct rambo *)PHYS_TO_K1(RAMBO_BASE));

	s = splhi();
	rambo_parity = rambo->rambo_creg;
	rambo->rambo_creg = rambo_parity & ~ENABLEPARITY;
	wbflush();
	if (pkbdset4() == 0) {		/* try to set up 4MB simms in I8042 */
		/*
		 * keyboard set up failed, assume 8mb anyway,
		 * that will work nomatter what.
		 */

		cmn_err(CE_WARN, "Keyboard Controller Failed Interaction, assuming 8Mb\n");
		return (0x800000 /  NBPC);
	}
	p1 = (volatile unsigned long *)PHYS_TO_K1((unsigned)0);
	p2 = (volatile unsigned long *)PHYS_TO_K1((unsigned)0x800000);
	t1 = *p1;
	t2 = *p2;
	*p1 = pattern1;
	chk_pattern(pattern1, p1);
	*(p1 + 1) = pattern3;
	chk_pattern(pattern3, (p1 + 1));
	*p2 = pattern2;
	chk_pattern(pattern2, p2);
	*(p2 + 1) = pattern4;
	chk_pattern(pattern4, (p2 + 1));
	if ((pattern1 == *p1) && (pattern2 == *p2) &&
	    (pattern3 == *(p1 + 1)) && (pattern4 == *(p2 + 1))) {
		/*
		 * we have 4MB simms installed
		 */
		simms_type = P_4SIMMS;
		mb = step = 0x2000000;
	} else if ((pattern2 == *p1) && (pattern2 == *p2) &&
	    (pattern4 == *(p1 + 1)) && (pattern4 == *(p2 + 1))) {
		/*
		 * we have 1MB simms installed
		 */
		simms_type = 0;
		mb = step = 0x800000;
		if (pkbdset1() == 0) {		/* set up 1MB simms in I8042 */
			cmn_err(CE_WARN, "Keyboard Controller Failed Interaction, assuming 8Mb\n");
			return (0x800000 /  NBPC);
		}
	} else {
		cmn_err(CE_PANIC,
			"Memory failure -\n0: (%x) != (%x)\n4: (%x) != (%x)\n",
			*p1, *p2, *(p1 + 1), *(p2 + 1));
	}
	*p2 = t2;	/* restore */
	*p1 = t1;

	/*
	 * we now step through memory looking at the start of blocks
	 * to see if they are there. If not, we assume that we have exceeded
	 * memory size. Step is the size of memory chunks.
	 */
	p1 = (volatile unsigned long *)PHYS_TO_K1(step);
	p2 = (volatile unsigned long *)PHYS_TO_K1(0);
	t0 = *p2;	/* save location 0 */
	for(; mb < max_bytes; mb += step) {
		if (showconfig)
			printf("Currently we have 0x%x bytes\n", mb);
		t1 = *p1;
		t2 = *(p1 + 1);
		t3 = *(p1 + 2);
		t4 = *(p1 + 3);
		*p1       = pattern1;
		chk_pattern(pattern1, p1);
		*(p1 + 1) = pattern2;
		chk_pattern(pattern2, (p1 + 1));
		*(p1 + 2) = pattern3;
		chk_pattern(pattern3, (p1 + 2));
		*(p1 + 3) = pattern4;
		chk_pattern(pattern4, (p1 + 3));
		/*
		 * we write to a well defined location to ensure that
		 * the data buss is not holding the last value by
		 * parasitic capacitance.
		 */
		*p2 = pattern5;
		chk_pattern(pattern5, p2);
		wbflush();
		if ((*p1 != pattern1) ||
		    (*(p1 + 1) != pattern2) ||
		    (*(p1 + 2) != pattern3) ||
		    (*(p1 + 3) != pattern4) ||
		    (*p2 != pattern5))
			break;	/* at end of memory */
		*p1       = t1;
		*(p1 + 1) = t2;
		*(p1 + 2) = t3;
		*(p1 + 3) = t4;
		/*
		 * yes, there is a bank here, if we are checking 4Mb simms
		 * this might be the one allowable 1Mb bank, check if this is
		 * so, and stop if it is. (We only allow contiguous memory
		 * currently, this will change shortly)
		 */
		if (step == 0x2000000) {
			register volatile unsigned long *p3;

			p3 = p1 + (0x800000 / sizeof(unsigned long));
			t3 = *p3;
			t1 = *p1;
			if (t3 == t1) {
				/*
				 * if this fails then we know we have a 4Mb
				 * bank installed here. It this succeeds then
				 * we have to do more testing!
				 */
				t2 = *(p1 + 1);	/* save the others too */
				t4 = *(p3 + 1);
				*p1 = pattern1;
				chk_pattern(pattern1, p1);
				*(p1 + 1) = pattern2;
				chk_pattern(pattern2, (p1 + 1));
				*p3 = pattern3;
				chk_pattern(pattern3, p3);
				*(p3 + 1) = pattern4;
				chk_pattern(pattern4, (p3 + 1));
				*p2 = pattern5;
				chk_pattern(pattern5, p2);
				wbflush();
				if ((*p1 != pattern1) ||
				    (*(p1 + 1) != pattern2) ||
				    (*p3 != pattern3) ||
				    (*(p3 + 1) != pattern4) ||
				    (*p2 != pattern5)) {
					/*
					 * a failure has occured, we assume
					 * that there was no memory here, or
					 * if there was it is faulty. we
					 * therefore assume that this bank was
					 * a 1Mb bank and be done with it!
					 */
					mb += 0x800000;
					*p1 = t1;
					*(p1 + 1) = t2;
					*p3 = t3;
					*(p3 + 1) = t4;
					break;
				}
				*p1 = t1;
				*(p1 + 1) = t2;
				*p3 = t3;
				*(p3 + 1) = t4;
			}
		}
		p1 += (step / sizeof(unsigned long));
	}
	*p2 = t2;
	if (showconfig)
		printf("Total Memory is: 0x%x bytes\n", mb);
	rambo->rambo_creg = rambo_parity | RESETERROR;
	rambo->rambo_creg = rambo_parity & ~(RESETERROR | CLRERRINT);
	rambo->rambo_creg = rambo_parity | CLRERRINT;
	splx(s);
	return (mb / NBPC);
}
