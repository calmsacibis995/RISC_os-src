#ident "$Header: param.h,v 1.4 90/01/23 14:19:07 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Machine dependent constants for mips.
 */
#define	NBPG		4096		/* bytes/page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	PGSHIFT		12		/* LOG2(NBPG) */
#define	DBSHIFT		9		/* LOG2(Disk block size) */

#define	CLSIZE		1
#define	CLSIZELOG2	0

#define	SSIZE		1		/* initial stack size/NBPG */
#define	SINCR		1		/* increment of stack/NBPG */

#define	UPAGES		2		/* pages of u-area */
#define	KERNELSTACK	0xffffe000	/* Top of kernel stack */
#define	UADDR		0xffffc000	/* address of u */
#define	UVPN		(UADDR>>PGSHIFT)/* virtual page number of u */

/*
 * Some macros for units conversion
 */
/* Core clicks (4096 bytes) to segments and vice versa */
#define	ctos(x)	(x)
#define	stoc(x)	(x)

/* Core clicks (4096 bytes) to disk blocks */
#define	ctod(x)	((x)<<(PGSHIFT-DBSHIFT))
#define	dtoc(x)	((unsigned)(x)>>(PGSHIFT-DBSHIFT))
#define	dtob(x)	((x)<<DBSHIFT)

/* clicks to bytes */
#define	ctob(x)	((x)<<PGSHIFT)

/* bytes to clicks */
#define	btoc(x)	(((unsigned)(x)+PGOFSET)>>PGSHIFT)

/*
 * Macros to decode processor status word.
 */
#define	USERMODE(sr)	(((sr) & SR_KUP) == SR_KUP)
#define	BASEPRI(sr)	(((sr) & SR_IMASK) == SR_IMASK)

/*
 * DELAY(n) should be n microseconds, roughly.  This is a first guess.
 */
#define DELAY(n)	{ \
	extern int delay_mult; \
	register int N = delay_mult*(n); \
	while (--N > 0); \
	}
