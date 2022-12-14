#ident "$Header: sysmacros.h,v 1.3 90/01/23 13:37:30 huang Exp $"
/* $Copyright$ */

/*
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:30 $
 * $State: Exp $	$Author: huang $
 * $Log:	sysmacros.h,v $
 * Revision 1.3  90/01/23  13:37:30  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:11:21  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  15:59:59  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  11:10:08  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Some macros for units conversion
 */

/* Core clicks to segments and vice versa */

#define ctos(x)		(((x) + (NCPS-1)) / NCPS)
#define	ctost(x)	((x) / NCPS)
#define	stoc(x)		((x) * NCPS)

/* byte address to segment and vice versa  */
#define sgnum(x)	(((unsigned)(x) >> 22) & 0x3ff)
#define stob(x)		((((unsigned)(x)) & 0x3ff) << 22)
#define	btos(x)		((unsigned)(x) >> 22)
#define secnum(x)	(@(unsigned)(x) >> 30)

/* Core clicks to immu max offset and vice versa */
#define ctomo(x)	(@(x) * 256 -1)
#define motoc(x)	(@(((x)+1)*8) >> BPCSHIFT)
#define motob(x)	(@8*((x)+1)-1)

/* Core clicks to disk blocks */
#define	ctod(x) ((x)*NDPC)

/* inumber to disk address */
#ifdef INOSHIFT
#define	itod(x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1))>>INOSHIFT)
#else
#define	itod(x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1))/INOPB)
#endif

/* inumber to disk offset */
#ifdef INOSHIFT
#define	itoo(x)	(int)(((unsigned)(x)+(2*INOPB-1))&(INOPB-1))
#else
#define	itoo(x)	(int)(((unsigned)(x)+(2*INOPB-1))%INOPB)
#endif

/* clicks to bytes */
#ifdef BPCSHIFT
#define	ctob(x)	((x)<<BPCSHIFT)
#else
#define	ctob(x)	((x)*NBPC)
#endif

#define btotp(x) ((x)>>BPTSHFT)

/* bytes to clicks */
#ifdef BPCSHIFT
#define	btoc(x)	(((unsigned)(x)+(NBPC-1))>>BPCSHIFT)
#define	btoct(x)	((unsigned)(x)>>BPCSHIFT)
#else
#define	btoc(x)	(((unsigned)(x)+(NBPC-1))/NBPC)
#define	btoct(x)	((unsigned)(x)/NBPC)
#endif

#ifdef INKERNEL

/* major part of a device internal to the kernel */

extern char MAJOR[128];
#define	major(x)	(int)(MAJOR[(unsigned)((x)>>8)&0x7F])
#define	bmajor(x)	(int)(MAJOR[(unsigned)((x)>>8)&0x7F])

/* minor part of a device internal to the kernel */
extern char MINOR[128];
#define	minor(x)	(int)(MINOR[(unsigned)((x)>>8)&0x7F]+((x)&0xFF))

#else

/* major part of a device external from the kernel */
#define	major(x)	(int)(((unsigned)x>>8)&0x7F)

/* minor part of a device external from the kernel */
#define	minor(x)	(int)(x&0xFF)
#endif	/* INKERNEL */

#define brdev(x)	(x&0x7fff)

/* make a device number */
#define	makedev(x,y)	(dev_t)(((x)<<8) | (y))

/*
 *   emajor() allows kernel/driver code to print external major numbers
 *   eminor() allows kernel/driver code to print external minor numbers
 */
#define emajor(x)	(int)(((unsigned)(x)>>8)&0x7F)
#define eminor(x)	(int)((x)&0xFF)


/*	Calculate user process priority.
*/

#define calcppri(p)	((p->p_cpu) >> 1) +  p->p_nice + (PUSER - NZERO)

#define min(x,y) ((x)<=(y)?(x):(y))
#define max(x,y) ((x)>=(y)?(x):(y))
