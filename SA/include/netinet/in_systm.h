#ident "$Header: in_systm.h,v 1.2 90/01/23 13:32:37 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*	in_systm.h	6.1	83/07/29	*/

/*
 * Miscellaneous internetwork
 * definitions for kernel.
 */

#ifndef LOCORE
/*
 * Network types.
 *
 * Internally the system keeps counters in the headers with the bytes
 * swapped so that VAX instructions will work on them.  It reverses
 * the bytes before transmission at each protocol level.  The n_ types
 * represent the types with the bytes in ``high-ender'' order.
 */
typedef u_short n_short;		/* short as received from the net */
typedef u_long	n_long;			/* long as received from the net */

typedef	u_long	n_time;			/* ms since 00:00 GMT, byte rev */
#endif

#ifndef LOCORE
#ifdef KERNEL
n_time	iptime();
#endif
#endif
