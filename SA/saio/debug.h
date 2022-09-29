#ident "$Header: debug.h,v 1.9 90/03/02 14:31:27 menna Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * debug.h -- definition of standalone debugging flags
 */

#define	YES	1
#define	NO	0

#define	DBG_PROTOCOL	0x1		/* serial line protocol */
#define	DBG_RMTDBG	0x2		/* remote debug protocol */
#define	DBG_PROTOOOB	0x4		/* print oob protocol input */
#define	DBG_BFS		0x8		/* debug bfs protocol */

/*
 * We are going to steal some bits here for XPRINTF emulation.  They
 * differ in that output ONLY goes to console, not any buffer.  Just
 * 'or' these bits into the DEBUG environment variable.
 */

#define XPR_ENET	0x10		/* lance driver */
#define XPR_SCSI	0x20		/* scsi driver */
#define XPR_MEM		0x40		/* memory related events */
#define XPR_TOD		0x80		/* tod related */
#define XPR_SLOAD	0x100		/* sload */
#define XPR_XXX		0x200		/* extra */

#ifdef LANGUAGE_C

#ifdef XPR_DEBUG

extern int Debug;

#define XPR1(flags, arg0) \
	if (Debug & flags) printf(arg0);
#define XPR2(flags, arg0, arg1) \
	if (Debug & flags) printf(arg0, arg1);
#define XPR3(flags, arg0, arg1, arg2) \
	if (Debug & flags) printf(arg0, arg1, arg2);
#define XPR4(flags, arg0, arg1, arg2, arg3) \
	if (Debug & flags) printf(arg0, arg1, arg2, arg3);
#define XPR5(flags, arg0, arg1, arg2, arg3, arg4) \
	if (Debug & flags) printf(arg0, arg1, arg2, arg3, arg4);
#define XPRINTF		XPR5		/* Synonym for old way */

#else

#define XPR1(flags, arg0)
#define XPR2(flags, arg0, arg1)
#define XPR3(flags, arg0, arg1, arg2)
#define XPR4(flags, arg0, arg1, arg2, arg3)
#define XPR5(flags, arg0, arg1, arg2, arg3, arg4)
#define XPRINTF		XPR5		/* Synonym for old way */

#endif XPR_DEBUG

#endif LANGUAGE_C
