#ident "$Header: dbgmon.h,v 1.2 90/01/11 14:13:17 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * dbgmon.h -- debug monitor definitions
 */

/*
 * catch bogus compiles
 */
#if defined(MIPSEB) && defined(MIPSEL)
# include "error -- both MIPSEB and MIPSEL defined"
#endif

#if !defined(MIPSEB) && !defined(MIPSEL)
# include "error -- neither MIPSEB or MIPSEL defined"
#endif

/*
 * memory reference widths
 */
#define	SW_BYTE		1
#define	SW_HALFWORD	2
#define	SW_WORD		4

/*
 * Monitor modes
 */
#define	MODE_DBGMON	0	/* debug monitor is executing */
#define	MODE_CLIENT	1	/* client is executing */

/*
 * String constants
 */
#define	DEFAULT_STRLEN	70		/* default max strlen for string cmd */

/*
 * dbgmon stack top
 * dbgmon stack grow down from here toward dbgmon bss
 * (see prom/entrypt.h for description on standalone memory map)
 */
#define	DBGSTACKTOP	0xa0020000
