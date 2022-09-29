/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: r3200mem.h,v 1.1.4.2 90/05/10 06:33:56 wje Exp $ */

/*
 * r3200mem.h -- definitions for R3200 system (M2000 system) memory boards
 *		ECC handler (driver) structures
 */

#define	R3200_MEM_MAX	4	/* max 4 boards per M2000 system */
#define	R3200_PBUS_MAX	5	/* max 5 private bus slots on R3200 */

#define	R3250VEC	0x20	/* ECC interrupt vector - for all boards */

struct lmem_cntrl {
	int	slot;	/* slot loc (number) on memory bus (-1 is not present) */
	char	*base;	/* base addr board is configured for on private bus */
	char	*vmebase;	/* base addr configured for on vme bus */
	int	errtime;	/* time (lbolt) of last error		*/
	int	errcount;	/* count of logged errors for the board	*/
	ushort	configured;	/* nonzero if board has been configured */
	ushort	leds;		/* low byte is board LEDs		*/
	ushort	board_type;	/* board type byte from id prom		*/
	ushort	board_size;	/* board size byte from id prom		*/
	char	board_sn[6];	/* board serial # (plus NULL) from id prom */
};
