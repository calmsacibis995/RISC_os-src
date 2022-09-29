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
/* $Header: if_imp.h,v 1.7.1.2 90/05/10 04:45:14 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_imp.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Structure of IMP 1822 long leader.
 */
struct bsd43_(control_leader) {
	u_char	dl_format;	/* leader format */
	u_char	dl_network;	/* src/dest network */
	u_char	dl_flags;	/* leader flags */
	u_char	dl_mtype;	/* message type */
	u_char	dl_htype;	/* handling type */
	u_char	dl_host;	/* host number */
	u_short	dl_imp;		/* imp field */
	u_char	dl_link;	/* link number */
	u_char	dl_subtype;	/* message subtype */
};

struct bsd43_(imp_leader) {
	struct	bsd43_(control_leader) il_dl;
#define	bsd43_il_format	il_dl.dl_format
#define	bsd43_il_network	il_dl.dl_network
#define	bsd43_il_flags	il_dl.dl_flags
#define	bsd43_il_mtype	il_dl.dl_mtype
#define	bsd43_il_htype	il_dl.dl_htype
#define	bsd43_il_host		il_dl.dl_host
#define	bsd43_il_imp		il_dl.dl_imp
#define	bsd43_il_link		il_dl.dl_link
#define	bsd43_il_subtype	il_dl.dl_subtype
	u_short	il_length;	/* message length */
};

#define	BSD43_IMP_DROPCNT	2	/* # of noops from imp to ignore */
/* insure things are even... */
#define	BSD43_IMPMTU		((8159 / BSD43_NBBY) & ~01)

/*
 * IMP-host flags
 */
#define	BSD43_IMP_NFF		0xf	/* 96-bit (new) format */
#define	BSD43_IMP_TRACE	0x8	/* trace message route */

#define	BSD43_IMP_DMASK	0x3	/* host going down mask */

/*
 * IMP-host message types.
 */
#define	BSD43_IMPTYPE_DATA		0	/* data for protocol */
#define	BSD43_IMPTYPE_BADLEADER	1	/* leader error */
#define	BSD43_IMPTYPE_DOWN		2	/* imp going down */
#define	BSD43_IMPTYPE_NOOP		4	/* noop seen during initialization */
#define	BSD43_IMPTYPE_RFNM		5	/* request for new messages */
#define	BSD43_IMPTYPE_HOSTDEAD	6	/* host doesn't respond */
#define	BSD43_IMPTYPE_HOSTUNREACH	7	/* host unreachable */
#define	BSD43_IMPTYPE_BADDATA		8	/* data error */
#define	BSD43_IMPTYPE_INCOMPLETE	9	/* incomplete message, send rest */
#define	BSD43_IMPTYPE_RESET		10	/* reset complete */
/* non-blocking IMP interface */
#define	BSD43_IMPTYPE_RETRY		11	/* IMP refused, try again */
#define	BSD43_IMPTYPE_NOTIFY		12	/* IMP refused, will notify */
#define	BSD43_IMPTYPE_TRYING		13	/* IMP refused, still rexmt'ng */
#define	BSD43_IMPTYPE_READY		14	/* ready for next message */

/*
 * IMPTYPE_DOWN subtypes.
 */
#define	BSD43_IMPDOWN_GOING		0	/* 30 secs */
#define	BSD43_IMPDOWN_PM		1	/* hardware PM */
#define	BSD43_IMPDOWN_RELOAD		2	/* software reload */
#define	BSD43_IMPDOWN_RESTART		3	/* emergency restart */

/*
 * IMPTYPE_BADLEADER subtypes.
 */
#define	BSD43_IMPLEADER_ERR		0	/* error flip-flop set */
#define	BSD43_IMPLEADER_SHORT		1	/* leader < 80 bits */
#define	BSD43_IMPLEADER_TYPE		2	/* illegal type field */
#define	BSD43_IMPLEADER_OPPOSITE	3	/* opposite leader type */

/*
 * IMPTYPE_HOSTDEAD subtypes.
 */
#define	BSD43_IMPHOST_NORDY		1	/* ready-line negated */
#define	BSD43_IMPHOST_TARDY		2	/* tardy receiving mesgs */
#define	BSD43_IMPHOST_NOEXIST		3	/* NCC doesn't know host */
#define	BSD43_IMPHOST_IMPSOFT		4	/* IMP software won't allow mesgs */
#define	BSD43_IMPHOST_PM		5	/* host down for scheduled PM */
#define	BSD43_IMPHOST_HARDSCHED	6	/* " " " " hardware work */
#define	BSD43_IMPHOST_SOFTSCHED	7	/* " " " " software work */
#define	BSD43_IMPHOST_RESTART		8	/* host down for emergency restart */
#define	BSD43_IMPHOST_POWER		9	/* down because of power outage */
#define	BSD43_IMPHOST_BREAKPOINT	10	/* host stopped at a breakpoint */
#define	BSD43_IMPHOST_HARDWARE	11	/* hardware failure */
#define	BSD43_IMPHOST_NOTUP		12	/* host not scheduled to be up */
/* 13-14 currently unused */
#define	BSD43_IMPHOST_COMINGUP	15	/* host in process of coming up */

/*
 * IMPTYPE_HOSTUNREACH subtypes.
 */
#define	BSD43_IMPREACH_IMP		0	/* destination IMP can't be reached */
#define	BSD43_IMPREACH_HOSTUP		1	/* destination host isn't up */
#define	BSD43_IMPREACH_LEADER		2	/* host doesn't support long leader */
#define	BSD43_IMPREACH_PROHIBITED	3	/* communication is prohibited */

/*
 * IMPTYPE_INCOMPLETE subtypes.
 */
#define	BSD43_IMPCOMPLETE_SLOW	0	/* host didn't take data fast enough */
#define	BSD43_IMPCOMPLETE_TOOLONG	1	/* message was too long */
#define	BSD43_IMPCOMPLETE_TIMEOUT	2	/* mesg transmission time > 15 sec. */
#define	BSD43_IMPCOMPLETE_FAILURE	3	/* IMP/circuit failure */
#define	BSD43_IMPCOMPLETE_NOSPACE	4	/* no resources within 15 sec. */
#define	BSD43_IMPCOMPLETE_IMPIO	5	/* src IMP I/O failure during receipt */

/*
 * IMPTYPE_RETRY subtypes.
 */
#define	BSD43_IMPRETRY_BUFFER		0	/* IMP buffer wasn't available */
#define	BSD43_IMPRETRY_BLOCK		1	/* connection block unavailable */

/*
 * Data structure shared between IMP protocol module and hardware
 * interface driver.  Used to allow layering of IMP routines on top
 * of varying device drivers.  NOTE: there's a possible problem 
 * with ambiguity in the ``unit'' definition which is implicitly
 * shared by the both IMP and device code.  If we have two IMPs,
 * with each on top of a device of the same unit, things won't work.
 * The assumption is if you've got multiple IMPs, then they all run
 * on top of the same type of device, or they must have different units.
 */
struct bsd43_(impcb) {
	char	ic_oactive;		/* output in progress */
	int	(*ic_init)();		/* hardware init routine */
	int	(*ic_start)();		/* hardware start output routine */
};

/*
 * State of an IMP.
 */
#define	BSD43_IMPS_DOWN	0		/* unavailable, don't use */
#define	BSD43_IMPS_GOINGDOWN	1		/* been told we go down soon */
#define	BSD43_IMPS_INIT	2		/* coming up */
#define	BSD43_IMPS_UP		3		/* ready to go */
#define	BSD43_IMPS_RESET	4		/* reset in progress */

#define	BSD43_IMPTV_DOWN	(30*60)		/* going down timer 30 secs */

#ifdef IMPLEADERS
char *impleaders[BSD43_IMPTYPE_READY+1] = {
	"DATA", "BADLEADER", "DOWN", "bad", "NOOP", "RFNM", "HOSTDEAD",
	"HOSTUNREACH", "BADDATA", "INCOMPLETE", "RESET", "RETRY",
	"NOTIFY", "TRYING", "READY"
};
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define IMPCOMPLETE_FAILURE BSD43_IMPCOMPLETE_FAILURE
#   define IMPCOMPLETE_IMPIO BSD43_IMPCOMPLETE_IMPIO
#   define IMPCOMPLETE_NOSPACE BSD43_IMPCOMPLETE_NOSPACE
#   define IMPCOMPLETE_SLOW BSD43_IMPCOMPLETE_SLOW
#   define IMPCOMPLETE_TIMEOUT BSD43_IMPCOMPLETE_TIMEOUT
#   define IMPCOMPLETE_TOOLONG BSD43_IMPCOMPLETE_TOOLONG
#   define IMPDOWN_GOING BSD43_IMPDOWN_GOING
#   define IMPDOWN_PM BSD43_IMPDOWN_PM
#   define IMPDOWN_RELOAD BSD43_IMPDOWN_RELOAD
#   define IMPDOWN_RESTART BSD43_IMPDOWN_RESTART
#   define IMPHOST_BREAKPOINT BSD43_IMPHOST_BREAKPOINT
#   define IMPHOST_COMINGUP BSD43_IMPHOST_COMINGUP
#   define IMPHOST_HARDSCHED BSD43_IMPHOST_HARDSCHED
#   define IMPHOST_HARDWARE BSD43_IMPHOST_HARDWARE
#   define IMPHOST_IMPSOFT BSD43_IMPHOST_IMPSOFT
#   define IMPHOST_NOEXIST BSD43_IMPHOST_NOEXIST
#   define IMPHOST_NORDY BSD43_IMPHOST_NORDY
#   define IMPHOST_NOTUP BSD43_IMPHOST_NOTUP
#   define IMPHOST_PM BSD43_IMPHOST_PM
#   define IMPHOST_POWER BSD43_IMPHOST_POWER
#   define IMPHOST_RESTART BSD43_IMPHOST_RESTART
#   define IMPHOST_SOFTSCHED BSD43_IMPHOST_SOFTSCHED
#   define IMPHOST_TARDY BSD43_IMPHOST_TARDY
#   define IMPLEADER_ERR BSD43_IMPLEADER_ERR
#   define IMPLEADER_OPPOSITE BSD43_IMPLEADER_OPPOSITE
#   define IMPLEADER_SHORT BSD43_IMPLEADER_SHORT
#   define IMPLEADER_TYPE BSD43_IMPLEADER_TYPE
#   define IMPMTU BSD43_IMPMTU
#   define IMPREACH_HOSTUP BSD43_IMPREACH_HOSTUP
#   define IMPREACH_IMP BSD43_IMPREACH_IMP
#   define IMPREACH_LEADER BSD43_IMPREACH_LEADER
#   define IMPREACH_PROHIBITED BSD43_IMPREACH_PROHIBITED
#   define IMPRETRY_BLOCK BSD43_IMPRETRY_BLOCK
#   define IMPRETRY_BUFFER BSD43_IMPRETRY_BUFFER
#   define IMPS_DOWN BSD43_IMPS_DOWN
#   define IMPS_GOINGDOWN BSD43_IMPS_GOINGDOWN
#   define IMPS_INIT BSD43_IMPS_INIT
#   define IMPS_RESET BSD43_IMPS_RESET
#   define IMPS_UP BSD43_IMPS_UP
#   define IMPTV_DOWN BSD43_IMPTV_DOWN
#   define IMPTYPE_BADDATA BSD43_IMPTYPE_BADDATA
#   define IMPTYPE_BADLEADER BSD43_IMPTYPE_BADLEADER
#   define IMPTYPE_DATA BSD43_IMPTYPE_DATA
#   define IMPTYPE_DOWN BSD43_IMPTYPE_DOWN
#   define IMPTYPE_HOSTDEAD BSD43_IMPTYPE_HOSTDEAD
#   define IMPTYPE_HOSTUNREACH BSD43_IMPTYPE_HOSTUNREACH
#   define IMPTYPE_INCOMPLETE BSD43_IMPTYPE_INCOMPLETE
#   define IMPTYPE_NOOP BSD43_IMPTYPE_NOOP
#   define IMPTYPE_NOTIFY BSD43_IMPTYPE_NOTIFY
#   define IMPTYPE_READY BSD43_IMPTYPE_READY
#   define IMPTYPE_RESET BSD43_IMPTYPE_RESET
#   define IMPTYPE_RETRY BSD43_IMPTYPE_RETRY
#   define IMPTYPE_RFNM BSD43_IMPTYPE_RFNM
#   define IMPTYPE_TRYING BSD43_IMPTYPE_TRYING
#   define IMP_DMASK BSD43_IMP_DMASK
#   define IMP_DROPCNT BSD43_IMP_DROPCNT
#   define IMP_NFF BSD43_IMP_NFF
#   define IMP_TRACE BSD43_IMP_TRACE
#   define il_flags bsd43_il_flags
#   define il_format bsd43_il_format
#   define il_host bsd43_il_host
#   define il_htype bsd43_il_htype
#   define il_imp bsd43_il_imp
#   define il_link bsd43_il_link
#   define il_mtype bsd43_il_mtype
#   define il_network bsd43_il_network
#   define il_subtype bsd43_il_subtype
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


