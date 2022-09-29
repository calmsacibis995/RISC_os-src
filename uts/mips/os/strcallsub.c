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
#ident	"$Header: strcallsub.c,v 1.7.1.2 90/05/10 05:54:09 wje Exp $"

/* stream system call support functions
 * 
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/os/RCS/strcallsub.c,v $
 * $Revision: 1.7.1.2 $
 * $Date: 90/05/10 05:54:09 $
 */

/* Original includes:
 * "../h/param.h" "../h/types.h" "../h/file.h" "../h/inode.h"
 * "../h/proc.h" "../h/user.h" "../streams/stream.h"
 * "../streams/stropts.h" "../streams/poll.h" "ustrm.h" "tcp.h"
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/cmn_err.h"

#if NUSTRM > 0 || NTCP > 0
/*
 * Removes all event cells that refer to the current process in the
 * given stream's poll list.
 */
pollreset(stp)
register struct stdata *stp;
{
	register struct strevent *psep, *sep, *tmp;
	register int s;

	s = splstr();
	sep = stp->sd_pollist;
	psep = NULL;
	while (sep) {
		tmp = sep->se_next;
		if (sep->se_procp == u.u_procp) {
			if (psep)
				psep->se_next = tmp;
			else
				stp->sd_pollist = tmp;
			sefree(sep);
		} else {
			psep = sep;
		} 
		if ( sep == tmp ) {
			cmn_err(CE_WARN,"pollreset looping strevent\n");
			sep->se_next = 0;
			break;
		}
		sep = tmp;
	}
	/*
	 * Recalculate pollflags
	 */
	stp->sd_pollflags = 0;
	for (sep = stp->sd_pollist; sep; sep = sep->se_next)
		stp->sd_pollflags |= sep->se_events;
	splx(s);
}
#endif
