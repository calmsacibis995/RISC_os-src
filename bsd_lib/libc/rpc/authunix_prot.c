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
#ident	"$Header: authunix_prot.c,v 1.2.1.2 90/05/07 20:56:30 wje Exp $"
/*
 * @(#)authunix_prot.c 1.1 86/09/24 SMI
 *
 * authunix_prot.c
 * XDR for UNIX style authentication parameters for RPC
 *
 * Original kernel includes:
 * ../h/param.h ../h/systm.h ../h/user.h ../h/kernel.h ../h/proc.h
 * ../rpc/types.h ../rpc/xdr.h ../rpc/auth.h ../rpc/auth_unix.h
 */

#ifdef KERNEL
#include "sys/types.h"
#include "sys/param.h"
#include "sys/utsname.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"

#include "sys/errno.h"
#include "sys/systm.h"

#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include "../rpc/auth.h"
#include "../rpc/auth_unix.h"

#else KERNEL
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/auth.h>
#include <rpc/auth_unix.h>
#endif KERNEL

/*
 * XDR for unix authentication parameters.
 */
bool_t
xdr_authunix_parms(xdrs, p)
	register XDR *xdrs;
	register struct authunix_parms *p;
{

	if (xdr_u_long(xdrs, &(p->aup_time))
	    && xdr_string(xdrs, &(p->aup_machname), MAX_MACHINE_NAME)
	    && xdr_int(xdrs, &(p->aup_uid))
	    && xdr_int(xdrs, &(p->aup_gid))
	    && xdr_array(xdrs, (caddr_t *)&(p->aup_gids),
		    &(p->aup_len), NGRPS, sizeof(int), xdr_int) ) {
		return (TRUE);
	}
	return (FALSE);
}

#ifdef KERNEL
/*
 * XDR kernel unix auth parameters.
 * Goes out of the u struct directly.
 * NOTE: this is an XDR_ENCODE only routine.
 */
xdr_authkern(xdrs)
	register XDR *xdrs;
{
	gid_t	*gp;
	int	 uid = (short)u.u_uid;
	int	 gid = (short)u.u_gid;
	int	 len;
	int	groups[NGROUPS], *lp;
	char	*name = hostname;

	if (xdrs->x_op != XDR_ENCODE) {
		return (FALSE);
	}

	for (gp = &u.u_groups[NGRPS]; gp > u.u_groups; gp--) {
		if (gp[-1] != NOGROUP) {
			break;
		}
	}
	len = gp - u.u_groups;
	for (lp = groups, gp = u.u_groups; gp < &u.u_groups[len]; )
		*lp++ = (short)(*gp++);
        if (xdr_u_long(xdrs, (u_long *)&time.tv_sec)
            && xdr_string(xdrs, &name, MAX_MACHINE_NAME)
            && xdr_int(xdrs, &uid)
            && xdr_int(xdrs, &gid)
	    && xdr_array(xdrs, (caddr_t)groups, (u_int *)&len, NGRPS, 
		sizeof (int), xdr_int) ) {
                return (TRUE);
	}
	return (FALSE);
}
#endif
