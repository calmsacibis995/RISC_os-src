/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: hostname.c,v 1.6.4.1 89/11/28 10:15:50 wje Exp $ */

/* Host and Domain name system calls
 *
 * $Header: hostname.c,v 1.6.4.1 89/11/28 10:15:50 wje Exp $
 */

#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/fs/s5dir.h"
#include "sys/user.h"
#include "sys/param.h"
#include "sys/limits.h"
#include "sys/utsname.h"
#include "sys/errno.h"


gethostname()
{
	register struct a {
		char	*hostname;
		int	len;
	} *uap = (struct a *)u.u_ap;
	register u_int len;

	len = uap->len;
	if (len > hostnamelen + 1)
		len = hostnamelen + 1;
	if (copyout((caddr_t)hostname, (caddr_t)uap->hostname, (int)len))
		u.u_error = EFAULT;
}

sethostname()
{
	register struct a {
		char	*hostname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;

	if (!suser())
		return;
	if (uap->len > MAXHOSTNAMELEN) {
		u.u_error = EINVAL;
		return;
	}
	hostnamelen = uap->len;
	if (copyin((caddr_t)uap->hostname, hostname, (int)uap->len))
		u.u_error = EFAULT;
	else {
		hostname[hostnamelen] = 0;
		strncpy(utsname.sysname, hostname, sizeof(utsname.sysname)-1);
		strncpy(utsname.nodename, hostname, sizeof(utsname.nodename)-1);
		utsname.sysname[sizeof(utsname.sysname)-1] = 0;
		utsname.nodename[sizeof(utsname.nodename)-1] = 0;
	}
}

getdomainname()
{
	register struct a {
		char	*domainname;
		int	len;
	} *uap = (struct a *)u.u_ap;
	register u_int len;

	len = uap->len;
	if (len > domainnamelen + 1)
		len = domainnamelen + 1;
	if (copyout((caddr_t)domainname,(caddr_t)uap->domainname,len))
		u.u_error = EFAULT;
}

setdomainname()
{
	register struct a {
		char	*domainname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;

	if (!suser())
		return;
	if (uap->len > MAXHOSTNAMELEN) {
		u.u_error = EINVAL;
		return;
	}
	domainnamelen = uap->len;
	if (copyin((caddr_t)uap->domainname, domainname, uap->len))
		u.u_error = EFAULT;
	domainname[domainnamelen] = 0;
}
