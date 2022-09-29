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
#ident	"$Header: spec_clone.c,v 1.2.1.2 90/05/10 05:06:27 wje Exp $"
/*
 * Clone device driver.  Forces a clone open of some other
 * character device.  Since its purpose in life is to force
 * some other device to clone itself, there's no need for
 * anything other than the open routine here.
 */

/* Original includes: param.h systm.h user.h */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/errno.h"
#include "sys/sysmacros.h"

/*
 * Do a clone open.  The (major number of the) device to be cloned
 * is specified by minor(dev).  We tell spec_open to do the work
 * by returning EEXIST after naming the device to clone.
 */
/* ARGSUSED */
cloneopen(dev, flag, newdevp)
	dev_t	dev;
	int	flag;
	dev_t	*newdevp;
{
	/* Convert to the device to be cloned. */
	*newdevp = makedev(minor(dev), 0);

	return (EEXIST);
}
