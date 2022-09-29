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
#ident	"$Header: posix_ioctl.c,v 1.4.1.2 90/05/10 06:01:01 wje Exp $"


/*
 * Headers: r !types.h user.h param.h errno.h var.h debug.h
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/file.h"
#include "bsd43/sys/file.h"
#include "sys/ioctl.h"
#include "bsd43/sys/ioctl.h"
#include "sys/termio.h"
#include "posix/sys/termios.h"
#include "sys/vnode.h"

/*
 * System call routines which implement the POSIX ioctl()
 * system call.  These calls pass some ioctl operations on to
 * the main system routines, and implement others locally.
 */

/*
 * character special i/o control
 */
posix_ioctl()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	switch (uap->cmd) {
	/*
	 *	convert termios to termio structure and vice versa
	 */
	case TCGETA:
	{
		struct termios new_termios;
		struct termio old_termio;
		register int i;

		u.u_error = fioctl(fp, TCGETA, &old_termio);
		if (u.u_error)
			return;
		new_termios.c_iflag = (unsigned long)old_termio.c_iflag;
		new_termios.c_oflag = (unsigned long)old_termio.c_oflag;
		new_termios.c_cflag = (unsigned long)old_termio.c_cflag;
		new_termios.c_cflag |= (old_termio.c_cflag & CBAUD) << CIBDSHFT;
		new_termios.c_lflag = (unsigned long)old_termio.c_lflag;
		if (old_termio.c_lflag & POSIX_NOIEXTEN)
			new_termios.c_lflag &= ~IEXTEN;
		else
			new_termios.c_lflag |= IEXTEN;

		bcopy(&old_termio.c_cc[0], &new_termios.c_cc[0], NCCS);

		if (copyout((caddr_t) &new_termios, (caddr_t) uap->arg,
		    sizeof(struct termios)))
		u.u_error = EFAULT;
		return;
	};

	case TCSETA:
	case TCSETAW:
	case TCSETAF:
	{
		struct termios new_termios;
		struct termio old_termio;
		register int i;

		if (copyin((caddr_t)uap->arg, (caddr_t) &new_termios,
			   sizeof(struct termios))) {
			u.u_error = EFAULT;
			return;
		};
		u.u_error = fioctl(fp, TCGETA, &old_termio);
		if (u.u_error)
			return;
		old_termio.c_iflag = new_termios.c_iflag;
		old_termio.c_oflag = new_termios.c_oflag;
		old_termio.c_cflag = new_termios.c_cflag;
		old_termio.c_lflag = new_termios.c_lflag;
		if (new_termios.c_lflag & IEXTEN)
			old_termio.c_lflag &= ~POSIX_NOIEXTEN;
		else
			old_termio.c_lflag |= POSIX_NOIEXTEN;

		bcopy(&new_termios.c_cc[0], &old_termio.c_cc[0], NCCS);

		u.u_error = fioctl(fp, uap->cmd, &old_termio);
		return;
	};


	/*
	 *	call ioctl() to do some special checking (rather than
	 * 	duplicating the code here) before calling the 
	 * 	appropriate ioctl routine.
	 */
	case TIOCSPGRP:
	case TIOCGPGRP:
		/* make sure terminal is controlling terminal before
		 * calling ioctl()
		 */
		if (u.u_procp->p_ttyvp != NULL &&
		    (struct vnode *)fp->f_data == u.u_procp->p_ttyvp)
			ioctl();
		else 
			u.u_error = ENOTTY;
		return;

	/*
	 *	pass through unknown or future ioctl's unchanged
	 */
	default:
		break;
	};
	u.u_error = (*fp->f_ops->fo_ioctl)(fp, uap->cmd, uap->arg);
}
