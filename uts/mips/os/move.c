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
#ident	"$Header: move.c,v 1.8.1.2 90/05/10 05:50:58 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/uio.h"
#include "sys/errno.h"
#include "sys/buf.h"

/*
 * Move n bytes at byte location
 * &bp->b_un.b_addr[o] to/from (flag) the
 * user/kernel (u.segflg) area starting at u.base.
 * Update all the arguments by the number
 * of bytes moved.
 */
iomove(cp, n, flag)
register caddr_t cp;
register n;
{
	register t;

	if (n==0)
		return;
	if (u.u_uio == NULL) {
		if (u.u_segflg != 1)  {
			if (flag==B_WRITE)
				t = copyin(u.u_base, (caddr_t)cp, n);
			else
				t = copyout((caddr_t)cp, u.u_base, n);
			if (t) {
				u.u_error = EFAULT;
				return;
			}
		}
		else
			if (flag == B_WRITE)
				bcopy(u.u_base,(caddr_t)cp,n);
			else
				bcopy((caddr_t)cp,u.u_base,n);
		u.u_base += n;
		u.u_offset += n;
		u.u_count -= n;
	} else {
		t=uiomove(cp, n, ((flag==B_WRITE)?UIO_WRITE:UIO_READ), u.u_uio);
		if (t) {
			u.u_error = EFAULT;
			return;
		}
	}
	return;
}

/*
 * Pass back  c  to the user at his location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * on the last character of the user's read.
 * u_base is in the user data space.
 */
passc(c)
register c;
{
	if (subyte(u.u_base, c) < 0) {
		u.u_error = EFAULT;
		return(-1);
	}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return(u.u_count == 0? -1: 0);
}

/*
 * Pick up and return the next character from the user's
 * write call at location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * when u_count is exhausted.
 * u_base is in the user data space.
 */
cpass()
{
	register c;

	if (u.u_count == 0)
		return(-1);
	if ((c = fubyte(u.u_base)) < 0) {
		u.u_error = EFAULT;
		return(-1);
	}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return(c);
}

/*	copy 8 or fewer characters from string *b to string *a	*/
/*	terminated by null character	*/
/*	string *a must be at least 9 characters long	*/
/*	used by machdep.c	*/
str8cpy(a, b)
register char	*a, *b;
{
	register int	i = 0;

	while (*a++ = *b++)
		if (++i >= 8)
			break;
	*a = '\0';
}

/*
 * This bcopy handles overlaps.  
 * It could be optimized in assembly like bcopy, 
 * but backward copies are seldom large, so there is little gain.
 */
ovbcopy(from,to,len)
register char *from,*to;
register len;
{
	if (from < to) {
		from += len;
		to += len;
		while(len--)
			*--to = *--from;
	} else {
		while(len--)
			*to++ = *from++;
	}
}
