/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: fio.c,v 1.6.1.1 89/11/28 12:09:15 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*#ident	"@(#)kern-port:os/fio.c	10.8"*/
#ident	"$Header: fio.c,v 1.6.1.1 89/11/28 12:09:15 wje Exp $"

#include "sys/types.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/file.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/acct.h"
#include "sys/sysinfo.h"

/*
 * Essentially all routines here have been taken from NFS4.0:kern_descrip.c.
 * I preserved the freelist from SysV, though, which meant keeping the init
 * routine.  I also kept the unfalloc() routine for cleanliness.
 */

/*
 * Convert a user supplied file descriptor into a pointer
 * to a file structure.
 * Only task is to check range of the descriptor.
 */
struct file *
getf(f)
register int f;
{
	register struct file *fp;

	if (0 <= f && f < v.v_nofiles) 
		if ((fp = u.u_ofile[f]) != NULL) 
			return(fp);
	u.u_error = EBADF;
	return(NULL);
}

/*
 * Internal form of close.
 * Decrement reference count on file structure.
 * Also make sure the pipe protocol does not constipate.
 *
 * Decrement reference count on the inode following
 * removal to the referencing file structure.
 * On the last close switch out to the device handler for
 * special files.  Note that the handler is called
 * on every open but only the last close.
 */
closef(fp)
register struct file *fp;
{
	/* Sanity check. */
	if (fp == NULL || (fp->f_count) <= 0)
		return;
	if ((unsigned)fp->f_count > 1) {
		fp->f_count--;
		return;
	}
	if (fp->f_ops != NULL)
		(*fp->f_ops->fo_close)(fp);
	fp->f_count = 0;
	crfree(fp->f_cred);
	fp->f_next = ffreelist;
	ffreelist = fp;
}

/*
 * Test if the current user is the super user.
 */
suser()
{

	if (u.u_uid == 0) {
		u.u_acflag |= ASU;
		return(1);
	}
	u.u_error = EPERM;
	return(0);
}

/*
 * Allocate a user file descriptor.
 */
ufalloc(i)
register i;
{

	for (; i < v.v_nofiles; i++)
		if (u.u_ofile[i] == NULL) {
			u.u_rval1 = i;
			u.u_pofile[i] = 0;
			return(i);
		}
	u.u_error = EMFILE;
	return(-1);
}

/*
 * Count the number of unused file descriptors.
 */
/*
 * We have a choice of looking through the table, or trundling down the 
 * freelist.  If the freelist is long, this should be quicker (we don't
 * have to read the extra pointer).  If the freelist is short, we lose.
 * But this is all in the noise, anyway.... (I hope!)
 */
ufavail()
{
	register int i, avail = 0;

	for (i = 0; i < v.v_nofiles; i++)
		if (u.u_ofile[i] == NULL)
			avail++;
	return (avail);
}

/*
 * Allocate a user file descriptor and a file structure.
 * Initialize the descriptor to point at the file structure.
 *
 * file table overflow -- if there are no available file structures.
 */
struct file *
falloc()
{
	register struct file *fp;
	register i;

	if ((i = ufalloc(0)) < 0)
		return(NULL);
	if ((fp = ffreelist) == NULL) {
		cmn_err(CE_NOTE, "File table overflow\n");
		syserr.fileovf++;
		u.u_error = ENFILE;
		return(NULL);
	}
	ffreelist = fp->f_next;
	u.u_ofile[i] = fp;
	fp->f_count = 1;
	fp->f_type = 0;
	fp->f_ops = NULL;
	fp->f_data = 0;
	fp->f_offset = 0;
	crhold(u.u_cred);
	fp->f_cred = u.u_cred;
	return (fp);
}

struct file *ffreelist;

finit()
{
	register struct file *fp;

	for (ffreelist = fp = &file[0]; fp < &file[v.v_file-1]; fp++)
		fp->f_next = fp+1;
}

unfalloc(fp)
register struct file	*fp;
{
	if (--fp->f_count <= 0) {
		crfree(fp->f_cred);
		fp->f_next = ffreelist;
		ffreelist = fp;
	}
}
