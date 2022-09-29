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
#ident	"$Header: wait3.c,v 1.1.2.2 90/05/07 21:28:34 wje Exp $"

#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

/*
 * C library -- wait3
 *
 * pid = wait3(&status, options, &rusage);
 *
 * pid == -1 if error
 * 
 * Status indicates fate of process, if given.  Options may indicate process
 * is not to hang or that untraced stopped children are to be reported.
 * Rusage optionally returns detailed resource usage information. This uses
 * the "real" mips_wait3() system call so that when rusage is extended to
 * contain mips specific information old programs will work that used this.
 */
wait3(status, options, rusage)
union wait *status;
int options;
struct rusage *rusage;
{
	return(mips_wait3(status, options, rusage, sizeof(struct rusage)));
}
