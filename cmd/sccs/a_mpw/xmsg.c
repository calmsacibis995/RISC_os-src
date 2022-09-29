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
#ident	"$Header: xmsg.c,v 1.6.2.2 90/05/09 18:43:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"
# include	<errno.h>


/*
	Call fatal with an appropriate error message
	based on errno.  If no good message can be made up, it makes
	up a simple message.
	The second argument is a pointer to the calling functions
	name (a string); it's used in the manufactured message.
*/

xmsg(file,func)
char *file, *func;
{
	register char *str;
	char d[FILESIZE];
	extern int errno;
	extern char Error[];

	switch (errno) {
	case ENFILE:
		str = "no file (ut3)";
		break;
	case ENOENT:
		sprintf(str = Error,"`%s' nonexistent (ut4)",file);
		break;
	case EACCES:
		str = d;
		copy(file,str);
		file = str;
		sprintf(str = Error,"directory `%s' unwritable (ut2)",
			dname(file));
		break;
	case ENOSPC:
		str = "no space! (ut10)";
		break;
	case EFBIG:
		str = "write error (ut8)";
		break;
	default:
		sprintf(str = Error,"errno = %d, function = `%s' (ut11)",errno,
			func);
		break;
	}
	return(fatal(str));
}
