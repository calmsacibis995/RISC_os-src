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
/* $Header: sysexits.h,v 1.3.2.2 90/05/07 20:09:11 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
**  SYSEXITS.H -- Exit status codes for system programs.
**
**	This include file attempts to categorize possible error
**	exit statuses for system programs, notably delivermail
**	and the Berkeley network.
**
**	Error numbers begin at EX__BASE to reduce the possibility of
**	clashing with other exit statuses that random programs may
**	already return.  The meaning of the codes is approximately
**	as follows:
**
**	EX_USAGE -- The command was used incorrectly, e.g., with
**		the wrong number of arguments, a bad flag, a bad
**		syntax in a parameter, or whatever.
**	EX_DATAERR -- The input data was incorrect in some way.
**		This should only be used for user's data & not
**		system files.
**	EX_NOINPUT -- An input file (not a system file) did not
**		exist or was not readable.  This could also include
**		errors like "No message" to a mailer (if it cared
**		to catch it).
**	EX_NOUSER -- The user specified did not exist.  This might
**		be used for mail addresses or remote logins.
**	EX_NOHOST -- The host specified did not exist.  This is used
**		in mail addresses or network requests.
**	EX_UNAVAILABLE -- A service is unavailable.  This can occur
**		if a support program or file does not exist.  This
**		can also be used as a catchall message when something
**		you wanted to do doesn't work, but you don't know
**		why.
**	EX_SOFTWARE -- An internal software error has been detected.
**		This should be limited to non-operating system related
**		errors as possible.
**	EX_OSERR -- An operating system error has been detected.
**		This is intended to be used for such things as "cannot
**		fork", "cannot create pipe", or the like.  It includes
**		things like getuid returning a user that does not
**		exist in the passwd file.
**	EX_OSFILE -- Some system file (e.g., /etc/passwd, /etc/utmp,
**		etc.) does not exist, cannot be opened, or has some
**		sort of error (e.g., syntax error).
**	EX_CANTCREAT -- A (user specified) output file cannot be
**		created.
**	EX_IOERR -- An error occurred while doing I/O on some file.
**	EX_TEMPFAIL -- temporary failure, indicating something that
**		is not really an error.  In sendmail, this means
**		that a mailer (e.g.) could not create a connection,
**		and the request should be reattempted later.
**	EX_PROTOCOL -- the remote system returned something that
**		was "not possible" during a protocol exchange.
**	EX_NOPERM -- You did not have sufficient permission to
**		perform the operation.  This is not intended for
**		file system problems, which should use NOINPUT or
**		CANTCREAT, but rather for higher level permissions.
**		For example, kre uses this to restrict who students
**		can send mail to.
**
**	Maintained by Eric Allman (eric@berkeley, ucbvax!eric) --
**		please mail changes to me.
**
**			@(#)sysexits.h	4.2		7/31/83
*/

# define BSD43_EX_OK		0	/* successful termination */

# define BSD43_EX__BASE		64	/* base value for error messages */

# define BSD43_EX_USAGE		64	/* command line usage error */
# define BSD43_EX_DATAERR		65	/* data format error */
# define BSD43_EX_NOINPUT		66	/* cannot open input */
# define BSD43_EX_NOUSER		67	/* addressee unknown */
# define BSD43_EX_NOHOST		68	/* host name unknown */
# define BSD43_EX_UNAVAILABLE	69	/* service unavailable */
# define BSD43_EX_SOFTWARE	70	/* internal software error */
# define BSD43_EX_OSERR		71	/* system error (e.g., can't fork) */
# define BSD43_EX_OSFILE		72	/* critical OS file missing */
# define BSD43_EX_CANTCREAT	73	/* can't create (user) output file */
# define BSD43_EX_IOERR		74	/* input/output error */
# define BSD43_EX_TEMPFAIL	75	/* temp failure; user is invited to retry */
# define BSD43_EX_PROTOCOL	76	/* remote error in protocol */
# define BSD43_EX_NOPERM		77	/* permission denied */
# define BSD43_EX_CONFIG		78	/* configuration error */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define EX_CANTCREAT BSD43_EX_CANTCREAT
#   define EX_CONFIG BSD43_EX_CONFIG
#   define EX_DATAERR BSD43_EX_DATAERR
#   define EX_IOERR BSD43_EX_IOERR
#   define EX_NOHOST BSD43_EX_NOHOST
#   define EX_NOINPUT BSD43_EX_NOINPUT
#   define EX_NOPERM BSD43_EX_NOPERM
#   define EX_NOUSER BSD43_EX_NOUSER
#   define EX_OK BSD43_EX_OK
#   define EX_OSERR BSD43_EX_OSERR
#   define EX_OSFILE BSD43_EX_OSFILE
#   define EX_PROTOCOL BSD43_EX_PROTOCOL
#   define EX_SOFTWARE BSD43_EX_SOFTWARE
#   define EX_TEMPFAIL BSD43_EX_TEMPFAIL
#   define EX_UNAVAILABLE BSD43_EX_UNAVAILABLE
#   define EX_USAGE BSD43_EX_USAGE
#   define EX__BASE BSD43_EX__BASE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


