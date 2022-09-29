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
#ident	"$Header: siglongjmperr.c,v 1.1.1.2 90/05/10 04:11:27 wje Exp $"

#define ERRMSG	"siglongjmp botch\n"

/*
 * This routine is called from siglongjmp() when an error occurs.
 * Programs that wish to exit gracefully from this error may
 * write their own versions.
 * If this routine returns, the program is aborted.
 */
_siglongjmperr()
{

	write(2, ERRMSG, sizeof(ERRMSG));
}
