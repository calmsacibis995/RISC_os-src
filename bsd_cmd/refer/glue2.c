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
#ident	"$Header: glue2.c,v 1.1.2.3 90/05/07 19:09:32 wje Exp $"

char refdir[50];

savedir()
{
#ifdef RISCOS
	if (refdir[0] == 0 )
	    (void)getwd(refdir);
#else
	if (refdir[0]==0)
		corout ("", refdir, "/bin/pwd", "", 50);
	trimnl(refdir);
#endif
}

restodir()
{
	chdir(refdir);
}
