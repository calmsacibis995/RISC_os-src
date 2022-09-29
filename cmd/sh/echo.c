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
#ident	"$Header: echo.c,v 1.8.1.2 90/05/09 18:56:43 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */

#include	"defs.h"

#define	exit(a)	flushb();return(a)

extern int exitval;

echo(argc, argv)
char **argv;
{
	register char	*cp;
	register int	i, wd;
	int	j;
#if BRL || BSD_SYS || RISCOS
	BOOL		no_nl;		/* no newline if set */
#endif
	
	if(--argc == 0) {
		prc_buff('\n');
		exit(0);
	}
#if BRL || BSD_SYS || RISCOS
#ifdef RISCOS
	if (! (flags & echo_dash_n_flg))
	    no_nl = 0;
	else
#endif RISCOS
	    if(no_nl = eq(argv[1], "-n"))	/* old-style no-newline flag */
	    {
		--argc;			/* skip over "-n" argument */
		++argv;
	    }
#endif

	for(i = 1; i <= argc; i++) 
	{
		sigchk();
		for(cp = argv[i]; *cp; cp++) 
		{
			if(*cp == '\\')
			switch(*++cp) 
			{
				case 'b':
					prc_buff('\b');
					continue;

				case 'c':
					exit(0);

				case 'f':
					prc_buff('\f');
					continue;

				case 'n':
					prc_buff('\n');
					continue;

				case 'r':
					prc_buff('\r');
					continue;

				case 't':
					prc_buff('\t');
					continue;

				case 'v':
					prc_buff('\v');
					continue;

				case '\\':
					prc_buff('\\');
					continue;
				case '0':
					j = wd = 0;
					while ((*++cp >= '0' && *cp <= '7') && j++ < 3) {
						wd <<= 3;
						wd |= (*cp - '0');
					}
					prc_buff(wd);
					--cp;
					continue;

				default:
					cp--;
			}
			prc_buff(*cp);
		}
		if (flags & echo_dash_n_flg) {
			if(i != argc)
				prc_buff(' ');
			else if (! no_nl)
				prc_buff('\n');
		} else {
			prc_buff(i == argc? '\n': ' ');
		};
	}
	exit(0);
}

