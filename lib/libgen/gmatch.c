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
#ident	"$Header: gmatch.c,v 1.2.2.2 90/05/10 02:36:46 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

gmatch(s, p)
register char	*s, *p;
{
	register char scc;
	char c;

	scc = *s++;
	switch (c = *p++)
	{
	case '[':
		{
			int ok;
			int lc = -1;
			int notflag = 0;

			ok = 0;
			if (*p == '!')
			{
				notflag = 1;
				p++;
			}
			while (c = *p++)
			{
				if (c == ']')
					return(ok ? gmatch(s, p) : 0);
				else if (c == '-' && lc > 0 && *p!= ']')
				{
					if (notflag)
					{
						if ((unsigned char)scc < lc || scc > *(p++))
							ok++;
						else
							return(0);
					}
					else
					{
						if (lc <= (unsigned char)scc && scc <= (*p++))
							ok++;
					}
				}
				else
				{
					if(c == '\\') /* skip to quoted character */
						c = *p++;
					lc = (unsigned char)c;
					if (notflag)
					{
						if (scc && (unsigned char)scc != lc)
							ok++;
						else
							return(0);
					}
					else
					{
						if ((unsigned char)scc == lc)
							ok++;
					}
				}
			}
			return(0);
		}

	case '\\':	
		c = *p++; /* skip to quoted character and see if it matches */
	default:
		if (c != scc)
			return(0);

	case '?':
		return(scc ? gmatch(s, p) : 0);

	case '*':
		while (*p == '*')
			p++;

		if (*p == 0)
			return(1);
		--s;
		while (*s)
		{
			if (gmatch(s++, p))
				return(1);
		}
		return(0);

	case 0:
		return(scc == 0);
	}
}
