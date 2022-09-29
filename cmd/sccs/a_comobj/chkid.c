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
#ident	"$Header: chkid.c,v 1.6.2.2 90/05/09 18:39:49 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"ctype.h"
# include	"defines.h"


char *strchr();

chkid(line,idstr)

char *line;
char *idstr;

{
	register char *lp;
	register char *p;
	char *pp;
	extern int Did_id;

	if (!Did_id && (lp = strchr(line,'%') ) )
		if (!idstr || idstr[0]=='\0' ) 
			for( ; *lp != 0; lp++) {
				if(lp[0] == '%' && lp[1] != 0 && lp[2] == '%')
					if (isupper(lp[1]))
						switch (lp[1]) {
						case 'J':
							break;
						case 'K':
							break;
						case 'N':
							break;
						case 'O':
							break;
						case 'V':
							break;
						case 'X':
							break;
						default:
							return(Did_id++);
						}
			}
		else {
			if ( (lp = strchr(idstr,'%')) == NULL ) return(Did_id);
			for( ; *lp != 0; lp++) {
				if(lp[0] == '%' && lp[1] != 0 && lp[2] == '%')
					if (isupper(lp[1]))
						switch (lp[1]) {
						case 'J':
							break;
						case 'K':
							break;
						case 'N':
							break;
						case 'O':
							break;
						case 'V':
							break;
						case 'X':
							break;
						default:
							Did_id++;
						}
			}
			if (!Did_id) return(Did_id); /* There's no keyword in idstr */
			Did_id = 0;
			p=idstr;
			lp=line;
			while(lp=strchr(lp,*p))
				if(!(strncmp(lp,p,strlen(p))))
					return(Did_id++);
				else
					++lp;
		}

	return(Did_id);
}
