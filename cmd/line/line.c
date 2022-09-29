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
#ident	"$Header: line.c,v 1.5.2.2 90/05/09 16:22:30 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	This program reads a single line from the standard input
	and writes it on the standard output. It is probably most useful
	in conjunction with the shell.
*/
#define LSIZE 512
int EOF;
char nl = '\n';
main()
{
	register char c;
	char line[LSIZE];
	register char *linep, *linend;

EOF = 0;
linep = line;
linend = line + LSIZE;

while ((c = readc()) != nl)
	{
	if (linep == linend)
		{
		write (1, line, LSIZE);
		linep = line;
		}
	*linep++ = c;
	}
write (1, line, linep-line);
write(1,&nl,1);
if (EOF == 1) exit(1);
exit (0);
}
readc()
{
	char c;
if (read (0, &c, 1) != 1) {
	EOF = 1;
	return(nl);
	}
else
	return (c);
}
