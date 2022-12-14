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
#ident	"$Header: ask.c,v 1.2.2.2 90/05/09 18:07:34 wje Exp $"
#include <stdio.h>
#define	bool	short
#define	TRUE	1
#define FALSE	0

char *
ask(prompt)
	char	*prompt;
{
	static	char	ans[256];
	static	bool	ttyclosed = TRUE;
	static	FILE	*tty;

	if (ttyclosed)
		if ((tty = fopen("/dev/tty", "r")) == NULL) {
			perror("/dev/tty");
			exit(1);
		} else
			ttyclosed = FALSE;

	fprintf(stderr, prompt);
	fgets(ans, sizeof(ans), tty);
	/* fclose(tty); */
	return ans;
}

/* VARARGS1 */
char *
askf(format, val1, val2, val3, val4)
	char	*format;
	int	val1, val2, val3, val4;
{
	char	prompt[80];

	sprintf(prompt, format, val1, val2, val3, val4);
	return ask(prompt);
}

bool
y_or_n(prompt)
	char	*prompt;
{
	char	*ans;
	bool	yes, no;

	char	*ask();

	do {
		ans = ask(prompt);
		no  = ans[1] == '\n' && (ans[0]  == 'n' || ans[0]  == 'N' );
		yes = ans[1] == '\n' && (ans[0]  == 'y' || ans[0]  == 'Y' );
	} while (! (yes || no) );
	return(yes);
}
