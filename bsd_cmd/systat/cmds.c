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
#ident	"$Header: cmds.c,v 1.3.1.2 90/05/07 19:30:00 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Command support.
 */

#include "systat.h"
#include <ctype.h>

#ifdef RISCOS
#include <bsd43/sys/syscall.h>
#define kill(x,y) syscall(BSD43_SYS_kill,(x),(y))
#define signal sigset
#endif RISCOS

command(cmd)
        char *cmd;
{
        register char *cp;
        register struct cmdtab *p;
	int interval, omask;

#ifdef RISCOS
	sighold(SIGALRM);
#else RISCOS
	omask = sigblock(mask(SIGALRM));
#endif RISCOS
        for (cp = cmd; *cp && !isspace(*cp); cp++)
                ;
        if (*cp)
                *cp++ = '\0';
	if (*cmd == '\0')
		return;
	for (; *cp && isspace(*cp); cp++)
		;
        if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "q") == 0)
                die();
	if (strcmp(cmd, "load") == 0) {
		load();
		goto done;
	}
        if (strcmp(cmd, "stop") == 0) {
                alarm(0);
                mvaddstr(CMDLINE, 0, "Refresh disabled.");
                clrtoeol();
		goto done;
        }
	if (strcmp(cmd, "help") == 0) {
		int col, len;

		move(CMDLINE, col = 0);
		for (p = cmdtab; p->c_name; p++) {
			len = strlen(p->c_name);
			if (col + len > COLS)
				break;
			addstr(p->c_name); col += len;
			if (col + 1 < COLS)
				addch(' ');
		}
		clrtoeol();
		goto done;
	}
	interval = atoi(cmd);
        if (interval <= 0 &&
	    (strcmp(cmd, "start") == 0 || strcmp(cmd, "interval") == 0)) {
		interval = *cp ? atoi(cp) : naptime;
                if (interval <= 0) {
			error("%d: bad interval.", interval);
			goto done;
                }
	}
	if (interval > 0) {
                alarm(0);
                naptime = interval;
                display();
                status();
		goto done;
        }
	p = lookup(cmd);
	if (p == (struct cmdtab *)-1) {
		error("%s: Ambiguous command.", cmd);
		goto done;
	}
        if (p) {
                if (curcmd == p)
			goto done;
                alarm(0);
		(*curcmd->c_close)(wnd);
		wnd = (*p->c_open)();
		if (wnd == 0) {
			error("Couldn't open new display");
			wnd = (*curcmd->c_open)();
			if (wnd == 0) {
				error("Couldn't change back to previous cmd");
				exit(1);
			}
			p = curcmd;
		}
		if ((p->c_flags & CF_INIT) == 0) {
			if ((*p->c_init)())
				p->c_flags |= CF_INIT;
			else
				goto done;
		}
                curcmd = p;
		labels();
                display();
                status();
		goto done;
        }
	if (curcmd->c_cmd == 0 || !(*curcmd->c_cmd)(cmd, cp))
		error("%s: Unknown command.", cmd);
done:
#ifdef RISCOS
	sigrelse(SIGALRM);
#else RISCOS
	sigsetmask(omask);
#endif RISCOS
}

struct cmdtab *
lookup(name)
	register char *name;
{
	register char *p, *q;
	register struct cmdtab *c, *found;
	register int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = (struct cmdtab *) 0;
	for (c = cmdtab; p = c->c_name; c++) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return (c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return ((struct cmdtab *)-1);
	return (found);
}

status()
{

        error("Showing %s, refresh every %d seconds.",
          curcmd->c_name, naptime);
}

suspend()
{
        int oldmask;
	extern (*sigtstpdfl)();

	alarm(0);
        move(CMDLINE, 0);
        refresh();
        echo();
        nocrmode();
        signal(SIGTSTP, sigtstpdfl);
#ifdef RISCOS
	sigrelse(SIGTSTP);
#else RISCOS
	oldmask = sigsetmask(sigblock(0) &~ sigmask(SIGTSTP));
#endif RISCOS
        kill(getpid(), SIGTSTP);
#ifdef RISCOS
	sighold(SIGTSTP);
#else RISCOS
	sigblock(sigmask(SIGTSTP));
#endif RISCOS
        signal(SIGTSTP, suspend);
        crmode();
        noecho();
        move(CMDLINE, col);
	alarm(naptime);
}

prefix(s1, s2)
        register char *s1, *s2;
{

        while (*s1 == *s2) {
                if (*s1 == '\0')
                        return (1);
                s1++, s2++;
        }
        return (*s1 == '\0');
}
