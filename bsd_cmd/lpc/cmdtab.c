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
#ident	"$Header: cmdtab.c,v 1.1.2.2 90/05/07 18:47:03 wje Exp $"

/*
 * lpc -- command tables
 */

#include "lpc.h"

int	abort(), clean(), enable(), disable(), down(), help();
int	quit(), restart(), startup(), status(), stop(), topq(), up();

char	aborthelp[] =	"terminate a spooling daemon immediately and disable printing";
char	cleanhelp[] =	"remove cruft files from a queue";
char	enablehelp[] =	"turn a spooling queue on";
char	disablehelp[] =	"turn a spooling queue off";
char	downhelp[] =	"do a 'stop' followed by 'disable' and put a message in status";
char	helphelp[] =	"get help on commands";
char	quithelp[] =	"exit lpc";
char	restarthelp[] =	"kill (if possible) and restart a spooling daemon";
char	starthelp[] =	"enable printing and start a spooling daemon";
char	statushelp[] =	"show status of daemon and queue";
char	stophelp[] =	"stop a spooling daemon after current job completes and disable printing";
char	topqhelp[] =	"put job at top of printer queue";
char	uphelp[] =	"enable everything and restart spooling daemon";

struct cmd cmdtab[] = {
	{ "abort",	aborthelp,	abort,		1 },
	{ "clean",	cleanhelp,	clean,		1 },
	{ "enable",	enablehelp,	enable,		1 },
	{ "exit",	quithelp,	quit,		0 },
	{ "disable",	disablehelp,	disable,	1 },
	{ "down",	downhelp,	down,		1 },
	{ "help",	helphelp,	help,		0 },
	{ "quit",	quithelp,	quit,		0 },
	{ "restart",	restarthelp,	restart,	0 },
	{ "start",	starthelp,	startup,	1 },
	{ "status",	statushelp,	status,		0 },
	{ "stop",	stophelp,	stop,		1 },
	{ "topq",	topqhelp,	topq,		1 },
	{ "up",		uphelp,		up,		1 },
	{ "?",		helphelp,	help,		0 },
	{ 0 },
};

int	NCMDS = sizeof (cmdtab) / sizeof (cmdtab[0]);
