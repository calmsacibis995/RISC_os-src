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
#ident	"$Header: main.c,v 1.1.2.2 90/05/07 19:54:05 wje Exp $"

#include "defs.h"
#include <sys/signal.h>
#include <stdio.h>
#include "string.h"
#include "char.h"
#include "local.h"

#define next(a) (*++*(a) ? *(a) : (*++(a) ? *(a) : (char *)usage()))

#ifdef NULLDEREF
/*
 * Looking for NULL dereferences in window has made me crazy.
 *
 * This code just catches all memory faults, advances the pc by 4,
 * and continues.  It is ugly, but window doesn't core dump any more.
 */

#include <mips/cpu.h>
null_deref_handler(sig, code, scp)  
struct sigcontext *scp;
{
	int i;
	int BD = 0;
	int branch_instruction;
	int exception_instruction;
	extern int __start();

	BD = !!(scp->sc_cause & CAUSE_BD);

	/*
	 * Grab the instructions at the exception.
	 */
	if (BD) {
		branch_instruction = *(int*)(scp->sc_pc);
		exception_instruction = *(int*)(scp->sc_pc + 4);
	} else {
		exception_instruction = *(int*)(scp->sc_pc);
	}

	/*
	 * NOTE: A more robust implementation would examine 
	 * exception_instruction.  Only load should be handled,
	 * and in that case this code should actually stuff a 0
	 * into the appropriate register.
	 */

	/*
	 * Restart the instruction.
	 */
	if (BD) {
		emulate_branch(scp, branch_instruction);
	} else {
		scp->sc_pc += 4;
	}

	sigreturn(scp);
}
#endif NULLDEREF

/*ARGSUSED*/
main(argc, argv)
char **argv;
{
	register char *p;
	char fflag = 0;
	char dflag = 0;
	char xflag = 0;
	char *cmd = 0;
	char tflag = 0;

#ifdef NULLDEREF
	signal(SIGSEGV, null_deref_handler);
	signal(SIGBUS, null_deref_handler);
#endif NULLDEREF

	nbufline = NLINE;
	escapec = ESCAPEC;	
	if (p = rindex(*argv, '/'))
		p++;
	else
		p = *argv;
	debug = strcmp(p, "a.out") == 0;
	while (*++argv) {
		if (**argv == '-') {
			switch (*++*argv) {
			case 'f':
				fflag++;
				break;
			case 'c':
				if (cmd != 0) {
					(void) fprintf(stderr,
						"Only one -c allowed.\n");
					(void) usage();
				}
				cmd = next(argv);
				break;
			case 'e':
				setescape(next(argv));
				break;
			case 't':
				tflag++;
				break;
			case 'd':
				dflag++;
				break;
			case 'D':
				debug = !debug;
				break;
			case 'x':
				xflag++;
				break;
			default:
				(void) usage();
			}
		} else
			(void) usage();
	}
	if ((p = getenv("SHELL")) == 0)
		p = SHELL;
	if ((shellfile = str_cpy(p)) == 0)
		nomem();
	if (p = rindex(shellfile, '/'))
		p++;
	else
		p = shellfile;
	shell[0] = p;
	shell[1] = 0;
	(void) gettimeofday(&starttime, (struct timezone *)0);
	if (wwinit() < 0) {
		(void) fprintf(stderr, "%s.\n", wwerror());
		exit(1);
	}
	if (debug)
		wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
	if (xflag) {
		wwnewtty.ww_tchars.t_stopc = wwoldtty.ww_tchars.t_stopc;
		wwnewtty.ww_tchars.t_startc = wwoldtty.ww_tchars.t_startc;
	}
	if (debug || xflag)
		(void) wwsettty(0, &wwnewtty, &wwoldtty);

	if ((cmdwin = wwopen(wwbaud > 2400 ? WWO_REVERSE : 0, 1, wwncol,
			     0, 0, 0)) == 0) {
		(void) wwflush();
		(void) fprintf(stderr, "%s.\r\n", wwerror());
		goto bad;
	}
	cmdwin->ww_mapnl = 1;
	cmdwin->ww_nointr = 1;
	cmdwin->ww_noupdate = 1;
	cmdwin->ww_unctrl = 1;
	if ((framewin = wwopen(WWO_GLASS|WWO_FRAME, wwnrow, wwncol, 0, 0, 0))
	    == 0) {
		(void) wwflush();
		(void) fprintf(stderr, "%s.\r\n", wwerror());
		goto bad;
	}
	wwadd(framewin, &wwhead);
	if ((boxwin = wwopen(WWO_GLASS, wwnrow, wwncol, 0, 0, 0)) == 0) {
		(void) wwflush();
		(void) fprintf(stderr, "%s.\r\n", wwerror());
		goto bad;
	}
	fgwin = framewin;

	wwupdate();
	wwflush();
	(void) signal(SIGCHLD, wwchild);
	setvars();

	setterse(tflag);
	setcmd(1);
	if (cmd != 0)
		(void) dolongcmd(cmd, (struct value *)0, 0);
	if (!fflag) {
		if (dflag || doconfig() < 0)
			dodefault();
		if (selwin != 0)
			setcmd(0);
	}

	mloop();

bad:
	wwend();
	return 0;
}

usage()
{
	(void) fprintf(stderr, "Usage: window [-e escape-char] [-c command] [-t] [-f] [-d]\n");
	exit(1);
	return 0;			/* for lint */
}

nomem()
{
	(void) fprintf(stderr, "Out of memory.\n");
	exit(1);
}
