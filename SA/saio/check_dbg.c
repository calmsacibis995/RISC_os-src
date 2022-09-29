#ident "$Header: check_dbg.c,v 1.2 90/01/16 15:38:29 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

#ifndef PROM
/*
 * check_dbg.c -- check for $dbgmon in environ, if so load debug monitor
 * and transfer control to it
 */

#include "prom/entrypt.h"
#include "saio/stringlist.h"

#define	streq(a,b)	(strcmp(a,b)==0)

struct string_list dbg_argv;
struct string_list dbg_environ;

_check_dbg(argc, argv, environ, start_pc)
int argc;
char **argv;
char **environ;
{
	register char *cp, *bp;
	register char **wp;
	register int commas;
	register char c;
	int filelen;
	int (*init_pc)();
	struct promexec_args pa;
	char bootfile[64];
	extern char *getenv();
	extern char *index();
	extern (*_prom_exec())();

	if (argc == 0)
		return;
	/*
	 * Filenames that end in .dbg force loading of the debugger
	 */
	filelen = strlen(argv[0]);
	if (filelen > 4 && streq(".dbg", &argv[0][filelen-4]))
		goto load_debugger;
	cp = getenv("dbgmon");
	if (!cp)
		return;	/* debugging not requested */
	cp = index(argv[0], ')');
	if (!cp) {
		printf("bad filename format, can't load dbgmon\n");
		return;
	}
	cp++;
	if (streq(cp, "sash"))	/* don't debug sash! */
		return;
	/*
	 * boot dbgmon from same device this command was booted from
	 * but force partition 0, since prom doesn't understand file
	 * systems
	 */
load_debugger:
	bp = bootfile;
	cp = argv[0];
	commas = 0;
	while ((c = *cp) && commas < 2 && c != ')') {
		if (c == ',')
			commas++;
		*bp++ = *cp++;
	}
	while(commas++ < 2)
		*bp++ = ',';
	strcpy(bp, "0)dbgmon");

	/*
	 * copy args and environment
	 */
	init_str(&dbg_argv);
	for (wp = argv; wp && *wp; wp++)
		if (new_str1(*wp, &dbg_argv))
			return;
	init_str(&dbg_environ);
	for (wp = environ; wp && *wp; wp++)
		if (new_str1(*wp, &dbg_environ))
			return;

	/*
	 * boot in the debug monitor, it had better not overlay us!
	 */
	printf("Loading %s\n", bootfile);
	pa.pa_bootfile = bootfile;
	pa.pa_argc = dbg_argv.strcnt;
	pa.pa_argv = dbg_argv.strptrs;
	pa.pa_environ = dbg_environ.strptrs;
	pa.pa_flags = EXEC_NOGO;
	init_pc = _prom_exec(&pa);
	if ((int)init_pc == -1) {
		printf("couldn't load dbgmon\n");
		return;
	}
	/*
	 * transfer control to dbgmon
	 * give the debugger our argc, argv, environ and the pc of our
	 * main routine so it can properly start us back up.
	 */
	(*init_pc)(argc, argv, environ, start_pc);
	/* shouldn't return */
}
#endif !PROM
