#ident "$Header: main.c,v 1.25.1.3 90/12/14 12:35:47 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * main.c -- main line prom/sash code
 */

#include "prom/prom.h"
#include "sys/param.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "saio/parser.h"
#include "saio/setjmp.h"
#include "saio/stringlist.h"

jmp_buf restart_buf;
struct string_list environ_str;

/*
 * PROMPT is different for debugging version so
 * it's easy to tell which monitor you're talking to
 * when debugging monitor with monitor
 */
#ifdef PROM

#ifdef DEBUG
#define	PROMPT	">>> "
#else
#define	PROMPT	">> "
#endif

#endif

#ifdef SASH
#define	PROMPT	"sash: "
#endif

/*
 * commands
 */
extern int boot(), disable(), enable(), get(), printenv(), unsetenv();
extern int cat(), load(), put(), setenv(), go(),scsi_probe();
extern int autoboot(), warm_cmd(), spin(), fill(), dump(), help();
extern int sload();
extern int diagmon();
#if !defined(SABLE) && !defined(SASH) && defined(NO_MORE)
extern int test();
#endif

#ifdef SASH
extern int copy();
#endif SASH

#ifdef PROM
extern int _init();
#endif PROM

extern int pr_tod();
extern int init_tod();

#ifdef notdef
extern int read_spin(), write_spin(), rewrite_verify();
extern int write_verify();
#endif

/*
 * cmd_table -- interface between parser and command execution routines
 * Add new commands by making entry here.
 */
static struct cmd_table cmd_table[] = {
	{ "auto",	autoboot,	"autoboot:\tauto" },
	{ "boot",	boot,		"boot:\t\tboot [-f FILE] [-n] [ARGS]" },
	{ "cat",	cat,		"cat:\t\tcat FILE_LIST" },
#if defined(SASH) || defined(M120) || defined(Rx3230) || defined(RB3125)
	{ "sprobe",	scsi_probe,	"sprobe:\t\tsprobe" },
#endif SASH || M120 || Rx3230 || RB3125
#ifdef SASH
	{ "cp",		copy,		"copy:\t\tcp [-b BLOCKSIZE] [-c BCOUNT] SRC_FILE DST_FILE" },
#endif
	{ "disable",	disable,	"disable:\tdisable CONSOLE_DEVICE" },
	{ "dump",	dump,		"dump:\t\tdump [-(b|h|w)] [-(o|d|u|x|c|B)] RANGE" },
	{ "enable",	enable,		"enable:\t\tenable CONSOLE_DEVICE" },
	{ "fill",	fill,		"fill:\t\tfill [-(b|h|w)] [-v VAL] RANGE"},
	{ "g",		get,		"get:\t\tg [-(b|h|w)] ADDRESS" },
	{ "go",		go,		"go:\t\tgo [INITIAL_PC]" },
	{ "help",	help,		"help:\t\thelp [COMMAND]" },
	{ "?",		help,		"help:\t\t? [COMMAND]" },
#ifdef PROM
	{ "init",	_init,		"initialize:\tinit" },
#endif PROM
	{ "p",		put,		"put:\t\tp [-(b|h|w)] ADDRESS VALUE"},
	{ "printenv",	printenv,	"printenv:\tprintenv [ENV_VAR_LIST]" },
	{ "setenv",	setenv,		"setenv:\t\tsetenv ENV_VAR STRING" },
	{ "sload",	sload,		"sload:\t\tsload [-b] CHAR_DEVICE" },
	{ "spin",	spin,		"spin:\t\tspin [-i CNT] [[-v VAL] [-c CNT] [-(r|w)(b|h|w) ADDR]]*" },
	{ "unsetenv",	unsetenv,	"unsetenv:\tunsetenv ENV_VAR" },
	{ "warm",	warm_cmd,	"warm:\t\twarm" },
	{ "pr_tod",	pr_tod,		"pr_tod:\t\tpr_tod" },
	{ "init_tod",	init_tod,	"init_tod:\tinit_tod [SECS]" },
#ifdef RC6280
#ifdef PROM
	{ "diagmon",	diagmon,	"diag monitor:\tdiagmon" },
#endif PROM
#endif RC6280
	{ 0,		0,		"" }
};

extern	char	*getenv();

/*
 * main -- called from csu after prom data area has been cleared
 */
main(argc, argv)
int argc;
char **argv;
{
	extern char sccs_version[];
	extern char version[];
	char	*cp;
	char *scsi_id,sid;
	int target_id;

#ifdef PROM
	extern unsigned _icache_size, _dcache_size, _scache_size;
	C_SHARED(unsigned,total_memsize);

	extern int machine_type;
	extern char cpu_bd_rev;
	extern int _diff_ticks; 
#endif
#ifdef SASH
	extern char *index();
	extern int ignore_xoff;
#endif


#ifdef PROM
#ifdef R3030
	if (IS_R3030) _count_rambo_r3030();
#endif
	printf("\n%s %s Monitor: %s\n",
	       getenv("model"), getenv("vendor"), sccs_version);

	printf("Memory size: %d (0x%x) bytes, %d MB\n",
	    total_memsize[0], total_memsize[0], total_memsize[0]/(1024*1024));
	printf("Icache size: %d (0x%x) bytes\n",
	    _icache_size, _icache_size);
	printf("Dcache size: %d (0x%x) bytes\n",
	    _dcache_size, _dcache_size);
	if (_scache_size) {
		printf("Scache size: %d (0x%x) bytes\n",
		    _scache_size, _scache_size);
	}
#ifdef R3030
	if (IS_R3030) {
	    if (cpu_bd_rev & 0x20) { 		/* Rx3330 */
		if ((35 < _diff_ticks) || (_diff_ticks < 31))	/* nominal 33 */
		    printf("\nWarning: Rx3330 operating at %dMhz.\n\n",_diff_ticks);
	    }
	    else { 				/* Rx3230 */
		if ((27 < _diff_ticks) || (_diff_ticks < 23))	/* nominal 25 */
		    printf("\nWarning: Rx3230 operating at %dMhz.\n\n",_diff_ticks);
	    }
	}
#endif R3030
#endif PROM

#ifdef SASH
	init_env();
	ignore_xoff = 1;
	printf("Standalone Shell: %s\n", sccs_version);

	/*
	 * Next two lines are a temporary hack so lowsash can be faked
	 * in for kernel without infinite "boot loops" on autoboot
	 * testing
	 */
	if (argc > 0 && (cp = index(argv[0], ')'))
	    && strcmp(++cp, "vmunix") == 0)
		argc = 1;

	/*
	 * Deal with autoboots, remote debugging boots, and two-level
	 * boots
	 */
	if (argc >= 2 && strcmp(argv[1], "-a") == 0)
		boot2(kernel_name(argv[0]), &argv[1]);
	else if (argc >= 2 && strcmp(argv[1], "-r") == 0) {
		if (argc >= 3) {
			replace_str("dbgmon", "1", &environ_str);
			replace_str("rdebug", "1", &environ_str);
			boot2(argv[2], &argv[3]);
		} else
			printf("-r flag requires file to boot\n");
	} else if (argc >= 2)
		boot2(argv[1], &argv[2]);

#endif
	if (IS_R3030) {
	    target_id = 7;
	    sid = (scsi_id = getenv("scsi_id")) ? *scsi_id : 0;
	    if ((sid >= '0') && (sid < '8')) target_id = sid - '0';
	    verify_scsi_id(target_id);
	}

	setjmp(restart_buf);
	command_parser(cmd_table, PROMPT, 1);
}

/*
 * prom_error -- deal with internal prom coding error
 */
prom_error(msg)
char *msg;
{
	extern int *nofault;
	extern int ignore_xoff;

	ignore_xoff = 0;
	_set_timer(0);	/* cancel timer if running */
	nofault = 0;	/* cancel nofault processing */
#ifdef PROM
	printf("PROM ERROR detected in %s\n", msg);
	_init();
#endif
#ifdef SASH
	printf("SASH ERROR detected in %s\n", msg);
	_exit();
#endif
}
