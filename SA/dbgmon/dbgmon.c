#ident "$Header: dbgmon.c,v 1.6 90/05/31 15:38:34 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * dbgmon.c -- main loop for dbgmon
 */

#include "sys/types.h"
#include "sys/file.h"
#include "saio/setjmp.h"
#include "saio/parser.h"
#include "saio/saioctl.h"
#include "prom/entrypt.h"
#include "dbgmon/dbgmon.h"
#include "machine/cpu_board.h"

#define	PROMPT	"DBG: "

jmp_buf _restart_buf;
int Debug;
static void exception_close();

/*
 * commands
 */
extern int _cont(), _get(), _put(), _rdebug(), _string();
extern int _brk(), _call(), _go_to(), _step(), _Step(), _unbrk();
extern int _disass(), _calc(), _cacheflush();
extern int _dump(), help(), _quit();
extern int disable(), enable();
extern int _tlbdump(), _tlbflush(), _tlbvtop(), _tlbptov();
extern int _tlbpid(), _tlbmap(), _phystagdump(), _tlbmap_6000(), _tlbunmap();

/*
 * cmd_table -- interface between parser and command execution routines
 * Add new commands by making entry here.
 * There is now a seperate cmd_table for 6000s and non-6000s. If necessary
 * seperate cmd_tables for more machines can be created. There also
 * exists an array of pointers to these cmd_tables. And this array is indexed
 * according to machine_type.
 */
static struct cmd_table cmd_table_std[] = {
	{ "brk",	_brk,		"breakpoint:\tbrk [ADDRLIST]" },
	{ "c",		_cont,		"continue:\tc" },
 	{ "cacheflush",	_cacheflush,	"cacheflush:\tcacheflush [RANGE]" },
/****	{ "calc",	_calc,		"calculator:\tcalc" }, ****/
	{ "call",	_call,		"call:\t\tcall ADDR [ARGLIST]" },
	{ "debug",	_rdebug,	"debug:\t\tdebug [-r] CHAR_DEVICE" },
	{ "dis",	_disass,	"disassemble:\tdis RANGE" },
	{ "disable",	disable,	"disable:\tdisable CONSOLE_DEVICE" },
	{ "dump",	_dump,		"dump:\t\tdump [-(b|h|w)] [-(o|d|x|c)] RANGE" },
	{ "enable",	enable,		"enable:\t\tenable CONSOLE_DEVICE" },
	{ "g",		_get,		"get:\t\tg ADDRESS_OR_REGISTER" },
	{ "goto",	_go_to,		"goto:\t\tgoto ADDR" },
	{ "help",	help,		"help:\t\thelp [COMMAND]" },
	{ "?",		help,		"help:\t\t? [COMMAND]" },
	{ "p",		_put,		"put:\t\tp ADDRESS_OR_REGISTER NUMBER"},
	{ "quit",	_quit,		"quit:\t\tquit" },
	{ "s",		_step,		"step:\t\ts [COUNT]" },
	{ "S",		_Step,		"Step:\t\tS [COUNT]" },
	{ "string",	_string,	"string:\t\tstring ADDR [MAXLEN]" },
	{ "tlbdump",	_tlbdump,	"tlbdump:\ttlbdump [RANGE]" },
	{ "tlbflush",	_tlbflush,	"tlbflush:\ttlbflush [RANGE]" },
	{ "tlbmap",	_tlbmap,	"tlbmap:\t\ttlbmap [-i INX] [-(n|d|v|g)]* VADDR PADDR PID" },
	{ "tlbpid",	_tlbpid,	"tlbpid:\t\ttlbpid [PID]" },
	{ "tlbptov",	_tlbptov,	"tlbptov:\ttlbptov ADDR" },
	{ "tlbvtop",	_tlbvtop,	"tlbvtop:\ttlbvtop ADDR [PID]" },
	{ "unbrk",	_unbrk,		"unbrk:\t\tunbrk [BPNUMLIST]" },
	{ 0,		0,		"" }
};

static struct cmd_table cmd_table_6000[] = {
	{ "brk",	_brk,		"breakpoint:\tbrk [ADDRLIST]" },
	{ "c",		_cont,		"continue:\tc" },
 	{ "cacheflush",	_cacheflush,	"cacheflush:\tcacheflush [RANGE]" },
/***	{ "calc",	_calc,		"calculator:\tcalc" }, ***/
	{ "call",	_call,		"call:\t\tcall ADDR [ARGLIST]" },
	{ "debug",	_rdebug,	"debug:\t\tdebug [-r] CHAR_DEVICE" },
	{ "dis",	_disass,	"disassemble:\tdis RANGE" },
	{ "disable",	disable,	"disable:\tdisable CONSOLE_DEVICE" },
	{ "dump",	_dump,		"dump:\t\tdump [-(b|h|w)] [-(o|d|x|c)] RANGE" },
	{ "enable",	enable,		"enable:\t\tenable CONSOLE_DEVICE" },
	{ "g",		_get,		"get:\t\tg ADDRESS_OR_REGISTER" },
	{ "goto",	_go_to,		"goto:\t\tgoto ADDR" },
	{ "help",	help,		"help:\t\thelp [COMMAND]" },
	{ "?",		help,		"help:\t\t? [COMMAND]" },
	{ "p",		_put,		"put:\t\tp ADDRESS_OR_REGISTER NUMBER"},
	{ "ptagdump",	_phystagdump,	"ptagdump:\tptagdump RANGE" },
	{ "quit",	_quit,		"quit:\t\tquit" },
	{ "s",		_step,		"step:\t\ts [COUNT]" },
	{ "S",		_Step,		"Step:\t\tS [COUNT]" },
	{ "string",	_string,	"string:\t\tstring ADDR [MAXLEN]" },
	{ "tlbdump",	_tlbdump,	"tlbdump:\ttlbdump RANGE" },
	{ "tlbmap",	_tlbmap_6000,	"tlbmap:\t\ttlbmap -i SIDE [-(n|d|g)]* VADDR PADDR [PID]" },
	{ "tlbpid",	_tlbpid,	"tlbpid:\t\ttlbpid [PID]" },
	{ "tlbunmap",	_tlbunmap,	"tlbunmap:\ttlbunmap VADDR [PID]" },
	{ "tlbvtop",	_tlbvtop,	"tlbvtop:\ttlbvtop ADDR [PID]" },
	{ "unbrk",	_unbrk,		"unbrk:\t\tunbrk [BPNUMLIST]" },
	{ 0,		0,		"" }
};

/* 
 * array of pointers to appropriate cmd_table structs for each machine_type
 */

static struct cmd_table *_cmd_table[] = {
			cmd_table_std,			/* M500 */
			cmd_table_std,			/* M800 */
			cmd_table_std,			/* M1000 */
			cmd_table_std,			/* M120 */
			cmd_table_std,			/* M2000 */
			cmd_table_6000,			/* EXCALIBUR */
			cmd_table_std,			/* M20 */
			cmd_table_std,			/* M20 sable */
			cmd_table_std,			/* M180 */
			cmd_table_std,			/* Pizazz */
			cmd_table_std,			/* Genesis */
			};

static char *debug_argv[] = {
	"debug",
	"tty(1)",
	0
};
#define	DEBUG_ARGC	(sizeof(debug_argv)/sizeof(debug_argv[0]) - 1)
#define	streq(a,b)	(strcmp(a,b) == 0)

/*
 * _dbgmon -- called from csu
 */
_dbgmon(argc, argv)
char **argv;
{
	register struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;
	int filelen;
	extern int breakpoint();
	extern int _regs[];
	extern char *sccs_version;

	printf("MIPS SA Debugger %s\n", sccs_version);
	rb->rb_bpaddr = breakpoint;
	atob(getenv("DEBUG"), &Debug);
	filelen = argc > 0 ? strlen(argv[0]) : 0;
	if (getenv("rdebug")
	    || (filelen > 4 && streq(".dbg", &argv[0][filelen-4])))  {
		printf("remote debugging mode\n");
		if (setjmp(_restart_buf))
			goto command_mode;
		_rdebug(DEBUG_ARGC, debug_argv);
	}
	_show_inst(_regs[R_EPC]);
	setjmp(_restart_buf);
command_mode:
	sa_spl();
	command_parser(MACHDEP(_cmd_table), PROMPT, 0);
}

/*
 * _fatal_error -- deal with internal coding error
 */
_fatal_error(msg)
char *msg;
{
	extern int *nofault;

	nofault = 0;	/* cancel nofault processing */
	_hook_exceptions();
	printf("DEBUGGER ERROR detected in %s\n", msg);
	/* careful here */
	_restart();	/* enter command mode */
}

/*
 * error -- print error message for syntax and other errors
 */
_dbg_error(msg, arg1, arg2)
char *msg;
int arg1, arg2;
{
	int *jbp;

	_hook_exceptions();	/* incase halfway through a load/boot */
	printf("Error: ");
	printf(msg, arg1, arg2);
	putchar('\n');
	/*
	 * Check for nofault in case remote debugging is active
	 */
	if (nofault) {
		jbp = nofault;
		nofault = 0;
		longjmp(jbp, 1);
	}
	_restart();
}

/*
 * restart -- reset stack and return to main loop
 */
_restart()
{
	extern int _prom_mode;

	_hook_exceptions();
	_prom_mode = MODE_DBGMON;
	exception_close();
	longjmp(_restart_buf, 1);
}

#define	MAXCLOSE	3
static close_cnt;
static close_list[MAXCLOSE];

/*
 * close_on_exception -- mark a descriptor to be closed on any exception
 */
void
close_on_exception(fd)
{
	if (close_cnt < MAXCLOSE)
		close_list[close_cnt++] = fd;
}

/*
 * exception_close -- close the descriptors which have been marked to be
 * closed on exception
 */
static void
exception_close()
{
	while (close_cnt > 0)
		close(close_list[--close_cnt]);
	close_cnt = 0;
}
