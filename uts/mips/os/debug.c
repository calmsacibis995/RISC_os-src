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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: debug.c,v 1.41.1.9.1.1.1.7 90/12/20 19:30:18 beacker Exp $"

#include "sys/debug.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/fs/s5dir.h"
#include "sys/signal.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/immu.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/mipskopt.h"
#include "sys/ioa.h"
#include "sys/fpu.h"

extern char *memory_limit;
extern int _nfs_sync_writes;
extern int _posix_chown_restricted;
extern int _posix_vdisable;
extern int _riscos_group_parent;
extern int _riscos_utime_owner;
extern int _riscos_link_owner;
extern int _riscos_kill_stopped_orphans;
extern int _riscos_max_dquot_cache;

#ifdef SABLE
char	*arg_root = (char *)1;		/* so we can boot from ram disk */
#else
char	*arg_root;
#endif SABLE
char	*arg_rootname;
char	*arg_swap;
char	*arg_dump;
char	*prom_version;
int	prom_mon;
int 	console_panic_enabled=0;
char	*arg_initstate;
char	*arg_initfile;
int	showconfig;
int	scsiexterr = 0;		/* default scsi extended error print flag */
int	scsi_id = 0x7;		/* default scsi id */
int	scsi_reset = 0x1;	/* default is to reset scsi bus */
int	scsi_plock = 0x1;	/* default is to enable partition locking */
int	disable_parity;
int	user_error;		/* If this bit is set two things will happen.
				 * First all programs that get a sigsegv or
				 * sigbus will have the program name, pc and
				 * badvaddr printed.  Second, any time errno
				 * is set when returning from a system call
				 * the program name, syscall number and error
				 * will be printed.
				 */
int	isd_syncmode = 0;	/* syncronous is working.  If
				 * this flag is set the drivers will try to
				 * use disconnect/reconnect.
				 */
int	sha_disc = 1;		/* Disconnect is working.  If
				 * this flag is set the drivers will try to
				 * use disconnect/reconnect.
				 */
int	blank = 300;		/* the default number of seconds before
				 * a workstation console in terminal
				 * emulator mode will go blank 
				 */
int	uart_silo;		/* silo mode appears to work.  I don't trust
				 * it yet because both Russ and I have seen
				 * mysterious hangs and disk problems which
				 * have only started happening since silo
				 * mode was turned on.  This variable allows
				 * me to build on kernel that can have silo
				 * mode on as a option. */
int	limitmem;
int	dbg_qt;
int	dbg_uart;
int	dbg_kbd;
int	cheapdbg;
int	ecc_noprint;
int	cache_bufs;	/* set true if disk bufs should be in cached space */
int	nocache_bufs;	/* set true if disk bufs should  be in UNcached space */
int	check_mem_at_vfault;/* set true if hogs free mem before getting more */
extern int	disable_macsi;
extern int	egl_noroundup;	/* No need to set this for 62x0 machines since
				 * we don't do roundup.
				 * Set true to disable the Eagle transmit
				 * packet roundup to a multiple of 4 bytes.
				 * Roundup is performed so controller can use
				 * block mode to obtain packet.  Setting this
				 * flag will disable roundup & cause controller
				 * to NOT use block mode on packets whose
				 * length is not a four byte multiple.  Only
				 * use to talk to machines which can't handle
				 * the extra bytes -- VME transfer rate drops
				 * from 30 MB/sec to 5 MB/sec.
				 */
extern int	_riscos_ttys_default_clocal;
extern int	_riscos_idle_clear_pages;
extern char	*console;	/* console ala monitor variable */
extern char	*keyswtch;	/* keyswtch ala monitor variable */

extern int machine_type; 

char	**save_argv, **save_environ;

/*
 * The following is a table of symbolic names and addresses of kernel
 * variables which can be tuned to alter the performance of the system.
 * They can be modified at boot time as a boot parameter or by the mipskopt
 * system call.  Variables marked as readonly can't be modifed after system
 * boot time (i.e. through the mipskopt call).  "func" is called after the
 * variable is set in case there is processing beyond storing the new value.
 */
extern struct	kernargs kernargs[];

/*
 * Convert the ascii representation of a digit to its binary representation
 */
static unsigned
digit(c)
	register char c;
{
	unsigned d;

	if (c >= '0' && c <= '9')
		d = c - '0';
	else if (c >= 'a' && c <= 'f')
		d = c - 'a' + 10;
	else if (c >= 'A' && c <= 'F')
		d = c - 'A' + 10;
	else
		d = 999999; /* larger than any base to break callers loop */
	return(d);
}

/*
 * Convert ascii to integer.  Accepts all C numeric formats.
 */
int
atoi(cp)
	register char *cp;
{
	int minus = 0;
	register int value = 0;
	unsigned base = 10;
	unsigned d;
	char *oldcp = cp;

	ASSERT(cp != NULL);

	while (*cp == ' ' || *cp == '\t')
		cp++;

	while (*cp == '-') {
		cp++;
		minus = !minus;
	}

	/*
	 * Determine base by looking at first 2 characters
	 */
	if (*cp == '0') {
		switch (*++cp) {
		case 'X':
		case 'x':
			base = 16;
			cp++;
			break;

		case 'B':	/* a frill: allow binary base */
		case 'b':
			base = 2;
			cp++;
			break;
		
		default:
			base = 8;
			break;
		}
	}

	while ((d = digit(*cp)) < base) {
		value *= base;
		value += d;
		cp++;
	}

	if (minus)
		value = -value;

	if (*cp)
		dprintf("WARNING: badly formed numeric argument \"%s\"\n",
				  oldcp);
	return (value);
}

#ifndef SABLE
/*
 * Parse kernel arguments.  Kernel arguments have the following syntax:
 *	"arg=value", where value is a C style number (decimal, octal, hex)
 *	"arg=string", where string is a simple alpha-numeric sequence
 *	"arg", which just defines arg to have a value of "1"
 * For string arguments, the string cannot begin with a "-" or a
 * "0" throuh "9"
 *
 * The kernel must copy string arguments to someplace safe because the
 * location they are passed in at may be zeroed soon.  (If the kernel is
 * loaded directly from prom the arguments are safe, but if loaded from
 * sash then they are in memory the kernel will zero.)
 *
 * The value "initarg=" is a special case.  There may be multiple
 * "initarg=" values passed in, and they all get copied into the argument
 * vector of icode for the init program.  So, for instance:
 *
 *	>> boot dkip()unix initfile=/bin/mv initarg=/etc/inittab initarg=/foo
 *
 * is a sneaky way of cleaning up w/out a miniroot if you messed up.
 */
void
getargs(argc, argv, environ)
	int argc;
	char *argv[];
	char *environ[];
{
	register struct kernargs *kp;
	register int i;
	register char *cp;
	register char **argp;
#define ARGBUFSZ 512			/* This could go somewhere else */
	static char argbuf[ARGBUFSZ];	/* Safe place to copy arguments. */
	int argbufi = 0;		/* Index to unused part of argbuf. */

	if ((unsigned int)argv < 0x80000000) {
		return;
	}
	save_argv = argv;
	save_environ = environ;
	/*
	 *  RC2030/RS203 do not use nvram, but environment instead.
	 */
	if(environ == 0)
		return;
	environ_save(environ);		/* stub in all but R?2030 */
	argp = environ;

loop:
	for (; *argp; argp++) {
		if (strncmp("initarg=", *argp, 8) == 0) {
			extern int icode[];
			extern char *icode_args[];
			extern char *icode_argv[];
			extern int icode_argc[];

			cp = &(*argp)[8];

			icode_argv[icode_argc[0]++] =
			    (char *)(icode_args[0] - (char *)icode + USRDATA);
			do {
				*icode_args[0]++ = *cp;
			} while (*cp++);
			icode_argv[icode_argc[0]] = 0;
			continue;
		}
		for (kp = kernargs; kp->name; kp++) {
			i = strlen(kp->name);
			if (strncmp(kp->name, *argp, i) == 0) {
				if (showconfig)
				  cmn_err (CE_CONT, "%s=", kp->name);
				cp = &((*argp)[i]);
				if (*cp == 0) {
					*kp->ptr = 1;
					if (showconfig)
					  cmn_err (CE_CONT, "1\n");
				}
				else if (*cp == '=') {
					cp++;
					while ((*cp == ' ') || (*cp == '\t'))
						cp++;
					if ((kp->dostring == 0) &&
						((*cp>='0') && (*cp<='9')) ||
					    (*cp == '-')) {
					    if (showconfig)
					      cmn_err (CE_CONT, "%s\n", cp);
					    if (kp->boolean) 
						*kp->ptr = (atoi(cp) ? -1 : 0);
					    else
						*kp->ptr = atoi(cp);
					} else {
					    /* copy the string somewhere safe */
					    i=strlen(cp) + 1;
					    strncpy(&argbuf[argbufi], cp, i);
					    *kp->ptr = (int)&argbuf[argbufi];
					    argbufi += i;
					    if (showconfig)
					      cmn_err (CE_CONT,
						       "%s\n", *kp->ptr);
					    if (kp->boolean)
						*kp->ptr = -1;
					}
				} else
					continue;
				break;
			}
		}
	}
	if (argv) {
		argp = argv+1;	/* skip boot device */
		argv = 0;
		goto loop;
	}
}
/*
 *  Just like getargs, but looks for console= only, special need of R?2030
 */
void
con_getargs(argc, argv, environ)
	int argc;
	char *argv[];
	char *environ[];
{
	register struct kernargs *kp;
	register int i;
	register char *cp;
	register char **argp;
#define CON_ARGBUFSZ 16			/* This could go somewhere else */
	static char con_argbuf[ARGBUFSZ]; /* Safe place to copy arguments. */

	if ((unsigned int)argv < 0x80000000) {
		return;
	}
	if(environ == 0)
		return;
	argp = environ;
con_loop:
	for (; *argp; argp++) {
		if (strncmp("console=", *argp, 8) == 0) {
			/* copy console definition to con_argbuf */
			cp = &(*argp)[8];
			while ((*cp == ' ') || (*cp == '\t'))
				cp++;
			i=strlen(cp) + 1;
			strncpy(&con_argbuf[0], cp, i);
			for (kp = kernargs; kp->name; kp++) {
				i = strlen(kp->name);
				if (strncmp(kp->name, "console", i) == 0) {
					*kp->ptr = (int)&con_argbuf[0];
					break;
				}
			}
		}
	}
	if (argv) {
		argp = argv+1;	/* skip boot device */
		argv = 0;
		goto con_loop;
	}
}
#endif	/* !SABLE */

/*
 * System call which allows a user to read/write kernel variables specified
 * in the above table by symbolic name.  Only the super-user is allowed to
 * write non-readonly variables.
 */
mipskopt()
{
	register struct a {
		char *argname;
		int value;
		int op;
	} *uap;
#define MAXKVARNAME 256
	char nbuf[MAXKVARNAME];
	register struct kernargs *kp;
	int lencopied;

	uap = (struct a *)u.u_ap;
	u.u_error = copyinstr(uap->argname, nbuf, MAXKVARNAME, &lencopied);
	if (u.u_error == ENAMETOOLONG || lencopied == 0)
		u.u_error = EINVAL;
	if (u.u_error)
		return;

	for (kp = kernargs; kp->name; kp++)
		if (strcmp(kp->name, nbuf) == 0) {
			u.u_error = do_opt(kp, uap->value, uap->op);
			return;
		}
	
	u.u_error = EINVAL;
}

do_opt(kp, val, op)
struct kernargs *kp;
{
	/* return old value */
	u.u_rval1 = *(kp->ptr);

	if (op != KOPT_GET && (!suser() || kp->readonly))
		return (EPERM);

	switch (op) {
	case KOPT_GET:
		return(0);

	case KOPT_SET:
		*(kp->ptr) = val;
		break;

	case KOPT_BIS:
		*(kp->ptr) |= val;
		break;

	case KOPT_BIC:
		*(kp->ptr) &= ~val;
		break;

	default:
		return(EINVAL);
	}
	if (kp->boolean &&
	    *(kp->ptr) != 0)
		*(kp->ptr) = -1;
	return(0);
}

/*
 * ????
 * ADD DESCRIPTIONS FOR CMAPS
 */

struct reg_values ibusmast_values[] = {
	{ (IBUS_PCAT4<<1),		"Cause: AT-L4" },
	{ (IBUS_PCAT3<<1),		"Cause: AT-L3" },
	{ (IBUS_PCAT2<<1),		"Cause: AT-L2" },
	{ (IBUS_PCAT1<<1),		"Cause: AT-L1" },
	{ (IBUS_CHAIN<<1),		"Cause: CHAIN" },
	{ (IBUS_PCATDMA<<1),		"Cause: ATDMA" },
	{ (IBUS_SCSI<<1),		"Cause: SCSI" },
	{ (IBUS_LANCE<<1),		"Cause: LANCE" },
	{ (IBUS_PCAT4<<1)|1,		"" },
	{ (IBUS_PCAT3<<1)|1,		"" },
	{ (IBUS_PCAT2<<1)|1,		"" },
	{ (IBUS_PCAT1<<1)|1,		"" },
	{ (IBUS_CHAIN<<1)|1,		"" },
	{ (IBUS_PCATDMA<<1)|1,		"" },
	{ (IBUS_SCSI<<1)|1,		"" },
	{ (IBUS_LANCE<<1)|1,		"" },
	{ 0,			0 }
};

struct reg_values accesstype_values[] = {
	{ ACCTYPEB_WORD,	"WORD" },
	{ ACCTYPEB_TRIBYTE,	"TRIBYTE" },
	{ ACCTYPEB_HALFWORD,	"HALFWORD" },
	{ ACCTYPEB_BYTE,	"BYTE" },
	{ 0,			0 }
};

struct reg_values parity_values[] = {
	{ FID_PARER0B, "1/2/3" },
	{ FID_PARER1B, "0/2/3" },
	{ FID_PARER2B, "0/1/3" },
	{ FID_PARER3B, "0/1/2" },
	{ FID_PARER0B|FID_PARER1B, "2/3" },
	{ FID_PARER0B|FID_PARER2B, "1/3" },
	{ FID_PARER0B|FID_PARER3B, "1/2" },
	{ FID_PARER1B|FID_PARER2B, "0/3" },
	{ FID_PARER1B|FID_PARER3B, "0/2" },
	{ FID_PARER2B|FID_PARER3B, "0/1" },
	{ FID_PARER0B|FID_PARER1B|FID_PARER2B, "3" },
	{ FID_PARER0B|FID_PARER1B|FID_PARER3B, "2" },
	{ FID_PARER0B|FID_PARER2B|FID_PARER3B, "1" },
	{ FID_PARER1B|FID_PARER2B|FID_PARER3B, "0" },
	{ FID_PARER0B|FID_PARER1B|FID_PARER2B|FID_PARER3B, "None" },
	{ 0,			"0/1/2/3" },
	{ 0,			0 }
};

struct reg_values iopoeb_values[] = {
	{ 0,			"Cause: IOP" },
	{ FID_IOPOEB,		"" },
	{ 0,			0 }
};

struct reg_values promflt_values[] = {
	{ 0,			"Cause: Write Prom" },
	{ FID_PROMFAULTB,	""},
	{ 0,			0 }
};
struct reg_values rw_values[] = {
	{ 0,			"Write" },
	{ FID_MREAD,		"Read" },
	{ 0,			0 }
};

struct reg_desc fid_desc[] = {
	/* mask	     shift      name   format  values */
	{ FID_PROCBRD,		0,	"Cause:ProcBrd",	NULL,	NULL },
	{ FID_IBUSMAST|FID_IBUSVALIDB,	-IBUSVALIDB_SHIFT,	NULL,	NULL,	ibusmast_values },
/*	{ FID_IOPOEB,		0,	NULL,		NULL,	iopoeb_values }, */
/*	{ FID_PROMFAULTB,	0,	NULL,		NULL,	promflt_values }, */
	{ FID_TIMEOUT,		0,	"Cause:BusTO",	NULL,	NULL },
	{ FID_MREAD,		0,	"Type",		NULL,	rw_values },
	{ FID_ACCTYPEB, -ACCTYPEB_SHIFT, "AccType",	NULL,	accesstype_values },
	{ FID_PARERB,		0,	"Parity Error Bytes",	NULL,	parity_values },
	{	0,		0,		0,	0,	0 }
};

struct reg_desc cp0_error_desc[] = {
	/* mask			shift	name   format  values */
	{ C0_ERROR_IEXT,	0,	"InhibitExternal",	NULL,	NULL },
	{ C0_ERROR_IRF,		0,	"InhibitRegFile",	NULL,	NULL },
	{ C0_ERROR_IDB,		0,	"InhibitDbus",		NULL,	NULL },
	{ C0_ERROR_IIB,		0,	"InhibitIbus",		NULL,	NULL },
	{ C0_ERROR_EXT,		0,	"ExternalParity",	NULL,	NULL },
	{ C0_ERROR_RF,		0,	"RegisterFileParity",	NULL,	NULL },
	{ C0_ERROR_DB,		0,	"DataBusParity",	NULL,	NULL },
	{ C0_ERROR_IB,		0,	"InstrBusParity",	NULL,	NULL },
	{ 0,			0,	NULL,			NULL,	NULL },
};

struct reg_desc fpc_parity_desc[] = {
	/* mask			shift	name   format  values */
	{ FPPARITY_FRF,		0,	"ForceRegFileErr",	NULL,	NULL },
	{ FPPARITY_IRF,		0,	"InhibitRegFile",	NULL,	NULL },
	{ FPPARITY_IIB,		0,	"InhibitIbus",		NULL,	NULL },
	{ FPPARITY_RF,		0,	"RegisterFileParity",	NULL,	NULL },
	{ FPPARITY_IB,		0,	"InstrBusParity",	NULL,	NULL },
	{ 0,			0,	NULL,			NULL,	NULL },
};

struct reg_desc ioc_errorinfo_desc[] = {
	/* mask			shift	name			format  value */
	{ G0_SEC_ERR,		0,	"G0_2ndErr",		NULL,	NULL },
	{ G0_ADMUX_PTY_ERR,	0,	"G0_ADmuxParity",	NULL,	NULL },
	{ G0_DOUT_PTY_ERR,	0,	"G0_DoutParity",	NULL,	NULL },
	{ G0_BUS_ERR,		0,	"G0_BusErr",		NULL,	NULL },
	{ G0_ERR,		0,	"G0_Err",		NULL,	NULL },
	{ G0_BUS_FREE,		0,	"G0_BusFree",		NULL,	NULL },
	{ G0_BRD_FREE,		0,	"G0_BrdFree",		NULL,	NULL },
	{ G1_SEC_ERR,		0,	"G1_2ndErr",		NULL,	NULL },
	{ G1_ADMUX_PTY_ERR,	0,	"G1_ADmuxParity",	NULL,	NULL },
	{ G1_DOUT_PTY_ERR,	0,	"G1_DoutParity",	NULL,	NULL },
	{ G1_BUS_ERR,		0,	"G1_BusErr",		NULL,	NULL },
	{ G1_ERR,		0,	"G1_Err",		NULL,	NULL },
	{ G1_BUS_FREE,		0,	"G1_BusFree",		NULL,	NULL },
	{ G1_BRD_FREE,		0,	"G1_BrdFree",		NULL,	NULL },
	{ IOC_DUART_INT,	0,	"IOC_DuartInt",		NULL,	NULL },
	{ GBA0_EXISTS,		0,	"G0_exists",		NULL,	NULL },
	{ GBA1_EXISTS,		0,	"G1_exists",		NULL,	NULL },
	{ IOC_ENA_FORCE_BUSY,	0,	"IOC_EnaForceBusy",	NULL,	NULL },
	{ 0,			0,	NULL,			NULL,	NULL },
};

struct reg_values gba_pty_err_mask_values[] = {
	{ 0,			"ok" },
	{ GBA_PARITY_RCVD_OP,	"RcvdOp" },
	{ GBA_PARITY_RD_MISS,	"RdMiss" },
	{ GBA_PARITY_PREFETCH,	"Prefetch" },
	{ GBA_PARITY_TOUCH,	"Touch" },
	{ 0,			NULL },
};

struct reg_desc gba_status_desc[] = {
	/* mask			shift	name			format  value */
	{ GBA_TIMEOUT,		0,	"Timeout",		NULL,	NULL },
	{ GBA_MASTER_BUS_ERR,	0,	"MasterBusErr",		NULL,	NULL },
	{ GBA_PARITY_ERR_MASK,	0,	"Parity",NULL,gba_pty_err_mask_values },
	{ GBA_DC_FAIL,		0,	"DCfail",		NULL,	NULL },
	{ GBA_AC_FAIL,		0,	"ACfail",		NULL,	NULL },
	{ GBA_RESET_MODE,	0,	"ResetMode",		NULL,	NULL },
	{ 0,			0,	NULL,			NULL,	NULL },
};


#ifdef REG_VALUES

struct reg_values pstat_values[] = {
	{ SSLEEP,		"SLEEP" },
	{ SWAIT,		"WAIT" },
	{ SRUN,			"RUN" },
	{ SIDL,			"IDL" },
	{ SZOMB,		"ZOMB" },
	{ SSTOP,		"STOP" },
	{ 0,			0 }
};

struct reg_values sig_values[] = {
	{ SIGHUP,		"SIGHUP" },
	{ SIGINT,		"SIGINT" },
	{ SIGQUIT,		"SIGQUIT" },
	{ SIGILL,		"SIGILL" },
	{ SIGTRAP,		"SIGTRAP" },
	{ SIGIOT,		"SIGIOT" },
	{ SIGEMT,		"SIGEMT" },
	{ SIGFPE,		"SIGFPE" },
	{ SIGKILL,		"SIGKILL" },
	{ SIGBUS,		"SIGBUS" },
	{ SIGSEGV,		"SIGSEGV" },
	{ SIGSYS,		"SIGSYS" },
	{ SIGPIPE,		"SIGPIPE" },
	{ SIGALRM,		"SIGALRM" },
	{ SIGTERM,		"SIGTERM" },
	{ SIGURG,		"SIGURG" },
	{ SIGSTOP,		"SIGSTOP" },
	{ SIGTSTP,		"SIGTSTP" },
	{ SIGCONT,		"SIGCONT" },
	{ SIGCHLD,		"SIGCHLD" },
	{ SIGTTIN,		"SIGTTIN" },
	{ SIGTTOU,		"SIGTTOU" },
	{ SIGIO,		"SIGIO" },
	{ SIGXCPU,		"SIGXCPU" },
	{ SIGXFSZ,		"SIGXFSZ" },
	{ SIGVTALRM,		"SIGVTALRM" },
	{ SIGPROF,		"SIGPROF" },
	{ 0,			0 },
};

struct reg_values imask_values[] = {
	{ SR_IMASK8,	"8" },
	{ SR_IMASK7,	"7" },
	{ SR_IMASK6,	"6" },
	{ SR_IMASK5,	"5" },
	{ SR_IMASK4,	"4" },
	{ SR_IMASK3,	"3" },
	{ SR_IMASK2,	"2" },
	{ SR_IMASK1,	"1" },
	{ SR_IMASK0,	"0" },
	{ 0,		NULL },
};

struct reg_desc sr_desc[] = {
	/* mask	     shift      name   format  values */
	{ SR_CU3,	0,	"CU3",	NULL,	NULL },
	{ SR_CU2,	0,	"CU2",	NULL,	NULL },
	{ SR_CU1,	0,	"CU1",	NULL,	NULL },
	{ SR_CU0,	0,	"CU0",	NULL,	NULL },
	{ SR_BEV,	0,	"BEV",	NULL,	NULL },
	{ SR_TS,	0,	"TS",	NULL,	NULL },
	{ SR_PE,	0,	"PE",	NULL,	NULL },
	{ SR_CM,	0,	"CM",	NULL,	NULL },
	{ SR_PZ,	0,	"PZ",	NULL,	NULL },
	{ SR_SWC,	0,	"SwC",	NULL,	NULL },
	{ SR_ISC,	0,	"IsC",	NULL,	NULL },
	{ SR_IBIT8,	0,	"IM8",	NULL,	NULL },
	{ SR_IBIT7,	0,	"IM7",	NULL,	NULL },
	{ SR_IBIT6,	0,	"IM6",	NULL,	NULL },
	{ SR_IBIT5,	0,	"IM5",	NULL,	NULL },
	{ SR_IBIT4,	0,	"IM4",	NULL,	NULL },
	{ SR_IBIT3,	0,	"IM3",	NULL,	NULL },
	{ SR_IBIT2,	0,	"IM2",	NULL,	NULL },
	{ SR_IBIT1,	0,	"IM1",	NULL,	NULL },
	{ SR_IMASK,	0,	"IPL",	NULL,	imask_values },
	{ SR_KUO,	0,	"KUo",	NULL,	NULL },
	{ SR_IEO,	0,	"IEo",	NULL,	NULL },
	{ SR_KUP,	0,	"KUp",	NULL,	NULL },
	{ SR_IEP,	0,	"IEp",	NULL,	NULL },
	{ SR_KUC,	0,	"KUc",	NULL,	NULL },
	{ SR_IEC,	0,	"IEc",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_values exc_values[] = {
	{ EXC_INT,	"INT" },
	{ EXC_MOD,	"MOD" },
	{ EXC_RMISS,	"RMISS" },
	{ EXC_WMISS,	"WMISS" },
	{ EXC_RADE,	"RADE" },
	{ EXC_WADE,	"WADE" },
	{ EXC_IBE,	"IBE" },
	{ EXC_DBE,	"DBE" },
	{ EXC_SYSCALL,	"SYSCALL" },
	{ EXC_BREAK,	"BREAK" },
	{ EXC_II,	"II" },
	{ EXC_CPU,	"CPU" },
	{ EXC_OV,	"OV" },
	{ SEXC_SEGV,	"SW_SEGV" },
	{ SEXC_RESCHED,	"SW_RESCHED" },
	{ SEXC_PAGEIN,	"SW_PAGEIN" },
	{ SEXC_CPU,	"SW_CP_UNUSABLE" },
	{ 0,		NULL },
};

struct reg_desc exccode_desc[] = {
	/* mask	     shift      name   format  values */
	{ 1,		0,	"USER",	NULL,	NULL },
	{ CAUSE_EXCMASK,0,	"EXC",	NULL,	exc_values },
	{ 0,		0,	NULL,	NULL,	NULL }
};

struct reg_desc cause_desc[] = {
	/* mask	     shift      name   format  values */
	{ CAUSE_BD,	0,	"BD",	NULL,	NULL },
	{ CAUSE_CEMASK,	-CAUSE_CESHIFT,	"CE",	"%d",	NULL },
	{ CAUSE_IP8,	0,	"IP8",	NULL,	NULL },
	{ CAUSE_IP7,	0,	"IP7",	NULL,	NULL },
	{ CAUSE_IP6,	0,	"IP6",	NULL,	NULL },
	{ CAUSE_IP5,	0,	"IP5",	NULL,	NULL },
	{ CAUSE_IP4,	0,	"IP4",	NULL,	NULL },
	{ CAUSE_IP3,	0,	"IP3",	NULL,	NULL },
	{ CAUSE_SW2,	0,	"SW2",	NULL,	NULL },
	{ CAUSE_SW1,	0,	"SW1",	NULL,	NULL },
	{ CAUSE_EXCMASK,0,	"EXC",	NULL,	exc_values },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc tlbhi_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBHI_VPNMASK,0,	"VA",	"0x%x",	NULL },
	{ TLBHI_PIDMASK,-TLBHI_PIDSHIFT,"PID",	"%d",	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc tlblo_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBLO_PFNMASK,0,	"PA",	"0x%x",	NULL },
	{ TLBLO_N,	0,	"N",	NULL,	NULL },
	{ TLBLO_D,	0,	"D",	NULL,	NULL },
	{ TLBLO_V,	0,	"V",	NULL,	NULL },
	{ TLBLO_G,	0,	"G",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc tlbinx_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBINX_PROBE,	0,	"PFAIL",NULL,	NULL },
	{ TLBINX_INXMASK, -TLBINX_INXSHIFT, "INDEX", "%d", NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc tlbrand_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBRAND_RANDMASK, -TLBRAND_RANDSHIFT, "RANDOM", "%d", NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc tlbctxt_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBCTXT_BASEMASK, 0,	"PTEBASE", "0x%x", NULL },
	{ TLBCTXT_VPNMASK, 11,	"BADVAP", "0x%x", NULL},
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_values fileno_values[] = {
	/* value			name */
	{ PG_FZERO>>PTE_FILENOSHIFT,	"ZERO" },
	{ PG_FTEXT>>PTE_FILENOSHIFT,	"TEXT" },
	{ 0,				NULL }
};

struct reg_values prot_values[] = {
	/* value			name */
	{ PG_KR,			"KR" },
	{ PG_KW,			"KW" },
	{ PG_URKR,			"URKR" },
	{ PG_UW,			"UW" },
	{ 0,				NULL }
};

struct reg_desc pte_desc[] = {
	/* mask	     shift      name   format  values */
	{ PG_PFNUM,	0,	"PADDR", "0x%x", NULL },
	{ PG_N,		0,	"N",	NULL,	NULL },
	{ PG_M,		0,	"M",	NULL,	NULL },
	{ PG_V,		0,	"V",	NULL,	NULL },
	{ PG_G,		0,	"G",	NULL,	NULL },
	{ PG_FILENO,	PTE_FILENOSHIFT,"FILENO", "%d",	fileno_values },
	{ PG_SWAPM,	0,	"SWAPM",NULL,	NULL },
	{ PG_FOD,	0,	"FOD",	NULL,	NULL },
	{ PG_PROT,	0,	"PROT",	NULL,	prot_values },
	{ 0,		0,	NULL,	NULL,	NULL }
};

struct reg_values syscall_values[] = {
	/* value			name */
	{ SYS_syscall,			"syscall" },
	{ SYS_exit,			"exit" },
	{ SYS_fork,			"fork" },
	{ SYS_read,			"read" },
	{ SYS_write,			"write" },
	{ SYS_open,			"open" },
	{ SYS_close,			"close" },
	{ 7,				"old_wait" },
	{ SYS_creat,			"creat" },
	{ SYS_link,			"link" },
	{ SYS_unlink,			"unlink" },
	{ SYS_execv,			"execv" },
	{ SYS_chdir,			"chdir" },
	{ 13,				"old_time" },
	{ SYS_mknod,			"mknod" },
	{ SYS_chmod,			"chmod" },
	{ SYS_chown,			"chown" },
	{ SYS_brk,			"brk" },
	{ 18,				"old_stat" },
	{ SYS_lseek,			"lseek" },
	{ SYS_getpid,			"getpid" },
	{ SYS_omount,			"old_mount" },
	{ SYS_oumount,			"old_umount" },
	{ 23,				"old_setuid" },
	{ SYS_getuid,			"getuid" },
	{ 25,				"old_stime" },
	{ SYS_ptrace,			"ptrace" },
	{ 27,				"old_alarm" },
	{ 28,				"old_fstat" },
	{ 29,				"old_pause" },
	{ 30,				"old_utime" },
	{ 31,				"old_stty" },
	{ 32,				"old_gtty" },
	{ SYS_access,			"access" },
	{ 34,				"old_nice" },
	{ 35,				"old_ftime" },
	{ SYS_sync,			"sync" },
	{ SYS_kill,			"kill" },
	{ SYS_stat,			"stat" },
	{ 39,				"old_setpgrp" },
	{ SYS_lstat,			"lstat" },
	{ SYS_dup,			"dup" },
	{ SYS_pipe,			"pipe" },
	{ 43,				"old_times" },
	{ SYS_profil,			"profil" },
	{ 46,				"old_setgid" },
	{ SYS_getgid,			"getgid" },
	{ 48,				"old_sigsys" },
	{ SYS_acct,			"acct" },
	{ 52,				"old_phys" },
	{ 53,				"old_syslock" },
	{ SYS_ioctl,			"ioctl" },
	{ SYS_reboot,			"reboot" },
	{ 56,				"old_mpxchan" },
	{ SYS_symlink,			"symlink" },
	{ SYS_readlink,			"readlink" },
	{ SYS_execve,			"execve" },
	{ SYS_umask,			"umask" },
	{ SYS_chroot,			"chroot" },
	{ SYS_fstat,			"fstat" },
	{ SYS_getpagesize,		"getpagesize" },
	{ SYS_mremap,			"mremap" },
	{ SYS_vfork,			"vfork" },
	{ 67,				"old_vread" },
	{ 68,				"old_vwrite" },
	{ SYS_sbrk,			"sbrk" },
	{ SYS_sstk,			"sstk" },
	{ SYS_mmap,			"mmap" },
	{ SYS_vadvise,			"vadvise" },
	{ SYS_munmap,			"munmap" },
	{ SYS_mprotect,			"mprotect" },
	{ SYS_madvise,			"madvise" },
	{ SYS_vhangup,			"vhangup" },
	{ 77,				"old_vlimit" },
	{ SYS_mincore,			"mincore" },
	{ SYS_getgroups,		"getgroups" },
	{ SYS_setgroups,		"setgroups" },
	{ SYS_getpgrp,			"getpgrp" },
	{ SYS_setpgrp,			"setpgrp" },
	{ SYS_setitimer,		"setitimer" },
	{ SYS_wait3,			"wait3" },
	{ SYS_swapon,			"swapon" },
	{ SYS_getitimer,		"getitimer" },
	{ SYS_gethostname,		"gethostname" },
	{ SYS_sethostname,		"sethostname" },
	{ SYS_getdtablesize,		"getdtablesize" },
	{ SYS_dup2,			"dup2" },
	{ SYS_getdopt,			"getdopt" },
	{ SYS_fcntl,			"fcntl" },
	{ SYS_select,			"select" },
	{ SYS_setdopt,			"setdopt" },
	{ SYS_fsync,			"fsync" },
	{ SYS_setpriority,		"setpriority" },
	{ SYS_socket,			"socket" },
	{ SYS_connect,			"connect" },
	{ SYS_accept,			"accept" },
	{ SYS_getpriority,		"getpriority" },
	{ SYS_send,			"send" },
	{ SYS_recv,			"recv" },
	{ SYS_sigreturn,		"sigreturn" },
	{ SYS_bind,			"bind" },
	{ SYS_setsockopt,		"setsockopt" },
	{ SYS_listen,			"listen" },
	{ 107,				"was_vtimes" },
	{ SYS_sigvec,			"sigvec" },
	{ SYS_sigblock,			"sigblock" },
	{ SYS_sigsetmask,		"sigsetmask" },
	{ SYS_sigpause,			"sigpause" },
	{ SYS_sigstack,			"sigstack" },
	{ SYS_recvmsg,			"recvmsg" },
	{ SYS_sendmsg,			"sendmsg" },
	{ 115,				"old_vtrace" },
	{ SYS_gettimeofday,		"gettimeofday" },
	{ SYS_getrusage,		"getrusage" },
	{ SYS_getsockopt,		"getsockopt" },
	{ 119,				"old_resuba" },
	{ SYS_readv,			"readv" },
	{ SYS_writev,			"writev" },
	{ SYS_settimeofday,		"settimeofday" },
	{ SYS_fchown,			"fchown" },
	{ SYS_fchmod,			"fchmod" },
	{ SYS_recvfrom,			"recvfrom" },
	{ SYS_setreuid,			"setreuid" },
	{ SYS_setregid,			"setregid" },
	{ SYS_rename,			"rename" },
	{ SYS_truncate,			"truncate" },
	{ SYS_ftruncate,		"ftruncate" },
	{ SYS_flock,			"flock" },
	{ SYS_sendto,			"sendto" },
	{ SYS_shutdown,			"shutdown" },
	{ SYS_socketpair,		"socketpair" },
	{ SYS_mkdir,			"mkdir" },
	{ SYS_rmdir,			"rmdir" },
	{ SYS_utimes,			"utimes" },
	{ SYS_sigcleanup,		"old sigcleanup" },
	{ SYS_adjtime,			"adjtime" },
	{ SYS_getpeername,		"getpeername" },
	{ SYS_gethostid,		"gethostid" },
	{ SYS_sethostid,		"sethostid" },
	{ SYS_getrlimit,		"getrlimit" },
	{ SYS_setrlimit,		"setrlimit" },
	{ SYS_killpg,			"killpg" },
	{ 147,				"unused" },
	{ SYS_setquota,			"setquota" },
	{ SYS_quota,			"quota" },
	{ SYS_getsockname,		"getsockname" },
	{ SYS_sysmips,			"sysmips" },
	{ SYS_cachectl,			"cachectl" },
	{ SYS_cacheflush,		"cacheflush" },
	{ SYS_debug,			"nfs debug" },
	{ 155,				"unused" },
	{ 156,				"unused" },
	{ 157,				"unused" },
	{ SYS_nfssvc,			"nfs_svc" },
	{ SYS_getdirentries,		"getdirentries" },
	{ SYS_statfs,			"statfs" },
	{ SYS_fstatfs,			"fstatfs" },
	{ SYS_unmount,			"unmount" },
	{ SYS_async_daemon,		"async_daemon" },
	{ SYS_getfh,			"nfs_getfh" },
	{ SYS_getdomainname,		"getdomainname" },
	{ SYS_setdomainname,		"setdomainname" },
	{ 167,				"old pcfs_mount" },
	{ SYS_quotactl,			"quotactl" },
	{ SYS_old_exportfs,		"old_exportfs" },
	{ SYS_mount,			"mount" },
	{ SYS_mipshwconf,               "mipshwconf" },
	{ SYS_exportfs,                 "exportfs" },
	{ 0,				NULL }
};

struct reg_values sym_values[] = {
	/* value			name */
	{ XPR_CLOCK,			"clock" },
	{ XPR_TLB,			"tlb" },
	{ XPR_INIT,			"init" },
	{ XPR_SCHED,			"sched" },
	{ XPR_PROCESS,			"process" },
	{ XPR_EXEC,			"exec" },
	{ XPR_SYSCALL,			"syscall" },
	{ XPR_TRAP,			"trap" },
	{ XPR_NOFAULT,			"nofault" },
	{ XPR_VM,			"vm" },
	{ XPR_SWAP,			"swap" },
	{ XPR_SWTCH,			"swtch" },
	{ XPR_DISK,			"disk" },
	{ XPR_TTY,			"tty" },
	{ XPR_TAPE,			"tape" },
	{ XPR_BIO,			"bio" },
	{ XPR_INTR,			"intr" },
	{ XPR_RMAP,			"rmap" },
	{ XPR_TEXT,			"text" },
	{ XPR_CACHE,			"cache" },
	{ XPR_NFS,			"nfs" },
	{ XPR_RPC,			"rpc" },
	{ XPR_RPC|XPR_NFS,		"nfsrpc" },
	{ XPR_SIGNAL,			"signal" },
	{ XPR_TLB|XPR_SCHED|XPR_PROCESS
	   |XPR_EXEC|XPR_SYSCALL|XPR_TRAP
	   |XPR_NOFAULT|XPR_VM|XPR_SWAP
	   |XPR_SWTCH|XPR_RMAP|XPR_TEXT
	   |XPR_SIGNAL,
					"kernel" },
	{ 0,				NULL }
};

#endif REG_VALUES

unsigned xpr_flags = 0;

#ifdef XPR_DEBUG

struct xprbuf *xprbase, *xprptr, *xprend;
int xprsize, xprinitflag;
#endif XPR_DEBUG

/*
 * xprinit: initialize xprintf buffer.  The minimum buffer size is one
 * page.  If debugging is not enabled, no buffers are allocated.  The
 * first physical page beyond the xprintf buffers is returned.  Xprbufs
 * are assumed to mapped virtual = physical.
 * Note that xpr buffers are uncached.
 */
xprinit(fpage)
{
#ifdef XPR_DEBUG
	register i, j;
	extern char xpr_addr;

	xprbase = (struct xprbuf *)PHYS_TO_K1(ctob(fpage));
	xprptr = xprbase;
	if (!xprsize)
		xprsize = 1*NBPC;
	/* Clear out the buffer */
	fpage += btoc(xprsize);
	xprend = (struct xprbuf *)PHYS_TO_K1(ctob(fpage)) - 1;
#ifndef SABLE
	bzero(xprbase, (char *)xprend - (char *)xprbase);
#endif
	xprsize /= sizeof(struct xprbuf);
	xprinitflag = 1;
#endif XPR_DEBUG
	return(fpage);
}

/*
 * Store a pointer (msg) and up to 3 arguments into the circular xpr buffer.
 * msg is assumed to be a pointer into the kernel text segment (which 
 * does not change after boot time) and all arguments are assumed to be 
 * non-pointers.  The printf string processing is delayed until the buffer
 * is dumped.
 */
xprintf(msg, arg1, arg2, arg3, arg4)
char *msg;
{
#ifdef XPR_DEBUG
	register struct xprbuf *xp;
	register s;
	extern struct proc *curproc;

	if (xprinitflag == 0)
		return;
	s = splhigh();
	xp = xprptr;
	xp->xp_msg = msg;
	xp->xp_arg1 = arg1;
	xp->xp_arg2 = arg2;
	xp->xp_arg3 = arg3;
	xp->xp_arg4 = arg4;
	xp->xp_timestamp = lbolt;
	xp->xp_pid = curproc->p_pid;
	xp->xp_tlbpid = curproc->p_tlbpid;
	if (++xprptr >= xprend)
		xprptr = xprbase;
	splx(s);
#else XPR_DEBUG
	nulldev();
#endif XPR_DEBUG
}

xprdump()
{
#ifdef XPR_DEBUG
	xprtail(xprsize);
#else XPR_DEBUG
	nulldev();
#endif XPR_DEBUG
}

/*
 * Dump the current xprbuf.  This is generally called after the system has
 * crashed (from the monitor).  The string processing for xprintf calls
 * is done here via printf which doesn't call xprintf.
 */
xprtail(lines)
unsigned lines;
{
#ifdef XPR_DEBUG
	register struct xprbuf *xp;
	register i;

	if (!xprinitflag)
		printf("Buffer not initialized.\n");
	else {
		if (lines > xprsize)
			lines = xprsize;
		xp = xprptr - lines;
		if (xp < xprbase)
			xp += xprsize;
		printf("pid / tlb @ ticks: message\n");
		for(i = 0; i < lines; i++) {
			if (xp->xp_msg) {
				printf("%d / %x @ %d:\t",
				    xp->xp_pid, xp->xp_tlbpid,
				    xp->xp_timestamp);
				printf(xp->xp_msg, xp->xp_arg1, xp->xp_arg2,
					xp->xp_arg3, xp->xp_arg4);
				if (xp->xp_msg[strlen(xp->xp_msg)-1] != '\n')
					printf("\n");
			}
			if (++xp >= xprend)
				xp = xprbase;
		}
		printf("Done\n");
	}
#else XPR_DEBUG
	nulldev();
#endif XPR_DEBUG
}
