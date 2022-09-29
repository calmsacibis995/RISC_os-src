#ident "$Header: rdebug.c,v 1.4 90/06/22 11:07:28 rpharris Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * rdebug.c -- remote debugging facilities
 */

/*
 * TODO:
 */

#include "sys/errno.h"
#include "sys/types.h"
#include "sys/file.h"
#include "sys/wait.h"
#include "sys/signal.h"
#include "sys/ptrace.h"
#include "machine/cpu.h"
#include "saio/setjmp.h"
#include "saio/protoio.h"
#include "saio/protocol.h"
#include "saio/stringlist.h"
#include "saio/saioctl.h"
#include "saio/debug.h"
#include "prom/entrypt.h"
#include "dbgmon/dbgmon.h"

/*
 * debug protocol fields for ptrace packets
 */
#define	PID_FIELD	0	/* process id field of ptrace pkt */
#define	REQ_FIELD	1	/* request field of ptrace pkt */
#define	ADDR_FIELD	2	/* address field of ptrace pkt */
#define	DATA_FIELD	3	/* data field of ptrace pkt */

/*
 * debug protocol fields for boot packets
 */
#define	PROG_FIELD	2	/* program name field of boot pkt */
#define	ARGS_FIELD	3	/* first argument field of boot pkt */

static char *btoh();
static mapreg();
extern unsigned _regs[];
extern show_fault();

/*
 * rdebug -- remote debugging interface command
 */
_rdebug(argc, argv)
int argc;
char **argv;
{
	struct string_list fields;
	struct promexec_args pa;
	char linebuf[MAXPACKET];
	unsigned *wp, val, oval;
	int pkt_type;
	int reg, fcnt, len;
	jmp_buf dbg_buf;
	int fd;
	int restart = 0;
	int pc;
	extern int *nofault;
	extern int *bp_nofault;
	extern int Debug;
	extern char **environ;
	extern unsigned _client_sp;
	extern char *atob();

	argv++; argc--;
	if (argc == 2 && strcmp(*argv, "-r") == 0) {
		argv++; argc--;
		restart++;	/* restarting after ^Z */
	}
	if (argc != 1)
		return(1);

	/*
	 * open char device that is to be used as comm path
	 * with debugger
	 */
	fd = open(*argv, O_RDWR);
	if (fd < 0) {
		printf("can't open %s\n", *argv);
		return(0);
	}
	close_on_exception(fd);

	/*
	 * set-up comm path to speak protocol
	 */
	proto_enable(fd);

	/*
	 * send a reply to indicate that boot was successful
	 * and we're in debug mode
	 */
	if (!restart) {
		init_proto(fd);
		_argvize("0x1 b junk", &fields);
		send_reply(fd, &fields, 0, mk_status(SIGTRAP, WSTOPPED));
	}

	for (;;) {
		pkt_type = DATA_PKTTYPE;
		len = getpkt(fd, linebuf, MAXPACKET, &pkt_type);
		if (len < 0) {
			printf("ABNORMAL PROTOCOL TERMINATION");
			goto done;
		}

		/*
		 * DEBUG PROTOCOL DEFINITION
		 *
		 * Debug protocol incoming packets are of two formats:
		 *
		 * PTRACE PACKETS:
		 *
		 *	PID	REQUEST	ADDR	DATA
		 *	0x%x	%c	0x%x	0x%x
		 *
		 * where the fields correspond to the definitions in
		 * ptrace(2) with the exception that requests are specified
		 * as single characters rather than integers.  All fields
		 * are separated by one or more blanks.  Currently, the
		 * following requests are accepted:
		 *
		 *	char	ptrace#		operation
		 *
		 *	i	1		read i-space
		 *	d	2		read d-space
		 *	r	3		read registers
		 *	I	4		write i-space
		 *	D	5		write d-space
		 *	R	6		write registers
		 *	c	7		continue
		 *	x	8		terminate
		 *	s	9		single step
		 *	g	10		get multiple registers
		 *	f	11		get multiple float-pt registers
		 *	G	12		set multiple gp registers
		 *	F	13		set multiple float-pt registers
		 * 	p	14		read byte d-space
		 *	P	15		write byte d-space
		 *	h	16		read halfword d-space
		 *	H	17		write halfword d-space
		 *
		 * Use of addr and data is described below for each
		 * command.
		 *
		 * BOOT PACKETS:
		 *
		 * The other class of request is the boot (b) request
		 * which is formatted:
		 *
		 *	PID	REQUEST	PROGNAME	ARGS
		 *	0x%x	b	%s		%s
		 *
		 * where:
		 *	PROGNAME is the program to boot (with the path
		 *	specified appropriately for the standalone
		 *	environment) and ARGS are a list of strings
		 *	which should be passed to the program being
		 *	debugged as its argv.
		 */
		linebuf[len] = 0;
		if ((fcnt = _argvize(linebuf, &fields)) < 3) {
			send_reply(fd, &fields, -1, EINVAL);
			break;
		}
		if (Debug & DBG_RMTDBG) {
			int i;

			printf("RECV:");
			for (i = 0; i < fcnt; i++)
				printf(" %s", fields.strptrs[i]);
			printf("\n");
		}

		if (setjmp(dbg_buf)) {
			sa_spl();
			show_fault();
			error_reply(fd, &fields);
			continue;
		}

		ioctl(fd, FIOCSCAN, 0);
		nofault = dbg_buf;	/* in case of trouble */
		bp_nofault = dbg_buf;
		switch (*fields.strptrs[REQ_FIELD]) {

#ifdef notdef
		/*
		 * Currently this is only useful for booting programs
		 * via the ethernet (most cases actually!) or from volume
		 * headers or from tpd tapes.   (The prom doesn't
		 * know about 4.2BSD or SysV file systems.)
		 *
		 * I suppose we could include file system code with
		 * the dbgmon, but that would make it huge!
		 * Another possibility would be to go through a two
		 * level boot process.
		 */
		case 'b':
			pa.pa_bootfile = fields.strptrs[PROG_FIELD];
			pa.pa_argc = fcnt - ARGS_FIELD;
			pa.pa_argv = &fields.strptrs[ARGS_FIELD];
			pa.pa_environ = environ;
			pa.pa_flags = EXEC_NOGO;
			if ((pc = promexec(&pa)) == -1)
				send_reply(fd, &fields, -1, ENOENT);
			else {
				send_reply(fd, &fields, 0,
				    mk_status(SIGTRAP, WSTOPPED));
				_regs[R_SP] = _client_sp;
				_regs[R_EPC] = pc;
				_regs[R_A0] = 0;	/* no args, sorry */
				_regs[R_A1] = 0;
				_regs[R_A2] = (unsigned)environ;
			}
			break;
#endif

		case 'i':	/* read i-space word */
		case 'd':	/* read d-space word */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			val = _get_memory(wp, SW_WORD);
			send_reply(fd, &fields, 0, val);
			break;

		case 'p':	/* Read byte d-space */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			val = _get_memory(wp, SW_BYTE);
			send_reply(fd, &fields, 0, val);
			break;

		case 'h':	/* Read haflword d-space */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			val = _get_memory(wp, SW_HALFWORD);
			send_reply(fd, &fields, 0, val);
			break;

		case 'I':	/* write i-space word */
		case 'D':	/* write d-space word */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (fcnt<4 || *atob(fields.strptrs[DATA_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			oval = _get_memory(wp, SW_WORD);
			_set_memory(wp, SW_WORD, val);
			send_reply(fd, &fields, 0, oval);
			break;

		case 'P':	/* Write byte d-space */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (fcnt<4 || *atob(fields.strptrs[DATA_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			oval = _get_memory(wp, SW_BYTE);
			_set_memory(wp, SW_BYTE, val);
			send_reply(fd, &fields, 0, oval);
			break;

		case 'H':	/* Write halfword d-space */
			if (*atob(fields.strptrs[ADDR_FIELD], &wp)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (fcnt<4 || *atob(fields.strptrs[DATA_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			oval = _get_memory(wp, SW_HALFWORD);
			_set_memory(wp, SW_HALFWORD, val);
			send_reply(fd, &fields, 0, oval);
			break;

		case 'r':	/* read register */
			if (*atob(fields.strptrs[ADDR_FIELD], &reg) ||
			    (reg = mapreg(reg)) < 0) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			val = _get_register(reg);
			send_reply(fd, &fields, 0, val);
			break;

		case 'R':	/* write register */
			if (*atob(fields.strptrs[ADDR_FIELD], &reg) ||
			    (reg = mapreg(reg)) < 0) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (fcnt<4 || *atob(fields.strptrs[DATA_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			oval = _get_register(reg);
			_set_register(reg, val);
			send_reply(fd, &fields, 0, oval);
			break;
		
		case 'g':
			g_reply(fd, &fields);
			break;

		case 'c':	/* continue execution */
			if (fcnt<4 || *atob(fields.strptrs[ADDR_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (val != 1)
				_regs[R_EPC] = val;
			_resume();
			break;

		case 's':
			if (fcnt<4 || *atob(fields.strptrs[ADDR_FIELD], &val)) {
				send_reply(fd, &fields, -1, EINVAL);
				break;
			}
			if (val != 1)
				_regs[R_EPC] = val;
			single_step();
			break;

		case 'x':	/* exit debug mode */
			send_reply(fd, &fields, 0, 0);
			printf("Exiting debug monitor\n");
			nofault = 0;
			bp_nofault = 0;
			proto_disable(fd);
			close(fd);
			_quit();

		default:
			send_reply(fd, &fields, -1, EINVAL);
			break;
		}
		nofault = 0;
		bp_nofault = 0;
	}
done:
	nofault = 0;
	bp_nofault = 0;
	proto_disable(fd);
	close(fd);
	return(0);
}

/*
 * error_reply -- send appropriate remote debugging reply after an
 * exception
 */
static
error_reply(fd, fieldp)
register int fd;
register struct string_list *fieldp;
{
	extern unsigned _exc_save;
	extern unsigned _cause_save;

	switch (*fieldp->strptrs[1]) {
	case 'b':
		send_reply(fd, fieldp, -1, ENOENT);
		break;

	case 's':
	case 'c':
		switch (_exc_save) {
		case EXCEPT_NORM:
			switch (_cause_save & CAUSE_EXCMASK) {
			case EXC_INT:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGINT, WSTOPPED));
				break;

			case EXC_MOD:
			case EXC_RMISS:
			case EXC_WMISS:
				send_reply(fd, fieldp, 0,
					mk_status(SIGSEGV, WSTOPPED));
				break;

			case EXC_RADE:
			case EXC_WADE:
			case EXC_IBE:
			case EXC_DBE:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGBUS, WSTOPPED));
				break;

			case EXC_SYSCALL:
			case EXC_II:
			case EXC_CPU:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGILL, WSTOPPED));
				break;

			case EXC_OV:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGFPE, WSTOPPED));
				break;

			case EXC_BREAK:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGTRAP, WSTOPPED));
				break;

			default:
				send_reply(fd, fieldp, 0,
				    mk_status(SIGTERM, WSTOPPED));
				break;
			}
			break;

		case EXCEPT_UTLB:
			send_reply(fd, fieldp, 0,
			    mk_status(SIGSEGV, WSTOPPED));
			break;

		case EXCEPT_BRKPT:
			send_reply(fd, fieldp, 0,
			    mk_status(SIGTRAP, WSTOPPED));
			break;
		}
		break;

	case 'i':
	case 'd':
	case 'r':
	case 'I':
	case 'D':
	case 'R':
	case 'h':
		send_reply(fd,fieldp,-1,EFAULT);
		break;

	case 'x':
		_fatal_error("Error on remote debugging exit");
	default:
		send_reply(fd,fieldp,-1,EINVAL);
		break;
	}
}

/*
 * mk_status -- put together an exit status
 */
static
mk_status(signal_code, exit_status)
unsigned signal_code, exit_status;
{
#ifdef COMPILER_FIXED
	union wait wait_status;

	wait_status.w_stopsig = signal_code;
	wait_status.w_stopval = exit_status;
	return (*(unsigned short *)&wait_status);
#else
	return(((signal_code&0xff)<<8)|(exit_status&0xff));
#endif
}

/*
 * g_reply -- reply to general register read command
 */
static
g_reply(fd, fieldp)
register int fd;
struct string_list *fieldp;
{
	unsigned regmask;
	register i;
	char reply_buf[MAXPACKET];
	extern char *atob();
	extern int Debug;

	if (*atob(fieldp->strptrs[ADDR_FIELD], &regmask)) {
		send_reply(fd, fieldp, -1, EINVAL);
		return;
	}

	ioctl(fd, FIOCSCAN, 0);
	if (regmask & 1)
		append(reply_buf, btoh(_regs[R_EPC]));

	for (i = 1; i < 32; i++)
		if (regmask & (1 << i))
			append(reply_buf, btoh(_regs[i]));

	if (Debug & DBG_RMTDBG)
		printf("SEND: %s\n", reply_buf);
	putpkt(fd, reply_buf, strlen(reply_buf), DATA_PKTTYPE);
}
	
/*
 * send_reply -- send a normal debugging protocol reply
 */
static
send_reply(fd, fieldp, num1, num2)
int fd;
struct string_list *fieldp;
unsigned num1, num2;
{
	char reply_buf[MAXPACKET];
	extern int Debug;

	ioctl(fd, FIOCSCAN, 0);
	strcpy(reply_buf, fieldp->strptrs[PID_FIELD]);	/* pid */
	append(reply_buf, fieldp->strptrs[REQ_FIELD]);	/* req */
	append(reply_buf, btoh(num1));			/* result */
	append(reply_buf, btoh(num2));			/* data */
	if (Debug & DBG_RMTDBG)
		printf("SEND: %s\n", reply_buf);
	putpkt(fd, reply_buf, strlen(reply_buf), DATA_PKTTYPE);
}

static
append(buf, str)
{
	strcat(buf, " ");
	strcat(buf, str);
}

/*
 * btoh -- convert to ascii base 16 representation (0x%x)
 */
static char *
btoh(val)
register unsigned val;
{
	static char buf[12];
	register int i;
	register char *cp;
	int digit;

	for (i = 28; i > 0; i -= 4)
		if ((val >> i) & 0xf)
			break;

	buf[0] = '0';
	buf[1] = 'x';
	cp = &buf[2];
	for (; i >= 0; i -= 4) {
		digit = (val >> i) & 0xf;
		if (digit < 10)
			*cp++ = '0' + digit;
		else
			*cp++ = 'a' + (digit - 10);
	}
	*cp = 0;
	return (buf);
}

/*
 * map dbgmon internal register numbers to ptrace register numbers
 */
static struct regmap {
	int	rm_ptrbase;
	int	rm_nregs;
	int	rm_dbgbase;
} regmap[] = {
	{ GPR_BASE,	NGP_REGS,	R_R0 },
	{ FPR_BASE,	NFP_REGS,	R_F0 },
	{ PC,		1,		R_EPC },
	{ CAUSE,	1,		R_CAUSE },
	{ MMHI,		1,		R_MDHI },
	{ MMLO,		1,		R_MDLO },
	{ FPC_CSR,	1,		R_C1_SR },
	{ FPC_EIR,	1,		R_C1_EIR },
	{ 0,		0,		0 }
};

static
mapreg(ptrace_reg)
{
	register struct regmap *rm;
	int off;

	for (rm = regmap; rm->rm_nregs; rm++) {
		if (ptrace_reg >= rm->rm_ptrbase
		    && ptrace_reg < rm->rm_ptrbase + rm->rm_nregs) {
			off = ptrace_reg - rm->rm_ptrbase;
			return(rm->rm_dbgbase + off);
		}
	}
	return(-1);
}
