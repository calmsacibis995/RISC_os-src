/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
#ident	"$Header: prf.c,v 1.33.1.6.1.1.1.2 90/10/05 09:55:23 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/uio.h"		/* needed for uprintf() */
#include "sys/errno.h"		/* needed for uprintf() */
#include "sys/vnode.h"		/* needed for uprintf() */
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "bsd43/sys/msgbuf.h"
#include "bsd43/sys/syslog.h"
#include "sys/kmem.h"
#include "sys/file.h"

extern	struct 	bsd43_msgbuf 	*bsd43_pmsgbuf;

#include <varargs.h>

# define VRNVRAM	rnvram
# define VWNVRAM	wnvram

#define SET_PRT_WHERE(_s_) \
    if (*_s_ == '!') { \
	prt_where = PRW_BUF; \
	_s_++; \
    } else if (*_s_ == '^') { \
	prt_where = PRW_CONS; \
	_s_++; \
    } else \
    prt_where = PRW_BUF | PRW_CONS;

#ifdef NOTDEF
/*	A delay is required when outputting many
**	lines to the console if it is a dmd 5620
**	terminal.  The following value has been
**	chosen empirically.  If your console is
**	a normal terminal, set the delay to 0.
**	Note that dmd_delay can be set to 0
**	on the running system with demon.
*/

#define	DMD_DELAY	0x1000

int	dmd_delay = DMD_DELAY;
#endif


/*	Save output in a buffer where we can look at it
**	with demon or crash.  If the message begins with
**	a '!', then only put it in the buffer, not out
**	to the console.
*/

extern	char	putbuf[];
extern	int	putbufsz;
int		putbufndx;
short		prt_where;


/* 	Save character to both putbuf (the buffer used by cmn_err)
 *	and msgbuf (the buffer used by syslog).
 */
#define	output(c) \
	if (prt_where & PRW_BUF) { \
		putbuf[putbufndx++ % putbufsz] = c; \
	} \
	if (prt_where & PRW_MSGBUF) { \
		bsd43_pmsgbuf->msg_bufc[bsd43_pmsgbuf->msg_bufx++] = c; \
		if (bsd43_pmsgbuf->msg_bufx == BSD43_MSG_BSIZE) \
			bsd43_pmsgbuf->msg_bufx = 0; \
	} \
	if (prt_where & PRW_CONS) \
		console_putc(c);

#ifdef notdef
#define	loutput(c, tp) \
	if (prt_where & PRW_BUF) { \
		putbuf[putbufndx++ % putbufsz] = c; \
	} \
	if (prt_where & PRW_MSGBUF) { \
		bsd43_pmsgbuf->msg_bufc[bsd43_pmsgbuf->msg_bufx++] = c; \
		if (bsd43_pmsgbuf->msg_bufx == BSD43_MSG_BSIZE) \
			bsd43_pmsgbuf->msg_bufx = 0; \
	} \
	if (prt_where & PRW_CONS) \
		putc(c, &tp->t_outq);
#endif notdef

/*
 * In case console is off,
 * panicstr contains argument to last
 * call to panic.
 */
char	*panicstr ;

extern int	sbdpit ;
extern int	sbdrcsr ;
extern char	sbdnvram ;
extern char	*mmusrama ;
extern char	*mmusramb ;
extern char	*mmufltar ;
extern char	*mmufltcr ;
extern int	*save_r0ptr ;
extern int	sdata[];
extern int	bssSIZE[];

/* code names for exceptions.  used in trap.c */
char * code_names[] = {
	"interrupt ",
	"TLB mod ",
	"Read TLB Miss ",
	"Write TLB Miss ",
	"Read Address Error ",
	"Write Address Error ",
	"Instruction Bus Error ",
	"Data Bus Error ",
	"SYSCALL ",
	"BREAKpoint ",
	"Illegal Instruction ",
	"CoProcessor Unusable ",
	"Overflow ",
	"Trap",
	"Doubleword Noncached",
	"Machine Check",
	"Software detected SEGV ",
	"resched request ",
	"page-in request ",
	"coprocessor unusable ",
	"software detected bus error ",
	"bad page in process (vfault) ",
};



/*
 * Scaled down version of C Library printf.
 * Only %s %u %d (==%u) %o %x %D %r %R %n %N %c %C %b are recognized.
 * Used to print diagnostic information directly on console tty.
 * Since it is not interrupt driven, all system activities are
 * suspended.  Printf should not be used for chit-chat.
 *
 * DESCRIPTION of %b format (used to decode error registers)
 *
 * Usage is:
 *	printf("reg=%b\n", regval, "<base><arg>*");
 * Where <base> is the output base expressed as a control character,
 * e.g. \10 gives octal; \20 gives hex.  Each arg is a sequence of
 * characters, the first of which gives the bit number to be inspected
 * (origin 1), and the next characters (up to a control character, i.e.
 * a character <= 32), give the name of the register.  Thus
 *	printf("reg=%b\n", 3, "\10\2BITTWO\1BITONE\n");
 * would produce output:
 *	reg=2<BITTWO,BITONE>
 *
 * DESCRIPTION of %r and %R formats
 *
 * printf("%r %R", val, reg_descp);
 * struct reg_desc *reg_descp;
 *
 * the %r and %R formats allow formatted print of bit fields.  individual
 * bit fields are described by a struct reg_desc, multiple bit fields within
 * a single word can be described by multiple reg_desc structures.
 * %r outputs a string of the format "<bit field descriptions>"
 * %R outputs a string of the format "0x%x<bit field descriptions>"
 *
 * The fields in a reg_desc are:
 *	unsigned rd_mask;	An appropriate mask to isolate the bit field
 *				within a word, and'ed with val
 *
 *	int rd_shift;		A shift amount to be done to the isolated
 *				bit field.  done before printing the isolate
 *				bit field with rd_format and before searching
 *				for symbolic value names in rd_values
 *
 *	char *rd_name;		If non-null, a bit field name to label any
 *				out from rd_format or searching rd_values.
 *				if neither rd_format or rd_values is non-null
 *				rd_name is printed only if the isolated
 *				bit field is non-null.
 *
 *	char *rd_format;	If non-null, the shifted bit field value
 *				is printed using this format.
 *
 *	struct reg_values *rd_values;	If non-null, a pointer to a table
 *				matching numeric values with symbolic names.
 *				rd_values are searched and the symbolic
 *				value is printed if a match is found, if no
 *				match is found "???" is printed.
 *				
 * For examples of register descriptions see the kernel file os/debug.c
 */

printf(fmt, va_alist)
register char *fmt;
va_dcl
{
	register va_list ap;

	va_start(ap);
	_log(BSD43_LOG_INFO, fmt, ap); 
	va_end(ap);
}

#define PUTS(str) { register char *xx = str; while(*xx) { output(*xx); xx++; }; }
#define PUTCHAR(c)	{ output(c) }

_doprint(fmt, args)
    register char *fmt;
    va_list args;
{
	register int	c, b, any, i;
	register char	*s;
	register int	opri;

	opri = splhi();

loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0') {
			splx(opri);
			wakeup(putbuf);
			return;
		}
		output(c);
	}
	c = *fmt++;
	if (c == 'd' || c == 'u' || c == 'o' || c == 'x')
		printn(va_arg(args, long), c=='o'? 8: (c=='x'? 16:10));
	else if (c == 's') {
		s = va_arg(args, char *);
		if ((unsigned)s >= K0BASE) {
			while (s && (c = *s++)) {
				output(c);
			}
		}
	} else if (c == 'D') {
		printn(va_arg(args, long), 10);
	} else if (c == 'b') {
		b = va_arg(args, int);
		s = va_arg(args, char *);
		printn((unsigned)b, *s++);
		any = 0;
		if (b) {
			PUTCHAR('<');
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					if (any)
						PUTCHAR(',');
					any = 1;
					for (; (c = *s) > 32; s++)
						PUTCHAR(c);
				} else
					for (; *s > 32; s++)
						;
			}
			PUTCHAR('>');
		}
	} else if (c == 'r' || c == 'R') {
		b = va_arg(args, int);
		s = va_arg(args, char *);
		if (c == 'R') {
			PUTS("0x");
			printn(b, 16);
		}
		any = 0;
		if (c == 'r' || b) {
			register struct reg_desc *rd;
			register struct reg_values *rv;
			register unsigned field;

			PUTCHAR('<');
			for (rd = (struct reg_desc *)s; rd->rd_mask; rd++) {
				field = b & rd->rd_mask;
				field = (rd->rd_shift > 0)
				    ? field << rd->rd_shift
				    : field >> -rd->rd_shift;
				if (any &&
				      (rd->rd_format || rd->rd_values
				         || (rd->rd_name && field)
				      )
				)
					PUTCHAR(',');
				if (rd->rd_name) {
					if (rd->rd_format || rd->rd_values
					    || field) {
						PUTS(rd->rd_name);
						any = 1;
					}
					if (rd->rd_format || rd->rd_values) {
						PUTCHAR('=');
						any = 1;
					}
				}
				if (rd->rd_format) {
					printf(rd->rd_format, field);
					any = 1;
					if (rd->rd_values)
						PUTCHAR(':');
				}
				if (rd->rd_values) {
					any = 1;
					for (rv = rd->rd_values;
					    rv->rv_name;
					    rv++) {
						if (field == rv->rv_value) {
							PUTS(rv->rv_name);
							break;
						}
					}
					if (rv->rv_name == NULL)
						PUTS("???");
				}
			}
			PUTCHAR('>');
		}

	} else if (c == 'c' || c == 'C') {
		b = va_arg(args, int);
		for (i = 24; i >= 0; i -= 8) {
			if (c == 'c') {
				c = (b >> i) & 0x7f;
			} else {
				c = (b >> i) & 0xff;
			}
			if (c) PUTCHAR(c);
		}
	} else if (c == 'n' || c == 'N') {
		register struct reg_values *rv;

		b = va_arg(args, int);
		s = va_arg(args, char *);
		for (rv = (struct reg_values *)s; rv->rv_name; rv++) {
			if (b == rv->rv_value) {
				PUTS(rv->rv_name);
				break;
			}
		}
		if (rv->rv_name == NULL)
			PUTS("???");
		if (c == 'N' || rv->rv_name == NULL) {
			PUTCHAR(':');
			printn(b, 10);
		}
	}


	goto loop;
}

printn(n, b)
long n;
register b;
{
	register i, nd, c;
	int	flag;
	int	plmax;
	char d[12];

	c = 1;
	flag = n < 0;
	if (flag)
		n = (-n);
	if (b==8)
		plmax = 11;
	else if (b==10)
		plmax = 10;
	else if (b==16)
		plmax = 8;
	if (flag && b==10) {
		flag = 0;
		output('-');
	}
	for (i=0;i<plmax;i++) {
		nd = n%b;
		if (flag) {
			nd = (b - 1) - nd + c;
			if (nd >= b) {
				nd -= b;
				c = 1;
			} else
				c = 0;
		}
		d[i] = nd;
		n = n/b;
		if ((n==0) && (flag==0))
			break;
	}
	if (i==plmax)
		i--;
	for (;i>=0;i--) {
		output("0123456789ABCDEF"[d[i]]);
	}
}

/*
 * Panic is called on unresolvable fatal errors.
 */

panic(s)
    register char *s;
{
	cmn_err(CE_PANIC,s);
}

real_panic(s)
    register char *s;
{
     int x, i;

    /* memory double-bit ecc errors cause exceptions and
     * interrupts that can lead here.
     * As a short-term solution, once we are committed to crashing
     * the system, lets look at the memory arrays to see if
     * there are any errors.  If not, the routine will return
     * without any real effect.
     * TBD: call the scan routines "earlier" in the exception process.
     * (unfortunatly, an initial attempt caused undesirable side effects
     * for unknown reasons -cprice).
     */

     prt_where = PRW_CONS|PRW_BUF;
     lmem_err_scan();

    /*
     * save panic string (needed for routines elsewhere)
     */
	panicstr = s;

	x = splhi();
	splx(x);

    /*
     * "sync"
     */
	if (BASEPRI(x)) {
		printf("syncing disks... ");
		update();
		{ register struct buf *bp;
		  int iter, nbusy;

		  for (iter = 0; iter < 20; iter++) {
			nbusy = 0;
			for (bp = &buf[v.v_buf]; --bp >= buf; )
				if (bp->b_flags & B_BUSY)
					nbusy++;
			if (nbusy == 0)
				break;
			printf("%d ", nbusy);
			DELAY(40000 * iter);
		  }
		}
		printf("done\n");
	}
#ifdef LATER
	/*
	 * If we've been adjusting the clock, the todr
	 * will be out of synch; adjust it now.
	 */
	resettodr();
#endif LATER
	splhi();			/* extreme priority */
	(void) save();
	dumpsys();
	reset_ctlrs();	/* reset dkip ctlr since it could be in macsi mode */
	prom_reboot();	/* follows $bootmode */
	/*NOTREACHED*/

	/* just in case prom_reboot doesn't work...sit and spin */
	while(1);
}

/*
 * Warn that a system table is full.  From NFS4.0, altered
 */
tablefull(tab)
	char *tab;
{
	cmn_err(CE_CONT, "%s: table is full\n", tab);
}

/*
 * This is a SysV implementation of the BSD uprintf().  This routine
 * should send a message to the controlling tty responsible for generating
 * the system call which calls uprintf().  Since our implementation
 * depends on streams [accessed by the vn_rdwr() func], we call setjmp()
 * in case of longjmps from the stream code.  Presently, RISC/OS streams
 * don't call longjmp().
 */
uprintf(va_alist)
	va_dcl
{
	register struct vnode	*ttyvp = u.u_procp->p_ttyvp;
	struct file	*ttyfp = u.u_procp->p_ttyfp;
	char			*mesg = NULL;
	register char		*format;
	va_list			args;
	label_t			jbs;
	int			error_save;
	int			ret_value;
	int			i;
	struct vattr		vattr;	

	if (ttyvp == NULL)
		return(0);
	bcopy((caddr_t) u.u_qsav, (caddr_t)jbs, sizeof(jbs));
	error_save = u.u_error;
	u.u_error = 0;

	mesg = kmemalloc(80 * 10,M_TEMP,M_NOWAIT);
	if (mesg == NULL) {
		ret_value = ENOMEM;
		goto out;
	};
	FP_HOLD(ttyfp);
	if (setjmp(u.u_qsav)) {
		ret_value = u.u_error;
		if (ret_value == 0)
			ret_value = EINTR;
	} else {
	        if (rootdev != -1) {
		  /*
		   *	check that terminal is writeable by group or
		   *	world (no uprintf()'s if "mesg n")
		   *	IF the terminal is NOT a remotely mounted device!
		   */
		  ret_value = VOP_GETATTR(ttyvp, &vattr, u.u_cred);
		  if (ret_value)
		    goto out;

		  if ((vattr.va_mode & ((VWRITE >> 3) | (VWRITE >> 6))) == 0) {
		    ret_value = EPERM;
		    goto out;
		  };
		};

		va_start(args);
		format = va_arg(args, char *);
		vsprintf(mesg, format, args);

		ret_value = vn_rdwr(UIO_WRITE, ttyvp, (caddr_t)mesg,
			strlen(mesg), 0, UIO_SYSSPACE, IO_UNIT, (int *)0);
		va_end(args);
	}
	FP_RELE(ttyfp);

out:
	u.u_error = error_save;
	bcopy((caddr_t) jbs, (caddr_t) u.u_qsav, sizeof(jbs));
	if (mesg != NULL)
		kmemfree(mesg,M_TEMP,M_NOWAIT);
	return(ret_value);
}


#define VPUTS(str) \
{ register char *xx = str; while(*xx) { *outp++ = *xx;	xx++; }; }
#define VPUTCHAR(c) { *outp++ = c; }
#define VPRINTN(out, a, b) \
{ char *t = out; vprintn(&t, a, b); out = t; }

/*
 * Should work as documented in an ansi C book.  This code was originally
 * taken from _doprint().
 */
vsprintf(out, fmt, args)
    register char *out;
    register char *fmt;
    va_list args;
{
	register int	c, b, any, i;
	register char	*s, *outp = out;
	register int	opri;

loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0') {
			VPUTCHAR('\0');
			return;
		}
		VPUTCHAR(c);
	}
	c = *fmt++;
	if (c == 'd' || c == 'u' || c == 'o' || c == 'x') {
		VPRINTN(outp, va_arg(args, long), c=='o'? 8: (c=='x'? 16:10));
	} else if (c == 's') {
		s = va_arg(args, char *);
		if ((unsigned)s >= K0BASE) {
			while (s && (c = *s++)) {
				VPUTCHAR(c);
			}
		}
	} else if (c == 'D') {
		VPRINTN(outp, va_arg(args, long), 10);
	} else if (c == 'b') {
		b = va_arg(args, int);
		s = va_arg(args, char *);
		VPRINTN(outp, (unsigned)b, *s++);
		any = 0;
		if (b) {
			VPUTCHAR('<');
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					if (any)
						VPUTCHAR(',');
					any = 1;
					for (; (c = *s) > 32; s++)
						VPUTCHAR(c);
				} else
					for (; *s > 32; s++)
						;
			}
			VPUTCHAR('>');
		}
	} else if (c == 'r' || c == 'R') {
		b = va_arg(args, int);
		s = va_arg(args, char *);
		if (c == 'R') {
			VPUTS("0x");
			VPRINTN(outp, b, 16);
		}
		any = 0;
		if (c == 'r' || b) {
			register struct reg_desc *rd;
			register struct reg_values *rv;
			register unsigned field;

			VPUTCHAR('<');
			for (rd = (struct reg_desc *)s; rd->rd_mask; rd++) {
				field = b & rd->rd_mask;
				field = (rd->rd_shift > 0)
				    ? field << rd->rd_shift
				    : field >> -rd->rd_shift;
				if (any &&
				      (rd->rd_format || rd->rd_values
				         || (rd->rd_name && field)
				      )
				)
					VPUTCHAR(',');
				if (rd->rd_name) {
					if (rd->rd_format || rd->rd_values
					    || field) {
						VPUTS(rd->rd_name);
						any = 1;
					}
					if (rd->rd_format || rd->rd_values) {
						VPUTCHAR('=');
						any = 1;
					}
				}
				if (rd->rd_format) {
					printf(rd->rd_format, field);
					any = 1;
					if (rd->rd_values)
						VPUTCHAR(':');
				}
				if (rd->rd_values) {
					any = 1;
					for (rv = rd->rd_values;
					    rv->rv_name;
					    rv++) {
						if (field == rv->rv_value) {
							VPUTS(rv->rv_name);
							break;
						}
					}
					if (rv->rv_name == NULL)
						VPUTS("???");
				}
			}
			VPUTCHAR('>');
		}

	} else if (c == 'c' || c == 'C') {
		b = va_arg(args, int);
		for (i = 24; i >= 0; i -= 8) {
			if (c == 'c') {
				c = (b >> i) & 0x7f;
			} else {
				c = (b >> i) & 0xff;
			}
			if (c) VPUTCHAR(c);
		}
	} else if (c == 'n' || c == 'N') {
		register struct reg_values *rv;

		b = va_arg(args, int);
		s = va_arg(args, char *);
		for (rv = (struct reg_values *)s; rv->rv_name; rv++) {
			if (b == rv->rv_value) {
				VPUTS(rv->rv_name);
				break;
			}
		}
		if (rv->rv_name == NULL)
			VPUTS("???");
		if (c == 'N' || rv->rv_name == NULL) {
			VPUTCHAR(':');
			VPRINTN(outp, b, 10);
		}
	}


	goto loop;
}

vprintn(outp, n, b)
    char **outp;
    long n;
    register b;
{
	register i, nd, c;
	int	 flag;
	int	 plmax;
	char	 d[12];

	c = 1;
	flag = n < 0;
	if (flag)
		n = (-n);
	if (b==8)
		plmax = 11;
	else if (b==10)
		plmax = 10;
	else if (b==16)
		plmax = 8;
	if (flag && b==10) {
		flag = 0;
		**outp = '-';
		(*outp)++;
/*		output('-');*/
	}
	for (i=0;i<plmax;i++) {
		nd = n%b;
		if (flag) {
			nd = (b - 1) - nd + c;
			if (nd >= b) {
				nd -= b;
				c = 1;
			} else
				c = 0;
		}
		d[i] = nd;
		n = n/b;
		if ((n==0) && (flag==0))
			break;
	}
	if (i==plmax)
		i--;
	for (;i>=0;i--) {
		**outp = "0123456789ABCDEF"[d[i]];
		(*outp)++;
/*		output("0123456789ABCDEF"[d[i]]);*/
	}
}

/*
 * prdev prints a warning message.
 * dev is a block special device argument.
 */

prdev(str, dev)
char *str;
dev_t dev;
{
	register maj;

	maj = bmajor(dev);
	if (maj >= bdevcnt) {
		cmn_err(CE_WARN,"%s on bad dev %o(8)\n", str, dev);
		return;
	}
	(*bdevsw[maj].d_print)(dev, str);
}

/*
 * prcom prints a diagnostic from a device driver.
 * prt is device dependent print routine.
 */
prcom(prt, bp, er1, er2)
int (*prt)();
register struct buf *bp;
{
	(*prt)(bp->b_dev, "\nDevice error");
	cmn_err(CE_NOTE,"bn = %D er = %o,%o\n", bp->b_blkno, er1, er2);
}


#ifdef NOTDEF	/* XXX - what to do? */
r0()				/* returns value of r0 (!) */
{
}

# define REG(x,r)	asm(" MOVW r,%r0") ; x = r0() ;

pansave(p)
struct	systate	*p ;
{
	/* save_r0ptr is set in trap when trap is entered
	 * save_r0ptr points to R0 in the stack after the trap
	 * the format of the stack is in ttrap.s
	 * ofp gets the fp of the process that caused the trap
	 * lfp gets the last frame pointer (of pansave)
	 */
	
	p->csr = Rcsr ;
	if (save_r0ptr != NULL) {
		*(int *)&p->psw = save_r0ptr[PS] ;
		p->r3 = save_r0ptr[R3] ;
		p->r4 = save_r0ptr[R4] ;
		p->r5 = save_r0ptr[R5] ;
		p->r6 = save_r0ptr[R6] ;
		p->r7 = save_r0ptr[R7] ;
		p->r8 = save_r0ptr[R8] ;
		p->oap = save_r0ptr[AP] ;
		p->opc = save_r0ptr[PC] ;
		p->osp = save_r0ptr[SP] ;
		p->ofp = save_r0ptr[FP] ;
	}
	REG(p->lfp,%fp)
	REG(p->isp,%isp)
	REG(p->pcbp,%pcbp)

	p->mmufltcr = *fltcr ;
	p->mmufltar = *(long *)fltar ;
	p->mmusrama[0] = *(SRAMA *)(srama) ;
	p->mmusrama[1] = *(SRAMA *)(srama + 1) ;
	p->mmusrama[2] = *(SRAMA *)(srama + 2) ;
	p->mmusrama[3] = *(SRAMA *)(srama + 3) ;
	p->mmusramb[0] = *(SRAMB *)(sramb) ;
	p->mmusramb[1] = *(SRAMB *)(sramb + 1) ;
	p->mmusramb[2] = *(SRAMB *)(sramb + 2) ;
	p->mmusramb[3] = *(SRAMB *)(sramb + 3) ;
}
#endif NOTDEF


# define IUCONSOLE	0
# define SEC		300000	/* approx # loops per second */

int	panic_level;

/*
 * Determine where to send the message (console, putbuf or both), then
 * output a level and the message.  Since levels are  different for
 * cmn_err and syslog, we call separate routines to put just the level
 * into the appropriate buffer, and call _doprint to do the rest.
 */

/* we use this buffer to store the last print message so we can count them */
#define WAS_SAME		(~0)
struct cmn_msg_cnt {
	int level;
	char *fmt;
	int cnt;
} last_call;

/* return 1 if found a % else 0 */
ispercent(fmt)
register char *fmt;
{
	while (*fmt) {
		if (*fmt == '%') return(1);
		fmt++;
	}
	return(0);
}

extern int print_every;

int
store_cmn_err_msg(level,fmt, va_alist)
register int	level;
char		*fmt;
va_dcl
{
	/* can't call cmn_err as this is infinite loop */
	/* careful about calling _doprint also.... */
	register va_list ap;
	register int ret_val;
	register int rep_msg = 0, prt_cnt = 0;
	struct cmn_msg_cnt old;
	old = last_call;		/* save it */
	if ((level != CE_PANIC) &&
	    (last_call.level == level) &&
	    (last_call.fmt == fmt) && !ispercent(fmt) ) {	/* same msg */
		last_call.cnt++;
		if ((last_call.cnt % print_every) == 0) {	/* repeat it? */
			rep_msg++;			/* rep msg */
			prt_cnt = print_every;		/* got PRINT_EVERY */
			last_call.cnt = 1;		/* reset it */
		}
		ret_val = WAS_SAME;
	}
	else {							/* new msg */
		if (last_call.cnt > 1) {	/* if multiple, print count */
			rep_msg++;
			prt_cnt = last_call.cnt;
		}
		/* store new message info */
		last_call.level = level;
		last_call.fmt = fmt;
		last_call.cnt = 1;
		ret_val = !WAS_SAME;
	}
	if (prt_cnt) {
		_doprint("\nFollowing message repeated ");
		printn(prt_cnt,10);	/* number,base */
		_doprint(" times.\n");
	}
	if (rep_msg) {
		if (old.level == CE_WARN)
			_doprint("WARNING: ",0);
		if (old.level == CE_NOTE)
			_doprint("NOTICE: ",0);
		_doprint(old.fmt);	/* repeat it */
		_doprint("\n",0);
	}
	return(ret_val);
}

cmn_err(level, fmt, va_alist)
register int	level;
char		*fmt;
va_dcl
{
	register va_list ap;
	register int	*ip;
	register int	i;
	register int	x;
	register short	old_prt_where = prt_where; 

	/*	Set up to print to putbuf, console, or both
	**	as indicated by the first character of the
	**	format.
	*/

	va_start(ap);
	SET_PRT_WHERE(fmt);
	if (store_cmn_err_msg(level,fmt,ap) == WAS_SAME) {
		goto out;
	}

	switch (level) {
		case CE_CONT:
			logpri(BSD43_LOG_INFO);
			_doprint(fmt, ap);
			break;

		case CE_NOTE:
			logpri(BSD43_LOG_NOTICE);
			_doprint("\nNOTICE: ",0);
			_doprint(fmt, ap);
			_doprint("\n",0);
			break;

		case CE_WARN:
			logpri(BSD43_LOG_WARNING);
			_doprint("\nWARNING: ",0);
			_doprint(fmt, ap);
			_doprint("\n",0);
			break;

		case CE_PANIC:
			prt_where |= (PRW_CONS | PRW_BUF | PRW_MSGBUF);
			if (panic_level == 0) {
				x = splhi();
				panic_level = 1;
				logpri(BSD43_LOG_EMERG);
				printf("\nPANIC: ");
				_doprint(fmt, ap);
				printf("\n");
				splx(x);
				real_panic(fmt);
			} else if (panic_level == 1) {
				panic_level = 2 ;
				logpri(BSD43_LOG_EMERG);
				printf("\nDOUBLE PANIC: ");
				_doprint(fmt, ap);
				printf("\n");
				prom_reboot();
			} else {
				prom_reboot();			
		        }
		        break;

		default :
			cmn_err(CE_PANIC,
			  "unknown level in cmn_err (level=%d, msg=\"%s\")",
			  level, fmt, va_alist) ;
	}

	/* 
	 * If message went to bsd43_pmsgbuf, then 
	 * call logwakeup to drain buffer.
	 */
	if (prt_where & PRW_MSGBUF) {
		prt_where &= ~PRW_MSGBUF; 
		logwakeup();
	}
out:

	va_end(ap);

	/* restore old value of prt_where for printf */
	prt_where = old_prt_where;
}

/* 
 * Called by the ASSERT macro in debug.h when an assertion fails.
 */

assfail(a, f, l)
register char *a, *f;
{
	cmn_err(CE_PANIC, "assertion failed: %s, file: %s, line: %d",
		a, f, l);
}


nomemmsg(func, count, contflg, lockflg)
register char	*func;
register int	count;
register int	contflg;
register int	lockflg;
{
	cmn_err(CE_NOTE,
		"%s - Insufficient memory to%s %d%s page%s - %s",
		func, (lockflg ? " lock" : " allocate"),
		count, (contflg ? " contiguous" : ""),
		(count == 1 ? "" : "s"),
		"system call failed");
#ifdef MIPS_LOCAL
	cmn_err(CE_CONT, 
		"%s (%d) -- freemem = %d, availsmem = %d, availrmem = %d .\n",
		u.u_comm, u.u_procp->p_pid, freemem, availsmem, availrmem);
#endif
}

/*	The following is an interface routine for the drivers.
 */

dri_printf(fmt, va_alist)
	register char *fmt;
	va_dcl
{
	va_list ap;

	va_start(ap);
	_log(BSD43_LOG_INFO,fmt, ap);
	va_end(ap);
}


#ifdef notdef
printputbuf()
{
	int  dummy = 1;

	register int		pbi;
	register int		cc;
	register int		lim;
	register struct tty	*tp;
	register int		opl;
	int			delay;
	struct tty		*errlayer();

	asm("	PUSHW  %r0");
	asm("	PUSHW  %r1");
	asm("	PUSHW  %r2");

	opl = splhi();

	if (conlayers() == 1) {
		if (cfreelist.c_next == NULL) {
			splx(opl);
			return;
		}
		tp = errlayer();  /* get tty pointer to error layer */
	} else {
		tp = NULL;
	}

	pbi = putbufndx % putbufsz;
	lim = putbufsz;

	while (1) {
		if (pbi < lim  &&  (cc = putbuf[pbi++])) {
			if (tp)
				putc(cc, tp->t_outq);
			else
				iuputchar(cc);
			if (cc == '\n'  &&  dmd_delay) {
				delay = dmd_delay;
				while (delay--) ;
			}
		} else {
			if (lim == putbufndx % putbufsz) {
				break;
			} else {
				lim = putbufndx % putbufsz;
				pbi = 0;
			}
		}
	}

	if (tp)
		xtvtproc(tp, T_OUTPUT);	/* Output buffer. */
	splx(opl);

	asm("	POPW  %r2");
	asm("	POPW  %r1");
	asm("	POPW  %r0");
}

printnvram()
{
	int  dummy = 1;

	struct	xtra_nvr	nvram_copy ;
	struct	xtra_nvr	*p ;
	register int		oldpri;

	asm("	PUSHW  %r0");
	asm("	PUSHW  %r1");
	asm("	PUSHW  %r2");

	oldpri = splhi();
	p = (struct xtra_nvr *)(&sbdnvram+XTRA_OFSET) ;
	VRNVRAM(p,&nvram_copy,sizeof(nvram_copy)) ;
	dumpnvram(&nvram_copy) ;
	splx(oldpri);

	asm("	POPW  %r2");
	asm("	POPW  %r1");
	asm("	POPW  %r0");
}

dumpnvram(p)
struct	xtra_nvr	*p ;
{
	int  dummy = 1;

	int	i ;
	int	delay;
	static	char	*csr_names[] =
	{
		"i/o fault" ,
		"dma" ,
		"disk" ,
		"uart" ,
		"pir9" ,
		"pir8" ,
		"clock" ,
		"inhibit fault" ,
		"inhibit time" ,
		"unassigned" ,
		"floppy" ,
		"led" ,
		"alignment" ,
		"req reset" ,
		"parity" ,
		"bus timeout" ,
	} ;

	prt_where = PRW_CONS;
	printf("nvram status:\t") ;
	switch (p->nvsanity)
	{
		case NVSANITY :
			printf("sane") ;
			break ;

		case ~NVSANITY :
			printf("incompletely updated") ;
			break ;

		default :
			printf("invalid") ;
			break ;
	}
	printf("\n\n") ;
	printf("csr:\t%x\t",p->systate.csr) ;
	for (i=15 ; i>=0 ; i--)
		if ((p->systate.csr&(1<<i)) != 0)
			printf("%s, ",csr_names[i]) ;
	printf("\n") ;
	printf("\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
 	printf("psw:\tCFD QIE CD OE NZVC TE IPL CM PM R I ISC TM FT\n") ;
	printf
	(
		"(hex)\t  %x   %x  %x  %x  %x    %x  %x   %x  %x  %x %x %x   %x  %x\n",
		p->systate.psw.CSH_F_D,
		p->systate.psw.QIE,
		p->systate.psw.CSH_D,
		p->systate.psw.OE,
		p->systate.psw.NZVC,
		p->systate.psw.TE,
		p->systate.psw.IPL,
		p->systate.psw.CM,
		p->systate.psw.PM,
		p->systate.psw.R,
		p->systate.psw.I,
		p->systate.psw.ISC,
		p->systate.psw.TM,
		p->systate.psw.FT
	) ;
	printf("\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
	printf("r3:\t%x\n",p->systate.r3) ;
	printf("r4:\t%x\n",p->systate.r4) ;
	printf("r5:\t%x\n",p->systate.r5) ;
	printf("r6:\t%x\n",p->systate.r6) ;
	printf("r7:\t%x\n",p->systate.r7) ;
	printf("r8:\t%x\n",p->systate.r8) ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
	printf("oap:\t%x\n",p->systate.oap) ;
	printf("opc:\t%x\n",p->systate.opc) ;
	printf("osp:\t%x\n",p->systate.osp) ;
	printf("ofp:\t%x\n",p->systate.ofp) ;
	printf("isp:\t%x\n",p->systate.isp) ;
	printf("pcbp:\t%x\n",p->systate.pcbp) ;
	printf("\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
	printf("fltcr:\treqacc\txlevel\tftype\n") ;
	printf("\t%d\t%d\t%d\n", p->systate.mmufltcr.reqacc,
		p->systate.mmufltcr.xlevel,
		p->systate.mmufltcr.ftype) ;
	printf("fltar:\t0x%x\n",p->systate.mmufltar) ;
	printf("\n") ;
	printf("\tsrama\tsramb\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
	for (i=0 ; i<4 ; i++)
		printf("[%d]\t0x%x\t0x%x\n",
			i, p->systate.mmusrama[i],
			p->systate.mmusramb[i].SDTlen) ;
	printf("\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
	printf("time\tmessage\n") ;
	for (i=0 ; i<NERRLOG ; i++)
	{
		printf("\n") ;
		printf("-------------------------------------------\n") ;
		printf("%d\n",p->errlog[i].time) ;
		if ((unsigned)sdata <= (unsigned)p->errlog[i].message &&
		    (unsigned)p->errlog[i].message < (unsigned)sdata + (unsigned)bssSIZE)
			printf(p->errlog[i].message,
				p->errlog[i].param1,
				p->errlog[i].param2,
				p->errlog[i].param3,
				p->errlog[i].param4,
				p->errlog[i].param5) ;
		else
			printf("(0x%x, 0x%x, 0x%x)",
				p->errlog[i].message,
				p->errlog[i].param1,
				p->errlog[i].param2,
				p->errlog[i].param3,
				p->errlog[i].param4,
				p->errlog[i].param5) ;
		if (dmd_delay) {
			delay = dmd_delay;
			while (delay--) ;
		}
	}
	printf("\n") ;
	if (dmd_delay) {
		delay = dmd_delay;
		while (delay--) ;
	}
}

lprintf(fmt, va_alist)
register char	*fmt;
unsigned	va_alist;
{
	register		c;
	register uint		*adx;
	char			*s;
	register struct tty	*tp;
	register int		sr;	/* saved interrupt level */
	struct tty		*errlayer();

	tp = errlayer();	/* get tty pointer to error layer */
	sr = splhi();		/* ??? */
	if (cfreelist.c_next == NULL) { /* anywhere to buffer output? */
		splx(sr);		/* back to where we were */
		return;			/* nope, just return	*/
	}
	adx = &va_alist;
loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0') {
			xtvtproc(tp, T_OUTPUT);	/* ??? */
			splx(sr);		/* back to where we were */
			return;
		}
		loutput(c, tp);
	}
	c = *fmt++;
	if (c == 'd' || c == 'u' || c == 'o' || c == 'x')
		lprintn((long)*adx, c=='o'? 8: (c=='x'? 16:10), tp);
	else if (c == 's') {
		s = (char *)*adx;
		while (c = *s++) {
			loutput(c, tp);
		}
	} else if (c == 'D') {
		lprintn(*(long *)adx, 10, tp);
		adx += (sizeof(long) / sizeof(int)) - 1;
	}
	adx++;
	goto loop;
}

lprintn(n, b, tp)
long n;
register b;
register struct tty *tp;
{
	register i, nd, c;
	int	flag;
	int	plmax;
	char d[12];

	c = 1;
	flag = n < 0;
	if (flag)
		n = (-n);
	if (b==8)
		plmax = 11;
	else if (b==10)
		plmax = 10;
	else if (b==16)
		plmax = 8;
	if (flag && b==10) {
		flag = 0;
		loutput('-', tp);
	}
	for (i=0;i<plmax;i++) {
		nd = n%b;
		if (flag) {
			nd = (b - 1) - nd + c;
			if (nd >= b) {
				nd -= b;
				c = 1;
			} else
				c = 0;
		}
		d[i] = nd;
		n = n/b;
		if ((n==0) && (flag==0))
			break;
	}
	if (i==plmax)
		i--;
	for (;i>=0;i--) {
		loutput("0123456789ABCDEF"[d[i]], tp);
	}
}

conlayers()		/* is the console running layers?? */
{
	return(0);
	struct tty	*tp;
	extern int	xtin();

	/* console has major dev. entry of 0 */
	/* get pointer to console tty structure */

	tp = cdevsw[0].d_ttys;

	/* use console line discipline and linesw */
	/* to really determine if layers is running */

	if (linesw[tp->t_line].l_input == xtin)
		return(1);	/* true, layers is running */
	else
		return(0);	/* false, layers is not running */
}
#endif notdef


extern 	pmsgbuf_initialized;
/*
 * Log writes to the log buffer if it has been initialized,
 * and guarantees not to sleep (so can be called by interrupt routines).
 * If there is no process reading the log yet, it writes to the console also.
 */
/*VARARGS2*/
log(level, fmt, va_alist)
	char *fmt;
	va_dcl
{
	register va_list ap;

	va_start(ap);
	_log(level, fmt, ap);
	va_end(ap);
}

_log(level, fmt, args)
	int level;
	char *fmt;
	va_list args;
{
	register s = splhigh();
	extern int log_open;
	register short old_prt_where = prt_where;

	if (panic_level != 0)
		prt_where |= (PRW_CONS | PRW_BUF | PRW_MSGBUF);
	else
		prt_where = PRW_BUF;

	/* Don't try to write to pmsgbuf if it hasn't been set up yet. */
	if (pmsgbuf_initialized) {
		prt_where |= PRW_MSGBUF;
		logpri(level);
	}
	_doprint(fmt, args);
	splx(s);

	/* If /dev/klog hasn't been opened, send the message to 
	 * the console (unless it has already been sent) 
	 * so that the message doesn't get lost.
	 */
	if (!log_open && !(prt_where & PRW_CONS)) {
		prt_where = PRW_CONS;
		_doprint(fmt, args);
	}

	if (pmsgbuf_initialized)
		logwakeup();

	prt_where = old_prt_where;
}


/* Log priorities only go into psmgbuf, so save old prt_where and
 * set prt_where to only go to pmsgbuf.  Before exiting OR back
 * in the old value, leaving PRW_MSGBUF set so that rest of message
 * get written to pmsgbuf.
 */

logpri(level)
	int level;
{
	short old_prt_where = prt_where;

	/* Don't try to write to pmsgbuf if it hasn't been set up yet. */
	if (!pmsgbuf_initialized) return;

	prt_where = PRW_MSGBUF;
	/* Omit printing level between fragments of a single line */
	switch (bsd43_pmsgbuf->msg_bufc[((bsd43_pmsgbuf->msg_bufx == 0)
			? BSD43_MSG_BSIZE 
			: bsd43_pmsgbuf->msg_bufx) - 1]) {
	case 0:
	case '\n':
		PUTCHAR('<');
		printn((u_long)level, 10);
		PUTCHAR('>');
		break;
	default:
		break;
	};

	prt_where |= old_prt_where;
}
