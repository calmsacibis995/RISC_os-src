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
#ident	"$Header: bsd_ioctl.c,v 1.15.1.5.1.1.1.3 90/10/23 13:44:33 beacker Exp $"

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/file.h"
#include "bsd43/sys/file.h"
#include "sys/termio.h"
#include "bsd43/sys/ioctl.h"
#include "sys/dvh.h"
#include "bsd43/mips/dvh.h"
#include "sys/dkio.h"
#include "bsd43/mips/dkio.h"
#include "sys/mtio.h"
#include "bsd43/sys/mtio.h"
#include "bsd43/sys/socket.h"
#include "bsd43/net/if.h"

/*
 * System call routines which implement the BSD ioctl()
 * system call.  These calls pass some ioctl operations on to
 * the main system routines, and implement others locally.
 * Generally, any ioctl defined in termio.h is done in the main
 * routines, whereas ioctl's defined only in bsd43/sys/ioctl.h
 * are implemented here.
 */


/*
 * perform BSD I/O control operation
 */

struct bit_match {
	u_short	bit;
	u_short	match;
	};

#define MATCH_LSAVE_ICANON (LSAVE_RAW | LSAVE_CBREAK)
#define MATCH_LSAVE_OPOST (LSAVE_RAW)

static struct bit_match saved_bit_list[] = {
	{LSAVE_OPOST,	(MATCH_LSAVE_OPOST)},
	{LSAVE_ISIG,	(LSAVE_RAW | LSAVE_CBREAK)},
	{LSAVE_ICANON,	(MATCH_LSAVE_ICANON)},
	{LSAVE_IUCLC,	(LSAVE_RAW | LSAVE_LCASE)},
	{LSAVE_OLCUC,	(LSAVE_LCASE)},
	{LSAVE_ICRNL,	(LSAVE_RAW | LSAVE_CRMOD)},
	{LSAVE_ONLCR,	(LSAVE_CRMOD)},
	{LSAVE_XCASE,	(LSAVE_LCASE)},
	{LSAVE_IGNCR,	(LSAVE_RAW)},
	{LSAVE_BRKINT,	(LSAVE_RAW | LSAVE_CBREAK)},
	{0,		0}};

bsd43_ioctl_virtual_cbreak(tp,is_set)
	struct termio *tp;
	int	is_set;
{
	if (is_set)
		return((tp->c_lflag & ISIG) != 0 &&
		       (tp->c_lflag & ICANON) == 0 &&
		       (tp->c_iflag & BRKINT) != 0);
	else
		return((tp->c_lflag & ISIG) != 0 &&
		       (tp->c_lflag & ICANON) != 0 &&
		       (tp->c_iflag & BRKINT) != 0);
}


bsd43_ioctl_virtual_crmod();
bsd43_ioctl_virtual_lcase();

bsd43_ioctl_virtual_raw(tp,is_set)
	struct	termio *tp;
	int	is_set;
{
	if (is_set)
		return((tp->c_lflag & ISIG) == 0 &
		       (tp->c_lflag & ICANON) == 0 &&
		       (tp->c_iflag & IUCLC) == 0 && 
		       (tp->c_iflag & IGNCR) == 0 && 
		       (tp->c_iflag & ICRNL) == 0 && 
		       (tp->c_iflag & INLCR) == 0 && 
		       (tp->c_iflag & BRKINT) == 0 && 
		       (tp->c_oflag & OPOST) == 0);
	else
		return((bsd43_ioctl_virtual_cbreak(tp,1) ||
				bsd43_ioctl_virtual_cbreak(tp,0)) &&
		       (bsd43_ioctl_virtual_crmod(tp,1) ||
				bsd43_ioctl_virtual_crmod(tp,0)) &&
		       (bsd43_ioctl_virtual_lcase(tp,1) ||
				bsd43_ioctl_virtual_lcase(tp,0)) &&
		       (tp->c_iflag & IGNCR) == 0 && 
		       (tp->c_iflag & INLCR) == 0);
}


bsd43_ioctl_virtual_crmod(tp,is_set)
	struct	termio *tp;
	int	is_set;
{
	if (is_set) 
		return((tp->c_iflag & ICRNL) != 0 &&
		       (tp->c_oflag & ONLCR) != 0);
	else
		return((tp->c_iflag & ICRNL) == 0 &&
		       (tp->c_oflag & ONLCR) == 0);
}


bsd43_ioctl_virtual_lcase(tp,is_set)
	struct	termio *tp;
	int	is_set;
{
	if (is_set)
		return((tp->c_lflag & XCASE) != 0 &&
		       (tp->c_iflag & IUCLC) != 0 &&
		       (tp->c_oflag & OLCUC) != 0);
	else 
		return((tp->c_lflag & XCASE) == 0 &&
		       (tp->c_iflag & IUCLC) == 0 &&
		       (tp->c_oflag & OLCUC) == 0);
}


static save_and_restore_bits(old_tp,new_tp,save_set,restore_set)
	struct	termio *old_tp;
	struct	termio *new_tp;
	u_short	save_set;
	u_short	restore_set;
{
	struct bit_match *bm;
	u_short	leave_set;
	int	value;
	u_short	new_save_set;

	for (bm = saved_bit_list; bm->bit != 0; bm++) {
		/* skip this bit if not in the set of bits to save */
		if ((bm->match & save_set) == 0) 
			continue;
		/* skip this bit if already saved */
		if ((bm->match & old_tp->c_saved_flags) != 0)
			continue;
		switch (bm->bit) {
		case LSAVE_OPOST:
			value = old_tp->c_oflag & OPOST;
			break;
		case LSAVE_ISIG:
			value = old_tp->c_lflag & ISIG;
			break;
		case LSAVE_ICANON:
			value = old_tp->c_lflag & ICANON;
			new_tp->c_cc[V_SAVED_EOF] = (value ? 1 : CEOF);
			new_tp->c_cc[V_SAVED_EOL] = (value ? 0 : CNUL);
			break;
		case LSAVE_IUCLC:
			value = old_tp->c_iflag & IUCLC;
			break;
		case LSAVE_OLCUC:
			value = old_tp->c_oflag & OLCUC;
			break;
		case LSAVE_ICRNL:
			value = old_tp->c_iflag & ICRNL;
			break;
		case LSAVE_ONLCR:
			value = old_tp->c_oflag & ONLCR;
			break;
		case LSAVE_XCASE:
			value = old_tp->c_lflag & XCASE;
			break;
		case LSAVE_IGNCR:
			value = old_tp->c_iflag & IGNCR;
			break;
		case LSAVE_BRKINT:
			value = old_tp->c_iflag & BRKINT;
			break;
		default:
			value = 0;
			break;
		};
		if (value)
			new_tp->c_saved_flags |= bm->bit;
		else
			new_tp->c_saved_flags &= ~(bm->bit);
	};
	new_tp->c_saved_flags |= save_set;

	new_save_set = new_tp->c_saved_flags & ~restore_set;
	for (bm = saved_bit_list; bm->bit != 0; bm++) {
		/* skip this bit if not in the restore set */
		if ((bm->match & restore_set) == 0) 
			continue;
		/* skip this bit if some other item wants to keep it */
		if ((bm->match & new_save_set) != 0)
			continue;
		if (old_tp->c_saved_flags & bm->bit) {
			switch (bm->bit) {
			case LSAVE_OPOST:
				new_tp->c_oflag |= OPOST;
				break;
			case LSAVE_ISIG:
				new_tp->c_lflag |= ISIG;
				break;
			case LSAVE_ICANON:
				if ((new_tp->c_lflag & ICANON) == 0) {
					new_tp->c_cc[VEOF] =
						old_tp->c_cc[V_SAVED_EOF];
					new_tp->c_cc[VEOL] =
						old_tp->c_cc[V_SAVED_EOL];
				};
				new_tp->c_cc[V_SAVED_EOF] = CDEL;
				new_tp->c_cc[V_SAVED_EOL] = CDEL;
				new_tp->c_lflag |= ICANON;
				break;
			case LSAVE_IUCLC:
				new_tp->c_iflag |= IUCLC;
				break;
			case LSAVE_OLCUC:
				new_tp->c_oflag |= OLCUC;
				break;
			case LSAVE_ICRNL:
				new_tp->c_iflag |= ICRNL;
				break;
			case LSAVE_ONLCR:
				new_tp->c_oflag |= ONLCR;
				break;
			case LSAVE_XCASE:
				new_tp->c_lflag |= XCASE;
				break;
			case LSAVE_IGNCR:
				new_tp->c_iflag |= IGNCR;
				break;
			case LSAVE_BRKINT:
				new_tp->c_iflag |= BRKINT;
				break;
			default:
				break;
			};
			new_tp->c_saved_flags &= ~(bm->bit);
		} else {
			switch (bm->bit) {
			case LSAVE_OPOST:
				new_tp->c_oflag &= ~OPOST;
				break;
			case LSAVE_ISIG:
				new_tp->c_lflag &= ~ISIG;
				break;
			case LSAVE_ICANON:
				if ((new_tp->c_lflag & ICANON) != 0) {
					new_tp->c_cc[VEOF] =
						old_tp->c_cc[V_SAVED_EOF];
					new_tp->c_cc[VEOL] =
						old_tp->c_cc[V_SAVED_EOL];
				};
				new_tp->c_cc[V_SAVED_EOF] = CDEL;
				new_tp->c_cc[V_SAVED_EOL] = CDEL;
				new_tp->c_lflag &= ~ICANON;
				break;
			case LSAVE_IUCLC:
				new_tp->c_iflag &= ~IUCLC;
				break;
			case LSAVE_OLCUC:
				new_tp->c_oflag &= ~OLCUC;
				break;
			case LSAVE_ICRNL:
				new_tp->c_iflag &= ~ICRNL;
				break;
			case LSAVE_ONLCR:
				new_tp->c_oflag &= ~ONLCR;
				break;
			case LSAVE_XCASE:
				new_tp->c_lflag &= ~XCASE;
				break;
			case LSAVE_IGNCR:
				new_tp->c_iflag &= ~IGNCR;
				break;
			case LSAVE_BRKINT:
				new_tp->c_iflag &= ~BRKINT;
				break;
			default:
				break;
			};
		};
	};
	new_tp->c_saved_flags &= ~restore_set;  
					/* 
					 * note:  do not use new_save_set here,
					 * as the saved termio bits are being
					 * changed inside the loop.
					 */
}


static swap_eof_eol(tp)
	struct termio *tp;
{
	char	tc;

	tc = tp->c_cc[VEOF];
	tp->c_cc[VEOF] = tp->c_cc[V_SAVED_EOF];
	tp->c_cc[V_SAVED_EOF] = tc;
	tc = tp->c_cc[VEOL];
	tp->c_cc[VEOL] = tp->c_cc[V_SAVED_EOL];
	tp->c_cc[V_SAVED_EOL] = tc;
}


/*
 * This routine is used in computed transitions for the complex
 * emulated sgttyb bits, RAW, CBREAK, LCASE, and CRMOD.
 *
 * In the following the cases are identified
 * with transitions of the form
 *	{ old saved state, old virtual state, new state }
 *		-> { new saved state, new virtual state }
 * In these, a virtual state of "maybe" means that the
 * bits are in an inconsistent state.  A value of "?"
 * means not yet determined.
 */

struct	bit_sets {
		u_short	save;
		u_short	restore;
		u_short	set_virtual;
		u_short reset_virtual;
	};

static develop_sets(new_bit,old_tp,test_bit,virtual_test,bit_sets)
	int	new_bit;
	struct	termio *old_tp;
	u_short	test_bit;
	int (*virtual_test)();
	struct	bit_sets *bit_sets;
{
	if (new_bit) {
	    /* { ?, ?, on } -> { ? , on } */
	    if (old_tp->c_saved_flags & test_bit) {
		/* { on, ?, on } -> { ? , on } */
		if ((*virtual_test)(old_tp,1)) {
		    /* { on, on, on } -> { on, on } */
		    /* nop */ ;
		} else {
		    /* { on, off or maybe, on } -> { off, on } */
		    bit_sets->restore |= test_bit;
	        };
	    } else {
		/* { off, ?, on } -> { ? , on } */
		if ((*virtual_test)(old_tp,1)) {
		    /* { off, on, on } -> { off, on } */
		    /* nop */ ;
		} else {
		    /* { off, off or maybe, on } -> { on, on } */
		    bit_sets->save |= test_bit;
		    bit_sets->set_virtual |= test_bit;
		};
	    };
	} else {
	    /*  { ?, ?, off } -> { ? , off } */
	    if (old_tp->c_saved_flags & test_bit) {
		/* { on, ?, off } -> { ? , off } */
		if ((*virtual_test)(old_tp,0)) {
		    /* { on, off, off } -> { on, off } */
		    /* nop */ ;
		} else {
		    /* { on, on or maybe, off } -> { off, off } */
		    bit_sets->restore |= test_bit;
	        };
	    } else {
		/* { off, ?, off } -> { ? , off } */
		if ((*virtual_test)(old_tp,0)) {
		    /* { off, off, off } -> { off, off } */
		    /* nop */ ;
		} else {
		    /* { off, on or maybe, off } -> { on, off } */
		    bit_sets->save |= test_bit;
		    bit_sets->reset_virtual |= test_bit;
		};
	    };
	};
}


bsd43_ioctl()
{
	register struct file *fp;
	struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	register int com;
	register u_int size;
	char data[BSD43_IOCPARM_MASK+1];
	int	old_rval1;
#define argi	(* ((int *)data))
	struct	termio termio;
#define ctrl(x)	((x) & 0x1f)
	int	need_set;
	struct	termio old_termio;
	struct	bit_sets bit_sets;
	u_short	base_icanon;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	com = uap->cmd;

	if ((com & (BSD43_IOC_VOID | BSD43_IOC_IN | BSD43_IOC_OUT)) == 0) {
	  	/*
		 *	old style IOCTL:  must be System V
		 */
		ioctl();
		return;
	};
	switch (com) {
	case TIOCSPGRP:
	case FIOCLEX:
	case FIONCLEX:
	case TIOCNOTTY:
		/*
	 	 *	new IOCTL implemented in ioctl():
		 */
		ioctl();
		return;
	default:
		break;
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = (com >> 16) & BSD43_IOCPARM_MASK;
	if (size > sizeof (data)) {
		u.u_error = EFAULT;
		return;
	}
	if (com&BSD43_IOC_IN) {
		if (size == sizeof (int) && uap->cmarg == NULL)
			argi = 0;
		else if (size > 0) {
			u.u_error =
			    bsd_copyin(uap->cmarg, (caddr_t)data, (u_int)size);
			if (u.u_error)
				return;
		} else
			*(caddr_t *)data = uap->cmarg;
	} else if ((com&BSD43_IOC_OUT) && size)
		/*
		 * Zero the buffer on the stack so the user
		 * always gets back something deterministic.
		 */
		bzero((caddr_t)data, size);
	else if (com&BSD43_IOC_VOID)
		*(caddr_t *)data = uap->cmarg;

	switch (com) {
	/*
	 *	emulate the FIO ioctl's (comparable to various fcntl's)
	 */
	case BSD43_FIONBIO:
		u.u_error = fset(fp, FNDELAY,argi);
		return;

	case BSD43_FIOASYNC:
		u.u_error = fset(fp, FASYNC,argi);
		return;

	case BSD43_FIOSETOWN:
		u.u_error = fsetown(fp,argi);
		return;

	case BSD43_FIOGETOWN:
		old_rval1 = u.u_rval1;
		u.u_error = fgetown(fp);
		argi = u.u_rval1;
		u.u_rval1 = old_rval1;
		return;
	
	/*
	 *	map ioctl's which differ only in binary encoding
	 */
	case BSD43_DIOCFMTMAP:
		com = DIOCFMTMAP;
		break;

	case BSD43_DIOCVFYSEC:
		com = DIOCVFYSEC;
		break;

	case BSD43_DIOCGETCTLR:
		com = DIOCGETCTLR;
		break;

	case BSD43_DIOCDIAG:
		com = DIOCDIAG;
		break;

	case BSD43_DIOCSETDP:
		com = DIOCSETDP;
		break;

	case BSD43_DIOCGETVH:
		com = DIOCGETVH;
		break;

	case BSD43_DIOCSETVH:
		com = DIOCSETVH;
		break;

	case BSD43_DIOCNOECC:
		com = DIOCNOECC;
		break;

	case BSD43_DIOCRDEFECTS:
		com = DIOCRDEFECTS;
		break;

	case BSD43_DIOCINITVH:
		com = DIOCINITVH;
		break;

	case BSD43_DIOCRECONFIG:
		com = DIOCRECONFIG;
		break;

	case BSD43_DIOCSOFTCNT:
		com = DIOCSOFTCNT;
		break;

	case BSD43_DIOCSEEK:
		com = DIOCSEEK;
		break;

	case BSD43_DIOCWRTVFY:
		com = DIOCWRTVFY;
		break;

	case BSD43_DIOCREMOVE:
		com = DIOCREMOVE;
		break;

	case BSD43_DIOCDISKCACHE:
		com = DIOCDISKCACHE;
		break;

	case BSD43_DIOCDISKGEOM:
		com = DIOCDISKGEOM;
		break;

	case BSD43_DIOCGETDKTAB:
		com = DIOCGETDKTAB;
		break;

	case BSD43_DIOCADDDKTAB:
		com = DIOCADDDKTAB;
		break;

	case BSD43_DIOCDELDKTAB:
		com = DIOCDELDKTAB;
		break;

	case BSD43_DIOCSETATTR:
		com = DIOCSETATTR;
		break;

	case BSD43_DIOCTRKID:
		com = DIOCTRKID;
		break;

	case BSD43_DIOCGETSIZE:
		com = DIOCGETSIZE;
		break;

	case BSD43_MTIOCGET:
#define argptr ((struct bsd43_mtget *) data)
		fioctl(fp,MTIOCGET,argptr);
		if (u.u_error)
			return;
		switch (argptr->mt_type) {
		case MT_ISQIC:
			argptr->mt_type = BSD43_MT_ISQIC;
			break;
		case MT_ISXM:
			argptr->mt_type = BSD43_MT_ISXY;
			break;
		default:
			argptr->mt_type |= 0x8000;
			break;
		};
		goto return_data;
#undef argptr

	case BSD43_MTIOCTOP:
#define argptr ((struct bsd43_mtop *) data)
		switch (argptr->mt_op) {
		case BSD43_MTWEOF:			
		case BSD43_MTFSF:
		case BSD43_MTBSF:
		case BSD43_MTFSR:
		case BSD43_MTBSR:
		case BSD43_MTREW:
		case BSD43_MTOFFL:
		case BSD43_MTNOP:
			com = MTIOCTOP;
			break;

		case BSD43_MTRET:
			argptr->mt_op = MTRET;
			com = MTIOCTOP;
			break;

		case BSD43_MTRST:
			argptr->mt_op = MTRST;
			com = MTIOCTOP;
			break;

		case BSD43_MTCACHE:
		case BSD43_MTNOCACHE:
		case BSD43_MTERASE:
		case BSD43_MTRETEN:
		default:
			break;
		}
		break;
#undef argptr
	
	/*
	 *	emulate ioctl's which are not defined
	 */
	case BSD43_TIOCSTOP:
		{
			int	xarg;

			xarg = 0;
			fioctl(fp,TCXONC,&xarg);
			break;
		};
		
	case BSD43_TIOCSTART:
		{
			int	xarg;

			xarg = 1;
			fioctl(fp,TCXONC,&xarg);
			break;
		};
		
	case BSD43_TIOCFLUSH:
		{
			int 	xarg;

			switch (argi) {
			case 0:
				xarg = 2;
				break;
			case BSD43_FREAD:
				xarg = 0;
				break;
			case BSD43_FWRITE:
				xarg = 1;
				break;
			default:
				u.u_error = EINVAL;
				return;
			};
			u.u_error = fioctl(fp,TCFLSH,xarg);
			return;
		};

	case BSD43_TIOCGETC:
#define arg_tchars (* ((struct bsd43_tchars *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		arg_tchars.t_intrc = termio.c_cc[VINTR];
		arg_tchars.t_quitc = termio.c_cc[VQUIT];
		arg_tchars.t_startc = termio.c_cc[V_START];
		arg_tchars.t_stopc = termio.c_cc[V_STOP];
		if (termio.c_lflag & ICANON) {
			arg_tchars.t_eofc = termio.c_cc[VEOF];
			arg_tchars.t_brkc = termio.c_cc[VEOL];
		} else {
			arg_tchars.t_eofc = termio.c_cc[V_SAVED_EOF];
			arg_tchars.t_brkc = termio.c_cc[V_SAVED_EOL];
		};
		goto return_data;
#undef arg_tchars

	case BSD43_TIOCGETD:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		switch (termio.c_line) {
		case LDISC0:
			argi = BSD43_OTTYDISC;
			break;
		case LDISC_NEW:
			argi = BSD43_NTTYDISC;
			break;
		case LDISC_SLIP:
			argi = BSD43_SLIPDISC;
			break;
		default:
			argi = termio.c_line ^ C_BSD43_LINE;
			break;
		};
		goto return_data;

	case BSD43_TIOCGETP:
#define arg_sgttyb (* ((struct bsd43_sgttyb *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		arg_sgttyb.sg_ispeed =
			arg_sgttyb.sg_ospeed = termio.c_cflag & CBAUD;
		arg_sgttyb.sg_erase = termio.c_cc[VERASE];
		arg_sgttyb.sg_kill = termio.c_cc[VKILL];
		if (termio.c_iflag & IXOFF) 
			arg_sgttyb.sg_flags |= BSD43_TANDEM;
		if (((termio.c_saved_flags & LSAVE_RAW) != 0)
			? bsd43_ioctl_virtual_raw(&termio,1) 
			: ! bsd43_ioctl_virtual_raw(&termio,0))
			arg_sgttyb.sg_flags |= BSD43_RAW;
		if (((termio.c_saved_flags & LSAVE_CBREAK) != 0)
			? bsd43_ioctl_virtual_cbreak(&termio,1) 
			: ! bsd43_ioctl_virtual_cbreak(&termio,0))
			arg_sgttyb.sg_flags |= BSD43_CBREAK;
		if (((termio.c_saved_flags & LSAVE_CRMOD) != 0)
			? bsd43_ioctl_virtual_crmod(&termio,1) 
			: ! bsd43_ioctl_virtual_crmod(&termio,0))
			arg_sgttyb.sg_flags |= BSD43_CRMOD;
		if (((termio.c_saved_flags & LSAVE_LCASE) != 0)
			? bsd43_ioctl_virtual_lcase(&termio,1) 
			: ! bsd43_ioctl_virtual_lcase(&termio,0))
			arg_sgttyb.sg_flags |= BSD43_LCASE;
		if (termio.c_lflag & ECHO) 
			arg_sgttyb.sg_flags |= BSD43_ECHO;
		arg_sgttyb.sg_flags |= (BSD43_EVENP | BSD43_ODDP);
		if ((termio.c_oflag & NLDLY) == NL1)
			arg_sgttyb.sg_flags |= BSD43_NL1;
		switch (termio.c_oflag & TABDLY) {
		case TAB0:
			arg_sgttyb.sg_flags |= BSD43_TAB0;
			break;
		case TAB1:
			arg_sgttyb.sg_flags |= BSD43_TAB1;
			break;
		case TAB2:
			arg_sgttyb.sg_flags |= BSD43_TAB2;
			break;
		case TAB3:
			arg_sgttyb.sg_flags |= BSD43_XTABS;
			break;
		};
		switch (termio.c_oflag & CRDLY) {
		case CR0:
			arg_sgttyb.sg_flags |= BSD43_CR0;
			break;
		case CR1:
			arg_sgttyb.sg_flags |= BSD43_CR1;
			break;
		case CR2:
			arg_sgttyb.sg_flags |= BSD43_CR2;
			break;
		case CR3:
			arg_sgttyb.sg_flags |= BSD43_CR3;
			break;
		};
		if ((termio.c_oflag & FFDLY) == FF1)
			arg_sgttyb.sg_flags |= BSD43_FF1;
		if ((termio.c_oflag & BSDLY) == BS1)
			arg_sgttyb.sg_flags |= BSD43_BS1;

		goto return_data;
#undef arg_sgttyb

	case BSD43_TIOCGLTC:
#define arg_ltchars (* ((struct bsd43_ltchars *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		arg_ltchars.t_suspc = termio.c_cc[V_SUSP];
		arg_ltchars.t_dsuspc = termio.c_cc[V_DSUSP];
		arg_ltchars.t_rprntc = termio.c_cc[V_RPRNT];
		arg_ltchars.t_flushc = termio.c_cc[V_FLUSH];
		arg_ltchars.t_werasc = termio.c_cc[V_WERAS];
		arg_ltchars.t_lnextc = termio.c_cc[V_LNEXT];
		goto return_data;
#undef arg_ltchars

	case BSD43_TIOCHPCL:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;
		if (termio.c_cflag & HUPCL)
			return;
		termio.c_cflag |= HUPCL;
		u.u_error = fioctl(fp,TCSETA,&termio);
		return;
		
	case BSD43_TIOCLBIC:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;
		ftioclbic(fp,argi,&termio);
		return;
		
	case BSD43_TIOCLBIS:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;
		ftioclbis(fp,argi,&termio);
		return;
		
	case BSD43_TIOCLGET:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		if (termio.c_lflag & ECHOE)
			argi |= BSD43_LCRTERA;
		if (((termio.c_saved_flags & LSAVE_RAW) != 0) 
			? ((termio.c_saved_flags & LSAVE_OPOST) == 0)
			: ((termio.c_oflag & OPOST) == 0))
			argi |= BSD43_LLITOUT;
		argi |= BSD43_LPASS8;
		if ((termio.c_iflag & IXANY) == 0)
			argi |= BSD43_LDECCTQ;
		if (termio.c_lflag & NOFLSH)
			argi |= BSD43_LNOFLSH;
		if (termio.c_lflag & ECHOK)
			argi |= BSD43_LCRTKIL;
		if (termio.c_lflag & LNEW_CRTBS)
			argi |= BSD43_LCRTBS;
		if (termio.c_cflag & CNEW_MDMBUF)
			argi |= BSD43_LMDMBUF;
		if ((termio.c_cflag & HUPCL) == 0)
			argi |= BSD43_LNOHANG;
		if (termio.c_lflag & LNEW_CTLECH)
			argi |= BSD43_LCTLECH;
		if (termio.c_lflag & LNEW_PRTERA)
			argi |= BSD43_LPRTERA;
		goto return_data;

	case BSD43_TIOCLSET:
		{
			struct termio initial_termio;
			int	real_error;

			u.u_error = fioctl(fp,TCGETA,&termio);
			if (u.u_error)
				return;
			bcopy(&termio,&initial_termio,sizeof(termio));
			ftioclbis(fp,argi,&termio);
			if (! u.u_error) {
				argi = ~argi;
				ftioclbic(fp,argi,&termio);
			};
			if (u.u_error) {
				real_error = u.u_error;
				fioctl(fp,TCSETA,&initial_termio);
				u.u_error = real_error;
			};
			return;
		};

	case BSD43_TIOCSETC:
#define arg_tchars (* ((struct bsd43_tchars *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;
		need_set = 1;
		if (termio.c_cc[VINTR] != arg_tchars.t_intrc) {
		  	termio.c_cc[VINTR] = arg_tchars.t_intrc;
			need_set = 1;
		};
		if (termio.c_cc[VQUIT] != arg_tchars.t_quitc) {
			termio.c_cc[VQUIT] = arg_tchars.t_quitc;
			need_set = 1;
		};
		if (termio.c_cc[V_START] != arg_tchars.t_startc) {
			termio.c_cc[V_START] = arg_tchars.t_startc;
			need_set = 1;
		};
		if (termio.c_cc[V_STOP] != arg_tchars.t_stopc) {
			termio.c_cc[V_STOP] = arg_tchars.t_stopc;
			need_set = 1;
		};
		if (termio.c_lflag & ICANON) {
			if (termio.c_cc[VEOF] != arg_tchars.t_eofc) {
				termio.c_cc[VEOF] = arg_tchars.t_eofc;
				need_set = 1;
			};
			if (termio.c_cc[VEOL] != arg_tchars.t_brkc) {
				termio.c_cc[VEOL] = arg_tchars.t_brkc;
				need_set = 1;
			};
		} else {
			if (termio.c_cc[V_SAVED_EOF] != arg_tchars.t_eofc) {
				termio.c_cc[V_SAVED_EOF] = arg_tchars.t_eofc;
				need_set = 1;
			};
			if (termio.c_cc[V_SAVED_EOL] != arg_tchars.t_brkc) {
				termio.c_cc[V_SAVED_EOL] = arg_tchars.t_brkc;
				need_set = 1;
			};
		};
	
		if (need_set)
			u.u_error = fioctl(fp,
				       TCSETA,
				       &termio);
		return;
#undef arg_tchars

	case BSD43_TIOCSETD:
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;
		switch (argi) {
		case BSD43_OTTYDISC:
			argi = LDISC0;
			break;
		case BSD43_NTTYDISC:
			argi = LDISC_NEW;
			break;
		case BSD43_SLIPDISC:
			argi = LDISC_SLIP;
			break;
		default:
			argi ^= C_BSD43_LINE;
			break;
		};
		if (argi == LDISC1 &&
		    termio.c_line == LDISC0) {
			if (termio.c_cc[V_LNEXT] == CESC)
				termio.c_cc[V_LNEXT] = CLNEXT;
		} else if (argi == LDISC_NEW &&
		    termio.c_line == LDISC0) {
			if (termio.c_cc[V_SUSP] == CDEL)
			  	termio.c_cc[V_SUSP] = CSUSP;
			if (termio.c_cc[V_DSUSP] == CDEL)
			  	termio.c_cc[V_DSUSP] = CDSUSP;
			if (termio.c_cc[V_LNEXT] == CESC)
				termio.c_cc[V_LNEXT] = CLNEXT;
		} else if (argi == LDISC0 &&
			   (termio.c_line == LDISC1 ||
			    termio.c_line == LDISC_NEW)) {
			if (termio.c_line == LDISC_NEW) {
				if (termio.c_cc[V_SUSP] == CSUSP)
				  	termio.c_cc[V_SUSP] = CDEL;
				if (termio.c_cc[V_DSUSP] == CDSUSP)
				  	termio.c_cc[V_DSUSP] = CDEL;
			};
			if (termio.c_cc[V_LNEXT] == CLNEXT)
				termio.c_cc[V_LNEXT] = CESC;
		};
		termio.c_line = argi;
		fioctl(fp,TCSETAW,&termio);
		return;		

	case BSD43_TIOCSETN:
	case BSD43_TIOCSETP:
#define arg_sgttyb (* ((struct bsd43_sgttyb *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		old_termio = termio;

		if (arg_sgttyb.sg_ispeed != arg_sgttyb.sg_ospeed) {
			u.u_error = EINVAL;
			return;
		};
		termio.c_cflag &= ~CBAUD;
		termio.c_cflag |= (arg_sgttyb.sg_ispeed & CBAUD);
		
		termio.c_cc[VERASE] = arg_sgttyb.sg_erase;
		termio.c_cc[VKILL] = arg_sgttyb.sg_kill;

		if (arg_sgttyb.sg_flags & BSD43_TANDEM) {
			termio.c_iflag |= IXOFF;
		} else {
			termio.c_iflag &= ~IXOFF;
		};

		/*
		 * The following code attempts to minimize surprising
		 * behavior of the emulation of the 4.3 BSD RAW, CBREAK,
		 * LCASE, and CRMOD flags, all of which affect multiple
		 * termio bits.
		 * We first develop a set of transitions to make:
		 */
		bit_sets.save = 0;
		bit_sets.restore = 0;
		bit_sets.set_virtual = 0;
		bit_sets.reset_virtual = 0;

		develop_sets(arg_sgttyb.sg_flags & BSD43_RAW,&old_termio,
			     LSAVE_RAW,bsd43_ioctl_virtual_raw,&bit_sets);
		develop_sets(arg_sgttyb.sg_flags & BSD43_CBREAK,&old_termio,
			     LSAVE_CBREAK,bsd43_ioctl_virtual_cbreak,&bit_sets);
		develop_sets(arg_sgttyb.sg_flags & BSD43_LCASE,&old_termio,
			     LSAVE_LCASE,bsd43_ioctl_virtual_lcase,&bit_sets);
		develop_sets(arg_sgttyb.sg_flags & BSD43_CRMOD,&old_termio,
			     LSAVE_CRMOD,bsd43_ioctl_virtual_crmod,&bit_sets);

		/*
		 * Now copy the saved bits as appropriate
		 */
		save_and_restore_bits(&old_termio,&termio,bit_sets.save,
				      bit_sets.restore);

		/*
		 * Now set the bits as required for new "virtual" states
		 * We do these setting operations in ascending order
		 * of precedence.
		 */
		base_icanon = termio.c_lflag & ICANON;
		if (bit_sets.reset_virtual & LSAVE_RAW) {
			if ((termio.c_saved_flags & LSAVE_OPOST) != 0)
				termio.c_oflag |= OPOST;
			else
				termio.c_oflag &= ~OPOST;
			termio.c_lflag |= (ISIG | ICANON);
			termio.c_iflag &= ~(IUCLC | IGNCR | ICRNL | INLCR |
					BRKINT);
		};
		if (bit_sets.set_virtual & LSAVE_LCASE) {
			termio.c_lflag |= XCASE;
			termio.c_iflag |= IUCLC;
			termio.c_oflag |= OLCUC;
		};
		if (bit_sets.reset_virtual & LSAVE_LCASE) {
			termio.c_lflag &= ~XCASE;
			termio.c_iflag &= ~IUCLC;
			termio.c_oflag &= ~OLCUC;
		};
		if (bit_sets.set_virtual & LSAVE_CRMOD) {
			termio.c_iflag |= ICRNL;
			termio.c_oflag |= ONLCR;
		};
		if (bit_sets.reset_virtual & LSAVE_CRMOD) {
			termio.c_iflag &= ~ICRNL;
			termio.c_oflag &= ~ONLCR;
		};
		if (bit_sets.set_virtual & LSAVE_CBREAK) {
			termio.c_lflag |= ISIG;
			termio.c_lflag &= ~ICANON;
			termio.c_iflag |= BRKINT;
		};
		if (bit_sets.reset_virtual & LSAVE_CBREAK) {
			termio.c_lflag |= (ISIG | ICANON);
			termio.c_iflag |= BRKINT;
		};
		if (bit_sets.set_virtual & LSAVE_RAW) {
			termio.c_oflag &= ~OPOST;
			termio.c_lflag &= ~(ISIG | ICANON);
			termio.c_iflag &= ~(IUCLC | IGNCR | ICRNL | INLCR |
					BRKINT);
		};

		/*
		 * If we are setting or resettng 
		 * a virtual mode which changed ICANON,
		 * swap eof and eol with saved values.
		 */
		if (base_icanon != (termio.c_lflag & ICANON)) 
			swap_eof_eol(&termio);

		if (arg_sgttyb.sg_flags & BSD43_ECHO) {
			termio.c_lflag |= ECHO;
		} else {
			termio.c_lflag &= ~ECHO;
		};
		if (arg_sgttyb.sg_flags & BSD43_NLDELAY) {
			termio.c_oflag &= ~NLDLY;
			termio.c_oflag |= NL1;
		} else {
			termio.c_oflag &= ~NLDLY;
		};

		switch(arg_sgttyb.sg_flags & BSD43_TBDELAY) {
		case BSD43_TAB0:
			termio.c_oflag &= ~TABDLY;
			termio.c_oflag |= TAB0;
			break;
		case BSD43_TAB1:
			termio.c_oflag &= ~TABDLY;
			termio.c_oflag |= TAB1;
			break;
		case BSD43_TAB2:
			termio.c_oflag &= ~TABDLY;
			termio.c_oflag |= TAB2;
			break;
		case BSD43_XTABS:
			termio.c_oflag &= ~TABDLY;
			termio.c_oflag |= TAB3;
			break;
		};
		switch(arg_sgttyb.sg_flags & BSD43_TBDELAY) {
		case BSD43_CR0:
			termio.c_oflag &= ~CRDLY;
			termio.c_oflag |= CR0;
			break;
		case BSD43_CR1:
			termio.c_oflag &= ~CRDLY;
			termio.c_oflag |= CR1;
			break;
		case BSD43_CR2:
			termio.c_oflag &= ~CRDLY;
			termio.c_oflag |= CR2;
			break;
		case BSD43_CR3:
			termio.c_oflag &= ~CRDLY;
			termio.c_oflag |= CR3;
			break;
		};
		if (arg_sgttyb.sg_flags & BSD43_VTDELAY) {
			termio.c_oflag &= ~FFDLY;
			termio.c_oflag |= FF1;
		} else {
			termio.c_oflag &= ~FFDLY;
			termio.c_oflag |= FF0;
		};

		if (arg_sgttyb.sg_flags & BSD43_BSDELAY) {
			termio.c_oflag &= ~BSDLY;
			termio.c_oflag |= BS1;
		} else {
			termio.c_oflag &= ~BSDLY;
			termio.c_oflag |= BS0;
		};

		if (bcmp((caddr_t) &old_termio, (caddr_t) &termio,
			 sizeof(struct termio)))
			u.u_error = fioctl(fp,
				       (com == BSD43_TIOCSETP ? TCSETAF : TCSETA),
				       &termio);
		return;
#undef arg_sgttyb

	case BSD43_TIOCSLTC:
#define arg_ltchars (* ((struct bsd43_ltchars *) data))
		u.u_error = fioctl(fp,TCGETA,&termio);
		if (u.u_error)
			return;

		need_set = 0;
		if (termio.c_cc[V_SUSP] != arg_ltchars.t_suspc) {
			termio.c_cc[V_SUSP] = arg_ltchars.t_suspc;
			need_set = 1;
		};
		if (termio.c_cc[V_DSUSP] != arg_ltchars.t_dsuspc) {
			termio.c_cc[V_DSUSP] = arg_ltchars.t_dsuspc;
			need_set = 1;
		};
		if (termio.c_cc[V_RPRNT] != arg_ltchars.t_rprntc) {
			termio.c_cc[V_RPRNT] = arg_ltchars.t_rprntc;
			need_set = 1;
		};
		if (termio.c_cc[V_FLUSH] != arg_ltchars.t_flushc) {
			termio.c_cc[V_FLUSH] = arg_ltchars.t_flushc;
			need_set = 1;
		};
		if (termio.c_cc[V_WERAS] != arg_ltchars.t_werasc) {
			termio.c_cc[V_WERAS] = arg_ltchars.t_werasc;
			need_set = 1;
		};
		if (termio.c_cc[V_LNEXT] != arg_ltchars.t_lnextc) {
			termio.c_cc[V_LNEXT] = arg_ltchars.t_lnextc;
			need_set = 1;
		};
		if (need_set)
			u.u_error = fioctl(fp,
				       TCSETA,
				       &termio);
		return;
#undef arg_ltchars

	/*
	 *	pass through implemented BSD43 ioctl's unchanged
	 */
	case BSD43_MTIOCEEOT:
	case BSD43_MTIOCIEOT:
	case BSD43_SIOCGIFSTATS:
	case BSD43_TIOCCBRK:
	case BSD43_TIOCCDTR:
	case BSD43_TIOCSBRK:
	case BSD43_TIOCSDTR:
	case BSD43_TIOCMODG:
	case BSD43_TIOCMODS:
	case BSD43_TIOCMBIC:
	case BSD43_TIOCMBIS:
	case BSD43_TIOCMGET:
	case BSD43_TIOCMSET:
	/*
	 *	pass through unknown or future ioctl's unchanged
	 */
 	default:
		break;
	}
	u.u_error = fioctl(fp, com, data);
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
return_data:
	if (u.u_error == 0 && (com&BSD43_IOC_OUT) && size)
		u.u_error = bsd_copyout(data, uap->cmarg, (u_int)size);

#undef argi
}


ftioclbic(fp,argi,termio)
	register struct file *fp;
	int	argi;
	struct	termio *termio;
{
	int	need_set;

	need_set = 0;
	if (argi & BSD43_LCRTERA &&
	    termio->c_lflag & ECHOE) {
		termio->c_lflag &= ~ECHOE;
		need_set = 1;
	};
	if (argi & BSD43_LCRTKIL &&
	    termio->c_lflag & ECHOK) {
		termio->c_lflag &= ~ECHOK;
		need_set = 1;
	};
	if (argi & BSD43_LLITOUT) {
		if ((termio->c_saved_flags & LSAVE_RAW) != 0) {
			if ((termio->c_saved_flags & LSAVE_OPOST) == 0) {
				termio->c_saved_flags |= LSAVE_OPOST;
				need_set = 1;
			};
		} else {
			if ((termio->c_oflag & OPOST) == 0) {
				termio->c_oflag |= OPOST;
				need_set = 1;
			};
		};
	};
	if (argi & BSD43_LDECCTQ &&
	    (termio->c_iflag & IXANY) == 0) {
		termio->c_iflag |= IXANY;
		need_set = 1;
	};
	if (argi & BSD43_LNOFLSH &&
	    termio->c_lflag & NOFLSH) {
		termio->c_lflag &= ~NOFLSH;
		need_set = 1;
	};
	if (argi & BSD43_LCRTBS &&
	    termio->c_lflag & LNEW_CRTBS) {
		termio->c_lflag &= ~LNEW_CRTBS;
		need_set = 1;
	};
	if (argi & BSD43_LMDMBUF &&
	    termio->c_cflag & CNEW_MDMBUF) {
		termio->c_cflag &= ~CNEW_MDMBUF;
		need_set = 1;
	};
	if (argi & BSD43_LNOHANG &&
	    (termio->c_cflag & HUPCL) == 0) {
		termio->c_cflag |= HUPCL;
		need_set = 1;
	};
	if (argi & BSD43_LFLUSHO &&
	    termio->c_lflag & LNEW_FLUSHO) {
		termio->c_lflag &= ~LNEW_FLUSHO;
		need_set = 1;
	};
	if (argi & BSD43_LTOSTOP &&
	    termio->c_lflag & TOSTOP) {
		termio->c_lflag &= ~TOSTOP;
		need_set = 1;
	};
	if (argi & BSD43_LPENDIN &&
	    termio->c_lflag & LNEW_PENDIN) {
		termio->c_lflag &= ~LNEW_PENDIN;
		need_set = 1;
	};
	if (argi & BSD43_LPRTERA &&
	    termio->c_lflag & LNEW_PRTERA) {
		termio->c_lflag &= ~LNEW_PRTERA;
		need_set = 1;
	};
	if (argi & BSD43_LCTLECH &&
	    termio->c_lflag & LNEW_CTLECH) {
		termio->c_lflag &= ~LNEW_CTLECH;
		need_set = 1;
	};

	if (need_set)
		u.u_error = fioctl(fp,TCSETA,termio);
}

ftioclbis(fp,argi,termio)
	register struct file *fp;
	int	argi;
	struct	termio	*termio;
{
	int	need_set;

	need_set = 0;
	if (argi & BSD43_LCRTERA &&
	    (termio->c_lflag & ECHOE) == 0) {
		termio->c_lflag |= ECHOE;
		need_set = 1;
	};
	if (argi & BSD43_LCRTKIL &&
	    (termio->c_lflag & ECHOK) == 0) {
		termio->c_lflag |= ECHOK;
		need_set = 1;
	};
	if (argi & BSD43_LLITOUT) {
		if ((termio->c_saved_flags & LSAVE_RAW) != 0) {
			if ((termio->c_saved_flags & LSAVE_OPOST) != 0) {
				termio->c_saved_flags &= ~LSAVE_OPOST;
				need_set = 1;
			};
		} else {
			if ((termio->c_oflag & OPOST) != 0) {
				termio->c_oflag &= ~OPOST;
				need_set = 1;
			};
		};
	};
	if (argi & BSD43_LDECCTQ &&
	    termio->c_iflag & IXANY) {
		termio->c_iflag &= ~IXANY;
		need_set = 1;
	};
	if (argi & BSD43_LNOFLSH &&
		    (termio->c_lflag & NOFLSH) == 0) {
		termio->c_lflag |= NOFLSH;
		need_set = 1;
	};
	if (argi & BSD43_LCRTBS &&
	    (termio->c_lflag & LNEW_CRTBS) == 0) {
		termio->c_lflag |= LNEW_CRTBS;
		need_set = 1;
	};
	if (argi & BSD43_LMDMBUF &&
	    (termio->c_cflag & CNEW_MDMBUF) == 0) {
		termio->c_cflag |= CNEW_MDMBUF;
		need_set = 1;
	};
	if (argi & BSD43_LNOHANG &&
	    termio->c_cflag & HUPCL) {
		termio->c_cflag &= ~HUPCL;
		need_set = 1;
	};
	if (argi & BSD43_LFLUSHO &&
	    (termio->c_lflag & LNEW_FLUSHO) == 0) {
		termio->c_lflag |= LNEW_FLUSHO;
		need_set = 1;
	};
	if (argi & BSD43_LTOSTOP &&
	    (termio->c_lflag & TOSTOP) == 0) {
		termio->c_lflag |= TOSTOP;
		need_set = 1;
	};
	if (argi & BSD43_LPENDIN &&
	    (termio->c_lflag & LNEW_PENDIN) == 0) {
		termio->c_lflag |= LNEW_PENDIN;
		need_set = 1;
	};
	if (argi & BSD43_LPRTERA &&
	    (termio->c_lflag & LNEW_PRTERA) == 0) {
		termio->c_lflag |= LNEW_PRTERA;
		need_set = 1;
	};
	if (argi & BSD43_LCTLECH &&
	    (termio->c_lflag & LNEW_CTLECH) == 0) {
		termio->c_lflag |= LNEW_CTLECH;
		need_set = 1;
	};
	if (need_set)
		u.u_error = fioctl(fp,TCSETA,termio);
}
