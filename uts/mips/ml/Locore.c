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
#ident	"$Header: Locore.c,v 1.4.4.2 90/05/10 05:40:55 wje Exp $"

#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/mount.h"
#include "sys/inode.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/pcb.h"
#include "sys/message.h"
#include "sys/stream.h"
#include "sys/strstat.h"
#include "sys/sema.h"
#include "sys/comm.h"
#include "sys/dir.h"
#include "sys/file.h"
#include "sys/signal.h"
#include "sys/proc.h"
#include "sys/user.h"
struct tty {
	int foo;
};
#include "sys/conf.h"

#undef NESTED
#define NESTED(a, b, c)
#undef VECTOR
#define VECTOR(a, b)
#undef LEAF
#define LEAF(a)
#undef EXPORT
#define EXPORT(a)

#undef u
struct user u;
unsigned int userstack[1];

EXPORT(prom_reset)

prom_exec() {}

EXPORT(prom_restart)

prom_reinit() {}
prom_reboot() {}
prom_autoboot() {}

EXPORT(prom_getchar)
EXPORT(prom_putchar)
EXPORT(andb_rmw)
EXPORT(andh_rmw)
EXPORT(andw_rmw)
EXPORT(orb_rmw)
EXPORT(orh_rmw)
EXPORT(orw_rmw)
EXPORT(dprintf)
EXPORT(prom_open)
EXPORT(prom_close)
EXPORT(prom_read)
EXPORT(prom_ioctl)
EXPORT(prom_disablecmd)

extern int dumpsys();
doadump() { dumpsys(); }

checkfp(procp, exiting) struct proc *procp; int exiting; {}

NESTED(utlbmiss, 0, k1)
EXPORT(eutlbmiss)
VECTOR(exception, M_EXCEPT)
	EXPORT(eexception)
VECTOR(VEC_int, M_EXCEPT|M_TFISAVE)
VECTOR(VEC_int, M_EXCSAVE)
VECTOR(VEC_tlbmod, M_EXCEPT|M_TFISAVE)
VECTOR(VEC_tlbmod, M_EXCSAVE)
VECTOR(VEC_tlbmiss, M_EXCEPT|M_TFISAVE)
VECTOR(VEC_tlbmiss, M_EXCSAVE)
VECTOR(VEC_addrerr, M_EXCEPT)
VECTOR(VEC_ibe, M_EXCEPT)
VECTOR(VEC_dbe, M_EXCEPT)
VECTOR(VEC_trap, M_EXCEPT|M_TFISAVE|M_TRAP)
VECTOR(VEC_trap, M_TRAPSAVE)
VECTOR(VEC_nofault, M_EXCEPT|M_TFISAVE)
VECTOR(VEC_nofault, M_EXCSAVE)
VECTOR(VEC_syscall, M_EXCEPT|M_SYSCALL)
VECTOR(VEC_syscall, M_SYSCALLSAVE)
VECTOR(VEC_breakpoint, M_EXCEPT)
EXPORT(sstepbp)
VECTOR(VEC_breakpoint, foobobj)
VECTOR(VEC_cpfault, M_EXCEPT)
LEAF(tfi_save)
LEAF(tfi_restore)

spl0() {}
spl1() {}
spl5() { return(0); }
spltty() { return(0); }
spl6() { return(0); }		/* not currently used */
splnet() { return(0); }
spl7() { return(0); }
splhi() { return(0); }
splimp() { return(0); }

get_cause() { return(0); }

/* ARGSUSED */
splx(s) int s; {}

#ifdef notdef
acksoftnet() {}
setsoftclock() {}
acksoftclock() {}
#endif notdef

/* ARGSUSED */
siron(level) int level; {}
/* ARGSUSED */
siroff(level) int level; {}
VECTOR(exception_exit, M_EXCEPT)
VECTOR(VEC_unexp, M_EXCEPT)
EXPORT(mfc0_start)
EXPORT(mfc0_end)
/* ARGSUSED */
badaddr(addr, len) caddr_t addr; int len; { return(0); }

#ifdef notdef
wbadaddr(addr, len)
wbadmemaddr(addr)
#endif /* notdef */

NESTED(baerror, BADADDRFRM, zero)

/* ARGSUSED */
ffs(word) long word; { return(0); }

#ifdef notdef
ffintr(cause_register) -- find first bit set in interrupt pending byte
#endif /* notdef */

/* ARGSUSED */
scanc(size, cp, table, mask)
    unsigned size; char *cp, table[]; int mask; { return(0); }

#ifdef notdef
Xchecksum(addr, len, prevcksum)
#endif /* notdef */
LEAF(nuxi_s)
LEAF(nuxi_l)
LEAF(wbflush)
int	icode[8];
EXPORT(icode_argv)
EXPORT(icode_file)
EXPORT(icode_args)
EXPORT(icode_argc)
EXPORT(eicode)

cntl_p() {}
stackdepth() {}
save() {}
/* ARGSUSED */
resume(p) struct proc *p; {}

/* ARGSUSED */
setjmp(jmp_buf) label_t *jmp_buf; { return(0); }
/* ARGSUSED */
longjmp(jmp_buf) label_t *jmp_buf; { /*NOTREACHED*/ }

unmaptlb(rpid, vpage) int rpid; unsigned int vpage; {}
unmodtlb(rpid, vpage) int rpid; unsigned int vpage; {}
invaltlb(i) int i; {}
tlbwired(indx, tlbpid, vaddr, pte)
	int indx; int tlbpid; pde_t *vaddr; unsigned int pte; {}
tlbdropin(tlbpid, vaddr, pte)
	int tlbpid; pde_t *vaddr; unsigned int pte; {}
flush_tlb() {}
set_tlbpid(tlbpid) int tlbpid; {}

LEAF(copypage)
/* ARGSUSED */
copyseg(udaddr, pf) caddr_t udaddr; unsigned int pf; {}
/* ARGSUSED */
clearseg(dst_ppn) unsigned int dst_ppn; {}

/* ARGSUSED */
copyin(user_src, kernel_dst, bcount)
	caddr_t user_src, kernel_dst; unsigned int bcount; { return(0); }
/* ARGSUSED */
copyout(kernel_src, user_dst, bcount)
	caddr_t kernel_src, user_dst; unsigned int bcount; { return(0); }
NESTED(cerror, COPYIOFRM, zero)
/* ARGSUSED */
bcopy(src, dst, bcount) caddr_t src, dst; unsigned int bcount; {}
/* ARGSUSED */
bzero(dst, bcount) caddr_t dst; unsigned int bcount; {}
/* ARGSUSED */
blkclr(base, count) caddr_t base; unsigned int count; {}

/* ARGSUSED */
bcmp(src, dst, bcount) caddr_t src, dst; unsigned int bcount; {}

typedef	struct uprof {		/* profile arguments */
	short	*pr_base;	/* buffer base */
	unsigned int pr_size;	/* buffer size */
	unsigned int pr_off;	/* pc offset */
	unsigned int pr_scale;	/* pc scaling */
}
/* ARGSUSED */
addupc(pc, prof, ticks) int pc; struct uprof *prof; int ticks; {}

LEAF(adderr)
/* ARGSUSED */
fubyte(from) caddr_t from; { return(0); }
/* ARGSUSED */
fuibyte(from) caddr_t from; { return(0); }
/* ARGSUSED */
subyte(to, count) caddr_t to; int count; { return(0); }
/* ARGSUSED */
suibyte(to, count) caddr_t to; int count; { return(0); }
/* ARGSUSED */
fuword(from) caddr_t from; { return(0); }
/* ARGSUSED */
fuiword(from) caddr_t from; { return(0); }
/* ARGSUSED */
suword(to, count) caddr_t to; int count; { return(0); }
/* ARGSUSED */
suiword(to, count) caddr_t to; int count; { return(0); }

LEAF(uerror)

/* ARGSUSED */
useracc(addr, bcnt, rw) caddr_t addr; unsigned int bcnt; int rw; { return(0); }

LEAF(uaerror)
LEAF(strlen)
clean_cache(base, count) caddr_t base; unsigned int count; {}
flush_cache() {}
config_cache() {}
size_cache() {}
/* ARGSUSED */
searchdir(buf, n, target) caddr_t buf; unsigned short n; char *target; {}

NESTED(softfp, FRAME_SIZE, ra)
LEAF(softfp_unusable)
NESTED(post_signal, FRAME_SIZE, ra)
EXPORT(softfp_adderr)
EXPORT(softfp_insterr)
LEAF(emulate_instr)
NESTED(flush_ea, EF_SIZE, zero)
LEAF(get_fpc_csr)
LEAF(set_fpc_csr)
LEAF(get_fpc_eir)

get_cpu_irr() {}
get_fpc_irr() {}

LEAF(set_fpc_led)
LEAF(reviderror)
NESTED(fp_intr, FRAME_SIZE, ra)

#ifdef notdef BSD
#include "dz.h"
#include "mba.h"
#include "uba.h"

#include "pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "vm.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "buf.h"
#include "msgbuf.h"
#include "mbuf.h"
#include "protosw.h"
#include "domain.h"
#include "map.h"

#include "ioa.h"
#include "ka630.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

struct	scb scb;
int	(*UNIvec[128])();
#if NUBA > 1
int	(*UNI1vec[128])();
#endif
struct	rpb rpb;
int	dumpflag;
int	intstack[3*NBPG];
int	masterpaddr;		/* p_addr of current process on master cpu */
struct	user u;
int	icode[8];
int	szicode = sizeof (icode);
/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.3 BSD UNIX ....";
int	etext;


lowinit()
{
#if !defined(GPROF)
	caddr_t cp;
#endif
	extern int dumpmag;
	extern int rthashsize;
	extern int arptab_size;
	extern int dk_ndrive;
	extern struct domain unixdomain;
#ifdef PUP
	extern struct domain pupdomain;
#endif
#ifdef INET
	extern struct domain inetdomain;
#endif
#include "imp.h"
#if NIMP > 0
	extern struct domain impdomain;
#endif
#ifdef NS
	extern struct domain nsdomain;
#endif

	/* cpp messes these up for lint so put them here */
	unixdomain.dom_next = domains;
	domains = &unixdomain;
#ifdef PUP
	pupdomain.dom_next = domains;
	domains = &pupdomain;
#endif
#ifdef INET
	inetdomain.dom_next = domains;
	domains = &inetdomain;
#endif
#if NIMP > 0
	impdomain.dom_next = domains;
	domains = &impdomain;
#endif
#ifdef NS
	nsdomain.dom_next = domains;
	domains = &nsdomain;
#endif
	dumpmag = 0;			/* used only by savecore */
	rthashsize = rthashsize;	/* used by netstat, etc. */
	arptab_size = arptab_size;	/* used by arp command */
	dk_ndrive = dk_ndrive;		/* used by vmstat, iostat, etc. */

	/*
	 * Pseudo-uses of globals.
	 */
	lowinit();
	intstack[0] = intstack[1];
	rpb = rpb;
	scb = scb;
	maxmem = physmem = freemem = 0;
	u = u;
	fixctlrmask();
	main(0);
	Xustray();

	/*
	 * Routines called from interrupt vectors.
	 */
	panic("Machine check");
	printf("Write timeout");
	(*UNIvec[0])();
#if NUBA > 1
	(*UNI1vec[0])();
#endif
	ubaerror(0, (struct uba_hd *)0, 0, 0, (struct uba_regs *)0);
	cnrint(0);
	cnxint(0);
	consdin();
	consdout();
#if NDZ > 0
	dzdma();
#endif
#if NMBA > 0
	mbintr(0);
#endif
	hardclock((caddr_t)0, 0);
	softclock((caddr_t)0, 0);
	trap(0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, (unsigned)0, 0, 0);
	rawintr();
#ifdef INET
	ipintr();
#endif
#ifdef NS
	nsintr();
#endif

	if (vmemall((struct pte *)0, 0, (struct proc *)0, 0))
		return;		/* use value */
	machinecheck((caddr_t)0);
	memerr();
	boothowto = 0;
	dumpflag = 0; dumpflag = dumpflag;
#if !defined(GPROF)
	cp = (caddr_t)&etext;
	cp = cp;
#endif
}
#endif notdef BSD

/*VARARGS*/
not_dead_code()
{
}
