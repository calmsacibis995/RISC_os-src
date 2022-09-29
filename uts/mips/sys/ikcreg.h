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
/* $Header: ikcreg.h,v 1.7.4.2 90/05/10 06:22:59 wje Exp $ */

#ifndef	_SYS_IKCREG_
#define	_SYS_IKCREG_	1


/*
 * ikcreg.h
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/ikcreg.h,v $
 * $Revision: 1.7.4.2 $
 * $Date: 90/05/10 06:22:59 $
 */

# define IK_MAXCTLR	2		/* # of max ctlr */

/* max i/o size */
# define IK_IOLIM	(63*1024)	/* max i/o size */

/* XXX Hack for compiler bug */
#ifdef mips	/* was sgi */
#undef btoc
#define	btoc(x)	(((x)+(NBPC-1))>>BPCSHIFT)
#endif sgi

# define IK_NSG		(btoc(IK_IOLIM)+1) /* # of sg entries for IK_IOLIM */


/*
 * per ctlr status information
 */
struct iksoftc {
	int c_flags;			/* flags */
	short c_dtype;			/* printer type */
	struct buf c_tab;		/* i/o queue */
	int c_unit;			/* unit number */
	int c_timo;			/* max timo */

	int c_lock;			/* lock for private variables */
	int c_iostate;			/* i/o state */
	int c_reg;			/* cmd reg */
	int c_val;			/* cmd val */
	struct buf c_buf;		/* raw cmd buf */
	struct sg c_sg[IK_NSG];		/* scatter/gather vec */
	int c_nsg;			/* number of active sg entries */
	int c_cursg;			/* current sg entry */
	int c_timer;			/* ticks left on current cmd */

	ushort *c_base;			/* virtual address of i/o port */
	ushort c_modvec;		/* sw copy of modvec reg */
	ushort c_mode;			/* sw copy of mode reg */
	int c_brl;			/* intr level */
};


# ifdef IK_DRIVER

/* system definitions */
# define VOLATILE	volatile
# define WBFLUSH	wbflush


/*
 * the minor device is decoded as follows:
 *	noreset		7:7
 *	ctlr		3:3
 *	type		0:2
 */
# define DEVCTLR(dev)		((minor(dev)>>3)&01)
# define DEVTYPE(dev)		((minor(dev)>>0)&07)
# define DEVNORESET(dev)	((minor(dev)>>7)&01)

/* minor device types */
# define VERS		0x01		/* versatec */
# define CENTRONICS	0x02		/* centronics */
# define RAW		0x05		/* raw interface */


/* interrupt level */
# define IKINTLEV	5
# define USEPRI		register int s
# define RAISE		s = spl5()
# define LOWER		splx(s)

/* address modifier */
# define PHYSPACE	VME_A24NPAMOD

/* sleeping priority */
# define IKPRI		PUSER		/* killable */


/* register offsets */
# define IK_LR		0x00		/* (w) latched fcn */
# define IK_SR		0x00		/* (r) iface status */
# define IK_PR		0x01		/* (w) pulsed function */
# define IK_DSR		0x01		/* (r) dev status */
# define IK_DOR		0x02		/* (w) data out */
# define IK_DDR		0x02		/* (r) diagnostic data */
# define IK_MODVEC	0x03		/* (w) addr mod + intr vec */
# define IK_DMAHI	0x04		/* (w) dma hi */
# define IK_DMALO	0x05		/* (w) dma lo */
# define IK_BC		0x06		/* (w) dma byte cnt */

/* latch register bits */
# define LR_TENABLE	(1<<7)		/* test mode enable */
# define LR_VTEST	(1<<6)		/* versatec test */
# define LR_IPRIME	(1<<5)		/* input prime */
# define LR_OPTION	(1<<4)		/* select option port */
# define LR_STREAMING	(1<<3)		/* opt:  data streaming mode */
# define LR_IENABLE	(1<<2)		/* interrupt enable */
# define LR_VPPLOT	(1<<1)		/* vers:  print/plot mode */
# define LR_VPLOT	(1<<0)		/* vers:  plot mode */

/* status register bits */
# define SR_READY	(1<<7)		/* interface ready */
# define SR_DRY		(1<<6)		/* device ready */
# define SR_DMA		(1<<5)		/* dma in progress */
# define SR_OPTION	(1<<4)		/* option port selected */
# define SR_STREAMING	(1<<3)		/* opt:  streaming mode */
# define SR_IENABLE	(1<<2)		/* interrupts enabled */
# define SR_INTR	(1<<1)		/* interrupt happened */
# define SR_BERR	(1<<0)		/* bus error */

/* pulse fcn bits */
# define PR_SOFTACK	(1<<7)		/* software ACK */
# define PR_RESET	(1<<6)		/* master reset */
# define PR_IACK	(1<<5)		/* interrupt ACK */
# define PR_GO		(1<<4)		/* dma GO */
# define PR_CLEAR	(1<<3)		/* vers:  clear */
# define PR_FF		(1<<2)		/* vers:  form feed */
# define PR_EOT		(1<<1)		/* vers:  remote EOT */
# define PR_EOL		(1<<0)		/* vers:  remote line terminate */

/* device status bits */
# define DSR_VTTL	(1<<15)		/* vers:  TTL mode selected */
# define DSR_VRDY	(1<<14)		/* vers:  ready */
# define DSR_VPPR	(1<<13)		/* vers:  paper present */
# define DSR_VONLINE	(1<<12)		/* vers:  online */
# define DSR_VPPLOT	(1<<11)		/* vers:  print / plot selected */
# define DSR_VPLOT	(1<<10)		/* vers:  plot selected */
# define DSR_TENABLE	(1<<8)		/* test mode selected */
# define DSR_OPTACK	(1<<7)		/* opt:  ack asserted */
# define DSR_OPTRDY	(1<<6)		/* opt:  ready */
# define DSR_OPTPPR	(1<<5)		/* opt:  paper present */
# define DSR_OPTION_	(1<<4)		/* opt:  not selected */
# define DSR_OPTFAULT	(1<<3)		/* opt:  fault */
# define DSR_OPTLRESET	(1<<2)		/* opt:  long reset asserted */


/* reads and writes to the registers */
# define IK_INREG(ci, a) \
		(((VOLATILE unsigned short *)(ci)->c_base)[a])
# define IK_OUTREG(ci, a, d) \
		(((VOLATILE unsigned short *)(ci)->c_base)[a]=(d))


/* .c_state's */
# define CTLR_IDLE	0
# define CTLR_IO	1
# define CTLR_CMD	2

/* .c_flags bits */
# define CTLR_ACTIVE	(1<<0)		/* is active */
# define CTLR_OPEN	(1<<1)		/* is open */
# define CTLR_HUNG	(1<<2)		/* is hung */
# define CTLR_TICKING	(1<<3)		/* has timer on */

/* .c_lock bits */
# define LOCK_WANTED	(1<<0)
# define LOCK_BUSY	(1<<1)


typedef struct iksoftc CINFO;

# endif IK_DRIVER

#endif	_SYS_IKCREG_
