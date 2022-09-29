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
/* $Header: cpreg.h,v 1.7.1.2 90/05/10 04:43:50 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/* 
 * Integrated Solutions Communication Processor definitions
 */
struct bsd43_(cpdevice) {
	u_short	cp_sel;			/* selection register */
#define	BSD43_SEL_MC		0x8000		/* master clear */
#define	BSD43_SEL_MV		0x4000		/* multi vector */
#define	BSD43_SEL_SHIFT	4
#define	BSD43_SEL_SILO	16 << BSD43_SEL_SHIFT
#define	BSD43_SEL_LP		17 << BSD43_SEL_SHIFT
#define BSD43_SEL_CONF_NLINES	0x001F		/* number of lines supported */
#define BSD43_SEL_CONF_BR	0x0020		/* baud rate table used */
#define BSD43_SEL_CONF_LP	0x0040		/* line printer supported */
#define BSD43_SEL_CONF_LPCEN	0x0080		/* line printer is CENTRONICS */
#define BSD43_SEL_CONF_CLK	0x0100		/* battery clock supported */
#define BSD43_SEL_CONF_QIC2	0x0200		/* qic2 supported */
#define BSD43_SEL_CONF_DRV	0x0400		/* drv11 supported */
	u_short	cp_isr; 		/* interrupt status register */
#define	BSD43_ISR_NI		0x80		/* non-existant memory interrupt */
#define	BSD43_ISR_SI		0x40		/* silo service interrupt */
#define	BSD43_ISR_TI		0x20		/* transmitter service interrupt */
#define	BSD43_ISR_CI		0x10		/* carrier transition interrupt */
#define	BSD43_ISR_RI		0x08		/* ring transition interrupt */
#define	BSD43_ISR_PI		0x04		/* printer service interrupt */
#define	BSD43_ISR_IE		(BSD43_ISR_NI|BSD43_ISR_SI|BSD43_ISR_TI|BSD43_ISR_CI|BSD43_ISR_RI|BSD43_ISR_PI)
	u_short	cp_ler;			/* line enable register */
	u_short	cp_tcr;			/* transmit control register */
	u_short cp_brk;			/* break register */
	u_short	cp_swr;			/* silo window register */
#define	BSD43_SWR_VDP		0x8000		/* valid data present */
#define	BSD43_SWR_FE		0x4000		/* framing error/break */
#define	BSD43_SWR_PE		0x2000		/* parity error */
#define	BSD43_SWR_DO		0x1000		/* data overrun error */
#define	BSD43_SWR_LN_MASK	0x0F00		/* mask for line */
#define BSD43_SWR_LN_SHIFT	8		/* shift for line number */
#define	BSD43_SWR_CH_MASK	0x00FF		/* mask for character */
	u_short	cp_acr;			/* assert carrier register */
	u_short	cp_dcr;			/* detect carrier register */
	u_short	cp_drr;			/* detect ring register */
	u_short	cp_pr;			/* parameter register */
#define	BSD43_PR_BITS5	0x0000
#define	BSD43_PR_BITS6	0x0100
#define	BSD43_PR_BITS7	0x0200
#define	BSD43_PR_BITS8	0x0300
#define	BSD43_PR_TWOSB	0x0400
#define	BSD43_PR_OPAR		0x0800
#define	BSD43_PR_PENABLE	0x1000
#define	BSD43_PR_EPAR		0x0000
#define	BSD43_PR_XOFF		0x4000		/*   stop transmit on recieve of ^S */
#define	BSD43_PR_HDUPLX	0x8000
	u_short	cp_sr;			/* status register */
#define	BSD43_LSR_GO		0x0001		/*   LINE PRINTER STATUS: go bit */
#define	BSD43_LSR_FLUSH	0x0002		/*   flush action go bit */
#define	BSD43_LSR_CABLE	0x0200		/*   cable attached */
#define	BSD43_LSR_PAPE	0x0400		/*   paper empty */
#define	BSD43_LSR_BUSY	0x0800		/*   busy */
#define	BSD43_LSR_SEL		0x1000		/*   selected KLUDGE */
#define	BSD43_LSR_RDY		0x2000		/*   ready, no fault*/
#define	BSD43_LSR_FF		0x4000		/*   fifo full */
#define	BSD43_LSR_FE		0x8000		/*   fifo empty */
#define	BSD43_LSR_BITS	\
	"\20\20FIFOE\17FIFOF\16READY\15SELECT\14BUSY\13PAPER_EMPTY\2FLUSH\1GO"
/*	char	*cp_ba;			/* bus address registers */
	u_short	cp_bah;
	u_short	cp_bal;
	u_short	cp_bc;			/* byte count register */
};

/* minor numbers for line printers */
#define	BSD43_ISLP(dev)	(bsd43_minor(dev)&0x80)
#define	BSD43_CPUNIT(m)	(((m)&0x70)>>4)
#define	BSD43_CPLINE(m)	((m)&0x0F)
#define	BSD43_LPCANON(m)	BSD43_CPLINE(m)
#define	BSD43_LP_CANON_CAP	1		/* printer only supports lower case */
#define	BSD43_LP_CANON_RAW	2		/* do not canonize, just dump chars */
#define BSD43_LP_MAXCOL	132		/* overide with flag in qb_device */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CPLINE BSD43_CPLINE
#   define CPUNIT BSD43_CPUNIT
#   define ISLP BSD43_ISLP
#   define ISR_CI BSD43_ISR_CI
#   define ISR_IE BSD43_ISR_IE
#   define ISR_NI BSD43_ISR_NI
#   define ISR_PI BSD43_ISR_PI
#   define ISR_RI BSD43_ISR_RI
#   define ISR_SI BSD43_ISR_SI
#   define ISR_TI BSD43_ISR_TI
#   define LPCANON BSD43_LPCANON
#   define LP_CANON_CAP BSD43_LP_CANON_CAP
#   define LP_CANON_RAW BSD43_LP_CANON_RAW
#   define LP_MAXCOL BSD43_LP_MAXCOL
#   define LSR_BITS BSD43_LSR_BITS
#   define LSR_BUSY BSD43_LSR_BUSY
#   define LSR_CABLE BSD43_LSR_CABLE
#   define LSR_FE BSD43_LSR_FE
#   define LSR_FF BSD43_LSR_FF
#   define LSR_FLUSH BSD43_LSR_FLUSH
#   define LSR_GO BSD43_LSR_GO
#   define LSR_PAPE BSD43_LSR_PAPE
#   define LSR_RDY BSD43_LSR_RDY
#   define LSR_SEL BSD43_LSR_SEL
#   define PR_BITS5 BSD43_PR_BITS5
#   define PR_BITS6 BSD43_PR_BITS6
#   define PR_BITS7 BSD43_PR_BITS7
#   define PR_BITS8 BSD43_PR_BITS8
#   define PR_EPAR BSD43_PR_EPAR
#   define PR_HDUPLX BSD43_PR_HDUPLX
#   define PR_OPAR BSD43_PR_OPAR
#   define PR_PENABLE BSD43_PR_PENABLE
#   define PR_TWOSB BSD43_PR_TWOSB
#   define PR_XOFF BSD43_PR_XOFF
#   define SEL_CONF_BR BSD43_SEL_CONF_BR
#   define SEL_CONF_CLK BSD43_SEL_CONF_CLK
#   define SEL_CONF_DRV BSD43_SEL_CONF_DRV
#   define SEL_CONF_LP BSD43_SEL_CONF_LP
#   define SEL_CONF_LPCEN BSD43_SEL_CONF_LPCEN
#   define SEL_CONF_NLINES BSD43_SEL_CONF_NLINES
#   define SEL_CONF_QIC2 BSD43_SEL_CONF_QIC2
#   define SEL_LP BSD43_SEL_LP
#   define SEL_MC BSD43_SEL_MC
#   define SEL_MV BSD43_SEL_MV
#   define SEL_SHIFT BSD43_SEL_SHIFT
#   define SEL_SILO BSD43_SEL_SILO
#   define SWR_CH_MASK BSD43_SWR_CH_MASK
#   define SWR_DO BSD43_SWR_DO
#   define SWR_FE BSD43_SWR_FE
#   define SWR_LN_MASK BSD43_SWR_LN_MASK
#   define SWR_LN_SHIFT BSD43_SWR_LN_SHIFT
#   define SWR_PE BSD43_SWR_PE
#   define SWR_VDP BSD43_SWR_VDP
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


