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
/* $Header: edt.h,v 1.18.4.3 90/05/10 06:11:43 wje Exp $ */

#ifndef	_SYS_EDT_
#define	_SYS_EDT_	1



/************************************************************/
/*    Structures for the Equipped Device Table              */
/************************************************************/

struct vme_intrs {
	int	(*v_vintr)();	/* interrupt routine tied to this interrupt */
	unsigned char	v_vec;	/* vme vector */
	unsigned char	v_brl;	/* interrupt priority level */
	unsigned char	v_unit;	/* software identifier */
	unsigned char	v_bus;	/* vme bus id (for RC6280) */
	struct vme_intrs *v_link;/* link in chain of entries */
};

struct atbus_intrs {
	int	(*a_aintr)();	/* interrupt routine tied to this interrupt */
	unsigned char	a_irq;	/* interrupt priority level */
	unsigned char	a_unit;	/* software identifier */
	struct atbus_intrs *a_link;/* link in chain of entries */
};

struct edt {
	char	*e_tag;
	paddr_t	e_base;
	struct	vme_intrs	*e_intr_info;
	struct	atbus_intrs	*e_atbusintr_info;
	int	(*e_init)();	/* device initialization/run-time probe */
};

#ifdef	INKERNEL
extern struct edt edt[];
extern int nedt;
#endif

/* EDT device tags */
#define GROTAG	"gro_"		/* Graphics output device */
#define	XYLDTAG	"xyl712"	/* xylogics 712 disk controller */
#define	CD3608TAG "cd3608"	/* Central data 3608 serial controller */

#endif	_SYS_EDT_
