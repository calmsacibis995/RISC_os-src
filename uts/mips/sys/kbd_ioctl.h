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
/* $Header: kbd_ioctl.h,v 1.2.2.3 90/05/10 06:25:58 wje Exp $ */

#ifndef	_SYS_KBD_IOCTL_
#define	_SYS_KBD_IOCTL_	1


/*
 * Kbd ioctls
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/kbd_ioctl.h,v $
 * $Revision: 1.2.2.3 $
 * $Date: 90/05/10 06:25:58 $
 */
struct colorm {
	unsigned short cmstart;		/* starting register */
	unsigned short cmcount;		/* number of registers */
	unsigned char cmap[3*256];	/* register data */
};

#define	KTCSETTIMESTAMP		_IO(K,0x7f)	/* kbd input timestamped */
#define	KTCCLRTIMESTAMP		_IO(K,0x7e)	/* no kbd input timestamped */
#define	KTCPRGMBUZZER		_IOW(K,0x7d,struct buzzer)	
						/* program the buzzer */
#define	KTCRINGBELL		_IO(K,0x7c)	/* ring standard bell (^g) */
#define	KTCSETLIGHTS		_IOW(K,0x7b,int)	
						/* set keyboard lights */
#define	KTCGETLIGHTS		_IOR(K,0x7a,int)	
						/* get kbd lights last set */
#define KTCWRTCOLOR		_IOW(K,0x79,struct colorm)
						/* write the color map */
#define	KTMBLANK		_IOW(K,0x78,int)
						/* blank/unblank mono screen */
#define KLNUMLOCK	0x2		/* light num lock light */
#define KLCAPSLOCK	0x4		/* light caps lock light */
#define KLSCRLLOCK	0x1		/* light scroll lock light */

/*
 *  Kbd Input Timestamping - when turned on all recieved characters
 *  that would normally be received as a single character are received
 *  eight characters in the format of the following structure.
 */

#define KBDSYNCCHAR 0xff
struct ktimestamp {
	unsigned char kt_sync[3];	/* 3 sync characters == 0xff */
	unsigned char kt_char;		/* actual timestamped character */
	time_t kt_time;			/* time in HZ since last boot */
};

#endif	_SYS_KBD_IOCTL_
