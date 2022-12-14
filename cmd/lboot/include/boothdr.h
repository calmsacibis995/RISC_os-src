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
/* $Header: boothdr.h,v 1.5.2.2 90/05/09 16:19:38 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define	Offset(ptr,base)	((offset) ((char*)(ptr) - (char*)(base)))
#define	POINTER(offset,base)	((char*)(base) + (offset))

#define	ROUNDUP(p)		(((int)(p)+sizeof(int)-1) & ~(sizeof(int)-1))

typedef	unsigned short		offset;

#define	PARAMNMSZ	8	/* maximun size of a parameter name in /etc/master data base */
#define DONTCARE	0xff	/* lboot assigns external major number */

#ifdef MULTIPLE_MAJOR
#ifndef MAX_MAJOR
#define MAX_MAJOR	8
#endif
#endif MULTIPLE_MAJOR

/*
 * configuration information generated from object's master file
 */

struct	master {
	unsigned short	flag;		/* /etc/master flags */
	unsigned char	vec;		/* first interrupt vector */
	char		prefix[15+1];	/* module prefix, '\0' terminated */
	unsigned char	soft;		/* software module external major # */
	unsigned char	ndev;		/* number of devices/controller */
	short		ndep;		/* number of dependent modules */
	short		nrtn;		/* number of routine names */
	offset		o_depend;	/* ==> additional modules required */
	offset		o_routine;	/* ==> routines to be stubbed if module is not loaded */
#ifdef MULTIPLE_MAJOR
	unsigned char	ncontmaj;  	/* number of controllers per major */
	unsigned char	msoft[MAX_MAJOR];	/* Array of multiple majors */
#endif MULTIPLE_MAJOR
};

/*
 *	FLAG bits
 */
#define	KOBJECT	0x8000	/* (k) this is a kernel object file */
#define FSTYP	0x0400	/* (j) file system type*/
#define	FUNDRV	0x0200	/* (f) framework/stream type device */
#define	FUNMOD	0x0100	/* (m) framework/stream module */
#define	ONCE	0x0080	/* (o) allow only one specification of device */
#define	REQADDR	0x0040	/* (a) xx_addr array must be generated */
#define	TTYS	0x0020	/* (t) cdevsw[].d_ttys == "prefix|_tty" */
#define	REQ	0x0010	/* (r) required device */
#define	BLOCK	0x0008	/* (b) block type device */
#define	CHAR	0x0004	/* (c) character type device */
#define	SOFT	0x0002	/* (s) software device driver */
#define	NOTADRV	0x0001	/* (x) not a driver; a configurable module */

/*
 * Dependencies: if the current module is loaded, then the following
 *               modules must also be loaded
 */
struct	depend {
	offset		name;		/* module name */
};

/*
 * Routines: if the current module is not loaded, then the following
 *           routines must be stubbed off
 */
struct	routine {
	char		id;		/*  routine type */
	offset		name;		/* ==> routine name */
};

/*
 *	Routine types
 */
#define RNOTHING	0	/* void rtn() { } */
#define RNULL		1		/* void rtn() { nulldev(); } */
#define RNOSYS		2		/* rtn() { return(nosys()); } */
#define RNODEV		3		/* rtn() { return(nodev()); } */
#define RTRUE		4		/* rtn() { return(1); } */
#define RFALSE		5		/* rtn() { return(0); } */
#define RFSNULL		6		/* rtn() { return(fsnull()); } */
#define RFSSTRAY	7	/* rtn() { return(fsstray()); } */
#define RNOPKG		8		/* void rtn() { nopkg(); } */
#define RNOREACH	9	/* void rtn() { noreach(); } */
#define RZERO		10

#define	XBUMP(p,what)	(union element *)((int)(p) + ((sizeof((p)->what)==ELENGTH)? \
							1+strlen((p)->what) : \
							sizeof((p)->what)))

