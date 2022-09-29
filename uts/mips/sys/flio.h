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
/* $Header: flio.h,v 1.3.2.3.1.1.1.3 91/01/03 14:45:30 twong Exp $ */

/*	flio.h	6.1	83/07/29	*/
/*
 * Structures and definitions for floppy disk io control commands
 */


/* floppy disk io control commands */
#define _FLIOC_(x)	(('d'<<8) | x)
#define FLIOCFORMAT	_FLIOC_(1)	/* Format floppy disk */
#define FLIOCMODSLCT	_FLIOC_(2)	/* Set disk parameters */
#define FLIOCMODSNS	_FLIOC_(3)	/* Get disk parameters */
#define FLIOC_LOW_LEVEL	_FLIOC_(4)	/* Get disk parameters */
#define FLIOC_SET_FIFO_DEPTH 	_FLIOC_(5)	/* set fifo depth */
#define FLIOC_SET_XFER_RATE	_FLIOC_(6)	/* set fifo depth */
#define FLIOC_SET_NRETRIES	_FLIOC_(7)
#define FLIOC_SENSE_DRIVE	_FLIOC_(8)

/*
 * Added from System 5.3:
 *	Ioctls to floppy disk drivers will pass the address to this
 *	structure which the driver handle as appropriate.
 *
 */
struct io_arg {
	int retval;
	unsigned long sectst;
	unsigned long memaddr;
	unsigned long datasz;
};

struct fl_io {
	unchar cmd[10];
	int clen;
	unchar *data;
	int dlen;
	unchar result[10];
	int rlen;
};
