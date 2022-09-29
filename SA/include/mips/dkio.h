/* $Header: dkio.h,v 1.3 90/01/23 14:11:31 huang Exp $ */
/* $Copyright$ */

#ifndef	_SYS_DKIO_
#define	_SYS_DKIO_	1


/*	dkio.h	6.1	83/07/29	*/
/*
 * Structures and definitions for disk io control commands
 */


/* disk io control commands */
#define _DIOC_(x)	(('d'<<8) | x)
#define DIOCFMTTRK	_DIOC_(1)	/* Format and Map */
#define DIOCVFYSEC	_DIOC_(2)	/* Verify sectors */
#define DIOCGETCTLR	_DIOC_(3)	/* Get ctlr info */
#define DIOCDIAG	_DIOC_(4)	/* Perform diag */
#define DIOCSETDP	_DIOC_(5)	/* Set devparams */
#define DIOCGETVH	_DIOC_(6)	/* Get volume header */

/* BSD compatibility for now */
#define DIOCFMTMAP	_DIOC_(1)	/* Format and Map */
#define DIOCNOECC	_DIOC_(20)	/* Disable/Enable ECC */
#define DIOCRDEFECTS	_DIOC_(21)	/* Read meadia defects */
#define DIOCINITVH	_DIOC_(22)	/* Initialize volume header */

/* Reserved for SGI. */
/* #define DIOCRESERVED	_DIOC_(7)	/* SGI - SETVH */
/* #define DIOCRESERVED	_DIOC_(8)	/* SGI - DRIVETYPE */
/* #define DIOCRESERVED	_DIOC_(9)	/* SGI - TEST  */
/* #define DIOCRESERVED	_DIOC_(10)	/* SGI - FORMAT */
/* #define DIOCRESERVED	_DIOC_(11)	/* SGI - SENSE */
/* #define DIOCRESERVED	_DIOC_(12)	/* SGI - SELECT */
/* #define DIOCRESERVED	_DIOC_(13)	/* SGI - READCAPACITY */
/* #define DIOCRESERVED	_DIOC_(14)	/* SGI - RDEFECTS */
/* #define DIOCRESERVED	_DIOC_(15)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(16)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(17)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(18)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(19)	/* SGI - unused */

/*
 * Added from System 5.3:
 *	Ioctls to disk drivers will pass the address to this
 *	structure which the driver handle as appropriate.
 *
 */
struct io_arg {
	int retval;
	unsigned long sectst;
	unsigned long memaddr;
	unsigned long datasz;
};

/*
 * driver ioctl() commands not supported
 */
#define VIOC			('V'<<8)
#define V_PREAD			(VIOC|1)	/* Physical Read */
#define V_PWRITE		(VIOC|2)	/* Physical Write */
#define V_PDREAD		(VIOC|3)	/* Read of Physical 
						 * Description Area */
#define V_PDWRITE		(VIOC|4)	/* Write of Physical 
						 * Description Area */
#define V_GETSSZ		(VIOC|5)	/* Get the sector size of media */

#endif	_SYS_DKIO_
