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
/* $Header: shareg.h,v 1.6.2.2.1.1.1.2 90/10/23 13:48:11 beacker Exp $ */
/*
 * $Header: shareg.h,v 1.6.2.2.1.1.1.2 90/10/23 13:48:11 beacker Exp $
 */
/*
 *         SCSI HOST ADAPTOR defines
 */

#define MAXPHYS		0x10000

#ifndef MAX_SGENTRY
#define MAX_SGENTRY	17
#endif MAX_SGENTRY

struct	shadevice {				/* HOST ADAPTOR COMMAND BLOCK */
	u_char		sha_cmd;		/* controller command reg */
	u_char		sha_cerr;		/* controller error code */
	u_char		sha_scsi_status;	/* scsi completion status */
	u_char		sha_dcr;		/* device control register */
	u_short		sha_timeout;		/* command timeout x10ms */
						/* debug level for SHA_DBGON */
						/* returned version for SHA_VERSION */
	u_short		sha_pgofset;		/* data start addr page offset*/
	u_long		sha_dlen;		/* amount of data */
	u_long sha_pfn[MAX_SGENTRY];	 	/* physical pages for DMA */
	union scsi_cdb	sha_cdb;		/* command descriptor block */
};

/* sha_cmd defines */
#define		SHA_INIT	0x0001	/*   start initialize sequence */
#define		SHA_SCSI	0x0002	/*   tell controller to look at cdb */
#define		SHA_DBGON	0x0003	/*   tell controller to turn debugging on */
#define		SHA_DBGOFF	0x0004	/*   tell controller to turn debugging off */
#define		SHA_VERSION	0x0005	/*   get version of controller firmware */
#define		SHA_RESET	0x0006	/*   reset SCSI bus */

/* sha_cerr defines */
#define		SHA_CERR_OK	0		/*   no controller errors */
#define		SHA_CERR_CMDERR	2		/*   host sent a bad command */
#define		SHA_CERR_SELERR	3		/*   error selecting target */
#define		SHA_CERR_IERR	4		/*   wrong interrupt occured */
#define		SHA_CERR_TMOERR	5		/*   host adaptor timeout */
#define		SHA_CERR_PCERR	6		/*   SPC in wrong phase */
#define		SHA_CERR_PARITY	8		/*   SPC parity error */
#define		SHA_CERR_RECERR	9		/*   unexpected reconnect */
#define		SHA_CERR_MAX (SHA_CERR_RECERR + 1)

/* sha_dcr defines */
#define		SHA_NOIE	0		/*   disable interrupts */
#define		SHA_IE		0x01		/*   enable interrupts */
#define		SHA_DIR_IN	0		/*   data direction to host */
#define		SHA_DIR_OUT	0x02		/*   data direction to target */
#define		SHA_DISC	0		/*   disconnect/reconnect */
#define		SHA_NODISC	0x04		/*   no disconnect/reconnect */
#define		SHA_SYNC	0x08		/*   (returned) targ is sync */
#define		SHA_SYNC_MODE	0x10		/*   allow sync mode */

/* sha_flags defines */

#define	SHA_NOWAIT	0x01
#define	SHA_SETDISC	0x02

/* __EOF__ */
