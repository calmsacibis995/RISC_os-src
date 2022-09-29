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
#ident	"$Header: dkin.c,v 1.4.1.2 90/05/10 05:14:47 wje Exp $"
/*
** INTROL Model 300 - SCSI controller driver
**
** TODO: what about keeping drive specific info like req sense count,
**       modesel count, etc in the volumn header? note 508 bytes used!!
** TODO: implement residual handling for space ioctl's
** TODO: Implement PRIORITY OVERRIDE function in the time-out function?
** TODO: devices with multiple LUN's are not supported, ie non-embedded
**	 scsi.
** TODO: handle large drives with lba > 21 bits (2 meg)
** TODO: handle requests that are not multiples of 512
*/ 

#undef DEBUGTAPE	/* enable tape specific debug printf's */
#undef SGPRINTS
#undef DEBUGSG
#undef DEBUGPOLL	/* enable polled debug print's */
#undef NOARB

#define SCAT_GATH	/* enable scatter/gather */
#define DEBUGRPH
#define FAST		/* don't spin if board busy (disk only) */
#define INTROL_DEBUG	/* Introl board debug */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#define		b_cylin	b_resid
#define		b_cmd	b_resid
#include "sys/vmevar.h"
#include "sys/dvh.h"
#include "sys/dkinreg.h"
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/ioctl.h"
#include "sys/dkio.h"
#include "sys/edt.h"
#include "sys/dump.h"

#define	TRUE	(1==1)
#define	FALSE	(1==0)

#ifndef	MIN
#define	MIN(a, b)	(((a) < (b)) ? (a) : (b))
#endif

#include "sys/mtio.h"		/* needed for "mt" command ioctl stuff */

/*
 * Time constants.  These were measured for a 450 foot tape, then fudged
 * up, then normalized for a 650 foot tape.  Thus, they should be plenty
 * long enough.  The units are seconds.
 */
#define	TIME_FSF	(60 * 17)		/* no more than 17 minutes */
#define	TIME_REWIND	(60 * 2)		/* no more than 2 minutes */
#define	TIME_RDWR	(60 * 2)		/* no more than 2 minutes */
#define	TIME_RESET	(TIME_FSF + TIME_REWIND)
#define	TIME_WAIT	(60 * 3)

#define	MAXLEN	(32*512)
char tbuffer[NDKINC][NDKIN][MAXLEN];

static char *dkin_name = "Introl VME 300";
extern ovlapseeks;

#define MAX_ERRBLOCKS	85
#define MAXTRK		1
#define WAITLOOPS	100000
#define LOOPS		2000000
#define SPINUP		2	/* ~seconds (and retries) for drive ready */ 
#define NRETRIES	3
#define NUM_IOPBS	8	/* reduced from 16 for scat/gath */
#define SG		1	/* use scatter/gather */
#define NOSG		0	/* don't use scatter/gather */
#define PRI		1	/* this is a priority request */
#define NOWAIT		0	/* don't wait for io to complete */
#define WAIT		1	/* wait for io to complete */

u_char taskidnum[NDKIN];	/* sequential task id's assigned */
struct sense_key {
	unsigned char	key;
	char *key_def;
} sense_keys[] = {
	{ REC_ERR, "command completed with recovery actions" },
	{ NOT_RDY, "drive can't be accessed" },
	{ MEDIUM_ERR, "Non-recoverable data error" },
	{ HW_ERR, "non-recoverable hardware failure (parity, etc)" },
	{ ILL_REQ, "command descripter block contains illegal parameter" },
	{ UNIT_ATN, "media change or drive was reset" },
	{ DATA_PROT, "cartridge is write-protected or not a 600 ft tape (QIC-120)" },
	{ BLANK_CHK, "no-data condition encountered on tape" },
	{ CMD_ABORT, "drive aborted the command" },
	{ VOL_OVFLOW, "physical EOM reached with data still in buffer" },
	{ 0, "status code not in table" }
};
struct int_errors {
	unsigned char	inerr_type;
	char *inerr_name;
} interrs[] = {
	/* SCSI Status */
	{ CHECK_CONDITION, "Check Condition" },
	{ SCSI_MET, "Condition Met" },
	{ SCSI_INTER, "Intermediate" },
	{ SCSI_COMBO, "Intermediate/Condition Met/Good" },
	{ SCSI_RESV, "Reservation Conflict" },
	/* Adaptor Status */
	{ AS_SELTMO, "selection timeout error" },
	{ AS_PHASERR, "scsi phase error" },
	{ AS_PBERROR, "I/O Parameter Block Negated" },
	{ AS_PARITY, "scsi parity error unrecoverable " },
	{ AS_HWERROR, "hardware failure" },
	{ AS_SCSITMO, "scsi timeout error" },
	{ AS_VMEBUSERR, "vme bus error" },
	{ 0xAA, "timed out waiting for adaptor" },
	{ 0, "status code not in table" }
};
char *
printkey(type)
	register unsigned char type;
{
	register struct sense_key *keys = sense_keys;
	register hold;

	while (hold = keys->key) {
		if (type == hold)
			return (keys->key_def);
		keys++;
	}
	return (keys->key_def);
}
char *
printerr(type)
	register unsigned char type;
{
	register struct int_errors *in = interrs;
	register hold;

	while (hold = in->inerr_type) {
		if (type == hold)
			return (in->inerr_name);
		in++;
	}
	return (in->inerr_name);
}
/* hard disk error codes */
#define	NO_SENSE_CODE		0x0
#define	NO_INDEX_SECTOR		0x1
#define NO_SEEK_CMP		0x2
#define DRV_FAULT		0x3
#define DRV_NOT_RDY		0x4
#define DRV_NOT_SEL		0x5
#define LUN_COM_FAIL		0x8
#define ID_FLD_RD_ERR		0x10
#define DATA_FLD_RD_ERR		0x11
#define ID_FLD_NO_SYNC		0x12
#define DATA_FLD_NO_SYNC	0x13
#define BAD_BLK_FND		0x14
#define SEEK_ERR		0x15
#define REC_READ_NO_ECC		0x17
#define REC_READ_WITH_ECC	0x18
#define DEF_LST_ERR		0x19
#define SYNC_XFER_ERR		0x1b
#define NO_P_DEF_LST		0x1c
#define CMP_ERR			0x1d
#define REC_ID_WITH_ECC		0x1e
#define INV_CMD_OP_CODE		0x20
#define ILL_LBA			0x21
#define ILL_FLD_CDB		0x24
#define INVALID_LUN		0x25
#define INV_FLD_PARAM_LST	0x26
#define WRIT_PROT		0x27
#define POW_RST			0x29
#define MODE_SEL_PARAMS		0x2a
#define FORMAT_FAIL		0x31
#define NO_DEF_SPARE		0x32
#define RAM_FAILURE		0x40
#define DATA_PATH_FAIL		0x41
#define PON_FAIL		0x42
#define MSG_REJECT		0x43
#define INT_CTLR_ERR		0x44
#define SEL_RESEL_FAILED	0x45
#define SCSI_PAR_ERROR		0x47
#define INIT_DET_ERROR		0x48
#define INAP_ILL_MSG		0x49
struct err_code {
	unsigned char	key;
	char *key_def;
} err_codes[] = {
	{ NO_INDEX_SECTOR, "no index/sector signal" },
	{ NO_SEEK_CMP, "no seek complete" },
	{ DRV_FAULT, "drive fault" },
	{ DRV_NOT_RDY, "drive not ready" },
	{ DRV_NOT_SEL, "drive not selected" },
	{ LUN_COM_FAIL, "logical unit communication failure" },
	{ ID_FLD_RD_ERR, "unrecovered read error of ID field" },
	{ DATA_FLD_RD_ERR, "unrecovered read error of DATA field" },
	{ ID_FLD_NO_SYNC, "no sync byte found in ID field" },
	{ DATA_FLD_NO_SYNC, "no sync byte found in DATA field" },
	{ BAD_BLK_FND, "no record found or bad block found" },
	{ SEEK_ERR, "seek error" },
	{ REC_READ_NO_ECC, "recovered read data with re-reads (no ecc)" },
	{ REC_READ_WITH_ECC, "recovered read data with ecc correction" },
	{ DEF_LST_ERR, "defect list error" },
	{ SYNC_XFER_ERR, "syncronous transfer error" },
	{ NO_P_DEF_LST, "primary defect list not found" },
	{ CMP_ERR, "compare error" },
	{ REC_ID_WITH_ECC, "recovered ID with ecc correction" },
	{ INV_CMD_OP_CODE, "invalid command operation code" },
	{ ILL_LBA, "illegal logical block address" },
	{ ILL_FLD_CDB, "illegal field in CDB" },
	{ INVALID_LUN, "invalid LUN" },
	{ INV_FLD_PARAM_LST, "invalid field in parameter list" },
	{ WRIT_PROT, "write protected" },
	{ POW_RST, "power on, reset condition, or bus device reset occurred" },
	{ MODE_SEL_PARAMS, "mode select parameters changed" },
	{ FORMAT_FAIL, "format failed" },
	{ NO_DEF_SPARE, "no defect spare location available" },
	{ RAM_FAILURE, "ram failure" },
	{ DATA_PATH_FAIL, "data path diagnostic failure" },
	{ PON_FAIL, "power on diagnostic failure" },
	{ MSG_REJECT, "message reject error" },
	{ INT_CTLR_ERR, "internal controller error" },
	{ SEL_RESEL_FAILED, "select/reselect failed" },
	{ SCSI_PAR_ERROR, "scsi parity error" },
	{ INIT_DET_ERROR, "initiator detected error" },
	{ INAP_ILL_MSG, "inappropriate/illegal message" },
	{ 0, "error code not in table" }
};
char *
print_errcode(type)
	register unsigned char type;
{
	register struct err_code *errs = err_codes;
	register hold;

	while (hold = errs->key) {
		if (type == hold)
			return (errs->key_def);
		errs++;
	}
	return (errs->key_def);
}
extern	int timeout(/*int (*func)(), caddr_t, int */);

void	dkinedtinit(), dkinattach(), dkin_timeout(),
	dkinintr(), dkinstart(), tapeioctl();

int	dkinslave();
char	*dkinvdread();

struct	int_ctlr {
	char   *c_io;		/* ptr to Introl I/O space */
	u_char	c_ipl;		/* normal and error interrupt priority level */
	u_char	c_vec;		/* normal and error interrupt vector */
	u_int	c_flags;	/* controller Flags */
	u_char	c_nunits;	/* number of active units on controller */
	u_char	c_spurintr;	/* number of spurious interrupts */
	u_int	c_timeouts;	/* number of command timeouts */
	char	c_waiting;	/* flag to indicate devices are waiting */
	struct int_unit {
		short pad;	/* required for correct allignment */
		u_short	un_sgentries; /* # of scatter/gather entries */
		struct insge    un_insge[MAX_SGENTRY]; /* scat/gath entries */
		u_int		un_command;
		daddr_t		un_bn;
		daddr_t		un_prev_bn;
		long		un_prev_b_resid;
		u_char		un_vhvalid;
		u_char		un_nretries;
		u_char		un_noecc;
		u_int		un_lastcyl;
		struct volume_header un_vh;	/* MIPS volumne header */
		u_char	 	un_sense[16];	/* Last Request Sense bytes */
		u_int	 	un_flags;	/* device Flags */
		struct inquiry	un_inq;		/* From Inquiry Command */
		struct err_page	un_err;		/* Mode select error page */
		struct int_iopb un_iopb;  	/* IOPB */
		struct int_iopb*un_iopbp; 	/* IOPB pointer */
		u_int  		un_prev_secs;  	/* sectors xfer'ed last */
		struct iotime 	un_iotime;	/* io statistics */
		int		un_unit;	/* this unit number */
		int		un_ctlr;	/* controller attached to */
		int		un_timeid;	/* timeout id */
	/* static buffer info */
		unchar	un_bufinuse;	/* non-zero if uibuf is in use */
		ushort	un_bufdata;	/* count of bytes in ui_buf */
		caddr_t	un_bufva;	/* kernel vaddr to copy ui_buf */
		long	un_buf[NBPSCTR/sizeof(long)];	/* static buffer */
	} c_un[NDKIN];
} dkin_softc[NDKINC];

struct iobuf  dkintab[NDKINC][NDKIN];	/* for linking per unit activity */
struct buf   rdkinbuf[NDKINC][NDKIN];	/* buffers for raw IO */
int vhbuf[NBPSCTR/sizeof(int)];	/* volume header buffer */
	
#define FS(dev)		(dev & 0xf)
#define UNIT(dev)	(((dev) >> 4) & 7) /* 7 units possible/brd */
#define CTLR(dev)	(((dev) >> 7) & 1) /* only 2 boards possible! */

/* how we talk to the Introl board
 */
#define INT_STATUS(ctlr) *((unsigned volatile char*)(dkin_softc[ctlr].c_io+0x201))
#define INT_START0(ctlr,x)	*((unsigned short*)dkin_softc[ctlr].c_io) = x
#define INT_START1(ctlr,x)	*((unsigned short*)dkin_softc[ctlr].c_io+1) = x
#define TASKID(ctlr)		*((unsigned char*)(dkin_softc[ctlr].c_io+0x07))
/*
** perform the probe for the controller on the VME bus
*/
void
dkinedtinit(e)
	struct edt *e;
{
	struct int_ctlr *introl;
	struct int_iopb *ip;
	register ctlr;
	int i;

	ctlr = e->e_intr_info->v_unit;
	introl = &dkin_softc[ctlr];
	if (badaddr(e->e_base, sizeof(e->e_base))) {
		printf("ins%d: controller not available\n",
					 e->e_intr_info->v_unit);
		return;
	}
	/*
	 * allocate a vector for normal/error interrupts.
	 */
	introl->c_vec = e->e_intr_info->v_vec;
	introl->c_ipl = e->e_intr_info->v_brl;

	dkin_softc[ctlr].c_flags |= INT_ALIVE;

	/* establish pointers
	 */
	dkin_softc[ctlr].c_io = (char *)e->e_base;
	for (i=0; i < NDKIN; i++) {
	       ip=(struct int_iopb*)K0_TO_K1(&dkin_softc[ctlr].c_un[i].un_iopb);
	       dkin_softc[ctlr].c_un[i].un_iopbp = ip;
	}

	/* Now attach drives to this controller.
	 * Initialize the buffer cache and the dynamic memory facilities now
	 */
	binit();
	for (i = 0; i < NDKIN; i++) {
		if (dkinslave(i, ctlr))
			dkinattach(i, ctlr);
	}
}
/*
 * check for slave's existence
 */
int
dkinslave(unit, ctlr)
{
	register struct int_unit *un;
	register struct int_iopb *ip;
	u_int status, addr, i, count = SPINUP;
	int tun = 0, inq = 0;

	if ((unit >= NDKIN) || (ctlr >= NDKINC))
		return 0;
	un = &dkin_softc[ctlr].c_un[unit];
	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	if ((INT_STATUS(ctlr) & HOST_A_RDY) == 0) { /* controller dead? */
		printf("in%d: Adapter Not Ready!\n",unit);
		goto failed;
	}
again:
	addr = K0_TO_PHYS(&un->un_inq);
	sameiopb(ctlr, unit, 0, 0);
	setupiopb(C0_INQUIRY,ctlr,unit,addr,un,INT_POLLED,
					0,0,sizeof(struct inquiry));
	if (status = int_startit(un, INT_POLLED, ctlr, unit, NOWAIT)) {
		/* some drives don't return inquiry data if they're
		 * spinning up (as they should) */
		if (status == AS_SELTMO) { /* Adaptor selection timeout */
			goto failed;
		} else if (status == CHECK_CONDITION) {
			inq++; /* set flag to try again (only once more) */
			goto reqsense;
		}
		goto failed;
	}
	printf("in%d: vendor id is '",unit);
	un->un_inq.inq_vendor[7] = 0;
	printf("%s", un->un_inq.inq_vendor);
	printf("' product id is '");
	un->un_inq.inq_product[15] = 0;
	printf("%s", un->un_inq.inq_product);
	printf("'\nand the revision level is '");
	un->un_inq.inq_rev[4] = 0;
	printf("%s", un->un_inq.inq_rev);
	printf("'; ");
	switch (un->un_inq.inq_type) {
		case TYPE_DISK:
			un->un_flags |= INT_DISK;
			if (un->un_inq.inq_qual & REMOVE_MEDIA) { 
				un->un_flags |= INT_RMV_MEDIA;
				printf("device is a floppy disk drive\n");
				printf("or a removable media hard disk\n");
			} else
				printf("device is a hard disk drive\n");
			break;
		case TYPE_TAPE:
			un->un_flags |= INT_TAPE;
			printf("device is a tape drive\n");
			break;
		case TYPE_WORM:
			un->un_flags |= INT_WORM;
			printf("device is a worm drive\n");
			break;
		case TYPE_RONLY_DISK:
			un->un_flags |= (INT_WORM|INT_READONLY);
			printf("device is a worm drive (read-only)\n");
			break;
		case TYPE_PRINTER:
			un->un_flags |= INT_PRINTER;
			printf("device is a printer\n");
		case TYPE_CPU:
		case TYPE_LUN_GONE:
		default:
			printf("device not supported\n");
			goto failed;
	}
retryit:
	setupiopb(C0_TESTRDY,ctlr,unit,0,un,INT_POLLED,0,0,0);
	status = int_startit(un, INT_POLLED, ctlr, unit, NOWAIT);
	if (status == CHECK_CONDITION) {
reqsense:
		addr = K0_TO_PHYS(&un->un_sense[0]);
		sameiopb(ctlr,unit,0,1); /* NO scat/gath and PRIORITY */
		setupiopb(C0_REQSENSE,ctlr,unit,addr,un,INT_POLLED,0,0,16);
		for (i=0; i < 16; i++)
			un->un_sense[i] = 0; /* flush out */
		if (int_startit(un, INT_POLLED, ctlr, unit, NOWAIT)) {
			printf("in%d: request sense ERROR\n", unit);
			goto failed;	/* no need to retry */
		} else if (un->un_sense[2] & UNIT_ATN) {
			if (inq == 1) goto again; /* give it one more try */
			else if (inq == 2) goto failed;
			if (!tun) {
				tun++;
				goto retryit;
			/* needed for no cartridge condition */
			} else if (un->un_flags & INT_TAPE)
				 goto success;
		} else if (un->un_sense[2] & NOT_RDY) {
#ifdef DEBUGRPH
			printf("\ndrive not ready");
#endif DEBUGRPH
		} else {
			printf("in%d SENSE DATA: ",unit);
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
			if (i = (un->un_sense[2] & KEYMASK))
	     			printf("SENSE key %d; %s\n",i,printkey(i));
			if (i = un->un_sense[12])
			    	printf("error code 0x%x; %s\n",
						i,print_errcode(i));
		}
		if (inq == 1) goto again; /* give it one more try */
		goto failed;
	} else if (status)
		goto failed;
#ifdef notdef
	/* if we're a disk drive set up error handling page */
	if ((un->un_flags & INT_DISK)&&(!(un->un_flags & INT_RMV_MEDIA))) {
		un->un_err.err_pagecode = 1;
		un->un_err.err_pagelength = 6;
		un->un_err.err_flags = PER;
		un->un_err.err_reserved0 = 0x8; /* retry count */
		addr = K0_TO_PHYS(&un->un_err);
		setupiopb(C0_MODESEL,ctlr,unit,addr,un,INT_POLLED,
						0,0,sizeof(struct err_page));
		status = int_startit(un,INT_POLLED,ctlr,unit,NOWAIT);
		if (status == CHECK_CONDITION) {
			addr = K0_TO_PHYS(&un->un_sense[0]);
			sameiopb(ctlr,unit,0,1); /* NO scat/gath and PRIORITY */
			setupiopb(C0_REQSENSE,ctlr,unit,addr,un,
						INT_POLLED,0,0,16);
		   	for (i=0; i < 16; i++)
				un->un_sense[i] = 0; /* flush out */
			if (int_startit(un,INT_POLLED,ctlr,unit,NOWAIT)) {
				goto failed;
			} else {
	      			printf("mode select ERROR\n");
				printf("SENSE DATA: ");
				for (i=0; i < 16; i++)
					printf("%x ",un->un_sense[i]);
				printf("\n");
				if (i = (un->un_sense[2] & KEYMASK))
	     		    	     printf("SENSE key %d; %s\n",i,printkey(i));
				if (i = un->un_sense[12])
			       	    	printf("error code %d; %s\n",
							i,print_errcode(i));
			}
		}
	}
#endif notdef
success:
	un->un_flags |= INT_ALIVE;
	dkin_softc[ctlr].c_nunits++;
	un->un_ctlr = ctlr;
	un->un_unit = unit;
	return 1; /* found a device */
failed:
	return 0;
}
/*
 * attach slave device
 */
void
dkinattach(unit, ctlr)
{
	register struct volume_header *inv;
	register struct device_parameters *dp;
	register trk, i, addr;
	register struct int_iopb *ip;
	struct int_unit *un;
	int lbn;

	if ((unit >= NDKIN) || (ctlr >= NDKINC))
		return;
	un = &dkin_softc[ctlr].c_un[unit];
	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	if (!(un->un_flags & INT_DISK)) { /* if this unit isn't a disk */
		un->un_flags |= INT_READY;
		goto valid;
	}
	inv = &un->un_vh;
	/*
	 *
	 * Try reading sector 0 of some number of tracks on cylinder 0
	 *
	 * TODO: How do I know how many logical blocks to increment
	 * for sector 0 of following tracks for an arbitrary SCSI drive???
	 * The CDC Wren III (155Meg) drive has 35 sectors/track
	 * MAXTRK will be set to 1 for now
	for (trk=0,lbn=0; trk < MAXTRK; trk++,lbn += ???) {
	 */
	for (trk=0,lbn=0; trk < MAXTRK; trk++) {
		((struct volume_header *)vhbuf)->vh_magic = 0;
		/*
		 * read sector 0 for a number of tracks of cylinder 0
		 * looking for a valid volume header
		 */
		addr = K0_TO_PHYS(vhbuf);
		sameiopb(ctlr, unit, 0, 0);
		setupiopb(C0_READ,ctlr,unit,addr,un,INT_POLLED,1,0,0);
		if (int_startit(un, INT_POLLED, ctlr, unit, NOWAIT)) {
			continue;	/* error so try again */
		}
		un->un_flags |= INT_READY; /* read OK */

		XPRINTF(XPR_CACHE, "dkin cleaning cache 0x%x %d", 
			vhbuf, NBPSCTR, 0, 0);
		clean_cache(vhbuf, NBPSCTR);
		if (is_vh((struct volume_header *)vhbuf)) {
			goto gotvh;
		}
	}
	if (trk == MAXTRK) {
		printf("dkin(%d,%d): NO volume header found\n",ctlr,unit); 
		return;
	}
gotvh:
	/* Save volume header information in active "per unit" struct.
	 */
	*inv = *(struct volume_header *)vhbuf;

	/* mark volume header as valid
	 */
valid:
	un->un_vhvalid = 1;
	return;
}
/*
 * interrupt handler
 */
void
dkinintr(ctlr)
int ctlr;
{
	register struct int_iopb *ip;
	register struct   buf *bp;
	register struct iobuf *dp;
	struct int_ctlr *ci;
	struct int_unit *un;
	int unit, i, status, taskid, key;
	u_int count, addr, s;
	int is_a_tape = 0;

	taskid = TASKID(ctlr);	/* Read the completion code & Task ID */
	unit = (taskid & 0x70)>>4;
	if (unit >= NDKIN) {
		printf("dkin: spurious interrupt; taskid= 0x%x\n",taskid);
		return;
	}
	ASSERT((taskid & 0xf) == taskidnum[unit]);
	/*
	 * clear safety timeout and initialize some variables
	 */
	un = &dkin_softc[ctlr].c_un[unit];
	s = splclock();
	if (un->un_timeid) {
		untimeout(un->un_timeid);
		un->un_timeid = 0;
	}
	splx(s);
	ci = &dkin_softc[ctlr];
	dp = &dkintab[ctlr][unit];
	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	if (un->un_flags & INT_TAPE) {
		is_a_tape = 1;
		bp = &rdkinbuf[ctlr][unit];	/* Tape buf pointer */
	} else {
		bp = dp->b_actf; 	/* grab active buffer */
	}
	/* 
	 * see if an error (or Check Condition) occurred on the last command
	 */
	if ((taskid & INTR_OK) == 0) {	/* FAILURE or CHECK Condition */
		if (ip->in_adstatus) {
			printf("in%d: ERROR on Task %x\n",unit,taskid&0x7f);
			printf("Adapter ERROR status: %s\n",
				(char *)printerr(ip->in_adstatus));
		}
		status = ip->in_scstatus;
		if (status == CHECK_CONDITION) {
#ifdef DEBUGRPH
			/* let's see what the fault'ed iopb looked like */
			dump_iopb(ctlr,unit);
#endif DEBUGRPH
			/* issue polled request sense to grab status */
			addr = K0_TO_PHYS(&un->un_sense[0]);
			sameiopb(ctlr,unit,0,1); /* NO scat/gath and PRIORITY */
			setupiopb(C0_REQSENSE,ctlr,unit,addr,un,INT_POLLED,
								0,0,16);
		   	for (i=0; i < 16; i++)
				un->un_sense[i] = 0; /* flush out */
			if (int_startit(un, INT_POLLED, ctlr, unit, NOWAIT)) {
				if (is_a_tape) {
					bp->b_flags |= B_ERROR;
					goto tapedone;
				} else goto error;	/* no need to retry */
			} else {
#ifdef DEBUGRPH
				printf("in%d SENSE DATA: ",unit);
				for (i=0; i < 16; i++)
					printf("%x ",un->un_sense[i]);
				printf("\n");
#endif DEBUGRPH
				key = un->un_sense[2] & KEYMASK;
				if (!((key == NOT_RDY) || (key == UNIT_ATN))) {
					if (key)
						printf("SENSE key %d; %s\n",
							 key,printkey(key));
				}
				/* see if residual is valid for disk */
				if (!is_a_tape && (un->un_sense[0] & VALID)) {
					addr =(int)((un->un_sense[3]<<24)|
					    (un->un_sense[4]<<16)|
					    (un->un_sense[5]<< 8)|
					    (un->un_sense[6]));
				    printf("physical block address 0x%x (%d)\n",
								addr,addr);
				}
				if (i = un->un_sense[12])
			       	    	printf("error code 0x%x(%d); %s\n",
							i,i,print_errcode(i));
				/* see if residual is valid for tape */
				if (is_a_tape && (un->un_sense[0] & VALID)) {
					count=(int)((un->un_sense[4]<<16)|
						    (un->un_sense[5]<< 8)|
						    (un->un_sense[6]));
					count <<= SCTRSHFT;
					bp->b_resid += count;
				}
	 			/* tape file mark or EOM reached? */
				if (is_a_tape) {
					if (un->un_sense[2] & FM)
						un->un_flags |= INT_FM;
					else if (un->un_sense[2] & EOM) {
						un->un_flags |= INT_EOM;
						bp->b_flags |= B_ERROR;
					} else if (key == UNIT_ATN) {
						un->un_flags |= INT_ATN;
					/* offline or no tape installed */
					} else if (key == NOT_RDY) {
						un->un_flags |= INT_NOT_RDY;
						bp->b_flags |= B_ERROR;
					} else
						bp->b_flags |= B_ERROR;
					goto tapedone;
				}
				if (key == REC_ERR) {
					printf("Command completed with recovery\n");
					goto normcomp;
				}
			}
		} else {
			printf("in%d: ERROR on Task %x\n",unit,taskid&0x7f);
			printf("Scsi ERROR status: %s\n",
				(char *)printerr(ip->in_scstatus));
		}
		if (un->un_nretries++ <= NRETRIES) {
			un->un_bufinuse = 0;
			un->un_bn  = un->un_prev_bn;
			bp->b_resid = un->un_prev_b_resid;
			dkin_command(ctlr, unit, bp);
			return;
		} else {
error:
			un->un_nretries = 0;
			bp->b_flags |= B_ERROR;
			count = un->un_prev_secs * NBPSCTR;
			bp->b_resid += count;
		}
	} else {
	/* 
	 * normal command completion
	 */
normcomp:
		un->un_nretries = 0;
		count=((ip->in_cnth<<16)|(ip->in_cntm<<8)|ip->in_cntl);
		if (count) printf("(%x %d)",count,unit);
/*
		ASSERT(count == 0);
 */
		if (is_a_tape)
			goto tapedone;
		/* 
		 * Temporary buffer may hold data from transfer.  This buffer
		 * is only used when a transfer must cross a page boundary.
		 */
		if (un->un_bufinuse) {
			ASSERT(un->un_bufva >= bp->b_dmaaddr);
			ASSERT(un->un_bufva < bp->b_dmaaddr + bp->b_bcount);
			ASSERT(un->un_bufdata <= NBPSCTR);
			if (bp->b_flags & B_READ) {
				/*
				 * Copy data from private buffer into mapped
				 * region.
				 */
				bcopy((caddr_t) K0_TO_K1(un->un_buf),
				      un->un_bufva, (int) un->un_bufdata);
			}
			un->un_bufinuse = 0;
			un->un_bufva = 0;
		}
		/*
		 * check for more data to transfer
		 */
		if (bp->b_resid > 0) {
			dkin_command(ctlr, unit, bp);
			return;
		}
	}
	/* 
	 * advance buffer queue 
	 */
	dp->b_active = 0; /* this drive no longer active */
#ifdef FAST
	if (ci->c_waiting) {
	   ci->c_waiting = 0; /* clear now so it can be set again if needbe */
	   for (i=0; i < NDKIN; i++) {
		un = &dkin_softc[ctlr].c_un[i];
		if (!(un->un_flags & INT_ALIVE)) continue;
		/* can't fire off tape since it sleeps!! */
		if (un->un_flags & INT_TAPE) continue;
		dp = &dkintab[ctlr][i];
		if (dp->b_active == 2) /* iopb not submitted */
			int_startit(un, INT_INTERRUPT, ctlr, i, NOWAIT);
	   }
	   dp = &dkintab[ctlr][unit];
	}
#endif FAST
	if (dp->b_actf = bp->av_forw) dkinstart(ctlr, unit, dp);
	/* lastly, mark the completed buffer done */
	iounmap(bp);
	iodone(bp);
	return;
tapedone:
#ifdef FAST
	if (ci->c_waiting) {
	   ci->c_waiting = 0; /* clear now so it can be set again if needbe */
	   for (i=0; i < NDKIN; i++) {
		un = &dkin_softc[ctlr].c_un[i];
		if (!(un->un_flags & INT_ALIVE)) continue;
		/* can't fire off tape since it sleeps!! */
		if (un->un_flags & INT_TAPE) continue;
		dp = &dkintab[ctlr][i];
		if (dp->b_active == 2) /* iopb not submitted */
			int_startit(un, INT_INTERRUPT, ctlr, i, NOWAIT);
	   }
	   un = &dkin_softc[ctlr].c_un[unit];
	}
#endif FAST
	un->un_flags &= ~(INT_BUSY | INT_REWINDING);
	if (un->un_flags & INT_WAITING) {
		un->un_flags &= ~INT_WAITING;
		wakeup((caddr_t) un);
	}
}
/*
** device open routine
*/
dkinopen(dev)
dev_t dev;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct int_unit *un;
	register struct buf *bp;
	int s;

	if ((unit >= NDKIN) || (ctlr >= NDKINC)) {
		u.u_error = EIO;
		return;
	}
	if ((dkin_softc[ctlr].c_flags & INT_ALIVE) == 0) { /* cntrlr OK? */
		u.u_error = ENODEV;
		return;
	}
	un = &dkin_softc[ctlr].c_un[unit];
	if ((un->un_flags & (INT_ALIVE|INT_READY)) == 0) { /* unit OK? */
		u.u_error = ENODEV;
		return;
	}
	if (un->un_flags & INT_TAPE) {
		if (un->un_flags & INT_OPEN) { /* sorry, we're exclusive open */
			u.u_error = EBUSY;
			return;
		}
		bp = &rdkinbuf[ctlr][unit];
		s = splclock();
		un->un_flags |= INT_OPEN;
		splx(s);
		if (tapeopen(dev, ctlr, unit, un)) {
			s = splclock();
			un->un_flags &= ~INT_OPEN;
			bp->b_flags  &= ~B_ERROR;
			splx(s);
			return;
		}
	}
	return;
}
tapeopen(dev, ctlr, unit, un)
dev_t dev;
int ctlr, unit;
struct int_unit *un;
{
	register struct buf *bp;
	register int s;

	/* wait for a previous rewind to complete */
	s = splbio();
	while (un->un_flags & INT_REWINDING) {
		un->un_flags |= INT_WAITING;
		if (sleep((caddr_t) un, PUSER|PCATCH)) {
			/* user gave up waiting...fail the open */
			un->un_flags &= ~(INT_WAITING | INT_OPEN);
			splx(s);
			u.u_error = EINTR;
			return 1;
		}
	}
	splx(s);
	/* use TUN to see if cartridge installed, etc.
	 * and (TODO) report findings to the user
	 * if the request sense doesn't report 
	 */
	bp = &rdkinbuf[ctlr][unit];
	sameiopb(ctlr, unit, 0, 0);
	setupiopb(C0_TESTRDY,ctlr,unit,0,un,INT_INTERRUPT,0,0,0);
	int_startit(un, INT_INTERRUPT, ctlr, unit, WAIT);
	if (bp->b_flags & B_ERROR) {
		if (un->un_flags & INT_NOT_RDY)
			printf("tape not ready; offline or no tape\n");
		u.u_error = EIO;
		return 1;
	}
	if (un->un_flags & INT_ATN)
	      printf("tape unit attention; media change or drive was reset\n");
	return 0;
}
/*
** device close routine
*/
dkinclose(dev, flag)
dev_t dev;
register int flag;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct int_unit *un;
	register struct int_iopb *ip;
	register struct buf *bp;
	register int s;

	un = (struct int_unit *)&dkin_softc[ctlr].c_un[unit];
	bp = &rdkinbuf[ctlr][unit];
	if (un->un_flags & INT_TAPE) {
		ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
		tapeclose(dev, ctlr, unit, un, ip); 
	}
	s = splclock();
	un->un_flags &= ~(INT_OPEN|INT_WRITTEN|INT_READ|INT_FM|INT_EOM|INT_ATN);
	bp->b_flags  &= ~B_ERROR;
	splx(s);
	return;
}
tapeclose(dev, ctlr, unit, un, ip)
dev_t dev;
int ctlr, unit;
struct int_unit *un;
struct int_iopb *ip;
{
	register struct buf *bp;

	bp = &rdkinbuf[ctlr][unit];
	if (un->un_flags & INT_WRITTEN) {
		sameiopb(ctlr, unit, 0, 0);
		setupiopb(C0_WRFM,ctlr,unit,0,un,INT_INTERRUPT,1,0,0);
		int_startit(un, INT_INTERRUPT, ctlr, unit, WAIT);
		if (bp->b_flags & B_ERROR) {
			u.u_error = EIO;
			return;
		}
	}
	/* rewind the tape as appropriate
	 * and don't wait for completion
	 */
	if (!(NOREWIND(dev))) {
		/* TODO: what should addr be here */
		un->un_flags |= INT_REWINDING;
		sameiopb(ctlr, unit, 0, 0);
		setupiopb(C0_REWIND,ctlr,unit,0,un,INT_INTERRUPT,0,0,0);
		int_startit(un, INT_INTERRUPT, ctlr, unit, NOWAIT);
	}
}
/*
**
*/
dkinstrategy(bp)
register struct buf *bp;
{
	daddr_t bn;
	long sc;
	register struct iobuf *dp;
	struct volume_header *vh;
	struct device_parameters *devp;
	struct partition_table *pt;
	register struct int_unit *un;
	register int unit;
	int cmd, s, ctlr;

	/*
	 * check for valid unit number and unit's existence
	 */
	unit = UNIT(bp->b_dev);
	ctlr = CTLR(bp->b_dev);
	if ((unit >= NDKIN) || (ctlr >= NDKINC))
		goto badio;
	if ((dkin_softc[ctlr].c_flags & INT_ALIVE) == 0) /* cntrlr OK? */
		goto badio;
	un = &dkin_softc[ctlr].c_un[unit];
	if ((un->un_flags & (INT_ALIVE|INT_READY)) == 0) /* unit OK? */
		goto badio;
	/*
	 * make sure unit has a valid volume header
	 */
	if (un->un_vhvalid == 0)
		goto badio;
	/* 
	 * calc number of blocks in transfer and make sure the request 
	 * is contained in the partition
	 */
	vh = &un->un_vh;
	pt = &vh->vh_pt[FS(bp->b_dev)];
	sc = BTOBB(bp->b_bcount);
	bn = bp->b_blkno;
	if ((bn < 0) || ((bn + sc) > pt->pt_nblks)) {
		printf("bn= %d sc= %d pt_nblks= %d\n",bn,sc,pt->pt_nblks);
		goto badio;
	}
	devp = &vh->vh_dp;
	/*
	 * set cylinder number for disksort()
	 */
	bp->b_cylin = (bn + pt->pt_firstlbn) / (devp->dp_secs * devp->dp_trks0);
	bp->av_forw = NULL;

	/* update accounting for this buffer */
	s = splbio();
	bp->b_start = lbolt;
	un->un_iotime.io_cnt++;
	un->un_iotime.io_bcnt += sc;

	/* queue request */
	iomap(bp);
	dp = &dkintab[ctlr][unit];
	disksort(dp, bp, un->un_lastcyl);
	if (dp->b_active == 0) {
		dkinstart(ctlr, unit, dp);
	}
	splx(s);
	return;
badio:
	bp->b_flags |= B_ERROR;
	iounmap(bp);
	iodone(bp);
}
/*
 * setup a device operation
 */
void
dkinstart(ctlr, unit, dp)
register int ctlr, unit;
register struct iobuf *dp;
{
	register struct buf *bp;
	struct int_unit *un;
	int s;

	if ((bp = dp->b_actf) == NULL) { /* no work for this drive? */
		dp->b_active = 0;
		return;
	}
	un = &dkin_softc[ctlr].c_un[unit];
	s = splclock();
#ifdef FAST
	if (dp->b_active == 2) { /* waiting to issue iopb */
		int_startit(un, INT_INTERRUPT, ctlr, unit, NOWAIT);
		splx(s);
		return;
	} else if (dp->b_active) {
		splx(s);
		return;
	} else dp->b_active = 1; /* drive is now busy */
#else  FAST
	if (dp->b_active == 1) { /* already active? */
		splx(s);
		return;
	} else dp->b_active = 1; /* say the drive is busy (iopb submitted) */
#endif FAST
	splx(s);
	/*
	 * initialize information for data transfer and start command
	 */
	if (bp->b_flags & B_READ)
		un->un_command = C0_READ;
	else 
		un->un_command = C0_WRITE;
	un->un_lastcyl = bp->b_cylin; /* save the cyl we're on */
	bp->b_resid = bp->b_bcount;
	un->un_bn = bp->b_blkno + 
		un->un_vh.vh_pt[FS(bp->b_dev)].pt_firstlbn;
	dp->io_start = lbolt;
	dkin_command(ctlr, unit, bp);
}
/*
 * issue a data transfer command
 */
dkin_command(ctlr, unit, bp)
register int ctlr, unit;
register struct buf *bp;
{
	register struct int_unit *un;

	if (bp->b_resid == 0)
		return;
#ifdef SCAT_GATH
	dkin_sgsetup(ctlr, unit, bp);
#else  SCAT_GATH
	dkin_iopbset(ctlr, unit, bp);
#endif SCAT_GATH
	un = &dkin_softc[ctlr].c_un[unit];
	/*
	 * start controller on command
	 */
	int_startit(un, INT_INTERRUPT, ctlr, unit, NOWAIT);
}
/*
 * setup iopb with scatter/gather entries for data transfer
 */
dkin_sgsetup(ctlr, unit, bp)
register int ctlr, unit;
register struct buf *bp;
{
	register struct int_unit *un;
	register struct insge *sge;
	register long amount;
	register int links;
	register long dmaaddr;
	register long physaddr;
	register long total;
	register long offset;
	register long t_resid;

	un = &dkin_softc[ctlr].c_un[unit];
	/*
	 * save current state for software retry if command fails
	 */
	un->un_prev_b_resid = bp->b_resid;
	un->un_prev_bn = un->un_bn;

	un->un_sgentries = 0;	/* reset */
	un->un_prev_secs = 0;	/* reset */
	sge = un->un_insge;
	/*
	 * fill in scatter gather structs
	 */
	dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
	links = 0;
	total = 0;
	while (bp->b_resid) {
		/* limit this sge to this page */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;

		/* fill in sg struct */
		physaddr = ctob(kvtokptbl(dmaaddr)->pgm.pg_pfn);
		ASSERT(physaddr != 0);
		sge->insg_cnth = HB(amount/sizeof(int));
		sge->insg_cntm = MB(amount/sizeof(int));
		sge->insg_cntl = LB(amount/sizeof(int));
		sge->insg_addr = physaddr | offset;
		sge->insg_addrmod = VME_A32NPAMOD;

		bp->b_resid -= amount;
		dmaaddr += amount;
		total += amount;
		un->un_sgentries++;
		if (++links >= NSGENTRY) {
			/*
			 * Ran out of links.  
			 * Truncate request to a sector boundary.
			 * Stop now and start transfer.
			 */
			t_resid = total & (NBPSCTR - 1);
			if (t_resid != 0) {
				ASSERT (amount >= t_resid);
				amount -= t_resid;
				total -= t_resid;
				dmaaddr -= t_resid;
				bp->b_resid += t_resid;
				sge->insg_cnth = HB(amount/sizeof(int));
				sge->insg_cntm = MB(amount/sizeof(int));
				sge->insg_cntl = LB(amount/sizeof(int));
			}
			break;
		}
		sge++;
	}
	total >>= SCTRSHFT;			/* convert to sectors */
	if (!total) total = 1;
	un->un_bn += total;
	un->un_prev_secs = total;
	if (links == 1) {
		/*
		 * With only one link, don't bother scatter/gathering
		 */
		physaddr |= offset;
		sameiopb(ctlr, unit, NOSG, 0);
		setupiopb(un->un_command,ctlr,unit,physaddr,un,INT_INTERRUPT,
						total,un->un_prev_bn,0);
	} else {
		physaddr = K0_TO_PHYS((u_int)&un->un_sgentries);
		sameiopb(ctlr, unit, SG, 0);
		setupiopb(un->un_command,ctlr,unit,physaddr,un,INT_INTERRUPT,
						total,un->un_prev_bn,0);
	}
}
/*
 * setup iopb for data transfer
 */
dkin_iopbset(ctlr, unit, bp)
register int ctlr, unit;
register struct buf *bp;
{
	register struct int_unit *un;
	register int amount;
	register long dmaaddr;
	register long physaddr;
	register long offset;
	register sc;

	dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
	/*
	 * save current state for software retry if command fails
	 */
	un = &dkin_softc[ctlr].c_un[unit];
	un->un_prev_b_resid = bp->b_resid;
	un->un_prev_bn = un->un_bn;
	un->un_prev_secs = 0;
	/*
	 * limit this iopb to this page
	 */
	amount = bp->b_resid;
	offset = dmaaddr & (NBPP - 1);
	if (offset + amount > NBPP)
		amount = NBPP - offset;
	amount &= ~(NBPSCTR - 1);		/* round down */

	if (amount < NBPSCTR) {
		ASSERT(amount == 0);
		if (bp->b_resid < NBPSCTR)
			amount = bp->b_resid;
		else
			amount = NBPSCTR;

		un->un_bufinuse = 1;
		un->un_bufva = (caddr_t) dmaaddr;
		un->un_bufdata = amount;
		if (bp->b_flags & B_READ) {
			/*
			 * reads get data from disk. copy is done in
			 * interrupt routine
			 */
		} else {
			/*
			 * Copy data from users buffer into
			 * static buffer where disk can get to it.
			 */
			bcopy((caddr_t) dmaaddr,
			      (caddr_t) K0_TO_K1(un->un_buf), amount);
		}
		physaddr = K0_TO_PHYS(un->un_buf);
	} else {
		/* fill in iopb struct */
		physaddr = ctob(kvtokptbl(dmaaddr)->pgm.pg_pfn)
				| offset;
		ASSERT(physaddr != offset);
	}
	/*
	 * set up information for iopb command
	 */ 
	sc = amount >> SCTRSHFT;
	if (!sc) sc = 1; /* for raw xfers < 512 bytes need to round up */
	sameiopb(ctlr, unit, NOSG, 0);
	setupiopb(un->un_command,ctlr,unit,physaddr,un,
					INT_INTERRUPT,sc,un->un_bn,0);
	un->un_prev_secs = sc;
	bp->b_resid -= amount;
	ASSERT(bp->b_resid >= 0);
	un->un_bn += sc;
}
/*
 * no interrupt received from controller, within timeout period,
 * for last command issued to it.
 */
void
dkin_timeout(un)
register struct int_unit *un;
{
	register struct insge *sge;
	int s, i;

	sge = un->un_insge;
	s = splbio();
	printf("\ndkin(%d,%d): COMMAND TIMEOUT!\n",un->un_ctlr,un->un_unit);
	dkin_softc[un->un_ctlr].c_timeouts++;
	dump_iopb(un->un_ctlr,un->un_unit);
	for (i=0;i<4;i++,sge++) {
		printf("sge %d: h= %x m= %x l= %x addr= %x amod= %x\n",i,
			sge->insg_cnth,
			sge->insg_cntm,
			sge->insg_cntl,
			sge->insg_addr,
			sge->insg_addrmod);
	}
	splx(s);
}
/*
 * read from device
 */
dkinread(dev)
dev_t dev;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct buf *bp;
	register struct int_unit *un;
	register int amount, error, s;

	if ((unit >= NDKIN) || (ctlr >= NDKINC)) {
		u.u_error = EIO;
		return;
	}
	un = &dkin_softc[ctlr].c_un[unit];
	if (!(un->un_flags & INT_TAPE)) {
		if (physck(un->un_vh.vh_pt[FS(dev)].pt_nblks, B_READ))
			physio(dkinstrategy, 0, dev, B_READ);
		return;
	}
	/* tape-only code
	 */
	bp = &rdkinbuf[ctlr][unit];
	if (un->un_flags & INT_WRITTEN) /* can't read a 'written' tape */
		return (EINVAL);
	if ((long)u.u_count & 1) {
		/*
		 * Only allow even i/o counts
		 */
		u.u_error = EIO;
		return;
	}
	while (u.u_count) {
		if (un->un_flags & INT_FM) /* can't read past FM */
			return (0);
		else if (un->un_flags & INT_EOM) /* EOM already reached */
			return (ENXIO);
		amount = u.u_count;
		if (amount > MAXLEN)
			amount = MAXLEN;
		if (amount & 511)
			amount += 512 - (amount & 511);
		if (tapeio(un, dev, bp, amount, B_READ)) {
			u.u_error = EIO;
			return;
		}
		if (bp->b_flags & B_ERROR) {
			if (un->un_flags & INT_EOM) {
				u.u_error = ENXIO;
			} else u.u_error = EIO;
			return;
		}
#ifdef DEBUGTAPE
		printf("\ntaperead: len= %x b_resid= %x ",amount, bp->b_resid);
#endif DEBUGTAPE
		amount = MIN(amount - bp->b_resid, amount);
		iomove((caddr_t) K0_TO_K1(tbuffer[ctlr][unit]),amount,B_READ);
		if (u.u_error)
			break;
	}
}
/*
 * write to device
 */
dkinwrite(dev)
dev_t dev;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct int_unit *un;
	register struct buf *bp;
	register int amount, error, s;

	if ((unit >= NDKIN) || (ctlr >= NDKINC)) {
		u.u_error = EIO;
		return;
	}
	un = (struct int_unit *)&dkin_softc[ctlr].c_un[unit];
	if (!(un->un_flags & INT_TAPE)) {
		if (physck(un->un_vh.vh_pt[FS(dev)].pt_nblks, B_WRITE))
			physio(dkinstrategy, 0, dev, B_WRITE);
		return;
	}
	/* tape-only code
	 */
	bp = &rdkinbuf[ctlr][unit];
	if (un->un_flags & INT_READ) { /* can't write a 'read' tape */
		u.u_error = EINVAL;
		return;
	}
	if ((long)u.u_count & 1) {
		/*
		 * Only allow even i/o counts
		 */
		u.u_error = EIO;
		return;
	}
	while (u.u_count) {
		if (un->un_flags & INT_EOM) { /* EOM already reached */
			u.u_error = ENXIO;
			return;
		}
		amount = MIN(MAXLEN, u.u_count);
		if (amount & 511) {
			/* clear the part not being used */
			bzero((caddr_t) K0_TO_K1(tbuffer[ctlr][unit])
				+ (amount & 511), 512 - (amount & 511));
			amount += 512 - (amount & 511);
		}
		iomove((caddr_t)K0_TO_K1(tbuffer[ctlr][unit]),u.u_count,B_WRITE);
		if (u.u_error)
			break;
		if (tapeio(un, dev, bp, amount, B_WRITE)) {
			u.u_error = EIO;
			return;
		}
		if (bp->b_resid || (bp->b_flags & B_ERROR)) {
			if (un->un_flags & INT_EOM)
				u.u_error = ENXIO;
			else u.u_error = EIO;
			return;
		}
	}
}
tapeio(un, dev, bp, len, rw)
	register struct int_unit *un;
	dev_t dev;
	register struct buf *bp;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register int s, sc;

	bp->b_flags |= rw;
	bp->b_un.b_addr = tbuffer[ctlr][unit];
	bp->b_resid = 0;
	sc = (len+(NBPSCTR-1)) >> SCTRSHFT;
	/*
	 * initialize information for data transfer and start command
	 */
	if (rw) {
		un->un_command = C0_READ;
		un->un_flags |= INT_READ;
	} else {
		un->un_command = C0_WRITE;
		un->un_flags |= INT_WRITTEN;
	}
	/*
	 * set up information for iopb command
	 */ 
	sameiopb(ctlr, unit, 0, 0);
	setupiopb(un->un_command, ctlr, unit, K0_TO_PHYS(bp->b_un.b_addr),
					un,INT_INTERRUPT, sc, 0, 0);
	if (int_startit(un, INT_INTERRUPT, ctlr, unit, WAIT))
		return 1;
	else return 0;
}
/*
 * ioctl routine
 */
dkinioctl(dev, cmd, arg)
register dev_t dev;
register unsigned int cmd;
register caddr_t arg;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct volume_header *ipv;
	register struct mount *mp = 0;
	register struct media_defect *md;
	struct diag_info *d;
	struct verify_info *v;
	struct int_unit *un;
	struct int_iopb *ip;
	int error, i;
	u_int status, addr;

	if ((unit >= NDKIN) || (ctlr >= NDKINC)) {
		u.u_error = EIO;
		return;
	}
	if ((dkin_softc[ctlr].c_flags & INT_ALIVE) == 0) { /* cntrlr OK? */
		u.u_error = ENODEV;
		return;
	}
	un = &dkin_softc[ctlr].c_un[unit];
	if ((un->un_flags & (INT_ALIVE|INT_READY)) == 0) { /* unit OK? */
		u.u_error = ENODEV;
		return;
	}
	if (un->un_flags & INT_TAPE) {
		tapeioctl(dev, cmd, arg);
		return;
	}
	ipv = &un->un_vh;

	switch (cmd) {
	  case DIOCGETVH:
		/* get volume header */
		{
			struct io_arg io_arg;

			if (!un->un_vhvalid)
				u.u_error = EIO;
			else
			if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
				u.u_error = EFAULT;
			else
			if (copyout((caddr_t)ipv,
				    (caddr_t) io_arg.memaddr,
				    (int) io_arg.datasz) < 0)
				u.u_error = EFAULT;
		}
		break;

	case DIOCINITVH:
		/*
		 * init driver's idea of drives volume header information
		 */

		{
			struct io_arg io_arg;

			if (suser() == 0) {
				u.u_error = EPERM;
				return;
			}
			if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
				u.u_error = EFAULT;
			if (copyin((caddr_t) io_arg.memaddr,
				    (caddr_t) ipv,
				    sizeof(struct volume_header) < 0))
				u.u_error = EFAULT;
			un->un_vhvalid = 1;
		}
		break;

	case DIOCFMTMAP:
		/*
		 * perform format operation.
		 * must be superuser and partition cannot be currently mounted.
		 */
		if (suser() == 0) {
			u.u_error = EPERM;
			return;
		}
#ifdef notdef
		for (mp = &mounttab[0]; mp < &mounttab[NMOUNT]; mp++) {
			if (mp->m_bufp != 0 && dev == mp->m_dev) {
				u.u_error = EBUSY;
				return;
			}
		}
#endif notdef
		break;

	case DIOCVFYSEC:
		/*
		 * verify sectors are readable
		 */
		printf("DIOCVFYSEC: unsupported in scsi\n");
		u.u_error = EINVAL;
		return;

	case DIOCDIAG:
		/*
		 * issue diagnostic command to controller
		 */
		printf("DIOCDIAG: unsupported in scsi\n");
		u.u_error = EINVAL;
		return;

	case DIOCNOECC:
		/*
		 * enable/disable ecc correction
		 */
		un->un_err.err_pagecode = 1;
		un->un_err.err_pagelength = 6;
		if (*(int *)arg) un->un_err.err_params1 = DCR|PER;
		else un->un_err.err_params1 = PER;
		un->un_err.err_params2 = 0x8; /* retry count */
		un->un_err.err_params3 = 0x8; /* correction span (bits) */
		addr = K1_TO_PHYS(&un->un_err);
		sameiopb(ctlr,unit,0,1); /* NO scat/gath and PRIORITY */
		setupiopb(C0_MODESEL,ctlr,unit,addr,un,INT_POLLED, 0,0,
						sizeof(struct err_page));
		if (status = int_startit(un, INT_POLLED, ctlr, unit, NOWAIT))
			goto stat;
		break;

	case DIOCRDEFECTS:
		/*
		 * read defect information off the specified track into
		 * temporary buffer. we only want to return the defect
		 * information to the caller.
		 */
		printf("DIOCRDEFECTS: unsupported in scsi\n");
		u.u_error = EINVAL;
		return;

	default:
		printf("in%d: Unknown ioctl 0x%x\n",unit,cmd);
		u.u_error = EINVAL;
		return;
	}
	return;
stat:
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&un->un_sense[0]);
		sameiopb(ctlr,unit,0,1); /* NO scat/gath and PRIORITY */
		setupiopb(C0_REQSENSE,ctlr,unit,addr,un,INT_POLLED,0,0,16);
		for (i=0; i < 16; i++)
			un->un_sense[i] = 0; /* flush out */
		if (int_startit(un, INT_POLLED, ctlr, unit, NOWAIT)) {
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
			if (i = (un->un_sense[2] & KEYMASK))
				printf("SENSE key %d; %s\n",i,printkey(i));
			/* see if residual is valid for disk */
			if (un->un_sense[0] & VALID) {
				addr =(int)((un->un_sense[3]<<24)|
				    (un->un_sense[4]<<16)|
				    (un->un_sense[5]<< 8)|
				    (un->un_sense[6]));
				printf("physical block address 0x%x (%d)\n",
								addr,addr);
			}
			if (i = un->un_sense[12])
		       	       printf("error code 0x%x(%d); %s\n",
						i,i,print_errcode(i));
		}
	}
	u.u_error = EIO;
	return;
}
void
tapeioctl(dev, cmd, data)
	dev_t dev;
	caddr_t data;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct buf *bp;
	struct int_unit *un;
	int command, count, code = 0;
	struct mtop *mtop;
	struct mtget *mtget;

	bp =  &rdkinbuf[ctlr][unit];
	un =  &dkin_softc[ctlr].c_un[unit];
	switch (cmd) {

	case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		switch (mtop->mt_op) {

		case MTWEOF:
			count = mtop->mt_count;
			un->un_command = C0_WRFM;
			break;

		case MTFSF: case MTBSF:
			count = mtop->mt_count;
			if (mtop->mt_op == MTBSF)
				count = ~count + 1; /* two's complement */
			code = FILES;
			un->un_command = C0_SPACE;
			break;

		case MTFSR: case MTBSR:
			count = mtop->mt_count;
			if (mtop->mt_op == MTBSR)
				count = ~count + 1; /* two's complement */
			code = BLOCKS;
			un->un_command = C0_SPACE;
			break;

		case MTREW:
			count = 0;
			un->un_command = C0_REWIND;
			break;

		case MTOFFL: case MTNOP: case MTRET: case MTRST:
			u.u_error = EINVAL;
			return;

		default:
			u.u_error = EINVAL;
			return;
		} /* end of switch */

		sameiopb(ctlr, unit, 0, 0);
		setupiopb(un->un_command,ctlr,unit,0,un,INT_INTERRUPT, count, 0, code);
		int_startit(un, INT_INTERRUPT, ctlr, unit, WAIT);
		if ((mtop->mt_op==MTFSR || mtop->mt_op==MTBSR) && bp->b_resid) {
			u.u_error = EIO;
			return;
		}
		if (bp->b_error) {
			u.u_error = EIO;
			return;
		}
		/* in this case we're no longer at EOM */
		if (mtop->mt_op==MTBSR || mtop->mt_op==MTBSF)
			un->un_flags &= ~INT_EOM;

	case MTIOCGET:	/* TODO: figure this out */
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = 0xcaca;	/* sc->sc_sts.s_xs0 */
		mtget->mt_erreg = 0xdada;	/* sc->sc_sts.s_xs1 */
		mtget->mt_resid = 0xacac;	/* sc->sc_resid	    */
		mtget->mt_type = MT_ISQIC;	/* new def for SCSI?? */
		break;
	default:
		u.u_error = EINVAL;
		return;
	}
}
int_format(dev)
dev_t dev;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	register struct int_unit *un;
	register struct int_iopb *ip;
	int s;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	un = &dkin_softc[ctlr].c_un[unit];
	if ((un->un_flags & INT_DISK) == 0) {
		printf("in%d: unit is NOT a disk drive\n", unit);
		u.u_error = EFAULT;
		return 1;
	}
	sameiopb(ctlr, unit, 0, 0);
	setupiopb(C0_FORMAT, ctlr, unit, 0, un, INT_INTERRUPT,0,0,0);
	int_startit(un, INT_INTERRUPT, ctlr, unit, NOWAIT);
	/* wait for a previous rewind to complete */
	s = splbio();
	printf("\nin%d: Formatting........ ", unit);
	un->un_flags |= INT_WAITING;
	while (un->un_flags & INT_WAITING) {
#ifdef notdef
		sleep((caddr_t) un, PRIBIO);
#endif notdef
		if (sleep((caddr_t) un, PUSER|PCATCH)) {
			/* user gave up waiting...fail the format */
			un->un_flags &= ~(INT_WAITING | INT_OPEN);
			splx(s);
			u.u_error = EINTR;
			return 1;
		}
	}
	un->un_flags |= INT_FORMATTED;
}
/*
** setupiopb() - Routine to set up the IOPB for the Introl Board.
*/
setupiopb(cmd, ctlr, unit, addr, un, mode, blkcount, lba, bcount)
	int cmd, ctlr, unit, addr;
	struct int_unit *un;
	int mode, blkcount, lba, bcount;
{
	register struct int_iopb *ip;
	register int c;
	struct int_ctlr *introl = &dkin_softc[ctlr];
	struct volume_header *vh;
	struct device_parameters *devp;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	/* MAKE EACH TASKID FOR A UNIT UNIQUE */
	if ((++taskidnum[unit] + unit) >= 0x10) taskidnum[unit] = unit;
	ip->in_task = (unit<<4)|taskidnum[unit];	/* Task ID */
	ip->in_bufh = ((u_int)addr>>16) & 0xFFFF;	/* Data Buffer */
	ip->in_bufl = ((u_int)addr) & 0xFFFF;		/* Data Buffer */
	if (!blkcount)
		c = bcount/sizeof(int);
	else
		c = blkcount*(NBPSCTR/sizeof(int));
	ip->in_cnth = HB(c);		/* Transfer Count */
	ip->in_cntm = MB(c);		/* Transfer Count */
	ip->in_cntl = LB(c);		/* Transfer Count */
	if (mode == INT_INTERRUPT) {
		ip->in_adstatus = 0;	/* Adapter status */
		ip->in_intlv = introl->c_ipl;  /* controller IPL */
	} else {
		ip->in_adstatus = 0xAA;	/* Adapter status */
		ip->in_intlv = 0;	/* forces POLLED operation */
	}
	ip->in_scstatus = 0;		/* SCSI status */
	ip->in_target = unit;		/* Target ID */
	/*
	** These are the Command Descriptor Bytes in the IOPB
	** Must be set up for a 6 byte or 10 byte CD.
	*/
	ip->incd_b0 = cmd;	/* Introl Command */
	/* Six -or- Ten Byte CDBlock - Depends on the Command used */
	if (cmd == C0_READ || cmd == C0_WRITE || cmd == C0_WRFM ||
					cmd == C0_SPACE) {
		if (un->un_flags & INT_TAPE) { /* are we a tape? */
			if (cmd == C0_WRFM)
				ip->incd_b1 = 0;
			else if (cmd == C0_SPACE)
				ip->incd_b1 = bcount; /* space code */
			else
				ip->incd_b1 = 1; /* Fixed block size */
			ip->incd_b2 = HB(blkcount);
			ip->incd_b3 = MB(blkcount);
			ip->incd_b4 = LB(blkcount);
		} else {
			ip->incd_b1 = (0<<5)|HB(lba);
			ip->incd_b2 = MB(lba);
			ip->incd_b3 = LB(lba);
			ip->incd_b4 = blkcount;
		}
		return;
	}
	ip->incd_b1 = (0<<5);
	ip->incd_b2 = 0;
	ip->incd_b3 = 0;
	ip->incd_b5 = 0;
	/* Incoming Data or Information */
	if (cmd == C0_TESTRDY || cmd == C0_REQSENSE ||
			cmd == C0_INQUIRY || cmd == C1_READCAP) {
		ip->incd_b4 = bcount;
	} else if (cmd == C0_FORMAT) {
		vh = &un->un_vh;
		devp = &vh->vh_dp;
		ip->incd_b3 = MB(devp->dp_interleave);
		ip->incd_b4 = LB(devp->dp_interleave);
	} else if (cmd == C0_MODESEL) {
		ip->incd_b1 |= PAGE_FORMAT;
		ip->incd_b4 = bcount;
	} else {
		ip->incd_b4 = bcount;
		ip->incd_b6 = 0;
		ip->incd_b7 = 0;
		ip->incd_b8 = 0;
		ip->incd_b9 = 0;
		ip->incd_b10 = 0;
		ip->incd_b11 = 0;
	}
}
/*
**
*/
sameiopb(ctlr, unit, scg, pri)
{
	register struct int_iopb *ip;
	struct int_unit *un;

	un = &dkin_softc[ctlr].c_un[unit];
	ip =  dkin_softc[ctlr].c_un[unit].un_iopbp;

#ifndef NOARB
	if (un->un_flags & INT_ARB_DISABLE)
		ip->in_ext_ctl = ARB_DISABLE;
	else 
		ip->in_ext_ctl = 0;
#else  NOARB
	ip->in_ext_ctl = ARB_DISABLE;
#endif NOARB
	ip->in_addmod = VME_A32NPAMOD;	/* AM 0x09 - 32 bit Addressing */
	if (scg)
		ip->in_adctl = (AC_BURST|AC_WIDTH32|AC_SCAT_GATH);
	else
		ip->in_adctl = (AC_BURST|AC_WIDTH32);
	if (pri)
		ip->in_adctl |= AC_IOPBPRI; /* go to head of class */
	ip->in_burst  = BURSTVALUE;	/* Data Throttle Burst */
	ip->in_break  = BURSTBREAK;	/* Data Throttle Break */
	ip->in_statid = dkin_softc[ctlr].c_vec; /* controller Vector ID */
}
/*
** fire off command
*/
int_startit(un, mode, ctlr, unit, wait)
	register struct int_unit *un;
	register int mode, ctlr, unit, wait;
{
	register struct int_iopb *ip;
	register int timesup = LOOPS;
	u_int addr, tv, s, t;
	u_short s1, s2;
	register struct iobuf *dp;
	struct int_ctlr *ci;
#ifdef DEBUGPOLL
	u_char d;
	if (mode != INT_INTERRUPT)
		dump_iopb(ctlr,unit);
#endif DEBUGPOLL
#ifdef DEBUGTAPE
	if (un->un_flags & INT_TAPE)
		dump_iopb(ctlr,unit);
#endif DEBUGTAPE
	s = splbio();

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	addr = K0_TO_PHYS(ip);	/* iopb address */
	dp = &dkintab[ctlr][unit];
	ci = &dkin_softc[ctlr];
#ifdef FAST
	while((INT_STATUS(ctlr) & HOST_A_RDY) == 0) { /* controller busy? */
		/* if we're tape we must wait since a sleep is required */
		if ((mode == INT_INTERRUPT) && (!(un->un_flags & INT_TAPE))) {
	/*		printf("%d",unit); /* DEBUG only */
			dp->b_active = 2; /* ready to sent iopb */
			ci->c_waiting = 1;
			return 0;
		} else {
	/*		printf("@"); /* DEBUG only */
			if (--timesup == 0) {
				printf("in%d: never ready for iopb\n",unit);
				return 1;
			}
		}
	}
#endif FAST
	if (mode == INT_INTERRUPT) {
		if (un->un_flags & INT_TAPE) { /* are we a tape? */
			switch (un->un_command) {
	  		case C0_REWIND:
				tv = HZ * TIME_REWIND;
				break;
	  		case C0_READ:
	  		case C0_WRITE:
			/*
			 * Normal read/write commands take almost zero time.
		 	 * However, if they are operating on a bad spot in
			 * the tape, they can take a long time.  This
			 * time is unmeasured, thus it is a guess.
			 */
				tv = HZ * TIME_RDWR;
				break;
		  	case C0_SPACE:
				tv = HZ * TIME_FSF;
				break;
		  	default:
				tv = HZ * (3*60);
				break;
			}
		} else tv = HZ * (30); /* disk drive */
		t = splclock();
		/* start timeout */
		ASSERT(un->un_timeid == 0);
		un->un_timeid = timeout(dkin_timeout, un, tv);
		splx(t);
	}
#ifndef FAST
	timesup = LOOPS;
	while((INT_STATUS(ctlr) & HOST_A_RDY) == 0) { /* ready for iopb? */
		if (--timesup == 0) {
			printf("in%d: never ready for iopb\n",unit);
			splx(s);
			return 1;
		}
	}
#else  FAST
	if (mode == INT_INTERRUPT)
		dp->b_active = 3; /* drive is busy (iopb submitted) */
#endif FAST
	un->un_flags |= INT_BUSY;
	INT_START0(ctlr,(int)((addr>>16)&0xFFFF)); wbflush();
#ifdef INTROL_DEBUG
	s1 = *((unsigned short*)dkin_softc[ctlr].c_io);
#endif INTROL_DEBUG
	INT_START1(ctlr,(int)(addr&0xFFFF));
#ifdef INTROL_DEBUG
	s2 = *((unsigned short*)dkin_softc[ctlr].c_io+1);
	if (((s1<<16)|s2) != addr)
		printf("(s/b %x was %x %x)",addr,s1,s2);
/*
		*((unsigned short*)dkin_softc[ctlr].c_io+4) = 0xcaca;
		printf("(s/b %x was %x %x)",addr,s1,s2);
	} else 
		*((unsigned short*)dkin_softc[ctlr].c_io+8) = 0xbeef;
 */
#endif INTROL_DEBUG
	if (wait) {	/* tape only */
		/* wait for command completion */
		while (un->un_flags & INT_BUSY) {
			un->un_flags |= INT_WAITING;
			(void) sleep((caddr_t) un, PRIBIO);
		}
	}
	splx(s);
	if (mode == INT_INTERRUPT)
		return 0;
#ifdef DEBUGPOLL
	s1 = *((unsigned short*)dkin_softc[ctlr].c_io);
	s2 = *((unsigned short*)dkin_softc[ctlr].c_io+1);
	d = INT_STATUS(ctlr);
	printf("&iopb= 0x%x%x hostrdy= 0x%x\n",s1,s2,d);
#endif DEBUGPOLL
	timesup = LOOPS;
	while (ip->in_adstatus == 0xAA && --timesup);
	if (ip->in_adstatus) {
		if (ip->in_adstatus != AS_SELTMO)
			printf("in%d: adapter status; %s\n", unit,
				(char *)printerr(ip->in_adstatus));
		return(ip->in_adstatus);
	}
	if (ip->in_scstatus) {
		if (ip->in_scstatus != CHECK_CONDITION)
			printf("in%d: scsi status; %s\n", unit,
				(char *)printerr(ip->in_scstatus));
		return(ip->in_scstatus);
	}
	return 0;
}
/*
 * return partition size in 'blocks'
 */
dkinsize(dev)
dev_t dev;
{
	register int unit = UNIT(dev);
	register int ctlr = CTLR(dev);
	struct int_unit *un;

	if ((unit >= NDKIN) || (ctlr > NDKINC))
		return (-1);
	un = &dkin_softc[ctlr].c_un[unit];
	if (un->un_vhvalid == 0)
		return (-1);
	return (un->un_vh.vh_pt[FS(dev)].pt_nblks);
}

xpr_dump_iopb(ctlr, unit)
{
	register struct int_iopb *ip;
	register struct int_unit *un;
	register int i, j;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	un = (struct int_unit *)&dkin_softc[ctlr].c_un[unit];
	j=(int)((ip->in_cnth<<16)|(ip->in_cntm<<8)|(ip->in_cntl));
	XPRINTF(XPR_DISK,"IOPB: Task=%x BuF=%x%x Cnt=%d",ip->in_task,
				ip->in_bufh,ip->in_bufl,j);
	XPRINTF(XPR_DISK," Adap=%x Scsi=%x",
				ip->in_adstatus,ip->in_scstatus,0,0);
	XPRINTF(XPR_DISK," AdCntrl=%x Ipl=%d Statid(vec)=%x",
		ip->in_adctl, ip->in_intlv, ip->in_statid,0);
	XPRINTF(XPR_DISK,"BRst=%x BK=%x AM=%x TAR=%x",
		ip->in_burst, ip->in_break,ip->in_addmod,ip->in_target);
	XPRINTF(XPR_DISK," CD(%x %x %x ",
		ip->incd_b0, ip->incd_b1, ip->incd_b2,0);
	if (un->un_flags & INT_TAPE) { /* are we a tape? */
		j=(int)((ip->incd_b2<<16)|(ip->incd_b3<<8)|(ip->incd_b4));
		XPRINTF(XPR_DISK,"%x %x %x (%d)blks",
	 		ip->incd_b3, ip->incd_b4, ip->incd_b5,j);
	} else {
		j=(int)((ip->incd_b1<<16)|(ip->incd_b2<<8)|(ip->incd_b3));
		XPRINTF(XPR_DISK,"%x %x %x (%d)lba",
	 		ip->incd_b3, ip->incd_b4, ip->incd_b5,j);
	}
	if (ip->incd_b0 & CD10BYTE) {
		XPRINTF(XPR_DISK," %x %x %x %x)\n",
			ip->incd_b6, ip->incd_b7, ip->incd_b8, ip->incd_b9);
	}
}
dump_iopb(ctlr, unit)
{
	register struct int_iopb *ip;
	register struct int_unit *un;
	register int i, j;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	un = (struct int_unit *)&dkin_softc[ctlr].c_un[unit];
	j=(int)((ip->in_cnth<<16)|(ip->in_cntm<<8)|(ip->in_cntl));
	printf("IOPB: Task=%x BuF=%x%x Cnt=%d Adap=%x Scsi=%x",ip->in_task,
		ip->in_bufh,ip->in_bufl,j,ip->in_adstatus,ip->in_scstatus);
	printf(" AdCntrl=%x Ipl=%d Statid(vec)=%x\nBRst=%x BK=%x AM=%x TAR=%x",
		ip->in_adctl, ip->in_intlv, ip->in_statid,
		ip->in_burst, ip->in_break,ip->in_addmod,ip->in_target);
	printf(" CD(%x %x %x %x %x %x",
		ip->incd_b0, ip->incd_b1, ip->incd_b2,
	 	ip->incd_b3, ip->incd_b4, ip->incd_b5);
	if (un->un_flags & INT_TAPE) { /* are we a tape? */
		j=(int)((ip->incd_b2<<16)|(ip->incd_b3<<8)|(ip->incd_b4));
		printf(" (%d)blks",j);
	} else {
		j=(int)((ip->incd_b1<<16)|(ip->incd_b2<<8)|(ip->incd_b3));
		printf(" (%d)lba",j);
	}
	if (ip->incd_b0 & CD10BYTE)
		printf(" %x %x %x %x)\n",
			ip->incd_b6, ip->incd_b7, ip->incd_b8, ip->incd_b9);
	else
		printf(")\n");
}
dkindump()
{}
