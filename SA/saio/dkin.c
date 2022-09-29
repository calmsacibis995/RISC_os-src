#ident "$Header: dkin.c,v 1.7 90/01/16 16:04:57 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * dkscsi.c -- Introl VME 300 standalone disk driver
 */
#undef DEBUG

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "machine/cpu.h"
#include "mips/dkinreg.h"
#include "mipsvme/vmereg.h"
#include "machine/dvh.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

/*
 * Standard address for Introl 300.
 * Address should be given relative to A16 Supervisor address space.
 */
static char *instd[] = {
	(char *)0x3000
};

#define	NUNITS		7	/* max drives per controller */
#define	NCTLRS		(sizeof(instd)/sizeof(instd[0]))

#define STDDELAY	3600	/* standard cmd delay */
#define FMTDELAY	3600	/* format delay in seconds */
#define SPINUP		60	/* ~number of seconds to allow for spin up */	

static struct volume_header invh[NCTLRS][NUNITS];
static int invhbuf[DEV_BSIZE/sizeof(int)];
/*
 * Per controller information
 */
struct	dkin_softc {
	char   *c_io;		/* ptr to Introl I/O space */
	struct int_unit {
		struct  int_iopb	un_iopb;  /* IOPB */
		struct  int_iopb*	un_iopbp; /* IOPB pointer */
		u_char	un_sense[16];	/* Last Request Sense bytes */
		struct inquiry	un_inq;	/* Last Inquiry Command bytes */
		struct err_page	un_err;	/* Mode select error page */
		int 	type;
	} c_un[NUNITS];
} dkin_softc[NCTLRS];

#define VALID		0x80	/* indicates the information data is valid */
/* Sense Keys */
#define NO_SENSE	0x00	/* FM, EOM or status unavailavble */
#define REC_ERR		0x01	/* command completed with recovery actions */
#define NOT_RDY		0x02	/* Tape drive can't be accessed */
#define MEDIUM_ERR	0x03	/* Non-recoverable data error */
#define HW_ERR		0x04	/* non-recoverable hdw failure (parity, etc) */
#define ILL_REQ		0x05	/* cmd block contains illegal parameter */
#define UNIT_ATN	0x06	/* cartridge change or tape drive was reset */
#define CMD_ABORT	0x0b	/* tape drive aborted the command */
#define VOL_OVFLOW	0x0d	/* physical EOM with data still in buffer */
#define KEYMASK		0x0f

struct sense_keyd {
	unsigned char	key;
	char *key_def;
} sense_keysd[] = {
	{ REC_ERR, "command completed with recovery actions" },
	{ NOT_RDY, "drive can't be accessed" },
	{ MEDIUM_ERR, "Non-recoverable data error" },
	{ HW_ERR, "non-recoverable hardware failure (parity, etc)" },
	{ ILL_REQ, "command descripter block contains illegal parameter" },
	{ UNIT_ATN, "media change or drive was reset" },
	{ CMD_ABORT, "drive aborted the command" },
	{ VOL_OVFLOW, "physical EOM reached with data still in buffer" },
	{ 0, "status code not in table" }
};
char *
printkeyd(type)
	register unsigned char type;
{
	register struct sense_keyd *keys = sense_keysd;
	register hold;

	while (hold = keys->key) {
		if (type == hold)
			return (keys->key_def);
		keys++;
	}
	return (keys->key_def);
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

/* how we talk to the Introl board */
#define INT_STATUS(ctlr)      *((unsigned char*)(dkin_softc[ctlr].c_io+0x201))
#define INT_START0(ctlr,x)    *((unsigned short*)dkin_softc[ctlr].c_io) = x
#define INT_START1(ctlr,x)    *((unsigned short*)dkin_softc[ctlr].c_io+1) = x
/*
 * _dkininit -- initialize driver global data
 */
_dkininit()
{
	bzero(invh, sizeof(invh));
}

/*
 * _dkinopen -- initialize introl 300 controller, read in volume header
 */
_dkinopen(io)
register struct iob *io;
{
	register struct int_unit *un;
	register struct int_iopb *in;
	register struct volume_header *inv;
	register struct device_parameters *dp;
	register unit = io->i_unit;
	register ctlr = io->i_ctlr;
	int tun = 0, flag = 0;
	int count, status, i;
	u_int addr;

	/*
	 * verify controller, unit, and partition numbers
	 */
	if (unit >= NUNITS) {
		printf("dkscsi(%d,%d,%d): bad unit\n",ctlr,unit,io->i_part);
		goto badio;
	}
	if (ctlr >= (int)NCTLRS) {
		printf("dkscsi(%d,%d,%d): bad controller number\n",
						ctlr,unit,io->i_part);
		goto badio;
	}
	if (io->i_part < 0 || io->i_part >= NPARTAB) {
	       printf("dkscsi(%d,%d,%d): bad partition\n",ctlr,unit,io->i_part);
	       goto badio;
	}
	dkin_softc[ctlr].c_io = (char *)
	    PHYS_TO_K1(VMESA16_TO_PHYS(instd[io->i_ctlr]));

	if (badaddr(dkin_softc[ctlr].c_io, sizeof(dkin_softc[ctlr].c_io))) {
		printf("no Introl controller at 0x%x\n", dkin_softc[ctlr].c_io);
		goto badio;
	}
	/* stash address in iob for future use
	 */
	io->i_devaddr = (unsigned)dkin_softc[ctlr].c_io;

	inv = &invh[ctlr][unit];
	un = &dkin_softc[ctlr].c_un[unit];
	in =(struct int_iopb*)(&dkin_softc[ctlr].c_un[unit].un_iopb);
	dkin_softc[ctlr].c_un[unit].un_iopbp = (struct int_iopb*)K0_TO_K1(in);

	/* wait for this drive to become ready */
	count = SPINUP;
again:
	addr = K1_TO_PHYS(&un->un_inq);
	setiopb(C0_INQUIRY,ctlr,unit,addr,0,0,sizeof(struct inquiry));
	if (status = startit(ctlr, unit, 0)) {
		if (status == AS_SELTMO) { /* Adaptor selection timeout */
			if (count--) {
				if (!flag) {
					printf("in%d: spinning up",unit);
					flag = 1;
				} else printf(".");
				/* bounce the LEDS and scan for abort */
				_scandevs();
				DELAY(800000);	/* ~.8 second delay */
				goto again;
			} else {
				if (flag) printf("\n");
				printf("dkscsi(%d,%d,%d): status; %s\n", ctlr,
				      unit,io->i_part,(char *)printerr(status));
				goto badio;
			}
		} else {
			printf("dkscsi(%d,%d,%d): failed inquiry\n", ctlr,
				      			unit,io->i_part);
			goto stat;
		}
	}
	if (flag) printf("\n");
	printf("vendor id is '");
	for (i=0; i < 8; i++)
		printf("%c", un->un_inq.inq_vendor[i]);
	printf("' product id is '");
	for (i=0; i < 16; i++)
		printf("%c", un->un_inq.inq_product[i]);
	printf("'\nand the revision level is '");
	for (i=0; i < 4; i++)
		printf("%c", un->un_inq.inq_rev[i]);
	printf("'");
	switch (un->un_inq.inq_type) {
		case TYPE_DISK:
			un->type = INT_DISK;
			if (un->un_inq.inq_qual & REMOVE_MEDIA) { 
				un->type |= INT_RMV_MEDIA;
				printf("; device is a floppy disk drive\n");
				printf("or a removable media hard disk\n");
			} else 
				printf("\n");
			break;
		case TYPE_TAPE:
			un->type = INT_TAPE;
			printf("; device is a tape drive not a disk\n");
			goto badio;
		case TYPE_WORM:
			un->type = INT_WORM;
			printf("; device is a worm drive\n");
			break;
		case TYPE_RONLY_DISK:
			un->type = (INT_WORM|INT_READONLY);
			printf("; device is a worm drive (read-only)\n");
			break;
		case TYPE_PRINTER:
			un->type = INT_PRINTER;
			printf("; device is a printer\n");
		case TYPE_CPU:
		case TYPE_LUN_GONE:
		default:
			printf("; device not supported\n");
			goto badio;
	}
	count = SPINUP;
retryit:
	setiopb(C0_TESTRDY,ctlr,unit,0,0,0,0);
	status = startit(ctlr, unit, 0);
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&un->un_sense[0]);
		setiopb(C0_REQSENSE,ctlr,unit,addr,0,0,16);
		if (startit(ctlr, unit, 0)) {
			printf("dkscsi(%d,%d,%d): request sense ERROR\n",
						ctlr, unit, io->i_part);
			goto badio;	/* no need to retry */
		} else if (un->un_sense[2] & UNIT_ATN) {
			if (!tun) {
#ifdef DEBUG
				printf("\nunit attention\n");
#endif DEBUG
				tun++;
			} else goto badio;
		} else if (un->un_sense[2] & NOT_RDY) {
			if (!flag) {
				printf("spinning up");
				flag = 1;
			} else printf(".");
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
			if (i = (un->un_sense[2] & KEYMASK))
			       printf("SENSE key %d; %s\n",i,printkey(i));
			if (i = un->un_sense[12]) {
			       printf("error code 0x%x(%d); %s\n",
						i,i,print_errcode(i));
			}
			goto badio;
		}
		if (count--) {
			_scandevs(); /* bounce the LEDS and scan for abort */
			DELAY(800000);	/* ~.8 second delay */
			goto retryit;
		}
		goto badio;
	} else if (status) {
		if (flag) printf("\n");
		printf("dkscsi(%d,%d,%d): failed open; %s\n",ctlr, unit,
					io->i_part,(char *)printerr(status));
		goto badio;
	}
	if (flag) printf("\n");
	/*
	 * Return if already configured
	 */
	if (inv->vh_magic == VHMAGIC)
		goto checkunit;
	/*
	 * read lba 0 looking for a valid volume header
	 */
	((struct volume_header *)invhbuf)->vh_magic = 0;
	addr = K1_TO_PHYS(invhbuf);
	setiopb(C0_READ,ctlr,unit,addr,1,0,0);
	status = startit(ctlr, unit, 0);
	clear_cache(invhbuf, DEV_BSIZE);
	if (status) {
		printf("dscsi(%d,%d,%d): volume header read failed\n",
	    		io->i_ctlr, io->i_unit, io->i_part);
		goto stat;
	}
	if (!(is_vh((struct volume_header *)invhbuf))) {
		printf("dscsi(%d,%d,%d): can't read volume header\n",
	    		io->i_ctlr, io->i_unit, io->i_part);
		/*
	 	* NOTE: this must return successful so formatters can
	 	* operate on raw disks
		*/
		return (0);
	}
	*inv = *(struct volume_header *)invhbuf;
checkunit:
	if (inv->vh_pt[io->i_part].pt_nblks == 0) {
	       printf("dkscsi(%d,%d,%d): bad partition\n",ctlr,unit,io->i_part);
		goto badio;
	}
	/*
	 * Try and figure out what type of file system is out there
	 */
	if (io->i_fstype == DTFS_AUTO)
		io->i_fstype = vh_mapfstype(inv->vh_pt[io->i_part].pt_type);
	return (0);
stat:
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&un->un_sense[0]);
		setiopb(C0_REQSENSE,ctlr,unit,addr,0,0,16);
		if (status = startit(ctlr, unit, 0)) {
			printf("dkscsi(%d,%d,%d): %s failed req sense\n",ctlr,
				unit,io->i_part,(char *)printerr(status));
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
			if (i = (un->un_sense[2] & KEYMASK)) {
				printf("SENSE key %d; %s\n",
						i,printkey(i));
			}
			if (un->un_sense[0] & VALID) {
				addr =(int)((un->un_sense[3]<<24)|
				    (un->un_sense[4]<<16)|
				    (un->un_sense[5]<< 8)|
				    (un->un_sense[6]));
			printf("physical block address 0x%x (%d)\n", addr,addr);
			}
			if (i = un->un_sense[12]) {
			       printf("error code 0x%x(%d); %s\n",
						i,i,print_errcode(i));
			}
		}
	} else
		printf("dkscsi(%d,%d,%d): %s\n",ctlr,
				unit,io->i_part,(char *)printerr(status));
badio:
	io->i_errno = ENXIO;
	return (-1);
}

/*
 * _dkipstrategy -- perform io
 */
_dkinstrategy(io, func)
register struct iob *io;
register int func;
{
	register struct partition_table *pt;
	register unit = io->i_unit;
	register ctlr = io->i_ctlr;
	register unsigned lbn, addr;
	struct int_unit *isu;
	int cmd, blks, status, i;

	if ((func == READ) || (func == WRITE)) {
		pt = &invh[io->i_ctlr][unit].vh_pt[io->i_part];
		if ((unsigned)io->i_bn > pt->pt_nblks) {
			printf("read beyond end of partition\n");
			io->i_errno = ENXIO;
			return (-1);
		}
		lbn = io->i_bn + pt->pt_firstlbn;
	}
	switch (func) {
	case READ:
		if (io->i_cc % DEV_BSIZE) {
			printf("cc not multiple of sector size\n");
			io->i_errno = EIO;
			return (-1);
		}
		blks = io->i_cc / DEV_BSIZE;
		addr = K1_TO_PHYS(io->i_ma);
		setiopb(C0_READ,ctlr,unit,addr,blks,lbn,0);
		status = startit(ctlr, unit, 0);
		clear_cache(io->i_ma, io->i_cc);
		if (status)
			goto stat;
		break;

	case WRITE:
		if (io->i_cc % DEV_BSIZE) {
			printf("cc not multiple of sector size\n");
			io->i_errno = EIO;
			return (-1);
		}
		blks = io->i_cc / DEV_BSIZE;
		addr = K1_TO_PHYS(io->i_ma);
		setiopb(C0_WRITE,ctlr,unit,addr,blks,lbn,0);
		if (status = startit(ctlr, unit, 0))
			goto stat;
		break;

	default:
		_io_abort("dkscsi bad function");
	}
	return (io->i_cc);
stat:
	isu = &dkin_softc[ctlr].c_un[unit];
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&isu->un_sense[0]);
		setiopb(C0_REQSENSE,ctlr,unit,addr,0,0,16);
		if (!(startit(ctlr, unit, 0))) {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",isu->un_sense[i]);
			printf("\n");
			if (isu->un_sense[0] & VALID) {
				addr =(int)((isu->un_sense[3]<<24)|
				    (isu->un_sense[4]<<16)|
				    (isu->un_sense[5]<< 8)|
				    (isu->un_sense[6]));
			printf("physical block address 0x%x (%d)\n", addr,addr);
			}
			if (i = (isu->un_sense[2] & KEYMASK))
				printf("SENSE key %d; %s\n",i,printkey(i));
			if (i = isu->un_sense[12])
			        printf("error code 0x%x(%d); %s\n",
						i,i,print_errcode(i));
		}
	} else {
		printf("dkscsi(%d,%d,%d): %s\n",ctlr,unit,io->i_part,
					(char *)printerr(status));
	}
	io->i_errno = EIO;
	return (-1);
}

/*
 * _dkinioctl -- io controls
 */
_dkinioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	register struct int_unit *un;
	register error = 0;
	register struct volume_header *inv;
	register struct fmt_map_info *fmi = (struct fmt_map_info*) arg;
	int status, i;
	u_int addr;

	un = &dkin_softc[io->i_ctlr].c_un[io->i_unit];
	inv = &invh[io->i_ctlr][io->i_unit];

	switch (cmd) {
		
	case DIOCGETVH:
		bcopy(inv, arg, sizeof(*inv));
		break;

	case DIOCSETVH:
		bcopy(arg, inv, sizeof(*inv));
		break;

	case DIOCFMTMAP:
		switch (fmi->fmi_action) {
		case FMI_FORMAT_TRACK:
		setiopb(C0_FORMAT,io->i_ctlr,io->i_unit,0,0,0,0);
		status = startit(io->i_ctlr, io->i_unit, 3600);
		if (status == CHECK_CONDITION) {
			addr = K1_TO_PHYS(&un->un_sense[0]);
			setiopb(C0_REQSENSE,io->i_ctlr,io->i_unit,addr,0,0,16);
			if (startit(io->i_ctlr, io->i_unit, 0)) {
			       printf("dkscsi(%d,%d,%d): request sense ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
			       return (-1);
			} else {
			       printf("dkscsi(%d,%d,%d): format ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
				printf("SENSE DATA: ");
				for (i=0; i < 16; i++)
					printf("%x ",un->un_sense[i]);
				printf("\n");
				if (i = (un->un_sense[2] & KEYMASK))
				     printf("SENSE key %d; %s\n",i,printkey(i));
				if (un->un_sense[0] & VALID) {
					addr =(int)((un->un_sense[3]<<24)|
					    (un->un_sense[4]<<16)|
					    (un->un_sense[5]<< 8)|
					    (un->un_sense[6]));
			printf("physical block address 0x%x (%d)\n",addr,addr);
				}
				if (i = un->un_sense[12])
			       		printf("error code %d; %s\n",
							i,print_errcode(i));
				return (-1);
			}
		} else if (status) {
			printf("dkscsi(%d,%d,%d): FORMAT failed; %s\n",
					io->i_ctlr,io->i_unit,
					io->i_part,(char *)printerr(status));
			return (-1);
		}
		break;
		case FMI_MAP_TRACK: /* reassign blocks */
		addr = K1_TO_PHYS(fmi->fmi_addr);
		setiopb(C0_REASSIGN,io->i_ctlr,io->i_unit,addr,0,0,fmi->fmi_cyl);
		status = startit(io->i_ctlr, io->i_unit, fmi->fmi_cyl);
		if (status == CHECK_CONDITION) {
			addr = K1_TO_PHYS(&un->un_sense[0]);
			setiopb(C0_REQSENSE,io->i_ctlr,io->i_unit,addr,0,0,16);
			if (startit(io->i_ctlr, io->i_unit, 0)) {
			       printf("dkscsi(%d,%d,%d): request sense ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
			       return (-1);
			} else {
			       printf("dkscsi(%d,%d,%d): reassign blocks ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
				printf("SENSE DATA: ");
				for (i=0; i < 16; i++)
					printf("%x ",un->un_sense[i]);
				printf("\n");
				if (i = (un->un_sense[2] & KEYMASK))
				     printf("SENSE key %d; %s\n",i,printkey(i));
				if (un->un_sense[0] & VALID) {
					addr =(int)((un->un_sense[3]<<24)|
					    (un->un_sense[4]<<16)|
					    (un->un_sense[5]<< 8)|
					    (un->un_sense[6]));
			printf("physical block address 0x%x (%d)\n",addr,addr);
				}
				if (i = un->un_sense[12])
			       		printf("error code 0x%x(%d); %s\n",
						i,i,print_errcode(i));
				return (-1);
			}
		} else if (status) {
			printf("dkscsi(%d,%d,%d): REASSIGN blocks failed; %s\n",
					io->i_ctlr,io->i_unit,
					io->i_part,(char *)printerr(status));
			return (-1);
		}
		break;
		default:
			io->i_errno = EINVAL;
			error = -1;
			break;
		}
		break;

	case DIOCNOECC:
		un->un_err.err_pagecode = 1;
		un->un_err.err_pagelength = 6;
		if (*(int *)arg) un->un_err.err_flags = DCR|PER;
		else un->un_err.err_flags = PER;
		un->un_err.err_reserved0 = 0x8; /* retry count */
		addr = K1_TO_PHYS(&un->un_err);
		setiopb(C0_MODESEL,io->i_ctlr,io->i_unit,addr,0,0,
						sizeof(struct err_page));
		status = startit(io->i_ctlr, io->i_unit, 0);
		if (status == CHECK_CONDITION) {
			addr = K1_TO_PHYS(&un->un_sense[0]);
			setiopb(C0_REQSENSE,io->i_ctlr,io->i_unit,addr,0,0,16);
			if (startit(io->i_ctlr, io->i_unit, 0)) {
			       printf("dkscsi(%d,%d,%d): request sense ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
			       return (-1);
			} else {
			       printf("dkscsi(%d,%d,%d): mode select ERROR\n",
					io->i_ctlr, io->i_unit, io->i_part);
				printf("SENSE DATA: ");
				for (i=0; i < 16; i++)
					printf("%x ",un->un_sense[i]);
				printf("\n");
				if (i = (un->un_sense[2] & KEYMASK))
				     printf("SENSE key %d; %s\n",i,printkey(i));
				if (i = un->un_sense[12])
			       		printf("error code 0x%x(%d); %s\n",
							i,i,print_errcode(i));
				return (-1);
			}
		} else if (status) {
			printf("dkscsi(%d,%d,%d): DIOCNOECC failed; %s\n",
					io->i_ctlr,io->i_unit,
					io->i_part,(char *)printerr(status));
			return (-1);
		}
		break;

	case DIOCVFYSEC:
	case DIOCDIAG:
	case DIOCRDEFECTS:
	default:
		io->i_errno = EINVAL;
		error = -1;
		break;
	}
	return (error);
}
/*
** setiopb() - Routine to set up the IOPB for the Introl Board.
*/
setiopb(cmd, ctlr, unit, addr, blkcount, lba, bcount)
{
	register struct int_iopb *ip;
	register int c;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	ip->in_addmod = VME_A32NPAMOD; /* 0x09 32-bit non-Supr Access */
	ip->in_adctl  = AC_BURST|AC_WIDTH32|AC_IOPBPRI;
	ip->in_burst  = BURSTVALUE;	/* Data Throttle Burst */
	ip->in_break  = BURSTBREAK;	/* Data Throttle Break */
	ip->in_bufh   = ((u_int)addr>>16) & 0xffff;	/* Data Buffer */
	ip->in_bufl   = ((u_int)addr) & 0xffff;		/* Data Buffer */
	if (!blkcount)
		c = bcount/4;
	else
		c = blkcount*(DEV_BSIZE/4);
	ip->in_cnth = HB(c);		/* Transfer Count */
	ip->in_cntm = MB(c);		/* Transfer Count */
	ip->in_cntl = LB(c);		/* Transfer Count */
	ip->in_adstatus = 0xAA;		/* Adapter status (for POLLING) */
	ip->in_intlv = 0;		/* forces POLLED operation */
	ip->in_scstatus = 0;		/* SCSI status */
	ip->in_target = unit;		/* Target ID */
	/*
	** SCSI Command Descriptor Bytes in the IOPB
	*/
	ip->incd_b0 = cmd;		/* SCSI Command */
	if (cmd == C0_READ || cmd == C0_WRITE) {
		ip->incd_b1 = (0<<5)|HB(lba);
		ip->incd_b2 = MB(lba);
		ip->incd_b3 = LB(lba);
		ip->incd_b4 = blkcount;
		return;
	}
	ip->incd_b1 = (0<<5);
	ip->incd_b2 = 0;
	ip->incd_b3 = 0;
	ip->incd_b4 = 0;
	if (cmd == C0_FORMAT) {
		ip->incd_b4 = 1; /* interleave is always 1 */
	} else if (cmd != C0_REASSIGN) {
		ip->incd_b4 = bcount;  /* byte count */
		ip->incd_b5 = 0;
	}
	if (cmd == C0_MODESEL)
		ip->incd_b1 |= PAGE_FORMAT;
}
/*
** fire off command
*  if delay is non-zero it indicates the number of seconds to delay
*/
startit(ctlr, unit, delay)
	register ctlr, unit, delay;
{
	register struct int_iopb *ip;
	register int timesup;
	u_int addr, tv, s;
#ifdef DEBUG
	u_short s1, s2;
	u_char c, d;
	dumpiopb(ctlr,unit);
#endif DEBUG
	if (delay) timesup = delay; /* used when long delays are needed */
	else timesup = STDDELAY;
	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	addr = K1_TO_PHYS(&ip->in_task);	/* iopb address */
	/*
	 * TODO: add timeout code here
	 */
	while((INT_STATUS(ctlr) & HOST_A_RDY) == 0);/* will Introl take iopb? */
	INT_START0(ctlr,(int)((addr>>16)&0xFFFF)); wbflush();
	INT_START1(ctlr,(int)(addr&0xFFFF));
#ifdef DEBUG
	s1 = *((unsigned short*)dkin_softc[ctlr].c_io);
	s2 = *((unsigned short*)dkin_softc[ctlr].c_io+1);
	d = INT_STATUS(ctlr);
	printf("&iopb= 0x%x%x hostrdy= 0x%x\n",s1,s2,d);
#endif DEBUG
	while (ip->in_adstatus == 0xAA && --timesup) {
		if (delay) {
			/* if we're formatting show we're alive */
			if (delay==FMTDELAY) printf(".");
			DELAY(1000000); /* ~1 second delay */
		}
		_scandevs();	/* bounce the LEDS and look for console abort */
	}
	if (delay==FMTDELAY) printf("\n");
	if (!timesup)
		printf("dkscsi: command timeout occurred\n");
	if (ip->in_adstatus) {
#ifdef DEBUG
		printf("dkscsi%d: Adapter ERROR status: %s\n", unit,
			(char *)printerr(ip->in_adstatus));
#endif DEBUG
		return(ip->in_adstatus);
	}
	if (ip->in_scstatus) {
#ifdef DEBUG
		printf("dkscsi%d: Scsi ERROR status: %s\n", unit,
			(char *)printerr(ip->in_scstatus));
#endif DEBUG
		return(ip->in_scstatus);
	}
	return 0;
}
dumpiopb(ctlr, unit)
{
	register struct int_iopb *ip;
	register int i, j;

	ip = dkin_softc[ctlr].c_un[unit].un_iopbp;
	j=(int)((ip->in_cnth<<16)|(ip->in_cntm<<8)|(ip->in_cntl));
	printf("IOPB: Task=%x DMAaddr=%x%x Cnt=%d Adap=%x Scsi=%x",ip->in_task,
		ip->in_bufh,ip->in_bufl,j,ip->in_adstatus,ip->in_scstatus);
	printf(" AdCntrl=%x Ipl=%d Statid(vec)=%x\nBRst=%x BK=%x AM=%x TAR=%x",
		ip->in_adctl, ip->in_intlv, ip->in_statid,
		ip->in_burst, ip->in_break,ip->in_addmod,ip->in_target);
	printf(" CD(%x %x %x %x %x %x",
		ip->incd_b0, ip->incd_b1, ip->incd_b2,
	 	ip->incd_b3, ip->incd_b4, ip->incd_b5);
	j=(int)((ip->incd_b1<<16)|(ip->incd_b2<<8)|(ip->incd_b3));
	printf(" (%d)lba",j);
	if (ip->incd_b0 & CD10BYTE)
		printf(" %x %x %x %x)\n",
			ip->incd_b6, ip->incd_b7, ip->incd_b8, ip->incd_b9);
	else
		printf(")\n");
}
