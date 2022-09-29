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
#ident	"$Header: common_scsi.c,v 1.14.1.24.1.8.1.15 91/01/30 16:38:29 beacker Exp $"

/*
** Common System V Disk and Tape SCSI driver
**
** TODO:
** + implement residual handling for space ioctl's
** + for efficiency check out NOT loading the chain address
**   since the s/g entries are contiguous (chain address reg will be correct)
*/ 

#ifdef STANDALONE
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/buf.h"
#include "mips/dvh.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "mips/scsi.h"
#include "sys/iobuf.h"
#include "sys/cmn_err.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "sys/mtio.h"		/* needed for "mt" command ioctl stuff */
#else STANDALONE
#include "sys/sbd.h"
#include "sys/debug.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/dvh.h"
#include "sys/scsi.h"
#include "sys/iobuf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/elog.h"
#include "sys/file.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/dump.h"
#include "sys/dkio.h"
#include "sys/ioctl.h"
#include "sys/gen_ioctl.h"
#include "sys/cpu_board.h"
#include "sys/mtio.h"		/* needed for "mt" command ioctl stuff */
#include "sys/open.h"
#include "sys/kmem.h"
#endif STANDALONE

int in_kmq = 0;

#define		b_cylin		b_resid
#define		b_cmd		b_resid
#define		B_SPL		B_FORMAT

#define		b_length	b_bufsize
#define		PBLKNO		((SCSI_BUF_EXT *)bp->av_back)->pblkno
#define		RESIDUAL	((SCSI_BUF_EXT *)bp->av_back)->resid
#define		ALGNPTR		((SCSI_BUF_EXT *)bp->av_back)->alptr
#define		N_PBLKNO	((SCSI_BUF_EXT *)nbp->av_back)->pblkno
#define		N_RESIDUAL	((SCSI_BUF_EXT *)nbp->av_back)->resid
#define		N_ALGNPTR	((SCSI_BUF_EXT *)nbp->av_back)->alptr
#define		current_bp	dp->io_s1

#define		PTNUM_VOLUME	10
extern int machine_type;

#ifdef STANDALONE

#define suser()				1

char *common_scsi_tapebuf;
int scsiexterr;
int showconfig;

#define SET_ERROR(x)	io->i_errno = (x)
#define ERR_RETURN(x) \
	{ \
		io->i_errno = (x); \
		return(-1); \
	}
#else STANDALONE
int spltty();
struct scsi_unit common_scsi_save_un;
static int un_saved = 0;

extern int scsiexterr;		/* extended error print flag - lbootable */
extern int showconfig;
extern int scsi_id;
extern int scsi_plock;

#define SET_ERROR(x)	u.u_error = (x)
#define ERR_RETURN(x) \
	{ \
		u.u_error = (x); \
		return; \
	}
#endif STANDALONE

void common_scsi_string();

/*
 * Local routines
 */
static void queue_request();
static void unqueue_request();
static void put_bext();
static SCSI_BUF_EXT *get_bext();
static void put_kmq(),wakeup_kmq();
static void sortit();
static int usepc8();
static int tapeopen();
static int common_scsi_format();
static void common_scsi_start();
static void tapeioctl();
static void tapeclose();
static void common_scsi_command();
static void common_scsi_iopbset();
static void common_scsi_sgsetup();
static void setupiopb();
static char *printkey();
static char *print_errcode();
static void add_motor_start();
static void motortimeout();
static void busytimeout();
static int readvh();
static int doscsicmd();
static int wait_ready();

#define MAXDEV		128	/* not defined anywhere */

SCSI_BUF_EXT	*scsi_buf_ext_hdr = 0;
SCSI_BUF_EXT	scsi_buf_ext[SCSI_MAX_BUF_EXT];

struct iobuf	scsi_noproc,scsi_kmq;
int kmq_timer_id = 0;

static struct low_scsi *common_scsi_map[MAXDEV];

#ifndef STANDALONE
static int Tapedensity[] = {
    QIC_DFLT_CODE,		/* Default Implicite density */
    QIC_24_CODE,		/* QIC-24 density code field*/
    QIC_120_CODE,		/* QIC-120 density code field */
    QIC_150_CODE		/* QIC-150 density code field */
};
#endif STANDALONE

int common_scsi_powerup = 180;	/* 3 min max wait in open */

struct motorstart {
	struct scsi_unit *active;
	struct scsi_unit *next;
} motor_start_list;

#define al_uaddr		al_info.ali_uaddr
#define al_size			al_info.ali_size
#define al_tbuff		al_info.ali_tbuff

typedef struct inq_fields {
	char *dp;
	int  offset;
	int  size;
}INQ_FIELDS;

INQ_FIELDS common_scsi_inq_fields[] = {
	{NULL,  3,  8},	/* vendor ID */
	{NULL, 11, 16},	/* product ID */
	{NULL, 27,  4},	/* revision level */
	{NULL, 31,  8},	/* microcode revision level */
	{NULL, 39, 12},	/* drive serial number */
	{NULL,  0, 16},	/* device type */
	{NULL,0,0}
};

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
	{ BLANK_CHK, "no-data condition encountered on tape or tape and drive are different formats" },
	{ CMD_ABORT, "drive aborted the command" },
	{ VOL_OVFLOW, "physical EOM reached with data still in buffer" },
	{ 0, "status code not in table" }
};
static struct int_errors {
	unsigned char	inerr_type;
	char *inerr_name;
} interrs[] = {
	/* SCSI Status */
	{ SCSI_CHECK, "Check Condition" },
	{ SCSI_MET, "Condition Met/Good" },
	{ SCSI_BUSY, "target is BUSY" },
	{ SCSI_INTER, "Intermediate/Good" },
	{ SCSI_COMBO, "Intermediate/Condition Met/Good" },
	{ SCSI_RESV, "Reservation Conflict" },
	/* Hardware Status */
	{ PARITY, "SCSI Parity Error Detected" },
	{ HWERROR, "Hardware Failure" },
	{ SCSITMO, "SCSI Timeout Error" },
	{ SELTMO, "Selection Timeout Error" },
	{ PHASE_ERROR, "SCSI Phase Error" },
	{ 0xAA, "POLLED time out" },
	{ 0, "status code not in table" }
};

static char *
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
scsiprinterr(type)
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
#define TRK_FOLLOW		0x9
#define ID_FLD_RD_ERR		0x10
#define DATA_FLD_RD_ERR		0x11
#define ID_FLD_NO_SYNC		0x12
#define DATA_FLD_NO_SYNC	0x13
#define BAD_BLK_FND		0x14
#define SEEK_ERR		0x15
#define REC_READ_NO_ECC		0x17
#define REC_READ_WITH_ECC	0x18
#define DEF_LST_ERR		0x19
#define PARAM_OVERRUN		0x1a
#define SYNC_XFER_ERR		0x1b
#define NO_P_DEF_LST		0x1c
#define CMP_ERR			0x1d
#define REC_ID_WITH_ECC		0x1e
#define INV_CMD_OP_CODE		0x20
#define ILL_LBA			0x21
#define ILL_FUNC		0x22
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
#define MSG_REJECTED		0x43
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
	{ TRK_FOLLOW, "track following error" },
	{ ID_FLD_RD_ERR, "unrecovered read error of ID field" },
	{ DATA_FLD_RD_ERR, "unrecovered read error of DATA field" },
	{ ID_FLD_NO_SYNC, "no sync byte found in ID field" },
	{ DATA_FLD_NO_SYNC, "no sync byte found in DATA field" },
	{ BAD_BLK_FND, "no record found or bad block found" },
	{ SEEK_ERR, "seek error" },
	{ REC_READ_NO_ECC, "recovered read data with re-reads (no ecc)" },
	{ REC_READ_WITH_ECC, "recovered read data with ecc correction" },
	{ DEF_LST_ERR, "defect list error" },
	{ PARAM_OVERRUN, "parameter overrun" },
	{ SYNC_XFER_ERR, "syncronous transfer error" },
	{ NO_P_DEF_LST, "primary defect list not found" },
	{ CMP_ERR, "compare error" },
	{ REC_ID_WITH_ECC, "recovered ID with ecc correction" },
	{ INV_CMD_OP_CODE, "invalid command operation code" },
	{ ILL_LBA, "illegal logical block address" },
	{ ILL_FUNC, "illegal function for device type" },
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
	{ MSG_REJECTED, "message reject error" },
	{ INT_CTLR_ERR, "internal controller error" },
	{ SEL_RESEL_FAILED, "select/reselect failed" },
	{ SCSI_PAR_ERROR, "scsi parity error" },
	{ INIT_DET_ERROR, "initiator detected error" },
	{ INAP_ILL_MSG, "inappropriate/illegal message" },
	{ 0, "error code not in table" }
};

static char *
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
/*
 * check for slave's existence
 */
int
common_scsi_slave(un,mode)
register struct scsi_unit *un;
int mode;
{
    register SCSI_EXT_SENSE *sense;
    register SCSI_INQUIRY *inq;
    u_char inq_string[IDENT_LEN];
    u_int status, i;
    int is_a_tape = 0, is_a_disk = 0;
    int align, temp, count, malloc_flag;

#ifndef STANDALONE
    if(un->un_target == scsi_id) {
	goto failed;
    }
    /* when called by open must allow to sleep waiting for memory */
    if (mode & POLLED)
	malloc_flag = M_NOWAIT | M_CONTIGUOUS;
    else
	malloc_flag = M_WAITOK | M_CONTIGUOUS;
#endif


    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inq);

    align = un->un_dmaalign;
    if ((IS_R3030 || IS_RB3125) && !un->un_tmpbuf) {
#ifdef STANDALONE
        un->un_tmpbuf = (u_char*)align_malloc(64,align);
        un->un_buf_64 = un->un_tmpbuf;
#else STANDALONE
	count = 64;
	un->un_tmpbuf = (u_char*)kmemalloc(count,0,malloc_flag);
	if (!un->un_tmpbuf) {
	    cmn_err(CE_PANIC,
		"SCSI %dL%d: Could not kmemalloc %d bytes for un_tmpbuf\n",
		un->un_target,un->un_lun,count);
	}
	temp = (align - 1) & (u_long)un->un_tmpbuf;
	if (temp) { /* aligned? */
	    kmemfree(un->un_tmpbuf,0,malloc_flag); /* free and alloc extra */
	    count = 64 + align;
	    un->un_tmpbuf = (u_char*)kmemalloc(count,0,malloc_flag);
	    if (!un->un_tmpbuf) {
		cmn_err(CE_PANIC,
		    "SCSI %dL%d: Could not kmemalloc %d bytes for un_tmpbuf\n",
		    un->un_target,un->un_lun,count);
	    }
	    temp = (align - 1) & (u_long)un->un_tmpbuf;
	    /* round up to address alignment */
	    un->un_buf_64 = un->un_tmpbuf + (align - temp);
	} else
	    un->un_buf_64 = un->un_tmpbuf;
#endif STANDALONE
    }
    if (!un->un_vh) {
#ifdef STANDALONE
        un->un_vh = (struct volume_header*)align_malloc(512,align);
        un->un_vh_k1ptr = (struct volume_header*)(K2_TO_K1(un->un_vh));
#else STANDALONE
	count = 512;
	un->un_vh = (struct volume_header*)kmemalloc(count,0,malloc_flag);
	if (!un->un_vh) {
	    cmn_err(CE_PANIC,
		"SCSI %dL%d: Could not kmemalloc %d bytes for un_vh\n",
		un->un_target,un->un_lun,count);
	}
	temp = (align - 1) & (u_long)un->un_vh;
	if (temp) { /* aligned? */
	    kmemfree(un->un_vh,0,malloc_flag); /* free and alloc extra */
	    count = 512 + align;
	    un->un_vh = (struct volume_header*)kmemalloc(count,0,malloc_flag);
	    if (!un->un_vh) {
		cmn_err(CE_PANIC,
		    "SCSI %dL%d: Could not kmemalloc %d bytes for un_vh\n",
		    un->un_target,un->un_lun,count);
	    }
	    temp = (align - 1) & (u_long)un->un_vh;
	    /* round up to address alignment */
	    un->un_vh += (align - temp);
	}
	un->un_vh_k1ptr = (struct volume_header*)(K2_TO_K1(un->un_vh));
#endif STANDALONE
    }
    un->un_sectsize = 512;
    status = common_scsi_spcmd(un,C0_INQUIRY,0,sizeof(SCSI_INQUIRY),inq,mode);
    if (status) {
	status=common_scsi_spcmd(un,C0_INQUIRY,0,sizeof(SCSI_INQUIRY),inq,mode);
	/* some drives don't return inquiry data if they're
	 * spinning up (as they should)
	 */
	if (status)
	    goto failed;
    }
    switch (inq->device_type) {
	case TYPE_M12_FLOPPY:
	    un->un_flags |= INT_FLOPPY;
	    un->un_flags |= INT_RMV_MEDIA;
	    break;
	case TYPE_DISK:
	    is_a_disk = 1;
	    un->un_flags |= INT_DISK;
	    if (inq->rm)
		un->un_flags |= INT_RMV_MEDIA;
	    break;
	case TYPE_TAPE:
	    is_a_tape = 1;
	    un->un_flags |= INT_TAPE;
	    break;
	case TYPE_COMM:
	    un->un_flags |= INT_COMM;
	    break;
	case TYPE_WORM:
	    un->un_flags |= INT_WORM;
	    break;
	case TYPE_RONLY_DISK:
	    un->un_flags |= (INT_WORM|INT_READONLY);
	    break;
	case TYPE_PRINTER:
	    un->un_flags |= INT_PRINTER;
	case TYPE_CPU:
	case TYPE_LUN_GONE:
	default:
	    if (showconfig)
		cmn_err(CE_CONT,"SCSI %dL%d: device not supported\n",
				 un->un_target, un->un_lun);
	    goto failed;
    }
    un->un_nretries = NRETRIES; /* keep it in common_scsi! */
    if (showconfig) {
	common_scsi_string(un,inq_string,mode);
	cmn_err(CE_CONT,"SCSI %dL%d: %s\n",
		un->un_target,un->un_lun,inq_string);
    }
    if (status = common_scsi_spcmd(un,C0_TESTRDY,0,0,0,mode)) {
	if (is_a_tape && ((status == SCSI_CHECK && sense->key == NOT_RDY) ||
		status == SCSI_BUSY))
	    goto success;
	else if (is_a_disk && status == SCSI_CHECK && sense->key == NOT_RDY) {
	    if ((status = common_scsi_spcmd(un,C0_TESTRDY,0,0,0,mode)) ==0)
		un->un_flags |= INT_READY;
	    else
		add_motor_start(un,mode);
	} else
	    goto failed;
    } else {
	un->un_flags |= INT_READY;
    }
success:
    un->un_flags |= INT_ALIVE;
    return 1; /* found a device */
failed:
#ifndef STANDALONE
    if (un->un_tmpbuf) {
	kmemfree(un->un_tmpbuf,0,malloc_flag);
        un->un_tmpbuf = un->un_buf_64 = (u_char *)NULL;
    }
    if (un->un_vh) {
	kmemfree(un->un_vh,0,malloc_flag);
        un->un_vh = un->un_vh_k1ptr = (struct volume_header *)NULL;
    }
#endif STANDALONE
    return 0;
}

static void
add_motor_start(un,mode)
register struct scsi_unit *un;
int mode;
{
#ifndef STANDALONE
    register struct scsi_unit *unp;
    register int s;

    s = splclock();
    if (!motor_start_list.active) {
	motor_start_list.active = un;
	splx(s);
	if (scsiexterr > 1)
	    cmn_err(CE_CONT,
		"SCSI %dL%d: Add_motor_start issuing motor start\n",
		un->un_target,un->un_lun);
	if (common_scsi_spcmd(un,C0_STARTSTOP,1,1,0,mode)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: Cannot Start Motor\n",
		    un->un_target,un->un_lun);
	    motor_start_list.active = NULL;
	} else {
	    (void)timeout_spl(motortimeout, 0, 10 * HZ, spltty);
	}
    } else if (un != motor_start_list.active) {
	if (scsiexterr > 1)
	    cmn_err(CE_CONT,"SCSI %dL%d: On motor_start list\n",
		un->un_target,un->un_lun);
	unp = (struct scsi_unit *)&motor_start_list;
	if (unp->un_motorstart) {
	    while (unp->un_motorstart) {
		if (unp->un_motorstart == un) {
		    unp->un_motorstart = un->un_motorstart;
		    un->un_motorstart = motor_start_list.next;
		    unp = (struct scsi_unit *)&motor_start_list;
		    break;
		}
		unp = unp->un_motorstart;
	    }
	}
	unp->un_motorstart = un;
	splx(s);
    }
#else STANDALONE
    if (common_scsi_spcmd(un,C0_STARTSTOP,0,1,0,POLLED)) {
        cmn_err(CE_CONT,"SCSI %dL%d: Cannot Start Motor\n",
                un->un_target,un->un_lun);
    } else {
        un->un_flags |= INT_READY;
    }
#endif STANDALONE
}

#ifndef STANDALONE
static void
busytimeout(un)
register struct scsi_unit *un;
{
    if (scsiexterr)
	cmn_err(CE_CONT,"SCSI %dL%d: busy retry on command 0x%x\n",
	    un->un_target,un->un_lun,un->un_command);
    (void)(*LOW_SCSI(un->un_dp->b_dev,startop))(un,INTERRUPT);
}

static void
motortimeout()
{
    register int s;
    register struct scsi_unit *un;

    if (!motor_start_list.active) {
	cmn_err(CE_CONT,"SCSI: Motor Start Timeout with no active unit\n");
    }
doagain:
    s = splclock();
    un = motor_start_list.next;
    motor_start_list.active = un;
    if (un)
	motor_start_list.next = un->un_motorstart;
    else
	motor_start_list.next = NULL;
    splx(s);
    if (un) {
	if (un->un_flags & INT_READY || un->un_dp->b_active)
	    goto doagain;
	if (scsiexterr > 1)
	    cmn_err(CE_CONT,"SCSI %dL%d: Motortimeout issuing motor start\n",
		un->un_target,un->un_lun);
	if (common_scsi_spcmd(un,C0_STARTSTOP,1,1,0,POLLED)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: Cannot Start Motor\n",
		    un->un_target,un->un_lun);
	    goto doagain;
	} else {
	    (void)timeout_spl(motortimeout, 0, 10 * HZ, spltty);
	}
    }
}

static int
wait_ready(un,mode)
register struct scsi_unit *un;
int mode;
{
    register int status, wait_amount;
    register SCSI_EXT_SENSE *sense;

    if ((un->un_flags & INT_READY) == 0) {  /* disk READY? */
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	wait_amount = common_scsi_powerup;
	add_motor_start(un,mode);
checkagain:
	if (status = common_scsi_spcmd(un,C0_TESTRDY,0,0,0,mode)) {
	    if (status == SELTMO || 
		(status == SCSI_CHECK && sense->key == NOT_RDY)) {
		DELAY(1000000);		/* wait 1 second */
		if (--wait_amount)
		    goto checkagain;
	    }
	    return(-1);
	} else {
	    if (scsiexterr > 1)
		cmn_err(CE_CONT,"SCSI %dL%d: drive ready after %d tries\n",
		    un->un_target,un->un_lun,common_scsi_powerup-wait_amount);
	    un->un_flags |= INT_READY;
	    common_scsi_attach(un,mode);
	}
    }
    return(0);
}

#else STANDALONE
static int
wait_ready(un,mode)
register struct scsi_unit *un;
int mode;
{
    if ((un->un_flags & INT_READY) == 0) {  /* disk READY? */
	if (common_scsi_spcmd(un,C0_TESTRDY,0,0,0,mode))
	    return(-1);
	else {
	    un->un_flags |= INT_READY;
	    return(0);
	}
    } else {
        return(0);
    }
}
#endif STANDALONE

/*
 * attach slave device
 */
void
common_scsi_attach(un,mode)
register struct scsi_unit *un;
int mode;
{
    register addr, temp, temp1;
    register SCSI_MS_ERROR *err;
    register SCSI_MS_RDAHD *rhd;
    register SCSI_MS_CACHE *cp;

    if (!(un->un_flags & INT_DISK)) { /* if this unit isn't a disk */
	/* mark volume header as valid
	 */
	un->un_vhvalid = 1;
	return;
    }
    if (!(un->un_flags & INT_READY)) { /* if this unit isn't ready */
	return;
    }
    addr = K0_TO_K1(&un->un_readcap);
    if (common_scsi_spcmd(un,C1_READCAP,0,sizeof(SCSI_READCAP),addr,mode))
        if (scsiexterr)
            cmn_err(CE_CONT,"SCSI %dL%d: Cannot read capacity\n",
                    un->un_target,un->un_lun);
    err = (SCSI_MS_ERROR *)K0_TO_K1(&un->un_mserror);
    temp = sizeof(SCSI_MS_ERROR);
    if (common_scsi_spcmd(un,C0_MSENSE,PD_ERROR,temp,err,mode)) {
        if (scsiexterr)
	    cmn_err(CE_CONT,
		"SCSI %dL%d: cannot get error recovery parameters\n",
	        un->un_target,un->un_lun);
    } else if ((err->per != 1) || (err->eec != 0) || (err->dcr != 0)) {
	temp1 = sizeof(SCSI_MS_ERROR) -
		(sizeof(SCSI_MODE_SENSE) + sizeof(SCSI_MS_PAGE_HDR));
	if (err->error_hdr.page_length != temp1) {
	    temp = err->error_hdr.page_length +
		    sizeof(SCSI_MODE_SENSE) + sizeof(SCSI_MS_PAGE_HDR);
	    if (temp > (sizeof(SCSI_MS_ERROR) + sizeof(un->un_mspad))) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: no space for error recovery parameters\n",
		    un->un_target,un->un_lun);
		goto nochange;
	    }
	    if (common_scsi_spcmd(un,C0_MSENSE,PD_ERROR,temp,err,mode)) {
		if (scsiexterr)
		    cmn_err(CE_CONT,
			"SCSI %dL%d: cannot get error recovery parameters\n",
			un->un_target,un->un_lun);
		goto nochange;
	    }
	}
	err->per = 1;
	err->eec = 0;
	err->dcr = 0;
	err->msense.hdr.sense_data_len = 0;
	err->error_hdr.ps = 0;
	if (common_scsi_spcmd(un,C0_MSELECT,PD_ERROR,temp,err,mode))
	    if (scsiexterr)
		cmn_err(CE_CONT,
		    "SCSI %dL%d: cannot set error recovery parameters\n",
		    un->un_target,un->un_lun);
	    
    }
nochange:
    addr = K0_TO_K1(&un->un_msdisrec);
    if (common_scsi_spcmd(un,C0_MSENSE,PD_DISREC,
			sizeof(SCSI_MS_DISREC),addr,mode))
        if (scsiexterr)
	    cmn_err(CE_CONT,
		"SCSI %dL%d: cannot get disconnect/reconnect parameters\n",
	        un->un_target,un->un_lun);
    addr = K0_TO_K1(&un->un_msformat);
    if (common_scsi_spcmd(un,C0_MSENSE,PD_FORMAT,
			sizeof(SCSI_MS_FORMAT),addr,mode))
        if (scsiexterr)
	    cmn_err(CE_CONT,"SCSI %dL%d: cannot get format parameters\n",
	        un->un_target,un->un_lun);
    addr = K0_TO_K1(&un->un_msgeom);
    if (common_scsi_spcmd(un,C0_MSENSE,PD_GEOM,sizeof(SCSI_MS_GEOM),addr,mode))
        if (scsiexterr)
	    cmn_err(CE_CONT,"SCSI %dL%d: cannot get disk geometry parameters\n",
	        un->un_target,un->un_lun);
    if (usepc8(un))
	un->un_flags |= INT_PC8;
    if (un->un_flags & INT_PC8) {
	rhd = (SCSI_MS_RDAHD *)K0_TO_K1(&un->un_msrdahd);
	if (common_scsi_spcmd(un,C0_MSENSE,PD_RDAHD,
			sizeof(SCSI_MS_RDAHD),rhd,mode)) {
	    if (scsiexterr)
		cmn_err(CE_CONT,
		    "SCSI %dL%d: cannot get cache control parameters\n",
		    un->un_target,un->un_lun);
	} else if (rhd->rcd == 1) {
	    rhd->rcd = 0;
	    rhd->msense.hdr.sense_data_len = 0;
	    rhd->rdahd_hdr.ps = 0;
	    if (common_scsi_spcmd(un,C0_MSELECT,PD_RDAHD,
			    sizeof(SCSI_MS_RDAHD),rhd,mode))
		cmn_err(CE_WARN,"SCSI %dL%d: Cannot enable disk cache\n",
		    un->un_target,un->un_lun);
	}
    } else {
	cp = (SCSI_MS_CACHE *)K0_TO_K1(&un->un_mscache);
	if (common_scsi_spcmd(un,C0_MSENSE,PD_CACHE,
			sizeof(SCSI_MS_CACHE),cp,mode)) {
	    if (scsiexterr)
		cmn_err(CE_CONT,
		    "SCSI %dL%d: cannot get cache control parameters\n",
		    un->un_target,un->un_lun);
	} else if (cp->ce == 0) {
	    cp->ce = 1;
	    cp->msense.hdr.sense_data_len = 0;
	    cp->cache_hdr.ps = 0;
	    if (common_scsi_spcmd(un,C0_MSELECT,PD_CACHE,
			    sizeof(SCSI_MS_CACHE),cp,mode))
		cmn_err(CE_WARN,"SCSI %dL%d: Cannot enable disk cache\n",
		    un->un_target,un->un_lun);
	}
    }
    (void)readvh(un,mode);
}

static int
usepc8(un)
register struct scsi_unit *un;
{
    register SCSI_INQUIRY *inq;

    inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inq);
    if (inq->ansi == 2)
	return(1);
    if (strncmp(inq->vendor_id,"CDC",3) == 0)
	return(0);
    return(1);
}

static int
readvh(un,mode)
register struct scsi_unit *un;
{
    register trk, i, addr;
    register struct volume_header *inv;
    u_int lbn, status;
    int errflg = 0;

    inv = un->un_vh_k1ptr;
    /*
     *
     * Try reading sector 0 of some number of tracks on cylinder 0
     * looking for a valid volume header
     *
     * MAXTRK will be set to 1 for now.
     */
    for (trk=0,lbn=0; trk < MAXTRK; trk++) {
	addr = K1_TO_PHYS(inv);
	if (status = common_scsi_spcmd(un,C0_READ,0,512,addr,mode)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: failed vh read with status %x\n",
	    			un->un_target,un->un_lun,status); 
	    errflg = 1;
	    continue;	/* error so try again */
	} else
	    errflg = 0;
	if (is_vh(inv)) {
	    /* mark volume header as valid
	     */
	    un->un_vhvalid = 1;
	    return(0);
	} else {
	    cmn_err(CE_CONT,"SCSI %dL%d: Volume Header is incorrect\n",
	    	un->un_target,un->un_lun); 
	    un->un_vhvalid = 0;
	    continue;
	}
    }
    if (trk == MAXTRK) {
	cmn_err(CE_CONT,"SCSI %dL%d: NO volume header found\n",
	    un->un_target,un->un_lun); 
    }
    return(errflg);
}

#ifndef STANDALONE
static void
wakeup_kmq()
{
    register struct iobuf *dp = &scsi_kmq;
    register struct buf *bp;
    register s,count;

    s = splbio();
    in_kmq = 1;
    kmq_timer_id = 0;
again:
    if(!(bp = dp->b_actf)) {
	splx(s);
	in_kmq = 0;
	return;
    }
    dp->b_actf = bp->av_forw;
    count = dp->qcnt--;
    common_scsi_strategy1(bp,0);
    if(count > dp->qcnt)
	goto again;
    if(dp->b_actf && (kmq_timer_id == 0))
	kmq_timer_id = timeout(wakeup_kmq,0,HZ/10);
    in_kmq = 0;
    splx(s);
}
    

static void
put_kmq(bp)
register struct buf *bp;
{
    register struct iobuf *dp = &scsi_kmq;

    if (dp->b_actf)
	dp->b_actl->av_forw = bp;
    else
	dp->b_actf = bp;
    bp->av_forw = (struct buf *)0;
    dp->b_actl = bp;
    dp->qcnt++;
    if(kmq_timer_id == 0)
	kmq_timer_id = timeout(wakeup_kmq,0,HZ/10);
}

static
SCSI_BUF_EXT *
get_bext()
{
	register SCSI_BUF_EXT *ptr;

	if(ptr = scsi_buf_ext_hdr) {
		scsi_buf_ext_hdr = ptr->nxt;
		return(ptr);
	} else
		return(NULL);
}

static void
put_bext(ptr)
register SCSI_BUF_EXT *ptr;
{
    if (scsi_noproc.b_actf) {
	unqueue_request(ptr);
	return;
    }

	ptr->nxt = scsi_buf_ext_hdr;
	scsi_buf_ext_hdr =  ptr;
	
}

	
/*
 * interrupt handler
 */
void
common_scsi_intr(un)
register struct scsi_unit *un;
{
    register struct iobuf *dp;
    register struct scsi_iopb *ip;
    register int status;
    register struct buf *bp, *nbp, *savedbp;
    register SCSI_ALIGN *al;
    register SCSI_EXT_SENSE *sense;
    register int dmaaddr, offset, count;
    int is_a_tape, short_read = 0;
    u_int temp, temp1, addr, bn;
    struct volume_header *vh;
    struct device_parameters *devp;
    struct partition_table *pt;
    SCSI_BUF_EXT *buf_ext;

    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    is_a_tape = (un->un_flags & INT_TAPE) ? 1 : 0;
    ip = un->un_iopbp;
    dp = un->un_dp;
    vh = un->un_vh_k1ptr;
    if (!(bp = (struct buf*)current_bp))
	bp = dp->b_actf;    /* grab active buffer (non-combined case) */
    else current_bp = (int)0; /* */
    ASSERT(bp != 0);
    /* 
     * see if this command was a request sense
     */
    if (ip->cmd_blk.cdb_0.cdb_0_cmd == C0_REQSENSE) {
	short_read = 1;
	un->un_xfer = bp->b_bcount;
	un->un_resid = bp->b_resid;
	count = common_scsi_sense_status(un,ip->scsi_status);
	bp->b_resid = un->un_resid;
        if (!(bp->b_flags & B_SPL)) {
            nbp = bp->av_forw;
            while (nbp) {
                nbp->b_resid = nbp->b_bcount;
                nbp = nbp->av_forw;
            }
        }
	switch (count) {
	    case 0:
		goto normcomp1;
	    case 1:
		goto errcomp;
	    case 2:
		if (bp->b_resid != bp->b_bcount) {
		    if (!(un->un_flags & INT_VARIABLE)) {
 			un->un_eomcount = un->un_weomcount = 0;
			common_scsi_command(un, bp);
			return;
		    }
		    goto normcomp1;
		}
		bp->b_error = ENOSPC;
		goto errcomp1;
	    default:
		goto errcomp;
	}
    }
    /* 
     * see if an error (or Check Condition) occurred on the last command
     */
    if (ip->scsi_taskid & ERROR) {
	if (ip->scsi_hwstatus) {
	    if (scsiexterr || ip->scsi_hwstatus != SELTMO)
	        cmn_err(CE_CONT,
		    "SCSI %dL%d: scsi hardware ERROR status: %x %s\n",
		    un->un_target,un->un_lun,ip->scsi_hwstatus,
		    scsiprinterr(ip->scsi_hwstatus));
	    if (is_a_tape) { /* for TAPE just get out */
		bp->b_flags |= B_ERROR;
		goto normcomp;
	    }
	}
	status = ip->scsi_status;
	/* handle case of command timeout/scsi reset whereby tape
	 * was disconnected on the reset. we DON'T want to 
	 * retry. If disk this would be the case of a silent
	 * retry for all devices that were disconnected on a
	 * scsi bus reset.
	 */
	if (!status && is_a_tape) {
	    bp->b_flags |= B_ERROR;
	    bp->b_error = EIO;
	    goto normcomp;
	}
	if (status == SCSI_CHECK) {
	    if (scsiexterr > 1) {
		cmn_err(CE_CONT,"SCSI %dL%d: cdb= ", un->un_target,un->un_lun);
		for (temp = 0; temp < 4; temp++)
		    cmn_err(CE_CONT,"0x%x ",ip->cmd_blk.cdb_raw[temp]);
		cmn_err(CE_CONT,"\n");
	    }
	    current_bp = (int)bp; /* save bp for interrupt handler!  */
	    ip->scsi_count0 = ip->scsi_count1 = 0;
	    ip->scsi_bufaddr = ip->scsi_bufaddr0 = ip->scsi_bufaddr1 = 0;
	    temp = un->un_command;		/* save old command */
	    un->un_command = C0_REQSENSE;	/* for setupdma! */
	    (*LOW_SCSI(bp->b_dev,setupdma))(un,NOSG,DMA_READ,16,
		K1_TO_PHYS(sense),0);
	    un->un_command = temp;		/* restore old command */
	    setupiopb(C0_REQSENSE,sense,un,INTERRUPT,16,0);
	    (void)(*LOW_SCSI(bp->b_dev,startop))(un,INTERRUPT);
	    return; /* just bail and let it happen */
	} else if (status == SCSI_BUSY) {
	    current_bp = (int)bp; /* save bp for interrupt handler!  */
	    ip->scsi_taskid &= ~ERROR;
	    (void)timeout(busytimeout, un, 1*HZ);
	    return;
	}
errcomp:
	if (ip->cmd_blk.cdb_0.cdb_0_cmd != C0_REQSENSE)
	    ++un->un_hardcount;
        if (!(bp->b_flags & B_SPL) && !is_a_tape && 
		(++(bp->b_error) <= un->un_nretries)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: cmd = 0x%x retrying(%d)...\n",
		un->un_target,un->un_lun,un->un_command,bp->b_error);
	    temp = bp->b_error;
	    devp = &vh->vh_dp;
	    temp1 = devp->dp_secs * devp->dp_trks0;
	    while (bp) { /* need to handle combined reads/writes! */
		    if (al = ALGNPTR) {
			bp->b_dmaaddr = (caddr_t)al->al_uaddr;
			bp->b_bcount = (unsigned)al->al_size;
		    }
		    nbp = bp->av_forw; /* disksort clobbers! */
		    pt = &vh->vh_pt[FS(bp->b_dev)];
		    /*
		     * these values must be reset for disksort!
		     */
		    PBLKNO  = (unsigned int)(bp->b_blkno + pt->pt_firstlbn);
		    bp->b_cylin = PBLKNO / temp1;
		    disksort(dp, bp, un->un_lastcyl);
		    bp = nbp; /* combined reads/writes? */
		    if (bp)
			    bp->b_error = temp; /* separate combined rds/wrts */
	    }
    	    dp->b_active = 0; /* this drive no longer active */
	    common_scsi_start(un, dp);
	    return;
	} else { /* NOTE that we'll only get here after de-combining! */
	    bp->b_error = EIO;
	    un->un_flags &= ~INT_WRITTEN;
errcomp1:
	    bp->b_flags |= B_ERROR;
	    count = un->un_prev_secs * un->un_sectsize;
		    /* valid since we've split-up combines previously */
	    bp->b_resid += count;
	}
    }
    /* 
     * normal command completion
     */
normcomp:
    if ((un->un_flags & INT_TAPE) && !(bp->b_flags & B_SPL))
	un->un_flags &= ~INT_FM;
normcomp1:
    /* 
     * Temporary buffer may hold data from transfer.
     */
    if (al = ALGNPTR) {
	bp->b_dmaaddr = (caddr_t)al->al_uaddr;
	bp->b_bcount = (unsigned)al->al_size;
	if((bp->b_flags & B_SPL) || !bp->b_resid || (bp->b_error == EIO)) {
	    if (bp->b_flags & B_READ) {
		dmaaddr = (int)al->al_tbuff;
		offset = poff(dmaaddr);
		while (count = al->al_size) {
		    if ((count + offset) > NBPP) {
			count = NBPP - offset;
			offset = 0;
		    }
		    bcopy(K2_TO_K1(dmaaddr),al->al_uaddr,count);
		    al->al_uaddr += count;
		    dmaaddr += count;
		    al->al_size -= count;
		}
	    }
	    kmemfree(al,0,M_NOWAIT);
	    ALGNPTR = (SCSI_ALIGN *)NULL;
	}
    }
    dp->b_active = 0; /* this drive no longer active */

    if (bp->b_flags & B_SPL) {
	if (short_read && !(bp->b_flags & B_ERROR)) {
	    common_scsi_start(un, dp);
	    return;
	}
	un->un_iotime->io_resp += lbolt - bp->b_start;
	un->un_iotime->io_act  += lbolt - dp->io_start;
	if (dp->b_actf = bp->av_forw)
	    common_scsi_start(un, dp);
	/* link back into free list */
	buf_ext = (SCSI_BUF_EXT *)bp->av_back;
	put_bext(buf_ext);
	iodone(bp);
    } else {
	do {
	    /*
	     * check for more data to transfer
	     */
	    savedbp = bp;
	    if (bp->b_resid && (bp->b_error != EIO)) {
		nbp = bp->av_forw;
    		/* remember the 'short read' case for tape! */	
		if(un->un_flags & INT_TAPE) {
		    if (short_read)
			goto normal;
		    RESIDUAL = bp->b_resid; /* b_resid gets clobbered!! */
		    PBLKNO  = (unsigned int)0;
		    bp->b_cylin = 0;
		} else {
		    RESIDUAL = bp->b_resid; /* b_resid gets clobbered!! */
		    bn = (u_int)(bp->b_blkno + 
			    ((bp->b_bcount - bp->b_resid) >> SCTRSHFT));
		    pt = &vh->vh_pt[FS(bp->b_dev)];
		    devp = &vh->vh_dp;
		    temp = devp->dp_secs * devp->dp_trks0;
		    PBLKNO  = (unsigned int)(bn + pt->pt_firstlbn);
		    bp->b_cylin = PBLKNO / temp;
		}
		disksort(dp, bp, un->un_lastcyl);
		bp = nbp; /* restart all of them */
	    } else {
normal:
		/*
		 * If there were a number of writes combined together
		 * in one iopb, walk through the list of bp's and call
		 * IODONE for each. bp->av_forw is the indication.
		 *
	         * update accounting
	         */
		un->un_iotime->io_resp += lbolt - bp->b_start;
		nbp = bp->av_forw;
		/* NOTE that this should never happen since the combines
		 * should have been split and retried individually before
		 * we ever get here */
    		if (nbp && (bp->b_flags & B_ERROR)) { /* set ERROR for all! */
		    nbp->b_flags |= B_ERROR;      /* iodone clobbers!   */
		    nbp->b_error  = EIO;
		}
		iounmap(bp);
		if(ALGNPTR) {
			kmemfree(ALGNPTR,0,M_NOWAIT);
			ALGNPTR = (SCSI_ALIGN *)NULL;
		}
		/* link back into free list */
		buf_ext = (SCSI_BUF_EXT *)bp->av_back;
		put_bext(buf_ext);
	        iodone(bp);
		bp = nbp;
	    }
	} while(bp);

	bp = savedbp; /* restore last bp */
	un->un_iotime->io_act  += lbolt - dp->io_start;
	if (dp->b_actf)
	    common_scsi_start(un, dp);
    }
    /* update accounting
     */

    un->un_flags &= ~(INT_BUSY | INT_REWINDING);
    if (un->un_flags & INT_WAITING) {
	un->un_flags &= ~INT_WAITING;
	wakeup((caddr_t) un);
    }
    if (bp->b_flags & B_SPL)
	bp->b_flags &= ~(B_BUSY | B_SPL);
}
#endif STANDALONE

/*
** device open routine
*/
#ifdef STANDALONE
common_scsi_open(io,dev,type)
register struct iob *io;
register dev_t dev;
int type;
{
    register flag = 0;
#else STANDALONE
void
common_scsi_open(dev,flag,type)
dev_t dev;
int flag;
{
#endif STANDALONE
    register struct scsi_unit *un;
    register partition = FS(dev);

    if ((un = common_scsi_getun(dev)) == NULL) {
	ERR_RETURN(ENXIO);
    }
    if (!un->un_flags) { /* 1st open for non-embedded lun? */
	if (common_scsi_slave(un,SP_WAIT))
	    common_scsi_attach(un,SP_WAIT);
    }
#ifdef STANDALONE
    if ((un->un_flags & type) == 0) {  /* type OK? */
        cmn_err(CE_CONT,"SCSI %dL%d: Device is wrong type\n",
                un->un_target,un->un_lun);
        ERR_RETURN(ENODEV);
    }
#endif STANDALONE
    if ((un->un_flags & INT_ALIVE) == 0) {  /* unit OK? */
	ERR_RETURN(ENODEV);
    }
    if (un->un_flags & INT_TAPE) {
	if (un->un_flags & INT_OPEN) {  /* sorry, we're exclusive open */
	    ERR_RETURN(EBUSY);
	}
#ifndef STANDALONE
	if (tapeopen(un, flag, dev))
	    return;
	un->un_flags |= INT_OPEN;
    } else if (wait_ready(un,SP_WAIT))	/* is disk ready */
	ERR_RETURN(ENODEV);

	if((flag & FWRITE) && (un->un_flags & INT_DISK)) {
		unsigned tmp;

		if(scsi_plock && scsi_overlap(un,dev,type))
			ERR_RETURN(EBUSY);
		if(scsi_plock && scsi_inuse(un,dev,type))
			ERR_RETURN(EBUSY);
		switch(type) {
		case OTYP_BLK:
			tmp = un->un_open[partition].usage.blk;
			if(tmp++ < 2)
				un->un_open[partition].usage.blk = tmp;
			break;
		case OTYP_CHR:
			tmp = un->un_open[partition].usage.chr;
			if(tmp++ < 2)
				un->un_open[partition].usage.chr = tmp;
			break;
		case OTYP_SWP:
			tmp = un->un_open[partition].usage.swp;
			if(tmp++ < 2)
				un->un_open[partition].usage.swp = tmp;
			break;
		case OTYP_MNT:
			tmp = un->un_open[partition].usage.mnt;
			if(tmp++ < 2)
				un->un_open[partition].usage.mnt = tmp;
			break;
		case OTYP_LYR:
			tmp = un->un_open[partition].usage.layer;
			if(++tmp >= 65536) {
				cmn_err(CE_CONT,"Number of layer open exceeds limit (65536).\nPartition locking is broken!\n");
				tmp = 65536;
			}
			un->un_open[partition].usage.layer = tmp;
			break;
		default: /* assume to be chr */
			cmn_err(CE_CONT,"unknown device type 0x%x\n",type);
			break;
		}
	}
    return;
#else STANDALONE
	if (tapeopen(io, un))
	    return(-1);
    }
    else {		/* disk */
	if (wait_ready(un,SP_WAIT))	/* is disk ready */
	    ERR_RETURN(ENODEV);
	if (io->i_fstype == DTFS_AUTO)
	    io->i_fstype =
                vh_mapfstype(un->un_vh_k1ptr->vh_pt[io->i_part].pt_type);
    }
    return(0);
#endif STANDALONE
}

#ifndef STANDALONE
scsi_inuse(un,dev,type)
register struct scsi_unit *un;
dev_t dev;
{
	partition_usage *pu;
	register k = FS(dev);

/*	The algorithm used here is

	Current Status
	blk	chr	swp	mnt	layer

blk	Y	N	Y 	Y	N
chr	N	Y	Y	N	N
swp	N (*) 	N	N	N	N
mnt	Y	N	N	Y	N
layer	N	N	N	N	Y

* we only allow block and swap to be opened  at the same time when
  rootdev = swapdev = dev

*/

	pu = &un->un_open[k];
	switch(type) {
	case OTYP_BLK:
		if(pu->usage.chr || pu->usage.layer)
			return(1);
		break;
	case OTYP_CHR:
		if(pu->usage.blk || pu->usage.mnt || pu->usage.layer)
			return(1);
		break;
	case OTYP_SWP:
		if(pu->usage.blk) {
			if(!(dev == rootdev && dev == swapdev))
				return(1);
		}
		if(pu->usage.chr || pu->usage.swp || pu->usage.mnt || pu->usage.layer)
			return(1);
		break;
	case OTYP_MNT:
		if(pu->usage.chr || pu->usage.swp || pu->usage.layer)
			return(1);
		break;
	case OTYP_LYR:
		if(pu->usage.blk || pu->usage.chr || pu->usage.swp || pu->usage.mnt)
			return(1);
		break;
	default:
/* unknown type */
		return(1);
		break;
	}
	return(0);
}

scsi_overlap(un,dev,type)
dev_t dev;
register struct scsi_unit *un;
{
	register i,sb,eb,x,y;
	register struct partition_table *pt = &un->un_vh->vh_pt[0];
	register k = FS(dev);

	sb = pt[k].pt_firstlbn;
	eb = sb + pt[k].pt_nblks - 1;
	if(eb < sb) return(0);	/* no blocks allocated, won't do any harm */
	for(i = 0; i < NPARTAB; i++) {
/* we do not check on the current partition - to allow multiple open.
   if the i-th partition is not open or contains 0 blocks, we don't care
*/
		if((i == k) || (un->un_open[i].status == 0) || (pt[i].pt_nblks <= 0)) 
			continue;
		x = pt[i].pt_firstlbn;
		y = pt[i].pt_firstlbn + pt[i].pt_nblks - 1;
/* If our starting block is > the last block of the i-th partition
   or if our ending block is < the first block of the i-th partition,
   then there is no overlap.  Do you think this covers all cases?
*/
		if(y < sb || eb < x) continue;

/* We do not restrict "layer" opens */

		if(type == OTYP_LYR && un->un_open[i].usage.layer != 0)
			continue;

		return(1);
	}
	return(0);
}
#endif

#ifdef STANDALONE
static int
tapeopen(io, un)
register struct iob *io;
struct scsi_unit *un;
{
    register struct tp_dir *tpd;
    register int status, flag = 0;
    register int count = WAITREADY;
    struct scsi_unit *common_scsi_getun();
    register SCSI_EXT_SENSE *sense;
    register SCSI_MODE_SENSE *ms;
    register SCSI_RDBLKLIM *rb;

    if( !common_scsi_tapebuf ) {
	common_scsi_tapebuf = (char *)K0_TO_K1(align_malloc(TP_BLKSIZ,64));
	if (!common_scsi_tapebuf) {
	    cmn_err(CE_CONT,"SCSI %dL%d: cannot allocate tape buffer\n",
		un->un_target,un->un_lun);
	    return(-1);
	}
    }

    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);

    /* test the tape for ready */
again:
    if (status = common_scsi_spcmd(un, C0_TESTRDY, 0, 0, 0, POLLED)) {
	if (status == SCSI_CHECK && sense->key == NOT_RDY) {
	    if (!flag) {
		cmn_err(CE_CONT,"SCSI %dL%d: tape not ready\n",
		    un->un_target, un->un_lun);
		flag++;
	    } else 
		cmn_err(CE_CONT, ".");
	} else {
	    io->i_errno = ENXIO;
	    return(-1);
	}
	if (count--) {
	    _scandevs(); 
	    goto again;
	}
    }

    rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
    if (common_scsi_spcmd(un,C0_RDBLOCK,0,6,rb,SP_WAIT)) {
	cmn_err(CE_CONT, "SCSI %dL%d: cannot read block limits\n",
	    un->un_target,un->un_lun);
	io->i_errno = ENXIO;
	return(-1);
    }
    ms = (SCSI_MODE_SENSE *)K0_TO_K1(&un->un_mserror);
    if (common_scsi_spcmd(un,C0_MSENSE,0,sizeof(SCSI_MODE_SENSE),ms,SP_WAIT)) {
	cmn_err(CE_CONT,"SCSI %dL%d: error in mode sense\n",
	    un->un_target,un->un_lun);
	io->i_errno = ENXIO;
	return(-1);
    }
    if (rb->maxlen != rb->minlen) {
	ms->hdr.sense_data_len = 0;
	ms->hdr.medium_type = 0;
	ms->hdr.WP = 0;
	ms->hdr.blk_desc_len = 8;
	ms->blk_desc.density_code = 0;
	ms->blk_desc.nrblks = 0;
	ms->blk_desc.blk_len = 0;
	if (common_scsi_spcmd(un,C0_MSELECT,0,sizeof(SCSI_MODE_SENSE),
			ms,SP_WAIT)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: error in mode select\n",
		un->un_target,un->un_lun);
	    io->i_errno = ENXIO;
	    return(-1);
	}
	un->un_flags |= INT_VARIABLE;
	un->un_sectsize = 1;
    }

    /* rewind it */
    cmn_err(CE_CONT,"\nRewinding the tape.....");
    if (status = common_scsi_spcmd(un, C0_REWIND, 0, 0, 0, POLLED)) {
	io->i_errno = ENXIO;
	return(-1);
    }
    cmn_err(CE_CONT,"Done\n");

    /* space forward to the file specified by i_part */
    if (io->i_part) {
	count = io->i_part;
        cmn_err(CE_CONT,"\nForward spacing the tape %d files.....",count);
	if (status = common_scsi_spcmd(un, C0_SPACE, FILES, count, 0, POLLED)) {
	    io->i_errno = ENXIO;
	    return(-1);
	}
        cmn_err(CE_CONT,"Done\n");
    }

    /* read the boot tape header */
    un->un_tpnxtrec = 0;
    un->un_tpxfer = TP_BLKSIZ;
    un->un_resid = 0;
    if (status = common_scsi_spcmd(un, C0_READ, 0, TP_BLKSIZ, 
      K1_TO_PHYS(common_scsi_tapebuf), POLLED)) {
	    io->i_errno = ENXIO;
	    return(-1);
    } else
	un->un_tpxfer = TP_BLKSIZ - un->un_resid;
    tpd = (struct tp_dir *)io->i_fs_tape;
    bcopy (common_scsi_tapebuf, tpd, sizeof (struct tp_dir));

    /* examine the boot tape header */
    switch (io->i_fstype) {
	case DTFS_AUTO:
	    if (!is_tpd(tpd)) {
		    io->i_fstype = DTFS_NONE;
	    } else {
		    io->i_fstype = DTFS_TPD;
	    }
	    break;

	case DTFS_TPD:
	    if (!is_tpd(tpd)) {
		cmn_err(CE_CONT,"SCSI %dL%d: tape volume header ERROR\n",
		    un->un_target, un->un_lun);
		io->i_errno = ENXIO;
		return(-1);
	    }
	    break;

	default:
	    break;
    }
    return(0);
}
#else STANDALONE
static int
tapeopen(un, flag, dev)
struct scsi_unit *un;
dev_t dev;
{
    register SCSI_MODE_SENSE *ms;
    register SCSI_RDBLKLIM *rb;
    register int s, status;

    /* wait for a previous rewind to complete */
    s = splbio();
    while (un->un_flags & INT_REWINDING) {
	un->un_flags |= INT_WAITING;
	if (sleep((caddr_t) un, PUSER|PCATCH)) {
	    /* user gave up waiting...fail the open */
	    un->un_flags &= ~INT_WAITING;
	    splx(s);
	    SET_ERROR(EINTR);
	    return(1);
	}
    }
    splx(s);
    /* use TUN to see if cartridge installed, etc.
     * and (TODO) report findings to the user
     * if the request sense doesn't report 
     */
    status = common_scsi_spcmd(un,C0_TESTRDY,0,0,0,SP_WAIT);
    if (flag & FSYNC) {
	SET_ERROR(0);	/* clear possible error on test ready */
	return(0);
    }
    if (status) {
	if (un->un_flags & INT_NOT_RDY)
	    cmn_err(CE_CONT,
		"SCSI %dL%d: not ready; offline or tape not installed\n",
		un->un_target,un->un_lun);
	goto tpopenerr;
    }
    rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
    if (common_scsi_spcmd(un,C0_RDBLOCK,0,6,rb,SP_WAIT)) {
	cmn_err(CE_CONT, "SCSI %dL%d: cannot read block limits\n",
	    un->un_target,un->un_lun);
	goto tpopenerr;
    }
    ms = (SCSI_MODE_SENSE *)K0_TO_K1(&un->un_mserror);
    if (common_scsi_spcmd(un,C0_MSENSE,0,sizeof(SCSI_MODE_SENSE),ms,SP_WAIT)) {
	cmn_err(CE_CONT,"SCSI %dL%d: error in mode sense\n",
	    un->un_target,un->un_lun);
	goto tpopenerr;
    }
    if (ms->hdr.WP) {
	if (flag & FWRITE) {
	    cmn_err(CE_CONT,"SCSI %dL%d: write protected\n",
		un->un_target,un->un_lun);
	    goto tpopenerr;
	}
	un->un_flags |= INT_READONLY;
    }
    else un->un_flags &= ~INT_READONLY;
    ms->hdr.sense_data_len = 0;
    ms->hdr.medium_type = 0;
    ms->hdr.WP = 0;
    ms->hdr.blk_desc_len = 8;
    ms->blk_desc.nrblks = 0;
    if (rb->maxlen != rb->minlen) {
	ms->blk_desc.density_code = 0;
	ms->blk_desc.blk_len = 0;
	un->un_flags |= INT_VARIABLE;
	un->un_sectsize = 1;
    } else
	ms->blk_desc.density_code = Tapedensity[TAPEDENSITY(dev)];
    if (common_scsi_spcmd(un,C0_MSELECT,0,sizeof(SCSI_MODE_SENSE),
		    ms,SP_WAIT)) {
	cmn_err(CE_CONT,"SCSI %dL%d: error in mode select\n",
	    un->un_target,un->un_lun);
tpopenerr:
	SET_ERROR(EIO);
	return(1);
    }
    return(0);
}
#endif STANDALONE

/*
** device close routine
*/
#ifdef STANDALONE
common_scsi_close(io,dev)
register struct iob *io;
register dev_t dev;
{
    register flag = 0;
#else STANDALONE
void
common_scsi_close(dev, flag,type)
dev_t dev;
register int flag;
{
#endif STANDALONE
    register struct scsi_unit *un;
    int partition = FS(dev);

    if ((un = common_scsi_getun(dev)) == NULL) {
	ERR_RETURN(ENXIO);
    }

    if (un->un_flags & INT_TAPE)
#ifdef STANDALONE
	tapeclose(io, un, dev); 
    if (un->un_flags & INT_CLOBBER)
        un->un_flags = 0;
    return(0);
#else STANDALONE
	tapeclose(un, dev); 
    if (un->un_flags & INT_CLOBBER)
        un->un_flags = 0;
	if(un->un_flags & INT_DISK) {
		switch(type) {
		case OTYP_BLK:
			un->un_open[partition].usage.blk = 0;
			break;
		case OTYP_SWP:
			un->un_open[partition].usage.swp = 0;
			break;
		case OTYP_CHR:
			un->un_open[partition].usage.chr = 0;
			break;
		case OTYP_MNT:
			un->un_open[partition].usage.mnt = 0;
			break;
		case OTYP_LYR:
			if(un->un_open[partition].usage.layer > 0)
				un->un_open[partition].usage.layer--;
			break;
		default:
			break;
		}
		if(dev == rootdev && un->un_open[partition].usage.blk == 0)
			un->un_open[partition].usage.blk = 1;
		if(dev == swapdev && un->un_open[partition].usage.swp == 0)
			un->un_open[partition].usage.swp = 1;
	}
    return;
#endif STANDALONE
}

#ifdef STANDALONE
static void
tapeclose(io, un, dev)
register struct iob *io;
#else STANDALONE
static void
tapeclose(un, dev)
#endif STANDALONE
register struct scsi_unit *un;
dev_t dev;
{
    if (un->un_flags & INT_WRITTEN) {
	if (common_scsi_spcmd(un,C0_WRFM,0,1,0,SP_WAIT)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: error in writing file marks\n",
		un->un_target,un->un_lun);
	    SET_ERROR(EIO);
	} else if (un->un_flags & INT_VARIABLE) {
	  if (!ONEFM(dev)) {
	    if (common_scsi_spcmd(un,C0_WRFM,0,1,0,SP_WAIT)) {
		cmn_err(CE_CONT,"SCSI %dL%d: error in writing file marks\n",
		    un->un_target,un->un_lun);
		SET_ERROR(EIO);
#ifndef STANDALONE
	    } else if (NOREWIND(dev)) {
		/* backspace 1 file mark */
		if (common_scsi_spcmd(un,C0_SPACE,FILES,~1L + 1,0,SP_WAIT)) {
		    cmn_err(CE_CONT,"SCSI %dL%d: error in backspace file marks\n",
			un->un_target,un->un_lun);
		    SET_ERROR(EIO);
		}
#endif STANDALONE
	    }
	  }
	}
    }
    /* rewind the tape as appropriate
     * and don't wait for completion
     */
    if (!(NOREWIND(dev))) {
	un->un_eomcount = un->un_weomcount = 0;
#ifndef STANDALONE
	un->un_flags |= INT_REWINDING;
#endif STANDALONE
	un->un_flags &= ~(INT_FM|INT_EOM);
#ifdef STANDALONE
        cmn_err(CE_CONT,"\nRewinding the tape.....");
#endif STANDALONE
	if (common_scsi_spcmd(un,C0_REWIND,0,0,0,SP_NOWAIT)) {
	    cmn_err(CE_CONT,"SCSI %dL%d: error in rewinding\n",
		un->un_target,un->un_lun);
	    SET_ERROR(EIO);
	}
#ifdef STANDALONE
        cmn_err(CE_CONT,"Done\n");
#endif STANDALONE
    }
    un->un_flags &= ~(INT_OPEN|INT_WRITTEN|INT_READ|INT_RFM|
		    INT_NOT_RDY|INT_VARIABLE);
#ifdef STANDALONE
    un->un_tpxfer = 0;
    un->un_tpnxtrec = -1;
#endif STANDALONE
}

#ifdef STANDALONE
int
common_scsi_strategy(io, dev, func)
register struct iob *io;
register dev_t dev;
register int func;
{
    register struct partition_table *pt;
    register SCSI_EXT_SENSE *sense;
    register struct scsi_unit *un;
    register struct volume_header *vh;
    register struct device_parameters *devp;
    register unsigned lbn;
    register int status = 0, space = 0, cmd;
    register struct tp_dir *tpd;
    register unsigned newblk, blks, nxtblks, count, off;

    if ((un = common_scsi_getun(dev)) == NULL){
	io->i_errno = ENXIO;
	return (-1);
    }
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    un->un_resid = 0;

    if (un->un_flags & INT_TAPE) {
	/*
	 * If the request is for block zero, func equals a read
	 * and we have a valid tape volume header just return it.
	 * (Normally called from inside the file systems open routine)
	 */
	if (func == READ) {
	    tpd = (struct tp_dir *)io->i_fs_tape;
	    if (io->i_bn == 0 && (tpd->td_magic == TP_MAGIC) &&
		(io->i_cc == sizeof (struct tp_dir)) &&
		(un->un_tpnxtrec == 0)) {
		    bcopy (tpd, io->i_ma, io->i_cc);
		    return (io->i_cc);
	    }
	}
	/*
	 * in standalone environment we don't declare the device open until
	 * we give the filesystem a chance to fail the open.  the first real
	 * i/o request means we successfully opened.
	 */
	un->un_flags |= INT_OPEN;

	/*
	 * writes occur in sizes requested by the program but must be 
	 * a multiple of NBPSCTR. reads on the other hand may be any size but
	 * we optimize tape use by reading TP_BLKSIZ and copying it out to
	 * the user as requested.
	 */
	if (func == WRITE) {
	    if (io->i_cc % NBPSCTR) {
		cmn_err(CE_CONT,"SCSI %dL%d: request of %d not a multiple of %d\n",
		    un->un_target, un->un_lun, io->i_cc, NBPSCTR);
		io->i_errno = EIO;
		return (-1);
	    }
	    if (io->i_bn != un->un_tpnxtrec) {
		if ((io->i_bn == 0)) {
		    /* first write */
		    un->un_tpnxtrec = 0;
		} else {
		    cmn_err(CE_CONT,"SCSI %dL%d: non sequential write ERROR\n",
			un->un_target, un->un_lun);
		    io->i_errno = ENXIO;
		    return (-1);
		}
	    }
	    status = common_scsi_spcmd(un, C0_WRITE, 0, io->i_cc,
	      K0_TO_PHYS(io->i_ma), POLLED);
	    un->un_tpnxtrec = io->i_bn + (io->i_cc >> SCTRSHFT);
	    un->un_flags |= INT_WRITTEN;
	} else {
	    newblk = io->i_bn >> TP_SHIFT;
	    if (newblk != un->un_tpnxtrec) {
		un->un_tpnxtrec++;
		blks = newblk*TP_BLKS_PER_REC;
		nxtblks = un->un_tpnxtrec*TP_BLKS_PER_REC;
		if (newblk > un->un_tpnxtrec) {
		    count = blks - nxtblks;
		    status = common_scsi_spcmd(un, C0_SPACE, BLOCKS, count, 
		      0, POLLED);
		} else if (newblk < un->un_tpnxtrec) {
		    count = nxtblks - blks;
		    count = ~count + 1; /* two's complement */
		    status = common_scsi_spcmd(un, C0_SPACE, BLOCKS, count, 
		      0, POLLED);
		}
		un->un_tpnxtrec = newblk;
		if (status) {
		    space = 1;
		    cmn_err(CE_CONT,"SCSI %dL%d: tape space command ERROR\n",
			un->un_target, un->un_lun);
		    goto stat;
		}
		if (status = common_scsi_spcmd(un, C0_READ, 0, TP_BLKSIZ, 
		  K1_TO_PHYS(common_scsi_tapebuf), POLLED)) {
		    goto stat;
		}
		un->un_tpxfer = TP_BLKSIZ - un->un_resid;
		un->un_flags |= INT_READ;
	    }
	}
stat:
	if (status == SCSI_CHECK) {
	    io->i_errno = EIO;
	    return (-1);
	}
	if (func == READ) {
	    off = (io->i_bn & (TP_BLKS_PER_REC - 1)) << SCTRSHFT;
	    if (off > un->un_tpxfer) {	/* can only happen at end of file */
		io->i_errno = EIO;
		return (-1);
	    }
	    count = _min((un->un_tpxfer - off), io->i_cc);
	    bcopy (&common_scsi_tapebuf[off], io->i_ma, count);
	    un->un_resid = io->i_cc - count;
	}
    } else {
	if ((func == READ) || (func == WRITE)) {
	    vh = un->un_vh_k1ptr;
	    pt = &vh->vh_pt[io->i_part];
            if (un->un_vhvalid == 0) {
		if (io->i_part == PTNUM_VOLUME) {
		    devp = &vh->vh_dp;
		    devp->dp_secbytes = un->un_readcap.blklen;
		    devp->dp_secs = 1;
		    devp->dp_trks0 = 1;
		    devp->dp_cyls = 1;
		    pt->pt_firstlbn = 0;
		    pt->pt_nblks = un->un_readcap.maxblk;
		} else {
		    io->i_errno = ENXIO;
		    return (-1);
		}
	    } 
	    if ((unsigned)io->i_bn > pt->pt_nblks) {
		    cmn_err(CE_CONT,
			"SCSI %dL%d: read beyond end of partition\n",
			un->un_target, un->un_lun);
		    io->i_errno = ENXIO;
		    return (-1);
	    }
	    lbn = io->i_bn + pt->pt_firstlbn;
	    if (io->i_cc % NBPSCTR) {
		cmn_err(CE_CONT,"SCSI %dL%d: request of %d not a multiple of %d\n",
		    un->un_target, un->un_lun, io->i_cc, NBPSCTR);
		io->i_errno = EIO;
		return (-1);
	    }
	}
	switch (func) {
	    case READ:
		cmd = C0_READ;
		break;

	    case WRITE:
		cmd = C0_WRITE;
		break;

	    default:
		_io_abort("dksd bad function");
	}
	if (status = common_scsi_spcmd(un, cmd, lbn, io->i_cc, 
	  K0_TO_PHYS(io->i_ma), POLLED)) {
	    io->i_errno = ENXIO;
	    return(-1);
	}
    }
    return (io->i_cc - un->un_resid);
}
#else STANDALONE
void
common_scsi_strategy(bp)
struct buf *bp;
{
	common_scsi_strategy1(bp,1);
}

common_scsi_strategy1(bp,flag)
register struct buf *bp;
{
    daddr_t bn;
    long sc = 1;
    register struct iobuf *dp;
    struct volume_header *vh;
    struct device_parameters *devp;
    struct partition_table *pt;
    SCSI_RDBLKLIM *rb;
    SCSI_BUF_EXT *buf_ext;
    register struct scsi_unit *un;
    register int target, lun;
    int cmd, s; 

    /*
     * check for valid unit number and unit's existence
     */
    if ((un = common_scsi_getun(bp->b_dev)) == NULL) {
	bp->b_error = ENXIO;
	goto badio;
    }
    if (un->un_flags & INT_DISK) {
        if (!(bp->b_flags & B_SPL)) {
	    vh = un->un_vh_k1ptr;
	    pt = &vh->vh_pt[FS(bp->b_dev)];
	    devp = &vh->vh_dp;
	    /*
	     * make sure unit has a valid volume header
	     */
            if (un->un_vhvalid == 0) {
		if (FS(bp->b_dev) == PTNUM_VOLUME) {
		    devp = &vh->vh_dp;
		    devp->dp_secbytes = un->un_readcap.blklen;
		    devp->dp_secs = 1;
		    devp->dp_trks0 = 1;
		    devp->dp_cyls = 1;
		    pt->pt_firstlbn = 0;
		    pt->pt_nblks = un->un_readcap.maxblk + 1;
		} else {
		    bp->b_error = ENXIO;
		    goto badio;
		}
	    } 
	    /* 
	     * calc number of blocks in transfer and make sure the request 
	     * is contained in the partition
	     */
            if ((sc = BTOBB(bp->b_bcount)) == 0)
                goto badio1;
	    bn = bp->b_blkno;
	    if ((bn < 0) || ((bn + sc) > pt->pt_nblks)) {
		bp->b_error = ESPIPE;		/* illegal seek */
		goto badio;
	    }
	    else if (bn == pt->pt_nblks)
		goto badio1;			/* EOF condition */
	    /*
	     * set cylinder number for disksort()
	     */
	    bp->b_cylin = (bn + pt->pt_firstlbn) / 
			  (devp->dp_secs * devp->dp_trks0);
        } else
            bp->b_cylin = 0;
    } else {
	/* TAPE ONLY */
	if (un->un_flags & INT_RFM && !(bp->b_flags & B_SPL) &&
		bp->b_flags & B_READ) {
	    un->un_flags &= ~INT_RFM;
	    goto badio1;			/* EOF condition */
	}
	if (!(bp->b_flags & B_SPL)) {
	    if (un->un_flags & INT_VARIABLE) {
		rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
		if (bp->b_bcount > (unsigned)rb->maxlen ||
		    bp->b_bcount < (unsigned)rb->minlen) {
		    cmn_err(CE_CONT,
			"SCSI %dL%d: invalid block length request %d\n",
			un->un_target,un->un_lun,bp->b_bcount);
		    bp->b_error = EIO;
		    goto badio;
		}
	    }
	    if (un->un_weomcount ==1 && !(bp->b_flags & B_READ)) {
		++un->un_weomcount;
		bp->b_error = ENOSPC;
		goto badio;
	    } else if (un->un_eomcount > MAXEOM) {
		bp->b_error = ENOSPC;
		goto badio;
	    }
	}
	/*
	 * set cylinder number for disksort()
	 */
	bp->b_cylin = 0;
	if (un->un_flags & INT_VARIABLE)
	    sc = 1;
	else
	    sc = bp->b_bcount >> SCTRSHFT;
    }

    s = splbio();
    if (!(bp->b_flags & B_SPL) && flag) {

/* iomap does not like b_bcount to be 0 */

	if(bp->b_bcount == 0) {
		bp->b_dmalen = 0;
		bp->b_dmaaddr = 0;
		splx(s);
		goto badio1;
	}
	iomap(bp);
    }
    if((buf_ext = get_bext()) == NULL) {
	bp->av_back = (struct buf *)sc;		/* save sector count */
	queue_request(bp);
	dp = un->un_dp;
	if (dp->b_active == 0) {
		common_scsi_start(un, dp);
	}
	splx(s);
	return;
    } else
	bp->av_back = (struct buf *)buf_ext;

    sortit(un,bp,pt,sc);
    splx(s);
    return;
badio:
    bp->b_flags |= B_ERROR;
badio1:
    bp->b_resid = bp->b_bcount;
    iounmap(bp);
    iodone(bp);
    if (bp->b_flags & B_SPL)
	bp->b_flags &= ~(B_BUSY | B_SPL);
}

static void
queue_request(bp)
register struct buf *bp;
{
    register struct iobuf *dp;

    dp = &scsi_noproc;
    if (dp->b_actf)
	dp->b_actl->av_forw = bp;
    else
	dp->b_actf = bp;
    bp->av_forw = (struct buf *)0;
    dp->b_actl = bp;
    dp->qcnt++;
}

static void
unqueue_request(buf_ext)
register SCSI_BUF_EXT *buf_ext;
{
    register struct iobuf *dp;
    register struct buf *bp;
    register struct scsi_unit *un;
    register struct partition_table *pt;
    int sc;

    dp = &scsi_noproc;
    if (!(bp = dp->b_actf)) {
	put_bext(buf_ext);
	return;
    }
    dp->b_actf = bp->av_forw;
    dp->qcnt--;
    un = common_scsi_getun(bp->b_dev);
    pt = &un->un_vh_k1ptr->vh_pt[FS(bp->b_dev)];
    sc = (int)bp->av_back;
    bp->av_back = (struct buf *)buf_ext;
    sortit(un,bp,pt,sc);
}

static void
sortit(un,bp,pt,sc)
register int sc;
register struct buf *bp;
register struct scsi_unit *un;
register struct partition_table *pt;
{
    register struct iobuf *dp;
    register long amount, count;
    register SCSI_ALIGN *al;

    bp->b_error = 0;  /* zero out retry count */
    /* update accounting for this buffer */
    bp->b_start = lbolt;
    un->un_iotime->io_cnt++;
    un->un_iotime->io_bcnt += sc;

    if (!(bp->b_flags & B_SPL) && (un->un_flags & INT_DISK))
	/* save physical block number for SG setup rd/wr combining */
	PBLKNO = (unsigned int)(bp->b_blkno + pt->pt_firstlbn);
    else
	PBLKNO = 0;
    RESIDUAL = 0;
    /* queue request */
    if(bp->b_flags & B_SPL)
	amount = 0;
    else {	
    if (amount = bp->b_bcount % un->un_sectsize)
	amount = (un->un_sectsize - amount); /* round up */
    }
    if (amount || ((long)bp->b_dmaaddr & un->un_dmastartmask)) {
unaligned:
	count = sizeof(struct align_info) + amount + bp->b_bcount
		+ un->un_dmaalign;
	if ((al = (SCSI_ALIGN *)kmemalloc(count,0,M_NOWAIT)) == NULL) {
	    cmn_err(CE_CONT,
		"SCSI %dL%d: Could not kmemalloc %d bytes for alignment\n",
		un->un_target,un->un_lun,count);
	    put_bext(bp->av_back);
	    put_kmq(bp);
	    return;
	}
	al->al_size = (unsigned long)bp->b_bcount;
	al->al_tbuff = al->al_tbuffer;
	if (un->un_dmaalign)
	    al->al_tbuff = (char *)((u_long)(al->al_tbuff + un->un_dmaalign - 1)
			& ~(un->un_dmaalign - 1));
	ALGNPTR = al;
    }
    dp = un->un_dp;
    disksort(dp, bp, un->un_lastcyl);
    if (dp->b_active == 0) {
	    common_scsi_start(un, dp);
    }
}

/*
 * setup a device operation
 */
static void
common_scsi_start(un, dp)
struct scsi_unit *un;
register struct iobuf *dp;
{
    register struct scsi_iopb *ip;
    register struct buf *bp;
    struct volume_header *vh;
    struct partition_table *pt;
    int s;

    if ((bp = dp->b_actf) == NULL) { /* no work for this drive? */
	dp->b_active = 0;
	return;
    }
    s = splbio();
    if (dp->b_active == 1) { /* already active? */
	splx(s);
	return;
    } else dp->b_active = 1; /* say the drive is busy (iopb submitted) */
    splx(s);
    vh = un->un_vh_k1ptr;
    pt = &vh->vh_pt[FS(bp->b_dev)];
    /*
     * initialize information for data transfer and start command
     */
    if (!(bp->b_flags & B_SPL)) {
	if (bp->b_flags & B_READ) {
	    un->un_flags |= INT_READ;
	    un->un_command = C0_READ;
	} else  {
	    un->un_flags |= INT_WRITTEN;
	    un->un_command = C0_WRITE;
	}
    } else {
        ip = un->un_iopbp;
        ip->scsi_count0 = ip->scsi_count1 = 0;
        ip->scsi_bufaddr = ip->scsi_bufaddr0 = ip->scsi_bufaddr1 = 0;
        un->un_command = bp->b_length;
    }
    if (un->un_flags & INT_DISK) {
	un->un_lastcyl = bp->b_cylin; /* save the cyl we're on */
	un->un_bn = bp->b_blkno + pt->pt_firstlbn;
    }
    /* this could be the second time through for this bp! */
    if (RESIDUAL) { /* saved residual */
	bp->b_resid = RESIDUAL;
	un->un_bn += ((bp->b_bcount - bp->b_resid) >> SCTRSHFT);
	RESIDUAL = 0;
    } else
	bp->b_resid = bp->b_bcount;
    dp->io_start = lbolt;
    common_scsi_command(un, bp);
}

/*
 * issue a data transfer command
 */
static void
common_scsi_command(un, bp)
register struct scsi_unit *un;
register struct buf *bp;
{
    /*
     * save current state for software retry if command fails
     */
    un->un_prev_b_resid = bp->b_resid;
    un->un_prev_bn = un->un_bn;
    un->un_prev_secs = 0;

    if (bp->b_flags & B_SPL) {
	(void)doscsicmd(un,bp->b_dev,bp->b_bcount,
			bp->b_un.b_addr,bp->b_blkno,INTERRUPT);
	bp->b_resid = 0;
	return;
    }
    if (bp->b_resid == 0)
	return;
    common_scsi_sgsetup(un, bp);
    /*
     * start controller on command
     */
    (void)(*LOW_SCSI(bp->b_dev,startop))(un, INTERRUPT);
}

/*
 * setup iopb with scatter/gather entries for data transfer
 */
static void
common_scsi_sgsetup(un, bp)
register struct scsi_unit *un;
register struct buf *bp;
{
    register struct scsi_iopb *ip;
    register long amount;
    register int links, r_w;
    register long dmaaddr, physaddr, total, offset, t_resid, temp;
    register struct buf *nbp;
    register u_int cbn;	/* block number of current bp	*/
    register struct iobuf *dp;
    register SCSI_ALIGN *al;
    int read_write, count;
    int abnormal;

    abnormal = 0;
    dp = un->un_dp;
    current_bp = (int)bp; /* save bp for interrupt handler!  */
    if (bp == dp->b_actf) /* only unlink ourself */
	    dp->b_actf = bp->av_forw; /* unlink */
    r_w = (bp->b_flags & B_READ) ? DMA_READ : DMA_WRITE;
    ip = un->un_iopbp;
    ip->scsi_count0 = ip->scsi_count1 = 0;
    ip->scsi_bufaddr = ip->scsi_bufaddr0 = ip->scsi_bufaddr1 = 0;

    cbn = PBLKNO; /* (b_count-b_resid) handled in common_scsi_intr */
    un->un_sgentries = 0;	 /* reset */

    if (al = ALGNPTR) {
	if (amount = bp->b_bcount % un->un_sectsize)
	    amount = un->un_sectsize - amount;
	al->al_uaddr = bp->b_dmaaddr;
	if (!(bp->b_flags & B_READ) && (bp->b_bcount == bp->b_resid)) {
	    dmaaddr = (int)al->al_tbuff;
	    offset = poff(dmaaddr);
	    total = al->al_size;
	    while (total) {
		count = total;
		if ((count + offset) > NBPP) {
		    count = NBPP - offset;
		    offset = 0;
		}
		bcopy(al->al_uaddr,K2_TO_K1(dmaaddr),count);
		al->al_uaddr += count;
		dmaaddr += count;
		total -= count;
	    }
	    al->al_uaddr = bp->b_dmaaddr;
	}
	bp->b_dmaaddr = (caddr_t)al->al_tbuff;
	bp->b_bcount += amount;
	bp->b_resid += amount;
	abnormal = 1;
    }
    /*
     * fill in scatter gather structs
     */
    dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
    links = total = 0;
    if (temp = dmaaddr & un->un_dmaaddmask)
	temp = un->un_dmaaddmask  + 1 - temp;
    if (temp) {
	ip->scsi_bufaddr0 = (int)dmaaddr;
	ip->scsi_count0 = (int)temp;
	bp->b_resid -= temp;
	dmaaddr += temp;
	abnormal = 1;
    }
    read_write = bp->b_flags & B_READ;/* set reading or writing for combining */
lbpl:
    if (!abnormal)
    	dmaaddr = (long)bp->b_dmaaddr + bp->b_bcount - bp->b_resid;
    ip->scsi_bufaddr = (int)dmaaddr;
    do {
	/* limit this sge to this page */
	amount = bp->b_resid;
	offset = poff(dmaaddr);
	if (offset + amount > NBPP)
	    amount = NBPP - offset;
	if (temp = (amount & un->un_dmacntmask)) {
	    bp->b_resid -= temp;
	    amount -= temp;
	    ip->scsi_bufaddr1 = (int)(dmaaddr + amount);
	    ip->scsi_count1 = (int)temp;
	    if (amount == 0) {
		abnormal = 1;
		break;
	    }
	}
	/* fill in sg struct */
	physaddr = K2_TO_PHYS(dmaaddr);
	ASSERT(physaddr != offset);
	if (++links == un->un_maxsg) { /* save one sge */
	    if (temp = (bp->b_resid - amount) % un->un_sectsize) {
		temp =  un->un_sectsize - temp;
		amount -= temp;
	    }
	}
	if ((*LOW_SCSI(bp->b_dev,setupdma))(un,SG,r_w,amount,physaddr,
		un->un_sgentries))
		un->un_sgentries++;
	bp->b_resid -= amount;
	ASSERT(!(bp->b_resid & 0xff000000)); /* unsigned int */
	dmaaddr += amount;
	total += amount;
	if (links == un->un_maxsg) { /* save one sge */
	    abnormal = 1;
	    break; /* out of while */
	}
	if ((machine_type == BRDTYPE_I2000) &&  (amount != (NBPP-offset)))
	    abnormal = 1;
    } while (bp->b_resid);
    if (!abnormal) {
	/*
	 * If there is room on the scatter/gather list, check
	 * and see if there are a number of contiguous reads/writes
	 * in this unit queue.  If there are, combine them into
	 * one iopb transfer, using the s/g list for all of the
	 * bp's.
	 */
	nbp = bp->av_forw;
	if (nbp) {
	   if((!bp->b_error) &&			/* retry? */
	     ((nbp->b_flags & B_READ) == read_write) &&	/* nbp follows r/w? */
 	     (!nbp->b_dkspare) &&		/* residual */
	     (!nbp->b_error) &&			/* retry? */
	     /* is current physical block + sector count == next phys blk? */
	     ((cbn + (bp->b_bcount >> SCTRSHFT)) == N_PBLKNO) &&
	     /* test bcount and address */
	     (!((long)nbp->b_dmaaddr & un->un_dmastartmask)) &&
	     (!(nbp->b_bcount % un->un_sectsize))) {

		if (machine_type == BRDTYPE_I2000) {
		    dmaaddr = (long)nbp->b_dmaaddr + nbp->b_bcount 
						   - nbp->b_resid;
		    offset = poff(dmaaddr);
		    if (offset)
			abnormal = 1;
		}
		if (!abnormal) {
		    bp = nbp;
		    dp->b_actf = bp->av_forw; /* unlink */
		    bp->b_resid  = bp->b_bcount;
		    cbn = PBLKNO;
		    goto lbpl;
		}
	   }
	}
    }
    bp->av_forw = NULL;				/* end lbp list	*/
    total += (ip->scsi_count0 + ip->scsi_count1);
    if (!(un->un_flags & INT_VARIABLE))
	amount = total >> SCTRSHFT;	/* convert to sectors */
    if (!(un->un_flags & INT_TAPE)) {
	un->un_bn += amount;
	un->un_prev_secs = amount;
    }
    setupiopb(un->un_command,0,un,INTERRUPT,total,un->un_prev_bn);
}

/*
 * read from device
 */
void
common_scsi_read(dev)
dev_t dev;
{
    register struct scsi_unit *un;
    struct volume_header *vh;
    struct partition_table *pt;
    register int partition;

    if ((un = common_scsi_getun(dev)) == NULL) {
	ERR_RETURN(EIO);
    }
    partition = FS(dev);
    vh = un->un_vh_k1ptr;
    pt = &vh->vh_pt[FS(dev)];
    if (!(un->un_flags & INT_VARIABLE) && (u.u_offset % un->un_sectsize)) {
	ERR_RETURN(EIO);
    }
    if (un->un_flags & INT_TAPE) {
	if (!(un->un_flags & INT_VARIABLE) && un->un_flags & INT_WRITTEN) {
		/* can't read a 'written' tape */
		ERR_RETURN(EINVAL);
	}
    } else {
	if (un->un_vhvalid == 0) {
	    if (partition == PTNUM_VOLUME)
		pt->pt_nblks = un->un_readcap.maxblk;
	    else
		pt->pt_nblks = 0;
	}
	if (!physck(pt->pt_nblks, B_READ))
	    return;
    }
    physio(common_scsi_strategy, 0, dev, B_READ);
}

/*
 * write to device
 */
void
common_scsi_write(dev)
dev_t dev;
{
    register struct scsi_unit *un;
    struct volume_header *vh;
    struct partition_table *pt;
    register int partition;

    if ((un = common_scsi_getun(dev)) == NULL) {
	ERR_RETURN(EIO);
    }
    partition = FS(dev);
    vh = un->un_vh_k1ptr;
    pt = &vh->vh_pt[FS(dev)];
    if (!(un->un_flags & INT_VARIABLE) && (u.u_offset % un->un_sectsize)) {
	ERR_RETURN(EIO);
    }
    if (un->un_flags & INT_TAPE) {
	if (!(un->un_flags & INT_VARIABLE) && un->un_flags & INT_READ) {
		/* can't write a 'read' tape */
		ERR_RETURN(EINVAL);
	}
    } else {
	if (un->un_vhvalid == 0) {
	    if (partition == PTNUM_VOLUME)
		pt->pt_nblks = un->un_readcap.maxblk;
	    else
		pt->pt_nblks = 0;
	}
	if (!physck(pt->pt_nblks, B_WRITE))
	    return;
    }
    physio(common_scsi_strategy, 0, dev, B_WRITE);
}
#endif STANDALONE

static int
doscsicmd(un,dev,count,addr,block,mode)
register struct scsi_unit *un;
int dev,count,addr,block,mode;
{
    register int r_w;
    register int temp, total, amount, dmaaddr, phys, offset, entry = 0;
    register struct scsi_iopb *ip;

    switch (un->un_command) {
	case C0_COPY:
	case C0_FORMAT:
	case C0_MODESEL:
	case C0_REASSIGN:
	case C0_SENDDIAG:
	case C0_WRITE:
	case C1_WRITE:
	case C1_WRVERIFY:
	case C1_VERIFY:
	case C1_DAHIGH:
	case C1_DAEQUAL:
	case C1_DALOW:
	    r_w = DMA_WRITE;
	    break;
	default:
	    r_w = DMA_READ;
	    break;
    }
    total = count;
    dmaaddr = addr;
    offset = poff(dmaaddr);
    while (amount = total) {
	if ((amount + offset) > NBPP) {
	    amount = NBPP - offset;
	    offset = 0;
	}
	if (IS_KSEG0(dmaaddr))
	    phys = K0_TO_PHYS(dmaaddr);
	else if (IS_KSEG1(dmaaddr))
	    phys = K1_TO_PHYS(dmaaddr);
	else if (IS_KSEG2(dmaaddr))
	    phys = K2_TO_PHYS(dmaaddr);
	else
	    phys = dmaaddr;
	if ((*LOW_SCSI(dev,setupdma))(un,NOSG,r_w,amount,phys,entry))
		entry++;
	dmaaddr += amount;
	total -= amount;
    }
    setupiopb(un->un_command,addr,un,mode,count,block);
    return((*LOW_SCSI(dev,startop))(un, mode));
}

/*
 * ioctl routine
 */
#ifdef STANDALONE
common_scsi_ioctl(io, dev, cmd, arg)
register struct iob *io;
#else STANDALONE
void
common_scsi_ioctl(dev, cmd, arg)
#endif STANDALONE
register dev_t dev;
register unsigned int cmd;
register caddr_t arg;
{
    register u_short *cp;
    SCSI_MS_ERROR *err;
    SCSI_MS_GEOM *geom;
    SCSI_MS_FORMAT *fmt;
    SCSI_MS_RDAHD *rhp;
    SCSI_MS_CACHE *chp;
    struct scsi_unit *un;
    struct gioctl gioctl;
    struct ctlr_info ct;
    struct fmt_map_info fmt_info;
    GEOMETRY_INFO geom_info;
    struct volume_header vh;
    struct volume_header *ipv;
    struct io_arg io_arg;
    struct device_parameters *devp;
    int i, count;
    u_int status, addr;

    if ((un = common_scsi_getun(dev)) == NULL) {
	ERR_RETURN(EIO);
    }
#ifndef STANDALONE
    if ((un->un_flags & INT_TAPE) && 
	!(cmd == GIOCPRSTR || cmd == DIOCGETCTLR || cmd == DIOCREMOVE)) {
	tapeioctl(un, cmd, arg);
	return;
    }
#endif STANDALONE

    ipv = un->un_vh_k1ptr;
    devp = &ipv->vh_dp;
    status = 0;
    switch (cmd) {
	case DIOCPARTITION: {
		partition_usage status[NPARTAB],x;

		if (!un->un_vhvalid) {
		 	ERR_RETURN(ENXIO);
		}
		for(i = 0; i < NPARTAB; i++) {
			x = un->un_open[i];

#define DEV_EQ(x,y,i)	(major(x) == major(y) && TARGET(x) == TARGET(y) && \
			FS(y) == i)

			if(DEV_EQ(dev,rootdev,i))
				x.usage.rootdev = 1;
			if(DEV_EQ(dev,swapdev,i))
				x.usage.swapdev = 1;
			status[i] = x;
		}
	        if(copyout((caddr_t)&status[0], (caddr_t) arg, sizeof(status)))
			ERR_RETURN(EFAULT);
		goto ioctl_done;
	}
	case GIOCPRSTR:
	    /* Need to return scsi inquiry string */
	    common_scsi_string(un, gioctl.gi_ident, SP_WAIT);
	    if(copyout((caddr_t)gioctl.gi_ident, (caddr_t) arg, IDENT_LEN))
		ERR_RETURN(EFAULT);
	    goto ioctl_done;

	case DIOCVFYSEC:
	    /*
	     * verify sectors are readable
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (!io_arg.datasz)
		status = DIOC_BADSIZE;
	    else if (common_scsi_spcmd(un,C1_VERIFY,io_arg.sectst,
						io_arg.datasz,0,SP_WAIT))
		status = DIOC_OPERR;
	    break;

	case DIOCGETCTLR:
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(ct)) {
		status = DIOC_BADSIZE;
	    } else {
		bzero(&ct,sizeof(ct));
		ct.ci_flags = DP_SCSI;
		switch (machine_type) {
		    case BRDTYPE_R2400:
		    case BRDTYPE_M180:
		        strncpy(ct.ci_type,"MB87030CR-8 SPC",CITYPESIZE);
			break;
		    case BRDTYPE_I2000:
		        strncpy(ct.ci_type,"AIC-6250",CITYPESIZE);
			break;
		    case BRDTYPE_R3030:
		    case BRDTYPE_RB3125:
		        strncpy(ct.ci_type,"NCR-53C94",CITYPESIZE);
			break;
		    default:
		        strncpy(ct.ci_type,"Unknown Controller",CITYPESIZE);
			break;
		}
		if (copyout((caddr_t)&ct, io_arg.memaddr, sizeof(ct)) < 0)
		    status = DIOC_EFAULT;
	    }
	    break;
	case DIOCGETVH:
	    /* 
	     * get volume header - this routine can be called from
	     * interrupt level so it CAN NOT sleep - we must use
	     * a POLLED command
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(vh)) {
		status = DIOC_BADSIZE;
	    } else {
		if (!un->un_vhvalid) {
		    status = DIOC_NOTVOLHDR;
		} else if (copyout((caddr_t)ipv,(caddr_t) io_arg.memaddr,
			(int) sizeof(vh)) < 0) {
		    status = DIOC_EFAULT;
		}
	    }
	    break;

	case DIOCSETVH:
	case DIOCINITVH:
	case DIOCRECONFIG:
	    /*
	     * init driver's idea of drives volume header information
	     */
	    if (suser() == 0)
		ERR_RETURN(EPERM);
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(vh)) {
		status = DIOC_BADSIZE;
	    } else {
		if (copyin((caddr_t)io_arg.memaddr,(caddr_t)&vh,
			sizeof(vh)) < 0) {
		    status = DIOC_EFAULT;
/*	We should at least do the following test in the next release 

		} else if(bcmp(&vh.vh_pt[0],&ipv->vh_pt[0],sizeof(vh.vh_pt)) && scsi_dev_busy(un,dev)) {
			status = DIOC_DISKBUSY;

*/
		} else if (!is_vh(&vh)) {
		    status = DIOC_NOTVOLHDR;
		} else {
		    bcopy(&vh, ipv, sizeof(vh));
		    un->un_vhvalid = 1;
		    if (cmd == DIOCRECONFIG) {
			for (i = 0; i < (devp->dp_trks0 * devp->dp_secs);
					i+= devp->dp_secs) {
			    if (common_scsi_spcmd(un,C0_WRITE,i,512,ipv,
								 SP_WAIT)) {
				status = DIOC_OPERR;
				cmn_err(CE_CONT,
		    "SCSI %dL%d: Could not write Volume Header on track %d\n",
				    un->un_target,un->un_lun,i);
			    }
			}
		    }
		}
	    }
	    break;

	case DIOCDISKCACHE:
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (un->un_flags & INT_PC8) {
		rhp = (SCSI_MS_RDAHD *)K0_TO_K1(&un->un_msrdahd);
		if (io_arg.sectst) 
		    rhp->rcd = 0;		/* enable cache */
		else
		    rhp->rcd = 1;		/* disable cache */
		rhp->msense.hdr.sense_data_len = 0;
		rhp->rdahd_hdr.ps = 0;
		if (common_scsi_spcmd(un,C0_MSELECT,PD_RDAHD,
				sizeof(SCSI_MS_RDAHD),rhp,SP_WAIT))
		    status = DIOC_OPERR;
	    } else {
		chp = (SCSI_MS_CACHE *)K0_TO_K1(&un->un_mscache);
		if (io_arg.sectst) 
		    chp->ce = 1;		/* enable cache */
		else
		    chp->ce = 0;		/* disable cache */
		chp->msense.hdr.sense_data_len = 0;
		chp->cache_hdr.ps = 0;
		if (common_scsi_spcmd(un,C0_MSELECT,PD_CACHE,
				sizeof(SCSI_MS_CACHE),chp,SP_WAIT))
		    status = DIOC_OPERR;
	    }
	    break;

	case DIOCDISKGEOM:
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(geom_info)) {
		status = DIOC_BADSIZE;
	    } else {
		geom = (SCSI_MS_GEOM *)K0_TO_K1(&un->un_msgeom);
		fmt = (SCSI_MS_FORMAT *)K0_TO_K1(&un->un_msformat);
		geom_info.geom_cyl = (geom->cyls_hb << 16) | 
			(geom->cyls_mb << 8) | geom->cyls_lb;
		geom_info.geom_head = geom->heads;
		geom_info.geom_spt = fmt->spt;
		geom_info.geom_bps = fmt->bps;
		geom_info.geom_tpz = fmt->tpz;
		geom_info.geom_aspz = fmt->aspz;
		geom_info.geom_atpz = fmt->atpz;
		geom_info.geom_atpv = fmt->atpv;
		geom_info.geom_ilv = fmt->ilv;
		geom_info.geom_tsf = fmt->tsf;
		geom_info.geom_csf = fmt->tsf;
		if (copyout((caddr_t)&geom_info,(caddr_t)io_arg.memaddr,
			(int) io_arg.datasz) < 0)
		    status = DIOC_EFAULT;
	    }
	    break;

	case DIOCFMTMAP:
	    /*
	     * perform format operation.
	     * must be superuser and partition cannot be currently mounted.
	     */
	    if (suser() == 0)
		ERR_RETURN(EPERM);
	    if ((un->un_flags & INT_DISK) == 0) {
		cmn_err(CE_CONT,"SCSI %dL%d: unit is NOT a disk drive\n",
			un->un_target,un->un_lun);
		ERR_RETURN(EFAULT);
	    }
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(fmt_info)) {
		status = DIOC_BADSIZE;
	    } else if (scsi_dev_busy(un,dev)) {
		status = DIOC_DISKBUSY;
	    } else if (copyin((caddr_t)io_arg.memaddr, (caddr_t) &fmt_info,
			sizeof(fmt_info)) < 0) {
		status = DIOC_EFAULT;
	    } else {
		status = common_scsi_format(un, &fmt_info);
	    }
	    break;

	case DIOCSEEK:
	    /*
	     * seek to sector
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (common_scsi_spcmd(un,C0_SEEK,io_arg.sectst,0,0,SP_WAIT))
		status = DIOC_OPERR;
	    break;

	case DIOCREMOVE:
	    /*
	     * setup disk for removal
	     */
	    if (suser() == 0)
		ERR_RETURN(EPERM);
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (scsi_dev_busy(un,dev)) {
		status = DIOC_DISKBUSY;
	    } else {
		un->un_flags |= INT_CLOBBER;
	    }
	    break;

	case DIOCWRTVFY:
	    /*
	     * set/reset write verify flag
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    un->un_flags &= ~(INT_WRTVFY|INT_BYTVFY);
	    i = io_arg.sectst;
	    if (i)
		    un->un_flags |= INT_WRTVFY;
	    if (i > 1)
		    un->un_flags |= INT_BYTVFY;
	    break;

	case DIOCNOECC:
	    /*
	     * enable/disable ecc correction
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    err = (SCSI_MS_ERROR *)K0_TO_K1(&un->un_mserror);
	    err->per = 1;		/* post errors */
	    if (io_arg.sectst & 2)
		un->un_nretries = 0;
	    else
		un->un_nretries = NRETRIES;
	    if (io_arg.sectst & 1) {
		err->dcr = 1;		/* disable correction */
		err->retry_cnt = 0;	/* no retries */
	    } else {
		i = devp->dp_nretries;
		err->dcr = 0;		/* enable correction */
		err->retry_cnt = i ? i : 27;	/* retry count */
	    }
	    err->msense.hdr.sense_data_len = 0;
	    err->error_hdr.ps = 0;
	    i = err->error_hdr.page_length + sizeof(SCSI_MODE_SENSE) +
		sizeof(SCSI_MS_PAGE_HDR);
	    if (common_scsi_spcmd(un,C0_MSELECT,PD_ERROR,i,err,SP_WAIT))
		status = DIOC_OPERR;
	    break;

	case DIOCSOFTCNT:
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    io_arg.retval = un->un_softcount;
	    if (io_arg.sectst)
		un->un_softcount = io_arg.datasz;
	    if (copyout((caddr_t)&io_arg,(caddr_t)arg,sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    goto ioctl_done;
	case DIOCRDCAP:
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    if (io_arg.datasz != sizeof(un->un_readcap))
		status = DIOC_BADSIZE;
	    else if (copyout((caddr_t)&un->un_readcap,(caddr_t)io_arg.memaddr,
		    (int) io_arg.datasz) < 0)
		status = DIOC_EFAULT;
	    break;
	case DIOCRDEFECTS:
	    /*
	     * read defect information off the specified track into
	     * temporary buffer. we only want to return the defect
	     * information to the caller.
	     */
	    if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		ERR_RETURN(EFAULT);
	    
	    if (io_arg.datasz == 0) {
		status = DIOC_BADSIZE;
		break;
	    }
	    if (io_arg.sectst > ALL_DEFECTS) {
		status = DIOC_EINVAL;
		break;
	    }
	    i = io_arg.datasz;
	    /* round up size */
	    if (un->un_dmaalign)
		i = (i + un->un_dmaalign - 1) & ~(un->un_dmaalign - 1);
	    count = i + un->un_dmaalign;
#ifdef STANDALONE
            addr = (u_int)align_malloc(count,un->un_dmaalign);
#else STANDALONE
            addr = (u_int)kmemalloc(count,0,M_WAITOK|M_CONTIGUOUS);
	    if (!addr) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: DIOCRDEFECTS Could not kmemalloc %d bytes\n",
		    un->un_target,un->un_lun,count);
		status = DIOC_EFAULT;
		break;
	    }
#endif STANDALONE
	    cp = (u_short *)K2_TO_K1(addr);
	    /* align starting address */
	    if (un->un_dmaalign)
		cp = (u_short *)(((u_long)cp + un->un_dmaalign - 1)
				    & ~(un->un_dmaalign - 1));
	    if (common_scsi_spcmd(un,C1_READDEF,io_arg.sectst,
				i,cp,SP_WAIT)) {
		status = DIOC_OPERR;
	    } else {
		i = (int)cp[1] + 4;
		if (i > io_arg.datasz)
		    i = io_arg.datasz;
		if (copyout((caddr_t)cp,
			(caddr_t)io_arg.memaddr,i) < 0) {
		    status = DIOC_EFAULT;
		}
	    }
#ifndef STANDALONE
	    kmemfree(addr,0,M_WAITOK);
#endif STANDALONE
	    break;

	case GIOCGETVAL:
	    if (suser() == 0)
		ERR_RETURN(EPERM);
	    if (copyin(arg, (caddr_t) &gioctl, sizeof(gioctl)) < 0)
		ERR_RETURN(EFAULT);
	    
	    switch (gioctl.gi_page) {
		case PD_ERROR:
		    i = sizeof(SCSI_MS_ERROR);
		    break;
		case PD_DISREC:
		    i = sizeof(SCSI_MS_DISREC);
		    break;
		case PD_FORMAT:
		    i = sizeof(SCSI_MS_FORMAT);
		    break;
		case PD_GEOM:
		    i = sizeof(SCSI_MS_GEOM);
		    break;
		case PD_RDAHD:
		    i = sizeof(SCSI_MS_RDAHD);
		    break;
		case PD_CACHE:
		    i = sizeof(SCSI_MS_CACHE);
		    break;
		default:
		    i = sizeof(SCSI_MODE_SENSE);
		    break;
	    }
	    if (gioctl.gi_size < i) {
		status = GIOC_BADSIZE;
		goto gioctl_done;
	    }
	    count = gioctl.gi_size + un->un_dmaalign;
#ifdef STANDALONE
            addr = (u_int)align_malloc(count,un->un_dmaalign);
#else STANDALONE
            addr = (u_int)kmemalloc(count,0,M_WAITOK);
	    if (!addr) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: GIOCGETVAL Could not kmemalloc %d bytes\n",
		    un->un_target,un->un_lun,count);
		status = GIOC_EFAULT;
		break;
	    }
#endif STANDALONE
	    cp = (u_short *)K2_TO_K1(addr);
	    if (un->un_dmaalign)
		cp = (u_short *)(((u_long)cp + un->un_dmaalign - 1)
				    & ~(un->un_dmaalign - 1));
	    if (common_scsi_spcmd(un,C0_MSENSE,gioctl.gi_page,
				gioctl.gi_size,cp,SP_WAIT)) {
		status = GIOC_OPERR;
	    } else {
		if (copyout((caddr_t)cp,(caddr_t)gioctl.gi_addr,
				gioctl.gi_size) < 0) {
		    status = GIOC_EFAULT;
		}
	    }
	    goto gioctl_done1;

	case GIOCSETVAL:
	    if (suser() == 0)
		ERR_RETURN(EPERM);
	    if (copyin(arg, (caddr_t) &gioctl, sizeof(gioctl)) < 0)
		ERR_RETURN(EFAULT);
	    
	    if (gioctl.gi_size < sizeof(SCSI_MODE_SENSE)) {
		status = GIOC_BADSIZE;
		goto gioctl_done;
	    }
	    count = gioctl.gi_size + un->un_dmaalign;
#ifdef STANDALONE
            addr = (u_int)align_malloc(count,un->un_dmaalign);
#else STANDALONE
            addr = (u_int)kmemalloc(count,0,M_WAITOK);
	    if (!addr) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: GIOCSETVAL Could not kmemalloc %d bytes\n",
		    un->un_target,un->un_lun,count);
		status = GIOC_EFAULT;
		break;
	    }
#endif STANDALONE
	    cp = (u_short *)K2_TO_K1(addr);
	    if (un->un_dmaalign)
		cp = (u_short *)(((u_long)cp + un->un_dmaalign - 1)
				    & ~(un->un_dmaalign - 1));
	    if (copyin((caddr_t)gioctl.gi_addr,(caddr_t)cp,
				gioctl.gi_size) < 0) {
		status = GIOC_EFAULT;
		goto gioctl_done1;
	    }
	    err = (SCSI_MS_ERROR *)cp;
	    i = err->error_hdr.page_length + sizeof(SCSI_MODE_SENSE) +
		sizeof(SCSI_MS_PAGE_HDR);
	    if ((err->msense.hdr.blk_desc_len != sizeof(SCSI_MS_BLK_DESC)) ||
		(err->error_hdr.page_code != gioctl.gi_page) ||
		(i != gioctl.gi_size)) {
		status = GIOC_EINVAL;
		goto gioctl_done1;
	    }
	    err->msense.hdr.sense_data_len = 0;
	    err->error_hdr.ps = 0;
	    if (common_scsi_spcmd(un,C0_MSELECT,gioctl.gi_page,
				gioctl.gi_size,cp,SP_WAIT)) {
		status = GIOC_OPERR;
	    } else {
		switch (gioctl.gi_page) {
		    case PD_ERROR:
			i = min(gioctl.gi_size, sizeof(SCSI_MS_ERROR));
			bcopy(cp, &un->un_mserror, i);
			break;
		    case PD_DISREC:
			bcopy(cp, &un->un_msdisrec, gioctl.gi_size);
			break;
		    case PD_RDAHD:
			bcopy(cp, &un->un_msrdahd, gioctl.gi_size);
			break;
		    case PD_CACHE:
			bcopy(cp, &un->un_mscache, gioctl.gi_size);
			break;
		}
	    }
gioctl_done1:
#ifndef STANDALONE
	    kmemfree(addr,0,M_WAITOK);
#endif STANDALONE
gioctl_done:
	    gioctl.gi_retval = status;
	    if (copyout((caddr_t)&gioctl,arg,sizeof(gioctl)) < 0)
		    ERR_RETURN(EFAULT);
	    if (status)
		ERR_RETURN(EIO);
	    goto ioctl_done;

	case DIOCDIAG:
	    /*
	     * issue diagnostic command to controller
	     */
	default:
	    ERR_RETURN(EINVAL);
    }
    io_arg.retval = status;
    if (copyout((caddr_t)&io_arg,arg,sizeof(io_arg)) < 0)
	    ERR_RETURN(EFAULT);
    if (status)
	ERR_RETURN(EIO);
ioctl_done:
#ifdef STANDALONE
    return(0);
#else STANDALONE
    u.u_error = 0;
    return;
#endif STANDALONE
}

#ifndef STANDALONE
static void
tapeioctl(un, cmd, data)
register struct scsi_unit *un;
caddr_t data;
{
    register struct buf *bp;
    int status, command, count, code = 0;
    struct mtop *mtop;
    struct mtget *mtget;

    bp =  &un->un_buf;
    bp->b_flags = 0;
    switch (cmd) {

	case MTIOCTOP:	/* tape operation */
	    mtop = (struct mtop *)data;
	    if (un->un_eomcount > MAXEOM) {
		switch (mtop->mt_op) {
		    case MTREW:
		    case MTOFFL:
		    case MTRET:
		    case MTONL:
			break;
		    default:
			bp->b_error = EIO;
			goto ioctlerr;
		}
	    }
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
		    un->un_flags &= ~(INT_WRITTEN|INT_RFM|INT_FM|INT_EOM);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;

		case MTOFFL: 
		    if (status = common_scsi_spcmd(un,C0_REWIND,0,0,0,SP_WAIT))
			goto ioctlerr;
		    count = 0; /* unload tape */
		    un->un_command = C0_LOAD;
		    un->un_flags &= ~(INT_WRITTEN|INT_RFM|INT_FM|INT_EOM);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTRET: 
		    count = 3; /* retension and load tape */
		    un->un_command = C0_LOAD;
		    un->un_flags &= ~(INT_WRITTEN|INT_RFM|INT_FM|INT_EOM);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTONL: 
		    count = 1; /* load tape */
		    un->un_command = C0_LOAD;
		    un->un_flags &= ~(INT_WRITTEN|INT_RFM|INT_FM|INT_EOM);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTAPP: 
		    if (status = common_scsi_spcmd(un,C0_LOAD,0,1,0,SP_WAIT))
			goto ioctlerr;
		    count = 0;
		    code = PHYSEOM;
		    un->un_command = C0_SPACE;
		    break;
		case MTNOP: 
		case MTRST:
		    ERR_RETURN(EINVAL);

		default:
		    ERR_RETURN(EINVAL);
	    } /* end of switch */

	    status = common_scsi_spcmd(un,un->un_command,code,count,0,SP_WAIT);
	    if ((mtop->mt_op==MTFSR || mtop->mt_op==MTBSR) && bp->b_resid) {
		ERR_RETURN(EIO);
	    }
	    if (bp->b_flags & B_ERROR) {
ioctlerr:
		if (bp->b_error != EIO)
		    cmn_err(CE_CONT,"tape ioctl: error, bp->b_error= 0x%x\n",
			    bp->b_error);
		ERR_RETURN(EIO);
	    }
	    /* in this case we're no longer at EOM */
	    if (mtop->mt_op==MTBSR || mtop->mt_op==MTBSF)
		un->un_flags &= ~INT_EOM;
	    break;

	case MTIOCGET:
	    mtget = (struct mtget *)data;
	    mtget->mt_type = MT_M120;
	    mtget->mt_dsreg = un->un_flags;
	    mtget->mt_erreg = un->un_flags>>16;
	    mtget->mt_resid = 0;
	    break;
	default:
	    ERR_RETURN(EINVAL);
    }
}
#endif STANDALONE

void
common_scsi_string(un, pp, mode)
struct scsi_unit *un;
register char *pp;
int mode;
{
    register SCSI_INQUIRY *inq;
    register SCSI_RDBLKLIM *rb;
    register len, i, j;
    register char ch, *q;
    INQ_FIELDS inq_fields[7];
    register INQ_FIELDS *ifp;
    char *spaces = "  ";

    inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inq);

    switch (inq->device_type) {
	case TYPE_DISK:
		if (inq->rm)
			q = "REMOVABLE DISK";
		else
			q = "DISK";
		break;
	case TYPE_TAPE:
		rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
                (void)common_scsi_spcmd(un,C0_RDBLOCK,0,6,rb,mode);
		if (rb->maxlen != rb->minlen)
			q = "VARIABLE TAPE";
		else
			q = "FIXED TAPE";
		break;
	case TYPE_PRINTER:
		q = "PRINTER";
		break;
	case TYPE_CPU:
		q = "PROCESSOR";
		break;
	case TYPE_WORM:
		q = "WORM DISK";
		break;
	case TYPE_RONLY_DISK:
		q = "ROM DISK";
		break;
	case TYPE_SCANNER:
		q = "SCANNER";
		break;
	case TYPE_OPTICAL:
		q = "OPTICAL";
		break;
	case TYPE_CHANGER:
		q = "MEDIUM CHANGER";
		break;
	case TYPE_COMM:
		q = "COMMUNICATIONS";
		break;
	case TYPE_M12_FLOPPY:
		q = "Rx2030 FLOPPY";
		break;
	default:
		q = "UNKNOWN TYPE";
		break;
    }
    for (i = 0; i < 7; ++i)
	inq_fields[i] = common_scsi_inq_fields[i];
    inq_fields[0].dp = inq->vendor_id;
    inq_fields[1].dp = inq->product_id;
    inq_fields[2].dp = inq->revision_level;
    inq_fields[3].dp = inq->mcode_level;
    inq_fields[4].dp = inq->serial_nr;
    inq_fields[5].dp = q;

    len = inq->length;
    ifp = &inq_fields[0];

    while (ifp->dp) {
	if (len > ifp->offset)
	    j = len - ifp->offset;
	else
	    j = 0;
	q = ifp->dp;
	for (i = 0; i < ifp->size; i++) {
	    ch = *q & 0x7f;
	    if (ch == NULL || ch < ' ' || ch > '~' || i >= j)
		q = spaces;
	    *pp++ = *q++;
	}
	if ((++ifp)->dp)
	    *pp++ = '-';
    }
    *pp = NULL;
}

static int
common_scsi_format(un, fmi) 
register struct scsi_unit *un;
register struct fmt_map_info *fmi;
{
    register SCSI_MS_FORMAT *fmt;
    int status, i, *cp, *addr, count;

    status = 0;
    switch (fmi->fmi_action) {
	case FMI_MAP_TRACK:
	    if (fmi->fmi_intrlv == 0) {		/* list size */
		return(DIOC_BADSIZE);
	    }
	    count = fmi->fmi_intrlv + un->un_dmaalign;
#ifdef STANDALONE
	    addr = (int *)align_malloc(count,un->un_dmaalign);
#else STANDALONE
	    addr = (int *)kmemalloc(count,0,M_WAITOK);
	    if (!addr) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: FMI_MAP_TRACK Could not kmemalloc %d bytes\n",
		    un->un_target,un->un_lun,count);
		return(DIOC_EFAULT);
	    }
#endif STANDALONE
	    cp = (int *)K2_TO_K1(addr);
	    if (un->un_dmaalign)
		cp = (int *)(((u_long)cp + un->un_dmaalign - 1)
				    & ~(un->un_dmaalign - 1));
	    if (copyin(fmi->fmi_addr, (caddr_t) cp, fmi->fmi_intrlv) < 0) {
		status = DIOC_EFAULT;
	    } else if (common_scsi_spcmd(un,C0_REASSIGN,0,fmi->fmi_intrlv,
			cp,SP_WAIT))
		status = DIOC_OPERR;
#ifndef STANDALONE
	    kmemfree(addr,0,M_WAITOK);
#endif STANDALONE
	    break;
	case FMI_FORMAT_TRACK:
            fmt = (SCSI_MS_FORMAT *)K0_TO_K1(&un->un_msformat);
	    fmt->msense.hdr.sense_data_len = 0;
	    fmt->format_hdr.ps = 0;
	    fmt->tpz = fmi->fmi_tpz;
	    fmt->aspz = fmi->fmi_aspz;
	    fmt->atpv = fmi->fmi_atpv;
	    if (common_scsi_spcmd(un,C0_MSELECT,PD_FORMAT,
				sizeof(SCSI_MS_FORMAT),fmt,SP_WAIT)) {
		cmn_err(CE_WARN,
		    "SCSI %dL%d: Format could not select format modes\n",
		    un->un_target,un->un_lun);
		return(DIOC_OPERR);
	    }
	    if (common_scsi_spcmd(un,C0_FORMAT,0,fmi->fmi_intrlv,0,SP_WAIT)) {
		cmn_err(CE_WARN,"SCSI %dL%d: Could not format\n",
			un->un_target,un->un_lun);
		return(DIOC_OPERR);
	    }
	    addr = (int *)K0_TO_K1(&un->un_readcap);
	    if (common_scsi_spcmd(un,C1_READCAP,0,sizeof(SCSI_READCAP),
				addr,SP_WAIT)) {
		cmn_err(CE_CONT,"SCSI %dL%d: Cannot read capacity\n",
			    un->un_target,un->un_lun);
		return(DIOC_OPERR);
	    }
	    break;
	default:
	    status = DIOC_EINVAL;
    }
    return(status);
}

/*
** setupiopb() - Routine to set up the IOPB
*/
static void
setupiopb(cmd, physaddr, un, mode, count, lba)
int cmd, physaddr;
struct scsi_unit *un;
u_int mode, count, lba;
{
    register struct scsi_iopb *ip;
    register struct buf *bp;
    struct volume_header *vh;
    struct device_parameters *devp;

    ip = un->un_iopbp;
    ip->scsi_flags = MESSAGES|DISCON_RECON; /* init flags */
    ip->scsi_taskid = 0;		/* clear error bit */
    ip->scsi_un = un;
    if (mode & POLLED) {
	ip->scsi_hwstatus = 0xAA;	/* hardware status */
	ip->scsi_flags |= POLLED;	/* forces POLLED operation */
    } else {
	ip->scsi_hwstatus = 0;		/* reset hardware status */
    }
    ip->scsi_status = 0;		/* reset SCSI status */
    ip->scsi_target = un->un_target;	/* Target ID (0-6) */
    ip->scsi_lun = un->un_lun;		/* logical unit number (0-7) */
    ip->scsi_count = 0;
    ip->cmd_blk.cdb_raw[0] = ip->cmd_blk.cdb_raw[1] = ip->cmd_blk.cdb_raw[2] = 
	ip->cmd_blk.cdb_raw[3] = 0;
    ip->cmd_blk.cdb_0.cdb_0_cmd = cmd;	/* SCSI Command */
    ip->cmd_blk.cdb_0.cdb_0_lun = un->un_lun;
    switch (cmd) {
	case C1_READDEF:
	    ip->scsi_count = count - ip->scsi_count0 - ip->scsi_count1;
	    if (ip->scsi_count)
		ip->scsi_flags |= DMA_XFER; /* use dma xfer */	
	    else {
		ip->scsi_flags |= PTM_XFER; /* use ptm xfer */	
		if (!ip->scsi_count0) {
		    ip->scsi_count0 = ip->scsi_count1;
		    ip->scsi_count1 = 0;
		    ip->scsi_bufaddr0 = ip->scsi_bufaddr1;
		    ip->scsi_bufaddr1 = 0;
		}
	    }
	    lba = (lba & 0x3) << 3;				/* which list */
	    ip->cmd_blk.cdb_1.cdb_1_lba_h = lba | 0x5;	/* code */
	    ip->cmd_blk.cdb_1.cdb_1_len_h = MB(count);
	    ip->cmd_blk.cdb_1.cdb_1_len_l = LB(count);
	    ip->scsi_extra = un->un_extra;
	    return;
	case C0_REASSIGN:
	case C1_READCAP:
	case C0_RDBLOCK:
	    ip->scsi_bufaddr0 = physaddr;
	    ip->scsi_count0 = count;
	    count = 0;
	    break;
	case C0_MSENSE:
	    ip->scsi_bufaddr0 = physaddr;
	    ip->scsi_count0 = count;
	    count = (lba << 16) | count;		/* add page code */
	    break;
	case C0_MSELECT:
	    if (un->un_flags & INT_DISK)
		ip->cmd_blk.cdb_0.cdb_0_lba_h = PAGE_FORMAT | SAVE_PARAMS;
	case C0_REQSENSE:
	case C0_INQUIRY:
	    ip->scsi_bufaddr0 = physaddr;
	    ip->scsi_count0 = count;
	    break;
	case C0_SPACE:
	case C0_LOAD:
	    ip->cmd_blk.cdb_0.cdb_0_lba_h = lba;	/* code */
	    break;
	case C1_VERIFY:
	    ip->scsi_flags |= NO_XFER;
	    ip->cmd_blk.cdb_1.cdb_1_lba_h = XB(lba);
	    ip->cmd_blk.cdb_1.cdb_1_lba_mh = HB(lba);
	    ip->cmd_blk.cdb_1.cdb_1_lba_ml = MB(lba);
	    ip->cmd_blk.cdb_1.cdb_1_lba_l = LB(lba);
	    ip->cmd_blk.cdb_1.cdb_1_len_h = MB(count);
	    ip->cmd_blk.cdb_1.cdb_1_len_l = LB(count);
	    return;
	case C0_SEEK:
	    ip->scsi_flags |= NO_XFER;
	    if (lba < 0x200000) {  /* fits in 13 bits? */
		    ip->cmd_blk.cdb_0.cdb_0_lba_h = HB(lba);
		    ip->cmd_blk.cdb_0.cdb_0_lba_m = MB(lba);
		    ip->cmd_blk.cdb_0.cdb_0_lba_l = LB(lba);
	    } else { /* use extended command */
		    ip->cmd_blk.cdb_1.cdb_1_cmd |= CD10BYTE;
		    ip->cmd_blk.cdb_1.cdb_1_lba_h = XB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_mh = HB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_ml = MB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_l = LB(lba);
	    }
	    return;
    	case C0_READ:
	case C0_WRITE:
	    ip->scsi_count = count - ip->scsi_count0 - ip->scsi_count1;
	    if (!(un->un_flags & INT_VARIABLE))
		count >>= SCTRSHFT;	/* convert to sectors */
	    if (ip->scsi_count)
		ip->scsi_flags |= DMA_XFER; /* use dma xfer */	
	    else {
		ip->scsi_flags |= PTM_XFER; /* use ptm xfer */	
		if (!ip->scsi_count0) {
		    ip->scsi_count0 = ip->scsi_count1;
		    ip->scsi_count1 = 0;
		    ip->scsi_bufaddr0 = ip->scsi_bufaddr1;
		    ip->scsi_bufaddr1 = 0;
		}
	    }
	    ip->scsi_extra = un->un_extra;
	    if (un->un_flags & INT_TAPE) { /* are we a tape? */
		if (!(un->un_flags & INT_VARIABLE))
		    ip->cmd_blk.cdb_0.cdb_0_lba_h = 1;	/* fixed block size */
		ip->cmd_blk.cdb_0.cdb_0_lba_m = HB(count);
		ip->cmd_blk.cdb_0.cdb_0_lba_l = MB(count);
		ip->cmd_blk.cdb_0.cdb_0_len = LB(count);
	    } else { /* disk */
		if ((cmd == C0_WRITE) && (un->un_flags & INT_WRTVFY)) {
		    ip->cmd_blk.cdb_1.cdb_1_cmd = C1_WRVERIFY;
		    if (un->un_flags & INT_BYTVFY)
			ip->cmd_blk.cdb_1.cdb_1_rsvd1 = 1;
		    goto extendcmd;
		} else if (count < 0x100 &&  /* fits in 8 bits? */
			lba < MAXLBA) { /* fits in 21 bits? */
		    ip->cmd_blk.cdb_0.cdb_0_lba_h = HB(lba);
		    ip->cmd_blk.cdb_0.cdb_0_lba_m = MB(lba);
		    ip->cmd_blk.cdb_0.cdb_0_lba_l = LB(lba);
		    ip->cmd_blk.cdb_0.cdb_0_len = count;
		} else { /* use extended command */
		    ip->cmd_blk.cdb_1.cdb_1_cmd |= CD10BYTE;
extendcmd:
		    ip->cmd_blk.cdb_1.cdb_1_lba_h = XB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_mh = HB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_ml = MB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_lba_l = LB(lba);
		    ip->cmd_blk.cdb_1.cdb_1_len_h = MB(count);
		    ip->cmd_blk.cdb_1.cdb_1_len_l = LB(count);
		}
	    }
	    return;
	case C0_FORMAT:
	case C0_WRFM:
	case C0_TESTRDY:
	case C0_REWIND:
	default:
	    break;
    }
    if (ip->scsi_count0)
	ip->scsi_flags |= PTM_XFER;
    else
	ip->scsi_flags |= NO_XFER;
    ip->cmd_blk.cdb_0.cdb_0_lba_m = HB(count);
    ip->cmd_blk.cdb_0.cdb_0_lba_l = MB(count);
    ip->cmd_blk.cdb_0.cdb_0_len = LB(count);
    return;
}

#ifndef STANDALONE
/*
 * return partition size in 'blocks'
 */
int
common_scsi_size(dev)
dev_t dev;
{
    struct scsi_unit *un;
    struct volume_header *vh;
    struct partition_table *pt;

    if ((un = common_scsi_getun(dev)) == NULL)
	return (-1);
    if ((un->un_flags & INT_ALIVE) == 0)  /* unit OK? */
	return (-1);
    if ((un->un_flags & INT_DISK) && wait_ready(un,POLLED))/* is disk ready */
	return (-1);
    if (un->un_vhvalid == 0)
	return (-1);
    vh = un->un_vh_k1ptr;
    pt = &vh->vh_pt[FS(dev)];
    return (pt->pt_nblks);
}
#endif STANDALONE


/*
 * special command
 */
int
common_scsi_spcmd(un,cmd,block,count,addr,flag)
register struct scsi_unit *un;
int cmd, block, count, addr, flag;
{
    register struct buf *bp;
    register struct scsi_iopb *ip;
    register SCSI_EXT_SENSE *sense;
    u_int temp;
    int s, status;
    int stat, tun = 0;

    bp = &un->un_buf;
#ifndef STANDALONE
    s = splbio();
    while (bp->b_flags & B_BUSY)
    {
	bp->b_flags |= B_WANTED;
	sleep((caddr_t)bp,PRIBIO);
    }
    bp->b_flags = B_BUSY | B_SPL;
    splx(s);
#endif STANDALONE
    bp->b_dev = un->un_dp->b_dev;
again:
    if (flag & POLLED) {
	ip = un->un_iopbp;
	ip->scsi_count0 = ip->scsi_count1 = 0;
	ip->scsi_bufaddr = ip->scsi_bufaddr0 = ip->scsi_bufaddr1 = 0;
	un->un_command = cmd;
	status = doscsicmd(un,bp->b_dev,count,addr,block,POLLED);
	if (scsiexterr && (status == SELTMO))
	    cmn_err(CE_CONT,"SCSI %dL%d: Selction Timeout\n",
		    un->un_target,un->un_lun);
	if (status && (status != SELTMO)) {
	    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	    ip->scsi_count0 = ip->scsi_count1 = 0;
	    ip->scsi_bufaddr = ip->scsi_bufaddr0 = ip->scsi_bufaddr1 = 0;
	    temp = un->un_command;		/* save old command */
	    un->un_command = C0_REQSENSE;	/* for setupdma! */
	    (*LOW_SCSI(bp->b_dev,setupdma))(un,NOSG,DMA_READ,16,
		K1_TO_PHYS(sense),0);
	    un->un_command = temp;		/* restore old command */
	    setupiopb(C0_REQSENSE,sense,un,POLLED,16,0);
	    stat = (*LOW_SCSI(bp->b_dev,startop))(un,POLLED);
	    un->un_xfer = count;
	    un->un_resid = 0;
	    stat = common_scsi_sense_status(un,stat);
	    if (!stat) {
		if (!tun && sense->key == UNIT_ATN) {
		    ++tun;
		    goto again;
		} else {
		    status = 0;
		}
	    }
	}
	bp->b_flags &= ~(B_BUSY | B_SPL);
	return(status);
#ifdef STANDALONE
    } else
	_io_abort("panic: attempt to send SCSI command in interrupt mode\n");
#else STANDALONE
    } else {
	bp->b_un.b_addr = (caddr_t)addr;
	bp->b_bcount = count;
	bp->b_blkno = block;
	bp->b_length = cmd;
	common_scsi_strategy(bp);
	if (flag & WAIT) {
	    iowait(bp);
	    if (bp->b_flags & B_ERROR)
		    return(1);
	}
    }
#endif STANDALONE
    return(0);
}

#ifndef STANDALONE

/*
 * Dump data to disk.
 */
int
common_scsi_dump(dev, flag, bn, physaddr, count)
dev_t dev;
int flag;
daddr_t bn;
caddr_t physaddr;
int count;
{
    register struct scsi_unit *un;
    register SCSI_EXT_SENSE *sense;
    register u_char *cp;
    struct volume_header *vh;
    struct partition_table *pt;
    u_int status, i;

    if ((un = common_scsi_getun(dev)) == NULL) {
	return(EIO);
    }

    if (un_saved == 0) {
	un_saved = 1;
	common_scsi_save_un = *un;
    }
    /*
     * If the drive doesn't exist,
     * or if it doesn't have a valid label, return an error.
     */
    if ((un->un_flags & (INT_ALIVE|INT_READY)) != (INT_ALIVE|INT_READY)) {
	return(ENODEV);
    }
    if (un->un_vhvalid == 0)
	return (EINVAL);
    if (FS(dev) != 1)
	return (EINVAL);

    if (flag == DUMP_OPEN) {
	/* initialize device */
	(void)(*LOW_SCSI(dev,init))(1);
	return (0);
    }
    if (flag == DUMP_CLOSE) {
	/* nop */
	return (0);
    }

    /* insure that request is within partition boundaries */
    vh = un->un_vh_k1ptr;
    pt = &vh->vh_pt[FS(dev)];
    if ((bn < 0) || (bn + count > pt->pt_nblks)) {
	return (EINVAL);
    }
    bn += pt->pt_firstlbn;

    /* write count sectors worth of data
     */
    status = common_scsi_spcmd(un,C0_WRITE,bn,count<<SCTRSHFT,physaddr,POLLED);
    if (status) {
	if (scsiexterr || (status != SELTMO))
	    cmn_err(CE_CONT,"\nSCSI %dL%d: hw status; %s\n",
		un->un_target,un->un_lun,
		scsiprinterr(status));
	return (EIO);
    }
    return (0);
}
#endif STANDALONE

int
common_scsi_sense_status(un,status)
register struct scsi_unit *un;
{
    register SCSI_EXT_SENSE *sense;
    register SCSI_MODE_SENSE *ms;
    register int diff, flag, sense_info;

    flag = 0;
    diff = un->un_xfer;
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    ms = (SCSI_MODE_SENSE *)K0_TO_K1(&un->un_mserror);
    sense_info = (int)((sense->info1 << 24) | (sense->info2 << 16)|
		 (sense->info3 << 8) | (sense->info4));
    if (scsiexterr) {
	cmn_err(CE_CONT,"\n     SCSI %dL%d: sense status on command 0x%x\n",
				    un->un_target,un->un_lun,un->un_command);
	cmn_err(CE_CONT,"       valid   =%d\n",sense->valid);
	cmn_err(CE_CONT,"       segment =%d\n",sense->segment);
	cmn_err(CE_CONT,"       filmrk  =%d\n",sense->filmrk);
	cmn_err(CE_CONT,"       eom     =%d\n",sense->eom);
	cmn_err(CE_CONT,"       ilength =%d\n",sense->ilength);
	cmn_err(CE_CONT,"       key     =0x%x %s\n",sense->key,
		printkey(sense->key));
	cmn_err(CE_CONT,"       info    =0x%x\n",sense_info);
	cmn_err(CE_CONT,"       add_len =%d\n",sense->add_len);
    }
    if (status == CHECK_CONDITION) {
	cmn_err(CE_CONT,
		"SCSI %dL%d: check condition on request sense command\n",
		un->un_target, un->un_lun);
	goto xxxerr;
    }
    if (sense->key == REC_ERR) {
	un->un_softcount++;
	/* log soft errors! */
	cmn_err(CE_CONT,"SCSI %dL%d: %s\n", un->un_target, un->un_lun, 
			printkey(sense->key));
	if (un->un_flags & INT_TAPE)
		un->un_flags &= ~INT_FM;
	else if (sense->valid) {
	    cmn_err(CE_CONT,
		"\tphysical block address 0x%x (%d)\n",sense_info,sense_info);
	}
	return(0);
    } else if(sense->key == UNIT_ATN) {
	un->un_eomcount = un->un_weomcount = 0;
	un->un_resid = un->un_xfer;
	un->un_bn = un->un_prev_bn;
	if ((un->un_flags & (INT_TAPE|INT_OPEN)) == (INT_TAPE|INT_OPEN))
	    cmn_err(CE_CONT,
		"SCSI %dL%d: unit attention; media change or drive was reset\n",
		un->un_target,un->un_lun);
	return(0);
    } else if(sense->key == NOT_RDY) {
	    un->un_flags |= INT_NOT_RDY;
	    goto xxxerr;
    }
    if(un->un_flags & INT_TAPE) {
	if (sense->valid) {
	    diff = sense_info;
	    if (!(un->un_flags & INT_VARIABLE))
		    diff *= 512;
	}
	if(sense->key == BLANK_CHK) {	/* treat like a file mark */
	    sense->filmrk = 1;
	    sense->key = 0;
	}
	if(sense->filmrk) {
	    flag = 1;
	    if ((un->un_flags & INT_FM) && (diff == un->un_xfer)) {
		un->un_flags |= INT_EOM;
		un->un_eomcount = MAXEOM + 1;
	    }
	    else {
		un->un_flags |= INT_FM;
		if (diff != un->un_xfer)
		    un->un_flags |= INT_RFM;
	    }
	} else
	    un->un_flags &= ~INT_FM;
	if(sense->eom) {
	    flag = 1;
	    if (!sense->valid)
		diff = 0;
	    un->un_flags |= INT_EOM;
	    ++un->un_eomcount;
	    if (un->un_command == C0_WRITE) {
		++un->un_weomcount;
		if (diff && (un->un_weomcount == 1)) {
		    if (diff == un->un_xfer)
			++un->un_weomcount;
		    un->un_resid = diff;
		    return(2);
		}
	    }
	}
	if (sense->ilength) {
	    flag = 1;
	    if (diff <= 0) {
		cmn_err(CE_CONT,
		    "SCSI %dL%d: illegal block length %d, actual=%d\n",
		    un->un_target,un->un_lun,un->un_xfer,un->un_xfer - diff); 
		diff = un->un_xfer;
		goto xxxerr;
	    }
	} 
	if(sense->key == DATA_PROT && !ms->hdr.WP) {
	    cmn_err(CE_CONT,
		"SCSI %dL%d: Selected format not valid for tape cartridge\n",
		un->un_target,un->un_lun);
	    diff = un->un_xfer;
	    goto xxxerr;
	} else if (sense->key || !flag) {
	    cmn_err(CE_CONT,"SCSI %dL%d: %s\n", un->un_target,un->un_lun,
		printkey(sense->key));
	    goto xxxerr;
	}
	un->un_resid = diff;
	return(0);
    } else {
	/* see if residual is valid for disk */
	if (sense->valid) {
	    cmn_err(CE_CONT,
		"SCSI %dL%d: physical block address 0x%x (%d)\n",
		un->un_target,un->un_lun,sense_info,sense_info);
	}
	flag = sense->sense_code;
	if (flag && (flag != POW_RST))
	    cmn_err(CE_CONT,
		"SCSI %dL%d: error code 0x%x(%d); %s\n",
		un->un_target,un->un_lun,flag,flag,print_errcode(flag));
    }
xxxerr:
    un->un_resid = diff;
    return(1);
}

int
common_scsi_timeval(un, ip)
register struct scsi_unit *un;
register struct scsi_iopb *ip;
{
    register int tv;

    if (un->un_flags & INT_TAPE) { /* are we a tape? */
	switch (un->un_command) {
	    case C0_LOAD:
		if (ip->cmd_blk.cdb_0.cdb_0_len & 0x02)
		    tv = TIME_RETEN;
		else
		    tv = TIME_REWIND;
		break;
	    case C0_REWIND:
		tv = TIME_REWIND;
		break;
	    case C0_READ:
	    case C0_WRITE:
		/*
		 * Normal read/write commands take almost zero time.
		 * However, if they are operating on a bad spot in
		 * the tape, they can take a long time.  This
		 * time is unmeasured, thus it is a guess.
		 */
		tv = TIME_RDWR*2;  /* make it longer */
		break;
	    case C0_SPACE:
		tv = TIME_FSF;
		break;
	    default:
		tv = 3 * 60;
		break;
	}
    } else {
	switch (un->un_command) {
	    case C0_STARTSTOP:
		tv = 30;
		break;
	    case C0_FORMAT:
		tv = TIME_FORMAT;
		break;
	    default:	 /* disk drive */
                tv = 60; /* made longer for error recovery on some drives */
		break;
	}
    }
    return(tv);
}

struct scsi_unit *
common_scsi_getun(dev)
dev_t dev;
{
    register int target, lun;
    register struct low_scsi *lsp;

    if ((lsp = common_scsi_map[emajor(dev)]) == 0)
	return(0);
    target = TARGET(dev);
    lun = lsp->low_scsi_un->un_lun;

    if (target >= lsp->low_scsi_Ntarget || lun >= lsp->low_scsi_Nlun)
	return(0);
    return(lsp->low_scsi_un + TAR_LUN(target,lun,lsp->low_scsi_Nlun));
}

void
common_scsi_registerme(maj,lsp)
int maj;
struct low_scsi *lsp;
{
	register struct low_scsi **lp;

	lp = &common_scsi_map[maj];
	if (*lp == lsp)
		return;
	if (*lp)
		cmn_err(CE_PANIC,"Attempt to reuse scsi map entry major = %d\n",
			maj);
	*lp = lsp;
}

int
scsi_dev_busy(un,dev)
struct scsi_unit *un;
{
    int partition = FS(dev);
    register i,total;
    register partition_usage *pp;

    for(i = 0, pp = &un->un_open[0]; i < NPARTAB; pp++, i++) {

        /*  if any partition other than p is opened or
            if the un_open is greater than 1 for partition p, 
            the disk is considered busy.  */

	if(pp->status == 0)
		continue;
	if(i == partition) {
		if((total = pp->usage.blk + pp->usage.chr + pp->usage.swp
		+ pp->usage.mnt + pp->usage.layer) > 1)
			return(1);
	} else
		return(1);
    }

    return(0);
}

int
common_scsi_cmdlen(cmd)
register cmd;
{
    register int len;

    switch (cmd & GROUP_MASK) {
    case CD10BYTE:
	len = 10;
	break;
    case CD12BYTE:
	len = 12;
	break;
    case CD6BYTE:
	len = 6;
	break;
    default:
	len = 6;
	break;
    }
    return(len);
}

#ifdef STANDALONE
int
copyin(src,dest,size)
char *src, *dest;
int size;
{
	bcopy(src,dest,size);
	return(0);
}

int
copyout(src,dest,size)
char *src, *dest;
int size;
{
	bcopy(src,dest,size);
	return(0);
}
#endif STANDALONE
