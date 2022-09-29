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
#ident	"$Header: dkvj.c,v 1.31.1.26.1.6.1.14 91/01/28 18:04:48 beacker Exp $"


#define COMPILER_BUG
#define BLOCK_MODE 	 /* enable block-mode for all DMA (AM-B) */ 
#define MACSI_THROTTLE	/* limit the number of cmds/workque */
/* #define DKVJ_DEBUG	/* Enable extra checking code for debug */

/*
 ***
 *** Device decriptors:
 ***    Major device tags the driver for system reference.
 ***    Minor device definitions:
 ***            7    -   SCSI Bus 1 or 2
 ***            6    -   Drive bit 2    -   Max 7 device per bus
 ***            5    -   Drive bit 1
 ***            4    -   Drive bit 0
 ***
 ***            3    -   partition bit 3
 ***            2    -   partition bit 2
 ***            1    -   partition bit 1    - Max 16 partitions
 ***            0    -   partition bit 0
 ***
 ***/

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
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/elog.h"
#include "sys/cpu_board.h"

#ifdef STANDALONE
#include "sys/inode.h"
#include "sys/fs.h"
#include "machine/cpu.h"
#include "mipsvme/vmereg.h"
#include "machine/dvh.h"
#else STANDALONE
#include "sys/file.h"
#include "sys/edt.h"
#include "sys/ioctl.h"
#include "sys/mtio.h"
#include "sys/vmereg.h"
#include "sys/dvh.h"
#include "sys/dkio.h"
#include "sys/kmem.h"
#endif STANDALONE

#ifdef STANDALONE
#include "mipsvme/scsi.h"
#include "mipsvme/dkvj_IPtypes.h"
#include "mipsvme/dkvj_struct.h"
#include "mipsvme/dkvj_scsi.h"
#include "mipsvme/dkvj_reg.h"
#else STANDALONE
#include "sys/scsi.h"
#include "sys/dkvj_IPtypes.h"
#include "sys/dkvj_scsi.h"
#include "sys/dkvj_struct.h"
#include "sys/dkvj_reg.h"
#endif STANDALONE

#ifdef STANDALONE
#include "saio/saio.h"
#include "saio/saioctl.h"

int Nvjctlr = 4;
int ndkvj = 4 * 16;
int dkvjmajors[] = {0, 1, 2, 3};
static char *vjstd[] = {(char *)0x9000, (char *)0x9800,
			(char *)0xa000, (char *)0xa800 };
/*
			(char *)0xb000, (char *)0xb800,
			(char *)0xc000, (char *)0xc800 };
 */
SCSI_MS_ERROR	mode_sense1;	/* Error page for sense and select */
char *vj_tpbuf;
char *temp_buffer;
int vj_didinit;
char vj_didmalloc;

#define clean_cache	clear_cache
#define log(pri,fmt,args) printf(CE_CONT, fmt,args)

#else STANDALONE

#include "sys/dump.h"
#include "sys/gen_ioctl.h"
#include "sys/bsd_glue.h"
#include "bsd43/sys/syslog.h"

static void restart_ctlr();
void vjgetvolume();
void vjtimeout();
int vjSGsetup();
int splbio();
/* These come from master file */
unchar  vj_ilev[1];
unchar  vj_ivec[1];

#ifdef COMPILER_BUG
static ULONG real_temp_buffer[SECSIZE/sizeof(ULONG)];
static ULONG *temp_buffer = &real_temp_buffer[0];
#else COMPILER_BUG
static ULONG temp_buffer[SECSIZE/sizeof(ULONG)];
#endif COMPILER_BUG

extern int Nvjctlr, ndkvj;
extern int dkvjmajors[];
extern int vjexterr;

#endif STANDALONE

#ifdef STANDALONE
#define SET_ERROR(x)	io->i_errno = (x)
#define ERR_RETURN(x) \
	{ \
		io->i_errno = (x); \
		return(-1); \
	}
#else STANDALONE
#define SET_ERROR(x)	u.u_error = (x)
#define ERR_RETURN(x) \
	{ \
		u.u_error = (x); \
		return; \
	}
#endif STANDALONE

#if RISCOS
#define b_length b_bufsize
#endif

#ifdef BLOCK_MODE
#define Blockmode_ok	(IS_M2000_ARCH | IS_M6000)
#else
#define Blockmode_ok	0
#endif

#ifndef PTSIZE_VHDR     /* Define shared with stand/format.c */
	/* Size of partition ten on an un-initialized drive */
#define PTSIZE_VHDR     1000000         /* Size in bytes */
#endif

VJ_CTLR *vjctlrs[8];

#ifdef DKVJ_DEBUG
int dkvj_dump=0;
#endif DKVJ_DEBUG

/*
 * Local routines
 */

static void add_motor_start();
static void motortimeout();
static int wait_ready();

int powerup_time = 180;		/* 3 min max wait time */
int powerseq_delay = 15;	/* 15 seconds between drive startups */

/* This is the head of the queue for getting the units powered up */

#define TAPEDENSITY(x)	((x & 6)>>1)	/* Cartridge tape density */

static int Tapedensity[] = {
    0x00,		/* Default Implicite density */
    0x05,		/* QIC-24 density code field*/
    0x0f,		/* QIC-120 density code field */
    0x10		/* QIC-150 density code field */
};

static struct motor_start {
	VJ_UNIT *active;
	VJ_UNIT *next;
	int	timeout;
} motor_start_list;

int first_init;              /* 1/2 - num of times card getting init'ed */
int Dkvj_Error = 0;

#ifdef R6000
/* 6000 class machines get no additional performance by doing combined I/O
 * operations above 32.  Limiting to 32 leaves more GBA map registers available
 * for general use.
 */
int dkvj_max_combine_links = 32;
#endif R6000

char scsi_cmd[] = {
    0,
    C0_TESTRDY,
    C0_REZERO,
    C0_REQSENSE,
    C0_READ,
    C0_WRITE,
    C1_SEEK,
    C0_FORMAT,
    C0_REWIND,
    C0_WRFM,
    C0_SPACE,
    C0_INQUIRY,
    C0_SENSEMODE,
    C0_MODESEL,
    C0_RDBLOCK,
    C0_REASSIGN,
    C0_LOAD,
    0,
    0,
    0,
    0,
    C1_READCAP,
    C1_VERIFY,
    C1_READDEF,
};

char macsi_cmd[] = {
    0,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_RESET,
    CNTR_INIT,
    CNTR_INIT_WORKQ,
    CNTR_FLUSH_WORKQ,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
    SCSI_PASS_THRU,
};

#ifndef STANDALONE
SCSI_ERRORS scsi_err[] = {
    { 0x00, "Good Status" },
    { 0x02, "Request Sense needed (Check condition)."},
    { 0xFF, "Unknown Error." }
};

VJ_ERRORS   jaguar_err[] = {
    { 0x00, "Good Status" },
    { 0x01, "Work Queue Full" },
    { 0x02, "Work Queue not Initialized." },
    { 0x03, "First command not Initialize." },
    { 0x04, "Invalid Command Type." },
    { 0x05, "Invalid Work Queue Number." },
    { 0x06, "Re-initialization of a Work Queue Failed." },
    { 0x07, "Uninitialized Work Queue." },
    { 0x08, "Start Queue Mode before Initialize Command." },
    { 0x09, "Command Type not Implemented." },
    { 0x0a, "Invalid Priority." },
    { 0x10, "Reserved Field not Zero." },
    { 0x11, "SCSI Reset successful." },
    { 0x12, "Port 2 not installed." },
    { 0x13, "SCSI device ID conflict." },
    { 0x14, "SCSI bus in reset state." },
    { 0x15, "Command aborted by SCSI reset." },
    { 0x20, "Bus-Error Occurred during DMA." },
    { 0x21, "VME timeout." },
    { 0x23, "Invalid DMA Address." },
    { 0x24, "Invalid VME memory type." },
    { 0x25, "Illegal (odd) count specified." },
    { 0x30, "Selection phase of the SCSI device failed." },
    { 0x31, "Device did not reselect the board and timedout." },
    { 0x32, "SCSI operation did not complete successfully." },
    { 0x33, "SCSI device disconnected inappropriately." },
    { 0x34, "SCSI transfer count did not match the count given." },
    { 0x40, "Illegal (odd) count specified in S/G list." },
    { 0x41, "Invalid VME memory type in S/G list." },
    { 0x42, "Invalid DMA Address in S/G list." },
    { 0x80, "Flush on Error in Progress." },
    { 0x81, "Flush Work Queue status." },
    { 0x82, "Target reconnected but NO iopb." },
    { 0x83, "Target requesting more data than setup for." },
    { 0x84, "Target requesting transfer in opposite direction." },
    { 0xc0, "IOPB type error." },
    { 0xc1, "IOPB Timeout." },
    { 0xFF, "Unknown Error." }
};
#endif STANDALONE

SENSE_KEY_DEFS Sense_keys[] = {
    { SCSI_NO_SENSE,		"No Sense Data was available" },
    { SCSI_RECOVERABLE_ERROR,	"command completed with recovery actions" },
    { SCSI_NOT_READY,		"drive can't be accessed" },
    { SCSI_MEDIUM_ERROR,	"Non-recoverable data error" },
    { SCSI_HARDWARE_ERROR,	"non-recoverable hardware failure (parity, etc)" },
    { SCSI_ILLEGAL_REQUEST,	"Illegal parameter in cdb" },
    { SCSI_UNIT_ATTENTION,	"media change or drive was reset" },
    { SCSI_DATA_PROTECT,	"cartridge is write-protected" },
    { SCSI_BLANK_CHECK,		"no-data condition encountered on tape or tape and drive are different formats" },
    { SCSI_ABORT_COMMAND,	"drive aborted the command" },
    { SCSI_VOLUME_OVERFLOW,	"physical EOM reached with data still in buffer" },
    { 0xFF,			"status code not in table" }
};
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
} err_codesj[] = {
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

typedef struct inq_fields {
        char *dp;
        int  offset;
        int  size;
}INQ_FIELDS;

INQ_FIELDS vj_scsi_inq_fields[] = {
        {NULL,  3,  8}, /* vendor ID */
        {NULL, 11, 16}, /* product ID */
        {NULL, 27,  4}, /* revision level */
        {NULL, 31,  8}, /* microcode revision level */
        {NULL, 39, 12}, /* drive serial number */
        {NULL,  0, 16}, /* device type */
        {NULL,0,0}
};

static char *
print_errcode(type)
register unsigned char type;
{
	register struct err_code *errs = err_codesj;
	register hold;

	while (hold = errs->key) {
		if (type == hold)
			return (errs->key_def);
		errs++;
	}
	return (errs->key_def);
}

#ifndef STANDALONE
void
scsistr(un, pp)
VJ_UNIT *un;
register char *pp;
{
    SCSI_INQUIRY *inq;
    register len, i, j;
    register char ch, *q;
    INQ_FIELDS inq_fields[7];
    register INQ_FIELDS *ifp;
    char *spaces = "  ";

    inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inquiry);
    switch (inq->device_type) {
	case SCSI_TYPE_DISK:
		if (inq->rm)
			q = "REMOVABLE DISK";
		else
			q = "DISK";
		break;
	case SCSI_TYPE_TAPE:
#ifdef XXX
		/*
		 * XXX - This should work, but I get 
		 * PANIC: vfs_mountroot: cannot mount root
		 * when this is in the code
		 */
		(void)vjsplcmd(un,SCSI_RDBLKLIM_CMD,0,0,0,WAIT);
		rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
		if (rb->maxlen != rb->minlen)
			q = "VARIABLE TAPE";
		else
			q = "FIXED TAPE";
#else
		q = "TAPE";
#endif		
		break;
	case SCSI_TYPE_PRINTER:
		q = "PRINTER";
		break;
	case SCSI_TYPE_CPU:
		q = "PROCESSOR";
		break;
	case SCSI_TYPE_WORM:
		q = "WORM DISK";
		break;
	case SCSI_TYPE_RONLY_DISK:
		q = "ROM DISK";
		break;
	case SCSI_TYPE_SCANNER:
		q = "SCANNER";
		break;
	case SCSI_TYPE_OPTICAL:
		q = "OPTICAL";
		break;
	case SCSI_TYPE_CHANGER:
		q = "MEDIUM CHANGER";
		break;
	case SCSI_TYPE_COMM:
		q = "COMMUNICATIONS";
		break;
	case SCSI_TYPE_M12_FLOPPY:
		q = "Rx2030 FLOPPY";
		break;
	default:
		q = "UNKNOWN TYPE";
		break;
    }
    for (i = 0; i < 7; ++i)
	inq_fields[i] = vj_scsi_inq_fields[i];
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
#endif STANDALONE

/*
 * Perform a CPU cache writeback, possibly spanning physical memory page
 * boundaries.
 * Invalidate CPU cache so next access to will obtain the
 * latest data following the I/O operation.
*/

dkvj_wb_cache( dmaaddr, dmasize )
ulong dmaaddr;
ulong dmasize;
{

    writeback_virtual_data( dmaaddr, dmasize);
    invalidate_virtual_data( dmaaddr, dmasize);

}


/*
 * Map the entire VJ_CTLR structure into GBA I/O bus address space.
 * Save the map descriptor in the c_sph field within the vj_ctlr 
 * structure.
 */

dkvj_ctlr_iomap( c )
VJ_CTLR *c;
{
	
  ioaddr_t vme_addr;

  ASSERT( c->c_csh );
  ASSERT( c->c_sph == 0 );
 
  if (!vme_iomap(c->c_csh, c, sizeof(VJ_CTLR),
		    GBA_CONTIG_ADDR+GBA_NOPART_MAP,&c->c_sph,&vme_addr))
    
    cmn_err(CE_PANIC, "Can't map VJ_CTLR info!\n");

}

/*
 * Prepare specified area in the dkvjctlr structure for I/O.  The
 * dkvjctlr structure is mapped for I/O at controller initialization
 * and left permanently mapped.  This routine will cause a cache writeback
 * from the CPU cache and return the "io address" usable by a controller
 * on the GBA to access the specified region.
 */


dkvj_ctlr_ioinit( c, dmaaddr, dmasize, io_addr )
VJ_CTLR *c;
ulong dmaaddr;
ulong dmasize;
ioaddr_t *io_addr;
{
  ASSERT( c->c_sph );

  if (!ka_to_vmeaddr( c->c_sph, dmaaddr, io_addr))
      cmn_err(CE_PANIC, "Address not in dkvjctlr map!\n");

 /*
  * Make sure that any data in the CPU cache has been written back to
  * physical memory.
  */
  writeback_virtual_data( dmaaddr, dmasize );
}


/*
 * Complete the I/O operation on the specified area in the dkvjctlr
 * structure.  This routine will flush the area from the GBA cache and
 * invalidate the CPU caches so that subsequent access to the area will
 * obtain the data from the completed I/O operation.
 */

dkvj_c_iodone( c, dmaaddr, dmasize )
VJ_CTLR *c;
ulong dmaaddr;
ulong dmasize;
{
  ASSERT( c->c_sph );

  /*
   * Flush dma area from GBA cache.
   */

  if (!vme_ioflush( c->c_sph, dmaaddr, dmasize))
      cmn_err(CE_PANIC, "Can't flush cntlr region!\n");

  /*
   * Invalidate CPU cache so next access to will obtain the
   * latest data following the I/O operation.
   */

    invalidate_virtual_data( dmaaddr, dmasize );
}

/* flush dma area from GBA cache in the VJ_UNIT  struct */

dkvj_un_iodone( un )
VJ_UNIT *un;
{

  /*
   * Flush dma area from GBA cache.
   */
  if(un->un_sph) {

  if (!vme_ioflush( un->un_sph, un, sizeof(VJ_UNIT)))
      cmn_err(CE_PANIC, "Can't flush VJ_UNIT region!\n");

  }
  /*
   * Invalidate CPU cache so next access to will obtain the
   * latest data following the I/O operation.
   */

    invalidate_virtual_data( un, sizeof(VJ_UNIT) );
}


/* 
 * dkvj_buf_iodone
 * Flush all IO buffers from the GBA cache and unmap it
 */

dkvj_buf_iodone( buf_ext )
BUF_EXT *buf_ext;
{

  if(!buf_ext) /* then no resources held */
    return;

  /******************************************************************
   * If sph was allocated, flush associated data from GBA cache, and
   * return map registers to free list.
   *****************************************************************/

  if (buf_ext->sph)
    {
      vme_iounmap( buf_ext->sph );    /* free map registers */
      buf_ext->sph = 0;
    }
}

char *
sense_err_msg(err)
register UBYTE err;
{
    register SENSE_KEY_DEFS *kp;

    kp = Sense_keys;
    do {
	if (err == kp->key)
	    return (kp->key_msg);
	kp++;
    } while(kp->key != 0xFF);
    return (kp->key_msg);
}
void
vjto_shio(src, dst, cnt)
register USHORT *src, *dst;
register USHORT cnt;
{
    cnt >>= 1;                          /* divied by 2 */
    while (cnt--) {
        *dst++ = *src++;  wbflush();
    }
}
void
vjfrom_shio(src, dst, cnt)
register USHORT *src, *dst;
register USHORT cnt;
{
    cnt >>= 1;                          /* divied by 2 */
    while (cnt--) {
        *dst++ = *src++;
    }
}
void
vjzero_shio(src, cnt)
register USHORT *src;
register USHORT cnt;
{
    cnt >>= 1;                          /* divied by 2 */
    while(cnt--) {
        *src++ = 0;  wbflush();
    }
}
#ifdef STANDALONE
vj_initmem()
{
    int tmpptr;
    int howmuch;
    /*
     * Allocate memory for controller buffers.  Let's make sure that they
     * start on an even boundary.
     */
	temp_buffer = (char *)align_malloc(SECSIZE, 4);
	vj_tpbuf = (char *)align_malloc(TP_BLKSIZ, 4);
}
_dkvjinit()
{
    /* 
     * Just zero out any data that's necessary if memory didn't get
     * zeroed on reset
     */
    vj_didinit = 0;
    vj_didmalloc = 0;
}
#endif STANDALONE

/* ********************************************************************** */
/*                                                                        */
/*  vjmce_init.c:   MCE Initialization code.                              */
/*                                                                        */
/* ********************************************************************** */
int
vjmce_init(ctlr)
{
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register VJ_SHIO_OFF *shioff;
    register VJ_UNIT *un;
    volatile VJ_MSR *msr;
    volatile VJ_MCR *mcr;
    register INT i;
    ioaddr_t io_addr;
    VJ_CSB *csb;

    c    = vjctlrs[ctlr];
    shio = c->c_io;
    shioff = (VJ_SHIO_OFF*)c->c_io;
    msr = &shio->sh_MCSB.mcsb_MSR;
    mcr = &shio->sh_MCSB.mcsb_MCR;

    if (c->c_firsttime == 0) {
	    i = 50000;     /* Wait for Board OK */
	    while(!(WORDP(msr) & M_MSR_BOK) && i--)
		DELAY(512);
	    if (i <= 0)
		log(BSD43_LOG_ERR,
		"DKVJ %d: POWER-ON DIAGNOSTICS FAILED!\n",ctlr);
    }
    WORDP(mcr) |= M_MCR_RES;  wbflush();  /* Reset the controller */
    WORDP(msr) = 0;  wbflush();		/* clear BOK bit */

    /* 
     *if the card is getting reset for the second time during initialization
     *  - there is something terribly wrong. get out of here 
    */
    if (c->c_firsttime == 1) {
	if (first_init == 1) {
		log(BSD43_LOG_ERR,
		"DKVJ %d: 4210 BOARD NON-FUNCTIONAL! \n", ctlr);
		return(FALSE);
	}
    }
   
    DELAY(128);
    WORDP(mcr) &= ~M_MCR_RES;  wbflush(); /* Clear the controller */           
    DELAY(128);		/* wait for BOK to leave after reset */
    i = 50000;                    /* wait for BOK bit to show up */
    while (!(WORDP(msr) & M_MSR_BOK) && i--)
        DELAY(512);
    if (!(WORDP(msr) & M_MSR_BOK)) {
	log(BSD43_LOG_ERR,
	"DKVJ %d: board failed powerup diagnostics MSR=%x\n",
		ctlr,WORDP(msr));
        return (FALSE);
    }
    csb = &c->c_csb;
    vjfrom_shio(&shio->sh_CSS, csb, sizeof(VJ_CSB));
    if (showconfig)
	log(BSD43_LOG_ERR,
	    "Jaguar Version (%s-%x-%s) Date %s with %d Kbytes ram.\n",
            &csb->csb_PCODE[0],
            csb->csb_PVAR,
            &csb->csb_FREV[0],
            &csb->csb_FDATE[0],
            csb->csb_BSIZE);
    /*
     * note that off-board iopbs are supported starting with XAD firmware
     * and that the first XA* firmware used at MIPS is XAD.
     * All previously shipped firmware was X0* where * should be Y.
     */
    c->c_mode = 0; /* clear all mode bits to start */
    if ((csb->csb_FREV[1] > 0x30) || (csb->csb_FREV[0] != 0x58)) {
      	/* if we're not "X0?" then enable (i.e. if 2nd char > '0' OR
	 * first character NOT 'X'
	 */
	c->c_mode |= C_OFF_BOARD; /* off-board iopbs enabled */
    	if (showconfig)
		log(BSD43_LOG_ERR,"off-board iopb's are enabled\n");
	/* When MACSI_SORT is supported change the 0xff below */
    	if (csb->csb_FREV[2] > 0xff) { /* MACSI SORT supported with XAE? */
		c->c_mode |= C_MACSI_SORT; /* MACSI sorting enabled */
    		if (showconfig)
			log(BSD43_LOG_ERR,"MACSI sorting enabled\n");
	}
    }
    if (Blockmode_ok && showconfig)
    	log(BSD43_LOG_ERR,"block mode transfers are enabled\n");
    /*
     * Clear all important areas of short I/O
     */
    if (c->c_mode & C_OFF_BOARD) {
    	vjzero_shio(&shioff->sh_CQE_OFF[0], (sizeof(VJ_CQE_OFF) * NUM_CQE_OFF));
        vjzero_shio(&shioff->sh_HUS[0],   S_HUS_FREE_OFF);	
    } else {
    	vjzero_shio(&shio->sh_CQE[0],  (sizeof(VJ_CQE)  * NUM_CQE));
	vjzero_shio(&shio->sh_IOPB[0], (sizeof(VJ_IOPB) * NUM_IOPB));
    	vjzero_shio(&shio->sh_HUS[0],   S_HUS_FREE);
    }
    vjzero_shio(&shio->sh_MCE_IOPB, sizeof(VJ_IOPB));
    vjzero_shio(&shio->sh_CIB,      sizeof(VJ_CIB));
    vjzero_shio(&shio->sh_CRB,      sizeof(VJ_CRB));
    vjzero_shio(&shio->sh_RET_IOPB, sizeof(VJ_IOPB));

    if (c->c_mode & C_OFF_BOARD) {
    	c->c_off_top    = ((OFFBD_IOPB*)K0_TO_K1(&c->c_offbd[0]));
    	c->c_off_QHDP   = c->c_off_top;
    	for (i = 0; i< NUM_CQE_OFF; i++) { /* setup iopb address in cqe's */
    		W_QECR(shioff->sh_CQE_OFF[i].cqe_QECR) = M_QECR_FOB;/*offboard*/
        	wbflush();
        	W(shioff->sh_CQE_OFF[i].cqe_ADDR_TYPE)
			= (Blockmode_ok) ? ADDR_MOD_B : ADDR_MOD;
        	wbflush();

		/* init the ctlr space for io */
		dkvj_ctlr_ioinit(c, &c->c_offbd[i], sizeof(c->c_offbd)/
		NUM_CQE_OFF, &io_addr);

		shioff->sh_CQE_OFF[i].cqe_HOST_ADDR_msw = io_addr >> 16;
		wbflush();
		shioff->sh_CQE_OFF[i].cqe_HOST_ADDR_lsw = io_addr & 0xffff;
		wbflush();
	/*      shioff->sh_CQE_OFF[i].cqe_IOPB_LENGTH = sizeof(VJ_IOPB)/2; */
		/* optimization from 32 shorts to 22 */
		shioff->sh_CQE_OFF[i].cqe_IOPB_LENGTH = 22; /* 12 byte cdb */
        	wbflush();
	}
    	c->c_cqe_top    = (VJ_CQE*)&shioff->sh_CQE_OFF[ 0 ];
    	c->c_cqe_end    = (VJ_CQE*)&shioff->sh_CQE_OFF[ NUM_CQE_OFF - 1 ];
    	c->c_cqe_QHDP   = (VJ_CQE*)((INT)&shioff->sh_CQE_OFF[0] - (INT)shio);
    } else {
    	for (i = 0; i< NUM_CQE; i++) { /* setup iopb address in cqe's */
       		shio->sh_CQE[i].cqe_IOPB_ADDR =
				((INT)&shio->sh_IOPB[i] - (INT)shio);
        	wbflush();
	}
    	c->c_cqe_top    = (VJ_CQE*)&shio->sh_CQE[ 0 ];
    	c->c_cqe_end    = (VJ_CQE*)&shio->sh_CQE[ NUM_CQE - 1 ];
    	c->c_cqe_QHDP   = (VJ_CQE*)((INT)&shio->sh_CQE[0] - (INT)shio);
    }
#ifdef STANDALONE
    if (un = (VJ_UNIT *)align_malloc(sizeof(VJ_UNIT), 4))
	    bzero(un,sizeof(VJ_UNIT));
#else  STANDALONE
    if ((un = c->c_unit7) == 0)  {	/* only allocate if don't have it */
      un = (VJ_UNIT *)kmemzalloc(sizeof(VJ_UNIT), M_DEVBUF,
				 M_NOWAIT|M_CONTIGUOUS);
      un = c->c_unit7 =  (VJ_UNIT*) K2_TO_K0( un );
    }
#endif STANDALONE
    if (!un)
	    return(0);
    c->c_firsttime = TRUE;
    c->c_present   = TRUE;
    c->c_ctlr = un->un_ctlr = ctlr;
    c->c_max_to_que = 0x2;	
				/* 
				 * maximum que entries to send down
				 * to the Jaguar for each unit. We
				 * limit these so we can sort. 2 is
				 * a good compromise value that takes
				 * advantage of overlapped command
				 * execution on the Jaguar 
				 */
    /* Initialize Controller Command Format */
    vjcmd(un, VJ_CNTR_INIT, (char *)0, 0, 0, NO_INTERRUPT);
    if (vjwait(c, M_CRSW_CC, VJ_CNTR_INIT)) {
	c->c_present = FALSE;
    } else {
	vjfrom_shio(&shio->sh_CSS, csb, sizeof(VJ_CSB));
	c->c_pid = csb->csb_PID & 0x07;
	c->c_sid = (csb->csb_SID & 0x07) | 0x08; /* 
						 * cause secondary units are
						 * numbered 8-15 
						 */
        /* END Initialize Controller Command Format */
#ifndef STANDALONE
    	un->un_unit.U.b.BUS = 1; /* 
				  * try scsi reset on secondary scsi bus 
				  * in order to determine if daughter card
				  * is available 
				  */
    	vjcmd(un, VJ_SCSI_RESET, 0, 0, 0, NO_INTERRUPT);
    	if (vjwait(c, M_CRSW_CC, VJ_SCSI_RESET)) {
		c->c_maxunit = 8;
    	} else {
		c->c_maxunit = 16; /* we've a daughter board */
	        if (showconfig)
		    log(BSD43_LOG_ERR,"daughter card added for 2nd scsi bus\n");
    	}
    	/*     Start Queued Mode */
    	W(shio->sh_MCSB.mcsb_MCR) |= M_MCR_SQM;  wbflush();
    	if (vjwait(c, M_CRSW_QMS, VJ_SENSE)) {
		log(BSD43_LOG_ERR,
		"DKVJ %d: Unable to Start Queued Mode.\n", ctlr);
		c->c_present = FALSE;
    	}
    	/* END Start Queued Mode */
#endif STANDALONE
    }
    return (c->c_present);
}
/*
 * Determine existence of controller
 */
void
#ifdef STANDALONE
dkvjinit(io)
register struct iob *io;
#else STANDALONE
dkvjedtinit(e)
struct edt *e;
#endif STANDALONE
{
    extern void vjattach();
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register VJ_HSB  *hsb; /* Host Semaphore Block */
    int ctlr;
    VJ_SHIO *dev_ioaddr;
#ifndef STANDALONE
    register INT i,j;

    first_init = 1; /* entering init */
    ctlr = e->e_intr_info->v_unit;
    ASSERT( vjctlrs[ctlr] == 0 );	/* Otherwise it's a double init */
#ifndef	R6000    
    dev_ioaddr     = (VJ_SHIO*)(e->e_base);
#else
    if (!(dev_ioaddr =(VJ_SHIO*)find_r6000_controller( e, 0, sizeof(short)))) {
        if (showconfig)
		log(BSD43_LOG_ERR,"DKVJ %d: controller not available\n",ctlr);
        return;
      }
#endif	R6000
#else STANDALONE
    if (!vj_didmalloc) {
	vj_initmem();
	vj_didmalloc = 1;
    }
    ctlr = io->i_ctlr;
    c = vjctlrs[ctlr] = (VJ_CTLR *)align_malloc(sizeof(VJ_CTLR), 4);
    if (c)
    	bzero(c,sizeof(VJ_CTLR));
    else {
	log(BSD43_LOG_ERR,
	"DKVJ %d: cannot allocate memory for controller\n", ctlr);
	return;
    }
    dev_ioaddr = (VJ_SHIO*)PHYS_TO_K1(VMESA16_TO_PHYS(vjstd[io->i_ctlr]));
    io->i_devaddr = (unsigned)dev_ioaddr;
#endif STANDALONE

    if (IOBADADDR(dev_ioaddr, sizeof(short))) {
        if (showconfig)
		log(BSD43_LOG_ERR,"DKVJ %d: controller not available\n",ctlr);
	vjctlrs[ctlr] = 0;
        return;
    }
    if (showconfig)
	log(BSD43_LOG_ERR,
	"Interphase 4210 Jaguar controller at phys addr %x\n", dev_ioaddr);
#ifndef STANDALONE
    c = (VJ_CTLR*)kmemzalloc(sizeof(VJ_CTLR), M_DEVBUF, M_NOWAIT|M_CONTIGUOUS);
    
    /* Convert controller structure address to a K0 address (from K2) so we
     * don't need tlbs for translation and all of our K0/K1 accesses will work.
     */
    vjctlrs[ctlr] = c = (VJ_CTLR *)(K2_TO_K0(c));
    c->c_tab = (struct iobuf *)
      kmemzalloc(sizeof(struct iobuf), M_DEVBUF, M_NOWAIT|M_CONTIGUOUS);
#endif STANDALONE
    c->c_io = dev_ioaddr;	/* Store address to the controller board */

    shio    = c->c_io;
    c->c_firsttime = 0;
    hsb  = &shio->sh_HSB;
#ifndef STANDALONE
    /* set up for vectored interrupts, interrupt handler gets CTLR pointer */
    vj_ivec[0] = e->e_intr_info->v_vec;
    vj_ilev[0] = e->e_intr_info->v_brl;
    c->c_nintvec = (int)vj_ivec[0]; 	/* normal interrupt */
    c->c_eintvec = c->c_nintvec;        /* error interrupt  */
    c->c_qintvec = c->c_eintvec;        /* Queue entry Available */
    c->c_level   = (int)vj_ilev[0];
#endif STANDALONE
    /* reserve a Logical cache section in the GBA for use by the controller */

    if(!vme_reserve_iomap(ctlr, c->c_io, 64, &c->c_csh, GBA_CS_AUTOEXP))
	 cmn_err(CE_PANIC,"Couldn't allocate cache section in GBA !\n");

    dkvj_ctlr_iomap( c ); /* map the io space */

    if (vjmce_init(ctlr))     /* returns true if passed */
    {
	hsb->hsb_INITQ = HOST_ID;  wbflush();
	hsb->hsb_WORKQ = 0;  wbflush();
#ifndef STANDALONE
	/* init buf.h extension Free list    */
	for(i = 0; i < (MAX_BUF_EXT - 1); i++)
	    c->buf_ext[i].nxt = &c->buf_ext[i+1];
	c->buf_ext[i].nxt = (BUF_EXT *) 0; /* end list   */
	c->buf_ext_hd = &c->buf_ext[0];    /* init head  */
	/* init Scatter/Gather Free list    */
	for(i = 0; i < (NUM_M_SG - 1); i++) {
	    c->dkvjsg_fentry[i].nxt = &c->dkvjsg_fentry[i+1];
	    c->dkvjsg_fentry[i].align.al_no_block = 0;
	    for(j = 0; j < MACSI_SG; j++) {
		if (Blockmode_ok) {
			c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
				 (TT_BLOCK << 2) | MEMTYPE; /* Block mode */
			c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
		} else {
			c->dkvjsg_fentry[i].ipsg[j].sg_meminfo = MEMTYPE;
			c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPAMOD;
		}
	    }
	}
	c->dkvjsg_fentry[i].nxt = (IPSG_FREE *) 0; /* end list   */
	c->dkvjsg_fentry[i].align.al_no_block = 0;
	c->dkvjsg_hd = &c->dkvjsg_fentry[0];       /* init head   */
	for(j = 0; j < MACSI_SG; j++) {
	    if (Blockmode_ok) {
		c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
				 (TT_BLOCK << 2) | MEMTYPE; /* Block mode */
		c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
	    } else {
		c->dkvjsg_fentry[i].ipsg[j].sg_meminfo = MEMTYPE;
		c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPAMOD;
	    }
	}
	dkvj_wb_cache(c, sizeof(VJ_CTLR));
	for (i = 0; i < c->c_maxunit; i++) {   /* attach bus devices */
	    Dkvj_Error = 0;
	    vjattach(i,ctlr,0);
	    if (Dkvj_Error) {
		log(BSD43_LOG_ERR,
		   "DKVJ %d: 4210 BOARD NON FUNCTIONAL!\n",ctlr);
		break;
	    }
	}
#endif STANDALONE
    }
    first_init = 2; /* leaving init */
}
void
#ifdef STANDALONE
vjattach(io)
register struct iob *io;
{
    register int unit = io->i_unit;
    register int ctlr = io->i_ctlr;
#else STANDALONE
vjattach(unit,ctlr,un)
register int unit, ctlr;
register VJ_UNIT *un;
{
    VJ_UNIT *un_K2_addr=0;
#endif STANDALONE
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register int workq;
    char product[128];
    ioaddr_t io_addr;
    int s;
    unsigned temp, temp1;
    SCSI_INQUIRY *inquiry;
    SCSI_READCAP *readcap;
    SCSI_MS_ERROR *err;
    SCSI_MS_GEOM *msgeom;
    SCSI_MS_RDAHD *rhd;
    SCSI_MS_CACHE *cp;
    SCSI_MS_DISREC *disrec;
    SCSI_MS_FORMAT *msformat;

    c = vjctlrs[ctlr];
    shio = c->c_io;
    W(shio->sh_MCSB.mcsb_IQAR)   = 0;  wbflush();
    /*
     * Initialize slave (physical drive)
     */
    if (unit > c->c_maxunit || unit == c->c_pid || unit == c->c_sid)
	return;
    workq = unit + 1 - (unit > c->c_sid ? 1 : 0) - (unit > c->c_pid ? 1 : 0);
    /*     Init Work Queues */
    if (workq < MAX_WORK_QUEUES) {
#ifdef STANDALONE
	if (un = (VJ_UNIT *)align_malloc(sizeof(VJ_UNIT), 4))
	    bzero(un,sizeof(VJ_UNIT));
#else  STANDALONE
	if (unit == 7)
	  un = c->c_unit7;	/* Special use of unit 7 by vjmce_init */
	
	if (!un) {
	  un_K2_addr = (VJ_UNIT *) kmemzalloc(sizeof(VJ_UNIT), M_DEVBUF,
					      M_NOWAIT|M_CONTIGUOUS);
	  un = (VJ_UNIT*) K2_TO_K0( un_K2_addr );
	} else
	  ASSERT( un == c->c_unit[unit] );
#endif STANDALONE
	if (!un) return;
	c->c_unit[unit] = un;
	un->un_ctlr = ctlr;
	un->un_slave = (UBYTE)unit;
	un->un_unit.U.b.SCSI_ID = unit;
	if (unit >= 8)
	    un->un_unit.U.b.BUS = 1;
	un->un_retries = MAX_RETRIES;
	un->un_qcount = 0;
    	if (c->c_mode & C_OFF_BOARD)
	    un->un_queue_size = NUM_CQE_OFF;
	else
	    un->un_queue_size = NUM_CQE;
	un->un_workq = workq;
	un->un_iotime = &dkvjiotime[ctlr][unit];
	if (un->un_tab == 0)
	  /* OK to leave this as K2 address */
	  un->un_tab =  (struct iobuf *) kmemzalloc(sizeof(struct iobuf),
						    M_DEVBUF,
						    M_NOWAIT|M_CONTIGUOUS );
	un->un_c = c;
	dkvj_wb_cache(un, sizeof(VJ_UNIT));
	
	/* Make sure unit structure is mapped for I/O */
	if ((!un->un_sph) &&
	    (!vme_iomap(c->c_csh, un, sizeof(VJ_UNIT),
			GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			&un->un_sph, &io_addr)))
	    cmn_err(CE_PANIC, "Can't map unit structure!\n");
	
	s = splbio();
        if (vjsense(un, NO_INTERRUPT))
		goto attach_fail;
#ifndef STANDALONE
        vjcmd(un, VJ_INIT_WORKQ, (char *)0, 0, 0, NO_INTERRUPT);
        if (vjwait_unit(un, M_CRSW_CC, VJ_INIT_WORKQ)) 
		goto attach_fail;
	inquiry = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inquiry);
#else STANDALONE
	inquiry = &un->un_inquiry;
#endif STANDALONE

	/* 
	 * Obtain I/O address of inquiry field within the unit structure.
	 */

	if (!ka_to_vmeaddr(un->un_sph,inquiry, &io_addr))
	  cmn_err(CE_PANIC, "Can't map inquiry structure !\n");

	dkvj_wb_cache(inquiry, sizeof(SCSI_INQUIRY));
	vjcmd(un, VJ_INQUIRY_CMD, io_addr, 0, 1, NO_INTERRUPT);

	if (vjwait_unit(un, M_CRSW_CC, VJ_INQUIRY_CMD))
	{
	   /* check just one more time */
	   vjcmd(un, VJ_INQUIRY_CMD, io_addr, 0, 1, NO_INTERRUPT);

	   if (vjwait_unit(un, M_CRSW_CC, VJ_INQUIRY_CMD))
	    log(BSD43_LOG_ERR,"DKVJ %d:%d INQUIRY COMMAND FAILED\n",ctlr,unit);
	   else 
		goto all_ok;
attach_fail:
#ifndef STANDALONE
/*	    kern_free(addr);	*/
 	    if(un->un_sph && !vme_iounmap( un->un_sph ))
 	      cmn_err(CE_PANIC, "Can't flush/unmap unit structure!\n");
	    /* Note:  un == c->c_unit[unit]  */
	    if (un->un_tab)
	      kmemfree( un->un_tab, M_DEVBUF, M_NOWAIT );
	    if (un_K2_addr)
	      kmemfree( un_K2_addr, M_DEVBUF, M_NOWAIT );
#endif STANDALONE
	    c->c_unit[unit] = 0;
	    splx(s);
	    return;
	}
         
all_ok:

#ifndef STANDALONE
	scsistr(un,product);
#ifndef DKVJ_DEBUG
	if (showconfig)
#endif  DKVJ_DEBUG
	    log(BSD43_LOG_ERR,"Work Queue %d for device '%s'\n",workq, product);
#endif STANDALONE
	switch ((int)inquiry->device_type) {
		case SCSI_TYPE_DISK:
			un->un_flags |= IVJ_DISK;
#ifndef STANDALONE
			if (inquiry->rm)
				un->un_flags |= IVJ_RMV_MEDIA;
#endif STANDALONE
			break;
		case SCSI_TYPE_TAPE:
			un->un_flags |= IVJ_TAPE;
			un->un_retries  = 0;
			break;
#ifndef STANDALONE
		case SCSI_TYPE_WORM:
			un->un_flags |= IVJ_WORM;
			break;
		case SCSI_TYPE_RONLY_DISK:
			un->un_flags |= (IVJ_WORM|IVJ_READONLY);
			break;
		case SCSI_TYPE_PRINTER:
			un->un_flags |= IVJ_PRINTER;
			break;
		case SCSI_TYPE_CPU:
		case SCSI_TYPE_LUN_GONE:
#endif STANDALONE
		default:
			break;
	}
	un->un_flags |= IVJ_ALIVE;
	if( !(un->un_flags & INT_DISK ) ){
	    dkvj_un_iodone( un );
	    return;
	}

	vjgetvolume(un, NO_INTERRUPT);
	/*
	 * Read disk capacity.
	 */
	readcap = (SCSI_READCAP *)K0_TO_K1(&un->un_readcap);
	if (!ka_to_vmeaddr(un->un_sph,readcap, &io_addr))
	  cmn_err(CE_PANIC, "Can't map read capacity structure !\n");

	dkvj_wb_cache(readcap, sizeof(SCSI_READCAP));
        vjcmd(un,VJ_READCAP,io_addr,0,sizeof(SCSI_READCAP),NO_INTERRUPT);

        if (vjwait_unit(un, M_CRSW_CC, VJ_READCAP)) 
            if (vjexterr)
                cmn_err(CE_CONT,"DKVJ %d:%d: Cannot read capacity\n",
                    un->un_ctlr,un->un_slave);

	/*
	 * Get error recovery parameters
	 */
        err = (SCSI_MS_ERROR *)K0_TO_K1(&un->un_mserror);
	temp = sizeof(SCSI_MS_ERROR);
	if (!ka_to_vmeaddr(un->un_sph,err, &io_addr))
	  cmn_err(CE_PANIC, "Can't map error recovery parameters !\n");

	dkvj_wb_cache(err, sizeof(SCSI_MS_ERROR));
        vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_ERROR,temp,NO_INTERRUPT);

        if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD)) {
            if (vjexterr)
	        cmn_err(CE_CONT,
		    "DKVJ %d:%d: cannot get error recovery parameters\n",
	            un->un_ctlr,un->un_slave);
        } else if ((err->per != 1) || (err->eec != 0) || (err->dcr != 0)) {
	    temp1 = sizeof(SCSI_MS_ERROR) -
		(sizeof(SCSI_MODE_SENSE) + sizeof(SCSI_MS_PAGE_HDR));
	    if (err->error_hdr.page_length != temp1) {
	        temp = err->error_hdr.page_length +
		    sizeof(SCSI_MODE_SENSE) + sizeof(SCSI_MS_PAGE_HDR);
	        if (temp > (sizeof(SCSI_MS_ERROR) + sizeof(un->un_mspad))) {
		    cmn_err(CE_CONT,
		      "DKVJ %d:%d: no space for error recovery parameters\n",
		        un->un_ctlr,un->un_slave);
		    goto nochange;
	        }
	        vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_ERROR,temp,NO_INTERRUPT);
                if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD)) {
		    if (vjexterr)
		        cmn_err(CE_CONT,
			"DKVJ %d:%d: cannot get error recovery parameters\n",
			    un->un_ctlr,un->un_slave);
		    goto nochange;
	        }
	    }
	    err->per = 1;
	    err->eec = 0;
	    err->dcr = 0;
	    err->msense.hdr.sense_data_len = 0;
	    err->error_hdr.ps = 0;
	    vjcmd(un,VJ_MODE_SELECT_CMD,io_addr,PD_ERROR,temp,NO_INTERRUPT);
            if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SELECT_CMD))
	        if (vjexterr)
		    cmn_err(CE_CONT,
		    "DKVJ %d:%d: cannot set error recovery parameters\n",
		        un->un_ctlr,un->un_slave);
	    
        }
nochange:
	/*
	 * Get disconnect/reconnect parameters
	 */
        disrec = (SCSI_MS_DISREC *)K0_TO_K1(&un->un_msdisrec);
	if (!ka_to_vmeaddr(un->un_sph,disrec, &io_addr))
	  cmn_err(CE_PANIC, "Can't map disconnect/reconnect parameters!\n");

	dkvj_wb_cache(disrec, sizeof(SCSI_MS_DISREC));
        vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_DISREC,
			sizeof(SCSI_MS_DISREC),NO_INTERRUPT);
        if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD))
            if (vjexterr)
	        cmn_err(CE_CONT,
		"DKVJ %d:%d: cannot get disconnect/reconnect parameters\n",
	            un->un_ctlr,un->un_slave);

	/* 
	 * Get format parameters
	 */
        msformat = (SCSI_MS_FORMAT *)K0_TO_K1(&un->un_msformat);
	if (!ka_to_vmeaddr(un->un_sph,msformat, &io_addr))
	  cmn_err(CE_PANIC, "Can't map format parameters!\n");

	dkvj_wb_cache(msformat, sizeof(SCSI_MS_FORMAT));
        vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_FORMAT,
			sizeof(SCSI_MS_FORMAT),NO_INTERRUPT);
        if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD))
            if (vjexterr)
	        cmn_err(CE_CONT,"DKVJ %d:%d: cannot get format parameters\n",
	            un->un_ctlr,un->un_slave);

	/* 
	 * Get disk geometry info
	 */
        msgeom = (SCSI_MS_GEOM *)K0_TO_K1(&un->un_msgeom);
	if (!ka_to_vmeaddr(un->un_sph,msgeom, &io_addr))
	  cmn_err(CE_PANIC, "Can't map disk geometry info!\n");

	dkvj_wb_cache(msgeom, sizeof(SCSI_MS_GEOM));
        vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_GEOM,
		sizeof(SCSI_MS_GEOM),NO_INTERRUPT);
        if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD))
            if (vjexterr)
	        cmn_err(CE_CONT,
		    "DKVJ %d:%d: cannot get disk geometry parameters\n",
	            un->un_ctlr,un->un_slave);
        if (usepc8(inquiry))
	    un->un_flags |= INT_PC8;
        if (un->un_flags & INT_PC8) {
	    /*
	     * Get read ahead parameters
	     */
	    rhd = (SCSI_MS_RDAHD *)K0_TO_K1(&un->un_msrdahd);
	    if (!ka_to_vmeaddr(un->un_sph,rhd, &io_addr))
	      cmn_err(CE_PANIC, "Can't map disk geometry info!\n");
    
	    dkvj_wb_cache(rhd, sizeof(SCSI_MS_RDAHD));
	    vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_RDAHD,
			sizeof(SCSI_MS_RDAHD),NO_INTERRUPT);
            if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD)){
	        if (vjexterr)
		    cmn_err(CE_CONT,
		    "DKVJ %d:%d: cannot get cache control parameters\n",
		        un->un_ctlr,un->un_slave);
	    } else if (rhd->rcd == 1) {
	        rhd->rcd = 0;
	        rhd->msense.hdr.sense_data_len = 0;
	        rhd->rdahd_hdr.ps = 0;
	        vjcmd(un,VJ_MODE_SELECT_CMD,io_addr,PD_RDAHD,
			    sizeof(SCSI_MS_RDAHD),NO_INTERRUPT);
                if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SELECT_CMD))
		    cmn_err(CE_WARN,"DKVJ %d:%d: Cannot enable disk cache\n",
		        un->un_ctlr,un->un_slave);
	    }
        } else {
	    /* 
	     * Get cache control parameters
	     */
	    cp = (SCSI_MS_CACHE *)K0_TO_K1(&un->un_mscache);
	    if (!ka_to_vmeaddr(un->un_sph,cp, &io_addr))
	      cmn_err(CE_PANIC, "Can't map disk geometry info!\n");
    
	    dkvj_wb_cache(cp, sizeof(SCSI_MS_CACHE));
	    vjcmd(un,VJ_MODE_SENSE_CMD,io_addr,PD_CACHE,
			    sizeof(SCSI_MS_CACHE),NO_INTERRUPT);
            if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SENSE_CMD)){
	        if (vjexterr)
		    cmn_err(CE_CONT,
		        "DKVJ %d:%d: cannot get cache control parameters\n",
		        un->un_ctlr,un->un_slave);
	    } else if (cp->ce == 0) {
	        cp->ce = 1;
	        cp->msense.hdr.sense_data_len = 0;
	        cp->cache_hdr.ps = 0;
	        vjcmd(un,VJ_MODE_SELECT_CMD,io_addr,
			        PD_CACHE,sizeof(SCSI_MS_CACHE),NO_INTERRUPT);
                if (vjwait_unit(un, M_CRSW_CC, VJ_MODE_SELECT_CMD))
		    cmn_err(CE_WARN,"DKVJ %d:%d: Cannot enable disk cache\n",
		        un->un_ctlr,un->un_slave);
	    }
        }
	dkvj_wb_cache(un, sizeof(VJ_UNIT));
	dkvj_un_iodone( un );
	splx(s);
    }
}
void
vjgetvolume(un, mode)
register VJ_UNIT *un;
int mode;
{
    DVH *vh;
    ioaddr_t io_addr;
    sah_type temp_sph;
    VJ_CTLR *c;
    int status;
    register SCSI_EXT_SENSE *sense;
    int s;

#ifdef STANDALONE
    vh = (DVH *)&temp_buffer[0];
#else STANDALONE
    vh = (DVH *)K0_TO_K1(&temp_buffer[0]);
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
#endif STANDALONE

    un->un_xfer = 512;
    un->un_resid = 0;

    c = vjctlrs[un->un_ctlr];

    s = splbio();	/* Need to block motortimeout */
    bzero(sense, sizeof(SCSI_EXT_SENSE));
    vjcmd(un, VJ_UNIT_READY, 0, 0, 0, NO_INTERRUPT);

    if (!(status = vjwait_unit(un, M_CRSW_CC,VJ_UNIT_READY))) {
	/* good status! is he powered up? */
	if(sense->key == SCSI_NOT_READY){
	    /* check once again */
	    bzero(sense, sizeof(SCSI_EXT_SENSE));
	    vjcmd(un, VJ_UNIT_READY, 0, 0, 0, NO_INTERRUPT);
	    if (!(status=vjwait_unit(un,M_CRSW_CC,VJ_UNIT_READY))){
		if(sense->key == SCSI_NOT_READY){
		  add_motor_start(un,mode); 
		  splx(s);
		  return;
		}
		else /* looks like like he is ready NOW */
		   un->un_flags |= IVJ_READY;
	    } else { /* bad status the 2nd time */
		   cmn_err(CE_CONT,"vjgetvolume(%d:%d): failed , status %x sense key is %s\n",un->un_ctlr, un->un_slave,status&0xff, sense_err_msg(sense->key));
		   splx(s);
		   return;
	    }
	}
	else /* looks like he is powered up and ready to go */
	    un->un_flags |= IVJ_READY;
     } else { /* bad status */
		cmn_err(CE_CONT,"vjgetvolume(%d:%d): failed , status %x sense key is %s\n",un->un_ctlr, un->un_slave,
		status&0xff, sense_err_msg(sense->key));
		splx(s);
		return;
     } 

    if((un->un_flags & IVJ_READY)) {

    if(!vme_iomap(c->c_csh, vh, sizeof(DVH),
	GBA_CONTIG_ADDR+GBA_NOPART_MAP,&temp_sph, &io_addr))
	cmn_err(CE_PANIC, "Can't map vh structure!\n");

    dkvj_wb_cache(vh, sizeof(DVH));
    
    vjcmd(un, VJ_READ, io_addr, 0, 1, NO_INTERRUPT);

    if (vjwait_unit(un, M_CRSW_CC, VJ_READ))  {
      if(!vme_iounmap( temp_sph ))	/* Release the iomap registers */
	cmn_err(CE_PANIC, "Can't flush/unmap vh!\n");
      splx(s);
      return;
    }
	
    /* flush the gba cache and unmap registers */
    if(!vme_iounmap( temp_sph ))
      cmn_err(CE_PANIC, "Can't flush/unmap vh!\n");

    if (is_vh(vh) == FALSE)
    {
	log(BSD43_LOG_ERR,
	"DKVJ %d:%d Volume Header is incorrect\n",un->un_ctlr,un->un_slave);
	un->un_vhvalid = 0;
	splx(s);
	return;
    }
    un->un_vhvalid = 1;
    bcopy(vh, &un->un_vh, sizeof(DVH));
    }
    splx(s);
}

static void
add_motor_start(un,mode)
register VJ_UNIT *un;
int mode;
{
    register SCSI_EXT_SENSE *sense;
    VJ_UNIT *unp;
    int status;
    register int s;

    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    bzero(sense, sizeof(SCSI_EXT_SENSE));
#ifndef STANDALONE
	
    s = splclock();
    if (!motor_start_list.active) {
	motor_start_list.active = un;
	if (vjexterr > 1)
	    cmn_err(CE_CONT,
		"SCSI %dL%d: Add_motor_start issuing motor start\n",
		un->un_ctlr,un->un_slave);

	if (mode ==  NO_INTERRUPT){  /* POLLED */

	  /*  set the immed bit,so it returns as soon as cmd is accepted */
	  vjcmd(un,VJ_LOAD,0,1,0,NO_INTERRUPT);

	  if (status = vjwait_unit(un, M_CRSW_CC, VJ_LOAD)) {
	     cmn_err(CE_CONT,"Cannot start motor (%d:%d): fail,status %x sense key is %s\n", un->un_ctlr,un->un_slave, status&0xff, 
	     sense_err_msg(sense->key));
	     motor_start_list.active = NULL;
	  } else { /* motor start has been issued, let's wait */
	    (void)timeout_spl(motortimeout, 0, 1 * HZ, splbio);
	     motor_start_list.timeout = powerseq_delay;
	  }

	} else {  /* NOT POLLED */
	     if(vjsplcmd(un,VJ_LOAD,0,0,0,WAIT)){
	       cmn_err(CE_CONT,"Cannot start motor (%d:%d): fail, sense key is %s\n", un->un_ctlr,un->un_slave,  
	       sense_err_msg(sense->key));
	       motor_start_list.active = NULL;
	     } else { /* motor start has been issued, let's wait */
	    (void)timeout_spl(motortimeout, 0, 1 * HZ, splbio);
	     motor_start_list.timeout = powerseq_delay;
	    }
	}
    } else if ((un != motor_start_list.active) &&
	       (un != motor_start_list.next)){
	if (vjexterr > 1)
	    cmn_err(CE_CONT,"SCSI %dL%d: On motor_start list\n",
		un->un_ctlr,un->un_slave);
	unp = (VJ_UNIT *)&motor_start_list;
	if (unp->un_motorstart) {
	    while (unp->un_motorstart) {
		if (unp->un_motorstart == un) {
		    unp->un_motorstart = un->un_motorstart;
		    un->un_motorstart = motor_start_list.next;
		    unp = (VJ_UNIT *)&motor_start_list;
		    break;
		}
		unp = unp->un_motorstart;
	    }
	}
	unp->un_motorstart = un;
    }
    splx(s);
#else STANDALONE
    vjcmd(un,VJ_LOAD,0,0,0,NO_INTERRUPT);
    if(status = vjwait_unit(un, M_CRSW_CC, VJ_LOAD)) {
	cmn_err(CE_CONT,"Cannot start motor (%d:%d): fail,status %x 
        sense key is %s\n", un->un_ctlr,un->un_slave, status&0xff, 
	sense_err_msg(sense->key));
    } else {
        un->un_flags |= IVJ_READY;
    }
#endif STANDALONE
}

#ifndef STANDALONE

/* The following routine is invoked at level splbio */

static void
motortimeout()
{
    register VJ_UNIT *un;
    int status;
    register SCSI_EXT_SENSE *sense;
    int Dkvj_Error_save;

    /* pull the first one out of the queue and put the second one ahead */
    
    if (!motor_start_list.active) {
	cmn_err(CE_CONT,"SCSI: Motor Start Timeout with no active unit\n");
    }
    Dkvj_Error_save = Dkvj_Error;
doagain:
    /* Check if unit is ready */
    un = motor_start_list.active;
    if ((un->un_flags & IVJ_READY) == 0) {  /* disk READY? */
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	bzero(sense, sizeof(SCSI_EXT_SENSE));
    	vjcmd(un,VJ_UNIT_READY,0,0,0,NO_INTERRUPT);
    	if(!(status = vjwait_unit(un, M_CRSW_CC, VJ_UNIT_READY))) {
	    if (sense->key == SCSI_NOT_READY) {
	        if ((motor_start_list.timeout -= 1) > 0) {
		  /* Not yet ready, but timeout not exhausted.  Wait ... */
		  (void)timeout_spl(motortimeout, 0, 1 * HZ, splbio);
		  Dkvj_Error = Dkvj_Error_save;	/* restore global error flag */
		  return;
		}
		/* Power sequencing delay exhausted, but drive not yet up.
		 * Go on to next drive WITHOUT marking drive ready.  Caller
		 * will typically timeout after a maximum powerup time.
		 */
	      } else
		un->un_flags |= IVJ_READY;
	} else  /* bad status */
	    cmn_err(CE_CONT, "DKVJ%d:%d: Powerup bad status\n", un->un_ctlr, un->un_slave);
     }	    

    /* Move to next unit requiring a motor start */

    motor_start_list.active->un_motorstart = NULL;
    un = motor_start_list.next;
    motor_start_list.active = un;
    if (un)
	motor_start_list.next = un->un_motorstart;
    else
	motor_start_list.next = NULL;
    if (un) {
	if(un->un_flags & IVJ_READY || un->un_tab->b_active)
	    goto doagain;
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	bzero(sense, sizeof(SCSI_EXT_SENSE));
	if (vjexterr > 1)
	    cmn_err(CE_CONT,"SCSI %dL%d: Motortimeout issuing motor start\n",
		un->un_ctlr,un->un_slave);
	
	/* send command with the immed bit set */
    	vjcmd(un,VJ_LOAD,0,1,0,NO_INTERRUPT);

    	if(status = vjwait_unit(un, M_CRSW_CC, VJ_LOAD)) {
	    cmn_err(CE_CONT,"Cannot start motor (%d:%d): fail,status %x sense key is %s\n", un->un_ctlr,un->un_slave, status&0xff, 
	    sense_err_msg(sense->key));
	    
	    /* failed to start, so start the next guy in the line */
	    goto doagain;
	} else { /* successfully started, let's wait for completion */
	    (void)timeout_spl(motortimeout, 0, 1 * HZ, splbio);
	    motor_start_list.timeout = powerseq_delay;
	}
    }
    Dkvj_Error = Dkvj_Error_save;	/* restore global error flag */
}
static int
wait_ready(un,mode)
register VJ_UNIT *un;
int mode;
{
    register int wait_amount;
    register SCSI_EXT_SENSE *sense;
    int s;

    if ((un->un_flags & IVJ_READY) == 0) {  /* disk READY? */
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	bzero(sense, sizeof(SCSI_EXT_SENSE));
	wait_amount = powerup_time;
	add_motor_start(un,mode); /* move him to the head of the queue */
checkagain:
	/* If we're waiting awhile, let operator now (approx every 32 secs) */
	if ((wait_amount & 0x1f) == 0)
	  cmn_err(CE_CONT,
		  "DKVJ %d:%d drive NOT ready after %d secs, wait %d more\n",
		  un->un_ctlr, un->un_slave, powerup_time-wait_amount,
		  wait_amount);

	/* now let's wait some time for him */
	bzero(sense, sizeof(SCSI_EXT_SENSE));
	s = splbio();		/* Block motortimeout until have status */
    	vjcmd(un,VJ_UNIT_READY,0,0,0,NO_INTERRUPT);
    	if(!vjwait_unit(un, M_CRSW_CC, VJ_UNIT_READY)) {
	    if (sense->key == SCSI_NOT_READY) {
	      	splx(s);	/* can't re-enable until get sense value */
		DELAY(1000000);		/* wait 1 second */
		if (--wait_amount)
		    goto checkagain;
		cmn_err(CE_CONT,"DKVJ %d:%d drive NOT ready after %d secs\n",
			un->un_ctlr, un->un_slave, powerup_time);
		/* Prevent us from another long timeout the next time this
		 * routine is called.  Set "ready" flag, but no further
		 * action will occur since we can't read volume header.
		 */
		un->un_flags |= IVJ_READY;
		return(-1);
	    }
	    splx(s);
	    goto a_ok;
	} else  { /* bad status */
	    splx(s);
	    return(-1);
	}
     }	    
a_ok:
    if(vjexterr > 1)
	cmn_err(CE_CONT,"DKVJ %dL%d: drive ready after %d tries\n",
		    un->un_ctlr, un->un_slave, powerup_time-wait_amount);

    un->un_flags |= IVJ_READY;
    if (!un->un_vhvalid)
	vjgetvolume(un,mode);
	
    return(0);
}

#else STANDALONE
static int
wait_ready(un,mode)
register VJ_UNIT *un;
int mode;
{
    if ((un->un_flags & IVJ_READY) == 0) {  /* disk READY? */
	vjcmd(un,VJ_UNIT_READY,0,0,0,NO_INTERRUPT);
    	if(vjwait_unit(un, M_CRSW_CC, VJ_UNIT_READY)) {
	    return(-1);
	else {
	    un->un_flags |= IVJ_READY;
	    return(0);
	}
    } else {
        return(0);
    }
}
#endif STANDALONE


int
vjsense(un,flag)
VJ_UNIT *un;
LONG flag;
{
    ULONG i;
    SCSI_EXT_SENSE *sense;
    char is_a_tape;
    register char status;
    ioaddr_t io_addr;
    int s;

    is_a_tape = (un->un_flags & IVJ_TAPE);
#ifdef STANDALONE
    sense = &un->un_sense;
#else STANDALONE
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
#endif STANDALONE
    /*
     * Enoch says it does this 4 times, because sometimes it takes
     * a couple of tries before the drive (Micropolis especially)
     * returns the right stuff.
     */
    for (i=0; i<4; i++)
    {
	ASSERT( un->un_sph );
	if (!ka_to_vmeaddr(un->un_sph, sense, &io_addr ))
	  cmn_err(CE_PANIC, "Can't get addr for sense structure!\n");

	bzero(sense, sizeof(SCSI_EXT_SENSE));
	dkvj_wb_cache(sense , sizeof(SCSI_EXT_SENSE));		

#ifndef STANDALONE
	if (flag != NO_INTERRUPT) {
	    vjcmd(un, VJ_SENSE, io_addr , 0, 1, flag);
	    return(0);   /* interrupt routine checks status */
	}
	s = splbio();
#endif STANDALONE
	vjcmd(un, VJ_SENSE, io_addr , 0, 1, flag);
	status = vjwait_unit(un, M_CRSW_CC, VJ_SENSE);
#ifndef STANDALONE
	splx(s);
#endif
	
	if(!vme_ioflush(un->un_sph, sense, sizeof(SCSI_EXT_SENSE)))
	  cmn_err(CE_PANIC, "Can't flush sense!\n");

	if(is_a_tape && (sense->filmrk || sense->eom || sense->ilength))
		return(0);
	if (!status || (status & 0xff) == SELECT_TIMEOUT)
		break;
    }
    return(status);
}
vjwait(c, mask,cmd)
register VJ_CTLR *c;
register UWORD mask;
int cmd;
{
    register VJ_SHIO *shio;
    volatile VJ_CRB *crb;
    volatile VJ_IOPB *iopb;
    register VJ_UNIT *un;
    SCSI_CDB *cdb;
    register int count, rc, status;
    int delay_time;

    shio = c->c_io;
    crb   = &shio->sh_CRB;
    iopb = &shio->sh_RET_IOPB;
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];
    un = (VJ_UNIT *)c->c_mce.cqe_CTAG;

    dkvj_wb_cache(c, sizeof(VJ_CTLR));
    
    DELAY(4096);
    
    switch (cmd) {
      case VJ_READ:
      case VJ_WRITE:
      case VJ_FORMAT:
      case VJ_SEEK:
      case VJ_LOAD:
      case VJ_REWIND:
		count = 100000;
		delay_time = 1024;
		break;
      default:
		count = 10000;
		delay_time = 256;
		break;
    }
    
    while(!(W(crb->crb_CRSW) & M_CRSW_CRBV) && count--){
#ifdef STANDALONE
	_scandevs();	/* bounce the LEDs and scan for abort */
#endif STANDALONE
        DELAY(delay_time);
    }

    rc = 0;
    if (count <= 0) {
        rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d:%d CRSW(0x%x) CRBV bit not found.\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW));
	Dkvj_Error = TRUE;
    }else if ((W(crb->crb_CRSW) & mask) != mask) {
        rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d:%d CRSW(0x%x) mask=%x\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW),mask);
    }
    if (cdb->cmd == C0_REQSENSE) {
	if (vjsense_status(un,iopb->iopb_STATUS))
	    rc = 1;
	else
	    rc = 0;
	goto waitdone;
    }
    if (W(crb->crb_CRSW) & M_CRSW_ER && rc == 0) {
	if (((iopb->iopb_STATUS >> 8) & 0xff) == CHECK_CONDITION) {
	    CRB_CLR_DONE(crb->crb_CRSW);
	    if (iopb->iopb_STATUS = vjsense(un,NO_INTERRUPT))
		rc = 1;
	    goto waitdone;
	}
	status = iopb->iopb_STATUS & 0xff;
	if (status) {
	    if (status == TRANSFER_COUNT_ERROR ||
		(status == BUS_RESET_STATUS && iopb->iopb_CMD == SCSI_RESET))
		rc = 0;
	    else {
		rc = 1;
#ifndef STANDALONE
		if (status != NO_SECOND_PORT && status != SELECT_TIMEOUT)
#endif STANDALONE
		  log(BSD43_LOG_ERR,
	  "DKVJ %d:%d Error: Command = 0x%x(0x%x) CRSW(0x%x) status = 0x%x\n",
		    un->un_ctlr,un->un_slave,
		    iopb->iopb_CMD, cdb->cmd, W(crb->crb_CRSW), 
		    iopb->iopb_STATUS);
	    }
	}
	else rc = 1;
    }
waitdone:
    CRB_CLR_DONE(crb->crb_CRSW);
    return(rc ? (iopb->iopb_STATUS | 0x80000) : 0);
}
vjwait_unit(un, mask,cmd)
VJ_UNIT *un;     
register UWORD mask;
int cmd;
{
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    volatile VJ_CRB *crb;
    volatile VJ_IOPB *iopb;
    SCSI_CDB *cdb;
    int count,rc, status;
    int delay_time;
    unsigned int crsw;
    int s;
    ULONG ctag;
    VJ_OPT cmdopt;
    
    c = vjctlrs[un->un_ctlr];
    shio = c->c_io;
    crb   = &shio->sh_CRB;
    iopb = &shio->sh_RET_IOPB;
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];
    un = (VJ_UNIT *)c->c_mce.cqe_CTAG;

    dkvj_wb_cache(c, sizeof(VJ_CTLR));
    
    DELAY(4096);

    switch (cmd) {
      case VJ_READ:
      case VJ_WRITE:
      case VJ_FORMAT:
      case VJ_SEEK:
      case VJ_LOAD:
      case VJ_REWIND:
		count = 100000;
		delay_time = 1024;
		break;
       case VJ_UNIT_READY:
               /* In almost all cases the default value is OK.  However,
                * we have seen at least one case during system startup
                * where it took 18 seconds to get sense status (~72,000
                * loops with delay_time of 256).  Use longer delay
                * and larger count.
                */
               count = 100000;
               delay_time = 512;
               break;
      default:
		count = 10000;
		delay_time = 256;
		break;
    }
    
    while((!((crsw = W(crb->crb_CRSW)) & M_CRSW_CRBV) ||
	   (B_OPT_IE(iopb->iopb_OPTION)) ||
	  (W(iopb->iopb_UNIT) != un->un_unit.U.w)) &&  count--) {
#ifdef STANDALONE
	_scandevs();	/* bounce the LEDs and scan for abort */
#endif STANDALONE
	if (crsw & M_CRSW_CRBV) {
	  /* Only get here if something completed but unit number wrong or
	   * the unit number may match but it was an interrupt request.
	   * If the completed request has IE on, then this is an interrupt
	   * driven request for some other unit.  If IE off, then this
	   * MUST be INIT_WORKQ which has unit number in a different place.
	   */
	  vjfrom_shio( &iopb->iopb_OPTION, &cmdopt, sizeof(VJ_OPT));
	  if (!B_OPT_IE(cmdopt)) {
	    if (cmd == VJ_INIT_WORKQ) {
	      vjfrom_shio( &crb->crb_CTAG, &ctag, sizeof(ULONG));
	      if ((ULONG)un == ctag)
		break;	/* This cmd has unit pointer in the CTAG */
	    }
	    cmn_err(CE_WARN,"DKVJ %d vjwait_unit expected unit %d got %d\n",
		    un->un_ctlr, un->un_slave, W(iopb->iopb_UNIT));
	    /* Shouldn't get non-interrupt command for another unit.
	     * Clear the interupt and wait for expected unit.
	     */
	    CRB_CLR_DONE(crb->crb_CRSW);
	    continue;
	  }

#ifndef STANDALONE
	  /* The completed request is actually an interrupt driven request
	   * for another unit.  Invoked dkvjintr to properly complete this
	   * request, then go back and wait for the expected completion.
	   */
	  s = splbio();
	  dkvjintr(un->un_ctlr);	/* CC for another unit */
	  c->c_dkvjintr_call = 1;	/* dkvjintr called from poll loop */
	  splx(s);
#endif STANDALONE
	}   /* End of case where CRBV is set */
        DELAY(delay_time);
    }

    /* Flush the cntlr structure (including CRB) from the I/O cache.
     * Also flush unit table (in case sense data returned).
     */
    if(!vme_ioflush(c->c_sph, 0, 0))
       cmn_err(CE_PANIC, "Could not flush the CTLR struct \n");
    dkvj_un_iodone( un );	/* flush unit table info from GBA */

    rc = 0;
    if (count <= 0) {
        rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d:%d CRSW(0x%x) CRBV bit not found.\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW));
	Dkvj_Error = TRUE;
    
    }else if ((W(crb->crb_CRSW) & mask) != mask) {
        rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d:%d CRSW(0x%x) mask=%x\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW),mask);
    }
    if (cdb->cmd == C0_REQSENSE) {
	if (vjsense_status(un,iopb->iopb_STATUS))
	    rc = 1;
	else
	    rc = 0;
	goto waitdone;
    }
    if (W(crb->crb_CRSW) & M_CRSW_ER && rc == 0) {
	if (((iopb->iopb_STATUS >> 8) & 0xff) == CHECK_CONDITION) {
	    CRB_CLR_DONE(crb->crb_CRSW);
	    if (iopb->iopb_STATUS = vjsense(un,NO_INTERRUPT))
		rc = 1;
	    goto waitdone;
	}
	status = iopb->iopb_STATUS & 0xff;
	if (status) {
	    if (status == TRANSFER_COUNT_ERROR ||
		(status == BUS_RESET_STATUS && iopb->iopb_CMD == SCSI_RESET))
		rc = 0;
	    else {
		rc = 1;
#ifndef STANDALONE
		if (status != NO_SECOND_PORT && status != SELECT_TIMEOUT)
#endif STANDALONE
		  log(BSD43_LOG_ERR,
	  "DKVJ %d:%d Error: Command = 0x% x%x CRSW(0x%x) status = 0x%x\n",
		    un->un_ctlr,un->un_slave,
		    iopb->iopb_CMD, cdb->cmd, W(crb->crb_CRSW), 
		    iopb->iopb_STATUS);
	    }
	}
	else rc = 1;
    }
waitdone:
    CRB_CLR_DONE(crb->crb_CRSW);
    return(rc ? (iopb->iopb_STATUS | 0x80000) : 0);
}
/*
 *  routine to send commands directly to the controller
 *
 *  NOTE:
 *      flag determines if interrupts will be enabled or
 *      if the CSR will be polled.
 */
vjcmd(un, cmd, dmaddr, block, count, flag)
VJ_UNIT *un;
int cmd;
BYTE *dmaddr;
int block, count, flag;
{
    register VJ_IOPB *iopb;
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    VJ_WQCF *wqcf;                  /* Work Queue Command format */
#ifndef STANDALONE
    VJ_FQCF *fqcf;                  /* Flush Work Queue Command format */
    VJ_RESCF *rescf;                /* Reset SCSI bus Command format */
    int s, delay;
#endif STANDALONE
    VJ_CQE  *mce;                   /* Master Controller Entry   */
    VJ_CIB  *cib;
    SCSI_CDB *cdb;
    ioaddr_t io_addr;

    c = vjctlrs[un->un_ctlr];
    shio = c->c_io;
#ifndef STANDALONE
    if (W_QECR(shio->sh_MCE.cqe_QECR) & M_QECR_GO) {
	log(BSD43_LOG_ERR,
	"DKVJ %d: Master work queue not empty, cmd=0x%x\n",un->un_ctlr,cmd);
	restart_ctlr(c,un->un_ctlr);
	return;
    }
#endif STANDALONE
    mce     = &c->c_mce;
    bzero(mce, sizeof(VJ_CQE));
    if (cmd == VJ_CNTR_INIT) {
        iopb                    = &c->c_miopb;
        cib                     = &c->c_cib;
        bzero(iopb, sizeof(VJ_IOPB));
        bzero(cib, sizeof(VJ_CIB));
        c->c_mce_iopb           = (VJ_IOPB *)&shio->sh_MCE_IOPB;
        mce->cqe_IOPB_ADDR      = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG           = (ULONG)un;
        mce->cqe_IOPB_LENGTH    = sizeof(VJ_IOPB)/2;
        iopb->iopb_CMD          = CNTR_INIT;
        W(iopb->iopb_ADDR)      = SHIO_MOD;
        iopb->iopb_BUFF         = (UWORD)((INT)&shio->sh_CIB - (INT)shio);
        iopb->iopb_LENGTH       = S_CIB;
    	if (c->c_mode & C_OFF_BOARD)
       		cib->cib_NCQE           = NUM_CQE_OFF;
	else
       		cib->cib_NCQE           = NUM_CQE;
        cib->cib_BURST          = VJ_BURST_COUNT;
        W(cib->cib_NVECT)       = VEC(c, c->c_nintvec);
        W(cib->cib_EVECT)       = VEC(c, c->c_eintvec);
        W(cib->cib_PID)         = DEFAULT_SCSI_ID;
        W(cib->cib_SID)         = DEFAULT_SCSI_ID;
        cib->cib_SELECT_msw     = (VJ_SELECTION_TIMEOUT >> 16);
        cib->cib_SELECT_lsw     = VJ_SELECTION_TIMEOUT & 0xffff;
        cib->cib_RESELECT_msw   = VJ_INFINITE_TIMEOUT >> 16;
        cib->cib_RESELECT_lsw   = VJ_INFINITE_TIMEOUT & 0xffff;
	cib->cib_CRBO = (UWORD)((INT)&shio->sh_CRB - (INT)shio);
    	if (c->c_mode & C_OFF_BOARD) {

		/*
		 * get a vme addr for the crb
		 */

		    if(!ka_to_vmeaddr(c->c_sph, &c->c_off_crb, &io_addr))
		    cmn_err(CE_PANIC, " Can't map c_off_crb structure!\n");

		cib->cib_CRB_ADDRESS_msw= io_addr >> 16;
		cib->cib_CRB_ADDRESS_lsw= io_addr & 0xffff;
        	W(cib->cib_CRB_ADRMOD) = (Blockmode_ok) ? ADDR_MOD_B : ADDR_MOD;
	}
        vjto_shio(iopb, c->c_mce_iopb, sizeof(VJ_IOPB));
        vjto_shio(cib,  &shio->sh_CIB, sizeof(VJ_CIB));
    }
#ifndef STANDALONE
    else if (cmd == VJ_INIT_WORKQ) {
        wqcf                        = &c->c_wqcf;
        bzero(wqcf, sizeof(VJ_WQCF));
        mce->cqe_IOPB_ADDR          = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG               = (ULONG)un;
        mce->cqe_IOPB_LENGTH        = sizeof(VJ_WQCF)/2;
        wqcf->wqcf_CMD              = CNTR_INIT_WORKQ;
        wqcf->wqcf_NVCT             = c->c_nintvec;
        wqcf->wqcf_EVCT             = c->c_eintvec;
        wqcf->wqcf_ILVL             = c->c_level;
        wqcf->wqcf_WORKQ            = un->un_workq;
	/* Freeze queue on error and enable Parity checking */
        W(wqcf->wqcf_WOPT)          = M_WOPT_FE | M_WOPT_PE ;  
    	if (c->c_mode & C_OFF_BOARD)
        	wqcf->wqcf_SLOTS    = VJ_DISK_SLOTS_OFF + 1;
	else
        	wqcf->wqcf_SLOTS    = VJ_DISK_SLOTS + 1;
	if (un->un_flags & IVJ_TAPE)
        	wqcf->wqcf_PRIORITY = VJ_TAPE_PRIORITY; /* higher priority */
	else
        	wqcf->wqcf_PRIORITY = VJ_DISK_PRIORITY;
        vjto_shio(wqcf, c->c_mce_iopb, sizeof(VJ_WQCF));
    }
    else if (cmd == VJ_SCSI_RESET) {
        rescf                = (VJ_RESCF *)&c->c_wqcf;
        bzero(rescf, sizeof(VJ_RESCF));
        mce->cqe_IOPB_ADDR   = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG        = (ULONG)un;
        mce->cqe_IOPB_LENGTH = sizeof(VJ_RESCF)/2;
        rescf->rescf_CMD     = SCSI_RESET;
        rescf->rescf_NVCT    = c->c_nintvec;
        rescf->rescf_EVCT    = c->c_eintvec;
        rescf->rescf_ILVL    = c->c_level;
	if (flag == WANT_INTERRUPT)
		W(rescf->rescf_OPTION) = M_OPT_IE;        /* interrupt enable */
        rescf->rescf_BUSID.U.b.BUS =  un->un_unit.U.b.BUS;
        vjto_shio(rescf, c->c_mce_iopb, sizeof(VJ_RESCF));
    }
    else if (cmd == VJ_FLUSH_WORKQ) {
        fqcf                        = (VJ_FQCF *)&c->c_wqcf;
        bzero(fqcf, sizeof(VJ_FQCF));
        mce->cqe_IOPB_ADDR          = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG               = (ULONG)un;
        mce->cqe_IOPB_LENGTH        = sizeof(VJ_FQCF)/2;
        fqcf->fqcf_CMD              = CNTR_FLUSH_WORKQ;
        fqcf->fqcf_NVCT             = c->c_nintvec;
        fqcf->fqcf_EVCT             = c->c_eintvec;
        fqcf->fqcf_ILVL             = c->c_level;
        fqcf->fqcf_WORKQ            = un->un_workq;
        W(fqcf->fqcf_OPTION)        = M_FOPT_IE | M_FOPT_RPT; /* report */
        vjto_shio(fqcf, c->c_mce_iopb, sizeof(VJ_FQCF));
    }
#endif STANDALONE
    else {                /* Pass thru command      */
        iopb                    = &c->c_miopb;
        cdb                     = (SCSI_CDB *)&iopb->iopb_SCSI[0];
        bzero(iopb, sizeof(VJ_IOPB));
        mce->cqe_IOPB_ADDR      = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
	mce->cqe_CTAG           = (ULONG)un;
        mce->cqe_IOPB_LENGTH    = sizeof(VJ_IOPB)/2;
        iopb->iopb_NVCT         = c->c_nintvec;
        iopb->iopb_EVCT         = c->c_eintvec;
        iopb->iopb_LEVEL        = c->c_level;
        W(iopb->iopb_ADDR)      = ADDR_MOD; /* TT_NORMAL+MEMTYPE+AM-9 */
        iopb->iopb_BUFF         = (ULONG)dmaddr;
        iopb->iopb_LENGTH       = (SECTORSIZE * count);
#ifndef STANDALONE
        if (flag == WANT_INTERRUPT)
            W(iopb->iopb_OPTION) = M_OPT_IE; /* interrupt enable         */
#endif STANDALONE
        if (cmd == VJ_WRITE || cmd == VJ_MODE_SELECT_CMD )
            W(iopb->iopb_OPTION)  |= M_OPT_DIR;
        iopb->iopb_CMD          = macsi_cmd[ cmd ];
        W(iopb->iopb_UNIT)      = un->un_unit.U.w;
        cdb->cmd                = scsi_cmd[ cmd ];
        cdb->lun                = 0;
	switch(cdb->cmd) {
		case C0_LOAD:
			if (un->un_flags & IVJ_DISK) { /* start/stop command */
				cdb->high_addr = block; /* set Immediate bit */
				cdb->count = 1;     /* set start bit */
			}
			break;
		case C0_REQSENSE:
			CDB_ADDR(cdb,block);
			cdb->count = SENSE_LENGTH;
			iopb->iopb_LENGTH = sizeof(SCSI_EXT_SENSE);
			break;
		case C0_INQUIRY:
			cdb->count = sizeof(SCSI_INQUIRY);
			iopb->iopb_LENGTH = sizeof(SCSI_INQUIRY);
			break;
		case C0_SPACE:
			cdb->high_addr = block;  /* code field */
			CDB_XFER_LEN(cdb,count);
			break;
		case C0_WRFM:
			CDB_XFER_LEN(cdb,count);
			break;
		case C0_REASSIGN:
			W (iopb->iopb_OPTION) |= M_OPT_DIR;
			iopb->iopb_LENGTH = count;
			cdb->count = count;
			break;
		case C1_READCAP:
#ifndef STANDALONE
		case C0_RDBLOCK:
#endif
			iopb->iopb_LENGTH = count;
			break;
#ifdef STANDALONE
#ifndef PROM
		case C0_RDBLOCK:
			cdb->count = 0;
			iopb->iopb_BUFF = (ULONG)K1_TO_PHYS(&un->un_blklim);
			iopb->iopb_LENGTH = 6;
			break;
		case C0_MSENSE:
			iopb->iopb_BUFF = (ULONG)K1_TO_PHYS(&un->un_msense);
			iopb->iopb_LENGTH = sizeof(SCSI_MODE_SENSE);
			cdb->count = sizeof(SCSI_MODE_SENSE);
			break;
#endif PROM
		case C0_MSELECT:
			iopb->iopb_BUFF = (ULONG)K1_TO_PHYS(&un->un_msense);
			iopb->iopb_LENGTH = sizeof(SCSI_MODE_SENSE);
			cdb->count = sizeof(SCSI_MODE_SENSE);
			break;
#else STANDALONE
			
		case C0_MSELECT:
        		iopb->iopb_LENGTH = count;
			cdb->count = count;	/* Parameter list length */
			break;
		case C0_MSENSE:
        		iopb->iopb_LENGTH = count;
			cdb->mid_addr = block & 0x3f;	/* Page code */
			cdb->count = count;		/* allocation length */
			break;
#endif
		case C0_FORMAT:
			if (count) {
			    iopb->iopb_LENGTH = count;
			    W (iopb->iopb_OPTION) |= M_OPT_DIR;
			}
			cdb->high_addr = (((ULONG)block) >> 24) & 0xff;
			cdb->mid_addr = (((ULONG)block) >> 16) & 0xff;
			cdb->low_addr = (((ULONG)block) >> 8) & 0xff;
			cdb->count = ((ULONG)block) & 0xff;
			break;
		case C0_READ:
		case C0_WRITE:
			if (un->un_flags & IVJ_TAPE) {
			    if (un->un_flags & IVJ_VARIABLE) {
				iopb->iopb_LENGTH  =  count;
				cdb->high_addr = 0;
			    } else {
				cdb->high_addr = 1;
			    }
			    CDB_XFER_LEN(cdb,count);
			}
			else {
			    cdb->count = (count & 0xff);
			    CDB_ADDR(cdb,block);
			}
			break;
		case C0_SEEK:
			break;
		case C1_VERIFY:
			break;
		default:
			break;
	}
        vjto_shio(iopb, c->c_mce_iopb, sizeof(VJ_IOPB));
    }
    vjto_shio(mce,  &shio->sh_MCE, sizeof(VJ_CQE));
#ifndef STANDALONE
    if (flag == WANT_INTERRUPT) {
	s = splclock();
	un->un_qcount++;
	if (un->un_timeid == 0) {
		if(un->un_flags & IVJ_TAPE)
			delay = TIME_RDWR_TAPE;
		else	
			delay = TIME_RDWR_DISK;
	    	un->un_timeid = timeout_spl(vjtimeout,un,delay,splbio);
	}
	splx(s);
    }
    if (un->un_aborta) {
        CQE_AA_GO(shio->sh_MCE.cqe_QECR);
    } else 
#endif STANDALONE
        CQE_GO(shio->sh_MCE.cqe_QECR);
}
/*
 *  LATER allow open to work if device not there
 *      (for runtime formatting)
 */
#ifdef STANDALONE
_dkvjopen(io)
register struct iob *io;
{
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    register int unit = io->i_unit;
    register int ctlr = io->i_ctlr;

    if(!vj_didinit){
	dkvjinit(io);
	vj_didinit = 1;
    }
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0) {
	printf("dkij(%d,%d,%d): no controller present\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
	return(-1);
    }
    un = c->c_unit[unit];
    if (un == 0) {
        vjattach(io);
	un = c->c_unit[unit];
    }
    if (!un || (un = c->c_unit[unit]) == 0) {
	printf("dkij(%d,%d,%d): no device attached\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
	return(-1);
    }
    if (!(un->un_flags & IVJ_DISK)) {
	printf("dkij(%d,%d,%d): not a disk device\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
	return(-1);
    }
    if (!un->un_vhvalid)
    {
	vjgetvolume(un);
	if (un->un_vh.vh_pt[io->i_part].pt_nblks == 0)
	{
		printf("dkij(%d,%d,%d): bad %s ZERO\n",
			ctlr,unit,io->i_part,"partition");
	}
    }
    if (io->i_fstype == DTFS_AUTO)
	    io->i_fstype= vh_mapfstype(un->un_vh.vh_pt[io->i_part].pt_type);
    un->un_flags |= IVJ_OPEN;	/* set opened flag */
    return (0);
}
#else STANDALONE
dkvjopen(dev, flag)
dev_t dev;
int flag;
{
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    register SCSI_MODE_SENSE *ms;
    register SCSI_RDBLKLIM *rb;
    register int unit, ctlr;
    int partition = FS(dev);
    int s;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    un = c->c_unit[unit];
    if (un == 0) {
        vjattach(unit,ctlr,0);
	un = c->c_unit[unit];
    }
    if (!un || (un->un_flags & IVJ_ALIVE ) == 0) {
	u.u_error = ENODEV;
	return;
    }
    if(un->un_flags & IVJ_TAPE) {
     	if (un->un_flags & IVJ_OPEN) {
		/* Already opened */
		u.u_error = EBUSY;
		return;
	}
	un->un_flags |= IVJ_OPEN;	/* set opened flag */
	s = splbio();
	while (un->un_flags & IVJ_REWINDING) {
		un->un_flags |= IVJ_WAITING;
		if (sleep((caddr_t)un, PUSER|PCATCH)) {
			/* user gave up waiting...fail the open */
			un->un_flags &= ~(IVJ_WAITING | IVJ_OPEN);
			splx(s);
			u.u_error = EINTR;
			return;
		}
	}
	splx(s);
	if (flag & FSYNC)
		return;
	if (vjsplcmd(un,VJ_UNIT_READY,0,0,0,WAIT)) {
		log(BSD43_LOG_ERR,
		"DKVJ %d:%d tape not ready; offline or tape not installed\n",
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	if (vjsplcmd(un,VJ_RDBLKLIM_CMD,0,0,0,WAIT)) {
		log(BSD43_LOG_ERR,
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	ms = (SCSI_MODE_SENSE *)K0_TO_K1(&un->un_msense);
	if (vjsplcmd(un,VJ_MODE_SENSE_CMD,0,sizeof(SCSI_MODE_SENSE),ms,WAIT)) {
		log(BSD43_LOG_ERR,"DKVJ %d:%d error in MODE SENSE\n",
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
	if (ms->hdr.WP) {
		if (flag & FWRITE) {
		    log(BSD43_LOG_ERR,"DKVJ %d:%d write protected\n",
			un->un_ctlr,un->un_slave);
		    goto openerr;
		}
		un->un_flags |= IVJ_READONLY;
	}
	else un->un_flags &= ~IVJ_READONLY;
	ms->hdr.sense_data_len = 0;
	ms->hdr.medium_type = 0;
	ms->hdr.WP = 0;
	ms->hdr.blk_desc_len = 8;
	ms->blk_desc.nrblks = 0;
	if (rb->maxlen != rb->minlen) {
		ms->blk_desc.density_code = 0;
		ms->blk_desc.blk_len = 0;
		un->un_flags |= IVJ_VARIABLE;
	} else
	    ms->blk_desc.density_code = Tapedensity[TAPEDENSITY(dev)];
	if (vjsplcmd(un,VJ_MODE_SELECT_CMD,0,sizeof(SCSI_MODE_SENSE),ms,WAIT)) {
		log(BSD43_LOG_ERR,"DKVJ %d:%d error in MODE SELECT\n",
			un->un_ctlr,un->un_slave);
openerr:
		un->un_flags &= ~IVJ_OPEN;
		u.u_error = EIO;
		return;
	}
	if (un->un_flags & IVJ_ATN)
		log(BSD43_LOG_ERR,
		"DKVJ %d:%d unit attention; media change or drive reset\n",
			un->un_ctlr,un->un_slave);
    } else { /* DISK */
	if(wait_ready(un,WAIT)) { /* is the disk ready */
		u.u_error = ENODEV;
		return;
	}

        if (!un->un_vhvalid && (LPART(dev) != MAGIC_DEV)){
		u.u_error = ENXIO;
	        return;
	}
	else if(!un->un_vhvalid && LPART(dev) == MAGIC_DEV)
	{
		register struct partition_table *pt;

		/* Allow someone to write volume header on un-init drive */

		pt = &un->un_vh.vh_pt[ MAGIC_DEV ];
		pt->pt_firstlbn = 0;
		pt->pt_nblks    = (PTSIZE_VHDR / DEV_BSIZE);
	}
    }
    un->un_open[partition]++;
}
#endif STANDALONE

#ifdef STANDALONE
_dkvjclose(io)
register struct iob *io;
{
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    register int unit;

    c = vjctlrs[io->i_ctlr];
    un = c->c_unit[io->i_unit];

    un->un_flags &= ~(IVJ_OPEN|IVJ_WRITTEN|IVJ_READ|IVJ_FM|IVJ_RFM|IVJ_EOM|
			IVJ_ATN|IVJ_NOT_RDY);
    return (0);
}
#else STANDALONE
dkvjclose(dev, flag)
dev_t dev;
{
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register int unit, ctlr;
    int partition = FS(dev);
    SCSI_MS_ERROR *err;
    int i;

    unit        = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0 ||
	(un = c->c_unit[unit]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    if (un->un_flags & IVJ_TAPE) {
	/*
	 *  For tape only
	 */
	if (un->un_flags & IVJ_WRITTEN) {
	    if (vjsplcmd(un, VJ_W_FM,0,1,0,WAIT)) {
		   log(BSD43_LOG_ERR,"DKVJ %d:%d Error in writing file marks\n",
			un->un_ctlr,un->un_slave);
	    	   SET_ERROR(EIO);
	    } else if (un->un_flags & IVJ_VARIABLE) {
		   if (vjsplcmd(un, VJ_W_FM,0,1,0,WAIT)) {
			log(BSD43_LOG_ERR,
				"DKVJ %d:%d Error in writing file marks\n",
					un->un_ctlr,un->un_slave);
			SET_ERROR(EIO);
	    	   } else if (NOREWIND(dev)) {
			/* Backspace 1 file mark (2's complement) */
			if (vjsplcmd(un, VJ_SPACE,SP_FILEMARK,~1L + 1,0,WAIT)) {
		    		log(BSD43_LOG_ERR,"DKVJ %d:%d Error in bsf\n",
					un->un_ctlr,un->un_slave);
		    		SET_ERROR(EIO);
		   	}
	    	   }
	    }
        }
	if(!NOREWIND(dev)) {
	    un->un_eomcount = un->un_weomcount = 0;
	    un->un_flags |= IVJ_REWINDING;
	    un->un_flags &= ~(IVJ_FM|IVJ_EOM);
	    if (vjsplcmd(un, VJ_REWIND,0,0,0,NO_WAIT)) {
		log(BSD43_LOG_ERR,"DKVJ %d:%d Error in rewinding\n",
			un->un_ctlr,un->un_slave);
		SET_ERROR(EIO);
	    }
	}
#ifdef NOTYET
    }else if (un->un_flags & IVJ_DISK) {
        /*
         * Make sure ECC and maxretries are re-enabled
         */
        un->un_retries = MAX_RETRIES;
	err = (SCSI_MS_ERROR *)K0_TO_K1(&un->un_mserror);
	err->per = 1;
	err->dcr = 0;	/* enable ECC */
	i = un->un_vh.vh_dp.dp_nretries;
	err->retry_cnt = i ? i : 27;
	err->msense.hdr.sense_data_len = 0;
 	i = err->error_hdr.page_length + sizeof(SCSI_MODE_SENSE) +
             sizeof(SCSI_MS_PAGE_HDR);
	vjsplcmd(un,VJ_MODE_SELECT_CMD,PD_ERROR,i,err,WAIT);
#endif
    }

    un->un_flags &= ~(IVJ_OPEN|IVJ_WRITTEN|IVJ_READ|IVJ_ATN|IVJ_RFM|
			IVJ_NOT_RDY|IVJ_VARIABLE);
    un->un_open[partition] = 0;

}
#endif STANDALONE

#ifdef STANDALONE
_dkvjstrategy(io, func)
register struct iob *io;
register int func;
{
    register struct partition_table *pt;
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    unsigned addr, lbn;
    int blks, rc;

    c   = vjctlrs[io->i_ctlr];
    un  = c->c_unit[io->i_unit];
    rc = 0;
    un->un_xfer = io->i_cc;
    un->un_resid = 0;

    if ((func == READ) || (func == WRITE))
    {
	pt = &un->un_vh.vh_pt[io->i_part];
	if ((unsigned)io->i_bn > pt->pt_nblks)
	{
	    printf("read beyond end of partition\n");
	    io->i_errno = ENXIO;
	    return(-1);
	}
	lbn = io->i_bn + pt->pt_firstlbn;
    }
    switch(func)
    {
	case READ:
	    if (io->i_cc % DEV_BSIZE)
	    {
		printf("cc not multiple of sector size\n");
		io->i_errno = EIO;
		return(-1);
	    }
	    blks = io->i_cc /DEV_BSIZE;
	    addr = K1_TO_PHYS(io->i_ma);
	    vjcmd(un, VJ_READ, (char *)addr, lbn, blks, NO_INTERRUPT);
	    rc = vjwait_unit(un, M_CRSW_CC, VJ_READ);
	    clear_cache(io->i_ma, io->i_cc);
	    break;
	case WRITE:
	    if (io->i_cc % DEV_BSIZE)
	    {
		printf("cc not multiple of sector size\n");
		io->i_errno = EIO;
		return(-1);
	    }
	    blks = io->i_cc /DEV_BSIZE;
	    addr = K1_TO_PHYS(io->i_ma);
	    vjcmd(un, VJ_WRITE, (char *)addr, lbn, blks, NO_INTERRUPT);
	    rc = vjwait_unit(un, M_CRSW_CC, VJ_WRITE);
	    break;
	default:
		_io_abort("dkvjscsi: bad function.");
		rc = 1;
		break;
    }
    if (rc)
    	io->i_errno = ENXIO;
    return(io->i_cc - un->un_resid);
}
#else STANDALONE

#define		b_cylin		b_resid
#define		pblkno		buf_ext->ext_back

dkvjstrategy(bp)
register BUF *bp;
{
    register struct partition_table *pt;
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register int unit, ctlr;
    register daddr_t bn;
    register VJ_DISK_HD *dp;
    SCSI_RDBLKLIM *rb;
    struct volume_header *vh;
    struct device_parameters *devp;
    int s, sc;
    BUF_EXT *buf_ext, *buf_ext1;

    unit = BPTOVJN(bp);
    BPTOVJC(ctlr,bp);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0 || 
	    ((un = c->c_unit[unit])) == 0) {
	u.u_error = ENXIO;
	goto badio;
    }
    if ((un->un_flags & IVJ_ALIVE ) == 0) {
	bp->b_error = ENXIO;
	goto badio;
    }
    if (un->un_flags & IVJ_DISK) {
	 if(!(bp->b_flags & B_SPL)){
	/*
	 * Check for some disk specific errors 
	 */
	if (un->un_vhvalid == 0)
	    goto badio;
	pt = &un->un_vh.vh_pt[LPART(bp->b_dev)];
	sc = BTOBB(bp->b_bcount);
	bn = bp->b_blkno;
	if (bn < 0 || (bn + sc) > pt->pt_nblks){
	    bp->b_error = ESPIPE;           /* illegal seek */
	    goto badio;
	} else if (bn == pt->pt_nblks){     /* This is an EOF condition */
	    bp->b_resid = bp->b_bcount;
	    iounmap(bp);
	    iodone(bp);
	    return;
	} 
    	if (!(c->c_mode & C_MACSI_SORT)) {
		/* set cylinder number for disksort() */
		vh = &un->un_vh;
		devp = &vh->vh_dp;
		bp->b_cylin = (bn + pt->pt_firstlbn) /
				 (devp->dp_secs * devp->dp_trks0);
	}
	} else
	   bp->b_cylin = 0;
    } else { /* TAPE */
	/* TAPE ONLY */
	if (un->un_flags & IVJ_RFM && !(bp->b_flags & B_SPL) &&
		bp->b_flags & B_READ) {
	    un->un_flags &= ~IVJ_RFM;
	    goto badio1;		/* EOF condition */
	}
	if (!(bp->b_flags & B_SPL)) {
	    if (un->un_flags & IVJ_VARIABLE) {
		rb = (SCSI_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
		if (bp->b_bcount > (unsigned)rb->maxlen ||
		    bp->b_bcount < (unsigned)rb->minlen) {
		    cmn_err(CE_CONT,
			"DKVJ %d:%d: invalid block length request %d\n",
			un->un_ctlr,un->un_slave,bp->b_bcount);
		    bp->b_error = EIO;
		    goto badio;
		}
	    }
	    if (un->un_weomcount ==1 && !(bp->b_flags & B_READ)) {
		++un->un_weomcount;
		bp->b_error = ENOSPC;
		goto badio;
	    } else if (un->un_eomcount > MAXEOM) {
		cmn_err(CE_CONT,
			"DKVJ %d:%d: too many commands past eom, %d allowed\n",
			un->un_ctlr,un->un_slave,MAXEOM);
		bp->b_error = ENOSPC;
		goto badio;
	    }
	}
	/*
	 * set cylinder number for disksort()
	 */
	bp->b_cylin = 0;
	if (un->un_flags & IVJ_VARIABLE)
	    sc = 1;
	else
	    sc = bp->b_bcount >> SCTRSHFT;
    }
    s  = splbio();
    while (!(buf_ext = c->buf_ext_hd)) { /* get top free entry   */
	sleep((caddr_t)c, PRIBIO);
    }
    c->buf_ext_hd = buf_ext->nxt;/* unlink */
    bp->av_back = (BUF*)buf_ext; /* pointer to our buf extension struct */
    if ((un->un_flags & IVJ_DISK) && !(bp->b_flags & B_SPL))
	/* save physical block number for vjSGsetup rd/wr combining */
	pblkno = (BUF*)(bn + pt->pt_firstlbn);
    else
	pblkno = 0;
    if(!(bp->b_flags & B_SPL)) /* not for Special commands (we setup bp) */
        iomap(bp);
    if (c->c_mode & C_MACSI_SORT) {
    	    buf_ext->ext_forw1 = (BUF *)0;
	    dp = &c->c_disk_hd;
	    if (dp->av_forw == (BUF *)0)
		dp->av_forw = bp; /* we're it */
	    else {
                buf_ext1 = (BUF_EXT*)dp->av_back->av_back;
		buf_ext1->ext_forw1 = bp; /* otherwise but it on the end */
	    }
	    dp->av_back  = bp;		  /* we're the new tail end */
    }
    bp->b_error = 0;  /* zero out MACSI retry count */

    /* start accounting for this buffer */
    bp->b_start = lbolt;
    un->un_iotime->io_cnt++;
    un->un_iotime->io_bcnt += sc;
    if (!(c->c_mode & C_MACSI_SORT)) {
	    un->un_tab->qcnt++;
	    /* queue request */
	    disksort(un->un_tab, bp, un->un_lastcyl);
	    if (un->un_tab->b_active == 0) /* Que up if not already there */
		dkvjustart(un);
	    if (c->c_tab->b_actf)
		c->c_sstart += dkvjcstart(c);
    } else
	    c->c_sstart += vjgo(c);
    splx(s);
    return;
badio:
    bp->b_flags |= B_ERROR;
badio1:
    s  = splbio();
    bp->b_resid = bp->b_bcount;     /* whole request failed */
    if(!(bp->b_flags & B_SPL))
    	iounmap(bp);
    iodone(bp);
    if (bp->b_flags & B_SPL)
	bp->b_flags &= ~(B_BUSY | B_SPL);
    if (bp->b_flags & B_WANTED) {
	if( bp != &un->un_sbuf ){
	    /*
	     * Can't give away the "special" buffer
	     */
	    bp->b_flags &= ~B_WANTED;
	 wakeup((caddr_t)bp);
	}
    }
    splx(s);
}
/*
 * Unit start routine.  If unit is already active, or if unit has nothing
 * to do, just return.  Otherwise, put this unit queue on the controllers
 * activity queue.  The controllers b_actf and b_actl pointers are used
 * to point to the first unit header which needs processing.  The b_forw and
 * b_back linkage is used within each unit header to link multiple units to
 * a controller.
 */
dkvjustart(un)
register VJ_UNIT *un;
{
register VJ_CTLR *c;
register struct iobuf *utab;

	c = un->un_c;
	utab = un->un_tab;
	if (utab->b_active || (utab->b_actf == NULL))
		return;
	/* put unit on controller queue */
	if (c->c_tab->b_actf == NULL)
		c->c_tab->b_actf = (struct buf *) utab;
	else
		c->c_tab->b_actl->b_forw = (struct buf *) utab;
	c->c_tab->b_actl = (struct buf *) utab;
	utab->b_forw = NULL;
	utab->b_active = 1;
}
/*
 *	We will link the unit structure to the end of controller Q to insure
 *	that no unit gets starved off.
 *
 *	RETURNS: The number of commands started.
 */
dkvjcstart(c)
register VJ_CTLR *c;
{
register struct iobuf *utab;
register struct iobuf *utabn;
register struct iobuf *ctab;
register struct buf *bp;
register int cnt = 0;
register VJ_UNIT *un;
register int unit;
register struct iobuf *savetab;
int twice = 0;

	ctab = c->c_tab;
	utab = (struct iobuf *) ctab->b_actf;	/* get the Top unit	*/
	savetab = (struct iobuf *) 0;
	/*
	 * If Loop while there is something to do
	 */
	while(utab) {
		utabn = (struct iobuf *)utab->b_forw;
	/*
	 * Get first command off unit queue.  If this unit's queue is now
	 * empty, unlink from ctlr Q and try next;
	 */
		if ((bp = utab->b_actf) == NULL) {
			/*
			 * unit on controller queue is idle
			 */
			utab->b_active = 0;
			utab->b_forw = NULL;
			ctab->b_actf = (struct buf*)utabn;
			utab = utabn;
			continue;	/* start at top again */
		}
#ifdef MACSI_THROTTLE
		/* 
		 * this allows dsort to work by limiting the
		 * number of commands that can be sent to the
		 * board per unit 
		 */
		unit = BPTOVJN(bp);
		un = c->c_unit[unit];
		if (un->un_qcount == c->c_max_to_que) {
		    if(utabn) { /* another active unit? */
			if (!savetab) /* avoid a deadlock! */
				savetab = utab;
			else if (utabn == savetab)
				return(cnt);
			ctab->b_actf = (struct buf*) utabn;
			ctab->b_actl->b_forw = (struct buf *) utab;
			ctab->b_actl = (struct buf*) utab;	
			utab->b_forw = NULL;
			utab = utabn;
			continue;
		    } else return(cnt);
		}
#else
		if (!c->dkvjsg_hd) /* free scatter/gather entry? */
			return(cnt);
#endif MACSI_THROTTLE
        	if (vjenq(bp))          /* Controller Full?	*/
			break;		/* YES, get out of here	*/
		cnt++;
		/* this snippet is to allow 2 commands/que */
		if (utab->b_actf && !twice)
			twice++;
		else twice = 0;
	/*
	 * If there is i/o for another unit put the next unit at
	 * the top of the ctlr's Q and put the unit just queued at the end
	 * of the ctlr's i/o Q.
	 * Allow this after we try and send two cmds down for a unit.
	 * Otherwise we just sit on the same unit for as long as we can.
	 *
	 */
		if(utabn && !twice) {
			ctab->b_actf = (struct buf*) utabn;
			ctab->b_actl->b_forw = (struct buf *) utab;
			ctab->b_actl = (struct buf*) utab;	
			utab->b_forw = NULL;
			utab = utabn;
		}
	}
	return(cnt);
}
/*
 * scan thru all of the bp's and send them to the board.
 */
vjgo(c)
register VJ_CTLR *c;
{
    register BUF *bp, *actf;
    register cnt = 0;
    register VJ_DISK_HD *dp;
    BUF_EXT *buf_ext;

    dp = &c->c_disk_hd;
    bp = dp->av_forw;
    while(bp && !c->c_wait_cqe) {
        if (!c->dkvjsg_hd) 			/* free SG list entry?      */
            return(cnt);
        buf_ext = (BUF_EXT*)bp->av_back;
        actf = buf_ext->ext_forw1;
        if (!vjenq(bp)) {                       /* Command given to Jaguar?  */
            dp->av_forw = actf;                 /* delete it from link list  */
            if (bp == dp->av_back) {   		/* was it the last entry?    */
                dp->av_back = dp;     		/* then update head end ptr  */
	    }
	    cnt++;
        } else                                  /* jaguar full (wait for int)*/
	    break;
        bp  = actf;                             /* new forward link of list  */
    }
    return(cnt);
}
/*
 * iopb que routine
 */
vjenq(bp)
register BUF *bp;
{
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register VJ_UNIT *un;
    register int bn, ctlr, unit, length, cmd;
    register USHORT qecr;
    register struct iobuf *utab;
    register VJ_DISK_HD *dp;
    SCSI_CDB *cdb;
    SCSI_ECDB *ecdb;
    VJ_IOPB  *iopb;
    VJ_CQE   *cqe;
    OFFBD_IOPB   *off;
    VJ_CQE   Cqe;
    int s, delay;
    BUF_EXT *buf_ext, *buf_ext1;
    ioaddr_t io_addr;
    char *cp;

#ifdef DKVJ_DEBUG
    ASSERT(!(bp->b_flags & B_DONE));
#endif DKVJ_DEBUG    
    unit    = BPTOVJN(bp);
    BPTOVJC(ctlr,bp);
    c       = vjctlrs[ctlr];
    un      = c->c_unit[unit];
    if (!(c->c_mode & C_MACSI_SORT))
	    utab = un->un_tab;
    if (!(un->un_flags & IVJ_ALIVE)) {
    	if (!(c->c_mode & C_MACSI_SORT)) {
		utab->b_actf = bp->av_forw;	/* unlink from list	*/
		utab->qcnt--;			/* one less entry	*/
	}
	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	bp->b_resid = bp->b_bcount;
        if(!(bp->b_flags & B_SPL))
    	   iounmap(bp);
        iodone(bp);
	if (bp->b_flags & B_SPL)
	    bp->b_flags &= ~(B_BUSY | B_SPL);
	if (bp->b_flags & B_WANTED) {
	    bp->b_flags &= B_WANTED;
	    wakeup((caddr_t)bp);
	}
	return(0); /* allow to contiue */
    }
    shio    = c->c_io;
    if (c->c_mode & C_OFF_BOARD)
	    off = (OFFBD_IOPB *)c->c_off_QHDP;
    cqe = (VJ_CQE *)((INT)c->c_cqe_QHDP + (INT)shio);
    if ((qecr = W_QECR(cqe->cqe_QECR)) & M_QECR_GO) {
#ifdef DKVJ_DEBUG
	log(BSD43_LOG_ERR,"DKVJ %d:%d CQE's full, interrupt on queue avail\n",
				ctlr,unit);
#endif DKVJ_DEBUG
        if (!c->c_wait_cqe) { /* if we've not already been through here */
	    /* XXX do we want to wait till half empty? think about it.  */
            W(shio->sh_MCSB.mcsb_IQAR)   = /* interrupt on queue half full */
                (M_IQAR_IQEA | M_IQAR_IQHE | VEC(c,c->c_qintvec));
            wbflush();
            c->c_wait_cqe = TRUE;
        }
        return(1);
    } else {
	    if (un->un_qcount >= un->un_queue_size) /* overflowing this que? */
            	    return(1);			    /* wait for normal int   */
#ifdef DKVJ_DEBUG
	    if (vj_verify_bp( c, bp, 0)) /* cntlr should NOT have bp yet! */
	      vj_c_printf(c);
#endif DKVJ_DEBUG	
    	    if (!(c->c_mode & C_MACSI_SORT)) {
		    utab->b_actf = bp->av_forw;	/* unlink from list	*/
		    utab->qcnt--;		/* one less entry	*/
	    }
            buf_ext = (BUF_EXT*)bp->av_back;
    	    dp = &c->c_disk_hd;
	    /*
	     * save list of bp's sent to board (for catastrophic recovery)
	     */
	    buf_ext->ext_forw = (BUF *)0;
	    if (dp->b_forw == (BUF *)0)
		dp->b_forw = bp;
	    else {
                buf_ext1 = (BUF_EXT*)dp->b_back->av_back;
		buf_ext1->ext_forw = bp;
 	    }
	    dp->b_back = bp;

            if (bp->b_flags & B_SPL)
                cmd = bp->b_length;
            else {
		if (bp->b_flags & B_READ) {
			cmd = VJ_READ;
			un->un_flags |= IVJ_READ;
		} else {
			cmd = VJ_WRITE;
			un->un_flags |= IVJ_WRITTEN;
		}
		if (un->un_flags & IVJ_DISK) {
			bn = bp->b_blkno + 
				un->un_vh.vh_pt[LPART(bp->b_dev)].pt_firstlbn;
			un->un_lastcyl = bp->b_cylin; /* save the cyl we're on*/
		}
	    }
	    /* this could be the second time through for this bp! */
	    if (buf_ext->ext_resid) { /* saved residual */
    	    	bp->b_resid = buf_ext->ext_resid;
    		bn += ((bp->b_bcount - bp->b_resid) >> SCTRSHFT);
		buf_ext->ext_resid = 0;
	    } else
    	    	bp->b_resid = bp->b_bcount;
    	    dp->b_start = lbolt;	/* start io accounting */
    	    if (c->c_mode & C_OFF_BOARD)
            	iopb = (VJ_IOPB *)(&off->copyiopb);
	    else
            	iopb = &un->un_iopb;
            cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];
	    /*
	     * XXX Determine what item(s) need to be cleared!!
 	     */
            bzero(cdb, sizeof(SCSI_CDB)); /* clean it out! */
            iopb->iopb_CMD              = macsi_cmd[ cmd ]; wbflush();
            if (cmd == VJ_WRITE || cmd == VJ_MODE_SELECT_CMD || cmd == VJ_REASSIGN) {
            	W(iopb->iopb_OPTION)  = (M_OPT_DIR + M_OPT_IE);
            } else {
                W(iopb->iopb_OPTION)  = M_OPT_IE;
	    }
            iopb->iopb_NVCT             = c->c_nintvec; wbflush();
            iopb->iopb_EVCT             = c->c_eintvec; wbflush();
            iopb->iopb_LEVEL            = c->c_level; wbflush();
            W(iopb->iopb_ADDR) = (Blockmode_ok) ? ADDR_MOD_B : ADDR_MOD;
	    W(iopb->iopb_UNIT)          = un->un_unit.U.w;
            un->un_command = cdb->cmd   = scsi_cmd[ cmd ];
            cdb->lun                    = 0;
            if (bp->b_flags & B_SPL) {
		    /* 
		     * back down from BLOCK mode for any "specials".
		     * mandatory for MIPS hardware constraints, ie
		     * we must be long-word aligned and count must be
		     * modulo 4 
		     */
            	    W(iopb->iopb_ADDR) = ADDR_MOD; /* non-BLOCK MODE */
		    iopb->iopb_LENGTH = 0; /* SG can leave 'odd' link count! */
		    bp->b_resid = 0; /* for dkvjintr() */
		    /* Make sure unit structure is mapped for I/O */
		    if ((!un->un_sph) &&
	    	   	(!vme_iomap(c->c_csh, un, sizeof(VJ_UNIT),
			    GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			    &un->un_sph, &io_addr)))
	    	        cmn_err(CE_PANIC, "Can't map unit structure!\n");

		    dkvj_wb_cache((VJ_UNIT *)un, sizeof(VJ_UNIT));
		    switch (cdb->cmd)
		    {
		    case C0_RDBLOCK:
			cdb->count = 0;
			if(!ka_to_vmeaddr(un->un_sph,&un->un_blklim, &io_addr))
                        cmn_err(CE_PANIC, "Can't get addr for un structure!\n");
			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = 6;
			break;
		    case C0_MSENSE:
			/*
			 * The address is within the unit structure, but
			 * it's passed in the bp, since it's dependent
			 * upon which pages was specified.
			 */
			if(!ka_to_vmeaddr(un->un_sph,bp->b_un.b_addr, &io_addr))
                        cmn_err(CE_PANIC, "Can't get addr for un structure!\n");
			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = bp->b_bcount;
			cdb->count = bp->b_bcount;
			cdb->mid_addr = bp->b_blkno & 0x3f;
			break;
		    case C0_MSELECT:
			if(!ka_to_vmeaddr(un->un_sph,bp->b_un.b_addr, &io_addr))
                        cmn_err(CE_PANIC, "Can't get addr for un structure!\n");
			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = bp->b_bcount;
			cdb->count = bp->b_bcount;
			break;
		    case C0_REASSIGN:
			if(!ka_to_vmeaddr(un->un_sph,un->un_reassign, &io_addr))
                        cmn_err(CE_PANIC, "Can't get addr for un structure!\n");
			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = bp->b_bcount;
			break;
		    case C0_WRFM:
			CDB_XFER_LEN(cdb, bp->b_bcount);
			break;
		    case C0_SPACE:
			cdb->high_addr = bp->b_blkno;	/* code field */
			CDB_XFER_LEN(cdb, bp->b_bcount);
			break;
		    case C0_LOAD:
			cdb->count = bp->b_blkno;
			if (un->un_flags & IVJ_DISK) { /* start/stop command */
				cdb->high_addr = 1 ; /* set Immediate bit */
				cdb->count = 1;     /* set start bit */
			}
			break;
		    case C0_REWIND:
		    case C0_TESTRDY:
			break;
		    case C1_READDEF:
			/*
			 * Defects are allocated on the fly, so they aren't
			 * part of the unit structure itself.  However,
			 * there are 'spl' tags in the unit structure
			 */
			if(!ka_to_vmeaddr(un->un_splph,un->un_spltmp, &io_addr))
                            cmn_err(CE_PANIC, "Can't get addr for readdef structure!\n");
			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = bp->b_bcount;
			ecdb = (struct scsi_ecdb *)cdb;
			ecdb->lba_b0 = (bp->b_blkno << 3) | 0x5;  /* code */
			CDB_EXFER_LEN( ecdb, bp->b_bcount)
			break;
		    }
	    } else {
		    length = vjSGsetup(c, bp, iopb, un->un_flags&IVJ_VARIABLE);
		    /* XXX for now this has to be a panic */
		    if (!length) {
			cmn_err(CE_PANIC, "vjSGsetup failed!\n");
		    }
		    if(un->un_flags & IVJ_TAPE) {
			if (un->un_flags & IVJ_VARIABLE) {
			    cdb->high_addr = 0;
			} else
			    cdb->high_addr = 1;
			CDB_XFER_LEN(cdb, length);
		    } else {
			if (length < 0x100 && bn < 0x200000) {
			    CDB_ADDR(cdb, bn);
			    cdb->count = (length & 0xff);
			} else {
			    cdb->cmd |= 0x20;
			    CDB_EADDR(cdb, bn);
			    CDB_EXFER_LEN(cdb, length);
			}
		    }
	    }
            if (cqe >= c->c_cqe_end) {
                c->c_cqe_QHDP = (VJ_CQE*)((INT)c->c_cqe_top - (INT)shio);
    	    	if (c->c_mode & C_OFF_BOARD)
                      c->c_off_QHDP = c->c_off_top;
	    } else {
		c->c_cqe_QHDP = (VJ_CQE*)(((INT)cqe+sizeof(VJ_CQE)) -(INT)shio);
    	    	if (c->c_mode & C_OFF_BOARD)
		      c->c_off_QHDP =(OFFBD_IOPB*)((INT)off+sizeof(OFFBD_IOPB));
	    }
    	    if (c->c_mode & C_OFF_BOARD) {
		    off->copycqe.cqe_CTAG        = (ULONG)bp;
		    off->copycqe.cqe_IOPB_LENGTH = 22; /* shorts (12 byte cdb)*/
		    off->copycqe.cqe_WORK_QUEUE  = un->un_workq;
		    writeback_virtual_data(off, sizeof(OFFBD_IOPB));
	    } else {
		    vjfrom_shio(cqe, &Cqe, sizeof(VJ_CQE));
		    Cqe.cqe_CTAG        = (ULONG)bp;
		    Cqe.cqe_IOPB_LENGTH = sizeof(VJ_IOPB)/4;
		    Cqe.cqe_WORK_QUEUE  = un->un_workq;
		    vjto_shio(&Cqe, cqe, sizeof(VJ_CQE));
		    vjto_shio(iopb,
			(VJ_IOPB *)((INT)Cqe.cqe_IOPB_ADDR +
			(int)shio),sizeof(VJ_IOPB));
	    }
	    s = splclock();
	    un->un_qcount++;
	    if (un->un_timeid == 0) {
		if (un->un_qcount == 1 && un->un_timeout) {
			un->un_timeid =
			  timeout_spl(vjtimeout,un,un->un_timeout, splbio);
		} else {
		        if(un->un_flags & IVJ_TAPE)
				delay = TIME_RDWR_TAPE;
			else	
				delay = TIME_RDWR_DISK;
			un->un_timeid = timeout_spl(vjtimeout,un,delay, splbio);
		}
	    }
	    /* flush gba cache before sending cmd */
	    vme_ioflush(c->c_sph,0,0); 
	    splx(s);
    	    W_QECR(cqe->cqe_QECR) = qecr | M_QECR_GO; /* save a VME access */
    }
    return(0);
}

/*
 * Queue Entry Available interrupt.
 */
vjqint(c)
register VJ_CTLR *c;
{
    register VJ_CRB *crb;
    register count;
    
    crb = &c->c_io->sh_CRB;
#ifdef DKVJ_DEBUG
    cmn_err(CE_WARN, "vjqint: crsw: 0x%x\n", crb->crb_CRSW);
#endif DKVJ_DEBUG

    if (W(crb->crb_CRSW) & M_CRSW_CQA) {
        CRB_CLR_DONE(crb->crb_CRSW);
        if (c->c_wait_cqe) {
            c->c_wait_cqe = FALSE;
    	    if (!(c->c_mode & C_MACSI_SORT))
	    	count = dkvjcstart(c);
	    else
	    	count = vjgo(c);
#ifdef DKVJ_DEBUG
	    if (count > 1)
		printf ("vjqint %d commands\n",count);
#endif DKVJ_DEBUG
	    c->c_istart += count; /* accumulate commands out of int */
        }
    } else
        CRB_CLR_DONE(crb->crb_CRSW);
}

dkvjintr(ctlr_no)
int ctlr_no;     
{
    register VJ_CTLR *c;
    register VJ_SHIO    *shio;
    register VJ_UNIT    *un;
    register VJ_CRB     *crb;
    register VJ_IOPB    *iopb;
    register IPSG 	*sg;
    register IPSG_FREE  *ipsg_free;
    register int i;
    register VJ_DISK_HD *dp;
    register VJ_ALIGN   *al;
    register struct buf *nbp;
    uint unit, temp, temp1;
    BUF     *bp, *tp;
    VJ_CRB  Crb;
    VJ_IOPB Iopb;
    SCSI_CDB *cdb;
    register USHORT crsw;
    u_int delay, count = 0;
    struct device_parameters *devp;
    struct partition_table *pt;
    uint bn, is_a_tape, read_size;
    BUF_EXT *buf_ext, *buf_ext1;
    int Post_err = 0;
    int offboard_status;

    if (first_init == 0){
	if( showconfig )
	    cmn_err( CE_CONT, "ijc%d: spurious interrupt\n");
	return;
    }

    c = vjctlrs[ctlr_no];
#ifdef DKVJ_DEBUG    
    if (dkvj_dump) {
      vj_c_printf(c);
      dkvj_dump = 0;
    }
#endif DKVJ_DEBUG    
    if (!c->c_present) {
	log(BSD43_LOG_ERR,"DKVJ %d: Interrupt from non-existent board\n",
	    ctlr_no);
	return;
    }
    crsw = W(c->c_io->sh_CRB.crb_CRSW);

    if (crsw & M_CRSW_CQA) {
      vjqint(c);
      return;
    }

    if ((crsw & M_CRSW_CRBV) == 0) {
      /* If c_dkvjintr_call is set, then dkvjintr was called from polling
       * loop.  This leaves controller with CRBV == 0 but interrupt request
       * is still pending since no VME bus IACK was performed.  Don't log an
       * error if this occurs.
       */
      if (c->c_dkvjintr_call)
        c->c_dkvjintr_call = 0;
      else
        log(BSD43_LOG_ERR,
            "DKVJ %d: dkvjintr: no interrupt pending, CRSW(%x)\n",
            c->c_ctlr,crsw);
      return;
    }
    c->c_dkvjintr_call = 0;
    
    dp = &c->c_disk_hd;
    shio = c->c_io;

    /* 
       first check the onboard status. If error, read iopb status.If status
       says there was an error while posting Crb to memory then ignore offboard
       iopb and crb 
    */

    if(crsw & M_CRSW_ER){
       offboard_status = Iopb.iopb_STATUS = (shio->sh_RET_IOPB.iopb_STATUS);
       if((Iopb.iopb_STATUS & 0xff) == CRB_POST_ERROR)
	   Post_err = 1;
    }

    /* Flush the cntlr structure (including CRB) from the I/O cache.
     */
    if(!vme_ioflush(c->c_sph, 0, 0))
       cmn_err(CE_PANIC, "Could not flush the CTLR struct \n");
    
    crb     = &Crb;
    iopb    = &Iopb;
    if ((c->c_mode & C_OFF_BOARD) && !Post_err )
    {
	    /* We need to copy the crb & iopb fields because subsequent
	     * code accesses some of these fields AFTER the crsw done bit
	     * has been cleared, and could obtain information from the
	     * NEXT request being completed.
	     */
	    invalidate_virtual_data( &c->c_off_crb,
				    sizeof(VJ_CRB) + sizeof(VJ_IOPB) );
#ifdef DKVJ_DEBUG
	    /* Verify that on-board & off-board CRB & IOPB agree */
	    vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
    	    vjfrom_shio(&shio->sh_RET_IOPB, iopb, sizeof(VJ_IOPB));
	    if ((bcmp(&c->c_off_crb, crb, sizeof(VJ_CRB))) ||
		(bcmp(&c->c_off_iopb, iopb, sizeof(VJ_IOPB)))) {
	      cmn_err(CE_WARN,
		      "On board CRB/IOPB  and off-board CRB/IOPB disagree!");
	    }
#endif DKVJ_DEBUG	    
	    bcopy( &c->c_off_crb, crb, sizeof(VJ_CRB) );
	    bcopy( &c->c_off_iopb, iopb, sizeof(VJ_IOPB) );
    } else {
	    clean_cache(shio,sizeof(VJ_SHIO));
	    vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
    	    vjfrom_shio(&shio->sh_RET_IOPB, iopb, sizeof(VJ_IOPB));
    }

    if (crsw != W(crb->crb_CRSW)) {
      log(BSD43_LOG_ERR,
	  "DKVJ %d: on-board CRSW(0x%x) inconsistent with off-board (%x)\n",
	  c->c_ctlr, crsw, W(crb->crb_CRSW));
      log(BSD43_LOG_ERR,
          "DKVJ %d: on-board status(0x%x) off-board status(%x)\n",
          c->c_ctlr, iopb->iopb_STATUS, offboard_status);
      crsw = W(crb->crb_CRSW);
    }


    if (!(crsw & M_CRSW_CC)) { /* do we have command complete */
#ifdef DKVJ_DEBUG
        cmn_err(CE_NOTE, "dkvjintr: no CC in crsw (0x%x)\n", crsw);
#endif DKVJ_DEBUG      
        /* XXX ask MARK V. if off-board stuff is fully valid for this case! */
	if (crsw == (M_CRSW_SC|M_CRSW_ER|M_CRSW_CRBV)) { /* controller error */
		temp = iopb->iopb_CMD & 0xff; /* error field for this case */
		log(BSD43_LOG_ERR,
		"DKVJ %d: Controller error, CRSW(%x) error(%x)\n",
				 c->c_ctlr,crsw,temp);
		vjerror(temp);
		/*
		 * reset scsi bus for errors 83, 84, and 85 (done below)
		 * filter out and just return for the others
		 * because the crb and iopb data may be invalid!!! 
		 * The timer wasn't cleared so we can retry on a timeout.
		 * 
		 * These three cases require a SCSI reset which is done below
		 * along with normal error handling (but NO retry) since
		 * M_CRSW_ER is set.
		 */
		switch(temp) {
			case 0x83:
			case 0x84:
			case 0x85:
				break;
			default:
				return;
		}
	} else {
		log(BSD43_LOG_ERR,
		"DKVJ %d: Command not Complete, spurious interrupt? CRSW(%x)\n",
				c->c_ctlr,crsw);
		return;
	}
    }
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];

    if (iopb->iopb_CMD == SCSI_RESET) {
#ifdef DKVJ_DEBUG
      	cmn_err(CE_NOTE, "dkvjintr: SCSI_RESET complete\n");
#endif DKVJ_DEBUG	
	un = (VJ_UNIT *)crb->crb_CTAG;
	dkvj_un_iodone(un);
	un->un_qcount--;
	if (un->un_timeid) {
	    untimeout(un->un_timeid);
	    un->un_timeid = 0;
	} else
	    log(BSD43_LOG_ERR,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
	if (crsw & M_CRSW_EX) {
	    log(BSD43_LOG_ERR,
		"DKVJ %d:%d: Cmd = %x Exception = %x Work Queue = %d\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD, iopb->iopb_STATUS, crb->crb_WORK_QUEUE);
	    log(BSD43_LOG_ERR,
		"RESET crsw= %x options= %x ttlength= %x length= %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);
	}
	CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	goto splcomp;
    }
    /* 
     * note that RPT (report) has been set on the FLUSH so we'll handle
     * retries as required with each flushed command interrupting with error
     */
    if (iopb->iopb_CMD == CNTR_FLUSH_WORKQ) {
#ifdef DKVJ_DEBUG
	cmn_err(CE_NOTE, "dkvjintr: FLUSH_WORKQ complete\n");
#endif DKVJ_DEBUG	
	un = (VJ_UNIT *)crb->crb_CTAG;
	dkvj_un_iodone(un);
	un->un_qcount--;
	if (un->un_timeid) {
	    untimeout(un->un_timeid);
	    un->un_timeid = 0;
	} else
	    log(BSD43_LOG_ERR,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
	if (crsw & M_CRSW_EX) {
	    log(BSD43_LOG_ERR,
		"DKVJ %d:%d: Cmd = %x Exception = %x Work Queue = %d\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD, iopb->iopb_STATUS, crb->crb_WORK_QUEUE);
	    log(BSD43_LOG_ERR,
		"FLUSH crsw= %x options= %x ttlength= %x length= %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);
	}
	CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	count = ((VJ_FQCF *)iopb)->fqcf_FST.number;
	log(BSD43_LOG_ERR,"DKVJ %d Work queue %d flushed of %d commands\n",
		un->un_ctlr,un->un_workq,count);
	if (((VJ_FQCF *)iopb)->fqcf_FST.pip) {
	    log(BSD43_LOG_ERR,"Primary bus command in progress\n");
	    unit = (int)shio->sh_CSS.csb_PRI_SLCTD;
	    temp = (int)shio->sh_CSS.csb_PRI_PSNS & 0x08; /* busy bit */
	    if (unit == un->un_slave && temp) {
		log(BSD43_LOG_ERR,"Bus held busy\n");
	    }
	    log(BSD43_LOG_ERR,"Resetting SCSI bus...\n");
	    vjcmd(un,VJ_SCSI_RESET,0,0,0,WANT_INTERRUPT);
	}
	if (((VJ_FQCF *)iopb)->fqcf_FST.sip) {
	    log(BSD43_LOG_ERR,"Secondary bus command in progress\n");
	    unit = (int)shio->sh_CSS.csb_SEC_SLCTD;
	    temp = (int)shio->sh_CSS.csb_SEC_PSNS & 0x08; /* busy bit */
	    if (unit == un->un_slave && temp) {
		log(BSD43_LOG_ERR,"Bus held busy\n");
	    }
	    log(BSD43_LOG_ERR,"Resetting SCSI bus...\n");
	    vjcmd(un,VJ_SCSI_RESET,0,0,0,WANT_INTERRUPT);
	}
splcomp:
    	if (c->c_mode & C_MACSI_SORT)
		vjgo(c);
	else
		if (c->c_tab->b_actf)
			dkvjcstart(c);
	if (un->un_qcount && !un->un_timeid)  /* still more commands pending */
	    if (un->un_qcount == 1 && un->un_timeout) {
		un->un_timeid = timeout_spl(vjtimeout,un,un->un_timeout,splbio);
	    } else {
		if(un->un_flags & IVJ_TAPE)
			delay = TIME_RDWR_TAPE;
		else	
			delay = TIME_RDWR_DISK;
		un->un_timeid = timeout_spl(vjtimeout,un,delay, splbio);
	    }
	return;
    }
    /*
     * If we came in here with a request sense, it was from the vjsense
     * call later in this routine.  The bp is saved in un structure, so we
     * will have to get it from there 
     */
    if(cdb->cmd == C0_REQSENSE){
	un = (VJ_UNIT *)crb->crb_CTAG;
	dkvj_un_iodone(un);
	bp = un->un_savebp;
	if(bp == NULL) {
	    log(BSD43_LOG_ERR,"DKVJ %d:%d Illegal REQUEST SENSE command\n", 
		un->un_ctlr,un->un_slave);
	    /* Make sure controller isn't frozen */
            W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	    wbflush();
	    return;
	}
    } else { /* no REQ SENSE */
        bp      = (BUF *)crb->crb_CTAG;
#ifdef DKVJ_DEBUG
	if (!vj_verify_bp( c, bp, 1)) {
	  cmn_err(CE_WARN,
		  "dkvjintr: No I/O pending for bp value from cntlr (0x%x)", bp);
	  cmn_err(CE_CONT, "    b_flags: 0x%x\n", bp->b_flags);
	  vj_c_printf(c);
	  vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
	  vjfrom_shio(&shio->sh_RET_IOPB, iopb, sizeof(VJ_IOPB));
	  if (bp == (BUF *)crb->crb_CTAG) {
	    cmn_err(CE_CONT, "   On-board CRB gives same BAD value\n");
	    CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	    return;
	  }
	  bp      = (BUF *)crb->crb_CTAG;
	  if (!vj_verify_bp( c, bp, 1)) {
	    cmn_err(CE_CONT,
		    "     On-board bp (0x%x) has NO I/O pending!\n",bp);
	    CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	    return;
	  }
	  cmn_err(CE_CONT, "    On-board bp (0x%x) appears OK\n", bp);
	}
#else
#ifdef R6000
	/* Leave some special verificiation code in-place for 6000 */
	if (!vj_verify_bp( c, bp, 1)) {
	  cmn_err(CE_WARN,
		  "dkvjintr: No I/O pending for bp (0x%x) from cntlr, iopb_status (0x%x, 0x%x)\n",
		  bp, iopb->iopb_STATUS, offboard_status);
	  CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	  return;
	}
#endif R6000	
#endif DKVJ_DEBUG	
        buf_ext = (BUF_EXT*)bp->av_back;
        dkvj_buf_iodone( buf_ext);
	/*
	 * put scatter/gather free entry back into list
	 */
	if((ipsg_free = (IPSG_FREE*)buf_ext->ext_back)){/* SG entry to return?*/
	    /* link back into free list */
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    c->c_tab->qcnt--;
	    buf_ext->ext_back = NULL;
	    dkvj_c_iodone(c, ipsg_free, sizeof(IPSG_FREE));
	    al = &ipsg_free->align;
	    if (Blockmode_ok) {
	        if (al->al_no_block) { /* had to use non-block mode SG? */
    	    		sg = &ipsg_free->ipsg[0]; /* get top SG  entry    */
			/* al_no_block contains link count */
			for(i = 0; i < al->al_no_block; i++) {
			    sg->sg_meminfo = (TT_BLOCK << 2) | MEMTYPE;
			    sg->sg_addrmod = VME_A32NPBMOD;
			    sg++;
			}
	    		al->al_no_block = 0; /* reset our flag */
	        }
	    }
	    if (al->al_faddr) { /* did we have to malloc a buffer? */
		if (bp->b_flags & B_READ){
		    if ((iopb->iopb_STATUS & 0xff) == TRANSFER_COUNT_ERROR)
		      read_size = iopb->iopb_LENGTH;
		    else
		      read_size = al->al_size;
		    ASSERT( read_size <= al->al_size );
		    invalidate_virtual_data( al->al_faddr, read_size);
		    bcopy(al->al_faddr,al->al_uaddr,read_size);
		    dkvj_wb_cache(al->al_uaddr, read_size);
		}
		kmemfree(al->al_taddr, M_DEVBUF, M_NOWAIT);
		al->al_faddr = 0;
	    }
	}
    } /* end no REQ SENSE */
    unit = BPTOVJN(bp);
    un = c->c_unit[unit];
    dkvj_un_iodone(un);
    is_a_tape = (un->un_flags & IVJ_TAPE) ? 1 : 0;
    un->un_qcount--;
    if (un->un_timeid) {
	untimeout(un->un_timeid);
	un->un_timeid = 0;
    } else
	log(BSD43_LOG_ERR,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
    CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);

    if(cdb->cmd == C0_REQSENSE) {
	/* Thaw the controller */
	W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	wbflush();
	un->un_xfer = bp->b_bcount;
	un->un_resid = bp->b_resid;
	temp = vjsense_status(un,iopb->iopb_STATUS);
	bp->b_resid = un->un_resid;
        if (!(bp->b_flags & B_SPL)) {
            nbp = bp->av_forw;
            while (nbp) {
                nbp->b_resid = nbp->b_bcount;
                nbp = nbp->av_forw;
            }
        }
	switch (temp) {
	case 0:
	    goto normcomp1;
	case 1:
	    goto errcomp;
	case 2:
	    /* case of 1/4" partial write at eom. act like a 1/2" drive */
	    if (bp->b_resid != bp->b_bcount) { /* partial write */
		if (!(un->un_flags & IVJ_VARIABLE)) {/*if 1/4" write the rest */
		    un->un_eomcount = un->un_weomcount = 0;
        	    buf_ext = (BUF_EXT*)bp->av_back;
		    buf_ext->ext_resid = bp->b_resid;
		    /* catastrophic recovery que updating */
        	    buf_ext = (BUF_EXT*)bp->av_back;
		    if (dp->b_forw == bp)
			dp->b_forw = buf_ext->ext_forw; /* adv que to next bp */
		    else {
			/* others are queued up, so look forward in the queue */
			for (tp = dp->b_forw; tp; tp = buf_ext1->ext_forw) {
			    buf_ext1 = (BUF_EXT*)tp->av_back;
			    if (buf_ext1->ext_forw == bp) { /* on list */
				/* advance queue to next bp */
				buf_ext1->ext_forw = buf_ext->ext_forw;
				if (dp->b_back == bp) /* end of list? */
					dp->b_back = tp; /* update back pointer
					(since we have more bp's added on) */
				break;
			    }
			}
			if (!tp)
			    log(BSD43_LOG_ERR,
			    "DKVJ %d:%d error in recovery que update\n",
				un->un_ctlr,un->un_slave);
		    }
		    un->un_tab->qcnt++;
		    disksort(un->un_tab, bp, un->un_lastcyl);
		    dkvjustart(un);
		    return;
		}
		goto normcomp1;
	    }
	    bp->b_error = ENOSPC; /* nothing written so we can do this */
	    goto errcomp1;
	default:
	    goto errcomp;
	}
    } 
    if (crsw & M_CRSW_ER) {
        /*
         * See if a Check Condition occurred on last command 
         */
#ifdef DKVJ_DEBUG      
        cmn_err(CE_WARN, "CRSW_ER (crsw: 0x%x) iopb_STATUS 0x%x",
		crsw, iopb->iopb_STATUS);
#endif DKVJ_DEBUG
        if(((iopb->iopb_STATUS >> 8) & 0xff) == CHECK_CONDITION) {
	    if (vjexterr) {
		log(BSD43_LOG_ERR,"\nDKVJ %d:%d check condition\n",
			un->un_ctlr,un->un_slave);
		log(BSD43_LOG_ERR,"     SCSI cdb =  ");
		for (i=0;i<6;++i)
		    log(BSD43_LOG_ERR,"0x%x ",iopb->iopb_SCSI[i]);
		log(BSD43_LOG_ERR,"\n");
	    }
	    un->un_savebp = bp;
	    (void)vjsense(un, WANT_INTERRUPT);
	   /* 
	    * Since it's interrupt driven, get out of here.  We
	    * will do all of the processing when we come in from the
	    * interrupt returned after the sense command
	    */
	   return;		
	}
	/* Thaw the controller */
	W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	wbflush();
	if (((iopb->iopb_STATUS >> 8) & 0x0ff) == SCSI_BUSY) {
#ifdef DKVJ_DEBUG
		cmn_err(CE_NOTE, "dkvjintr: SCSI_BUSY\n");
#endif DKVJ_DEBUG		
		un->un_scsibusy++;
		if(!is_a_tape)
		  goto busy; /* resend the cmd */
	}
errcomp:
        if( cdb->cmd != C0_REQSENSE)
	    un->un_hardcount++;
        if (!is_a_tape&&(bp->b_error++ < un->un_retries)&&!(crsw & M_CRSW_SC)) {
	    log(BSD43_LOG_ERR,"DKVJ %d:%d retrying(%d)...\n",
			un->un_ctlr,un->un_slave,bp->b_error);
	    un->un_softcount++;
busy:
	    temp = bp->b_error;
	    devp = &un->un_vh.vh_dp;
	    temp1 = devp->dp_secs * devp->dp_trks0;
	    while (bp) { /* need to handle combined reads/writes! */
		    /* catastrophic recovery que updating */
        	    buf_ext = (BUF_EXT*)bp->av_back;
		    dkvj_buf_iodone(buf_ext);
		    if (dp->b_forw == bp)
			dp->b_forw = buf_ext->ext_forw; /* adv que to next bp */
		    else {
			/* others are queued up, so look forward in the queue */
			for (tp = dp->b_forw; tp; tp = buf_ext1->ext_forw) {
			    buf_ext1 = (BUF_EXT*)tp->av_back;
			    if (buf_ext1->ext_forw == bp) { /* on list */
				/* advance queue to next bp */
				buf_ext1->ext_forw = buf_ext->ext_forw;
				if (dp->b_back == bp) /* end of list? */
					/* update back pointer, since  */
					/* we have more bp's added on  */
					dp->b_back = tp;
				break;
			    }
			}
			if (!tp)
			    log(BSD43_LOG_ERR,
			    "DKVJ %d:%d error in 'retry' recovery que update\n",
				un->un_ctlr,un->un_slave);
		    }
		    if (c->c_mode & C_MACSI_SORT) {
			    nbp = (BUF *)0; /* just to make sure */
			    buf_ext->ext_forw1  = (BUF *)0;
			    if (dp->av_forw == (BUF *)0) /* requeue bp */
				dp->av_forw = bp;
			    else {
				buf_ext1 = (BUF_EXT*)dp->av_back->av_back;
				buf_ext1->ext_forw1 = bp;
			    }
		    } else {
		    	    nbp = bp->av_forw; /* disksort clobbers! */
	    		    pt = &un->un_vh.vh_pt[LPART(bp->b_dev)];
    			    bn = (uint)(bp->b_blkno + pt->pt_firstlbn);
			    bp->b_cylin = bn / temp1;
			    un->un_tab->qcnt++;
			    disksort(un->un_tab, bp, un->un_lastcyl);
			    if (un->un_tab->b_active == 0) /*not already there*/
				dkvjustart(un);
		    }
		    bp = nbp; /* combined reads/writes? */
		    if (bp)
			    bp->b_error = temp; /* separate combined rds/wrts */
	    }
	    if (c->c_mode & C_MACSI_SORT) {
		    if (un->un_scsibusy) /* handle unit busy case */
			un->un_timeid = timeout_spl(vjgo,c,HZ,splbio); /* before reissue */
		    else vjgo(c);
	    } else {
		    if (un->un_scsibusy) /* handle unit busy case */
			un->un_timeid = timeout_spl(dkvjcstart,c,HZ,splbio);
		    else dkvjcstart(c);
	    }
	    return;
        } else {
            bp->b_error  = EIO;
	    un->un_flags &= ~IVJ_WRITTEN;
errcomp1:
            bp->b_flags |= B_ERROR;
	    bp->b_resid = bp->b_bcount;
        }
    }
    if (crsw & M_CRSW_EX) {
	if((iopb->iopb_STATUS & 0xff) == TRANSFER_COUNT_ERROR) { /* 0x34 */
	        bp->b_resid = bp->b_bcount - iopb->iopb_LENGTH;
	        goto normcomp;
	}
	log(BSD43_LOG_ERR,
		"DKVJ %d:%d Cmd = %x (%x) Queue = %d SCSI error = %x bp = %x\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD,cdb->cmd,crb->crb_WORK_QUEUE,
		(iopb->iopb_STATUS >>8),bp);
	vjerror(iopb->iopb_STATUS & 0xff);
	log(BSD43_LOG_ERR,
		"crsw = %x options= %x tt length= %x length = %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);
    }
normcomp:
    if ((un->un_flags & IVJ_TAPE) && !(bp->b_flags & B_SPL))
	un->un_flags &= ~IVJ_FM;
normcomp1:
    if (un->un_scsibusy) {
	if(!is_a_tape)
	log(BSD43_LOG_ERR,"DKVJ %d:%d, %d device busy retry(s), scsi cmd %x\n",
		un->un_ctlr,un->un_slave,un->un_scsibusy,cdb->cmd);
	un->un_scsibusy = 0;
    }
    do {

	/*
	 * check for more data to transfer
	 */
    	if (bp->b_resid && !(bp->b_flags & B_SPL) && !is_a_tape &&
						(bp->b_error != EIO)) {
		nbp = bp->av_forw;
		un->un_tab->qcnt++;
        	buf_ext = (BUF_EXT*)bp->av_back;
	        dkvj_buf_iodone( buf_ext); /* flush & unmap GBA  */
		buf_ext->ext_resid = bp->b_resid; /* b_resid gets clobbered!! */
    	    	if (!(c->c_mode & C_MACSI_SORT)) {
    			bn = (uint)(bp->b_blkno + 
				((bp->b_bcount - bp->b_resid) >> SCTRSHFT));
			pt = &un->un_vh.vh_pt[LPART(bp->b_dev)];
			devp = &un->un_vh.vh_dp;
			temp = devp->dp_secs * devp->dp_trks0;
			pblkno  = (BUF*)(bn + pt->pt_firstlbn);
			bp->b_cylin = (uint)pblkno / temp;
		}
	        /* catastrophic recovery que updating */
	        if (dp->b_forw == bp)
	    		dp->b_forw = buf_ext->ext_forw;/* adv queue to next bp*/
	        else {
	    	/* others have become queued up, so look forward in the queue */
	        	for (tp = dp->b_forw; tp; tp = buf_ext1->ext_forw) {
                    		buf_ext1 = (BUF_EXT*)tp->av_back;
		    		if (buf_ext1->ext_forw == bp){/* found ourself*/
					/* advance queue to next bp */
		    			buf_ext1->ext_forw = buf_ext->ext_forw;
		    			if (dp->b_back == bp) /* end of list? */
						dp->b_back = tp;
		    			break;
		    		}
	    		}
			if (!tp)
			    	log(BSD43_LOG_ERR,
				"DKVJ %d:%d error in recovery que update\n",
				un->un_ctlr,un->un_slave);
		}
    	    	if (!(c->c_mode & C_MACSI_SORT)) {
			disksort(un->un_tab, bp, un->un_lastcyl);
			if (un->un_tab->b_active == 0) /* if not already there*/
				dkvjustart(un);
		}
                bp = nbp; /* restart all of them */

	} else {
		/*
		 * If there were a number of writes combined together
		 * in one iopb, walk through the list of bp's and call
		 * IODONE for each. bp->av_forw is the indication.
		 *
	         * update accounting
	         */
		un->un_iotime->io_resp += lbolt - bp->b_start;
        	buf_ext = (BUF_EXT*)bp->av_back;
		dkvj_buf_iodone(buf_ext);
	        /* catastrophic recovery que updating */
	        if (dp->b_forw == bp)
	    		dp->b_forw = buf_ext->ext_forw;/* adv queue to next bp*/
	        else {
	    	/* others have become queued up, so look forward in the queue */
	        	for (tp = dp->b_forw; tp; tp = buf_ext1->ext_forw) {
                    		buf_ext1 = (BUF_EXT*)tp->av_back;
		    		if (buf_ext1->ext_forw == bp){/* found ourself*/
					/* advance queue to next bp */
		    			buf_ext1->ext_forw = buf_ext->ext_forw;
		    			if (dp->b_back == bp) /* end of list? */
						dp->b_back = tp;
		    			break;
		    		}
	    		}
			if (!tp)
			    	log(BSD43_LOG_ERR,
				"DKVJ %d:%d error in recovery que update\n",
				un->un_ctlr,un->un_slave);
		}
		/* Commands with B_SPL are NOT chained.   Furthermore, the
		 * av_forw flags may be non-zero if other requests were
		 * queued.  So explicitly set nbp to zero for B_SPL.
		 */
		nbp = (bp->b_flags & B_SPL) ? 0 : bp->av_forw;
		/* 
		 * NOTE that this should never happen since the combines
		 * should have been split and retried individually before
		 * we ever get here 
		 */
    		if (nbp && (bp->b_flags & B_ERROR)) { /* set ERROR for all! */
			nbp->b_flags |= B_ERROR;      /* iodone clobbers!   */
			nbp->b_error  = EIO;
		}
	        /* link back into free list */
	    	if (!(buf_ext->nxt = c->buf_ext_hd)) {
	    		c->buf_ext_hd = buf_ext;
			wakeup((caddr_t)c);
		} else
	    		c->buf_ext_hd = buf_ext;
	        if (!(bp->b_flags & B_SPL))
	    		iounmap(bp);
		else bp->b_flags &= ~(B_BUSY | B_SPL);
	        iodone(bp);             /* wake up process just done    */
	        if(bp->b_flags & B_WANTED) {
			bp->b_flags &= ~B_WANTED;
			wakeup((caddr_t)bp);
	        }
		bp = nbp;
	}
    } while(bp);

    un->un_iotime->io_act  += lbolt - dp->b_start;
    if (!(crsw & M_CRSW_SC)) { /* scsi bus probably hung if SC set */
    	if (c->c_mode & C_MACSI_SORT)
    		c->c_istart = vjgo(c);
	else
		if (c->c_tab->b_actf)
			c->c_istart = dkvjcstart(c);
    }
    if (un->un_qcount && !un->un_timeid)  /* still more commands pending */
	if (un->un_qcount == 1 && un->un_timeout) {
		un->un_timeid = timeout_spl(vjtimeout,un,un->un_timeout,splbio);
	} else {
		if(un->un_flags & IVJ_TAPE)
			delay = TIME_RDWR_TAPE;
		else	
			delay = TIME_RDWR_DISK;
		un->un_timeid = timeout_spl(vjtimeout,un,delay,splbio);
	}
    un->un_flags &= ~(IVJ_BUSY | IVJ_REWINDING);
    if(un->un_flags & IVJ_WAITING) {
	un->un_flags &= ~IVJ_WAITING;
	wakeup((caddr_t)un);
    }
    if (crsw & M_CRSW_SC) { /* we need to reset */
	    log(BSD43_LOG_ERR,"Resetting SCSI bus after SC error\n");
	    vjcmd(un,VJ_SCSI_RESET,0,0,0,WANT_INTERRUPT);
    }
}

dkvjread(dev)
dev_t dev;
{
    register int unit, ctlr;
    register VJ_CTLR *c;
    register VJ_UNIT *un;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c =  vjctlrs[ctlr]) == 0 || 
	(un = c->c_unit[unit]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    if (!(un->un_flags & IVJ_VARIABLE) && (u.u_offset & (NBPSCTR -1))) {
	u.u_error = EIO;
	return;
    }
    if (un->un_flags & IVJ_TAPE) {
	if (!(un->un_flags & IVJ_VARIABLE) && un->un_flags & IVJ_WRITTEN) {
		/* can't read a 'written' tape */
		u.u_error = EINVAL;
		return;
	}
    } else if (!physck(un->un_vh.vh_pt[LPART(dev)].pt_nblks, B_READ))
	return;
    physio(dkvjstrategy,0,dev,B_READ);
}

dkvjwrite(dev)
dev_t dev;
{
    register int unit, ctlr;
    register VJ_CTLR *c;
    register VJ_UNIT *un;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c =  vjctlrs[ctlr]) == 0 || 
	(un = c->c_unit[unit]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    if (!(un->un_flags & IVJ_VARIABLE) && (u.u_offset & (NBPSCTR -1))) {
	u.u_error = EIO;
	return;
    }
    if (un->un_flags & IVJ_TAPE) {
	if (!(un->un_flags & IVJ_VARIABLE) && un->un_flags & IVJ_READ) {
		/* can't write a 'read' tape */
		u.u_error = EINVAL;
		return;
	}
    } else if (!physck(un->un_vh.vh_pt[LPART(dev)].pt_nblks, B_WRITE))
	return;
    physio(dkvjstrategy,0,dev,B_WRITE);
}
#endif STANDALONE

/*
 * ioctl routine
 *	See the vjsplxxxxx routines for support in ioctl handling
 */

#ifdef STANDALONE
_dkvjioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	extern int vjwaitf();
	register VJ_CTLR *c;
	register VJ_UNIT *un;
	register struct fmt_map_info *fmi = (struct fmt_map_info *)arg;
	int skip;
	int vj_cmd, vj_addr, vj_blks, vj_count;
	char *msg;
	register error = 0;
	
	c  = vjctlrs[io->i_ctlr];
	un   = c->c_unit[io->i_unit];

	skip = 0;
	switch (cmd)
	{
	    case DIOCGETVH:
		bcopy(&un->un_vh, arg, sizeof(DVH));
		skip = 1;
		break;
	    case DIOCSETVH:
		bcopy(arg, &un->un_vh, sizeof(DVH));
		skip = 1;
		break;
	    case DIOCFMTMAP:
		switch( fmi->fmi_action )
		{
		    case FMI_FORMAT_TRACK:
			vj_cmd		= VJ_FORMAT;
			vj_addr		= 0;
			vj_blks		= 0;
			vj_count	= 0;
			msg			= "Format Error"; 
			break;
		    case FMI_MAP_TRACK:		/* reassign blocks */
			vj_cmd		= VJ_REASSIGN;
			vj_addr		= K1_TO_PHYS( fmi->fmi_addr );
			vj_blks		= 0;
			vj_count	= fmi->fmi_cyl;
			msg = "Reassign blocks failed";
			break;
		    default:
			io->i_errno = EINVAL;
			error = -1;
		}
		break;
	    case DIOCNOECC:
		    vj_cmd		= VJ_MODE_SELECT_CMD;
		    vj_addr		= K1_TO_PHYS( &mode_sense1 );
		    vj_blks		= 0;
		    vj_count	= sizeof( SCSI_MS_ERROR );
		    bzero( &mode_sense1, sizeof( SCSI_MS_ERROR));
		    mode_sense1.hdr.blk_desc_len = 8;
		    mode_sense1.pg1.page_code  = 1;
		    mode_sense1.pg1.page_length = 6;
		    mode_sense1.pg1.retry_count = 8;
		    mode_sense1.pg1.correction_span = 8;
		    mode_sense1.pg1.per = 1;
		    if ( *(int *)arg )
			    mode_sense1.pg1.dcr = 1;
		    msg = "Mode select failed";
		    break;
	    default:
		    io->i_errno = EINVAL;
		    error = -1;
		    break;
	}

	if (!io->i_errno && !skip)
	{
	    vjcmd(un, vj_cmd, (char *)vj_addr, vj_blks, vj_count, NO_INTERRUPT);
	    if (((vj_cmd == VJ_FORMAT) ? vjwaitf : vjwait)(c, M_CRSW_CC,vj_cmd))
	    {
		if (vjsense(un, NO_INTERRUPT))
		{
		    printf("dkij(%d,%d,%d): bad %s\n", io->i_ctlr,io->i_unit,
			    io->i_part,"Request sense");
		    return(-1);
		}
		else
		{
		    printf("dkij(%d,%d,%d): bad %s\n", io->i_ctlr,io->i_unit,
			    io->i_part,msg);
		    return(-1);
		}
	    }
	}
   	return(error);
}
#else STANDALONE
dkvjioctl(dev, cmd, arg, flag)
dev_t dev;
unsigned int cmd;
caddr_t arg;
int flag;
{
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    register VJ_SHIO *shio;
    volatile VJ_MSR *msr;
    register BUF *bp;
    struct mtop *mtop;
    struct mtget *mtget;
    register char *cp;
    struct device_parameters *devp;
    SCSI_MS_ERROR *err;
    SCSI_MS_GEOM *geom;
    SCSI_MS_FORMAT *fmt;
    SCSI_MS_RDAHD *rhp;
    SCSI_MS_CACHE *chp;
    struct gioctl gioctl;
    struct io_arg io_arg;
    struct volume_header vh;
    struct volume_header *ipv;
    struct ctlr_info ct;
    struct fmt_map_info fmt_info;
    GEOMETRY_INFO geom_info;
    int ctlr, unit,i;
    int count, code = 0;
    int status;
    unsigned int addr;
    ioaddr_t io_addr;
    DEFECT_HEADER *dh;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0 ||
        (un = c->c_unit[unit]) == 0)

	ERR_RETURN(ENXIO)
    shio = c->c_io;
    msr = &shio->sh_MCSB.mcsb_MSR;
    status = 0;
    ipv = &un->un_vh;
    devp = &ipv->vh_dp;

    if(cmd == GIOCPRSTR){
	/*
	 * Need to return scsi inquiry string
	 */
	scsistr(un, gioctl.gi_ident);
	if(copyout((caddr_t)gioctl.gi_ident, (caddr_t) arg, IDENT_LEN))
	    u.u_error = EFAULT;
	return;
    }else if (cmd == DIOCGETCTLR ){
        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
            ERR_RETURN(EIO)
        else if (io_arg.datasz != sizeof(struct ctlr_info)) {
            io_arg.retval = DIOC_BADSIZE;
            ERR_RETURN(EIO)
        } else {
            bzero(&ct,sizeof(struct ctlr_info));
            ct.ci_flags = DP_SCSI;
            dkvjgetctlr( c, &ct );
            if (copyout((caddr_t) &ct,
                    (caddr_t) io_arg.memaddr, (int) io_arg.datasz) < 0)
                ERR_RETURN(EFAULT)
    }
	return;
    }

    if (un->un_flags & IVJ_DISK) {
       switch (cmd) {
	    case DIOCTEST:
	    case DIOCDIAG:
		/*
		 * for now just look at Board-OK
		 *
		 */
		if (!(WORDP(msr) & M_MSR_BOK))
		    ERR_RETURN(EIO)
		break;
	    case DIOCGETVH:
		/* get volume header */

		if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EIO)
		if((int)io_arg.datasz != sizeof(struct volume_header))
		    status = DIOC_BADSIZE;
		else if (!un->un_vhvalid)
		    status = DIOC_NOTVOLHDR;
		else if (copyout((caddr_t) &un->un_vh,
			(caddr_t) io_arg.memaddr, (int) io_arg.datasz) < 0)
		    ERR_RETURN(EFAULT)
		break;

	    case DIOCSETVH:
	    case DIOCINITVH:
	    case DIOCRECONFIG:
		/* set volume header */
		if(!suser() )
		    ERR_RETURN(EPERM)
		if (copyin(arg, (caddr_t)&io_arg, sizeof(io_arg)))
		    ERR_RETURN(EFAULT)
		if((int) io_arg.datasz != sizeof(vh))  
		    status = DIOC_BADSIZE;
/*
 * Breaks vdisk.  Need a more context specific check
		else if( dkvj_dev_busy(dev))
		    status = DIOC_DISKBUSY;
 */
		else if(copyin((caddr_t) io_arg.memaddr,
		         (caddr_t) &vh, (int) io_arg.datasz) < 0) {
		    status = DIOC_EFAULT;
		} else if (is_vh(&vh) == FALSE){
		    status = DIOC_NOTVOLHDR;
		}else {
		    bcopy((caddr_t)&vh, (caddr_t)&un->un_vh, sizeof(DVH));
		    un->un_vhvalid = 1;
                    if (cmd == DIOCRECONFIG) {
                        for (i = 0; i < (devp->dp_trks0 * devp->dp_secs);
                                        i+= devp->dp_secs) {
                            if (vjsplcmd(un,VJ_WRITE,i,512,ipv, WAIT)) {
                                status = DIOC_OPERR;
                                cmn_err(CE_CONT,
                   "DKVJ %dL%d: Could not write Volume Header on track %d\n",
                                    un->un_ctlr,un->un_slave,i);
                            }
                        }
                    }

		}
		break;

	    case DIOCFMTMAP:
                /*
                 * perform format operation.
                 * must be superuser and partition cannot be currently mounted.
                 */
                if (suser() == 0)
                    ERR_RETURN(EPERM)
                if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
                    ERR_RETURN(EFAULT)
                if (io_arg.datasz != sizeof(fmt_info)) {
                    status = DIOC_BADSIZE;
                } else if (dkvj_dev_busy(dev)) {
                    status = DIOC_DISKBUSY;
                } else if (copyin((caddr_t)io_arg.memaddr, (caddr_t) &fmt_info,
                        sizeof(fmt_info)) < 0) {
                    status = DIOC_EFAULT;
                } else {
                    status = dkvj_format(un, &fmt_info);
                }
                break;
	    case DIOCVFYSEC:
                /*
                 * verify sectors are readable
                 */
                if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
                    ERR_RETURN(EFAULT)
                if (!io_arg.datasz)
                    status = DIOC_BADSIZE;
                else if(vjsplcmd(un,VJ_VERIFY,io_arg.sectst,io_arg.datasz,
			0,WAIT))
		    status = DIOC_OPERR;
		break;
	    case DIOCDISKCACHE:
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
	        if (un->un_flags & INT_PC8) {
		    rhp = (SCSI_MS_RDAHD *)K0_TO_K1(&un->un_msrdahd);
		    if (io_arg.sectst) 
		        rhp->rcd = 0;		/* enable cache */
		    else
		        rhp->rcd = 1;		/* disable cache */
		    rhp->msense.hdr.sense_data_len = 0;
		    rhp->rdahd_hdr.ps = 0;
		    if (vjsplcmd(un,VJ_MODE_SELECT_CMD,PD_RDAHD,
				sizeof(SCSI_MS_RDAHD),rhp,WAIT))
		        status = DIOC_OPERR;
	        } else {
		    chp = (SCSI_MS_CACHE *)K0_TO_K1(&un->un_mscache);
		    if (io_arg.sectst) 
		        chp->ce = 1;		/* enable cache */
		    else
		        chp->ce = 0;		/* disable cache */
		    chp->msense.hdr.sense_data_len = 0;
		    chp->cache_hdr.ps = 0;
		    if (vjsplcmd(un,VJ_MODE_SELECT_CMD,PD_CACHE,
				sizeof(SCSI_MS_CACHE),chp,WAIT))
		        status = DIOC_OPERR;
	        }
	        break;

	    case DIOCDISKGEOM:
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
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
              
	    case DIOCSEEK:
	        /*
	         * seek to sector
	         */
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
	        if (vjsplcmd(un,VJ_SEEK,io_arg.sectst,0,0,WAIT))
		    status = DIOC_OPERR;
	        break;

	    case DIOCREMOVE:
	        /*
	         * setup disk for removal
	         */
	        if (suser() == 0)
		    ERR_RETURN(EPERM)
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
	        if (dkvj_dev_busy(dev)) {
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
		    ERR_RETURN(EFAULT)
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
		    ERR_RETURN(EFAULT)
	        err = (SCSI_MS_ERROR *)K0_TO_K1(&un->un_mserror);
	        err->per = 1;		/* post errors */
	        if (io_arg.sectst & 2)
		    un->un_retries = 0;
	        else
		    un->un_retries = NRETRIES;
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
	        if (vjsplcmd(un,VJ_MODE_SELECT_CMD,PD_ERROR,i,err,WAIT))
		    status = DIOC_OPERR;
	        break;

	    case DIOCSOFTCNT:
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
	        io_arg.retval = un->un_softcount;
	        if (io_arg.sectst)
		    un->un_softcount = io_arg.datasz;
	        if (copyout((caddr_t)&io_arg,(caddr_t)arg,sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
		u.u_error = 0;
		return;
	    case DIOCRDCAP:
	        if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
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
		    ERR_RETURN(EFAULT)
	        
	        if (io_arg.datasz == 0) {
		    status = DIOC_BADSIZE;
		    break;
	        }
	        if (io_arg.sectst > ALL_DEFECTS) {
		    status = DIOC_EINVAL;
		    break;
	        }
#ifdef STANDALONE
                addr = (u_int)align_malloc(io_arg.datasz,4);
#else STANDALONE
                addr = (u_int)kern_malloc(io_arg.datasz);
#endif STANDALONE
	        un->un_spltmp = (char *)K2_TO_K1(addr);
		un->un_spllen = io_arg.datasz;
	    	if(!vme_iomap(c->c_csh, un->un_spltmp, un->un_spllen,
			    GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			    &un->un_splph, &io_addr))
	    	    cmn_err(CE_PANIC, "Can't map DIOCRDEFECTS structure!\n");
	  	dkvj_wb_cache(un->un_spltmp, io_arg.datasz);
		if (vjsplcmd(un,VJ_READDEF,io_arg.sectst,
			io_arg.datasz, io_addr, WAIT)){
		    status = DIOC_OPERR;
	        } else {
		    /*
		     * The number of entries is in bytes 2 and 3
		     */
		    dh = (DEFECT_HEADER *)un->un_spltmp;
		    i = sizeof( DEFECT_HEADER) + 
				(dh->list_size * sizeof(DEFECT_ENTRY));

		    if (i > io_arg.datasz)
		        i = io_arg.datasz;
		    if (copyout((caddr_t)un->un_spltmp,
			    (caddr_t)io_arg.memaddr,i) < 0) {
		        status = DIOC_EFAULT;
		    }
	        }
#ifndef STANDALONE
	        kern_free(addr);
#endif STANDALONE
	        break;

	    case GIOCGETVAL:
	        if (suser() == 0)
		    ERR_RETURN(EPERM)
	        if (copyin(arg, (caddr_t) &gioctl, sizeof(gioctl)) < 0)
		    ERR_RETURN(EFAULT)
	        
	        switch (gioctl.gi_page) {
		    case PD_ERROR:
		        i = sizeof(SCSI_MS_ERROR);
			cp = (char *)K0_TO_K1( &un->un_mserror );
		        break;
		    case PD_DISREC:
		        i = sizeof(SCSI_MS_DISREC);
			cp = (char *)K0_TO_K1( &un->un_msdisrec );
		        break;
		    case PD_FORMAT:
		        i = sizeof(SCSI_MS_FORMAT);
			cp = (char *)K0_TO_K1( &un->un_msformat );
		        break;
		    case PD_GEOM:
		        i = sizeof(SCSI_MS_GEOM);
			cp = (char *)K0_TO_K1( &un->un_msgeom );
		        break;
		    case PD_RDAHD:
		        i = sizeof(SCSI_MS_RDAHD);
			cp = (char *)K0_TO_K1( &un->un_msrdahd );
		        break;
		    case PD_CACHE:
		        i = sizeof(SCSI_MS_CACHE);
			cp = (char *)K0_TO_K1( &un->un_mscache );
		        break;
		    default:
		        i = sizeof(SCSI_MODE_SENSE);
			cp = (char *)K0_TO_K1( &un->un_msense );
		        break;
	        }
	        if (gioctl.gi_size < i) {
		    status = GIOC_BADSIZE;
		    goto gioctl_done;
	        }
	        if (vjsplcmd(un,VJ_MODE_SENSE_CMD,gioctl.gi_page,
				    gioctl.gi_size,cp,WAIT)) {
		    status = GIOC_OPERR;
	        } else {
    		    if (!vme_ioflush( un->un_sph, un, sizeof(VJ_UNIT)))
      		        cmn_err( CE_PANIC, "Can't flush unit structure from I/O system!");
		    if (copyout((caddr_t)cp,(caddr_t)gioctl.gi_addr,
				    gioctl.gi_size) < 0) {
		        status = GIOC_EFAULT;
		    }
	        }
	        goto gioctl_done;
    
	    case GIOCSETVAL:
	        if (suser() == 0)
		    ERR_RETURN(EPERM)
	        if (copyin(arg, (caddr_t) &gioctl, sizeof(gioctl)) < 0)
		    ERR_RETURN(EFAULT)
	        
	        if (gioctl.gi_size < sizeof(SCSI_MODE_SENSE)) {
		    status = GIOC_BADSIZE;
		    goto gioctl_done;
	        }
#ifdef STANDALONE
                addr = (u_int)align_malloc(gioctl.gi_size,4);
#else STANDALONE
                addr = (u_int)kern_malloc(gioctl.gi_size);
#endif STANDALONE
		switch (gioctl.gi_page) {
		    case PD_ERROR:
			if( gioctl.gi_size != sizeof(SCSI_MS_ERROR)){
			    status = GIOC_EINVAL;
			    goto gioctl_done1;
			}
			cp = (char *)K0_TO_K1( &un->un_mserror );
			err = (SCSI_MS_ERROR *)cp;
	        	err->msense.hdr.sense_data_len = 0;
	        	err->error_hdr.ps = 0;
			break;
		    case PD_DISREC:
			if( gioctl.gi_size != sizeof(SCSI_MS_DISREC)){
			    status = GIOC_EINVAL;
			    goto gioctl_done1;
			}
			cp = (char *)K0_TO_K1( &un->un_msdisrec );
			break;
		    case PD_RDAHD:
			if( gioctl.gi_size != sizeof(SCSI_MS_RDAHD)){
			    status = GIOC_EINVAL;
			    goto gioctl_done1;
			}
			cp = (char *)K0_TO_K1( &un->un_msrdahd );
			break;
		    case PD_CACHE:
			if( gioctl.gi_size != sizeof(SCSI_MS_CACHE)){
			    status = GIOC_EINVAL;
			    goto gioctl_done1;
			}
			cp = (char *)K0_TO_K1( &un->un_mscache );
			break;
		    default:
			status = GIOC_EINVAL;
			goto gioctl_done1;
		}
		/*
		 * Save old data away in case of error 
		 */
		bcopy( cp, addr, gioctl.gi_size);

	        if (copyin((caddr_t)gioctl.gi_addr,(caddr_t)cp, gioctl.gi_size) < 0) {
		    status = GIOC_EFAULT;
		    goto gioctl_done1;
	        }
	        if (vjsplcmd(un,VJ_MODE_SELECT_CMD,gioctl.gi_page,
				    gioctl.gi_size,cp,WAIT)) {
		    status = GIOC_OPERR;
		    bcopy( addr, cp, gioctl.gi_size);
	        }
gioctl_done1:
#ifndef STANDALONE
	        kern_free(addr);
#endif STANDALONE
gioctl_done:
	        gioctl.gi_retval = status;
	        if (copyout((caddr_t)&gioctl,arg,sizeof(gioctl)) < 0)
		    ERR_RETURN(EFAULT)
	        if (status)
		    ERR_RETURN(EIO)
		else{
#ifdef STANDALONE
		    return(0);
#else
		    u.u_error = 0;
		    return;
#endif
		}
	    default:
		ERR_RETURN(EINVAL);
            }
        io_arg.retval = status;
        if( copyout((caddr_t)&io_arg,arg, sizeof(io_arg)) , 0 )
	    ERR_RETURN(EFAULT)
	if( status)
	    ERR_RETURN(EIO)
	u.u_error = 0;
	return;
	
    } else if(un->un_flags & IVJ_TAPE) {
       bp = &un->un_sbuf;
       switch (cmd) {

	case MTIOCTOP:	/* tape operation */
	    mtop = (struct mtop *)arg;
	    if (un->un_eomcount > MAXEOM) {
		switch (mtop->mt_op) {
		    case MTREW:
		    case MTOFFL:
		    case MTRET:
		    case MTONL:
			break;
		    default:
			cmn_err(CE_CONT,
			 "DKVJ %d:%d: too many commands past eom, %d allowed\n",
			 un->un_ctlr,un->un_slave,MAXEOM);
			bp->b_error = EIO;
			goto ioctlerr;
		}
	    }
	    switch (mtop->mt_op) {

		case MTWEOF:
		    count = mtop->mt_count;
		    un->un_command = VJ_W_FM;
		    break;

		case MTFSF: case MTBSF:
		    count = mtop->mt_count;
		    if (mtop->mt_op == MTBSF)
			count = ~count + 1; /* two's complement */
		    code = SP_FILEMARK;
		    un->un_command = VJ_SPACE;
		    break;

		case MTFSR: case MTBSR:
		    count = mtop->mt_count;
		    if (mtop->mt_op == MTBSR)
			count = ~count + 1; /* two's complement */
		    code = SP_BLOCK;
		    un->un_command = VJ_SPACE;
		    break;

		case MTREW:
		    count = 0;
		    un->un_command = VJ_REWIND;
		   un->un_flags&=~(IVJ_WRITTEN|IVJ_RFM|IVJ_FM|IVJ_EOM|IVJ_READ);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;

		case MTOFFL: 
		    if(vjsplcmd(un, VJ_REWIND, 0, 0, 0,WAIT))
			goto ioctlerr;
		    count = 0; /* unload tape */
		    un->un_command = VJ_LOAD;
		   un->un_flags&=~(IVJ_WRITTEN|IVJ_RFM|IVJ_FM|IVJ_EOM|IVJ_READ);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTRET: 
		    count = 3; /* retension and load tape */
		    un->un_command = VJ_LOAD;
		   un->un_flags&=~(IVJ_WRITTEN|IVJ_RFM|IVJ_FM|IVJ_EOM|IVJ_READ);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTONL: 
		    count = 1; /* load tape */
		    un->un_command = VJ_LOAD;
		   un->un_flags&=~(IVJ_WRITTEN|IVJ_RFM|IVJ_FM|IVJ_EOM|IVJ_READ);
		    un->un_eomcount = un->un_weomcount = 0;
		    break;
		case MTAPP: 
		    if (vjsplcmd(un, VJ_LOAD, 0, 1, 0,WAIT))
			goto ioctlerr;
		    count = 0;
		    code = SP_ENDOFDATA;
		    un->un_command = VJ_SPACE;
		    break;
		case MTNOP: 
		case MTRST:
		    ERR_RETURN(EINVAL)

		default:
		    ERR_RETURN(EINVAL)
	    } /* end of switch */

	    vjsplcmd(un,un->un_command,code,count,0,WAIT);
	    if ((mtop->mt_op==MTFSR || mtop->mt_op==MTBSR) && bp->b_resid)
		ERR_RETURN(EIO)
	    if (bp->b_flags & B_ERROR) {
ioctlerr:
		cmn_err(CE_CONT,"tape ioctl: error, bp->b_error= 0x%x\n",
		    bp->b_error);
		ERR_RETURN(EIO)
	    }
	    /* in this case we're no longer at EOM */
	    if (mtop->mt_op==MTBSR || mtop->mt_op==MTBSF)
		un->un_flags &= ~IVJ_EOM;
	    break;

	case MTIOCGET:
	    mtget = (struct mtget *)arg;
	    mtget->mt_type = MT_JAG;
	    mtget->mt_dsreg = un->un_flags;
	    mtget->mt_erreg = un->un_flags>>16;
	    mtget->mt_resid = 0;
	    break;
	default:
	    ERR_RETURN(EINVAL)
       }
    }
}

vjwaitcqa(c)      /* wait for CQA to complete */
register VJ_CTLR *c;
{
    volatile VJ_CRB *crb;

    crb   = &c->c_io->sh_CRB;

    DELAY(256);
    while(!(W(crb->crb_CRSW) & M_CRSW_CRBV))
    {
        DELAY(128);
    }

    if (W(crb->crb_CRSW) & M_CRSW_CQA) {
        CRB_CLR_DONE(crb->crb_CRSW);
     }
}
#endif STANDALONE

int
vjwaitf(c, mask,cmd)      /* wait for format to complete */
register VJ_CTLR *c;
register UWORD mask;
int cmd;
{
    register VJ_SHIO *shio;
    volatile VJ_CRB *crb;
    register VJ_IOPB *iopb;
    SCSI_CDB *cdb;
    register int rc;
#ifdef STANDALONE
    register count,x;

    printf("Formatting...Please be patient...");
    count = x = 0;
#endif STANDALONE

    shio = c->c_io;
    crb   = &shio->sh_CRB;
    iopb = &shio->sh_RET_IOPB;
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];

    DELAY(256);
    while (!(W(crb->crb_CRSW) & M_CRSW_CRBV))
    {
#ifdef STANDALONE
	if((++count % 0x800) == 0){
	    if(x){
		printf("\r");
		x = 0;
	    } else{
		printf("\r.");
		x = 1;
	    }
	}
	_scandevs();
#endif STANDALONE
        DELAY(128);
    }
#ifdef STANDALONE
    printf("\n");
#endif STANDALONE

    rc = 0;
    if (!(W(crb->crb_CRSW) & M_CRSW_CRBV)) {
	rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d: CRSW(0x%x) CRVB not found.\n",
		c->c_ctlr,W(crb->crb_CRSW));
    } else if ((W(crb->crb_CRSW) & mask) != mask) {
	rc = 1;
	log(BSD43_LOG_ERR,"DKVJ %d: CRSW(0x%x) mask=%x\n",W(crb->crb_CRSW),
		c->c_ctlr,mask);
    }
    if (W(crb->crb_CRSW) & M_CRSW_ER && rc == 0)
    {
	rc = 1;
	log(BSD43_LOG_ERR,
	    "DKVJ %d: Error: Command = 0x%x %x CRSW(0x%x) status = 0x%x\n",
		c->c_ctlr,
		iopb->iopb_CMD, cdb->cmd, W(crb->crb_CRSW), iopb->iopb_STATUS);
    }
    CRB_CLR_DONE(crb->crb_CRSW);
    return(rc);
}

#ifndef STANDALONE
/*
 * Setup a read/write command, using scatter gather if possible.
 * (MACSI Version)
 *  Word-wide scatter/gather will be used if the request is non-page
 *  aligned or if the request count is not modulo 4.
 */
int
vjSGsetup(c, bp, iopb, flag)
register VJ_CTLR *c;
register BUF *bp;
register VJ_IOPB *iopb;
int flag;
{
    register IPSG *sg;
    register long amount;
    register int links;
    register long dmaaddr;
    register ULONG physaddr;
    register long total;
    register long offset;
    register IPSG *firstsg;
    register VJ_ALIGN *al = NULL;
    register BUF *nbp;
    register uint cbn;	/* block number of current bp	*/
    IPSG_FREE *ipsg_free;
    register VJ_UNIT *un;
    register struct iobuf *utab;
    register VJ_DISK_HD *dp;
    int unit = BPTOVJN(bp);
    ioaddr_t io_addr;
    int abnormal, temp, no_combine, read_write;
    BUF_EXT *buf_ext, *obuf_ext, *buf_ext1;
    int not_aligned;
    int requested_size;

    un = c->c_unit[unit];
    ipsg_free = c->dkvjsg_hd;       /* get top free entry   */
    ASSERT(ipsg_free != 0);
    sg = &ipsg_free->ipsg[0];       /* get top SG  entry    */
    firstsg = (IPSG*)sg;
    al = &ipsg_free->align;
    al->al_faddr = 0;
    links = total = 0;
    abnormal = no_combine = 0;
    if (!(c->c_mode & C_MACSI_SORT))
	    utab = un->un_tab;
    else
  	    no_combine++;	    /* no rd/wr combining!  */	
    dp = &c->c_disk_hd;

    obuf_ext = buf_ext = (BUF_EXT*)bp->av_back;
    cbn = (uint)pblkno; /* (b_bcount - b_resid) handled in dkvjintr */

    dmaaddr = (long)bp->b_dmaaddr + bp->b_bcount - bp->b_resid;

    physaddr = ctob(kvtokptbl(dmaaddr)->pgm.pg_pfn);
    ASSERT(physaddr != 0);
    offset = dmaaddr & (POFFMASK);
    physaddr |= offset;
    requested_size = amount = bp->b_resid;
    /* The jaguar doesn't handle transfers which cross page boundaries if
     * the initial segment is smaller than 512 bytes, so we copy the data.
     * (If you don't, data for another controller may get "trashed")
     * Also copy the data for odd starting addresses or odd transfer count.
     *
     * Jaguar can't handle a non-512 aligned transfer which is setup to cross
     * a page boundary (i.e. has an SG list) unless we actually transfer data
     * into the block which spans the boundary.  This may not happen on reads
     * from variable record devices, so those transfers must be aligned.
     * Failure to do so results in "trashed" data for another device on the
     * same SCSI bus.
     */
    if (((physaddr & 0x01) || (amount & 0x1)) ||
	(((NBPP - offset) < NBPSCTR) && (bp->b_dmalen > 1)) ||
	(flag && (bp->b_dmalen > 1) && (physaddr & (NBPSCTR - 1)) &&
	 	(bp->b_flags & B_READ)) ) {
	al->al_size = amount;
	if (flag) {			   /* variable record device */
	    if (amount & 0x01)
		amount++; /* only short dma works (scsi cmd has correct count)*/
	} else if (amount & (NBPSCTR-1)) /* fixed record device */
	    amount += (NBPSCTR - (amount & (NBPSCTR - 1)));
	al->al_uaddr = dmaaddr;
	/* for now we'll allow a sleep here considering that we'll get here
	 * via strategy and not out of the interrupt handler (for tape).
	 * XXX a real fix for this needs to be implemented
 	 */
	al->al_taddr = physaddr = (ULONG)kmemalloc(amount, M_DEVBUF, M_WAITOK);
	/* Make sure we get a sector aligned buffer.  If not, return it and
	 * allocate a buffer that we can align to the next sector boundary.
	 * Place kmemalloc returned address in al_taddr, set al_faddr to the
	 * sector aligned address.
	 */
	if (al->al_taddr & (NBPSCTR-1)) {
	  kmemfree(al->al_taddr, M_DEVBUF, M_NOWAIT);
	  al->al_taddr = (ULONG)kmemalloc(amount+NBPSCTR, M_DEVBUF, M_WAITOK);
	  physaddr = ((al->al_taddr + NBPSCTR) & ~(NBPSCTR-1));
	}
	al->al_faddr = physaddr;
	if (!physaddr) {
	    log(BSD43_LOG_ERR,
	    "DKVJ %d:%d kmemalloc failed!\n", c->c_ctlr,unit);
	    return(0);
	}
	dmaaddr = physaddr;
        physaddr = K2_TO_PHYS(physaddr);
	bp->b_resid = amount; /* in case it changed */
	if (!(bp->b_flags & B_READ)) /* copy from user space if write */
	    bcopy(al->al_uaddr,dmaaddr,al->al_size);
	writeback_virtual_data(dmaaddr, al->al_size);
	abnormal = 1;
    }
    read_write = bp->b_flags & B_READ;/* set reading or writing for combining */
    /* Jaguar doesn't handle non-word alignment or non-word multiples very
     * well.  May need to use 16 bit transfer mode. Also, can't handle mixed
     * transfer modes in an SG list -- must all be 16 bit or 32 bit.
     */
    not_aligned = ((physaddr & 0x03) | (amount & 0x03));
lbpl:
    if (!abnormal)
    	dmaaddr = (long)bp->b_dmaaddr + bp->b_bcount - bp->b_resid;

    /* set up GBA system map for this request */

    if(!buf_ext->sph)
    if(!vme_iomap(c->c_csh, dmaaddr, bp->b_resid, 0, &buf_ext->sph, &io_addr)) 
	cmn_err(CE_PANIC, "Can't map dmaaddr !\n");

    while (bp->b_resid) {
        /* limit this sg to this page */
	amount = bp->b_resid;
        offset = dmaaddr & (POFFMASK);
        if (offset + amount > NBPP)
            amount = NBPP - offset; /* limit count to page size */

	if (!ka_to_vmeaddr(buf_ext->sph, dmaaddr, &io_addr)) {
#ifdef DKVJ_DEBUG
	  cmn_err(CE_WARN, "vjSGsetup: out of I/O map registers, links 0x%x\n",
		  links);
#endif DKVJ_DEBUG
	  no_combine++;
	  break; /* ran out of map regs. start transfer */
	}
	
        physaddr = io_addr;
	/* 
	   if what's left over at the end of a sg, is less than sector size,
	   reduce amount such that the next transfer will be a sector size.
	*/
	if (++links == (MACSI_SG - 1)) { /* save one sge */
	    temp =  NBPSCTR - ((bp->b_resid - amount) % NBPSCTR) ;
            amount -= temp;
	}
        dmaaddr += amount;
        total += amount;
	bp->b_resid -= amount;
	if (Blockmode_ok) {
	    /* 
	     * if we're sure we're using sg and some element will be non mod 4
	     * or the element address is non-word aligned or the element
	     * count is not a word multiple.  The check for not_aligned is a
	     * check on the entire transfer -- the other checks are only on
	     * the current SG element.
	     */
	    if (((amount%4) && (links>1)) || (physaddr & 2) || (amount & 2)
		|| not_aligned) {
		sg->sg_meminfo = MEMTYPE_16; /* use 16-bit transfers */
		sg->sg_addrmod = VME_A32NPAMOD; /* use non-block mode dma */
		al->al_no_block = links; /* indicate we need to put back */
		abnormal = 1;
	    }
	}
	/* this is CRITICAL! For FAST SG every element MUST be %512!! */
	if (amount & (NBPSCTR - 1))
		abnormal = 1;
        /* fill in scatter gather struct */
	sg->sg_count = amount;
        sg->sg_addr_msw = HI16(physaddr);
        sg->sg_addr_lsw = LO16(physaddr);
        sg++;
        if (links >= (MACSI_SG - 1)) {
            /* Too Many links, start up request */
	    no_combine++;
            break;
        }
    }
#ifdef R6000
    /* Limit number of bps which will be combined.  By placing check here, we
     * may use more than dkvj_max_combine_links if we need them to satisfy an
     * individual bp request, but will stop combining after that bp.  This
     * check is 6000 specific since the 6000 has a limited number of I/O
     * map registers, which we're attempting to preserve.
     */
    if (links > dkvj_max_combine_links)
      no_combine++;
#endif R6000

    if (!no_combine && !abnormal) {
	/*
	 * If there is room on the scatter/gather list, check
	 * and see if there are a number of contiguous reads/writes
	 * in this unit queue.  If there are, combine them into
	 * one iopb transfer, using the s/g list for all of the
	 * bp's.
	 */
	nbp = bp->av_forw;
	if (nbp) {
    	   buf_ext = (BUF_EXT*)nbp->av_back;
	   if((!bp->b_error) &&			/* retry? */
	     ((nbp->b_flags & B_READ) == read_write) &&	/* nbp follows r/w? */
	     (!buf_ext->ext_resid) &&		/* residual */
	     (!nbp->b_error) &&			/* retry? */
	     ((cbn + (bp->b_bcount >> SCTRSHFT)) == (uint)pblkno) &&
	     (!((long)nbp->b_dmaaddr & (NBPSCTR - 1))) &&
	     (!(nbp->b_bcount & (NBPSCTR - 1)))) {

		bp = nbp;
		utab->qcnt--;
		utab->b_actf = bp->av_forw; /* unlink */
		buf_ext = (BUF_EXT*)bp->av_back;
		/*
		 * save list of bp's sent to board (for catastrophic recovery)
		 */
		buf_ext->ext_forw = (BUF *)0;
		if (dp->b_forw == (BUF *)0)
			dp->b_forw = bp;
		else {
			buf_ext1 = (BUF_EXT*)dp->b_back->av_back;
			buf_ext1->ext_forw = bp;
		}
		dp->b_back = bp;

		bp->b_resid  = bp->b_bcount;
		cbn = (uint)pblkno;
		pblkno = (BUF*)0;
		goto lbpl;
	   }
	}
    }
    bp->av_forw = NULL;				/* end lbp list	*/
    /* Check for special case of non-sector multiple transfer to a fixed
     * sector size device but with additional bytes to be transferred as
     * part of this request.  This case can only occur if we run out of a
     * resource while setting up the request -- either run out of room in
     * the SG list for links OR run out of I/O map registers on the 62x0.
     * Truncate the last SG element back so total size is a sector multiple.
     */
    if (bp->b_resid && (!flag) && (amount = (total & (NBPSCTR -1)))) {
	sg--;
	if (sg->sg_count < amount)
	  cmn_err(CE_PANIC, "vjSGsetup: Need to truncate 0x%x, SG has 0x%x\n",
		  amount, sg->sg_count);
	sg->sg_count -= amount;
	total -=amount;
	bp->b_resid += amount;
	sg++;
    }

    if(!flag && (amount = (total & (NBPSCTR -1)))) {  /* non-sector multiple? */
      	/* We're about to toss data into/outof the bit bucket to round up
	 * to a sector multiple.  Check that the user request doesn't really
	 * want the data.  If the following check PANICS it's due to an
	 * unanticipated failure in setting up the request for I/O.
	 */
	if (bp->b_resid)
	  cmn_err(CE_PANIC, "vjSGsetup: b_resid non-zero (0x%x)\n",bp->b_resid);
	/* YES, then read the rest into 'temp buffer' i.e. throw it away */
	sg->sg_count = NBPSCTR - amount;
	total += sg->sg_count;
	bzero(temp_buffer, SECSIZE);

	if (c->c_iodata_sph[0] == 0) {
	  if(!vme_iomap(c->c_csh,temp_buffer,SECSIZE,
			GBA_CONTIG_ADDR+GBA_NOPART_MAP,&c->c_iodata_sph[0],
			&io_addr))
	    cmn_err(CE_PANIC, "Can't map temp_buffer !\n");
	} else
	  if (!ka_to_vmeaddr(c->c_iodata_sph[0], temp_buffer, &io_addr))
	    cmn_err(CE_PANIC, "Address not mapped properly!");
	
	writeback_virtual_data(temp_buffer, SECSIZE);

	physaddr = io_addr;
	sg->sg_addr_msw = HI16(physaddr);
	sg->sg_addr_lsw = LO16(physaddr);
	++links;
	log(BSD43_LOG_ERR,"DKVJ %d:%d Padded to 512 (%d) S/G links (%d)\n", 
		c->c_ctlr,unit,sg->sg_count,links);
	abnormal = 1;
	/* Jaguar can't handle block mode unless it's a word multiple */
	if (Blockmode_ok && ((sg->sg_count % 4) || not_aligned)) {
	  /* If not a multiple of words, switch to 16-bit, non-block mode */
	  sg->sg_meminfo = MEMTYPE_16; /* use 16-bit transfers */
	  sg->sg_addrmod = VME_A32NPAMOD; /* use non-block mode dma */
	  al->al_no_block = links; /* indicate we need to put back */
	}
    }
    if (links == 1) {
        /* With only one link, don't bother scatter/gathering */
        iopb->iopb_LENGTH       = total;   wbflush();
        iopb->iopb_TTLENGTH     = 0; wbflush();/*make error reporting clearer */
        iopb->iopb_BUFF = 
		(ULONG)((firstsg->sg_addr_msw << 16) | firstsg->sg_addr_lsw);
	wbflush();
	if (Blockmode_ok) {
	    /* must be modulo 4 for block mode and addr must be long aligned */
	    if ((iopb->iopb_LENGTH % 4) || (iopb->iopb_BUFF & 0x2)) {
        	W(iopb->iopb_ADDR)      = ADDR_MOD;   /* non-BLOCK MODE */
		wbflush();
	    }
	}
	if (al->al_faddr) { /* we need to free in dkvjintr */
		c->dkvjsg_hd = ipsg_free->nxt;  /* unlink */
		obuf_ext->ext_back = (BUF*)ipsg_free; /* for dkvjintr */
	} else {
		obuf_ext->ext_back = (BUF*)0; /* zero 'pblkno' */
	}
    } else {
        c->dkvjsg_hd = ipsg_free->nxt;  /* unlink */
	obuf_ext->ext_back = (BUF*)ipsg_free; /* save for int rout to free */
	c->c_tab->qcnt++;
	if (!abnormal) /* use the FAST scatter/gather option */
        	W(iopb->iopb_OPTION) |= (M_OPT_SG|M_OPT_FAST);
	else
        	W(iopb->iopb_OPTION) |= M_OPT_SG;
	/* force total count to modulo 2 if required (scsi cmd is correct) */
        iopb->iopb_TTLENGTH     = total; wbflush();
        iopb->iopb_LENGTH       = links; wbflush();

	dkvj_ctlr_ioinit( c, firstsg, sizeof(IPSG_FREE), &io_addr );

        iopb->iopb_BUFF         = io_addr; wbflush();
    }
    wbflush();

    if (flag) {
      	/* For variable length devices, we return requested size if we added
	 * added pad and setup "extra" bytes for VME blockmode transfer.  If
	 * fewer bytes were setup, then return the number of bytes setup.
	 * This allows us to write odd byte size variable length tape records.
	 */
      	if (total > requested_size)
	  return(requested_size);
	else
	  return(total);
      }
    else
	return(total >> SCTRSHFT);
}

/*----------------------------------------------------------------------*\
 *			IOCTL assist functions				*
\*----------------------------------------------------------------------*/

vjsplcmd(un, cmd, code, count, addr, wait)
VJ_UNIT *un;
int cmd,code,count,addr,wait;
{
    register BUF *bp;
    int s;

    bp = &un->un_sbuf;
    s = splclock();
    while (bp->b_flags & B_BUSY)
    {
	bp->b_flags |= B_WANTED;
	sleep((caddr_t)bp, PRIBIO);
    }
    bp->b_flags = B_BUSY|B_SPL;
    splx(s);
    bp->b_dev = MKDEV(un);	/* Fake up a DEV */
    bp->b_bcount = count;
    bp->b_blkno = code;	
    bp->b_length = cmd;
    bp->b_un.b_addr = (caddr_t)addr;
    bp->av_forw = 0;
    switch (cmd) {
	case VJ_LOAD:
	case VJ_REWIND:
	    un->un_timeout = TIME_REWIND * HZ;
	    break;
	case VJ_SPACE:
	    if (code == SP_FILEMARK)
		    un->un_timeout = TIME_FSF * HZ;
	    else
		    un->un_timeout = TIME_RDWR_TAPE;
	    break;
	case VJ_FORMAT:
	    un->un_timeout = TIME_FORMAT * HZ;
	    break;
	default:
    	    if (un->un_flags & IVJ_DISK)
	        un->un_timeout = TIME_RDWR_DISK;
	    else
	        un->un_timeout = TIME_RDWR_TAPE;
	    break;
    }
    dkvjstrategy(bp);
    if (wait) {
	iowait(bp);
	if(bp->b_flags & B_ERROR)
	    return(TRUE);
    }
    return(FALSE);
}

dkvjprint(dev,str)
dev_t dev;
char * str;
{
    log(BSD43_LOG_ERR,"ijc%dd%ds%d: %s (dev 0x%x)\n",
	    CTLR(dev), VJUNIT(dev), LPART(dev), str, dev);
}

/*
 * return partition size in 'blocks'
 */

dkvjsize(dev)
dev_t dev;
{
    register int unit , ctlr;
    register VJ_CTLR *c;
    register VJ_UNIT *un;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || vjctlrs[ctlr] == 0 ||
		vjctlrs[ctlr]->c_unit[unit] == 0) {
	return(-1);
    }
    c = vjctlrs[ctlr];
    un = c->c_unit[unit];
    if ((un->un_flags & IVJ_ALIVE) == 0)  /* unit OK? */
	return(-1);
    if ((un->un_flags & IVJ_DISK) && wait_ready(un,WAIT))/* is disk ready */
	return (-1);
    if (un->un_vhvalid == 0)
	return (-1);
    return (un->un_vh.vh_pt[LPART(dev)].pt_nblks);
}

/*
 * Dump data to disk.
 */

int
dkvjdump(dev, flag, bn, physaddr, count)
dev_t dev;
int flag;
daddr_t bn;
caddr_t physaddr;
int count;
{
    register int unit, ctlr;
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    SCSI_EXT_SENSE *sense;
    struct partition_table *pt;
    UINT status;
    ioaddr_t io_addr;
    sah_type temp_sph;
    int s;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    c = vjctlrs[ctlr];
    un = c->c_unit[unit];

    /*
     * If the drive doesn't exist,
     * or if it doesn't have a valid label, return an error.
     */
    if ((un->un_flags & IVJ_ALIVE) == 0)  /* unit OK? */
	return(ENODEV);
    if (wait_ready(un,WAIT))  /* is the disk ready */
	return(ENODEV);
    if (un->un_vhvalid == 0)
	return (EINVAL);
    if (LPART(dev) != 1)
	return (EINVAL);

    if (flag == DUMP_OPEN) {
	if (un->un_flags == 0)
		vjattach(unit,ctlr,un);
	if ((un->un_flags & (IVJ_ALIVE | IVJ_READY)) ==0)
		return(EIO);
	return (0);
    }
    if (flag == DUMP_CLOSE) {
	/* nop */
	return (0);
    }

    /* insure that request is within partition boundaries */
    pt = &un->un_vh.vh_pt[LPART(dev)];
    if ((bn < 0) || (bn + count > pt->pt_nblks)) {
	return (EINVAL);
    }
    bn += pt->pt_firstlbn;

    /* write count sectors worth of data
     */
tryagain:
    if (!vme_iomap(c->c_csh, PHYS_TO_K0(physaddr), count*NBPSCTR,
		   GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
      cmn_err(CE_PANIC, "Can't map area to be dumped!");

    writeback_virtual_data( PHYS_TO_K0(physaddr), count*NBPSCTR);

    s=splbio();
    vjcmd(un,VJ_WRITE,io_addr,bn,count,NO_INTERRUPT);
    status = vjwait_unit(un, M_CRSW_CC, VJ_WRITE);
    splx(s);

    if (!vme_iounmap( temp_sph ))
      cmn_err(CE_PANIC, "Can't flush/unmap dump area!");
    
    if ((status >> 8 & 0xff) == CHECK_CONDITION) {
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	if (vjsense(un, NO_INTERRUPT))
	    log(BSD43_LOG_ERR,"DKVJ %d:%d request sense ERROR\n",
		un->un_ctlr,un->un_slave);
	else if (sense->key == SCSI_UNIT_ATTENTION)
	    goto tryagain;
	else
	    log(BSD43_LOG_ERR,"DKVJ %d:%d %s\n",un->un_ctlr,un->un_slave,
		    sense_err_msg(sense->key));
	return(EIO);
    } else if (status & 0xff) {
	vjerror(status & 0xff);
	return(EIO);
    } else if (status) {
	log(BSD43_LOG_ERR,"DKVJ %d:%d error status = 0x%x\n",
		un->un_ctlr,un->un_slave,status);
	return(EIO);
    }
    return (0);
}

vjerror(type)
register unsigned type;
{
    register int i;
	
    for (i=0; jaguar_err[i].code != 0xff; i++)
	if (jaguar_err[i].code == type)
	    break;
    log(BSD43_LOG_ERR,"        Jaguar Error = %x (%s)\n",
	type, jaguar_err[i].msg);
}

void
vjtimeout(un)
register VJ_UNIT *un;
{
    register VJ_CTLR *c = vjctlrs[un->un_ctlr];
    register VJ_SHIO *shio = c->c_io;
    register USHORT crsw;
    VJ_CRB Crb, *crb = &Crb;
    VJ_CSB *csb = &shio->sh_CSS; /* Controller Specific Space/Block */
    int delay;

#ifdef DKVJ_DEBUG
    cmn_err(CE_NOTE,"DKVJ %d:%d command timeout\n", un->un_ctlr,un->un_slave);
#endif DKVJ_DEBUG    
    crsw = W(shio->sh_CRB.crb_CRSW);

    log(BSD43_LOG_ERR,"DKVJ %d:%d command timeout\n", un->un_ctlr,un->un_slave);
    if (un->un_timeid) {
        vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
	if (crsw & M_CRSW_CRBV) {
		/* see if the CRB is for this unit! */
		if(crb->crb_WORK_QUEUE-1 == un->un_slave) {
			log(BSD43_LOG_ERR,
			"DKVJ %d:%d CRB is valid for this unit\n",
			un->un_ctlr,un->un_slave);
#ifdef SPECIAL
			/* info from mips special firmware */
			log(BSD43_LOG_ERR,
			"vec_flag= %x timer_vec= %x iopb_vec= %x postvec= %x\n",
				csb->csb_VEC_FLAG,
				csb->csb_TIMER_VEC,
				csb->csb_IOPB_VEC,
				csb->csb_POST_VEC);
#endif SPECIAL
			dkvjintr(un->un_ctlr);
		} else {
			log(BSD43_LOG_ERR,
			"DKVJ %d:%d CRB valid but not for this unit\n",
			un->un_ctlr,un->un_slave);
			log(BSD43_LOG_ERR,"DKVJ %d:%d resetting timeout...\n",
				un->un_ctlr,un->un_slave);
			if(un->un_flags & IVJ_TAPE)
				delay = TIME_RDWR_TAPE;
			else	
				delay = TIME_RDWR_DISK;
			un->un_timeid = timeout_spl(vjtimeout,un,delay,splbio);
/* XXX right? */	dkvjintr(un->un_ctlr);
		}
	} else {
    		un->un_timeid = 0;
		log(BSD43_LOG_ERR,"DKVJ %d:%d No interrupt from controller\n",
			un->un_ctlr,un->un_slave);
		log(BSD43_LOG_ERR,"DKVJ %d:%d Flushing work queue %d\n",
			un->un_ctlr,un->un_slave,un->un_slave+1);
		vjcmd(un, VJ_FLUSH_WORKQ,0,0,0,WANT_INTERRUPT);
	}
    } else
	log(BSD43_LOG_ERR,"DKVJ %d:%d Timeout with no ID: qcount = %d\n",
		un->un_ctlr,un->un_slave,un->un_qcount);
}
#endif STANDALONE

int
vjsense_status(un,status) /* this routine taken from common_scsi.c */
register VJ_UNIT *un;
{
    register SCSI_EXT_SENSE *sense;
    register SCSI_MODE_SENSE *ms;
    register int diff, flag, sense_info;

    flag = 0;
    diff = un->un_xfer;
    if (!vme_ioflush( un->un_sph, un, sizeof(VJ_UNIT)))
      cmn_err( CE_PANIC, "Can't flush unit structure from I/O system!");
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    ms = (SCSI_MODE_SENSE *)K0_TO_K1(&un->un_msense);
    sense_info = (int)((sense->info1 << 24) | (sense->info2 << 16)|
		 (sense->info3 << 8) | (sense->info4));
    if (vjexterr) {
	cmn_err(CE_CONT,"\n     DKVJ %d:%d: sense status\n",un->un_ctlr,
					un->un_slave);
	cmn_err(CE_CONT,"       valid   =%d\n",sense->valid);
	cmn_err(CE_CONT,"       segment =%d\n",sense->segment);
	cmn_err(CE_CONT,"       filmrk  =%d\n",sense->filmrk);
	cmn_err(CE_CONT,"       eom     =%d\n",sense->eom);
	cmn_err(CE_CONT,"       ilength =%d\n",sense->ilength);
	cmn_err(CE_CONT,"       key     =0x%x %s\n",sense->key,
		sense_err_msg(sense->key));
	cmn_err(CE_CONT,"       info    =0x%x\n",sense_info);
	cmn_err(CE_CONT,"       add_len =%d\n",sense->add_len);
    }
    if (status == CHECK_CONDITION) {
	cmn_err(CE_CONT,
		"DKVJ %d:%d: check condition on request sense command\n",
		un->un_ctlr, un->un_slave);
	goto xxxerr;
    }
    if (sense->key == SCSI_RECOVERABLE_ERROR) {
	cmn_err(CE_CONT,"DKVJ %d:%d: %s\n", un->un_ctlr, un->un_slave, 
			sense_err_msg(sense->key));
	if (un->un_flags & IVJ_TAPE)
		un->un_flags &= ~IVJ_FM;
	return(0);
    } else if(sense->key == SCSI_UNIT_ATTENTION) {
	un->un_eomcount = un->un_weomcount = 0;
	un->un_flags |= IVJ_ATN;
	un->un_resid = un->un_xfer;
/*	un->un_bn = un->un_prev_bn;		*/
	return(0);
    }
    if(un->un_flags & IVJ_TAPE) {
	if (sense->valid) {
	    diff = sense_info;
	    if (!(un->un_flags & IVJ_VARIABLE))
		    diff *= 512;
	}
        if(sense->key == SCSI_NOT_READY) {
	    un->un_flags |= IVJ_NOT_RDY;
	    goto xxxerr;
        }
	if(sense->key == SCSI_BLANK_CHECK) {	/* treat like a file mark */
	    sense->filmrk = 1;
	    sense->key = 0;
	}
	if(sense->filmrk) {
	    flag = 1;
	    if ((un->un_flags & IVJ_FM) && (diff == un->un_xfer)) {
		un->un_flags |= IVJ_EOM;
		un->un_eomcount = MAXEOM + 1;
	    }
	    else {
		un->un_flags |= IVJ_FM;
		if (diff != un->un_xfer)
		    un->un_flags |= IVJ_RFM;
	    }
	} else
	    un->un_flags &= ~IVJ_FM;
	if(sense->eom) {
	    flag = 1;
	    if (!sense->valid)
		diff = 0;
	    un->un_flags |= IVJ_EOM;
	    ++un->un_eomcount;
    	    if (un->un_command == C0_WRITE) {
		++un->un_weomcount;
		/* 
		 * handle case of 1/4" tape not doing a complete write when
		 * EOM hit. The command will be reissued with the correct
		 * reduced b_resid. Note that a check condition for eom
		 * will be issued for each command past eom, at least for
		 * the archive drive we currently use.
		 */
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
		    "DKVJ %d:%d: illegal block length %d, actual=%d\n",
		    un->un_ctlr,un->un_slave,un->un_xfer,un->un_xfer - diff); 
		diff = un->un_xfer;
		goto xxxerr;
	    }
	} 
	if(sense->key == SCSI_DATA_PROTECT && !ms->hdr.WP) {
	    cmn_err(CE_CONT,
		"DKVJ %d:%d: Selected format not valid for tape cartridge\n",
		un->un_ctlr,un->un_slave);
	    diff = un->un_xfer;
	    goto xxxerr;
	} else if (sense->key || !flag) {
	    cmn_err(CE_CONT,"DKVJ %d:%d: %s\n", un->un_ctlr,un->un_slave,
		sense_err_msg(sense->key));
	    goto xxxerr;
	}
	un->un_resid = diff;
	return(0);
    } else {
	if (sense->key == SCSI_NOT_READY) {
	    un->un_flags |= IVJ_NOT_RDY;
	    return(0);
	}
	/* see if residual is valid for disk */
	if (sense->valid) {
	    cmn_err(CE_CONT,
		"DKVJ %d:%d: physical block address 0x%x (%d)\n",
		un->un_ctlr,un->un_slave,sense_info,sense_info);
	}
	flag = sense->sense_code;
	if (flag && (flag != POW_RST))
	    cmn_err(CE_CONT,
		"DKVJ %d:%d: error code 0x%x(%d); %s\n",
		un->un_ctlr,un->un_slave,flag,flag,print_errcode(flag));
    }
xxxerr:
    un->un_resid = diff;
    return(1);
}
#ifndef STANDALONE
static void
restart_ctlr(c,ctlr)
register VJ_CTLR *c;
{
    register IPSG_FREE *ipsg_free;
    register VJ_UNIT *un;
    register BUF *bp, *tp;
    register VJ_DISK_HD *dp;
    register int i, j, s;
    BUF_EXT *buf_ext;

    s = splbio();
    for (i = 0; i < c->c_maxunit; ++i) {
	if (un = c->c_unit[i]) {
	    un->un_qcount = 0;
	    if (un->un_timeid) {
		untimeout(un->un_timeid);
		un->un_timeid = 0;
	    }
	    if (un->un_flags & IVJ_WAITING) {
		un->un_flags &= ~IVJ_WAITING;
		wakeup((caddr_t)un);
	    }
	}
    }
    /* we must return errors for all outstanding commands */
    dp = &c->c_disk_hd;
    bp = dp->b_forw;
    while (bp) {
        buf_ext = (BUF_EXT*)bp->av_back;
	dkvj_buf_iodone(buf_ext);
	tp = buf_ext->ext_forw;
	if (dp->b_back == bp) /* end of list? */
	    dp->b_back = dp; /* update back pointer */
	if (ipsg_free = (IPSG_FREE *)buf_ext->ext_back) {
	    dkvj_c_iodone(c, ipsg_free, sizeof(IPSG_FREE));
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    buf_ext->ext_back = NULL;
	    if (ipsg_free->align.al_faddr)
		kmemfree(ipsg_free->align.al_taddr, M_DEVBUF, M_NOWAIT);
	}
	bp->b_error = EIO;
	bp->b_flags |= B_ERROR;
	if (!(bp->b_flags & B_SPL))
	    iounmap(bp);
	else
	    bp->b_flags &= ~(B_BUSY | B_SPL);
	iodone(bp);
	if (bp->b_flags & B_WANTED) {
	    bp->b_flags &= ~B_WANTED;
	    wakeup((caddr_t)bp);
	}
    	bp = dp->b_forw = tp; /* advance queue to next bp */
    }
    log(BSD43_LOG_ERR,"DKVJ %d: reseting controller\n",c->c_ctlr);
    if (vjmce_init(ctlr)) {    /* returns true if passed */
	/* init Scatter/Gather Free list    */
	for(i = 0; i < (NUM_M_SG - 1); i++) {
	    c->dkvjsg_fentry[i].nxt = &c->dkvjsg_fentry[i+1];
	    for(j = 0; j < MACSI_SG; j++) {
		if (Blockmode_ok) {
			c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
				 (TT_BLOCK << 2) | MEMTYPE; /* Block mode */
			c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
		} else {
			c->dkvjsg_fentry[i].ipsg[j].sg_meminfo = MEMTYPE;
			c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPAMOD;
		}
	    }
	}
	c->dkvjsg_fentry[i].nxt = (IPSG_FREE *) 0;/* end list   */
	c->dkvjsg_hd = &c->dkvjsg_fentry[0]; /* init head   */
	for(j = 0; j < MACSI_SG; j++) {
	    if (Blockmode_ok) {
	        c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
		 	(TT_BLOCK << 2) | MEMTYPE; /* Block mode */
	        c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
	    } else {
	        c->dkvjsg_fentry[i].ipsg[j].sg_meminfo = MEMTYPE;
	        c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPAMOD;
	    }
	}
	dkvj_wb_cache(c, sizeof(VJ_CTLR));
	for (i = 0; i < c->c_maxunit; i++)    /* attach bus devices */
	    vjattach(i,ctlr,c->c_unit[i]);
    	if (c->c_mode & C_MACSI_SORT)
		vjgo(c);
	else
		if (c->c_tab->b_actf)
			dkvjcstart(c);
    } else {
    	log(BSD43_LOG_ERR,"DKVJ %d: failed controller restart\n",c->c_ctlr);
    }
    splx(s);
}
extern struct devtable *Devboot;
extern struct devtable Dev_dkvj[];
int has_dkvj()
{
	Devboot = Dev_dkvj;
	return(1);
}
dkvjgetctlr(c,ct)
VJ_CTLR *c;
struct ctlr_info *ct;
{
    char tbuf[20];

    strcpy( ct->ci_type, "ijc");
    dk_btoa( c->c_ctlr, tbuf, 16);
    strcat( ct->ci_type, tbuf);
    strcat( ct->ci_type, " : Interphase Jaguar 4210 Revision (");
    strcat( ct->ci_type,  &c->c_csb.csb_PCODE[0]);
    strcat( ct->ci_type, "-"); 
    dk_btoa( c->c_csb.csb_PVAR , tbuf, 16);
    strcat( ct->ci_type, tbuf);
    strcat( ct->ci_type, "-"); 
    strcat( ct->ci_type,  &c->c_csb.csb_FREV[0]);
    strcat( ct->ci_type, ") Date ");
    strncat( ct->ci_type,  &c->c_csb.csb_FDATE[0], 2);
    strcat( ct->ci_type, "/"); 
    strncat( ct->ci_type,  &c->c_csb.csb_FDATE[2], 2);
    strcat( ct->ci_type, "/"); 
    strcat( ct->ci_type,  &c->c_csb.csb_FDATE[4]);
    return;

}

dk_btoa(n, str, base)
        u_char n;
        char *str;
{
        char prbuf[11];
        register char *cp;

        cp = prbuf;
	if( base == 16 ){
            do {
                *cp++ = "0123456789ABCDEF"[n%16];
                n >>= 4;
            } while (n);
	
	} else{
            do {
                *cp++ = "0123456789"[n%base];
                n /= base;
            } while (n);
	}

        do {
                *str++ = *--cp;
        } while (cp > prbuf);
	*str = NULL;
        return;
}

#endif STANDALONE


#if defined(DKVJ_DEBUG) || defined(R6000)
/* Given a bp returned by the controller, verify that it is a valid bp
 * (i.e. we think we have I/O outstanding against this bp).
 */

vj_verify_bp( c, ctlr_bp, expected )
VJ_CTLR *c;
struct buf *ctlr_bp;
int expected;
{
  struct buf *bp;
  VJ_DISK_HD *dp;
  BUF_EXT *buf_ext1;
  int found_bp=0;
  int s;

  s = splbio();
  dp = & c->c_disk_hd;

  if (bp = dp->b_forw) {
    while (bp) {
      buf_ext1 = (BUF_EXT*)bp->av_back;

      if (bp->av_forw == ctlr_bp)
	cmn_err(CE_WARN, "ctlr_bp (0x%x) combined into other bp (0x%x)\n",
		ctlr_bp, bp );
      if (bp == ctlr_bp)
	found_bp++;
      bp = buf_ext1->ext_forw;
    }
  }
  if (found_bp != expected)
    cmn_err(CE_WARN,
	"ctlr_bp (0x%x) found 0x%x times in pending list, expected 0x%x\n",
	    ctlr_bp, found_bp, expected );
  splx(s);
  return( found_bp );
}
#endif

#ifdef DKVJ_DEBUG 
vj_c_printf( c )
VJ_CTLR *c;
{
  int unit;
  VJ_UNIT *un;
  struct iobuf *utab;
  struct buf *bp;
  VJ_DISK_HD *dp;
  BUF_EXT *buf_ext1;
  VJ_SHIO *shio;
  VJ_CRB  Crb, Crb2, *crb;
  VJ_IOPB Iopb, Iopb2, *iopb;
  OFFBD_IOPB *off;
  int s,i;

  s=splbio();
  cmn_err( CE_NOTE, "dkvj: ctlr info (0x%x, 0x%x)  sph: 0x%x\n",
	  c, (int)c + sizeof(VJ_CTLR), c->c_sph );
  /*** Get info from CRB ***/

  /* Flush the cntlr structure (including CRB) from the I/O cache. */

  if(!vme_ioflush(c->c_sph, 0, 0))
    cmn_err(CE_PANIC, "Could not flush the CTLR struct \n");
  
  crb     = &Crb;
  iopb    = &Iopb;
  shio = c->c_io;
  vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
  vjfrom_shio(&shio->sh_RET_IOPB, iopb, sizeof(VJ_IOPB));
  
  if (c->c_mode & C_OFF_BOARD) {
    cmn_err(CE_CONT, "off-board CRB (0x%x) IOPB (0x%x)\n",
	    &c->c_off_crb, &c->c_off_iopb);
    invalidate_virtual_data( &c->c_off_crb, sizeof(VJ_CRB) + sizeof(VJ_IOPB));
    if ((bcmp(&c->c_off_crb, crb, sizeof(VJ_CRB))) ||
	(bcmp(&c->c_off_iopb, iopb, sizeof(VJ_IOPB)))) {
      cmn_err(CE_WARN, "On board CRB/IOPB  and off-board CRB/IOPB disagree!");
      crb = &c->c_off_crb;
      iopb = &c->c_off_iopb;
      cmn_err(CE_CONT, "CRB fields (off-board)\n");
      cmn_err(CE_CONT,
	  "   crsw 0x%x  IOPB_TYPE 0x%x CTAG 0x%x IOPB_LEN 0x%x workQ 0x%x\n", 
	      W(crb->crb_CRSW), crb->crb_IOPB_TYPE, crb->crb_CTAG,
	      crb->crb_IOPB_LENGTH,  crb->crb_WORK_QUEUE);
      cmn_err(CE_CONT,"IOPB fields (off-board)\n");
      cmn_err(CE_CONT,
	      "  cmd 0x%x opt 0x%x status 0x%x nvec 0x%x evec 0x%x lvl 0x%x\n",
	      iopb->iopb_CMD, W(iopb->iopb_OPTION), iopb->iopb_STATUS,
	      iopb->iopb_NVCT, iopb->iopb_EVCT, iopb->iopb_LEVEL );
      cmn_err(CE_CONT,
	 "  addrm 0x%x  bufaddr 0x%x  maxlen 0x%x  totSGlen 0x%x  unit 0x%x\n",
	      iopb->iopb_ADDR, iopb->iopb_BUFF, iopb->iopb_LENGTH,
	      iopb->iopb_TTLENGTH,  W(iopb->iopb_UNIT) );
      cmn_err(CE_CONT,
	      "  SCSI cmd words: 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
	      iopb->iopb_SCSI[0], iopb->iopb_SCSI[1], iopb->iopb_SCSI[2],
	      iopb->iopb_SCSI[3], iopb->iopb_SCSI[4], iopb->iopb_SCSI[5] );

      crb = & Crb;
      iopb = &Iopb;

      if(!vme_ioflush(c->c_sph, &c->c_off_crb, sizeof(VJ_CRB)+sizeof(VJ_IOPB)))
	cmn_err(CE_PANIC, "Could not explicitly flush the CRB \n");
      if ((bcmp(&c->c_off_crb, crb, sizeof(VJ_CRB))) ||
	  (bcmp(&c->c_off_iopb, iopb, sizeof(VJ_IOPB))))
	cmn_err(CE_WARN, "On & off board IOPB DIS-agree after explicit flush");
      else
	cmn_err(CE_WARN, "On & off board IOPB AGREE after explicit flush!");
    }
  }
  cmn_err(CE_NOTE, "CRB fields (on-board)\n");
  cmn_err(CE_CONT,
      "   crsw 0x%x  IOPB_TYPE 0x%x CTAG 0x%x IOPB_LEN 0x%x workQ 0x%x\n", 
      W(crb->crb_CRSW), crb->crb_IOPB_TYPE, crb->crb_CTAG, crb->crb_IOPB_LENGTH,
      crb->crb_WORK_QUEUE);
  cmn_err(CE_CONT,"IOPB fields (on-board)\n");
  cmn_err(CE_CONT,
      "  cmd 0x%x opt 0x%x status 0x%x nvec 0x%x evec 0x%x lvl 0x%x\n",
      iopb->iopb_CMD, W(iopb->iopb_OPTION), iopb->iopb_STATUS, iopb->iopb_NVCT,
      iopb->iopb_EVCT, iopb->iopb_LEVEL );
  cmn_err(CE_CONT,
      "  addrm 0x%x  bufaddr 0x%x  maxlen 0x%x  totSGlen 0x%x  unit 0x%x\n",
      iopb->iopb_ADDR, iopb->iopb_BUFF, iopb->iopb_LENGTH, iopb->iopb_TTLENGTH,
      W(iopb->iopb_UNIT) );
  cmn_err(CE_CONT,
      "  SCSI cmd words: 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
      iopb->iopb_SCSI[0], iopb->iopb_SCSI[1], iopb->iopb_SCSI[2],
      iopb->iopb_SCSI[3], iopb->iopb_SCSI[4], iopb->iopb_SCSI[5] );

  /*** Get info from on-board CQE ***/

  cmn_err(CE_CONT, "cqe_top 0x%x cqe_end 0x%x cqe_QHDP 0x%x\n",
	  c->c_cqe_top, c->c_cqe_end, c->c_cqe_QHDP);


  /*** Get info from off-board CRB/IOPB ***/
  
  cmn_err(CE_CONT, "off_top 0x%x  off_QHDP 0x%x\n",
	  c->c_off_top, c->c_off_QHDP );
  off = (OFFBD_IOPB *)c->c_off_top;
  for (i=0; i<NUM_CQE_OFF; i++,off++) {
    iopb = (VJ_IOPB *)(&off->copyiopb);
    if (off == c->c_off_QHDP)
      cmn_err(CE_CONT, "***** Following IOPB will be used NEXT ****\n");
    cmn_err(CE_CONT,
	    "  0x%x (0x%x)  cmd 0x%x opt 0x%x buff 0x%x len 0x%x  unit 0x%x\n",
	    i, off, iopb->iopb_CMD, W(iopb->iopb_OPTION), iopb->iopb_BUFF,
	    iopb->iopb_LENGTH, W(iopb->iopb_UNIT));
    cmn_err(CE_CONT,
	    "        tt_length 0x%x qecr 0x%x iopb_addr 0x%x CTAG 0x%x workQ 0x%x\n",
	    iopb->iopb_TTLENGTH,
	    W(off->copycqe.cqe_QECR), off->copycqe.cqe_IOPB_ADDR,
	    off->copycqe.cqe_CTAG, off->copycqe.cqe_WORK_QUEUE );
  }
  /*** Get info from unit table ***/

  for (unit = 0; unit < c->c_maxunit; unit++) {
    if (un = c->c_unit[unit]) {
      cmn_err(CE_CONT, "unit 0x%x: unit structure (0x%x, 0x%x) ctlr 0x%x\n",
	      unit, un, (int)un+sizeof(VJ_UNIT), un->un_ctlr);
      /*  un_tab info */
      utab = un->un_tab;
      cmn_err(CE_CONT, "  un_tab addr 0x%x b_active 0x%x b_forw 0x%x b_actf 0x%x",
	      utab, utab->b_active, utab->b_forw, utab->b_actf );
      if (utab->b_active || (utab->b_actf == NULL))
	cmn_err(CE_CONT, "  unit STARTED\n");
      else
	cmn_err(CE_CONT, "  unit NOT STARTED\n");
      bp = utab->b_actf;
      if (bp) {
	cmn_err(CE_CONT, "  bp chain ");
	while (bp) {
	  cmn_err(CE_CONT, " 0x%x\n", bp);
	  bp = bp->av_forw;
	}
      }
      /*              */
      cmn_err(CE_CONT, "\n  un_retries 0x%x un_qcount 0x%x un_queue_size 0x%x\n",
	      un->un_retries, un->un_qcount, un->un_queue_size);
      cmn_err(CE_CONT, "  un_workq 0x%x un_iotime 0x%x un_tab 0x%x un_sph 0x%x\n",
	      un->un_workq, un->un_iotime, un->un_tab, un->un_sph);
      cmn_err(CE_CONT, "  un_flags 0x%x ",
	      un->un_flags);
      if (un->un_flags & IVJ_ALIVE) cmn_err(CE_CONT, " ALIVE ");
      if (un->un_flags & IVJ_READY) cmn_err(CE_CONT, " READY ");
      if (un->un_flags & IVJ_BUSY) cmn_err(CE_CONT, " BUSY ");
      if (un->un_flags & IVJ_WRITTEN) cmn_err(CE_CONT, "  WRITTEN");
      if (un->un_flags & IVJ_TAPE) cmn_err(CE_CONT, " TAPE ");
      if (un->un_flags & IVJ_DISK) cmn_err(CE_CONT, " DISK ");
      if (un->un_flags & IVJ_WORM) cmn_err(CE_CONT, " WORM ");
      if (un->un_flags & IVJ_REWINDING) cmn_err(CE_CONT, " REWINDING ");
      if (un->un_flags & IVJ_SELECTED) cmn_err(CE_CONT, " SELECTED ");
      if (un->un_flags & IVJ_FORMATTED) cmn_err(CE_CONT, " FORMATTED ");
      if (un->un_flags & IVJ_WAITING) cmn_err(CE_CONT, " WAITING ");
      if (un->un_flags & IVJ_WANTED) cmn_err(CE_CONT, " WANTED ");
      if (un->un_flags & IVJ_OPEN) cmn_err(CE_CONT, " OPEN ");
      if (un->un_flags & IVJ_READ) cmn_err(CE_CONT, " READ ");
      if (un->un_flags & IVJ_EOM) cmn_err(CE_CONT, " EOM ");
      if (un->un_flags & IVJ_FM) cmn_err(CE_CONT, " FM ");
      if (un->un_flags & IVJ_PRINTER) cmn_err(CE_CONT, " PRINTER ");
      if (un->un_flags & IVJ_RMV_MEDIA) cmn_err(CE_CONT, " RMV_MEDIA ");
      if (un->un_flags & IVJ_ARB_DISABLE) cmn_err(CE_CONT, " ARB_DISABLE ");
      if (un->un_flags & IVJ_READONLY) cmn_err(CE_CONT, " READONLY ");
      if (un->un_flags & IVJ_ATN) cmn_err(CE_CONT, " ATN ");
      if (un->un_flags & IVJ_NOT_RDY) cmn_err(CE_CONT, " NOT_RDY ");
      if (un->un_flags & IVJ_VARIABLE) cmn_err(CE_CONT, " VARIABLE ");
      if (un->un_flags & IVJ_RFM) cmn_err(CE_CONT, " RFM ");
      cmn_err(CE_CONT, "\n");
      cmn_err(CE_CONT, "  un_eomcount 0x%x un_weomcount 0x%x\n",
	      un->un_eomcount, un->un_weomcount );
      cmn_err(CE_CONT, "  un_timeid 0x%x  un_timeout 0x%x  un_scsibusy 0x%x\n",
	      un->un_timeid, un->un_timeout, un->un_scsibusy );
      cmn_err(CE_CONT, "  un_aborta 0x%x  un_burst 0x%x  un_vhvalid 0x%x\n",
	      un->un_aborta, un->un_burst, un->un_vhvalid );
      cmn_err(CE_CONT, "  un_savebp 0x%x  un_lastcyl 0x%x\n",
	      un->un_savebp, un->un_lastcyl);
      cmn_err(CE_CONT, "  io_cnt 0x%x io_bcnt 0x%x io_resp 0x%x io_act 0x%x\n",
	      un->un_iotime->io_cnt, un->un_iotime->io_bcnt, un->un_iotime->io_resp, un->un_iotime->io_act);
    } else {
      cmn_err(CE_CONT, "unit 0x%x: no unit structure allocated\n", unit);
    }
  }
  cmn_err(CE_CONT,"Active unit queue, b_actf (0x%x) b_actl (0x%x)\n",
	  c->c_tab->b_actf, c->c_tab->b_actl);
  utab = (struct iobuf *)c->c_tab->b_actf;
  while (utab) {
    cmn_err(CE_CONT,  " 0x%x ", utab );
    utab = (struct iobuf *)utab->b_forw;
  }
  cmn_err(CE_CONT,"Free SG entry hd: 0x%x, buf_ext_hd 0x%x\n",
	  c->dkvjsg_hd, c->buf_ext_hd);
  dp = & c->c_disk_hd;
  cmn_err(CE_CONT,"I/O bp(s) active in controller, b_forw 0x%x b_back 0x%x\n",
	  dp->b_forw, dp->b_back );
  if (bp = dp->b_forw) {
    while (bp) {
      buf_ext1 = (BUF_EXT*)bp->av_back;
      cmn_err(CE_CONT, "  0x%x addr 0x%x (len 0x%x) combine_bp 0x%x (ext 0x%x, sph 0x%x)\n",
	      bp, bp->b_dmaaddr, bp->b_dmalen, bp->av_forw, buf_ext1, buf_ext1->sph );
      bp = buf_ext1->ext_forw;
    }
  }
  cmn_err(CE_CONT,"\nEND dump\n");
  splx(s);
  
}

#endif DKVJ_DEBUG
static int
dkvj_format(un, fmi) 
register VJ_UNIT *un;
register struct fmt_map_info *fmi;
{
    register SCSI_MS_FORMAT *fmt;
    int status, *addr;

    status = 0;
    switch (fmi->fmi_action) {
	case FMI_MAP_TRACK:
	    if (fmi->fmi_intrlv == 0) {		/* list size */
		return(DIOC_BADSIZE);
	    }
#ifdef STANDALONE
                addr = (int *)align_malloc(fmi->fmi_intrlv,4);
#else STANDALONE
                addr = (int *)kern_malloc(fmi->fmi_intrlv);
#endif STANDALONE
	    un->un_reassign = (char *)K2_TO_K1(addr);
	    if(copyin(fmi->fmi_addr,(caddr_t)un->un_reassign,fmi->fmi_intrlv) < 0) {
		status = DIOC_EFAULT;
	    } else if (vjsplcmd(un,VJ_REASSIGN,0, fmi->fmi_intrlv,
			un->un_reassign,WAIT))
		status = DIOC_OPERR;
#ifndef STANDALONE
	    kern_free(addr);
#endif STANDALONE
	    break;
	case FMI_FORMAT_TRACK:
            fmt = (SCSI_MS_FORMAT *)K0_TO_K1(&un->un_msformat);
	    fmt->format_hdr.ps = 0;
	    fmt->tpz = fmi->fmi_tpz;
	    fmt->aspz = fmi->fmi_aspz;
	    fmt->atpv = fmi->fmi_atpv;
	    if (vjsplcmd(un,VJ_MODE_SELECT_CMD,PD_FORMAT,
				sizeof(SCSI_MS_FORMAT),fmt,WAIT)) {
		cmn_err(CE_WARN,
		    "DKVJ %d:%d: Format could not select format modes\n",
		    un->un_ctlr,un->un_slave);
		return(DIOC_OPERR);
	    }
	    if (vjsplcmd(un,VJ_FORMAT,0,fmi->fmi_intrlv,0,WAIT)) {
		cmn_err(CE_WARN,"DKVJ %d:%d: Could not format\n",
			un->un_ctlr,un->un_slave);
		return(DIOC_OPERR);
	    }
	    break;
	default:
	    status = DIOC_EINVAL;
    }
    return(status);
}
static int 
dkvj_dev_busy(dev)
dev_t dev;
{
    int partition = FS(dev);
    register i;
    VJ_UNIT *un;
    VJ_CTLR *c;
    int unit,ctlr;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    c = vjctlrs[ctlr];
    un = c->c_unit[unit];

    /*
     * For SCSI drives, only the entire disk can be formatted.
     * This means that if any other partition is open, or if the
     * open count for this partition is > 1, the disk must be
     * considered busy.
     */
    for(i = 0; i < 16; i++) {
        if(un->un_open[i] == 0)
                continue;
        if(i == partition && un->un_open[i] <= 1)
                continue;
        return(1);
    }

    /*
     * Also make sure that this disk doesn't contain the swap partition
     */
    if( (major(dev) == major(swapdev)) && (VJUNIT(dev) == VJUNIT(swapdev)))
	return(1);
    return(0);
}

static int
usepc8(inq)
SCSI_INQUIRY *inq;
{
    if (inq->ansi == 2)
        return(1);
    if (strncmp(inq->vendor_id,"CDC",3) == 0)
        return(0);
    return(1);
}
