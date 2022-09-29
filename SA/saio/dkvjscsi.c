#ident "$Header: dkvjscsi.c,v 1.25 90/11/14 17:30:29 srinath Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#define SCSI_BUSY	0x08
#define COMPILER_BUG
/***
 ***      vj.c    1.2     87/10/22
 ***
 *** Copyright (C) Interphase Corporation - Dallas, TX 75229
 ***        Author: Keith Wiles
 ***        Date:    June 10, 1987
 ***
 ***
 *** Features:
 ***        * Multi-controller support for disk type devices.
 ***        * Supports up to 14 devices (or 7 per SCSI bus) per controller
 ***        * Variable number of devices per controller
 ***        * Drive types & geometries can be mixed on same controller
 ***        * Separate interrupt handlers (error, normal, status change)
 ***
 ***    NOTE:
 ***        The number of drives for a controller can be less than 14.
 ***        ALSO: since the number of drives for a controller can
 ***        vary, there is no simple calculation that can place a
 ***        dev entry with a given controller and drive. The user
 ***        will have to examine his config file. Hence, it may
 ***        be desirable to fix the number of drives per controller
 ***        in certain systems.
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
 *** BUGS:
 ***        LUNs are NOT supported
 ***        Status change interrupts are not supported
 ***
 ***/

#include "sys/errno.h"
#ifndef STANDALONE
#include "sys/types.h"
#endif STANDALONE
#include "sys/param.h"
#ifndef STANDALONE
#include "sys/systm.h"
#include "sys/sysmacros.h"
#include "sys/pcb.h"
#endif STANDALONE
#include "sys/buf.h"
#ifndef STANDALONE
#include "sys/file.h"
#include "sys/conf.h"
#endif STANDALONE
#include "sys/dir.h"
#ifdef STANDALONE
#include "sys/inode.h"
#include "sys/cmn_err.h"
#include "machine/cpu.h"
#include "mipsvme/vmereg.h"
#include "machine/dvh.h"
#else STANDALONE
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/map.h"
#include "sys/immu.h"
#include "sys/sbd.h"
#include "sys/edt.h"
#include "sys/ioctl.h"
#include "sys/mtio.h"
#include "sys/vmereg.h"
#include "sys/debug.h"
#include "sys/dvh.h"
#include "sys/dkio.h"
#endif STANDALONE

#ifdef STANDALONE
/* need no use files from uts/mips/sys */
#include "mipsvme/dkvj_IPtypes.h"
#include "mipsvme/dkvj_struct.h"
#include "mipsvme/dkvj_scsi.h"
#include "mipsvme/dkvj_reg.h"
#else STANDALONE
#include "sys/dkvj_IPtypes.h"
#include "sys/dkvj_struct.h"
#include "sys/dkvj_scsi.h"
#include "sys/dkvj_reg.h"
#endif STANDALONE

#include "mips/cpu_board.h"

#ifdef STANDALONE
#include "saio/saio.h"
#include "saio/saioctl.h"

static showconfig = 1;	/* Always print messages in standalone version */
int Nvjctlr = 8;
int ndkvj = 8 * 16;
int vjexterr;
/* XXX shouldn't this be 22,23,24...?*/
int dkvjmajors[] = {0, 1, 2, 3, 4, 5, 6, 7};
static char *vjstd[] = {(char *)0x9000, (char *)0x9800,
			(char *)0xa000, (char *)0xa800,
			(char *)0xb000, (char *)0xb800,
			(char *)0xc000, (char *)0xc800 };
VJ_MODE_SENSE1	mode_sense1;		/* Error page for sense and select */
char *vj_tpbuf;
char *temp_buffer;
int vj_didinit[8];
char vj_didmalloc;
csh_type ctlr_csh[8];

#define clean_cache	clear_page_cache

#define IOBADADDR badaddr

#else STANDALONE

#include "sys/dump.h"
#include "sys/gen_ioctl.h"
#include "sys/bsd_glue.h"

static void disable_controller();
void vjgetvolume();
void vjtimeout();
int vjSGsetup();

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
extern int vjexterr;
extern int dkvjmajors[];

#endif STANDALONE

#define K2_TO_PHYS(x)	(ctob(kvtokptbl((unsigned)x)->pgm.pg_pfn) |\
			(((unsigned)x) & PGOFSET))
#define K2_TO_K1(x)	((unsigned)K2_TO_PHYS(x) | 0xa0000000)
#define K2_TO_K0(x)	((unsigned)K2_TO_PHYS(x) | 0x80000000)

VJ_CTLR *vjctlrs[8];
int dkvj_inopen;

/* Local routines */

static void add_motor_start();

int powerup_time = 180;         /* 3 min max wait time */

/* This is the head of the queue for getting the units powered up */

struct motor_start {
	VJ_UNIT *active;
	VJ_UNIT *next;
} motor_start_list;

char scsi_cmd[] = {
    0,
    SCSI_TEST_UNIT_READY,
    SCSI_REZERO_UNIT,
    SCSI_REQUEST_SENSE,
    SCSI_READ,
    SCSI_WRITE,
    SCSI_SEEK_EXTENDED,
    SCSI_FORMAT_UNIT,
    SCSI_REWIND,
    SCSI_WRITE_FILE_MARKS,
    SCSI_SPACE_FM,
    SCSI_INQUIRY,
    SCSI_MODE_SENSE,
    SCSI_MODE_SELECT,
    SCSI_READ_BLOCK_LIMITS,
    SCSI_REASSIGN_BLOCK,
    SCSI_LOAD,
    0,
    0,
    0,
    0
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
    CNTR_FLUSH_WORKQ
};

#ifndef STANDALONE
SCSI_ERRORS scsi_err[] = {
    { 0x00, "Good Status" },
    { 0x02, "Request Sense needed (Check condition)."},
    { 0xFF, "Unknown Error." }
};
#endif STANDALONE

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
    { 0x34, "SCSI transfer count did not match the count given." },
    { 0x40, "Illegal (odd) count specified in S/G list." },
    { 0x41, "Invalid VME memory type in S/G list." },
    { 0x42, "Invalid DMA Address in S/G list." },
    { 0x80, "Flush on Error in Progress." },
    { 0x81, "Flush Work Queue status." },
    { 0xFF, "Unknown Error." }
};

SENSE_KEY_DEFS Sense_keys[] = {
    { SCSI_NO_SENSE,		"No Sense Data was available" },
    { SCSI_RECOVERABLE_ERROR,	"command completed with recovery actions" },
    { SCSI_NOT_READY,		"drive can't be accessed" },
    { SCSI_MEDIUM_ERROR,	"Non-recoverable data error" },
    { SCSI_HARDWARE_ERROR,	"non-recoverable hardware failure (parity, etc)" },
    { SCSI_ILLEGAL_REQUEST,	"Illegal parameter in cdb" },
    { SCSI_UNIT_ATTENTION,	"media change or drive was reset" },
    { SCSI_DATA_PROTECT,	"cartridge is write-protected" },
    { SCSI_BLANK_CHECK,		"no-data condition encountered on tape or tape is QIC-120 and drive is QIC-24" },
    { SCSI_ABORT_COMMAND,	"drive aborted the command" },
    { SCSI_VOLUME_OVERFLOW,	"physical EOM reached with data still in buffer" },
    { 0xFF,			"status code not in table" }
};

#ifndef STANDALONE
void
scsistr(inquiry, pp)
VJ_INQUIRY *inquiry;
register char *pp;
{
    register i;

    for(i=0; i<8; i++)
	*pp++ = inquiry->vendor_id[i];
    *pp++ = '-';
    for(i=0; i<16; i++)
	*pp++ = inquiry->product_id[i];
    *pp++ = '-';
    for(i=0; i<4; i++)
	*pp++ = inquiry->revision_level[i];
    *pp = NULL;
}
#endif STANDALONE


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

clear_page_cache(dmaaddr, dmasize)
ulong dmaaddr;
ulong dmasize;
{
      ulong bcount;
      ulong nbpc;

      if(!IS_KSEG1(dmaaddr))
      {
      if(IS_R6300)
	 nbpc = 16384;
      else
	 nbpc = 4096;

      while (dmasize > 0) {
       bcount = _min(nbpc-(dmaaddr & (nbpc-1)), dmasize); /* restrict to page */
       clear_cache( dmaaddr, bcount );
       dmasize -= bcount;
       dmaaddr += bcount;
      }
      }
}
_dkvjinit()
{
    register int i;
    /* 
     * Just zero out any data that's necessary if memory didn't get
     * zeroed on reset
     */
    for( i = 0; i < 8; ++i)
        vj_didinit[i] = 0;
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
    register VJ_UNIT *un = 0;
    register VJ_SHIO *shio;
    volatile VJ_MSR *msr;
    volatile VJ_MCR *mcr;
    VJ_CSB  Csb;
    char *addr;
    register INT i;

    c    = vjctlrs[ctlr];
    shio = c->c_io;
    msr = &shio->sh_MCSB.mcsb_MSR;
    mcr = &shio->sh_MCSB.mcsb_MCR;

    i = 50000;                                    /* Wait for Board OK */
    while(!(WORDP(msr) & M_MSR_BOK) && i--)
        DELAY(512);
    if (i <= 0)
        cmn_err(CE_CONT,"DKVJ %d: POWER-ON DIAGNOSTICS FAILED!\n",ctlr);

    WORDP(mcr) |= M_MCR_RES;  wbflush();  /* Reset the controller */
    WORDP(msr) = 0;  wbflush();		/* clear BOK bit */

    DELAY(128);

    WORDP(mcr) &= ~M_MCR_RES;  wbflush(); /* Clear the controller */           

    DELAY(128);		/* wait for BOK to leave after reset */

    i = 50000;                    /* wait for BOK bit to show up */
    while (!(WORDP(msr) & M_MSR_BOK) && i--)
        DELAY(512);

    if (!(WORDP(msr) & M_MSR_BOK))
    {
        cmn_err(CE_CONT,
		"DKVJ %d: board failed powerup diagnostics MSR=%x\n",
		ctlr,WORDP(msr));
        return (FALSE);
    }

    clear_page_cache(&Csb, sizeof(VJ_CSB));
    vjfrom_shio(&shio->sh_CSS, &Csb, sizeof(VJ_CSB));


    if (showconfig)
	cmn_err(CE_CONT,
	    "Jaguar Version (%s-%x-%s) Date %s with %d Kbytes ram.\n",
            &Csb.csb_PCODE[0],
            Csb.csb_PVAR,
            &Csb.csb_FREV[0],
            &Csb.csb_FDATE[0],
            Csb.csb_BSIZE);

    /*
     * Clear all important areas of short I/O
     */
    vjzero_shio(&shio->sh_CQE[0],  (sizeof(VJ_CQE) * NUM_CQE));
    vjzero_shio(&shio->sh_IOPB[0], (sizeof(VJ_IOPB) * NUM_IOPB));
    vjzero_shio(&shio->sh_MCE_IOPB, sizeof(VJ_IOPB));
    vjzero_shio(&shio->sh_CIB,      sizeof(VJ_CIB));
    vjzero_shio(&shio->sh_HUS[0],   S_HUS_FREE);
    vjzero_shio(&shio->sh_CRB,      sizeof(VJ_CRB));
    vjzero_shio(&shio->sh_RET_IOPB, sizeof(VJ_IOPB));

    c->c_cqe_top    = &shio->sh_CQE[ 0 ];
    c->c_cqe_end    = &shio->sh_CQE[ NUM_CQE - 1 ];

    for (i =0; i< NUM_CQE; i++) {           /* setup iopb address in cqe's */
        shio->sh_CQE[i].cqe_IOPB_ADDR = ((INT)&shio->sh_IOPB[i] - (INT)shio);
        wbflush();
    }

    shio->sh_MCSB.mcsb_QHDP = ((INT)&shio->sh_CQE[0] - (INT)shio);    wbflush();

    if (c->c_firsttime == 0)
    {
#ifdef STANDALONE
	if (addr = (char *)align_malloc(sizeof(VJ_UNIT), 4)) {
	    un = (VJ_UNIT *)addr;
	    bzero(un,sizeof(VJ_UNIT));
	}
#else  STANDALONE
	if (addr = kern_calloc(sizeof(VJ_UNIT),1))
	    un = (VJ_UNIT *)K2_TO_K0(addr);
#endif STANDALONE
	if (!un)
	    return;
        c->c_firsttime = 1;
	c->c_present = TRUE;
	c->c_ctlr = un->un_ctlr = ctlr;
     
        clear_page_cache(&Csb, sizeof(VJ_CSB));
      
	/* Initialize Controller Command Format */
        vjcmd(un, VJ_CNTR_INIT, (char *)0, 0, 0, NO_INTERRUPT);
        if (vjwait(c, M_CRSW_CC)) {
	    c->c_present = FALSE;
	} else {
	    vjfrom_shio(&shio->sh_CSS, &Csb, sizeof(VJ_CSB));


	    c->c_pid = Csb.csb_PID & 0x07;
	    c->c_sid = (Csb.csb_SID & 0x07) | 0x08;
	    /* END Initialize Controller Command Format */

#ifndef STANDALONE
	    un->un_unit.U.b.BUS = 1;
	    vjcmd(un, VJ_SCSI_RESET, 0, 0, 0, NO_INTERRUPT);
	    if (vjwait(c, M_CRSW_CC)) {
		c->c_maxunit = 8;
	    } else {
		c->c_maxunit = 16;
		if( showconfig)
		    cmn_err(CE_CONT,"    Daughterboard controller present\n");
	    }
	    /*     Start Queued Mode */
	    W(shio->sh_MCSB.mcsb_MCR) |= M_MCR_SQM;  wbflush();
	    if (vjwait(c, M_CRSW_QMS))
	    {
		cmn_err(CE_CONT,"DKVJ %d: Unable to Start Queued Mode.\n",
			ctlr);
		c->c_present = FALSE;
	    }
	    /* END Start Queued Mode */
#else STANDALONE
	    c->c_maxunit = 8;
#endif STANDALONE
	}
#ifdef STANDALONE
	release(addr);
#else STANDALONE
	kern_free(addr);
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
    register VJ_CTLR *c = 0;
    register VJ_SHIO *shio;
    register VJ_HSB *hsb;           /* Host Semaphore Block      */
    int ctlr;
    char *addr;
#ifndef STANDALONE
    register INT i,j;

    init_malloc();

    ctlr = e->e_intr_info->v_unit;
    if (addr = kern_calloc(sizeof(VJ_CTLR),1))
	c = (VJ_CTLR *)K2_TO_K0(addr);
    vjctlrs[ctlr] = c;

#else STANDALONE

    if (!vj_didmalloc) {
	vj_initmem();
	vj_didmalloc = 1;
    }
    ctlr = io->i_ctlr;
    if( addr = (char *)align_malloc(sizeof(VJ_CTLR), 4)) {
        c = vjctlrs[ctlr] = (VJ_CTLR *)addr;
        bzero(c,sizeof(VJ_CTLR));
    }
#endif STANDALONE
    if (c == 0) {
	cmn_err(CE_CONT,"DKVJ %d: cannot allocate memory for controller\n",
		ctlr);
	return;
    }
    /* Store address to board   */
#ifdef STANDALONE
    c->c_io = (VJ_SHIO*)(IS_R6300 ?
		find_r6000_controller(vjstd[io->i_ctlr], 0, sizeof(short)) : 
		PHYS_TO_K1(VMESA16_TO_PHYS(vjstd[io->i_ctlr])));
    io->i_devaddr = (unsigned)c->c_io;
#else STANDALONE
    c->c_io = (VJ_SHIO*)(e->e_base);
#endif STANDALONE

    if (IOBADADDR(c->c_io, sizeof(short))) {
        if (showconfig)
	    	cmn_err(CE_CONT,"DKVJ %d: controller not available\n",ctlr);
#ifdef STANDALONE
	release(addr);
#else
	kern_free(addr);
#endif STANDALONE
	vjctlrs[ctlr] = 0;
        return;
    }
    if (showconfig)
	cmn_err(CE_CONT,"Interphase 4210 Jaguar controller @ (paddr=%x)\n", 
		c->c_io);
    shio    = c->c_io;
    c->c_firsttime = 0;
    hsb  = &shio->sh_HSB;

#ifndef STANDALONE
    /* set up for vectored interrupts, interrupt handler gets CTLR pointer */
    vj_ivec[0] = e->e_intr_info->v_vec;
    vj_ilev[0] = e->e_intr_info->v_brl;
    c->c_nintvec = (int)vj_ivec[0]; /* normal interrupt */
    c->c_eintvec = c->c_nintvec;        /* error interrupt  */
    c->c_qintvec = c->c_eintvec;        /* Queue entry Available */
    c->c_level   = (int)vj_ilev[0];
#endif STANDALONE

    /* reserve a Logical cache section in the GBA for use by the controller */
    if(ctlr_csh[ctlr] == 0){
       if(!vme_reserve_iomap(ctlr, c->c_io, 32, &ctlr_csh[ctlr], 0))
         cmn_err(CE_PANIC,"Couldn't allocate cache section in GBA !\n");
    }

    if (vjmce_init(ctlr))     /* returns true if passed */
    {
	hsb->hsb_INITQ = HOST_ID;  wbflush();
	hsb->hsb_WORKQ = 0;  wbflush();

#ifndef STANDALONE
	/* init Scatter/Gather Free list    */
	for(i = 0; i < (NUM_M_SG - 1); i++)
	{
	    c->dkvjsg_fentry[i].nxt = &c->dkvjsg_fentry[i+1];
	    for(j = 0; j < MACSI_SG; j++)
	    {
		c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
				 (TT_BLOCK << 2) | MEMTYPE; /* Block mode */
		c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
	    }
	}

	c->dkvjsg_fentry[i].nxt = (IPSG_FREE *) 0;/* end list   */
	c->dkvjsg_hd = &c->dkvjsg_fentry[0]; /* init head   */
	for(j = 0; j < MACSI_SG; j++)
	{
	    c->dkvjsg_fentry[i].ipsg[j].sg_meminfo =
				 (TT_BLOCK << 2) | MEMTYPE; /* Block mode */
	    c->dkvjsg_fentry[i].ipsg[j].sg_addrmod = VME_A32NPBMOD;
	}
	for (i = 0; i < c->c_maxunit; i++)    /* attach bus devices */
	    vjattach(i,ctlr);
#endif STANDALONE
    }
    clear_page_cache(c, sizeof(VJ_CTLR));
}

void
#ifdef STANDALONE
vjattach(io)
register struct iob *io;
{
    register int unit = io->i_unit;
    register int ctlr = io->i_ctlr;
#else STANDALONE
vjattach(unit,ctlr)
register int unit, ctlr;
{
#endif STANDALONE
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register VJ_UNIT *un = 0;
    register int workq;
    VJ_INQUIRY	*inquiry;
    char *addr, product[32];
    sah_type  temp_sph;
    ioaddr_t  io_addr;

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
    if (workq < MAX_WORK_QUEUES)
    {
#ifdef STANDALONE
	if (addr = (char *)align_malloc(sizeof(VJ_UNIT), 4)){
	    un = (VJ_UNIT *)addr;
	    bzero(un,sizeof(VJ_UNIT));
	}
#else  STANDALONE
	if (addr = kern_calloc(sizeof(VJ_UNIT),1))
	    un = (VJ_UNIT *)K2_TO_K0(addr);
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
	un->un_workq = workq;

	if(!vme_iomap(ctlr_csh[ctlr],un,sizeof(VJ_UNIT), 
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &un->un_sph, &io_addr))
	  cmn_err(CE_PANIC, "Can't map unit structure !\n");

	clear_page_cache(un, sizeof(VJ_UNIT));
        if (vjsense(c, un, NO_INTERRUPT))
		goto attach_fail;

#ifndef STANDALONE
        vjcmd(un, VJ_INIT_WORKQ, (char *)0, 0, 0, NO_INTERRUPT);
        if (vjwait(c, M_CRSW_CC)) 
		goto attach_fail;
	inquiry = (VJ_INQUIRY *)K0_TO_K1(&un->un_inquiry);
#else STANDALONE
	inquiry = &un->un_inquiry;
#endif STANDALONE

	/* 
	 * Map inquiry into system space and obtain GBA bus address usable
	 * by the controller. Let the mapping be contiguous.
	 */

	if(!vme_iomap(ctlr_csh[ctlr],inquiry,sizeof(VJ_INQUIRY), 
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
	  cmn_err(CE_PANIC, "Can't map inquiry structure !\n");

	clear_page_cache(inquiry, sizeof(VJ_INQUIRY));

	vjcmd(un, VJ_INQUIRY_CMD, io_addr, 0, 1, NO_INTERRUPT);

	DELAY(512);

	if (vjwait(c, M_CRSW_CC))
	{
	    cmn_err(CE_CONT,"DKVJ %d:%d INQUIRY COMMAND FAILED\n",ctlr,unit);
attach_fail:
#ifdef STANDALONE
	    release(addr);
#else STANDALONE
	    kern_free(addr);
#endif STANDALONE
	    c->c_unit[unit] = 0;
	    return;
	}
	
	clear_page_cache(inquiry, sizeof(VJ_INQUIRY));
	
	/*
	 * By convention, we just flush GBA cache at
         * completion of each I/O operation.  This implies
         * GBA cache is flushed at I/O initiation.
         */

	   if(!vme_iounmap( temp_sph ))
		cmn_err(CE_PANIC, "Can't flush/unmap inquiry!\n");


#ifndef STANDALONE
	scsistr(inquiry,product);
	if (showconfig)
	    cmn_err(CE_CONT,"Work Queue %d for device '%s'\n", workq, product);
#endif STANDALONE
	switch ((int)inquiry->device_type) {
		case SCSI_TYPE_DISK:
			un->un_flags |= IVJ_DISK;
#ifndef STANDALONE
			if (inquiry->rm)
				un->un_flags |= IVJ_RMV_MEDIA;
			vjgetvolume(un, NO_INTERRUPT);
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
	c->c_unit[unit] = un;
	un->un_flags |= IVJ_ALIVE;
    }
}

void
vjgetvolume(un, mode)
register VJ_UNIT *un;
int mode;
{
    DVH *vh;
    VJ_CTLR *c;
    sah_type temp_sph;
    ioaddr_t io_addr;
    int status;
    register SCSI_EXT_SENSE *sense;

#ifdef STANDALONE
    vh = (DVH *)&temp_buffer[0];
#else STANDALONE
    vh = (DVH *)K0_TO_K1(&temp_buffer[0]);
#endif STANDALONE

    un->un_xfer = 512;
    un->un_resid = 0;

    c = vjctlrs[un->un_ctlr];
    sense = (SCSI_EXT_SENSE *)&un->un_sense;
    bzero(sense, sizeof(SCSI_EXT_SENSE));
    vjcmd(un, VJ_UNIT_READY, 0, 0, 0, NO_INTERRUPT);
    if (!(status = vjwait(vjctlrs[un->un_ctlr], M_CRSW_CC))) {
	/* good status! is he powered up */
	if(sense->key == SCSI_NOT_READY){
	    /* check once again */
            bzero(sense, sizeof(SCSI_EXT_SENSE));
            vjcmd(un, VJ_UNIT_READY, 0, 0, 0, NO_INTERRUPT);
	    if (!(status = vjwait(vjctlrs[un->un_ctlr],M_CRSW_CC))){
	       if(sense->key == SCSI_NOT_READY)
		  add_motor_start(un,mode);
	       else /* he is ready this time */
		  un->un_flags |= IVJ_READY;
	    }  else { /* bad status the second time */
		  cmn_err(CE_CONT,"vjgetvolume(%d:%d): failed , status %x sense  key is %s\n",un->un_ctlr, un->un_slave,status&0xff, sense_err_msg(sense->key));
		  return;
	    }
       }
       else /* looks like he is all powered up and ready to go */
	    un->un_flags |= IVJ_READY;
    } else {  /* bad status */
		cmn_err(CE_CONT,"vjgetvolume(%d:%d): failed , status %x sense key is %s\n",un->un_ctlr, un->un_slave,
		status&0xff, sense_err_msg(sense->key));
                return;
    }
    if((un->un_flags & IVJ_READY)) {

    if(!vme_iomap(ctlr_csh[un->un_ctlr], vh, sizeof(DVH),
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
	cmn_err(CE_PANIC, "Can't map vh structure!\n");

    clear_page_cache(vh, sizeof(DVH));

    vjcmd(un, VJ_READ, io_addr, 0, 1, NO_INTERRUPT);

    if (vjwait(vjctlrs[un->un_ctlr], M_CRSW_CC)) 
	    return;
    
    clear_page_cache(vh, sizeof(DVH));
	
    /* flush the gba cache and unmap registers */
             if(!vme_iounmap( temp_sph ))
	       cmn_err(CE_PANIC, "Can't flush/unmap vh!\n");


    if (is_vh(vh) == FALSE)
    {
	cmn_err(CE_CONT,"DKVJ %d:%d Volume Header is incorrect\n",
		un->un_ctlr,un->un_slave);
	un->un_vhvalid = 0;
	return;
    }
    un->un_vhvalid = 1;
    bcopy(vh, &un->un_vh, sizeof(DVH));
    }
}

static void
add_motor_start(un,mode)
register VJ_UNIT *un;
int mode;
{
    register SCSI_EXT_SENSE *sense;
    int status;
    register int s;

    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    bzero(sense, sizeof(SCSI_EXT_SENSE));
    vjcmd(un,VJ_LOAD,0,0,0,NO_INTERRUPT);
    if(status = vjwait(vjctlrs[un->un_ctlr], M_CRSW_CC)) {
	cmn_err(CE_CONT,"Cannot start motor (%d:%d): fail,status %x sense key is %s\n", un->un_ctlr,un->un_slave, status&0xff,
	sense_err_msg(sense->key));
    } else {
        un->un_flags |= IVJ_READY;
    }
}


int
vjsense(c,un,flag)
VJ_CTLR *c;
VJ_UNIT *un;
LONG flag;
{
    register VJ_SHIO *shio;
    ULONG i;
    SCSI_EXT_SENSE *sense;
    char is_a_tape;
    register char status;
    ioaddr_t  io_addr;

    shio = c->c_io;
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
	if(!un->un_sph)
	if(!vme_iomap(ctlr_csh[un->un_ctlr], un, sizeof(VJ_UNIT),
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &un->un_sph, &io_addr))
	  cmn_err(CE_PANIC, "Can't map UNIT structure! \n");
        if (!ka_to_vmeaddr(un->un_sph, sense, &io_addr ))
		  cmn_err(CE_PANIC, "Can't get addr for sense structure!\n");

	bzero(sense, sizeof(SCSI_EXT_SENSE));
	clear_page_cache(sense , sizeof(SCSI_EXT_SENSE));		

	vjcmd(un, VJ_SENSE, io_addr , 0, 1, flag);
#ifndef STANDALONE
	if (flag != NO_INTERRUPT)
		return(0);   /* interrupt routine checks status */
#endif STANDALONE
	status = vjwait(c, M_CRSW_CC);
	
	clear_page_cache(sense , sizeof(SCSI_EXT_SENSE));		
	
		if(!vme_ioflush(un->un_sph,0,0))
		cmn_err(CE_PANIC, "Can't flush/unmap sense!\n");
	
	if(is_a_tape && (sense->filmrk || sense->eom || sense->ilength))
		return(0);
	if (!status || (status & 0xff) == SELECT_TIMEOUT)
		break;
    }
    return(status);
}

vjwait(c, mask)
register VJ_CTLR *c;
register UWORD mask;
{
    register VJ_SHIO *shio;
    volatile VJ_CRB *crb;
    volatile VJ_IOPB *iopb;
    register VJ_UNIT *un;
    SCSI_CDB *cdb;
    register UWORD crsw;
    register int count, rc, status;

    shio = c->c_io;
    crb   = &shio->sh_CRB;
    iopb = &shio->sh_RET_IOPB;
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];
    un = (VJ_UNIT *)c->c_mce.cqe_CTAG;

    DELAY(4096);
    count = 1000000;

  
    while(!((crsw = W(crb->crb_CRSW)) & M_CRSW_CRBV) && count--)
    {
#ifdef STANDALONE
	_scandevs();	 /* bounce the LEDs and scan for abort */ 
#endif STANDALONE
        DELAY(1024);
    }

    rc = 0;
    if (count <= 0)
    {
        rc = 1;
        cmn_err(CE_CONT,"DKVJ %d:%d CRSW(0x%x) CRBV bit not found.\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW));
    }
    else if ((W(crb->crb_CRSW) & mask) != mask)
    {
        rc = 1;
        cmn_err(CE_CONT,"DKVJ %d:%d CRSW(0x%x) mask=%x\n",
		un->un_ctlr,un->un_slave,W(crb->crb_CRSW),mask);
    }

    if (cdb->cmd == SCSI_REQUEST_SENSE) {

	if (vjsense_status(un,iopb->iopb_STATUS))
	    rc = 1;
	else
	    rc = 0;
	goto waitdone;
    }
    if (W(crb->crb_CRSW) & M_CRSW_ER && rc == 0)
    {
	if (((iopb->iopb_STATUS >> 8) & 0xff) == CHECK_CONDITION) {
	    CRB_CLR_DONE(crb->crb_CRSW);
	    if (iopb->iopb_STATUS = vjsense(c,un,NO_INTERRUPT))
		rc = 1;
	    goto waitdone;
	}
	status = iopb->iopb_STATUS & 0xff;
	if (status)
	{
	    if (status == TRANSFER_COUNT_ERROR ||
		(status == BUS_RESET_STATUS && iopb->iopb_CMD == SCSI_RESET))
		rc = 0;
	    else
	    {
		rc = 1;
#ifndef STANDALONE
		if (status != NO_SECOND_PORT && status != SELECT_TIMEOUT)
#endif STANDALONE
		  cmn_err(CE_CONT,
	  "DKVJ %d:%d Error: Command = 0x%x(%x) CRSW(0x%x) status = 0x%x\n",
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
 *
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
    int s;
#endif STANDALONE
    VJ_CQE  *mce;                   /* Master Controller Entry   */
    VJ_CIB  *cib;
    SCSI_CDB *cdb;
    ioaddr_t io_addr;
    sah_type temp_sph;

    c = vjctlrs[un->un_ctlr];
    shio = c->c_io;
#ifndef STANDALONE
    if (W_QECR(shio->sh_MCE.cqe_QECR) & M_QECR_GO) {
	cmn_err(CE_WARN,"DKVJ %d: Master work queue not empty, cmd=0x%x\n",
		un->un_ctlr,cmd);
	disable_controller(c);
	return;
    }
#endif STANDALONE
    mce     = &c->c_mce;
    bzero(mce, sizeof(VJ_CQE));
    if (cmd == VJ_CNTR_INIT)
    {
        iopb                    = &c->c_miopb;
        cib                     = &c->c_cib;

        bzero(iopb, sizeof(VJ_IOPB));
        bzero(cib, sizeof(VJ_CIB));

        c->c_mce_iopb           = (VJ_IOPB *)&shio->sh_MCE_IOPB;

        mce->cqe_IOPB_ADDR      = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_IOPB_LENGTH    = sizeof(VJ_IOPB)/4;

        iopb->iopb_CMD          = CNTR_INIT;
        W(iopb->iopb_ADDR)      = SHIO_MOD;
        iopb->iopb_BUFF         = (UWORD)((INT)&shio->sh_CIB - (INT)shio);
        iopb->iopb_LENGTH       = S_CIB;

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
        cib->cib_CRBO           = (UWORD)((INT)&shio->sh_CRB - (INT)shio);

        vjto_shio(iopb, c->c_mce_iopb, sizeof(VJ_IOPB));
        vjto_shio(cib,  &shio->sh_CIB,      sizeof(VJ_CIB));
    }
#ifndef STANDALONE
    else if (cmd == VJ_INIT_WORKQ)
    {
        wqcf                        = &c->c_wqcf;

        bzero(wqcf, sizeof(VJ_WQCF));

        mce->cqe_IOPB_ADDR          = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG               = (ULONG)un;
        mce->cqe_IOPB_LENGTH        = sizeof(VJ_WQCF)/4;

        wqcf->wqcf_CMD              = CNTR_INIT_WORKQ;
        wqcf->wqcf_NVCT             = c->c_nintvec;
        wqcf->wqcf_EVCT             = c->c_eintvec;
        wqcf->wqcf_ILVL             = c->c_level;

        wqcf->wqcf_WORKQ            = un->un_workq;

        W(wqcf->wqcf_WOPT)        = M_WOPT_FE;        /* Freeze on error */

        wqcf->wqcf_SLOTS            = VJ_DISK_SLOTS + 1;
        wqcf->wqcf_PRIORITY         = VJ_DISK_PRIORITY;

        vjto_shio(wqcf, c->c_mce_iopb, sizeof(VJ_WQCF));
    }
    else if (cmd == VJ_SCSI_RESET)
    {
        rescf                = (VJ_RESCF *)&c->c_wqcf;

        bzero(rescf, sizeof(VJ_RESCF));

        mce->cqe_IOPB_ADDR   = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG        = (ULONG)un;
        mce->cqe_IOPB_LENGTH = sizeof(VJ_RESCF)/4;

        rescf->rescf_CMD     = SCSI_RESET;
        rescf->rescf_NVCT    = c->c_nintvec;
        rescf->rescf_EVCT    = c->c_eintvec;
        rescf->rescf_ILVL    = c->c_level;

	if (flag == WANT_INTERRUPT)
		W(rescf->rescf_OPTION) = M_OPT_IE;        /* interrupt enable */
        rescf->rescf_BUSID.U.b.BUS =  un->un_unit.U.b.BUS;

        vjto_shio(rescf, c->c_mce_iopb, sizeof(VJ_RESCF));
    }
    else if (cmd == VJ_FLUSH_WORKQ)
    {
        fqcf                        = (VJ_FQCF *)&c->c_wqcf;

        bzero(fqcf, sizeof(VJ_FQCF));

        mce->cqe_IOPB_ADDR          = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
        mce->cqe_CTAG               = (ULONG)un;
        mce->cqe_IOPB_LENGTH        = sizeof(VJ_FQCF)/4;

        fqcf->fqcf_CMD              = CNTR_FLUSH_WORKQ;
        fqcf->fqcf_NVCT             = c->c_nintvec;
        fqcf->fqcf_EVCT             = c->c_eintvec;
        fqcf->fqcf_ILVL             = c->c_level;

        fqcf->fqcf_WORKQ            = un->un_workq;

        W(fqcf->fqcf_OPTION)      = M_FOPT_IE | M_FOPT_RPT;

        vjto_shio(fqcf, c->c_mce_iopb, sizeof(VJ_FQCF));
    }
#endif STANDALONE
    else                /* Pass thru command      */
    {
        iopb                    = &c->c_miopb;
        cdb                     = (SCSI_CDB *)&iopb->iopb_SCSI[0];

        bzero(iopb, sizeof(VJ_IOPB));

        mce->cqe_IOPB_ADDR      = (UWORD)((INT)c->c_mce_iopb - (INT)shio);
	mce->cqe_CTAG           = (ULONG)un;
        mce->cqe_IOPB_LENGTH    = sizeof(VJ_IOPB)/4;

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
        if (cmd == VJ_WRITE || cmd == VJ_MODE_SELECT_CMD || cmd == VJ_REASSIGN)
            W(iopb->iopb_OPTION)  |= M_OPT_DIR;

        iopb->iopb_CMD          = macsi_cmd[ cmd ];

        W(iopb->iopb_UNIT)      = un->un_unit.U.w;

        cdb->cmd                = scsi_cmd[ cmd ];
        cdb->lun                = 0;
	switch(cdb->cmd)
	{
		case SCSI_LOAD:
			if(un->un_flags & IVJ_DISK) { /* start/stop command */
				cdb->high_addr = block; /* set Immediate bit */
				cdb->count = 1;     /* set start bit */
			}
			break;
		case SCSI_REQUEST_SENSE:
			CDB_ADDR(cdb,block);
			cdb->count = SENSE_LENGTH;
			iopb->iopb_LENGTH = sizeof(SCSI_EXT_SENSE);
			break;
		case SCSI_INQUIRY:
			cdb->count = sizeof(VJ_INQUIRY);
			iopb->iopb_LENGTH = sizeof(VJ_INQUIRY);
			break;
		case SCSI_SPACE_FM:
			cdb->high_addr = block;  /* code field */
			CDB_XFER_LEN(cdb,count);
			break;
		case SCSI_WRITE_FILE_MARKS:
			CDB_XFER_LEN(cdb,count);
			break;
		case SCSI_REASSIGN_BLOCK:
			iopb->iopb_LENGTH = count;
			break;
#ifdef STANDALONE
#ifndef PROM
		case SCSI_READ_BLOCK_LIMITS:
			cdb->count = 0;
			if(!un->un_sph)
			if(!vme_iomap(ctlr_csh[un->un_ctlr], un, 
			sizeof(VJ_UNIT), GBA_CONTIG_ADDR+GBA_NOPART_MAP, 
			&un->un_sph, &io_addr))
	  		cmn_err(CE_PANIC, "Can't map UNIT structure! \n");
        		if (!ka_to_vmeaddr(un->un_sph, &un->un_blklim, 
			&io_addr ))
		  	cmn_err(CE_PANIC, "Can't get addr for sense structure!\n");
			clear_page_cache(&un->un_blklim, 6);

			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = 6;
			break;
		case SCSI_MODE_SENSE:
			if(!un->un_sph)
			if(!vme_iomap(ctlr_csh[un->un_ctlr], un, 
			sizeof(VJ_UNIT), GBA_CONTIG_ADDR+GBA_NOPART_MAP, 
			&un->un_sph, &io_addr))
	  		cmn_err(CE_PANIC, "Can't map UNIT structure! \n");
        		if (!ka_to_vmeaddr(un->un_sph, &un->un_msense, 
			&io_addr ))
		  	cmn_err(CE_PANIC, "Can't get addr for msense structure!\n");
			clear_page_cache(&un->un_msense,sizeof(VJ_MODE_SENSE));

			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = sizeof(VJ_MODE_SENSE);
			cdb->count = sizeof(VJ_MODE_SENSE);
			break;
#endif PROM
		case SCSI_MODE_SELECT:
			if(!un->un_sph)
			if(!vme_iomap(ctlr_csh[un->un_ctlr], un, 
			sizeof(VJ_UNIT), GBA_CONTIG_ADDR+GBA_NOPART_MAP, 
			&un->un_sph, &io_addr))
	  		cmn_err(CE_PANIC, "Can't map UNIT structure! \n");
        		if (!ka_to_vmeaddr(un->un_sph, &un->un_msense, 
			&io_addr ))
		  	cmn_err(CE_PANIC, "Can't get addr for msense structure!\n");
			clear_page_cache(&un->un_msense,sizeof(VJ_MODE_SENSE));

			iopb->iopb_BUFF = (ULONG)io_addr;
			iopb->iopb_LENGTH = sizeof(VJ_MODE_SENSE);
			cdb->count = sizeof(VJ_MODE_SENSE);
			break;
#endif STANDALONE
		case SCSI_FORMAT_UNIT:
			if (count) {
			    iopb->iopb_LENGTH = count;
			    W (iopb->iopb_OPTION) |= M_OPT_DIR;
			}
			cdb->high_addr = (((ULONG)block) >> 24) & 0xff;
			cdb->mid_addr = (((ULONG)block) >> 16) & 0xff;
			cdb->low_addr = (((ULONG)block) >> 8) & 0xff;
			cdb->count = ((ULONG)block) & 0xff;
			break;
		case SCSI_READ:
		case SCSI_WRITE:
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
	    un->un_timeid = timeout(vjtimeout,un,TIME_RDWR);
	}
	splx(s);
    }
    if (un->un_aborta) {
        CQE_AA_GO(shio->sh_MCE.cqe_QECR);
    }
    else 
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

    if( ctlr >= Nvjctlr){
	io->i_errno = ENODEV;
	return(-1);
    }
    dkvj_inopen = 1;
    if(!vj_didinit[ctlr]){
	dkvjinit(io);
	vj_didinit[ctlr] = 1;
    }

    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0) {
	printf("dkij(%d,%d,%d): no controller present\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
        dkvj_inopen = 0;
	return(-1);
    }
    un = c->c_unit[unit];

    if (un == 0) {
        vjattach(io);
	un = c->c_unit[unit];
    }

    if (!un ){
	printf("dkij(%d,%d,%d): no device attached\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
        dkvj_inopen = 0;
	return(-1);
    }

    if (!(un->un_flags & IVJ_DISK)) {
	printf("dkij(%d,%d,%d): not a disk device\n",ctlr,unit,io->i_part);
	io->i_errno = ENXIO;
        dkvj_inopen = 0;
	return(-1);
    }

    if (!un->un_vhvalid)
    {
	vjgetvolume(un, NO_INTERRUPT);

	if(!(un->un_flags & IVJ_READY)){
	   printf("dkij(%d,%d,%d): disk not ready\n", ctlr, unit, io->i_part);
	   io->i_errno = ENXIO;
	   dkvj_inopen = 0;
	   return(-1);
	}

	if (un->un_vh.vh_pt[io->i_part].pt_nblks == 0)
	{
		printf("dkij(%d,%d,%d): bad %s ZERO\n",
			ctlr,unit,io->i_part,"partition");
	}
    }
    if (io->i_fstype == DTFS_AUTO)
	    io->i_fstype= vh_mapfstype(un->un_vh.vh_pt[io->i_part].pt_type);
    un->un_flags |= IVJ_OPEN;	/* set opened flag */
    dkvj_inopen = 0;
    return (0);
}
#else STANDALONE
dkvjopen(dev, flag)
dev_t dev;
int flag;
{
    register VJ_CTLR *c;
    register VJ_UNIT *un;
    register VJ_MODE_SENSE *ms;
    register VJ_RDBLKLIM *rb;
    register int unit, ctlr;
    int s;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);

    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    un = c->c_unit[unit];

    if (un == 0) {
        vjattach(unit,ctlr);
	un = c->c_unit[unit];
    }
    if (!un || (un->un_flags & (IVJ_ALIVE | IVJ_READY)) == 0) {
	u.u_error = ENODEV;
	return;
    }
    if(un->un_flags & IVJ_TAPE)
    {
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
	if (vjsplcmd(un,VJ_UNIT_READY,0,0,WAIT)) {
		cmn_err(CE_CONT,
		"DKVJ %d:%d tape not ready; offline or tape not installed\n",
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	if (vjsplcmd(un,VJ_RDBLKLIM_CMD,0,0,WAIT)) {
		cmn_err(CE_CONT,
			"DKVJ %d:%d cannot read block limits\n",
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	if (vjsplcmd(un,VJ_MODE_SENSE_CMD,0,0,WAIT)) {
		cmn_err(CE_CONT,"DKVJ %d:%d error in MODE SENSE\n",
			un->un_ctlr,un->un_slave);
		goto openerr;
	}
	ms = (VJ_MODE_SENSE *)K0_TO_K1(&un->un_msense);
	rb = (VJ_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
	if (ms->hdr.WP) {
		if (flag & FWRITE) {
		    cmn_err(CE_CONT,"DKVJ %d:%d write protected\n",
			un->un_ctlr,un->un_slave);
		    goto openerr;
		}
		un->un_flags |= IVJ_READONLY;
	}
	else un->un_flags &= ~IVJ_READONLY;
	if (rb->maxlen != rb->minlen) {
		ms->hdr.sense_data_len = 0;
		ms->hdr.medium_type = 0;
		ms->hdr.WP = 0;
		ms->hdr.blk_desc_len = 8;
		ms->blk_desc.density_code = 0;
		ms->blk_desc.nrblks = 0;
		ms->blk_desc.blk_len = 0;
		if (vjsplcmd(un,VJ_MODE_SELECT_CMD,0,0,WAIT)) {
			cmn_err(CE_CONT,"DKVJ %d:%d error in MODE SELECT\n",
				un->un_ctlr,un->un_slave);
openerr:
			un->un_flags &= ~IVJ_OPEN;
			u.u_error = EIO;
			return;
		}
		un->un_flags |= IVJ_VARIABLE;
	}
	if (vjexterr && (un->un_flags & IVJ_ATN))
		cmn_err(CE_CONT,
		"DKVJ %d:%d unit attention; media change or drive reset\n",
			un->un_ctlr,un->un_slave);
    }else {
        if (!un->un_vhvalid && (LPART(dev) != MAGIC_DEV)){
		u.u_error = ENXIO;
	        return;
	}
	else if(LPART(dev) == MAGIC_DEV)
	{
		register struct partition_table *pt;

		pt = &un->un_vh.vh_pt[ MAGIC_DEV ];
		pt->pt_firstlbn = 0;
		pt->pt_nblks    = 900000;
	}
    }
}
#endif STANDALONE

#ifdef STANDALONE
_dkvjclose(io)
register struct iob *io;
{
    return (0);
}
#else STANDALONE
dkvjclose(dev, flag)
dev_t dev;
{
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register int unit, ctlr, numb;

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
	if (un->un_flags & IVJ_WRITTEN)
	{				
	    if (un->un_flags & IVJ_VARIABLE)
		numb = 2;
	    else
		numb = 1;
	    /* write file marks */
	    if (vjsplcmd(un, VJ_W_FM,0,numb,WAIT))
		    cmn_err(CE_CONT,"DKVJ %d:%d Error in writing file marks\n",
			un->un_ctlr,un->un_slave);

	    if ((un->un_flags & IVJ_VARIABLE) && NOREWIND(dev)){
		/* Backspace 1 (2's complement) */
		if (vjsplcmd(un, VJ_SPACE,SP_FILEMARK,~1L + 1,WAIT))
		    cmn_err(CE_CONT,"DKVJ %d:%d Error in bsf\n",
			un->un_ctlr,un->un_slave);
	    }
	}
	if(!NOREWIND(dev)) {
	    un->un_eomcount = 0;
	    un->un_flags |= IVJ_REWINDING;
	    un->un_flags &= ~(IVJ_FM|IVJ_EOM);
	    if (vjsplcmd(un, VJ_REWIND,0,0,NO_WAIT))
		cmn_err(CE_CONT,"DKVJ %d:%d Error in rewinding\n",
			un->un_ctlr,un->un_slave);
	}
    }
    un->un_flags &= ~(IVJ_OPEN|IVJ_WRITTEN|IVJ_READ|IVJ_ATN|IVJ_RFM|
			IVJ_NOT_RDY|IVJ_VARIABLE);
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
    ioaddr_t io_addr;
    sah_type temp_sph;

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

	    if(!vme_iomap(ctlr_csh[io->i_ctlr], io->i_ma,
	    io->i_cc,GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
	       cmn_err(CE_PANIC, "Can't map i_ma  structure !\n");
	    clear_page_cache(io->i_ma ,io->i_cc);

	    vjcmd(un, VJ_READ, (char *)io_addr, lbn, blks, NO_INTERRUPT);

	    rc = vjwait(c, M_CRSW_CC);
            
	       if(!vme_iounmap( temp_sph ))
	       cmn_err(CE_PANIC, "Can't flush/unmap i_ma !\n");


	    clear_page_cache(io->i_ma, io->i_cc);
	    break;
	case WRITE:
	    if (io->i_cc % DEV_BSIZE)
	    {
		printf("cc not multiple of sector size\n");
		io->i_errno = EIO;
		return(-1);
	    }
	    blks = io->i_cc /DEV_BSIZE;
	    if(!vme_iomap(ctlr_csh[io->i_ctlr], io->i_ma,
	    io->i_cc,GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
	       cmn_err(CE_PANIC, "Can't map i_ma  structure !\n");
	    clear_page_cache(io->i_ma ,io->i_cc);

	    vjcmd(un, VJ_WRITE, (char *)io_addr, lbn, blks, NO_INTERRUPT);

	    rc = vjwait(c, M_CRSW_CC);
            
	       if(!vme_iounmap( temp_sph ))
	       cmn_err(CE_PANIC, "Can't flush/unmap i_ma !\n");


	    break;
	default:
		_io_abort("dkvjscsi: bad function.");
		rc = 1;
		break;
    }
    if (rc)
    {
	    rc = -1;
	    io->i_errno = ENXIO;
    }
    return(io->i_cc - un->un_resid);
}
#else STANDALONE
dkvjstrategy(bp)
register BUF *bp;
{
    register struct partition_table *pt;
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register int unit, ctlr;
    register daddr_t bn;
    register VJ_DISK_HD *dp;
    VJ_RDBLKLIM *rb;
    int s;
    caddr_t *phys;

    unit = BPTOVJN(bp);
    BPTOVJC(ctlr,bp);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0 || 
	    (un = c->c_unit[unit]) ==0) {
	u.u_error = ENXIO;
	goto err;
    }
    rb = (VJ_RDBLKLIM *)K0_TO_K1(&un->un_blklim);
    if ((un->un_flags & (IVJ_ALIVE | IVJ_READY)) == 0) {
	bp->b_error = ENXIO;
	goto err;
    }
    if ((un->un_flags & IVJ_VARIABLE) && !(bp->b_flags & B_SPL)) {
	if (!(bp->b_flags & B_READ)) {
	    if (
		bp->b_bcount > (unsigned)rb->maxlen ||
		bp->b_bcount < (unsigned)rb->minlen) {
		cmn_err(CE_CONT,
		    "DKVJ %d:%d invalid block length request %d\n",
		    un->un_ctlr,un->un_slave,bp->b_bcount);
		bp->b_error = ENXIO;
		goto err;
	    }
	    if (un->un_eomcount == 1) {
		++un->un_eomcount;
		bp->b_error = ENOSPC;
		goto err;
	    }
	}
	if (un->un_eomcount > MAXEOM) {
	    bp->b_error = ENOSPC;
	    goto err;
	}
    }
    if (un->un_flags & IVJ_DISK){
	/*
	 * Check for some disk specific errors 
	 */
	if (un->un_vhvalid == 0)
	    goto err;
	bn = bp->b_blkno;
	pt = &un->un_vh.vh_pt[LPART(bp->b_dev)];
	if (bn < 0 || (bn + BTOBB(bp->b_bcount)) > pt->pt_nblks){
	    bp->b_error = ESPIPE;           /* illegal seek */
	    goto err;
	}else if (bn == pt->pt_nblks){     /* This is an EOF condition */
	    bp->b_resid = bp->b_bcount;
	    iounmap(bp);
	    iodone(bp);
	    return;
	} 
    } else if ((un->un_flags & IVJ_RFM) && !(bp->b_flags & B_SPL) && 
		(bp->b_flags & B_READ)) {
	un->un_flags &= ~IVJ_RFM;
	goto err1;
    }
    s  = splall();
    dp = &c->c_disk_hd;
    /* 
     * Don't call iomap, since not every single entry in *bp is set up.
     * Notably, iomap makes sure that b_dmaaddr is NULL.  
     */
    if(!(bp->b_flags & B_SPL))
        iomap(bp);
    bp->av_forw  = (BUF *)0;
    if (dp->av_forw == (BUF *)0)
        dp->av_forw          = bp;
    else
        dp->av_back->av_forw  = bp;
    dp->av_back  = bp;
    bp->av_back  = 0;    /* Zero out S/G FREE pointer */
    bp->b_resid = bp->b_bcount;
    vjgo(c);
    splx(s);
    return;
err:
    bp->b_flags |= B_ERROR;
err1:
    s  = splall();
    bp->b_resid = bp->b_bcount;     /* whole request failed */
    iounmap(bp);
    iodone(bp);
    if (bp->b_flags & B_SPL)
	bp->b_flags &= ~(B_BUSY | B_SPL);
    if (bp->b_flags & B_WANTED) {
	bp->b_flags &= ~B_WANTED;
	wakeup((caddr_t)bp);
    }
    splx(s);
}

/*
 * scan thru all of the bp's and send them to the board.
 */
vjgo(c)
register VJ_CTLR *c;
{
    register BUF *bp, *pbp, *actf;

    bp  = c->c_disk_hd.av_forw;
    pbp = (BUF *)&c->c_disk_hd;
    while(bp && !c->c_wait_cqe)
    {
        if (!c->dkvjsg_hd)
        {
            return;
        }
        actf = bp->av_forw;                     /* get forward link because  */
        if (!vjenq(bp))                         /* vjenq destroyes this link.*/
        {                                       /* Command was sent to board */
            pbp->av_forw = actf;                /* delete it from link list  */
            if (bp == c->c_disk_hd.av_back)     /* was it the last entry in  */
                c->c_disk_hd.av_back = pbp;     /* list then update last ptr */
        }
        else {                                  /* did not send command      */
            pbp = bp;                           /* prev bp is bp             */
	    break;				/* jag full so get out!      */
	}
        bp  = actf;                             /* get forward link of list  */
    }
}

/*
 * iopb que routine
 */
vjenq(bp)
register BUF *bp;
{
    register VJ_DISK_HD *dp;
    register VJ_CTLR *c;
    register VJ_SHIO *shio;
    register VJ_UNIT *un;
    register int bn, ctlr, unit, length, cmd;
    SCSI_CDB *cdb;
    VJ_CQE   *qhp;
    VJ_IOPB  *iopb;
    VJ_CQE   *cqe;
    VJ_CQE   Cqe;
    VJ_MCSB  *mcsb;
    int s;

    unit    = BPTOVJN(bp);
    BPTOVJC(ctlr,bp);
    c       = vjctlrs[ctlr];
    un      = c->c_unit[unit];
    if (!(un->un_flags & IVJ_ALIVE)) {
	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	bp->b_resid = bp->b_bcount;
	iounmap(bp);
	iodone(bp);
	if (bp->b_flags & B_SPL)
	    bp->b_flags &= ~(B_BUSY | B_SPL);
	if (bp->b_flags & B_WANTED) {
	    bp->b_flags &= B_WANTED;
	    wakeup((caddr_t)bp);
	}
	return(0);
    }
    dp = &c->c_disk_hd;
    shio    = c->c_io;

    mcsb    = &shio->sh_MCSB;

    cqe = (VJ_CQE *)((INT)mcsb->mcsb_QHDP + (INT)shio);
    if (W_QECR(cqe->cqe_QECR) & M_QECR_GO)
    {
        if (!c->c_wait_cqe)
        {
            W(shio->sh_MCSB.mcsb_IQAR)   = /* set IQAR BIT and wait */
                (M_IQAR_IQEA | M_IQAR_IQED | VEC(c,c->c_qintvec));
            wbflush();
            c->c_wait_cqe = TRUE;
        }
        return(1);
    }
    else
    {
        if (un->un_qcount >= VJ_Q_SIZ) /* 20 queed up!  what's the max number of cqe's possible? */
        {
/*          c->c_wait_cqe = TRUE;	why not do this here also?  don't use IQAR cause one is good! */
            return(1);
        } else {
	    /* save list of bp's sent to board */
	    bp->av_forw = (BUF *)0;
	    if (dp->b_forw == (BUF *)0)
		dp->b_forw = bp;
	    else
		dp->b_back->av_forw = bp;
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
		if (un->un_flags & IVJ_DISK)
			bn = bp->b_blkno + 
				un->un_vh.vh_pt[LPART(bp->b_dev)].pt_firstlbn;
	    }

            iopb                = &un->un_iopb;
            cdb                 = (SCSI_CDB *)&iopb->iopb_SCSI[0];

            bzero(cdb, sizeof(SCSI_CDB)); /* clean it out! */
         /* bzero(iopb, sizeof(VJ_IOPB)); unnecessary overhead! */

            iopb->iopb_CMD              = macsi_cmd[ cmd ];

            if (cmd == VJ_WRITE || cmd == VJ_MODE_SELECT_CMD) {
            	W(iopb->iopb_OPTION)  = (M_OPT_DIR + M_OPT_IE);
		wbflush();
            } else {
                W(iopb->iopb_OPTION)  = M_OPT_IE;
		wbflush();
	    }
            iopb->iopb_NVCT             = c->c_nintvec;
            iopb->iopb_EVCT             = c->c_eintvec;
            iopb->iopb_LEVEL            = c->c_level;
            W(iopb->iopb_ADDR)          = ADDR_MOD_B; /* BLOCK MODE */
            W(iopb->iopb_UNIT)          = un->un_slave;

            cdb->cmd                    = scsi_cmd[ cmd ];
            cdb->lun                    = 0;

            if (bp->b_flags & B_SPL) {
		    /* back down from BLOCK mode for any "specials" */
            	    W(iopb->iopb_ADDR) = ADDR_MOD; /* NORMAL MODE */
		    iopb->iopb_LENGTH = 0; /* SG can leave 'odd' link count! */
		    switch (cdb->cmd)
		    {
		    case SCSI_READ_BLOCK_LIMITS:
			cdb->count = 0;
			iopb->iopb_BUFF = (ULONG)K0_TO_PHYS(&un->un_blklim);
			iopb->iopb_LENGTH = 6;
			break;
		    case SCSI_MODE_SENSE:
			iopb->iopb_BUFF = (ULONG)K0_TO_PHYS(&un->un_msense);
			iopb->iopb_LENGTH = sizeof(VJ_MODE_SENSE);
			cdb->count = sizeof(VJ_MODE_SENSE);
			break;
		    case SCSI_MODE_SELECT:
			iopb->iopb_BUFF = (ULONG)K0_TO_PHYS(&un->un_msense);
			iopb->iopb_LENGTH = sizeof(VJ_MODE_SENSE);
			cdb->count = sizeof(VJ_MODE_SENSE);
			break;
		    case SCSI_WRITE_FILE_MARKS:
			CDB_XFER_LEN(cdb, bp->b_bcount);
			break;
		    case SCSI_SPACE_FM:
			cdb->high_addr = bp->b_blkno;	/* code field */
			CDB_XFER_LEN(cdb, bp->b_bcount);
			break;
		    case SCSI_LOAD:
			cdb->count = bp->b_blkno;
			break;
		    case SCSI_REWIND:
			break;
		    }
	    } else {
		    length = vjSGsetup(c, bp, iopb, un->un_flags&IVJ_VARIABLE);
		    if (!length)
			return(1);			/* check this out. what is the significance? */
		    if(un->un_flags & IVJ_TAPE)
		    {
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
            if (cqe >= c->c_cqe_end)
                mcsb->mcsb_QHDP =  ((INT)c->c_cqe_top - (INT)shio);
	    else
		mcsb->mcsb_QHDP =  ((INT)cqe + sizeof(VJ_CQE)) - (INT)shio;
	    wbflush();


	/* this looks screwy!!!! */
	/* WHY do we copy this from the board??????? */
	/* why not just directly write the cqe to the board??? */

            vjfrom_shio(cqe, &Cqe, sizeof(VJ_CQE));
            Cqe.cqe_CTAG        = (ULONG)bp;
            Cqe.cqe_IOPB_LENGTH = sizeof(VJ_IOPB)/4;
            Cqe.cqe_WORK_QUEUE  = un->un_workq;
            vjto_shio(&Cqe, cqe, sizeof(VJ_CQE));
            vjto_shio(iopb,
                (VJ_IOPB *)((INT)Cqe.cqe_IOPB_ADDR +
                (int)shio),sizeof(VJ_IOPB));

	    s = splclock();
	    un->un_qcount++;
	    if (un->un_timeid == 0) {
		if (un->un_qcount == 1 && un->un_timeout) {
			un->un_timeid = timeout(vjtimeout,un,un->un_timeout);
		} else {
			un->un_timeid = timeout(vjtimeout,un,TIME_RDWR);
		}
	    }
	    splx(s);
            CQE_GO(cqe->cqe_QECR);
        }
    }
    return(0);
}

dkvjintr(ctlr)
int ctlr;
{
    register USHORT crsw;
    register VJ_CTLR *c;

    c = vjctlrs[ctlr];
    if (!c->c_present) {
	cmn_err(CE_CONT,"DKVJ %d: Interrupt from non-existent board\n",ctlr);
	return;
    }
    crsw = W(c->c_io->sh_CRB.crb_CRSW);

    if (crsw & M_CRSW_CQA)
        vjqint(c);
    else
        vjnint(c);
}

/*
 * Queue Entry Available interrupt.
 */
vjqint(c)
register VJ_CTLR *c;
{
    register VJ_CRB *crb;

    crb = &c->c_io->sh_CRB;

    if (W(crb->crb_CRSW) & M_CRSW_CQA)
    {
        CRB_CLR_DONE(crb->crb_CRSW);
        if (c->c_wait_cqe)
        {
            c->c_wait_cqe = FALSE;
            vjgo(c);
        }
    }
    else {
        CRB_CLR_DONE(crb->crb_CRSW);
    }
}

vjnint(c)
register VJ_CTLR *c;
{
    register VJ_SHIO    *shio;
    register VJ_UNIT    *un;
    register VJ_CRB     *crb;
    register VJ_IOPB    *iopb;
    register IPSG_FREE  *ipsg_free;
    register int i;
    register VJ_DISK_HD *dp;
    register VJ_ALIGN *al;
    int unit, temp;
    BUF     *bp, *tp;
    VJ_CRB  Crb;
    VJ_IOPB Iopb;
    SCSI_CDB *cdb;
    register USHORT crsw;
    u_int count;

    vjexterr = 1; /* be sure and set it for now */

    shio = c->c_io;
    crb     = &Crb;
    iopb    = &Iopb;
    dp = &c->c_disk_hd;

    clean_cache(shio,sizeof(VJ_SHIO));
    vjfrom_shio(&shio->sh_CRB, crb, sizeof(VJ_CRB));
    crsw = W(crb->crb_CRSW);
    if (!(crsw & M_CRSW_CC)) {
	cmn_err(CE_CONT,"DKVJ %d: Command not Complete  CRSW(%x)\n",
		c->c_ctlr,crsw);
	return;
    }

    vjfrom_shio(&shio->sh_RET_IOPB, iopb, sizeof(VJ_IOPB));
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];

    if (iopb->iopb_CMD == SCSI_RESET) {
	un = (VJ_UNIT *)crb->crb_CTAG;
	un->un_qcount--;
	if (un->un_timeid) {
	    untimeout(un->un_timeid);
	    un->un_timeid = 0;
	} else
	    cmn_err(CE_CONT,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
	if (crsw & M_CRSW_EX) {
	    cmn_err(CE_CONT,
		"DKVJ %d:%d: Cmd = %x Exception = %x Work Queue = %d\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD, iopb->iopb_STATUS, crb->crb_WORK_QUEUE);
	    cmn_err(CE_CONT,
		"RESET crsw = %x options= %x tt length= %x length = %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);
	}
	CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	goto splcomp;
    }

    if (iopb->iopb_CMD == CNTR_FLUSH_WORKQ) {
	un = (VJ_UNIT *)crb->crb_CTAG;
	un->un_qcount--;
	if (un->un_timeid) {
	    untimeout(un->un_timeid);
	    un->un_timeid = 0;
	} else
	    cmn_err(CE_CONT,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
	if (crsw & M_CRSW_EX) {
	    cmn_err(CE_CONT,
		"DKVJ %d:%d: Cmd = %x Exception = %x Work Queue = %d\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD, iopb->iopb_STATUS, crb->crb_WORK_QUEUE);
	    cmn_err(CE_CONT,
		"FLUSH crsw = %x options= %x tt length= %x length = %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);
	}
	CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);
	count = ((VJ_FQCF *)iopb)->fqcf_FST.number;
	cmn_err(CE_CONT,"DKVJ %d Work queue %d flushed of %d commands\n",
		un->un_ctlr,un->un_workq,count);
	if (((VJ_FQCF *)iopb)->fqcf_FST.pip) {
	    cmn_err(CE_CONT,"    Primary bus command in progress");
	    unit = (int)shio->sh_CSS.csb_PSEL;
	    temp = (int)shio->sh_CSS.csb_PBST & 0x08; /* busy bit */
	    if (unit == un->un_slave && temp) {
		cmn_err(CE_CONT,", Bus held busy");
	    }
	    cmn_err(CE_CONT,", Resetting SCSI bus\n");
	    vjcmd(un,VJ_SCSI_RESET,0,0,0,WANT_INTERRUPT);
	}
	if (((VJ_FQCF *)iopb)->fqcf_FST.sip) {
	    cmn_err(CE_CONT,"    Secondary bus command in progress");
	    unit = (int)shio->sh_CSS.csb_SSEL;
	    temp = (int)shio->sh_CSS.csb_SBST & 0x08; /* busy bit */
	    if (unit == un->un_slave && temp) {
		cmn_err(CE_CONT,", Bus held busy");
	    }
	    cmn_err(CE_CONT,", Resetting SCSI bus\n");
	    vjcmd(un,VJ_SCSI_RESET,0,0,0,WANT_INTERRUPT);
	}
splcomp:
	vjgo(c);
	if (un->un_qcount && !un->un_timeid)  /* still more commands pending */
	    if (un->un_qcount == 1 && un->un_timeout) {
		un->un_timeid = timeout(vjtimeout,un,un->un_timeout);
	    } else {
		un->un_timeid = timeout(vjtimeout,un,TIME_RDWR);
	    }
	return;
    }
    /*
     * If we came in here with a request sense, it was from the vjsense
     * call later in this routine.  The bp is saved in un structure, so we
     * will have to get it from there 
     */
    if(cdb->cmd == SCSI_REQUEST_SENSE){
	un = (VJ_UNIT *)crb->crb_CTAG;
	bp = un->un_savebp;
	if(bp == NULL){
	    cmn_err(CE_CONT,"DKVJ %d:%d Illegal REQUEST SENSE command\n", 
		un->un_ctlr,un->un_slave);
	    /* Make sure controller isn't frozen */
            W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	    wbflush();
	    return;
	}
    } else {
        bp      = (BUF *)crb->crb_CTAG;
	/*
	 * put scatter/gather free entry back into list
	 */
	if((ipsg_free = (IPSG_FREE*)bp->av_back)) /* any SG entry to return? */
	{   /* link back into free list */
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    bp->av_back = NULL;
	    al = &ipsg_free->align;
	    if (al->al_faddr) {
		if (bp->b_flags & B_READ)
		    bcopy(al->al_taddr,al->al_uaddr,al->al_size);
		kern_free(al->al_faddr);
		al->al_faddr = 0;
	    }
	}
	if (dp->b_forw == bp)
	    dp->b_forw = bp->av_forw;
	else {
	    for (tp = dp->b_forw; tp; tp = tp->av_forw) {
		if (tp->av_forw == bp) {
		    tp->av_forw = bp->av_forw;
		    if (dp->b_back == bp)
			dp->b_back = tp;
		    break;
		}
	    }
	    if (!tp)
		cmn_err(CE_CONT,"DKVJ %d: bp not on controller hash chain\n",
		    c->c_ctlr);
	}
    }
    unit = BPTOVJN(bp);
    un = c->c_unit[unit];
    un->un_qcount--;
    if (un->un_timeid) {
	untimeout(un->un_timeid);
	un->un_timeid = 0;
    } else
	cmn_err(CE_CONT,"DKVJ %d:%d No timeout set\n",
		un->un_ctlr,un->un_slave);
    CRB_CLR_DONE(shio->sh_CRB.crb_CRSW);

    if(cdb->cmd == SCSI_REQUEST_SENSE) {
	/* Thaw the controller */
	W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	wbflush();
	un->un_xfer = bp->b_bcount;
	un->un_resid = bp->b_resid;
	temp = vjsense_status(un,iopb->iopb_STATUS);
	bp->b_resid = un->un_resid;
	if (temp)
	    goto errcomp;
	else
	    goto normcomp1;
    } 

    if (crsw & M_CRSW_ER) {
        /*
         * See if a Check Condition occurred on last command 
         */
        if(((iopb->iopb_STATUS >> 8) & 0xff) == CHECK_CONDITION) {
	    if (vjexterr) {
		cmn_err(CE_CONT,"\nDKVJ %d:%d check condition\n",
			un->un_ctlr,un->un_slave);
		cmn_err(CE_CONT,"     SCSI cdb =  ");
		for (i=0;i<6;++i)
		    cmn_err(CE_CONT,"0x%x ",iopb->iopb_SCSI[i]);
		cmn_err(CE_CONT,"\n");
	    }
	    un->un_savebp = bp;
	    (void)vjsense(c, un, WANT_INTERRUPT);
	   /* 
	    * Since it's interrupt driven, get out of here.  We
	    * will do all of the processing when we come in from the
	    * interrupt returned after the sense command
	    */
	   return;		
	}
	/* what's this??? why do we thaw here? freeze on error is set? */

	/* Thaw the controller */
	W(shio->sh_MCSB.mcsb_THAW) = ((int)un->un_workq << 8) | M_THAW_TWQE;
	wbflush();
	if (((iopb->iopb_STATUS >> 8) & 0x0ff) == SCSI_BUSY) {
printf("SCSI BUSY\n");
		++un->un_scsibusy;
		bp->av_forw  = (BUF *)0;
		if (dp->av_forw == (BUF *)0)
			dp->av_forw          = bp;
		else
			dp->av_back->av_forw  = bp;
		dp->av_back  = bp;
		un->un_timeid = timeout(vjgo,c,HZ);
		return;
	}
	if((iopb->iopb_STATUS & 0xff) == TRANSFER_COUNT_ERROR) {
printf("TRANSFER COUNT ERROR\n"); /* what's this */
	        bp->b_resid = bp->b_bcount - iopb->iopb_LENGTH;
	        goto normcomp;
	}
	cmn_err(CE_CONT,"DKVJ %d:%d Cmd = %x (%x) Queue = %d SCSI error = %x\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD,cdb->cmd,crb->crb_WORK_QUEUE,
		(iopb->iopb_STATUS >>8));
	vjerror(iopb->iopb_STATUS & 0xff);
	cmn_err(CE_CONT,"crsw = %x options= %x tt length= %x length = %x buff= %x\n",
		crsw,iopb->iopb_OPTION,
        	iopb->iopb_TTLENGTH,
        	iopb->iopb_LENGTH,
        	iopb->iopb_BUFF);

	bp->b_resid = bp->b_bcount;
errcomp:
	/* what's this if mean?????? */
        if (un->un_retries && bp->b_error++ < un->un_retries){
printf("retrying....\n");
            bp->av_forw  = (BUF *)0;
            if (dp->av_forw == (BUF *)0)
                dp->av_forw          = bp;
            else
                dp->av_back->av_forw  = bp;

            dp->av_back  = bp;
	    vjgo(c);
	    return;
        } else {
            bp->b_flags |= B_ERROR;
            bp->b_error  = EIO;
	    un->un_flags &= ~IVJ_WRITTEN;
	    goto normcomp;
        }

    }
    if (crsw & M_CRSW_EX) {
	cmn_err(CE_CONT,"DKVJ %d:%d Cmd = %x Exception = %x Work Queue = %d\n",
		un->un_ctlr,un->un_slave,
		iopb->iopb_CMD, iopb->iopb_STATUS, crb->crb_WORK_QUEUE);
	cmn_err(CE_CONT,"crsw = %x options= %x tt length= %x length = %x buff= %x\n",
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
	if (vjexterr)
	    cmn_err(CE_CONT,"DKVJ %d:%d - %d busy retries, cmd = 0x%x\n",
		un->un_ctlr,un->un_slave,un->un_scsibusy,cdb->cmd);
	un->un_scsibusy = 0;
    }
    if (!(bp->b_flags & B_SPL))
	iounmap(bp);
    iodone(bp);                     /* wake up process just done    */
    vjgo(c);
    if (un->un_qcount && !un->un_timeid)  /* still more commands pending */
	if (un->un_qcount == 1 && un->un_timeout) {
		un->un_timeid = timeout(vjtimeout,un,un->un_timeout);
	} else {
		un->un_timeid = timeout(vjtimeout,un,TIME_RDWR);
	}
    un->un_flags &= ~(IVJ_BUSY | IVJ_REWINDING);
    if(un->un_flags & IVJ_WAITING) {
	un->un_flags &= ~IVJ_WAITING;
	wakeup((caddr_t)un);
    }
    if (bp->b_flags & B_SPL)
	bp->b_flags &= ~(B_BUSY | B_SPL);
    if(bp->b_flags & B_WANTED) {
	bp->b_flags &= ~B_WANTED;
	wakeup((caddr_t)bp);
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
    }
    else if (!physck(un->un_vh.vh_pt[LPART(dev)].pt_nblks, B_READ))
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
    }
    else if (!physck(un->un_vh.vh_pt[LPART(dev)].pt_nblks, B_WRITE))
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
	sah_type temp_sph = 0;
        ioaddr_t io_addr;

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
			if(!vme_iomap(ctlr_csh[io->i_ctlr], fmi->fmi_addr,
			sizeof(struct fmt_map_info), GBA_CONTIG_ADDR+
			GBA_NOPART_MAP, &temp_sph, &io_addr)) 
			cmn_err(CE_PANIC, "Can't map format structure !\n");
			clear_page_cache(fmi->fmi_addr, 
				      sizeof(struct fmt_map_info));
			vj_addr		= io_addr;
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
	            if(!vme_iomap(ctlr_csh[io->i_ctlr], &mode_sense1,
		    sizeof(VJ_MODE_SENSE1),GBA_CONTIG_ADDR+
		    GBA_NOPART_MAP, &temp_sph, &io_addr)) 
		    cmn_err(CE_PANIC, "Can't map VJ_MODE_SENSE1 structure !\n");
		    vj_addr		= io_addr;
		    vj_blks		= 0;
		    vj_count	= sizeof( VJ_MODE_SENSE1 );
		    bzero( &mode_sense1, sizeof( VJ_MODE_SENSE1));
		    mode_sense1.hdr.blk_desc_len = 8;
		    mode_sense1.pg1.page_code  = 1;
		    mode_sense1.pg1.page_length = 6;
		    mode_sense1.pg1.retry_count = 8;
		    mode_sense1.pg1.correction_span = 8;
		    mode_sense1.pg1.per = 1;
		    if ( *(int *)arg )
			    mode_sense1.pg1.dcr = 1;
		    clear_page_cache(&mode_sense1, 
				      sizeof(VJ_MODE_SENSE1));
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
	    
	    if (((vj_cmd == VJ_FORMAT) ? vjwaitf : vjwait)(c, M_CRSW_CC))
	    {
		if (vjsense(c, un, NO_INTERRUPT))
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
	    if(temp_sph){
		 if(!vme_iounmap(temp_sph))
		 cmn_err(CE_PANIC,"Can't unmap format buffer !\n");
	    }
	    clear_page_cache(vj_addr, vj_count);
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
    register VJ_INQUIRY *inquiry;
    struct mtop *mtop;
    struct mtget *mtget;
    struct gioctl gioctl;
    int rc, ctlr, unit;
    long bscnt;
    register i,j;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    if (ctlr > Nvjctlr || (c = vjctlrs[ctlr]) == 0 ||
        (un = c->c_unit[unit]) == 0) {
	u.u_error = ENXIO;
	return;
    }
    shio = c->c_io;
    msr = &shio->sh_MCSB.mcsb_MSR;
    inquiry = (VJ_INQUIRY *)K0_TO_K1(&un->un_inquiry);

    if(cmd == GIOCPRSTR){
	/*
	 * Need to return scsi inquiry string
	 */
	scsistr(inquiry, gioctl.ident);
	if(copyout((caddr_t)gioctl.ident, (caddr_t) arg, IDENT_LEN))
	    u.u_error = EFAULT;
	return;
    }
    if (un->un_flags & IVJ_DISK) {
	switch (cmd)
	{
#ifdef NOTDEF
	    case DIOCPARAMS:
	    case DIOCDRIVETYPE:
	    case DIOCPASSTHRU:
	    case DIOCRAWREAD:
	    case DIOCGETERROR:
	    case DIOCRAWSEEK:
	    case DIOCHANDSHAKE:
#endif
	    case DIOCDIAG:
		/*
		 * for now just look at Board-OK
		 *
		 */
		if (!(WORDP(msr) & M_MSR_BOK))
		{
		    u.u_error = EIO;
		}
		break;
	    case DIOCGETVH:
		/* get volume header */
		if (!un->un_vhvalid)
		{
		    u.u_error = EIO;
		}
		else
		{
		    if(copyout((caddr_t) &un->un_vh, (caddr_t) arg,sizeof(DVH)))
			u.u_error = EFAULT;
		}
		break;

	    case DIOCSETVH:
		/* set volume header */
		{
		struct volume_header vh;

		if (copyin(arg, (caddr_t)&vh, sizeof(DVH)))
		    u.u_error = EFAULT;
		else
		    if (is_vh(&vh) == FALSE)
			u.u_error = EIO;
		    else
		    {
			un->un_vhvalid = 1;
			bcopy((caddr_t)&vh, (caddr_t)&un->un_vh, sizeof(DVH));
		    }
		}
		break;

	    default:
		u.u_error = EINVAL;
		break;
	}
    }

    if(un->un_flags & IVJ_TAPE){
	switch(cmd)
	{
	    case MTIOCTOP:
		rc = 0;
		mtop = (struct mtop *)arg;
		switch(mtop->mt_op) {
		    case MTREW:
			if (vjsplcmd(un, VJ_REWIND,0,0,WAIT)) {
			    cmn_err(CE_CONT,
				"DKVJ %d:%d ioctl: Error in rewinding\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			un->un_flags &= ~(IVJ_RFM|IVJ_FM|IVJ_EOM);
			un->un_eomcount = 0;
			break;
		    case MTWEOF:
			if (vjsplcmd(un, VJ_W_FM,0,1,WAIT)) {
			    cmn_err(CE_CONT,
			    "DKVJ %d:%d ioctl: Error in writting file marks\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTFSF:
			if(vjsplcmd(un, VJ_SPACE,SP_FILEMARK,mtop->mt_count,
				WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in fsf\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTBSF:	/* Space 2's complement */
			bscnt = ~((long)mtop->mt_count) + 1;
			if(vjsplcmd(un, VJ_SPACE,SP_FILEMARK,bscnt,WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in bsf\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTFSR:
			if(vjsplcmd(un,VJ_SPACE,SP_BLOCK,mtop->mt_count,WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in fsr\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTBSR:	/* Space 2's complement */
			bscnt = ~((long)mtop->mt_count) + 1;
			if(vjsplcmd(un, VJ_SPACE, SP_BLOCK, bscnt,WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in bsr\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTOFFL:
			if(vjsplcmd(un, VJ_REWIND, 0, 0, WAIT)) {
			    cmn_err(CE_CONT,
				"DKVJ %d:%d ioctl: Error in rewind\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			    break;
			}
			if(vjsplcmd(un, VJ_LOAD, SCSI_UNLOAD_FLAG, 0, WAIT)) {
			    cmn_err(CE_CONT,
				"DKVJ %d:%d ioctl: Error in unload\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			un->un_flags &= ~(IVJ_RFM|IVJ_FM|IVJ_EOM);
			un->un_eomcount = 0;
			break;
		    case MTRET:
			if(vjsplcmd(un, VJ_LOAD, SCSI_LOAD_FLAG|SCSI_RETEN, 
					0, WAIT)) {
			    cmn_err(CE_CONT,
				"DKVJ %d:%d ioctl: Error in retension\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			un->un_flags &= ~(IVJ_RFM|IVJ_FM|IVJ_EOM);
			un->un_eomcount = 0;
			break;
		    case MTONL:
			if(vjsplcmd(un, VJ_LOAD, SCSI_LOAD_FLAG, 0, WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in load\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			un->un_flags &= ~(IVJ_RFM|IVJ_FM|IVJ_EOM);
			un->un_eomcount = 0;
			break;
		    case MTAPP:
			if(vjsplcmd(un, VJ_LOAD, SCSI_LOAD_FLAG, 0, WAIT)) {
			    cmn_err(CE_CONT,"DKVJ %d:%d ioctl: Error in load\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			    break;
			}
			if(vjsplcmd(un, VJ_SPACE, SP_ENDOFDATA, 0, WAIT)) {
			    cmn_err(CE_CONT,
				"DKVJ %d:%d ioctl: Error in append\n",
				un->un_ctlr,un->un_slave);
			    rc = 1;
			}
			break;
		    case MTNOP:
		    case MTRST:
		    default:
			u.u_error = EINVAL;
			break;
		}
		if(rc) u.u_error = EIO;
		break;

	    case MTIOCGET:
		mtget = (struct mtget *)arg;
		mtget->mt_type	 = MT_JAG;
		mtget->mt_dsreg	 = un->un_flags;
		mtget->mt_erreg	 = un->un_flags>>16;
		mtget->mt_resid	 = 0;
		break;
	    default:
		cmn_err(CE_CONT,"DKVJ %d:%d ioctl: unrecognized command type\n",
				un->un_ctlr,un->un_slave);
		u.u_error = EINVAL;
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
vjwaitf(c, mask)      /* wait for format to complete */
register VJ_CTLR *c;
register UWORD mask;
{
    register VJ_SHIO *shio;
    volatile VJ_CRB *crb;
    register VJ_IOPB *iopb;
    SCSI_CDB *cdb;
    register int rc;
#ifdef STANDALONE
    register count,x;

    count = 0;
#endif STANDALONE

    shio = c->c_io;
    crb   = &shio->sh_CRB;
    iopb = &shio->sh_RET_IOPB;
    cdb = (SCSI_CDB *)&iopb->iopb_SCSI[0];

    DELAY(256);
    while (!(W(crb->crb_CRSW) & M_CRSW_CRBV))
    {
#ifdef STANDALONE
	if((++count % 0x400) == 0){
		printf(".");
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
        cmn_err(CE_CONT,"DKVJ %d: CRSW(0x%x) CRVB not found.\n",
		c->c_ctlr,W(crb->crb_CRSW));
    } else if ((W(crb->crb_CRSW) & mask) != mask) {
	rc = 1;
        cmn_err(CE_CONT,"DKVJ %d: CRSW(0x%x) mask=%x\n",W(crb->crb_CRSW),
		c->c_ctlr,mask);
    }
    if (W(crb->crb_CRSW) & M_CRSW_ER && rc == 0)
    {
	rc = 1;
	cmn_err(CE_CONT,
	    "DKVJ %d: Error: Command = 0x%x %x CRSW(0x%x) status = 0x%x\n",
		c->c_ctlr,
		iopb->iopb_CMD, cdb->cmd, W(crb->crb_CRSW), iopb->iopb_STATUS);
    }
    CRB_CLR_DONE(crb->crb_CRSW);
    return(rc);
}

#ifndef STANDALONE
/*
 * Setup a read/write command, using the scatter gather.
 *      (MACSI Version)
 *  Word-wide scatter/gather will be used if the request is non-page. Doesn't look like it to me!!!!
 *  aligned or if the request is not a sector multiple.
 * NOTE: Word-wide scatter/gather requests still have to multiples of sectors
 *   if the request is not, the remaining portion of the request will
 *   dumped into a bit bucket.
 *
 * NOTE that word wide SG (when implemented) will need to use AM9 since block mode mode won't work!!!
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
    IPSG_FREE *ipsg_free;
    int unit = BPTOVJN(bp);

    ipsg_free = c->dkvjsg_hd;       /* get top free entry   */
    c->dkvjsg_hd = ipsg_free->nxt;  wbflush(); /* unlink       */
    sg = &ipsg_free->ipsg[0];       /* get top SG  entry    */
    firstsg = (IPSG*)sg;
    al = &ipsg_free->align;
    al->al_faddr = 0;

    bp->av_back = (struct buf *)ipsg_free; /* save for int rout to free */
    dmaaddr = (long)bp->b_dmaaddr + bp->b_bcount - bp->b_resid;
    links = 0;
    total = 0;

    physaddr = ctob(kvtokptbl(dmaaddr)->pgm.pg_pfn);
    ASSERT(physaddr != 0);
    offset = dmaaddr & (POFFMASK);
    physaddr |= offset;
    if (physaddr & 0x01) {
	al->al_size = amount = bp->b_resid;
	if (flag) {		/* variable record device */
	    if (amount & 0x01)
		amount++;
	} else if (amount & (NBPSCTR-1)) {	/* fixed record device */
	    amount += (NBPSCTR - (amount & (NBPSCTR - 1)));
	}
	al->al_uaddr = dmaaddr;
	al->al_faddr = physaddr = (ULONG)kern_malloc(amount);
	if (!physaddr) {
	    /*
	     * put scatter/gather entry back into free list
	     */
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    bp->av_back = NULL; /* no entry to free */
	    return(0);
	}
	dmaaddr = physaddr;
	physaddr = K2_TO_PHYS(physaddr);
	al->al_taddr = (ULONG)PHYS_TO_K1(physaddr);
	bp->b_resid = amount;
    }

    /* fill in scatter gather structs */

    while (bp->b_resid)
    {
        /* limit this sg to this page */
        amount = bp->b_resid;
        offset = dmaaddr & (POFFMASK);
        if (offset + amount > NBPP)
            amount = NBPP - offset;

        /* fill in sg struct */
        physaddr = ctob(kvtokptbl(dmaaddr)->pgm.pg_pfn);
        ASSERT(physaddr != 0);
	physaddr |= offset;
	if (amount & 0x01 && amount > 0x01)
	    --amount;
        dmaaddr += amount;
        total += amount;
	bp->b_resid -= amount;
	if (amount & 0x01) {
	    al->al_uaddr = dmaaddr - amount;
	    al->al_size = amount++;
	    al->al_faddr = physaddr = (ULONG)kern_malloc(amount);
	    if (!physaddr) {
		/*
		 * put scatter/gather entry back into free list
		 */
		ipsg_free->nxt = c->dkvjsg_hd;
		c->dkvjsg_hd = ipsg_free;
		bp->av_back = NULL; /* no entry to free */
		return(0);
	    }
	    physaddr = K2_TO_PHYS(physaddr);
	    al->al_taddr = (long)PHYS_TO_K1(physaddr);
	    if (!(bp->b_flags & B_READ))
		bcopy(al->al_uaddr,al->al_taddr,al->al_size);
	}
	sg->sg_count = amount;
        sg->sg_addr_msw = HI16(physaddr);
        sg->sg_addr_lsw = LO16(physaddr);
        sg++;
        if (++links >= (MACSI_SG - 1))
        {
            /*
             * Too Many links, start up request
             */
            break;
        }
    }

    if(!flag && (amount = (total & (NBPSCTR -1)))) {  /* non-sector multiple? */
	/* YES, then read the rest into 'temp buffer' i.e. throw it away */
	sg->sg_count = NBPSCTR - amount;
	total += sg->sg_count;
	bzero(temp_buffer, SECSIZE);
	physaddr = K0_TO_PHYS(temp_buffer);
	sg->sg_addr_msw = HI16(physaddr);
	sg->sg_addr_lsw = LO16(physaddr | offset);
	++links;
	cmn_err(CE_CONT, "DKVJ %d:%d Padded to 512 (%d) S/G links (%d)\n", 
		c->c_ctlr,unit,sg->sg_count,links);
    }

    if (links == 1)
    {
        /* With only one link, don't bother scatter/gathering */
        iopb->iopb_LENGTH       = total & 0x01 ? total + 1 : total;
        iopb->iopb_BUFF = 
		(ULONG)((firstsg->sg_addr_msw << 16) | firstsg->sg_addr_lsw);
	if (!al->al_faddr) {
	    /*
	     * put scatter/gather entry back into free list
	     */
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    bp->av_back = NULL; /* no entry to free */
	}
    }
    else
    {
        W(iopb->iopb_OPTION) |= M_OPT_SG;
        iopb->iopb_TTLENGTH     = total & 0x01 ? total + 1 : total;
        iopb->iopb_LENGTH       = links;
        iopb->iopb_BUFF         = K0_TO_PHYS(firstsg);
    }
    if (flag)
	return(total);
    else
	return(total >> SCTRSHFT);
}

/*----------------------------------------------------------------------*\
 *			IOCTL assist functions				*
\*----------------------------------------------------------------------*/

vjsplcmd(un, cmd, code, count, wait)
VJ_UNIT *un;
int cmd,code,count,wait;
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
    switch (cmd) {
	case VJ_LOAD:
	case VJ_REWIND:
	    un->un_timeout = TIME_REWIND;
	    break;
	case VJ_SPACE:
	    if (code == SP_FILEMARK)
		    un->un_timeout = TIME_FSF;
	    else
		    un->un_timeout = TIME_RDWR;
	    break;
	default:
	    un->un_timeout = TIME_RDWR;
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
    cmn_err(CE_CONT, "ijc%dd%ds%d: %s (dev 0x%x)\n",
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
    UINT status, addr, i;

    unit = VJUNIT(dev);
    VJCTLR(ctlr,dev);
    c = vjctlrs[ctlr];
    un = c->c_unit[unit];

    /*
     * If the drive doesn't exist,
     * or if it doesn't have a valid label, return an error.
     */
    if ((un->un_flags & (IVJ_ALIVE|IVJ_READY)) == 0)  /* unit OK? */
	return(ENODEV);
    if (un->un_vhvalid == 0)
	return (EINVAL);
    if (LPART(dev) != 1)
	return (EINVAL);

    if (flag == DUMP_OPEN) {
	if (un->un_flags == 0)
		vjattach(dev);
	if (un->un_flags & (IVJ_ALIVE | IVJ_READY) ==0)
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
    vjcmd(un,VJ_WRITE,physaddr,bn,count,NO_INTERRUPT);
    status = vjwait(vjctlrs[un->un_ctlr], M_CRSW_CC);
    if ((status >> 8 & 0xff) == CHECK_CONDITION) {
	sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
	if (vjsense(vjctlrs[un->un_ctlr], un, NO_INTERRUPT))
	    cmn_err(CE_CONT,"DKVJ %d:%d request sense ERROR\n",
		un->un_ctlr,un->un_slave);
	else if (sense->key == SCSI_UNIT_ATTENTION)
	    goto tryagain;
	else
	    cmn_err(CE_CONT,"DKVJ %d:%d %s\n",un->un_ctlr,un->un_slave,
		    sense_err_msg(sense->key));
	return(EIO);
    } else if (status & 0xff) {
	vjerror(status & 0xff);
	return(EIO);
    } else if (status) {
	cmn_err(CE_CONT,"DKVJ %d:%d error status = 0x%x\n",
		un->un_ctlr,un->un_slave,status);
	return(EIO);
    }
    return (0);
}

void
vjtimeout(un)
register VJ_UNIT *un;
{
    register VJ_CTLR *c = vjctlrs[un->un_ctlr];
    register VJ_SHIO *shio = c->c_io;
    register USHORT crsw;
    VJ_CQE Mce, *mce = &Mce;
    VJ_FQCF Fqcf, *fqcf = &Fqcf;

    if (un->un_timeid) {
	un->un_timeid = 0;
	un->un_timeout = TIME_RDWR;
	cmn_err(CE_CONT,"DKVJ %d:%d No interrupt from controller\n",
		un->un_ctlr,un->un_slave);
	vjcmd(un, VJ_FLUSH_WORKQ,0,0,0,WANT_INTERRUPT);
    } else
	cmn_err(CE_CONT,"DKVJ %d:%d Timeout with no ID: qcount = %d\n",
		un->un_ctlr,un->un_slave,un->un_qcount);
}
#endif STANDALONE

vjerror(type)
register unsigned type;
{
    register int i;
	
    for (i=0; jaguar_err[i].code != 0xff; i++)
	if (jaguar_err[i].code == type)
	    break;
    cmn_err(CE_CONT,"        Jaguar Error = %x (%s)\n",
	type, jaguar_err[i].msg);
}


int
vjsense_status(un,status)
register VJ_UNIT *un;
{
    SCSI_EXT_SENSE *sense;
    VJ_MODE_SENSE *ms;
    register int diff, flag;

    flag = 0;
    diff = un->un_xfer;
#ifdef STANDALONE
    sense = &un->un_sense;
    clear_page_cache(sense, sizeof(SCSI_EXT_SENSE));
    ms = &un->un_msense;
#else STANDALONE
    sense = (SCSI_EXT_SENSE *)K0_TO_K1(&un->un_sense);
    ms = (VJ_MODE_SENSE *)K0_TO_K1(&un->un_msense);
#endif STANDALONE
    /* flush the sense data from gba*/
    if(!vme_ioflush(un->un_sph, sense, sizeof(SCSI_EXT_SENSE)))
		cmn_err(CE_PANIC, "Can't flush sense!\n");
    if (vjexterr && !dkvj_inopen) {
	cmn_err(CE_CONT,"\n     SCSI sense status\n");
	cmn_err(CE_CONT,"       valid   =%d\n",sense->valid);
	cmn_err(CE_CONT,"       segment =%d\n",sense->segment);
	cmn_err(CE_CONT,"       filmrk  =%d\n",sense->filmrk);
	cmn_err(CE_CONT,"       eom     =%d\n",sense->eom);
	cmn_err(CE_CONT,"       ilength =%d\n",sense->ilength);
	cmn_err(CE_CONT,"       key     =0x%x %s\n",sense->key,
		sense_err_msg(sense->key));
	cmn_err(CE_CONT,"       info    =0x%x%x%x%x\n",sense->info1,
		sense->info2,sense->info3,sense->info4);
	cmn_err(CE_CONT,"       add_len =%d\n",sense->add_len);
    }
    if(((status >> 8) & 0xff) == CHECK_CONDITION) {
	cmn_err(CE_CONT,
		"DKVJ %d:%d check condition on request sense command\n",
		un->un_ctlr,un->un_slave);
	goto xxxerr;
    }
    if (sense->key == SCSI_RECOVERABLE_ERROR) {
	if (vjexterr)
	    cmn_err(CE_CONT,"DKVJ %d:%d %s\n",
		un->un_ctlr,un->un_slave,sense_err_msg(sense->key));
	if (un->un_flags & IVJ_TAPE)
		un->un_flags &= ~IVJ_FM;
	return(0);
    }
    if(un->un_flags & IVJ_TAPE) {
	if (sense->valid) {
	    diff = sense->info1<<24 | sense->info2<<16 | sense->info3<<8 | 
			sense->info4;
	    if (!(un->un_flags & IVJ_VARIABLE))
		    diff *= 512;
	    if (!sense->filmrk)
		un->un_flags &= ~IVJ_FM;
	}
	if(sense->filmrk) {
	    flag = 1;
	    if (un->un_flags & IVJ_FM) {
		un->un_flags |= IVJ_EOM;
		un->un_eomcount = MAXEOM + 1;
	    }
	    else {
		un->un_flags |= IVJ_FM;
		if (diff != un->un_xfer)
		    un->un_flags |= IVJ_RFM;
	    }
	}
	if(sense->eom) {
	    flag = 1;
	    if (!sense->valid)
		diff = 0;
	    un->un_flags |= IVJ_EOM;
	    if (un->un_flags & IVJ_VARIABLE)
		++un->un_eomcount;
	}
	if (sense->ilength) {
	    flag = 1;
	    if (diff <= 0) {
		cmn_err(CE_CONT,
		    "DKVJ %d:%d illegal block length unit %d, actual=%d\n",
		    un->un_ctlr,un->un_slave,un->un_xfer - diff); 
		diff = un->un_xfer;
		goto xxxerr;
	    }
	} 
	if(sense->key == SCSI_UNIT_ATTENTION) {
	    un->un_eomcount = 0;
	    un->un_flags |= IVJ_ATN;
	    diff = un->un_xfer;
	} else if(sense->key == SCSI_NOT_READY) {
	    un->un_flags |= IVJ_NOT_RDY;
	    goto xxxerr;
	} else if(sense->key == SCSI_DATA_PROTECT && !ms->hdr.WP) {
	    cmn_err(CE_CONT,
		"DKVJ %d:%d cartridge is not a 600 ft tape (QIC-120)\n",
		un->un_ctlr,un->un_slave);
	    diff = un->un_xfer;
	    goto xxxerr;
	} else if (sense->key || !flag) {
	    cmn_err(CE_CONT,"DKVJ %d:%d %s\n",un->un_ctlr,un->un_slave,
		sense_err_msg(sense->key));
	    goto xxxerr;
	}
	un->un_resid = diff;
	return(0);
    } else if(sense->key == SCSI_NOT_READY){
	un->un_flags |= IVJ_NOT_RDY;
	return(0);
    } else if (status == TRANSFER_COUNT_ERROR) {
	return(0);
    }
xxxerr:
    un->un_resid = diff;
    return(1);
}

#ifndef STANDALONE
static void
disable_controller(c)
register VJ_CTLR *c;
{
    register IPSG_FREE *ipsg_free;
    register VJ_UNIT *un;
    volatile VJ_MSR *msr;
    volatile VJ_MCR *mcr;
    register VJ_SHIO *shio;
    register BUF *bp, *tp;
    register int i, s;

    cmn_err(CE_WARN,"DKVJ %d: Controller disabled\n",c->c_ctlr);
    s = splhigh();
    shio = c->c_io;
    msr = &shio->sh_MCSB.mcsb_MSR;
    mcr = &shio->sh_MCSB.mcsb_MCR;
    WORDP(mcr) |= ~M_MCR_RES; wbflush();	/* Reset the controller */
    WORDP(msr) = 0; wbflush();			/* Clear BOK bit */
    DELAY(100);
    WORDP(mcr) &= ~M_MCR_RES; wbflush();	/* Clear Reset */
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
	    un->un_flags &= ~(IVJ_ALIVE | IVJ_READY);
	}
    }
/* must return errors for all outstanding commands */
    tp = c->c_disk_hd.b_forw;
    while (bp = tp) {
	tp = bp->av_forw;
	if (ipsg_free = (IPSG_FREE *)bp->av_back) {
	    ipsg_free->nxt = c->dkvjsg_hd;
	    c->dkvjsg_hd = ipsg_free;
	    bp->av_back = NULL;
	    if (ipsg_free->align.al_faddr)
		kern_free(ipsg_free->align.al_faddr);
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
    }
    vjgo(c);
    splx(s);
}
#endif STANDALONE
