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
#ident "$Header: dkip.c,v 1.22 90/10/25 15:34:28 hawkes Exp $"

/*
 * dkip.c -- Interphase VME 3200 standalone disk driver
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "mipsvme/dkipreg.h"
#include "mipsvme/vmereg.h"
#include "machine/dvh.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#ifdef	R6000_FAKEIO
int fakeio_memtype = IPMT_16BIT;
int fakeio_am = VME_A24NPAMOD;
#undef	IPMT_32BIT
#undef	VME_A32NPAMOD
#define VME_A32NPAMOD fakeio_am
#define	IPMT_32BIT fakeio_memtype
#endif	R6000_FAKEIO
/*
 * Standard addresses for VME 3200 registers.
 * MUST be at least 32 bytes apart.
 * Address should be given relative to A16 Supervisor address space.
 */
static struct ipdevice *ipstd[] = { 
	(struct ipdevice *)0x8600,
	(struct ipdevice *)0x8800,
	(struct ipdevice *)0x8a00,
	(struct ipdevice *)0x8c00,
	(struct ipdevice *)0x7600,
	(struct ipdevice *)0x7800,
	(struct ipdevice *)0x7a00,
	(struct ipdevice *)0x7c00
};
static char *ip_name = "Interphase VME 3200";	/* xstr pukes on ip_name[] */

/*
 * NUNITS MUST BE A POWER OF 2!
 */
#define	NUNITS		4	/* max drives per controller */
#define	NCTLRS		(sizeof(ipstd)/sizeof(ipstd[0]))
#define	MAXSECTRK	100	/* max sectors per track */
#define	NRETRIES	3	/* number of tries to read a block */
#define	MAXTRK		10	/* tracks to search for volume header */
#define WAITLOOPS	10000

/*
 * Internal io functions
 */
#define	MAPTRACK	0x10	/* map bad track */
#define	MAPSECTOR	0x11	/* slip bad sector */
#define	VERIFY		0x12	/* verify sectors readable */
#define	FMTTRACK	0x13	/* format track */
#define DIAG		0x14	/* cause controller to run diagnostics */
#define	FMTTRACKDATA	0x15	/* format track with data */
#define READ_DEFECT	0x16	/* read media defect information */
#define TRACKID		0x17	/* read track info (used for skewing) */
			

/*
 * Reserve one spare controller slot in volume header table
 * for dynamically assigned csr addresses
 */
static struct volume_header ipvh[NCTLRS+1][NUNITS];
static int vhbuf[DEV_BSIZE/sizeof(int)];
static int noecc[NCTLRS+1][NUNITS];
static unsigned char ctlr_type[NCTLRS+1];
static char isinit[NCTLRS];
static struct ipuib ipuib;
static sah_type vhbuf_sah;	/* System Page descriptor for map of vhbuf */
static csh_type ctlr_csh[NCTLRS];
struct volume_header Vh;	/* Volume header for current drive */
static char def_buf[512];		/* sector size */
#ifdef DKDEBUG
static struct ipdevice Ip;
#endif


/*
 * UIB's to try while attempting to read volume header to find
 * real uib info
 */
static struct ipuib inituib[] = {
/*sh0 nh0 sh1 nh1 sctk skw bytsech byscl gap1 gap2 ilv rty cylh cyll att */
/* unit initialization block for the Fuji 2333 */
/*
{  0, 10,  0,  0,  63,  0, 512>>8,  512,  10,  20,  1, NRETRIES, 0x3, 0x37, 0x45, 0, 0, 0 }
*/
{  0, 10,  0,  0,  64,  56, 512>>8,  512 & 0xff, 10,  20,  1, NRETRIES, 823>> 8, 823 & 0xff, 0x45, 0, 0, 0 }
};

#define	NUIB	(sizeof(inituib)/sizeof(inituib[0]))

/*
 * Csr register description
 */
static struct reg_desc ipcsr_desc[] = {
	/* mask			shift	name		format	values */
	{  IPCS_SCSRC,		0,	"SCSRC",	0,	0 },
	{  IPCS_ERLST,		0,	"ERLST",	0,	0 },
	{  IPCS_SC,		0,	"SC",		0,	0 },
	{  IPCS_OPDONE,		0,	"OPDONE",	0,	0 },
	{  IPCS_GOBUSY,		0,	"GOBUSY",	0,	0 },
	{  IPCS_BERR,		0,	"BERR",		0,	0 },
	{  IPCS_ABORT,		0,	"ABORT",	0,	0 },
	{  IPCS_BDCLR,		0,	"BDCLR",	0,	0 },
	{  IPCS_SFEN,		0,	"SFEN",		0,	0 },
	{  IPCS_BOK,		0,	"BOK",		0,	0 },
	{  IPCS_SLED,		0,	"SLED",		0,	0 },
	{  0,			0,	0,		0,	0 }
};

/*
 * Command exception code format
 */
static struct reg_desc ipexcept_desc[] = {
	/* mask			shift	name		format	values */
	{  IPEX_RETRYMASK,	0,	"RETRIES", 	"%d",	0 },
	{  IPEX_RECAL,		0,	"RECAL",	0,	0 },
	{  IPEX_ECC,		0,	"ECC",		0,	0 },
	{  0,			0,	0,		0,	0 }
};

/*
 * Command error values
 */
static struct reg_values iperror_values[] = {
	/* value		name */
	{  IPER_DNOTRDY,	"drive not ready" },
	{  IPER_SEEKERR,	"seek error, cyl not found" },
	{  IPER_ECCERR,		"ecc error, correctn not attempted" },
	{  IPER_INVCMD,		"invalid command" },
	{  IPER_ILLEXEC,	"illegal execute iopb" },
	{  IPER_INVSEC,		"invalid sector address" },
	{  IPER_ILLMEM,		"illegal memory type" },
	{  IPER_BUSTMOUT,	"bus timeout" },
	{  IPER_HDRCSUM,	"header checksum" },
	{  IPER_WPROT,		"write protect" },
	{  IPER_NOUNIT,		"unit select error" },
	{  IPER_SKERRTMOUT,	"seek error timeout" },
	{  IPER_FAULTTMOUT,	"fault timeout" },
	{  IPER_DFAULT,		"drive fault" },
	{  IPER_RDYTMOUT,	"ready timeout" },
	{  IPER_EOM,		"end of media" },
	{  IPER_VOLFAULT,	"volume uib not initialized" },
	{  IPER_INVHDRPAD,	"invalid header pad" },
	{  IPER_HARDECC,	"uncorrectable ecc error" },
	{  IPER_LGCLCYL,	"logical address error on cylinder" },
	{  IPER_LGCLTRK,	"logical address error on trk" },
	{  IPER_LGCLSEC,	"logical address error on sec" },
	{  IPER_DOVERRUN,	"data overrun" },
	{  IPER_INDEXTMOUT,	"index pulse timeout" },
	{  IPER_SECNFND,	"sector not found" },
	{  IPER_IDERR,		"head number wrong in header" },
	{  IPER_INVSYNCD,	"invalid sync char in data" },
	{  IPER_HDRINVAL,	"no valid header was found" },
	{  IPER_SEEKTMOUT,	"seek timeout" },
	{  IPER_BUSYTMOUT,	"busy timeout" },
	{  IPER_NOTONCYL,	"not on cylinder" },
	{  IPER_RTZTMOUT,	"rtz timeout" },
	{  IPER_INVSYNCH,	"invalid sync char in header" },
	{  IPER_UNITINIT,	"unit not initialized" },
	{  IPER_GAPERR,		"gap specification error" },
	{  IPER_DSEEKERR,	"drive reported seek error" },
	{  IPER_UIBSECTRK,	"uib sectors/trk spec incorrect" },
	{  IPER_UIBBYTSEC,	"uib bytes/sector spec incorrect" },
	{  IPER_UIBINTRLV,	"uib intrlv spec incorrect" },
	{  IPER_INVTRK,		"trk address mismatch with uib" },
	{  IPER_INVCYL,		"cyl address mismatch with uib" },
	{  IPER_ODDDMACNT,	"odd dma count" },
	{  IPER_PBBUSERR,	"bus error referencing iopb" },
	{  IPER_DMABUSERR,	"bus error on data transfer" },
	{  IPER_ADDRERR,	"unaligned address" },
	{  IPER_BADHDR,		"unrecognized header field" },
	{  IPER_BADMAPHDR,	"mapped header error" },
	{  IPER_NOSPARE,	"spare sector not spec'ed in uib" },
	{  IPER_CMDABORT,	"command aborted" },
	{  IPER_ACFAIL,		"acfail detected" },
	{  IPER_CMDNIMP,	"command not implemented" },
	{  0,			0 }
};

/*
 * Command error format
 */
static struct reg_desc iperror_desc[] = {
	/* mask			shift	name		format	values */
	{  0xff,		0,	"Error",	0,	iperror_values},
	{  0,			0,	0,		0,	0 }
};

/*
 * Drive status register format
 */
static struct reg_desc ipds_desc[] = {
	/* mask			shift	name		format	values */
	{  IPDS_DRDY,		0,	"DRDY",		0,	0 },
	{  IPDS_WPROT,		0,	"WPROT",	0,	0 },
	{  IPDS_DBUSY,		0,	"DBUSY",	0,	0 },
	{  IPDS_FAULT,		0,	"FAULT",	0,	0 },
	{  IPDS_ONCYL,		0,	"ONCYL",	0,	0 },
	{  IPDS_SKERR,		0,	"SKERR",	0,	0 },
	{  IPDS_UALIVE,		0,	"UALIVE",	0,	0 },
	{  IPDS_URDY,		0,	"URDY",		0,	0 },
	{  0,			0,	0,		0,	0 }
};

struct ipdevice ipsave;

/*
 * _dkipinit -- initialize driver global data
 */
_dkipinit()
{
	bzero(ipvh, sizeof(ipvh));
	bzero(vhbuf,sizeof(vhbuf));
	bzero(isinit, sizeof(isinit));
}

/*
 * _dkipopen -- initialize interphase 3200 controller, read in volume
 * header and configure controller for drive.
 */
_dkipopen(io)
register struct iob *io;
{
	register volatile struct ipdevice *ip;
	register volatile struct ipiopb *ipb;
	register struct ipuib *ipu;
	register struct volume_header *ipv;
	register struct device_parameters *dp;
	register unit, trk;
	sah_type ipuib_sah;
	ioaddr_t ioaddr;
	int i;

	/*
	 * verify controller, unit, and partition numbers
	 * NOTE: a negative controller number means the user wants to specify
	 * a controller address directly and not use the defaults.
	 */
	if (io->i_unit >= NUNITS) {
		printf("ip bad unit %d\n", io->i_unit);
		goto bad;
	}
	unit = io->i_unit & (NUNITS-1);	/* drive number */
	if (io->i_ctlr >= (int)NCTLRS) {
		printf("ip bad controller number %d\n", io->i_ctlr);
		goto bad;
	}
	if (io->i_part < 0 || io->i_part >= NPARTAB) {
		printf("ip bad partition %d\n", io->i_part);
		goto bad;
	}
	if (io->i_ctlr >= 0)
		ip = (struct ipdevice *)
		  (IS_R6300 ? find_r6000_controller( ipstd[io->i_ctlr], 2
						    ,sizeof(ip->ipcsr) )
		   : PHYS_TO_K1(VMESA16_TO_PHYS(ipstd[io->i_ctlr])));
	else {
		ip = (struct ipdevice *)
		  (IS_R6300 ? find_r6000_controller( -io->i_ctlr, 2
						    ,sizeof(ip->ipcsr) )
		   : PHYS_TO_K1(VMESA16_TO_PHYS(-io->i_ctlr)));
		/*
		 * last slot of tables are reserved for user specified 
		 * csr addresses
		 */
		io->i_ctlr = NCTLRS;
		ipvh[NCTLRS][unit].vh_magic = 0;	/* force config */
	}
	if (badaddr(&ip->ipcsr, sizeof(ip->ipcsr))) {
		printf("no interphase controller at 0x%x\n", &ip->ipcsr);
		goto bad;
	}
	/*
	 * Make sure board is reset.
	 */
	if( !isinit[io->i_ctlr]){
#ifndef SABLE
	    DELAY(1024);
	    ip->ipcsr = IPCS_BDCLR;
	    wbflush();
	    /*
	     * Have to delay at least 1 microsecond, so lets delay more 
	     * to be safe.  When we write a zero, the Reset will occur.
	     */
	    DELAY(64);
	    ip->ipcsr = 0;
	    wbflush();
	    /*
	     * Have to delay at least 100 microsecs.
	     */

	    i = 0;
	    do {		/* Reset takes between 655 & 786 millisecs */
	      DELAY(65536);	/* Check every 65 millseconds */
	    } while ((ip->ipcsr & IPCS_GOBUSY) && (++i < 1024));

	    if (ip->ipcsr & IPCS_GOBUSY) {
	      printf("Controller power up diag never completed!\n");
	      goto bad;
	    }

	    if (!(ip->ipcsr & IPCS_BOK)) {
	      printf("Controller power up diag failed!\n");
	      goto bad;
	    }
#else	    
	    ip->ipcsr = IPCS_BDCLR;
	    ip->ipcsr = 0;
#endif SABLE	    
	}
	
	io->i_devaddr = (unsigned)ip;	/* stash in iob for future use */
	ipb = &ip->ipiopb;
	ipv = &ipvh[io->i_ctlr][unit];

	/*
	 * Return if already configured
	 */
	if (ipv->vh_magic == VHMAGIC){
		goto checkunit;
	}

	/* 
	 * First find out which type of controller
	 */
	if( !isinit[io->i_ctlr]){
	    ip_initiopb(ipb, io->i_unit);
	    ipb->ippb_cmdcode = IP_HANDSHAKE;
	    wbflush();
	    ip_runcmd(io,0);
	    if (ipb->ippb_statcode != IPS_OK) {
		printf("ipc%dd%d error on handshake status : %x\n",io->i_ctlr, io->i_unit,ipb->ippb_statcode);
		ip_error(ip);
		goto bad;
	    }
	    ctlr_type[io->i_ctlr] = ((ipb->ippb_lbnhi & 0xff00) >> 8 );
	}
	if( ctlr_type[io->i_ctlr] != 0x82){  /* 4400 controller */
	    /*
	     * We've already checked that it's < 4.  If it's
	     * not a 4400, then it has to be 0 or 1
	     */
	    if( io->i_unit != 0 && io->i_unit != 1){
		printf("ip bad unit : %d\n", io->i_unit);
		goto bad;
	    }
	}
	if (ctlr_csh[io->i_ctlr] == 0)
	  vme_reserve_iomap( io->i_ctlr, ip, 32, &ctlr_csh[io->i_ctlr], 0 );
	
	if (vhbuf_sah == 0)
	  vme_iomap( ctlr_csh[io->i_ctlr], vhbuf, DEV_BSIZE,
		    GBA_CONTIG_ADDR+GBA_NOPART_MAP, &vhbuf_sah, &ioaddr );
	/*
	 * Try reading sector 0 of each track on cylinder 0 with a
	 * variety of uib setups until the volume header can be read
	 */
	for (trk = 0; trk < MAXTRK; trk++) {
		for (ipu = inituib; ipu < &inituib[NUIB]; ipu++) {
			((struct volume_header *)vhbuf)->vh_magic = 0;
			ipuib = *ipu;
			/*
			 * Do INITIALIZE command to try a uib setup
			 */
	 		if( !isinit[io->i_ctlr]){
			    ip_initiopb(ipb, io->i_unit);
			    ipb->ippb_cmdcode = IP_INITIALIZE;
			    wbflush();
			    /*
			     * CONTROLLER CAN ONLY ACCESS UIB VIA D16 REFERENCES
			     */
			    ipb->ippb_memtype = IPMT_16BIT;
			    wbflush();
			    if( ctlr_type[io->i_ctlr] == 0x82)
			        ipuib.ipu_options |= IPOP_4UNITSEL;
			    wbflush();
			    clear_cache(&ipuib,sizeof(struct ipuib));
			    vme_iomap( ctlr_csh[io->i_ctlr], &ipuib,
				      sizeof(struct ipuib),
				      GBA_CONTIG_ADDR+GBA_NOPART_MAP,
				      &ipuib_sah, &ioaddr );
			    ipb->ippb_bahi = HI16(ioaddr);
			    wbflush();
			    ipb->ippb_balo = LO16(ioaddr);
			    wbflush();

			    ip_unit_ready(io);
			    isinit[io->i_ctlr] = 1;
			    ip_runcmd(io,0);
			    vme_iounmap( ipuib_sah );
			    if (ipb->ippb_statcode != IPS_OK) {
				printf("ipc%dd%d error on INITIALIZE cmd\n",io->i_ctlr,io->i_unit);
				ip_error(ip);
				continue;
			    }

			    if( trk == 0 ){
			        /*
			         * Assume that drive needs to be recalibrated.
			         */
			        ip_initiopb(ipb, io->i_unit);
			        ipb->ippb_cmdcode = IP_RECAL;
			        wbflush();
			        ipb->ippb_memtype = 0;
			        wbflush();
			        ip_runcmd(io,1);
			        if (ipb->ippb_statcode != IPS_OK) {
				    printf("ipc%dd%d: recalibration of drive failed to clear fault\n",io->i_ctlr, io->i_unit);
				    ip_error(ip);
			        }
			        ip_initiopb(ipb, io->i_unit);
			        ipb->ippb_cmdcode = IP_SEEK;
			        wbflush();
			        ipb->ippb_cyl = 0;
			        wbflush();
			        ipb->ippb_trk = 0;
			        wbflush();
			        ipb->ippb_sec = 0;
			        wbflush();
			        ip_runcmd(io,1);
			        if (ipb->ippb_statcode != IPS_OK) {
				    printf("ipc%dd%d error on SEEK cmd\n",io->i_ctlr,io->i_unit);
				    ip_error(ip);
			        }
				/*
				 * Delay a bit to let the seek get a chance
				 * to do it's thing.
				 */
#ifndef SABLE
				DELAY(262144);
#endif !SABLE
			    }
			}

    
			/*
			 * read cyl 0, track trk, sector 0
			 */
			ip_initiopb(ipb, io->i_unit);
			ipb->ippb_cmdcode = IP_READ;
			wbflush();
			ipb->ippb_cyl = 0;
			wbflush();
			ipb->ippb_scnt = 1;
			wbflush();
			ka_to_ioaddr( vhbuf_sah, vhbuf, &ioaddr );
			ipb->ippb_balo = LO16(ioaddr);
			wbflush();
			ipb->ippb_trk = trk;
			wbflush();
			ipb->ippb_sec = 0;
			wbflush();
			ipb->ippb_bahi = HI16(ioaddr);
			wbflush();

			clear_cache(vhbuf, DEV_BSIZE);
#ifdef	R6000_FAKEIO
			r6000_fakeio_xfer( vhbuf_sah, vhbuf, DEV_BSIZE, 1);
#endif	R6000_FAKEIO			
			ip_runcmd(io,1);
			vme_ioflush( vhbuf_sah, vhbuf, DEV_BSIZE );
			if (ipb->ippb_statcode != IPS_OK) {
				printf("ipc%dd%d error reading cyl 0, sec 0, head %d\n",io->i_ctlr, io->i_unit, trk);
				ip_error(ip);
			} else {
				if (is_vh((struct volume_header *)vhbuf))
					goto gotvh;
			}
		}
	}
gotvh:
	if (trk == MAXTRK) {
		printf("ip(%d, %d, %d): can't read volume header\n",
		    io->i_ctlr, io->i_unit, io->i_part);
		/*
		 * NOTE: this must return successful so formatters can
		 * operate on raw disks
		 */
		return (0);
	}

	/*
	 * config as per dev params
	 */
	dp = &((struct volume_header *)vhbuf)->vh_dp;
	if (ip_config(io, dp, 1) < 0) /* enable RUNT sector */
		goto bad;

	*ipv = *(struct volume_header *)vhbuf;
checkunit:
	if (io->i_unit>=NUNITS){
		printf("ip bad unit number (volume)\n");
		goto bad;
	}
	if (ipv->vh_pt[io->i_part].pt_nblks == 0) {
		printf("ip bad partition\n");
		goto bad;
	}
	/*
	 * Try and figure out what type of file system is out there
	 */
	if (io->i_fstype == DTFS_AUTO)
		io->i_fstype = vh_mapfstype(ipv->vh_pt[io->i_part].pt_type);
	return (0);

bad:
	io->i_errno = ENXIO;
	return (-1);
}

/*
 * _dkipstrategy -- perform io
 */
_dkipstrategy(io, func)
register struct iob *io;
register int func;
{
	register volatile struct ipdevice *ip;
	register volatile struct ipiopb *ipb;
	register struct partition_table *pt;
	register unsigned lbn;
	register unit;
	struct device_parameters *dp;
	int cmd;
	sah_type io_sah;
	ioaddr_t ioaddr;
	u_short cyl;
	u_char hd;

	unit = io->i_unit & (NUNITS-1);
	ip = (struct ipdevice *)io->i_devaddr;
	ipb = &ip->ipiopb;

	/*
	 * fill in iopb
	 */
	ip_initiopb(ipb, io->i_unit);

	if ((func != FMTTRACKDATA) && (func != FMTTRACK) && 
	    (func != MAPTRACK) && (func != MAPSECTOR) && 
	    (func != READ_DEFECT) && (func != TRACKID)) {

	  	/* That leaves READ, WRITE, VERIFY, and DIAG */
	  
		pt = &ipvh[io->i_ctlr][unit].vh_pt[io->i_part];
		if (noecc[io->i_ctlr][io->i_unit])
			ipb->ippb_cmdopt |= IPO_LOGICAL;
		else
			ipb->ippb_cmdopt |= IPO_LOGICAL | IPO_ECCEN;
		wbflush();
		if ((unsigned)io->i_bn > pt->pt_nblks) {
			printf("read beyond end of partition\n");
			io->i_errno = ENXIO;
			return (-1);
		}
		lbn = io->i_bn + pt->pt_firstlbn;
	} else {
		lbn = io->i_bn;
	}
	ioaddr = K1_TO_PHYS(io->i_ma);	
	/*
	 * Any command that needs data to be transferred via a buffer must
	 * map that address into memory.
	 */
	if ((func == READ) || (func == WRITE) || (func == DIAG) ||
			( func == READ_DEFECT) || ( func == TRACKID)) {
	  if (!IS_KSEG1(io->i_ma))
	    clear_cache(io->i_ma, io->i_cc);
	  vme_iomap( ctlr_csh[io->i_ctlr], io->i_ma, io->i_cc,
		    GBA_CONTIG_ADDR+GBA_NOPART_MAP, &io_sah, &ioaddr );
	}
	if( func == TRACKID ){
	    /* 
	     * Must be in physical mode
	     */
	    if (noecc[io->i_ctlr][io->i_unit])
		ipb->ippb_cmdopt &= ~IPO_LOGICAL;
	    else
		ipb->ippb_cmdopt=(ipb->ippb_cmdopt | IPO_ECCEN) & ~IPO_LOGICAL;
	    wbflush();

	    dp = &Vh.vh_dp;
	    /* 
	     * It's really the number of tracks 
	     */
	    cyl = lbn / dp->dp_trks0;
	    hd = lbn % dp->dp_trks0;

	    ipb->ippb_lbnhi = cyl;
	    wbflush();
	    ipb->ippb_lbnlo = hd << 8;
	    wbflush();
	}else{
	    ipb->ippb_lbnhi = HI16(lbn);
	    wbflush();
	    ipb->ippb_lbnlo = LO16(lbn);
	    wbflush();
	}
	ipb->ippb_bahi = HI16(ioaddr);
	wbflush();
	ipb->ippb_balo = LO16(ioaddr);
	wbflush();

	switch (func) {
	case READ:
		if (io->i_cc % DEV_BSIZE) {
			printf("cc not multiple of sector size\n");
			io->i_errno = EIO;
			return (-1);
		}
		ipb->ippb_scnt = io->i_cc / DEV_BSIZE;
		wbflush();
		cmd = IP_READ;
		ipb->ippb_cmdcode = IP_READ;
		wbflush();
		break;

	case WRITE:
		if (io->i_cc % DEV_BSIZE) {
			printf("cc not multiple of sector size\n");
			io->i_errno = EIO;
			return (-1);
		}
		ipb->ippb_scnt = io->i_cc / DEV_BSIZE;
		wbflush();
		cmd = IP_WRITE;
		ipb->ippb_cmdcode = IP_WRITE;
		wbflush();
		break;

	case MAPTRACK:
		cmd = IP_MAPTRK;
		ipb->ippb_cmdcode = IP_MAPTRK;
		wbflush();
		ipb->ippb_scnt = HI16(io->i_cc);	/* rpl cyl */
		wbflush();
		ipb->ippb_bahi = LO16(io->i_cc);	/* rpl trk */
		wbflush();
		break;

	case MAPSECTOR:
		cmd = IP_MAPSEC;
		ipb->ippb_cmdcode = IP_MAPSEC;
		wbflush();
		break;

	case VERIFY:
		cmd = IP_VERIFY;
		ipb->ippb_cmdcode = IP_VERIFY;
		wbflush();
		ipb->ippb_scnt = io->i_cc;
		wbflush();
		break;

	case READ_DEFECT:
		cmd = IP_READCDCFLAW;
		ipb->ippb_cmdcode = IP_READCDCFLAW;
		wbflush();
		break;

	case TRACKID:
		cmd = IP_TRACKID;
		ipb->ippb_cmdcode = IP_TRACKID;
		wbflush();
		break;

	case FMTTRACK:
		cmd = IP_FORMATTRK;
		ipb->ippb_cmdcode = IP_FORMATTRK;
		wbflush();
		break;

	case FMTTRACKDATA:
		cmd = IP_FMTDATA;
		ipb->ippb_cmdcode = IP_FMTDATA;
		wbflush();
		break;


	case DIAG:
		cmd = IP_DIAG;
		ipb->ippb_cmdcode = IP_DIAG;
		wbflush();
		ipb->ippb_scnt = io->i_cc;
		wbflush();
		break;

	default:
		_io_abort("dkip bad function");
	}

	ip_runcmd(io,1);
	/*
	 * All commands with data transfers must unmap the address.
	 */
	if ((func == READ) || (func == WRITE) || (func == DIAG) ||
			( func == READ_DEFECT) || ( func == TRACKID)) {
	  vme_iounmap( io_sah );
	}
	/*
	 * All command that read must clear cache
	 */
	if (((func == READ) || (func == READ_DEFECT) || (func == TRACKID))
		&& (!IS_KSEG1(io->i_ma)))
		clear_cache(io->i_ma, io->i_cc);

	/*
	 * check for errors
	 */
	if (func == DIAG) {
		io->i_bn = (ipb->ippb_lbnhi << 16) | ipb->ippb_lbnlo;
		io->i_cc = ipb->ippb_scnt;
		io->i_ma = (char *)((ipb->ippb_bahi << 16) | ipb->ippb_balo);
	}
	if (ipb->ippb_statcode != IPS_OK) {
		if ((func == READ) || (func == WRITE))
			printf("ipc%dd%d error on cmd 0x%x, lbn = %d\n",io->i_ctlr, io->i_unit, cmd,lbn);
		else
			printf("ipc%dd%d error on cmd 0x%x\n",io->i_ctlr, io->i_unit, cmd);
		ip_error(ip);
		if (func == DIAG) {
			io->i_errno = ipb->ippb_errcode;
		} else if (ipb->ippb_statcode == IPS_EXCEPT) {
			goto ok;
		} else {
			io->i_errno = EIO;
		}
		return (-1);
	}
ok:
	return (io->i_cc);
}

/*
 * _dkipioctl -- io controls
 */
_dkipioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	register error = 0;
	register struct volume_header *ipv;
	register struct media_defect *md;
#ifdef notdef
	register struct fmt_info *fi;	/* where does fi get initialized?? */
	register oldflags;
#endif

	ipv = &ipvh[io->i_ctlr][io->i_unit & (NUNITS-1)];

	switch (cmd) {
		
	case DIOCGETVH:
		bcopy(ipv, arg, sizeof(*ipv));
		break;

	case DIOCSETVH:
		bcopy(arg, ipv, sizeof(*ipv));
		if (ipv->vh_dp.dp_spare0)  /* have we got a runt sector? */
			error = ip_config(io, &ipv->vh_dp, 1); /* enable runt */
		else
			error = ip_config(io, &ipv->vh_dp, 0);
		break;

	case DIOCGETCTLR:
		/*
		 * fill-in and return a controller capability struct
		 */
		strncpy(((struct ctlr_info *)arg)->ci_type,ip_name,CITYPESIZE);
		((struct ctlr_info *)arg)->ci_flags
		    = DP_SECTSLIP|DP_TRKFWD|DP_MULTIVOL;
		break;

	case DIOCNOECC:
		noecc[io->i_ctlr][io->i_unit] = *(int *)arg;
		break;

#ifdef notdef
	case DIOCREFMTTRK:
		/*
		 * Like DIOCFMTMAP but read contents of track first, then
		 * format, then rewrite contents
		 *
		 * SHOULD VERIFY THAT lbn FIRST BLOCK OF A TRACK
		 */
		/*
		 * temporarily change configuration to allow moving
		 * bad data
		 */
		oldflags = ipv->vh_dp.dp_flags;
		ipv->vh_dp.dp_flags |= DP_IGNOREERRORS;
		if (error = ip_config(io, &ipv->vh_dp))
			break;

		/*
		 * read contents of bad track
		 */
		io->i_ma = fwdbuf;
		io->i_bn = fi->fi_lbn;
		io->i_cc = ipv->vh_dp.dp_secbytes * ipv->vh_dp.dp_secs;
		_dkipstrategy(io, READ);	/* ignore errors */

		/*
		 * reset configuration to inhibit reading bad data
		 */
		ipv->vh_dp.dp_flags = oldflags;
		if (error = ip_config(io, &ipv->vh_dp))
			break;

		/*
		 * Format track as per fmt_info pointed to by arg
		 * Ignore errors, we're committed when trying format
		 */
		ip_frmtrk(io, arg);

		/*
		 * Write data to newly formatted track
		 */
		io->i_ma = fwdbuf;
		io->i_bn = fi->fi_lbn;
		io->i_cc = ipv->vh_dp.dp_secbytes
		    * ipv->vh_dp.dp_secs;
		_dkipstrategy(io, WRITE);
		if (io->i_errno)
			error = -1;
		break;
#endif

	case DIOCFMTMAP:
		error = ip_fmtmap(io, arg);
		break;

	case DIOCVFYSEC:
		/*
		 * verify sectors are readable (non-distructive)
		 */
		io->i_bn = ((struct io_arg *)arg)->sectst;
		io->i_cc = ((struct io_arg *)arg)->datasz;
		if (_dkipstrategy(io, VERIFY) < 0)
			error = -1;
		break;

	case DIOCDIAG:
		io->i_bn = ((struct io_arg *)arg)->sectst;
		io->i_cc = ((struct io_arg *)arg)->datasz;
		io->i_ma = (char *)((struct io_arg *)arg)->memaddr;
		if (_dkipstrategy(io, DIAG) < 0) {
			((struct io_arg *)arg)->retval = io->i_errno;
			io->i_errno = EIO;
			error = -1;
		} else {
			((struct io_arg *)arg)->retval = 0;
		}
		((struct io_arg *)arg)->sectst = io->i_bn;
		((struct io_arg *)arg)->datasz = io->i_cc;
		((struct io_arg *)arg)->memaddr = (unsigned long)io->i_ma;
		break;

	case DIOCRDEFECTS:
		/*
		 * read defect information off a track
		 */
		md = (struct media_defect *)arg;
		io->i_bn = (md->md_cyl << 16) | (md->md_trk << 8);
		io->i_ma = def_buf;
		io->i_cc = sizeof(struct md_entry);
		if (_dkipstrategy(io, READ_DEFECT) < 0) {
			error = -1;
		} else {
			bcopy(def_buf, (caddr_t)&md->md_entry, 
			    sizeof(struct md_entry));
		}
		break;
	case DIOCTRKID:
		/*
		 * Get sector headers starting at index pulse
		 */
		 io->i_bn = ((struct io_arg *)arg)->sectst;
		 io->i_ma = (char *) ((struct io_arg *)arg)->memaddr;
		 io->i_cc = ipv->vh_dp.dp_secs * sizeof(struct track_id);
		 if (_dkipstrategy(io,TRACKID) < 0 )
			error = -1;
		 break;

	default:
		io->i_errno = EINVAL;
		error = -1;
		break;
	}
	return (error);
}

/*
 * initialize controller with device parameter information
 */
static
ip_config(io, dp, runt)
register struct iob *io;
register struct device_parameters *dp;
int runt;
{
	register volatile struct ipdevice *ip
	    = (struct ipdevice *)io->i_devaddr;
	register volatile struct ipiopb *ipb = &ip->ipiopb;
	sah_type ipuib_sah;
	ioaddr_t ioaddr;

	ipuib.ipu_v0sh = dp->dp_shd0;
	ipuib.ipu_v0nh = dp->dp_trks0;
	ipuib.ipu_v1sh = dp->dp_shd1;
	ipuib.ipu_v1nh = dp->dp_trks1;
	ipuib.ipu_sectrk = dp->dp_secs;
	ipuib.ipu_skew = dp->dp_skew;
	ipuib.ipu_bytsechi = HI8(dp->dp_secbytes);
	ipuib.ipu_bytseclo = LO8(dp->dp_secbytes);
	ipuib.ipu_gap1 = dp->dp_gap1;
	ipuib.ipu_gap2 = dp->dp_gap2;
	ipuib.ipu_intrlv = dp->dp_interleave;
	ipuib.ipu_retries = dp->dp_nretries;
	ipuib.ipu_cylhi = HI8(dp->dp_cyls);
	ipuib.ipu_cyllo = LO8(dp->dp_cyls);
	if (runt)
		ipuib.ipu_attrib = IPAT_INCBYHEAD | IPAT_RUNTSECEN;
	else
		ipuib.ipu_attrib = IPAT_INCBYHEAD;
	if (dp->dp_flags & DP_RESEEK) 
		ipuib.ipu_attrib |= IPAT_RESEEK;
	if (dp->dp_flags & DP_SECTSLIP)
		ipuib.ipu_attrib |= IPAT_SPSECEN;
	if (dp->dp_flags & DP_IGNOREERRORS)
		ipuib.ipu_attrib |= IPAT_MVBADDATA;
	ipuib.ipu_options = 0;
	ipuib.ipu_statipl = 0;
	ipuib.ipu_statvec = 0;
	if( ctlr_type[io->i_ctlr] == 0x82)
	       ipuib.ipu_options |= IPOP_4UNITSEL;
	if( dp->dp_cyls >= 1024 )
	       ipuib.ipu_options |= IPOP_EXTADDR;

	ip_initiopb(ipb, io->i_unit);
	/*
	 * CONTROLLER CAN ONLY ACCESS UIB VIA D16 REFERENCES
	 */
	ipb->ippb_memtype = IPMT_16BIT;
	wbflush();
	ipb->ippb_cmdcode = IP_INITIALIZE;
	wbflush();
	clear_cache(&ipuib, sizeof(struct ipuib));
	vme_iomap( ctlr_csh[io->i_ctlr], &ipuib, sizeof(struct ipuib),
		  GBA_CONTIG_ADDR+GBA_NOPART_MAP, &ipuib_sah, &ioaddr );
	ipb->ippb_bahi = HI16(ioaddr);
	wbflush();
	ipb->ippb_balo = LO16(ioaddr);
	wbflush();

	ip_runcmd(io,1);
	vme_iounmap( ipuib_sah );
	if (ipb->ippb_statcode != IPS_OK) {
		printf("ipc%dd%d error initializing controller\n",io->i_ctlr, io->i_unit);
		ip_error(ip);
		return (-1);
	}
	return (0);
}

/*
 * perform track formatting, track mapping, and sector slipping
 */
static
ip_fmtmap(io, fmi)
register struct iob *io;
register struct fmt_map_info *fmi;
{
	register int error = 0;

	io->i_errno = 0;

	switch (fmi->fmi_action) {
	case FMI_FORMAT_TRACK_WDATA:
		/*
		 * format a track 
		 */
		io->i_bn = (fmi->fmi_cyl << 16) | (fmi->fmi_trk << 8);
		io->i_ma = fmi->fmi_addr;
		_dkipstrategy(io, FMTTRACKDATA);
		break;

	case FMI_FORMAT_TRACK:
		/*
		 * format a track 
		 */
		io->i_bn = (fmi->fmi_cyl << 16) | (fmi->fmi_trk << 8);
		io->i_ma = (char *)0xdb6db6db;	/* fill pattern */
		_dkipstrategy(io, FMTTRACK);
		break;

	case FMI_MAP_TRACK:
		/*
		 * map a track 
		 */
		io->i_bn = (fmi->fmi_cyl << 16) | (fmi->fmi_trk << 8);
		io->i_cc = (fmi->fmi_rplcyl << 16) | (fmi->fmi_rpltrk << 8);
		io->i_ma = (char *)0xdb6db6db;	/* fill pattern */
		_dkipstrategy(io, MAPTRACK);
		break;

	case FMI_SLIP_SECTOR:
		/*
		 * slip a sector
		 */
		io->i_bn = (fmi->fmi_cyl << 16) | (fmi->fmi_trk << 8) |
			   (fmi->fmi_sec);
		_dkipstrategy(io, MAPSECTOR);
		break;

	default:
		io->i_errno = EINVAL;
	}

	if (io->i_errno)
		error = -1;
	return (error);
}

static
ip_error(ip)
register volatile struct ipdevice *ip;
{
	register volatile struct ipiopb *ipb = &ip->ipiopb;
	int whichctlr();

	printf("dkip error: csr=%r ", ip->ipcsr, ipcsr_desc);
	switch (ipb->ippb_statcode) {

	case IPS_OK:
		printf("software error\n");
		break;

	case IPS_INPROG:
		printf("in progress\n");
		break;

	case IPS_EXCEPT:
		printf("recovered %r\n", ipb->ippb_errcode, ipexcept_desc);
		break;
	
	case IPS_ERROR:
		printf("unrecovered %R\n", ipb->ippb_errcode, iperror_desc);
		break;

	default:
		printf("unknown errcode\n");
		break;
	}
	if( ctlr_type[whichctlr(ip)] != 0x82 ){
	    printf("Drive 0 status: %r\n", ip->ipds0, ipds_desc);
	    printf("Drive 1 status: %r\n", ip->ipds1, ipds_desc);
	}else{
	    printf("Drive 0 status: %r\n", ip->ipdsr[3], ipds_desc);
	    printf("Drive 1 status: %r\n", ip->ipdsr[2], ipds_desc);
	    printf("Drive 2 status: %r\n", ip->ipdsr[1], ipds_desc);
	    printf("Drive 3 status: %r\n", ip->ipdsr[0], ipds_desc);
	}
}

/*
 * Wait for controller and drive to become ready.
 */
static
ip_waitready(io,checkunit)
register struct iob *io;
int checkunit;
{
	register volatile struct ipdevice *ip;
	register volatile struct ipiopb *ipb;
int i; 
short *tmp;

	ip = (struct ipdevice *)io->i_devaddr;
	ip_ctlr_ready(io);  
#ifdef DEBUG
	dkipsave(ip);
	ipb = &ipsave.ipiopb;
  	for( i = 0,tmp = (short *)&ipsave; 
			i < (sizeof( struct ipiopb ) >> 1 ) + 1; i++)
            printf("%x ", *(tmp + i) & 0xffff);
        printf("0:%x 1:%x SC:%x\n", ipsave.ipdsr[3] & 0xff, 
		ipsave.ipdsr[2] & 0xff, ipsave.ipscr & 0xffff);
#endif

	andh_rmw(&ip->ipcsr, ~(IPCS_OPDONE|IPCS_ERLST|IPCS_SC|IPCS_BERR));
	if( checkunit)
	    ip_unit_ready(io);
	    
}

/*
 * Initialize command independent iopb fields.
 */
static
ip_initiopb(ipb, unit)
register volatile struct ipiopb *ipb;
register int unit;
{
	int ctlr;
	/*
	 * command independent setup
	 * NOTE: writes are ordered to reduce number of write buffer flushes.
	 */
	/* really iopb which is ip plus 4*/
	ctlr = whichctlr((int)ipb & 0xff00);
	ipb->ippb_cmdopt = 0;
	wbflush();
	ipb->ippb_statcode = 0;
	wbflush();
	ipb->ippb_errcode = 0;
	wbflush();
	ipb->ippb_memtype = IPMT_32BIT;
	wbflush();
	ipb->ippb_addrmod = VME_A32NPAMOD;
	wbflush();
	ipb->ippb_dmaburst = 0;
	wbflush();
	ipb->ippb_errvec = 0;
	wbflush();
	ipb->ippb_iopblo = 0;
	wbflush();
	ipb->ippb_skew = 0;
	wbflush();
	ipb->ippb_sgentries = 0;
	wbflush();
	ipb->ippb_errcode = 0;
	wbflush();
	ipb->ippb_statcode = 0;
	wbflush();
	ipb->ippb_ipl = 0;
	wbflush();
	ipb->ippb_normvec = 0;
	wbflush();
	ipb->ippb_iopbhi = 0;
	wbflush();
	ipb->ippb_pbmemtype = IPMT_32BIT;
	wbflush();
	ipb->ippb_pbaddrmod = VME_A32NPAMOD;
	wbflush();
	if( ctlr_type[ctlr] != 0x82 ){
	    if (unit & 1){
		ipb->ippb_cmdopt |= IPO_UNIT;
	    }
	}else
	    ipb->ippb_ipl |= (unit << 4);
	wbflush();
}

/*
 * Issue a command to the controller and wait for its completion.
 */
static
ip_runcmd(io,checkunit)
register struct iob *io;
register int checkunit;
{
	register volatile struct ipdevice *ip;
	register int i;
	int last_csr, old_csr;

	ip = (struct ipdevice *)io->i_devaddr;
	i = 0;
	do {
		_scandevs();
	} while ((ip->ipcsr & IPCS_GOBUSY) && (++i < WAITLOOPS));
	if (i >= WAITLOOPS)
	  printf("timeout: still executing previous command\n");

#ifdef DKDEBUG
vjfrom_shio( ip, &Ip, sizeof( struct ipdevice));
#endif
	orh_rmw(&ip->ipcsr, IPCS_GOBUSY);
	i = 0;
#ifndef SABLE	
	DELAY(1000);
#endif SABLE	
	do {
	  	/* This code protects us against bogus values from I/O
		 * subsystem hardware (i.e. it's not really returning the
		 * controller csr).
		 */
	  	last_csr = ip->ipcsr;
	  	if ((last_csr != (IPCS_BOK | IPCS_GOBUSY)) &&
		    (last_csr != (IPCS_BOK | IPCS_OPDONE)) &&
		    (last_csr != (IPCS_BOK | IPCS_OPDONE | IPCS_ERLST))) {
		  if (!IS_RB3125)
		  	printf("bogus ip->ipcsr: 0x%x...", last_csr);
		  /* Keep reading until get same value twice */
		  do {
		    old_csr = last_csr;
		    last_csr = ip->ipcsr;
		    if (!IS_RB3125)
		    	printf(" current csr 0x%x  previous csr 0x%x\n",
			   	last_csr, old_csr );
		  } while (old_csr != last_csr);
		}
		/* Finally, we get back to normal scanning */
		_scandevs();
	} while ((last_csr & IPCS_GOBUSY) && (++i < WAITLOOPS));
	if (i >= WAITLOOPS)
	  printf("timeout: controller still executing command\n");
	i = 0;
	do {
	  	/* This code protects us against bogus values from I/O
		 * subsystem hardware (i.e. it's not really returning the
		 * controller csr).
		 */
	  	last_csr = ip->ipcsr;
	  	if ((last_csr != (IPCS_BOK | IPCS_GOBUSY)) &&
		    (last_csr != (IPCS_BOK | IPCS_OPDONE)) &&
		    (last_csr != (IPCS_BOK | IPCS_OPDONE | IPCS_ERLST))) {
		  printf("bogus ip->ipcsr: 0x%x...", last_csr);
		  /* Keep reading until get same value twice */
		  do {
		    old_csr = last_csr;
		    last_csr = ip->ipcsr;
		    printf(" current csr 0x%x  previous csr 0x%x\n",
			   last_csr, old_csr );
		  } while (old_csr != last_csr);
		}
		/* Finally, we get back to normal scanning */
		_scandevs();
	} while (!(last_csr & IPCS_OPDONE) && (++i < WAITLOOPS));
	if (i >= WAITLOOPS)
	  printf("timeout: BUSY cleared but OPDONE not set\n");
	ip_waitready(io,checkunit);

}

/*
 * wait for controller to become ready.
 */
ip_ctlr_ready(io)
register struct iob *io;
{
	register volatile struct ipdevice *ip;
	register int i;
	int numresets = 0;
	int last_csr=0;

	ip = (struct ipdevice *)io->i_devaddr;
	if ((last_csr = ip->ipcsr) & IPCS_GOBUSY) {
		printf("waiting for ip(%d, %d, %d)(csr 0x%x) to come ready...",
		    io->i_ctlr, io->i_unit, io->i_part, last_csr);
		i = 0;
		while (ip->ipcsr & IPCS_GOBUSY) {
			_scandevs();
			if (++i > WAITLOOPS) {
			  	ip->ipcsr = IPCS_BDCLR;
				wbflush();
				DELAY(64);
				ip->ipcsr = 0;
				wbflush();
				printf("issued controller reset...");
				if (++numresets > 5) {
					printf("NEVER ready!\n");
					return;
				}
				for (i=0; i < 524288; i++)
				  _scandevs();
				i = 0;
			}
		}
		printf("ready!\n");
	}
}

/*
 * wait for unit to become ready
 */
ip_unit_ready(io)
register struct iob *io;
{
	register volatile struct ipdevice *ip;
	register volatile unsigned char *ipds;
	register volatile struct ipiopb *ipb;
	register int i;

	ip = (struct ipdevice *)io->i_devaddr;
	if( !isinit[io->i_ctlr] ){
	    /* 
	     * spinning up is a special case, since we haven't configured
	     * the controller for 4 ports.  If it's drive 0 or 1, pretend
	     * it's a 2 port board.
	     */
	    if( io->i_unit < 2 )
		ipds = io->i_unit ? &ip->ipds1 : &ip->ipds0;
	    else{
		/*
		 * Well, first send the command, but if don't wait for the
		 * unit to come ready, since it could still be spinning up.
		 */
		ip_runcmd(io,0);
		/*
		 * Now that that's done, we can check the status bytes
		 * in the correct place
		 */
	        ipds = &ip->ipdsr[3 - io->i_unit];
	    }
	}else if( ctlr_type[io->i_ctlr] != 0x82 )
	    ipds = io->i_unit ? &ip->ipds1 : &ip->ipds0;
	else
	    ipds = &ip->ipdsr[3 - io->i_unit];

	/* 
	 * We're too fast,  try once and then delay before 
	 * printing message.
	 */
	if ((*ipds & IPDS_URDY) == 0)
#ifndef SABLE	  
		DELAY(50000);
#endif SABLE	

	if ((*ipds & IPDS_URDY) == 0) {
		printf("clearing drive fault on ip(%d, %d, %d) ...",
		    io->i_ctlr, io->i_unit, io->i_part);
recal:
		if (*ipds & (IPDS_FAULT|IPDS_SKERR)) {
			ipb = &ip->ipiopb;
			ip_initiopb(ipb, io->i_unit);
			ipb->ippb_cmdcode = IP_RECAL;
			wbflush();
			/*
			 * Call with 0, since if checkunit is 1,
			 * it will recursively call ip_unit_ready
			 */
			ip_runcmd(io,0);
			if (ipb->ippb_statcode != IPS_OK) {
				printf("ipc%dd%d: recalibration of drive failed to clear fault\n",io->i_ctlr, io->i_unit);
				ip_error(ip);
			}
		}
		i = 0;
		while ((*ipds & IPDS_URDY) == 0) {
			_scandevs();
			if (++i > WAITLOOPS)
				goto recal;
		}
		printf("cleared!\n");
#ifndef SABLE		
		DELAY(10000);
#endif SABLE		
	}
}
dkipsave(ip)
	register volatile struct ipdevice *ip;
{
	
	dkcpy(ip,&ipsave,sizeof(struct ipdevice));
}

dkcpy(src, dst, cnt)
register volatile short *src, *dst;
register short cnt;
{
    cnt >>= 1;                          /* divied by 2 */
    while (cnt--) {
        *dst++ = *src++;
    }
}
#ifdef DBXDBG
dkipprt(ip)
	register volatile struct ipdevice *ip;
{
	register int i,*ptr;
	for( i = 0, ptr = (int *)&ipsave; i < 4; i++)
		printf("%.8x %.8x %.8x %.8x\n", 
		*(ptr+i), *(ptr+i+1), *(ptr+i+2), *(ptr+i+3));
	printf("\n");
}

#endif
whichctlr(ip)
    struct ipdevice *ip;
{
	register int i;

	for( i = 0; i < NCTLRS; i++)
	   if( (int)ipstd[i] == ((int)ip & 0xffff))
	       break;
	return(i);
}
