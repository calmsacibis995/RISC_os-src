#ident "$Header: format.c,v 1.28.3.2 91/01/10 10:20:55 chungc Exp $"

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

/*
 * format.c - routines for disk formatting and bad block handling.
 *	      also sets up volume header information.
 */

#ifdef STANDALONE
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "sys/file.h"
#include "sys/errno.h"
#include "netinet/in.h"
#include "machine/dvh.h"
#include "mips/cpu_board.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/ctype.h"
#else STANDALONE
#include "sys/errno.h"
#include "sys/types.h" 
#include "sys/fcntl.h" 
#include "sys/dvh.h" 
#include "sys/ioctl.h" 
#include "sys/dkio.h" 
#include <stdio.h> 
#include <ctype.h>
#endif STANDALONE

extern int errno;

#define PHYS_TO_LOG	0
#define LOG_TO_PHYS	1
#define SLIP_PTRN	0xff

int qcompar();
unsigned digit();
extern char *index();
extern char *gets();
extern char *atob();
static char *btoa();
struct volume_directory *bst_allocsp();

/* drive interface */
#define SMD		0
#define ESDI		1
#define SCSI		2
#define ON_BOARD       16
#define ALL		3	/* Default case (used for other) */

#define CYLS_PER_GROUP	16
/* 
 * default settings for file system independent partitions 
 */
#define PTNUM_VOLHDR		8		/* volume header partition */
#define PTNUM_REPL		9		/* trk/sec replacement part */
#define PTNUM_VOLUME		10		/* entire volume partition */
#define PTSIZE_VHDR		1000000		/* 1 Megabyte for volhdr */

/*  
 * default settings for file system partitions 
 */
#define PTNUM_FSFIRST		0		/* first partition */
#define PTNUM_FSLAST		15		/* last partition */
#define PTNUM_FSROOT		0		/* root partition 'a' */
#define PTNUM_FSSWAP		1		/* swap partition 'b' */
#define PTNUM_FSALL		2		/* one big partition 'c' */
#define PTNUM_FSUSRd		3		/* usr partition 'd' */
#define PTNUM_FSUSRe		4		/* usr partition 'e' */
#define PTNUM_FSUSRf		5		/* usr partition 'f' */
#define PTNUM_FSUSRg		6		/* usr partition 'g' */
#define PTNUM_FSUSRh		7		/* usr partition 'h' */
#define PTNUM_FSUSRi		11		/* usr partition 'i' */
#define PTNUM_FSUSRj		12		/* usr partition 'j' */
#define PTNUM_FSUSRk		13		/* usr partition 'k' */
#define PTNUM_FSUSRl		14		/* usr partition 'l' */
#define PTNUM_FSUSRm		15		/* usr partition 'm' */

#define PTSIZE_ROOT		20000000	/* 20 Megabytes for root */
#define PTSIZE_SWAP		20000000	/* 20 Megabytes for swap */
#define PTSIZE_USRh		25000000	/* 25 Megabytes for more swap */
#define MIN_SIZE		 2500000	/*2.5 Megabytes for cg tuning */

/*
 * misc defines 
 */
#define BSTTAB_NAME	"bsttab"
#define LINESIZE	32
#define MAX_SCANPASS	3
#define MAX_BADBLOCKS	4000	/* Max for total # of defects on the disk */
#define MAX_SCSIBADBLOCKS	500	/* what should this be? */
#define MAX_NSECS	84	/* must include slip sector	*/
#define MAX_MTRKS	84	/* This is the maximum number of tracks
				 * in the TRKREPL partition.  There are
				 * 4 cylinders for the 2392, so that means
				 * that the max is 84.  This will change
				 * with larger drives 
				 */
#define INVALID	 	-1	

int fd;
int nbad;
int scsinbad;
int device, volpart;
int dpnretries;
int dpreseek;
int startcyl = INVALID;
int totalcyl = INVALID;
struct io_arg io_arg;
extern struct volume_header Vh;
struct ctlr_info ct;
GEOMETRY_INFO geom;
struct bst_table bsttab[MAX_BADBLOCKS];
struct partition_table *rplpart;
struct fmtdevtype {
	char *devname;
	int ctlrtype;
} fmtdevtype[] = {
	"dkip", SMD,
	"dkis", SCSI|ON_BOARD,
	"dksd", SCSI|ON_BOARD,
	"dkij", SCSI,
	0,0
};

struct track_id *track_id;
int ctlrtype;


/*
 * bad block list entry (used internally)
 */
struct bb_entry {
	u_short		cyl;
	u_char		trk;
	u_char		sec;
	int		rpltype;
	int		rpllbn;
} bb[MAX_BADBLOCKS];

struct reassign {
	char	pad;
	char 	pad1;
	u_short length;
	u_int   scsibb[MAX_SCSIBADBLOCKS];
} reassign;

/* Following data blocks need low memory (i.e. non-stack) addresses for the
 * M6000 since I/O transfers only occur into lowest 16MB of memory.
 */

char wptr_buff[DEV_BSIZE * MAX_NSECS + 64];
char wptr_in_buff[DEV_BSIZE * MAX_NSECS + 64];

char *wptr;				/* used in bb_scan & scsi_scan */
char *wptr_in;				/* used in scsi_scan */

/*
 * mtrk - contains an entry for every track in the replacment partition.
 * During formatting, it is set to one of the following 3 values.
 * btrk - a list of track numbers of bad tracks to map.
 */
char mtrk[MAX_MTRKS];			
#define NOTMAPPED	0
#define MAPPED		1
#define BBMAPPED	2
struct btrk {
    u_short	cyl;
    u_char	hd;
} btrk[MAX_MTRKS];

/*
 * known types of disk drives
 */
#define DEV_F2322	0
#define DEV_F2333	1       /* 63 sectors */
#define DEV_F2333R	2       /* 64 sectors */
#define DEV_F2344	3       /* 63 sectors */
#define DEV_F2344R	4       /* 64 sectors */
#define DEV_F2372       5       /* 63 sectors */
#define DEV_F2372R      6       /* 64 sectors */
#define DEV_F2392	7	/* 83 sectors */
#define DEV_F2392R	8	/* 80 sectors */
#define DEV_94161       9
#define DEV_94171      10
#define DEV_94181      11
#define DEV_94191      12
#define DEV_94351      13
#define DEV_94601      14
#define DEV_OTHER      15
#define MAX_DEVICES    16

struct dev_info {
	int	di_value;
	char	*di_name;
};

/* 
 * table of device parameters for known types of disk drives.
 *
 * NOTE: if slip sectoring is enabled, the number of sectors per
 *	track in the table below should be number of sectors - 1
 * 	due to the slip sector
 *
 * NOTE: order corresponds to DEV_ defines 
 *
 * NOTE: sectors/track must be <= to MAX_NSECS defined above
 *
 * Since the spare0 contains the runt info, use it.
 */
#define dp_runt	dp_spare0
#define dp_type	dp_spare2
struct device_parameters dp_defaults[] = {
/* 
 *sk gp1 gp2 runt cyls h0 trk0 h1 trk1 secs byts il flgs dr nr mpw type sp3 sp4 
 */
  {0, 12, 12,  1,  823, 0, 10, 0,  0,  32,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  0,  823, 0, 10, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  1,  823, 0, 10, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  0,  624, 0, 27, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  1,  624, 0, 27, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  0,  745, 0, 27, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  1,  745, 0, 27, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {4,  9, 10,  0, 1916, 0, 21, 0,  0,  83,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0, 12, 12,  0, 1916, 0, 21, 0,  0,  80,  512, 1, 0x25, 0, 3, 0,  SMD, 0, 0},
  {0,  0,  0,  0,  967, 0,  9, 0,  0,  35,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0, 1482, 0,  9, 0,  0,  48,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0,  768, 0, 15, 0,  0,  56,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0, 1542, 0, 15, 0,  0,  56,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0, 1065, 0,  9, 0,  0,  35,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0, 1689, 0, 15, 0,  0,  80,  512, 1,    0, 0, 0, 0, SCSI, 0, 0},
  {0,  0,  0,  0,    0, 0,  0, 0,  0,   0,    0, 0,    0, 0, 0, 0,  ALL, 0, 0}
};

/*
 * milliseconds per word on data transfer, used by unix commands like iostat(1).
 * NOTE: order corresponds to DEV_ defines 
 */
int dev_mspw[] = {
	132,
	132,
	132,
	132,
	132,
	132,	/* what should this be??? */
	132,	/* what should this be??? */
	132,	/* what should this be??? */
	132,	/* what should this be??? */
	260, /* scsi */
	260, /* scsi */
	260, /* scsi */
	260, /* scsi */
	260, /* scsi */
	260, /* scsi (94601) */
	0
};

/* 
 * names of disk drives for which we know the device parameters 
 * NOTE: order corresponds to DEV_ defines
 */
char *dev_names[] = {
	"fuji 2322  (170Meg unfmtd, 32 sectors)",
	"fuji 2333  (337Meg unfmtd, 63 sectors)",
	"fuji 2333  (337Meg unfmtd, 64 sectors)",
	"fuji 2344  (689Meg unfmtd, 63 sectors)",
	"fuji 2344  (689Meg unfmtd, 64 sectors)",
	"fuji 2372  (824Meg unfmtd, 63 sectors)",
	"fuji 2372  (824Meg unfmtd, 64 sectors)",
	"fuji 2392 (2027Meg unfmtd, 83 sectors)",
	"fuji 2392 (2027Meg unfmtd, 80 sectors)",
	"cdc  94161  (156Meg fmtd, SCSI, Wren III)",
	"cdc  94171  (328Meg fmtd, SCSI, Wren IV)",
	"cdc  94181  (330Meg fmtd, SCSI, Fast Access, Wren V)",
	"cdc  94191  (663Meg fmtd, SCSI, Wren VI)",
	"cdc  94351  (172Meg fmtd, SCSI, Swift )",
	"cdc  94601 (1037Meg fmtd, SCSI, IMPRIMIS )",
	"other"
};

/*
 * device parameterization information.
 * NOTE: order corresponds to DEV_ defines
 *
 * NOTE: max num defects must be <= MAX_BADBLOCKS
 */
struct device_info {
	int	trkbytes;		/* number of bytes in a track */
	int	length;			/* max defect length in bits */
	int	mdeftrks;		/* max num defective tracks */
	int	mdefects;		/* max num defects */
	int 	realbps;		/* real total bytes per sector */
} di_defaults[] = {
	{ 20480,	512,	28,	200,	603	}, /* 2322 */
	{ 40960,	512,	32,	600,	640	}, /* 2333-63 */
	{ 40960,	512,	32,	600,	622	}, /* 2333-64 */
	{ 40960,	512,	45,	700,	640	}, /* 2344-63 */
	{ 40960,	512,	45,	700,	621	}, /* 2344-64 */
	{ 40960,	512,	45,	700,	640	}, /* 2372-63 */
	{ 40960,	512,	45,	700,	622	}, /* 2372-64 */
	{ 50400,	512,	66,	1000,	600	}, /* 2392-83 */
	{ 50400,	512,	66,	1000,	622	}, /* 2392-80 */
	{     0,	0,	0,	0,	0	}, /* 94161 scsi */
	{     0,	0,	0,	0,	0	}, /* 94171 scsi */
	{     0,	0,	0,	0,	0	}, /* 94181 scsi */
	{     0,	0,	0,	0,	0	}, /* 94191 scsi */
	{     0,	0,	0,	0,	0	}, /* 94351 scsi */
	{     0,	0,	0,	0,	0	}, /* 94601 scsi */
	{     0,	0,	0,	0	}
};

#define MAX_ATTRIBUTES	6
struct dev_info dev_attribs[] = {
	/* value		name 					*/
	{  DP_SECTSLIP,		"sector slipping"			},
	{  DP_SECTFWD,		"sector forwarding"			},
	{  DP_TRKFWD,		"track forwarding"			},
	{  DP_MULTIVOL,		"multiple volumes"			},
	{  DP_IGNOREERRORS,	"transfering data regardless of errors"	},
	{  DP_RESEEK,		"recalibrate as last resort"		}
};

char attr_on[MAX_ATTRIBUTES];
/*
 * names corresponding to types of partitions
 */
#define NUM_PTYPES	7
char *part_types[] = {
	"volume header  ",
	"track replace  ",
	"sector replace ",
	"raw data       ",
	"BSD file sys   ",
	"SysV file sys  ",
	"entire volume  "
};

extern	char	*getenv();

/*
 * main routine
 */
main(argc, argv)
int argc;
char **argv;
{
	char linebuf[LINESIZE];
	char *cp;
	int partition;
	struct device_parameters *dp = &Vh.vh_dp;
#ifdef STANDALONE
	extern char *stand_version;
	int	flag;
#endif STANDALONE

	fd = INVALID;
	nbad = INVALID;
	scsinbad = INVALID;
	device = INVALID;
	rplpart = NULL;
	bsttab[0].bt_rpltype = BSTTYPE_EMPTY;

	wptr = (char *)((int)&wptr_buff[63] & ~63);
	wptr_in = (char *)((int)&wptr_in_buff[63] & ~63);
	track_id = (struct track_id *)align_malloc( MAX_NSECS * sizeof( struct track_id), 512);

#ifdef STANDALONE
	cp = getenv("flag");
	flag = atoi( cp ? cp : "0" );

	if ( flag & 0x1 ) {
		printf("\nStandalone Format Utility\n");
	} else {
		printf("\nMIPS Format Utility\n");
	}
	printf("%s\n", stand_version);
#else STANDALONE
	printf("\nMIPS Format Utility\n");
#endif STANDALONE
	printf("\n\nWARNING: Please refer to the Technical Reference Manual\n");
	printf("\t for this machine before running this program.\n");
	printf("\t Certain responses to many of the questions result in\n");
	printf("\t the permanent loss of data from the disks on this machine.\n\n");

	/*
	 * initialize volume header
	 */
	if (initvh() == 0)
		exit(1);

	/*
	 * do formatting and bad block remapping
	 * (ctlrtype is set in initvh)
	 */
	if ( ctlrtype == SMD ){

		bb_read();
		format();
		bb_scan();
		bb_input();
		bb_map();

		printf("\ndump bad sector table (y if yes)? ");
		cp = gets(linebuf);
		if (*cp == 'y')
			bst_dump();
	} else if (ctlrtype & SCSI) {
		scsi_format();
		scsi_scan();
		scsibb_input();
		scsibb_map();
	} else {
		printf("Controller type not supported\n");
		exit(1);
	}
	/*
	 * write volume header and bad sector table (if not scsi) out to disk
	 */
	wrapup();
	exit(0);
}

/*
 * initial setup of the volume header information
 */
initvh()
{
	char buf[LINESIZE];
	char buf2[LINESIZE];
	struct device_parameters *dp = &Vh.vh_dp;
	int i,ntrys;
	register int *ip, csum;
	struct partition_table *ppt;
	char *cp;
	struct dev_info *dptr;

again:
#ifdef STANDALONE
	printf("\nname of device? ");
	gets(buf);
	if (*buf == 0)
		return (0);
	i = 0;
	while( fmtdevtype[i].devname != NULL ){
	    if( strcmp( fmtdevtype[i].devname, buf) == 0 ){
		ctlrtype = fmtdevtype[i].ctlrtype;
		break;
	    }
	    i++;
	}
	
	strcat(buf, "(");
	if (ctlrtype == (SCSI|ON_BOARD)) {
		printf("LUN number? ");
	} else
		printf("controller number? ");
	gets(buf2);
	strcat(buf, buf2);
	strcat(buf, ",");
	if (ctlrtype & SCSI) {
		printf("target id? ");
	} else
		printf("unit number? ");
	gets(buf2);
	strcat(buf, buf2);
	strcat(buf, ",");
	strcat(buf, btoa(PTNUM_VOLUME));
	strcat(buf, ")");
#else STANDALONE
	printf("\nfull path to /dev entry for entire volume? ");
	gets(buf);
	if (*buf == 0)
		return;
#endif STANDALONE

	if ((fd = open(buf, O_RDWR)) < 0) {
		printf("cannot open %s\n", buf);
		goto again;
	}

	/* note that the Jaguar driver doesn't yet support this ioctl */
	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.memaddr = (unsigned long)&geom;
	    io_arg.datasz = (unsigned long)sizeof(geom);
	    ntrys = 4;
	    while (ntrys--) { 
	        if (ioctl(fd,DIOCDISKGEOM, &io_arg) < 0) {
		    printf("Cannot get disk geometry info.  Retrying...\n");
		    continue;
		}
		/* sanity check of geometry obtained */
		if (!geom.geom_cyl || !geom.geom_head || !geom.geom_spt || 
		    !geom.geom_bps) {
		    printf("Invalid disk geometry info.  Retrying...\n");
		    continue;
		}	
		goto geom_ok;		
  	    }
	    return(0);
geom_ok:
	    io_arg.memaddr = (unsigned long)&Vh;
	    io_arg.datasz = (unsigned long)sizeof(Vh);
	    if (ioctl(fd,DIOCGETVH, &io_arg) < 0) {
		if (io_arg.retval == DIOC_OPERR)
		    printf("Cannot read volume header\n");
		if (io_arg.retval == DIOC_NOTVOLHDR)
		    printf("Invalid volume header\n");
		goto setit;
	    }
	} else {
#ifdef STANDALONE
	    if (ioctl(fd, DIOCGETVH, &Vh) < 0) {
		printf("Cannot get volume header\n");
		return (0);
	    }
#else STANDALONE
	    if (read(fd, &Vh, sizeof(Vh)) != sizeof(Vh)) {
		printf("Cannot read volume header\n");
		bzero(Vh, sizeof(Vh));
	    }
#endif STANDALONE
	}

	if (is_vh(&Vh)) {
		if ( ctlrtype == SMD ) {
#ifdef STANDALONE
		    /*
		     * find bad sector table and read it in
		     */
		    for (i=0, ppt = Vh.vh_pt; i < NPARTAB; i++, ppt++) {
			if (ppt->pt_type == PTYPE_VOLUME) {
				break;
			}
		    }
		    if (i == NPARTAB) {
			printf("no volume header partition found\n");
			return (0);
		    }
		    if (i != PTNUM_VOLUME) {
			strcpy(buf2, buf);
			cp = index(buf2, ')');
			do {
				cp--;
			} while ((*cp != ',') && (*cp != '('));
			*(cp+1) = 0;
			strcat(buf2, btoa(i));
			strcat(buf2, ")");
			close(fd);
			if ((fd = open(buf2, O_RDWR)) < 0) {
				printf("cannot open %s\n", buf2);
				return (0);
			}
		    }
#endif STANDALONE
		    bst_readin();
		}
		/*
		 * Set attribute array
		 */
		for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES;i++,dptr++) {
			if (dp->dp_flags & dptr->di_value)
				attr_on[i] = 1;
			else 
				attr_on[i] = 0;
		}
		if (ctlrtype == (SCSI|ON_BOARD)) {
		    dp->dp_flags |= DP_SCSI;
		    if (dp->dp_mspw == 0)
			dp->dp_mspw = 260;
		} else {
		    for (i=0; i < MAX_DEVICES; i++) {
			if (bcmp(&dp_defaults[i], dp, 
			    sizeof(struct device_parameters)) == 0) {
				device = i;
				break;
			}
			dp_defaults[i].dp_mspw = dev_mspw[i];
			if (bcmp(&dp_defaults[i], dp, 
			    sizeof(struct device_parameters)) == 0) {
				device = i;
				break;
			}
		    }
		    if (i == MAX_DEVICES) {
			device = DEV_OTHER;
			printf("\ndevice parameters from disk don't match table entries!");
			printf("\nchoose new device parameters (y if yes)? ");
			gets(buf2);
			if (*buf2 == 'y')
				goto setit;
		    } else {
			printf("\nchoose new drive parameters (y if yes)? ");
			gets(buf2);
			if (*buf2 == 'y')
				goto setit;
		    }
		    if (dp->dp_mspw == 0)
			dp->dp_mspw = dev_mspw[device];
		}
	} else {
setit:
		bzero(&Vh, sizeof(struct volume_header));

		/*
		 * initialize volume header with defaults
		 */
		dp_init();
		/*
		 * Set attribute array
		 */
		for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES;i++,dptr++) {
			if (dp->dp_flags & dptr->di_value)
				attr_on[i] = 1;
			else 
				attr_on[i] = 0;
		}
		vd_init();
		pt_init();
	}

	if (ctlrtype == SMD) {
	    if (dp->dp_secs > MAX_NSECS) {
		printf("\nFormat must be rebuilt with MAX_NSECS adjusted\n");
		printf("\tThis drive has %d sectors per track, and MAX_NSECS = %d\n", dp->dp_secs, MAX_NSECS);
		exit(1);
	    }

	    if (di_defaults[device].mdefects > MAX_BADBLOCKS) {
		printf("\nFormat must be rebuilt with MAX_BADBLOCKS adjusted\n");
		printf("\tThis drive has %d max num defects, and MAX_BADBLOCKS = %d\n", di_defaults[device].mdefects, MAX_BADBLOCKS);
		exit(1);
	    }
	}

	/*
	 * verify device parameters 
	 */
	while (1) {
		printf("\ndump device parameters (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y')
		    	dp_dump();
		if (ctlrtype == (SCSI|ON_BOARD)) {
		    printf("\nuse default device parameters (y if yes)? ");
		    gets(buf2);
		    if (*buf2 == 'y') {
			bzero(&Vh, sizeof(struct volume_header));
			/*
		 	* initialize volume header with defaults 
		 	*/
			dp_init();
			/*
		 	* Set attribute array
		 	*/
			for (i = 0, dptr = dev_attribs; 
					i < MAX_ATTRIBUTES;i++,dptr++) {
				if (dp->dp_flags & dptr->di_value)
					attr_on[i] = 1;
				else 
					attr_on[i] = 0;
			}
			vd_init();
			pt_init();

			break;
		    }
		}
		printf("modify device parameters (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y') {
			if ( ctlrtype == SMD ){
				dp_get();
				dp_definfo();
			} else
				scsidp_get();
		} else
			break;
	}

	/*
	 * disable any error recovery that could be performed
	 * by the controller
	 */
	dpreseek = dp->dp_flags & DP_RESEEK;
	dp->dp_flags &= ~DP_RESEEK;
	dpnretries = dp->dp_nretries;
	dp->dp_nretries = 0;

	/*
	 * verify volume directory information
	 */
#ifdef notdef
	while (1) {
		printf("\ndump volume directory (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y')
			vd_dump();
		printf("modify volume directory (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y')
			vd_modif();
		else
			break;
	}
#endif notdef
	
	/*
	 * verify partition table information
	 */
parttab:
	while (1) {
		printf("\ndump partition table (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y')
			pt_dump();
		printf("modify partition table (y if yes)? ");
		gets(buf2);
		if (*buf2 == 'y')
			pt_modif();
		else
			break;
	}

	if (ctlrtype == SMD) {
	    /*
	     * find sector/track replacement partition 
	     */
	    if (dp->dp_flags & DP_TRKFWD) {
		for (i=0, ppt = Vh.vh_pt; i < NPARTAB; i++, ppt++) {
			if (ppt->pt_type == PTYPE_TRKREPL) {
				rplpart = ppt;
				break;
			}
		}
		if (rplpart == NULL) {
			printf("no track replacement partition found, please specify one\n");
			goto parttab;
		}
	    } else if (dp->dp_flags) {	/* XXX */
		for (i=0, ppt = Vh.vh_pt; i < NPARTAB; i++, ppt++) {
			if (ppt->pt_type == PTYPE_SECREPL) {
				rplpart = ppt;
				break;
			}
		}
		if (rplpart == NULL) {
			printf("no sector replacement partition found, please specify one\n");
			goto parttab;
		}
	    }
	}

	/*
	 * check for entire volume partiton
	 */
	for (i=0, ppt = Vh.vh_pt; i < NPARTAB; i++, ppt++) {
		if (ppt->pt_type == PTYPE_VOLUME)
			break;
	}
	if (i >= NPARTAB) {
	    printf("no entire volume partition found, please specify one\n");
	    goto parttab;
	}

	volpart = i;
#ifdef STANDALONE
	/*
	 * make sure the entire volume partition is the one open'd
	 */
	if (i != PTNUM_VOLUME) {
		strcpy(buf2, buf);
		cp = index(buf2, ')');
		do {
			cp--;
		} while ((*cp != ',') && (*cp != '('));
		*(cp+1) = 0;
		strcat(buf2, btoa(i));
		strcat(buf2, ")");
		close(fd);
		if ((fd = open(buf2, O_RDWR)) < 0) {
			printf("cannot open %s\n", buf2);
			return (0);
		}
	}
#endif STANDALONE

	/* 
	 * set volume header 
	 */
	Vh.vh_magic = VHMAGIC;
	Vh.vh_csum = 0;
	for (csum = 0, ip = (int *)&Vh; ip < (int *)(&Vh+1); ip++)
	    csum += *ip;
	Vh.vh_csum = -csum;
	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.memaddr = (unsigned long)&Vh;
	    io_arg.datasz = (unsigned long)sizeof(Vh);
	    if (ioctl(fd,DIOCINITVH, &io_arg) < 0) {
		printf("Cannot initialize volume header\n");
		return(0);
	    }
	} else {
#ifdef STANDALONE
	    if (ioctl(fd, DIOCSETVH, &Vh) < 0) {
		    printf("cannot set volume header\n");
		    return (0);
	    }
#else
	    {
		caddr_t addr;

		addr = (caddr_t)&Vh;
		if (ioctl(fd, DIOCINITVH, &addr) < 0) {
			printf("ioctl to cause init of vh failed, %d\n", errno);
			return (0);
		}
	    }
#endif STANDALONE
	}

	return (1);
}

/*
 * write volume header and bad sector table
 */
wrapup()
{
	int i;
	struct device_parameters *dp = &Vh.vh_dp;
	int *ip;
	int csum;
	struct volume_directory *vd;
	int nbytes;
	char buf[LINESIZE];
	int failed;

	printf("\nwrite new volume header? (n if no)? ");
	gets(buf);
	if (*buf == 'n') {
		close(fd);
		return;
	}
	if ( ctlrtype & SCSI )
		goto wvolhdr;

	/* 
	 * write out bad sector table 
	 */
	if ((vd = bst_allocsp()) == NULL) {
		printf("\nvd allocation for bad sector table failed\n");
		goto wvolhdr;
	}
	if ((vd->vd_nbytes != 0) && (vd->vd_lbn != INVALID)) {
		nbytes = ((vd->vd_nbytes+DEV_BSIZE-1) / DEV_BSIZE) * 
			DEV_BSIZE;
rewrite:
		printf("\nwriting bad sector table at lbn %d... ", vd->vd_lbn);
#ifndef STANDALONE
		fflush(stdout);
#endif STANDALONE
		failed = 0;
		if (lseek(fd, vd->vd_lbn * DEV_BSIZE, 0) < 0) {
			printf("seek failed, ");
			failed = 1;
		} else if (write(fd, bsttab, nbytes) != nbytes) {
			printf("write failed, ");
			failed = 1;
		}
		if (failed) {
			printf("retry? ");
			gets(buf);
			if (*buf == 'y')
				goto rewrite;
		}
		printf("\n");
	}

wvolhdr:
	/*
	 * compute volume header checksum and set magic number
	 */
	if (dpreseek)
		Vh.vh_dp.dp_flags |= DP_RESEEK;
	Vh.vh_dp.dp_nretries = dpnretries;
	Vh.vh_magic = VHMAGIC;
	Vh.vh_csum = 0;
	for (csum = 0, ip = (int *)&Vh; ip < (int *)(&Vh+1); ip++) {
		csum += *ip;
	}
	Vh.vh_csum = -csum;

	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.memaddr = (unsigned long)&Vh;
	    io_arg.datasz = (unsigned long)sizeof(Vh);
	    if (ioctl(fd,DIOCRECONFIG, &io_arg) < 0) {
		printf("Cannot initialize and write volume header\n");
		return;
	    }
	} else {
	    /* 
	     * write volume header in the 1st sector of each track of cyl 0 
	     */
	    printf("\nwriting volume header... ");
	    for (i=0; i < dp->dp_trks0; i++) {
		if (lseek(fd, i * dp->dp_secs * DEV_BSIZE, 0) < 0) {
			printf("lseek for volhdr failed\n");
			continue;
		}
		if (write(fd, &Vh, DEV_BSIZE) != DEV_BSIZE) {
			printf("write of volhdr at trk %d failed\n", i);
		}
	    }
	    printf("\n");
#ifndef STANDALONE
	    {
		caddr_t addr;

		addr = (caddr_t)&Vh;
		if (ioctl(fd, DIOCINITVH, &addr) < 0) {
		    printf("ioctl to cause init of vh failed, %d\n", errno);
		    return;
		}
	    }
#endif !STANDALONE
	}
	close(fd);
}
/*
 * format all the tracks on a disk
 */
format()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct partition_table *pt;
	register int ccyl;
	register int chd;
	struct fmt_map_info fmi;
	char buf[LINESIZE];
	int nsecscyl;
	int i;

	printf("\nformatting destroys disk data, perform format (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;
	printf("format entire disk (y if yes)? ");
	gets(buf);
	if (*buf != 'y') {
getpart:
		printf("entry number of partition to format? ");

		/* 
		 * make sure that all characters are numbers.  We don't
		 * want people to type in garbage and have atob return
		 * a zero, and partition zero will be formatted
		 */
		gets(buf);
		for( i=0; *(buf+i) != '\0';i++){
		    if( !isdigit( *(buf+i))){
			printf("invalid partition entry %s\n", buf);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		    }
		}

		atob(buf, &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
		pt = &Vh.vh_pt[i];
		nsecscyl = dp->dp_secs * dp->dp_trks0;
		startcyl = pt->pt_firstlbn / nsecscyl;
		totalcyl = startcyl + (pt->pt_nblks / nsecscyl);
	} else {
	    startcyl = 0;
	    totalcyl = dp->dp_cyls;
	}

	printf("\nformatting ");
#ifndef STANDALONE
	fflush(stdout);
#endif STANDALONE
	fmi.fmi_action = FMI_FORMAT_TRACK;
	for (ccyl = startcyl; ccyl < totalcyl; ccyl++) {
		fmi.fmi_cyl = ccyl;
		printf(".");
#ifndef STANDALONE
		fflush(stdout);
#endif STANDALONE

		for (chd = 0; chd < dp->dp_trks0; chd++) {
			fmi.fmi_trk = chd;
retry:
			if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
				printf("format error(%d) on cyl %d head %d\n",
					errno, ccyl, chd);
				printf("retry format (y if yes)? ");
				gets(buf);
				if (buf[0] == 'y')
					goto retry;
			}
		}
	}
	printf("\n");
}
/*
 * format a SCSI disk
 */
scsi_format()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct partition_table *pt;
	register int ccyl;
	register int chd;
	struct fmt_map_info fmi;
	char buf[LINESIZE];
	int nsecscyl;
	int i;

	printf("\nformatting destroys ALL SCSI disk data, perform format (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;

	startcyl = 0;
	totalcyl = dp->dp_cyls; /* needed for scsi_scan() */
	printf("\nformatting ");
#ifndef STANDALONE
	fflush(stdout);
#endif STANDALONE
	fmi.fmi_action = FMI_FORMAT_TRACK;
	if (ctlrtype == (SCSI|ON_BOARD)) {
	    fmi.fmi_intrlv = dp->dp_interleave;
	    if (!dp->dp_tpz) dp->dp_tpz = 1;
	    fmi.fmi_tpz = dp->dp_tpz;
	    if (!dp->dp_aspz) dp->dp_aspz = 1;
	    fmi.fmi_aspz = dp->dp_aspz;
	    if (!dp->dp_atpv) dp->dp_atpv = dp->dp_trks0 * 2; 
	    fmi.fmi_atpv = dp->dp_atpv;
	    io_arg.memaddr = (unsigned long)&fmi;
	    io_arg.datasz = (unsigned long)sizeof(fmi);
	    if (ioctl(fd,DIOCFMTMAP, &io_arg) < 0) {
		printf("Cannot format\n");
	    }
	} else {
	    if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
		    printf("\nformat error");
	    }
	}
	printf("\n");
}

/*
 * initialize partition table with default partition settings
 */
pt_init()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct partition_table *pt;
	register int i;
	int cyls;
	int nsecscyl;
	int fstype, tmp, tmp1, tmpcyls;
	char buf[LINESIZE];
	int rootend, replend, lastlbn;

	/*
	 * clear out partition table
	 */
	for (i=0, pt = Vh.vh_pt; i < NPARTAB; i++, pt++) {
		pt->pt_nblks = 0;
	}
	nsecscyl = dp->dp_secs * dp->dp_trks0;

	/*
	 * initialize file system independent partition table entries
	 */
	pt = &Vh.vh_pt[PTNUM_VOLHDR];
	pt->pt_type = PTYPE_VOLHDR;
	pt->pt_firstlbn = 0;
	cyls = (((PTSIZE_VHDR + dp->dp_secbytes - 1) / dp->dp_secbytes)
		+ nsecscyl - 1) / nsecscyl;
	pt->pt_nblks = cyls * nsecscyl;

	pt = &Vh.vh_pt[PTNUM_VOLUME];
	pt->pt_type = PTYPE_VOLUME;
	pt->pt_firstlbn = 0;
	lastlbn = pt->pt_nblks = nsecscyl * dp->dp_cyls;

	pt = &Vh.vh_pt[PTNUM_REPL];
	if (IS_COMMON_SCSI) { /* ok for genesis */
	    pt->pt_type = PTYPE_SECREPL;
	    cyls = 0;
	} else {
	    if (dp->dp_flags & DP_TRKFWD) {
		pt->pt_type = PTYPE_TRKREPL;
		cyls = (di_defaults[device].mdeftrks + dp->dp_trks0 - 1) / 
			dp->dp_trks0;
	    } else {
		pt->pt_type = PTYPE_SECREPL;
		cyls = (di_defaults[device].mdefects + nsecscyl - 1) / nsecscyl;
	    }
	}
	pt->pt_nblks = cyls * nsecscyl;
	pt->pt_firstlbn = Vh.vh_pt[PTNUM_VOLHDR].pt_nblks;

	replend = pt->pt_nblks + pt->pt_firstlbn;

	/*
	 * initialize Unix file system partitions
	 */
	printf("The Unix file system partitions may be either ffs (BSD) or System V\n");
	printf("do you desire fast file system (BSD) partitions (n if no)? ");
	gets(buf);
	if (*buf == 'n')
		fstype = PTYPE_SYSV;
	else
		fstype = PTYPE_BSD;

	/*
	 * initialize partition table entries
	 */
	strncpy(Vh.vh_bootfile, "/unix", BFNAMESIZE);
	i = PTNUM_FSFIRST;
	tmpcyls = (((MIN_SIZE + dp->dp_secbytes - 1) / 
			dp->dp_secbytes) + nsecscyl - 1) / nsecscyl;
	for (pt = &Vh.vh_pt[i]; i <= PTNUM_FSLAST; i++, pt++) {
		if ((i == PTNUM_VOLHDR) || (i == PTNUM_REPL) ||
			(i == PTNUM_VOLUME) || (i == PTNUM_FSUSRg) ||
					       (i == PTNUM_FSUSRh)) {
			continue;
		}
		pt->pt_type = fstype;
		switch (i) {
		case PTNUM_FSROOT:
			pt->pt_firstlbn = replend;
			cyls = (((PTSIZE_ROOT + dp->dp_secbytes - 1) / 
				dp->dp_secbytes) + nsecscyl - 1) / nsecscyl;
			tmp = cyls % CYLS_PER_GROUP;
			tmp *= nsecscyl;
			tmp *= dp->dp_secbytes;
			if (tmp < MIN_SIZE) /* last cyl group under 3 Meg? */
				cyls += tmpcyls; /* beef up the last cyl group*/
			tmp = cyls * nsecscyl;
			if ((tmp + pt->pt_firstlbn) > lastlbn)
				tmp = lastlbn - pt->pt_firstlbn;
			pt->pt_nblks = tmp;
			rootend = pt->pt_firstlbn + pt->pt_nblks;
			Vh.vh_rootpt = PTNUM_FSROOT;
			break;
		case PTNUM_FSSWAP:
			cyls = (((PTSIZE_SWAP + dp->dp_secbytes - 1) / 
				dp->dp_secbytes) + nsecscyl - 1) / nsecscyl;
			tmp = cyls * nsecscyl;
			if ((tmp + rootend) > lastlbn)
				tmp = lastlbn - rootend;
			pt->pt_nblks = tmp;
			pt->pt_firstlbn = lastlbn - pt->pt_nblks;
			Vh.vh_swappt = PTNUM_FSSWAP;
			break;
		case PTNUM_FSALL:
			pt->pt_firstlbn = replend;
			pt->pt_nblks = (dp->dp_cyls * nsecscyl) - 
					pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRd:
			pt->pt_firstlbn = rootend;
			tmp = lastlbn - Vh.vh_pt[PTNUM_FSSWAP].pt_nblks;
			pt->pt_nblks = tmp - pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRe:
			pt->pt_firstlbn = rootend;
			/*
			 * need to calculate 'SUSRh' now
			 */
			cyls = (((PTSIZE_USRh + dp->dp_secbytes - 1) / 
				dp->dp_secbytes) + nsecscyl - 1) / nsecscyl;
			tmp = (cyls * nsecscyl);
			tmp1 = Vh.vh_pt[PTNUM_FSSWAP].pt_firstlbn;
			if ((tmp + rootend) > tmp1)
				tmp = tmp1 - rootend;
			Vh.vh_pt[PTNUM_FSUSRh].pt_nblks = tmp;
			Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn =
				Vh.vh_pt[PTNUM_FSSWAP].pt_firstlbn -
				Vh.vh_pt[PTNUM_FSUSRh].pt_nblks;
			Vh.vh_pt[PTNUM_FSUSRh].pt_type = fstype;
			/*
			 * need to calculate 'SUSRg' now
			 */
			Vh.vh_pt[PTNUM_FSUSRg].pt_firstlbn = rootend;
			Vh.vh_pt[PTNUM_FSUSRg].pt_nblks =
			   Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn - pt->pt_firstlbn;
			Vh.vh_pt[PTNUM_FSUSRg].pt_type = fstype;
			/*
			 * now adjust sizes
			 */
			cyls = Vh.vh_pt[PTNUM_FSUSRg].pt_nblks / nsecscyl;
			if (cyls) {
			    tmp = cyls % CYLS_PER_GROUP;
			    tmp *= nsecscyl;
			    tmp *= dp->dp_secbytes;
			    if (tmp < MIN_SIZE) {  /* last group under 3 Meg? */
				cyls += tmpcyls; /* beef up the last cyl group*/
				Vh.vh_pt[PTNUM_FSUSRg].pt_nblks =
							(cyls * nsecscyl);
				Vh.vh_pt[PTNUM_FSUSRh].pt_nblks -=
							(tmpcyls * nsecscyl);
				Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn +=
							(tmpcyls * nsecscyl);
			
			    }
			}
			pt->pt_nblks = (Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn -
					pt->pt_firstlbn) / 3;
			pt->pt_nblks *= 2;	/* e = 2/3 of g */
			pt->pt_nblks = pt->pt_nblks - (pt->pt_nblks % nsecscyl);
			break;
		case PTNUM_FSUSRf:
			pt->pt_firstlbn = Vh.vh_pt[PTNUM_FSUSRe].pt_firstlbn +
					Vh.vh_pt[PTNUM_FSUSRe].pt_nblks;
			pt->pt_nblks = Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn -
					pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRg:
			pt->pt_firstlbn = rootend;
			pt->pt_nblks = Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn -
					pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRi:
			pt->pt_firstlbn = replend;
			pt->pt_nblks = Vh.vh_pt[PTNUM_FSUSRf].pt_firstlbn -
					pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRj:
			pt->pt_firstlbn = Vh.vh_pt[PTNUM_FSUSRf].pt_firstlbn;
			pt->pt_nblks = lastlbn - pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRk:
			pt->pt_firstlbn = Vh.vh_pt[PTNUM_FSUSRh].pt_firstlbn;
			pt->pt_nblks = lastlbn - pt->pt_firstlbn;
			break;
		case PTNUM_FSUSRl:
			pt->pt_firstlbn = replend;
			pt->pt_nblks = (Vh.vh_pt[PTNUM_FSUSRf].pt_firstlbn -
					pt->pt_firstlbn) / 2;
			pt->pt_nblks -= (pt->pt_nblks % nsecscyl);
			break;
		case PTNUM_FSUSRm:
			pt->pt_firstlbn = Vh.vh_pt[PTNUM_FSUSRl].pt_firstlbn +
					Vh.vh_pt[PTNUM_FSUSRl].pt_nblks;
			pt->pt_nblks = Vh.vh_pt[PTNUM_FSUSRf].pt_firstlbn -
					pt->pt_firstlbn;
			break;
		default:
			pt->pt_nblks = INVALID;
			pt->pt_firstlbn = INVALID;
			break;
		}
	}
}

/*
 * add/delete partition table entries
 */
pt_modif()
{
	struct device_parameters *dp = &Vh.vh_dp;
	struct partition_table *pt;
	struct partition_table *tpt;
	int i;
	register int j;
	char buf[LINESIZE];

	printf("\npartition table manipulation\n");
	printf("choose one of (list, add, delete, quit, init, modify, replace)\n");
	while (1) {
		printf("command? ");
		gets(buf);
		switch (*buf) {

		case 'i':
			pt_init();
			break;

		case 'l':
			pt_dump();
			break;

		case 'r':
			printf("\ttable entry number? ");
			atob(gets(buf), &i);
			pt = &Vh.vh_pt[i];
get_pttype0:
			printf("\tvalid partition types are:\n");
			for (j=0; j < NUM_PTYPES; j++) {
				printf("\t\t(%d) %s\n", j, part_types[j]);
			}
			printf("\tenter number for type? ");
			atob(gets(buf), &i);
			if (i >= NUM_PTYPES) {
				printf("invalid partition type %d\n", i);
				goto get_pttype0;
			}
			pt->pt_type = i;
get_stcyl0:
			printf("\tstarting cylinder? ");
			atob(gets(buf), &i);
			if (i >= dp->dp_cyls) {
				printf("\tbad cylinder number (0 thru %d)\n",
					dp->dp_cyls-1);
				goto get_stcyl0;
			}
			pt->pt_firstlbn = i * dp->dp_secs * dp->dp_trks0;

			printf("\tnumber of cylinders? ");
			atob(gets(buf), &i);
			pt->pt_nblks = i * dp->dp_secs * dp->dp_trks0;
			break;

		case 'a':
			/*
			 * find unused partition table entry
			 */
			for (j=0, pt = Vh.vh_pt; j < NPARTAB; j++, pt++) {
				if (pt->pt_nblks == 0)
					break;
			}
			if (j == NPARTAB) {
				printf("partition table full\n");
				break;
			}

get_pttype:
			printf("\tvalid partition types are:\n");
			for (j=0; j < NUM_PTYPES; j++) {
				printf("\t\t(%d) %s\n", j, part_types[j]);
			}
			printf("\tenter number for type? ");
			atob(gets(buf), &i);
			if (i >= NUM_PTYPES) {
				printf("invalid partition type %d\n", i);
				goto get_pttype;
			}
			pt->pt_type = i;

get_stcyl:
			printf("\tstarting cylinder? ");
			atob(gets(buf), &i);
			if (i >= dp->dp_cyls) {
				printf("\tbad cylinder number (0 thru %d)\n",
					dp->dp_cyls-1);
				goto get_stcyl;
			}
			pt->pt_firstlbn = i * dp->dp_secs * dp->dp_trks0;

			printf("\tnumber of cylinders? ");
			atob(gets(buf), &i);
			pt->pt_nblks = i * dp->dp_secs * dp->dp_trks0;
			break;

		case 'd':
			printf("\tentry number? ");
			atob(gets(buf), &i);
			if (i >= NPARTAB)
				printf("invalid partition entry %d\n", i);
			else
				Vh.vh_pt[i].pt_nblks = 0;
			break;

		case 'm':
			printf("default bootfile name? ");
			strncpy(Vh.vh_bootfile, gets(buf), BFNAMESIZE);
			printf("entry number of root (currently %d)? ", 
				Vh.vh_rootpt);
			atob(gets(buf), &i);
			if (i >= NPARTAB)
				printf("invalid partition entry %d\n", i);
			else
				Vh.vh_rootpt = i;
			printf("entry number of swap (currently %d)? ",
				Vh.vh_swappt);
			atob(gets(buf), &i);
			if (i >= NPARTAB)
				printf("invalid partition entry %d\n", i);
			else
				Vh.vh_swappt = i;
			break;

		case 'q':
			return;

		default:
			printf("choose one of (list, add, delete, quit, init, modify, replace)\n");
		}
	}
}

/*
 * dump partition table
 */
pt_dump()
{
	int i;
	struct partition_table *pt;
	struct device_parameters *dp;
	int nsecscyl;
	int cyls;

	dp = &Vh.vh_dp;
	nsecscyl = dp->dp_secs * dp->dp_trks0;
	printf("\tRoot partition is entry #%d\n", Vh.vh_rootpt);
	printf("\tSwap partition is entry #%d\n", Vh.vh_swappt);
	printf("\tDefault boot file is %s\n", Vh.vh_bootfile);
	printf("\nentry\ttype\t\t#blks\t#cyls\tcg(mod)\t1st_lbn\t1st_cyl\tnum_bytes\n");
	for (i=0, pt = Vh.vh_pt; i < NPARTAB; i++, pt++) {
		if (pt->pt_nblks == 0) {
			printf("partition %d size == 0\n",i);
			continue;
		}
		cyls = pt->pt_nblks / nsecscyl;  
		if (pt->pt_nblks == INVALID) {
			printf("%d-%c\t%s\t%d\t%d\t%d(%d)\t%d\t%d\t\t%d\n", i, 
			    'a'+i,part_types[pt->pt_type], 0, 0, 0, 0,
						 INVALID, INVALID, 0);
		} else {
			if ((i == PTNUM_VOLHDR) || (i == PTNUM_REPL) ||
						   (i == PTNUM_VOLUME))
			  printf("%d\t%s\t%d\t%d\t%d(%d)\t%d\t%d\t%d\n", i, 
			    part_types[pt->pt_type], pt->pt_nblks, cyls,
			    cyls / CYLS_PER_GROUP, cyls % CYLS_PER_GROUP,
			    pt->pt_firstlbn, pt->pt_firstlbn / nsecscyl,
			    pt->pt_nblks * dp->dp_secbytes);
			else if (i > PTNUM_VOLUME)
			  printf("%d-%c\t%s\t%d\t%d\t%d(%d)\t%d\t%d\t%d\n", i, 
			    'a'+i-3,part_types[pt->pt_type], pt->pt_nblks, cyls,
			    cyls / CYLS_PER_GROUP, cyls % CYLS_PER_GROUP,
			    pt->pt_firstlbn, pt->pt_firstlbn / nsecscyl,
			    pt->pt_nblks * dp->dp_secbytes);
			else
			  printf("%d-%c\t%s\t%d\t%d\t%d(%d)\t%d\t%d\t%d\n", i, 
			    'a'+i,part_types[pt->pt_type], pt->pt_nblks, cyls,
			    cyls / CYLS_PER_GROUP, cyls % CYLS_PER_GROUP,
			    pt->pt_firstlbn, pt->pt_firstlbn / nsecscyl,
			    pt->pt_nblks * dp->dp_secbytes);
		}
	}
	printf("\n");
}

/*
 * set necessary directory entries
 */
vd_init()
{
	struct volume_directory *vd;
	int i;

	for (i=0, vd = Vh.vh_vd; i < NVDIR; i++, vd++) {
		vd->vd_nbytes = 0;
	}
	vd = Vh.vh_vd;
	strncpy(vd->vd_name, BSTTAB_NAME, VDNAMESIZE);
}

#ifdef notdef
/*
 * add/delete entries in the volume directory
 */
vd_modif()
{
	struct volume_directory *vd;
	int i;
	char buf[LINESIZE];

	printf("\nvolume directory manipulation\n");
	printf("choose one of (list, add, delete, quit, init)\n");
	while (1) {
		printf("command? ");
		gets(buf);
		switch (*buf) {

		case 'i':
			vd_init();
			break;

		case 'l':
			vd_dump();
			break;

		case 'a':
			/*
			 * find unused volume directory entry
			 */
			for (i=0, vd = Vh.vh_vd; i < NVDIR; i++, vd++) {
				if (vd->vd_nbytes == 0)
					break;
			}
			if (i == NVDIR) {
				printf("volume directory full\n");
				break;
			}

			printf("\tname? ");
			gets(buf);
			if ((i=strlen(buf)) >= VDNAMESIZE) {
				printf("name must be less than %d chars\n",
					VDNAMESIZE);
				break;
			}
			strncpy(vd->vd_name, buf, i);

			printf("\tstarting logical block number? ");
			atob(gets(buf), &vd->vd_lbn);

			printf("\tlength in bytes? ");
			atob(gets(buf), &vd->vd_nbytes);
			break;

		case 'd':
			printf("\tentry number? ");
			atob(gets(buf), &i);
			Vh.vh_vd[i].vd_nbytes = 0;
			break;

		case 'q':
			return;

		default:
			printf("valid commands are: add, delete, list, quit\n");
		}
	}
}

/*
 * dump volume directory
 */
vd_dump()
{
	int i;
	struct volume_directory *vd;

	printf("\tentry\tname\t\tlbn\t#bytes\n");
	for (i=0, vd = Vh.vh_vd; i < NVDIR; i++, vd++) {
		if (vd->vd_nbytes == 0)
			continue;
		printf("\t%d\t%s\t\t%d\t%d\n", i, vd->vd_name, vd->vd_lbn, 
		    vd->vd_nbytes);
	}
}
#endif notdef

/*
 * set device parameters
 */
dp_init()
{
	int readcap[2], i, j = 0;
	char linebuf[LINESIZE];
	register struct device_parameters *dp = &Vh.vh_dp;

	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.memaddr = (unsigned long)readcap;
	    io_arg.datasz = sizeof(readcap);
	    if (ioctl(fd,DIOCRDCAP,&io_arg) < 0) {
		printf("Unable to read drive capacity\n");
		goto getname;
	    }
	    dp->dp_trks0 = geom.geom_head;
	    /* nearest mult of 16 for number of sectors */
	    dp->dp_secs = (geom.geom_spt + 15) & 0xfffffff0;
	    dp->dp_cyls = (readcap[0] + 1)/(dp->dp_trks0 * dp->dp_secs);
	    dp->dp_secbytes = geom.geom_bps;
	    dp->dp_interleave = geom.geom_ilv;
	    dp->dp_flags = DP_SCSI;
	    dp->dp_datarate = 0;
	    dp->dp_nretries = 27;
	    dp->dp_tpz = geom.geom_tpz;		/* tracks per zone */
	    dp->dp_aspz = geom.geom_aspz;	/* alt sectors per zone */
	    dp->dp_atpv = geom.geom_atpv;	/* alt tracks per volume */
	    if (Vh.vh_dp.dp_mspw == 0)
		Vh.vh_dp.dp_mspw = 260;
	} else {
getname:
	    printf("device parameters are known for: \n");
	    for (i=0; i < MAX_DEVICES; i++) {
#ifdef STANDALONE
		if( dp_defaults[i].dp_type != ALL ){
		    if ( ctlrtype & SCSI ){
			if (dp_defaults[i].dp_type != SCSI)
				continue;
		    }else if( ctlrtype == SMD ){
			if (dp_defaults[i].dp_type != SMD)
				continue;
		    }
		}
#endif STANDALONE
		printf("\t(%d) %s\n", i, dev_names[i]);
	    }
	    printf("enter number for one of the above? ");
	    atob(gets(linebuf), &i);
	    if (i >= MAX_DEVICES) {
		printf("invalid choice\n");
		goto getname; 
	    }
	    device = i;
	    if (i == DEV_OTHER) {
		dp_get();
		dp_definfo();
	    } else {
		Vh.vh_dp = dp_defaults[i];
		if (Vh.vh_dp.dp_mspw == 0)
			Vh.vh_dp.dp_mspw = dev_mspw[i];
	    }
	}
}

/*
 * get device specific parameters
 */
dp_get()
{
	struct device_parameters *dp = &Vh.vh_dp;
	char linebuf[LINESIZE];
	int i;
	char *cp;
	int tmp;
	struct dev_info *dptr;

	/* 
	 * get device parameters 
	 */
	printf("enter device information: \n");

	printf("\tspiral skew (currently %d): ", dp->dp_skew);
	atob(gets(linebuf), &tmp);
	if( *linebuf != '\0')
	    dp->dp_skew = (u_char)tmp;
	printf("\tnumber words in gap1 (currently %d): ", dp->dp_gap1);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_gap1 = (u_char)tmp;
	printf("\tnumber words in gap2 (currently %d): ", dp->dp_gap2);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_gap2 = (u_char)tmp;
	printf("\tnumber of cylinders (currently %d): ", dp->dp_cyls);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_cyls = (u_short)tmp;
	printf("\tvol 0 starting head (currently %d): ", dp->dp_shd0);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_shd0 = (u_short)tmp;
	printf("\tnumber heads in vol 0 (currently %d): ", dp->dp_trks0);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_trks0 = (u_short)tmp;
	printf("\tvol 1 starting head (currently %d): ", dp->dp_shd1);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_shd1 = (u_short)tmp;
	printf("\tnumber heads in vol 1 (currently %d): ", dp->dp_trks1);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_trks1 = (u_short)tmp;
	printf("\tnumber sectors per track (currently %d): ", dp->dp_secs);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_secs = (u_short)tmp;
	printf("\tnumber bytes per sector (currently %d): ", dp->dp_secbytes);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_secbytes = (u_short)tmp;
	printf("\tsector interleave (currently %d): ", dp->dp_interleave);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_interleave = (u_short)tmp;
	printf("\tnumber retries on error (currently %d): ", dp->dp_nretries);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_nretries = (u_short)tmp;
	printf("\tmilliseconds per word (currently %d): ", dp->dp_mspw);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_mspw = (u_int)tmp;

	/* 
	 * get device attributes 
	 */
	printf("enter device attributes:\n");
	for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES; i++, dptr++) {
		printf("\tenable %s (currently", dptr->di_name);
		printf(" %s): ", attr_on[i] ? "on":"off");
		cp = gets(linebuf);
		while (isspace(*cp))
			cp++;
		if( *cp != 0 ){
		    if (*(cp+1) == 'n'){ /* on */
			dp->dp_flags |= dptr->di_value;
			attr_on[i] = 1;
		    }else{		/* *(cp+1) == 'f' */
			dp->dp_flags &= ~dptr->di_value;
			attr_on[i] = 0;
		    }
		}
	}
}
/*
 * get SCSI device specific parameters
 */
scsidp_get()
{
	struct device_parameters *dp = &Vh.vh_dp;
	char linebuf[LINESIZE];
	int i;
	char *cp;
	int tmp;
	struct dev_info *dptr;

	/* 
	 * get device parameters 
	 */
	printf("enter device information: \n");

	while (1) {
	    printf("\tnumber of cylinders (currently %d): ", dp->dp_cyls);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf == 0) break;
	    if (tmp) {
		dp->dp_cyls = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber heads (currently %d): ", dp->dp_trks0);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf == 0) break;
	    if (tmp) {
		dp->dp_trks0 = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber sectors per track (currently %d): ", dp->dp_secs);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf == 0) break;
	    if (tmp) {
		dp->dp_secs = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber bytes per sector (currently %d): ", dp->dp_secbytes);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf == 0) break;
	    if (tmp) {
		dp->dp_secbytes = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	printf("\tsector interleave (currently %d): ", dp->dp_interleave);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_interleave = (u_short)tmp;

	printf("\tmilliseconds per word (currently %d): ", dp->dp_mspw);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		dp->dp_mspw = (u_int)tmp;

	if (ctlrtype == (SCSI|ON_BOARD)) {
	    printf("\tdata rate (currently %d): ", dp->dp_datarate);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf != 0)
		    dp->dp_datarate = (u_int)tmp;

	    printf("\tnumber of retries (currently %d): ", dp->dp_nretries);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf != 0)
		    dp->dp_nretries = (u_int)tmp;

	    while (1) {
		printf("\ttracks per zone (currently %d): ", dp->dp_tpz);
	        atob(gets(linebuf), &tmp);
	        if (*linebuf == 0) break;
		if (tmp) {
		    dp->dp_tpz = (u_int)tmp;
		    break;
		}
		else printf("invalid value.\n");
	    }

	    while (1) {
	        printf("\talternate sectors per zone (currently %d): ",
			dp->dp_aspz);
	        atob(gets(linebuf), &tmp);
	        if (*linebuf == 0) break;
		if (tmp) {
		    dp->dp_aspz = (u_int)tmp;
		    break;
		}
		else printf("invalid value.\n");
	    }

	    while (1) {
	        printf("\talternate tracks per volume (currently %d): ",
			dp->dp_atpv);
	        atob(gets(linebuf), &tmp);
	        if (*linebuf == 0) break;
		if (tmp >= (dp->dp_trks0 * 2)) {
		    dp->dp_atpv = (u_int)tmp;
		    break;
		}
		else printf("invalid value.\n");
	    }
	}
}


/*
 * get device defect information
 */
dp_definfo()
{
	char linebuf[LINESIZE];
	struct device_info *di;
	int tmp;

	printf("enter device defect information:\n");
	di = &di_defaults[device];
	printf("\ttotal bytes per track (currently %d): ", di->trkbytes);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		di->trkbytes = tmp;
	printf("\tmaximum defect length in bits (currently %d): ", di->length);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		di->length = tmp;
	while(1){
	    printf("\tmaximum num defective tracks (currently %d): ", di->mdeftrks);
	    atob(gets(linebuf), &tmp);
	    if (*linebuf == 0)
		break;
	    if( tmp >= MAX_MTRKS )
		printf("invalid value. Must be less than %d.\n", MAX_MTRKS);
	    else{
		di->mdeftrks = tmp;
		break;
	    }
	}
	printf("\tmaximum num defects (currently %d): ", di->mdefects);
	atob(gets(linebuf), &tmp);
	if (*linebuf != 0)
		di->mdefects = tmp;
	if (di->realbps) {
		printf("\ttotal bytes per sector (currently %d): ", di->realbps);
		atob(gets(linebuf), &tmp);
		if (*linebuf != 0)
			di->realbps = tmp;
	}
}

/*
 * dump device parameters 
 */
dp_dump()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i;
	struct dev_info *dptr;
	struct device_info *iptr;

	if ( ctlrtype == SMD ){
		printf("\tspiral skew = %d\n", dp->dp_skew);
		printf("\tnumber words in gap1 = %d\n", dp->dp_gap1);
		printf("\tnumber words in gap2 = %d\n", dp->dp_gap2);
		printf("\tnumber cylinders = %d\n", dp->dp_cyls);
		printf("\tvol 0 starting head = %d\n", dp->dp_shd0);
		printf("\tnumber heads in vol 0 = %d\n", dp->dp_trks0);
		printf("\tvol 1 starting head = %d\n", dp->dp_shd1);
		printf("\tnumber heads in vol 1 = %d\n", dp->dp_trks1);
		printf("\tnumber sectors per track = %d\n", dp->dp_secs);
		printf("\tnumber bytes per sector = %d\n", dp->dp_secbytes);
		printf("\tsector interleave = %d\n", dp->dp_interleave);
		printf("\tnumber retries on error = %d\n", dp->dp_nretries);
		printf("\tmilliseconds per word = %d\n", dp->dp_mspw);

		printf("\tenabled attributes = \n");
		for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES;i++,dptr++) {
			if (dp->dp_flags & dptr->di_value)
				printf("\t\t%s\n", dptr->di_name);
		}

		iptr = &di_defaults[device];
		printf("\n\ttotal bytes per track = %d\n", iptr->trkbytes);
		printf("\tmaximum defect length in bits = %d\n", iptr->length);
		printf("\tmaximum num defective tracks = %d\n", iptr->mdeftrks);
		printf("\tmaximum num defects = %d\n", iptr->mdefects);
		if (iptr->realbps)
			printf("\ttotal bytes per sector = %d\n",iptr->realbps);
	} else {
	    printf("\tnumber cylinders = %d\n", dp->dp_cyls);
	    printf("\tnumber heads = %d\n", dp->dp_trks0);
	    printf("\tnumber sectors per track = %d\n", dp->dp_secs);
	    printf("\tnumber bytes per sector = %d\n", dp->dp_secbytes);
	    printf("\tsector interleave = %d\n", dp->dp_interleave);
	    if (ctlrtype == (SCSI|ON_BOARD)) {
		printf("\tdata rate = %d\n", dp->dp_datarate);
		printf("\tnumber of retries = %d\n", dp->dp_nretries);
		printf("\ttracks per zone = %d\n", dp->dp_tpz);
		printf("\talternate sectors per zone = %d\n", dp->dp_aspz);
		printf("\talternate tracks per volume = %d\n", dp->dp_atpv);
	    }
	}
}

/*
 * find bad sector table and read it in
 */
bst_readin()
{
	struct device_parameters *dp = &Vh.vh_dp;
	register struct volume_directory *vd;
	struct bst_table *bstp;
	register int i;
	int nbytes;
	int nsecscyl;
	int numentries;

	for (i=0, vd = Vh.vh_vd; i < NVDIR; i++, vd++) {
		if ((strncmp(vd->vd_name, BSTTAB_NAME, VDNAMESIZE) == 0) 
		    && (vd->vd_lbn > 0) && (vd->vd_nbytes > 0)) {
			break;
		}
	}
	if (i < NVDIR) {
		if (lseek(fd, vd->vd_lbn * DEV_BSIZE, 0) < 0) {
			printf("bst_readin: seek error\n");
			return;
		}
		nbytes = ((vd->vd_nbytes+DEV_BSIZE-1) / DEV_BSIZE) * DEV_BSIZE;
		if (read(fd, bsttab, nbytes) != nbytes) {
			printf("can't read bad sector table\n");
			return;
		}

		/*
		 * initialize bad block table from bad sector table
		 * on disk
		 */
		nsecscyl = dp->dp_secs * dp->dp_trks0;
		numentries = vd->vd_nbytes / sizeof(struct bst_table);
		for (i=0, bstp = bsttab; i < numentries; i++, bstp++) {
			if ((bstp->bt_rpltype <= BSTTYPE_EMPTY) ||
			    (bstp->bt_rpltype >= BSTTYPE_NOTREPL) ||
			    (bstp->bt_rpltype >= BSTTYPE_RUNTBAD))
				continue;
			++nbad;
			bb[nbad].cyl = bstp->bt_badlbn / nsecscyl;
			bb[nbad].trk = (bstp->bt_badlbn % nsecscyl)/dp->dp_secs;
			if (bstp->bt_rpltype == BSTTYPE_SLIPBAD)
				bb[nbad].sec =(bstp->bt_badlbn % dp->dp_secs)+1;
			else
				bb[nbad].sec = bstp->bt_badlbn % dp->dp_secs;
			bb[nbad].rpltype = bstp->bt_rpltype;
			bb[nbad].rpllbn = bstp->bt_rpllbn;
		}
		printf("read in %d defects from 'on disk' bad sector table\n"
						,nbad+1);
	}
}

/*
 * find a location in the volume header partition for the
 * bad sector table
 */
struct volume_directory *
bst_allocsp()
{
	struct device_parameters *dp = &Vh.vh_dp;
	struct partition_table *pt;
	struct volume_directory *vd;
	struct volume_directory *tvd;
	register int i;
	int empty;
	int nblks;
	int start;
	int oldnblks;

	/*
	 * find volume directory entry for bsttab
	 */
	start = dp->dp_secs * dp->dp_trks0;
	empty = INVALID;
	vd = NULL;
	for (i=0, tvd = Vh.vh_vd; i < NVDIR; i++, tvd++) {
		if ((vd == NULL) && 
		    (strncmp(tvd->vd_name, BSTTAB_NAME, VDNAMESIZE) == 0)) {
			vd = tvd;
		}
		if ((empty == INVALID) && (tvd->vd_nbytes == 0))
			empty = i;
		if ((tvd->vd_nbytes > 0) && (tvd->vd_lbn > 0) && 
		    (start < (tvd->vd_lbn+tvd->vd_nbytes))) {
			start = tvd->vd_lbn + ((tvd->vd_nbytes + 
				dp->dp_secbytes - 1) / dp->dp_secbytes);
		}
	}
	if (vd == NULL) {
		if (empty == INVALID) {
			return ((struct volume_directory *)0);
		} else {
			vd = &Vh.vh_vd[empty];
			strcpy(vd->vd_name, BSTTAB_NAME);
		}
	}

	oldnblks = (vd->vd_nbytes + dp->dp_secbytes - 1) / dp->dp_secbytes;
	vd->vd_nbytes = (nbad+1) * sizeof(struct bst_table);
	nblks = (vd->vd_nbytes + dp->dp_secbytes - 1) / dp->dp_secbytes;
	if ((nblks <= oldnblks) && (vd->vd_lbn > 0))
		return (vd);

	vd->vd_lbn = INVALID;
	for (i=0, pt = Vh.vh_pt; i < NPARTAB; i++, pt++) {
		if (pt->pt_type == PTYPE_VOLHDR) {
			if ((nblks+start) < (pt->pt_firstlbn + pt->pt_nblks)) {
				vd->vd_lbn = start;
				return (vd);
			} else {
				goto bad;
			}
		}
	}
bad:
	vd->vd_nbytes = 0;
	return ((struct volume_directory *)0);
}

/*
 * print out bad sector table
 */
bst_dump()
{
	int i;
	struct bst_table *bst;

	printf("\nbad sector table:\n");
#ifndef STANDALONE
	fflush(stdout);
#endif STANDALONE
	for (i=0, bst = bsttab; i < MAX_BADBLOCKS; i++, bst++) {
#ifndef STANDALONE
		fflush(stdout);
#endif STANDALONE
		switch (bst->bt_rpltype) {
		case BSTTYPE_SLIPSEC:
			printf("\tbad block %d slipped\n", bst->bt_badlbn);
			break;

		case BSTTYPE_SECFWD:
			printf("\tbad block %d forwarded to %d\n",
				bst->bt_badlbn, bst->bt_rpllbn);
			break;

		case BSTTYPE_TRKFWD:
			printf("\tbad track containing %d forwarded to %d\n",
				bst->bt_badlbn, bst->bt_rpllbn);
			break;

		case BSTTYPE_SLIPBAD:
			printf("\tbad block %d is the slip sector\n",
				bst->bt_badlbn);
			break;

		case BSTTYPE_RUNTBAD:
			printf("\tbad block %d is the runt sector\n",
				bst->bt_badlbn);
			break;

		case BSTTYPE_NOTREPL:
			printf("\tbad block %d not handled\n", 
				bst->bt_badlbn);
			break;

		case BSTTYPE_EMPTY:
			return;

		default:
			printf("\tinvalid rpl type %d\n", bst->bt_rpltype);
			break;
		}
	}
}

/*
 * read defect information off the drive
 */
bb_read()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct bb_entry *bbp;
	char buf[LINESIZE];
	struct media_defect md;
	register struct defects *d;
	register int i;
	register int cyl;
	register int trk;
	int secbytes;
	int sec;
	int len;

	if (!di_defaults[device].realbps) { /* don't know the real bytes/s */
	    if (dp->dp_flags & DP_SECTSLIP)
		if (dp->dp_runt)	/* we've got a runt now */
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 2);
		else
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 1);
	    else
		secbytes = di_defaults[device].trkbytes / dp->dp_secs;
	} else {
		secbytes = di_defaults[device].realbps;
	}

#ifndef STANDALONE
	printf("\nread defects from file (y if yes)? " );
	gets(buf);
	if (*buf == 'y') {
		if (nbad >= 0) { /* we must have read defects from disk */
			printf("\npurge defects read from disk (y if yes)? " );
			gets(buf);
			if (*buf == 'y') {
				for (i=0, bbp = bb; i <= nbad; i++, bbp++)
					bbp->cyl = bbp->trk = bbp->sec = 0;
				nbad = INVALID;
			
			}
		}
		bb_readfile(secbytes);
		return;
	}
#endif !STANDALONE

	printf("\nIf the drive is directly from the factory defects can be");
	printf("\nread from it ONLY ONCE before it is formatted.");
	printf("\nread factory defects from the drive (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;


	/*
	 * read media defect information off each track of the drive
	 */
	for (cyl = 0; cyl < dp->dp_cyls; cyl++) {
	    md.md_cyl = cyl;
	    for (trk = 0; trk < dp->dp_trks0; trk++) {
		md.md_trk = trk;
		if (ioctl(fd, DIOCRDEFECTS, &md) < 0) {
			printf("can't read defects on cyl=%d, trk=%d (%d)\n",
				cyl, trk, errno);
#ifndef STANDALONE
			fflush(stdout);
#endif !STANDALONE
			continue;
		}
		if (md.md_entry.sync != SYNC_PATTERN) {
			printf("bb_read; bad sync pattern = 0x%x\n", 
				md.md_entry.sync);
#ifndef STANDALONE
			fflush(stdout);
#endif !STANDALONE
			continue;
		}
		if (md.md_entry.tb != TB_PATTERH) {
			printf("bb_read: bad tb pattern = 0x%x\n", 
				md.md_entry.tb);
			continue;
		}
		for (i=0, d = md.md_entry.defects; i < MAX_DEFECTS; i++, d++) {
			if ((d->position == 0) && (d->length == 0)) {
				continue;
			}
			printf("\t(%d, %d, %d, %d) ", 
				md.md_entry.cylinder & ~DEFECTIVE_TRACK,
				md.md_entry.head, d->position, d->length);
			if (md.md_entry.cylinder & DEFECTIVE_TRACK)
				printf("DT");
			printf("\n");
#ifndef STANDALONE
			fflush(stdout);
#endif !STANDALONE

			sec = d->position / secbytes;
			if (bb_insert(cyl, trk, sec) == 0) {
				printf("\tmedia defect table full\n");
				break;
			}
			len = d->length >> 3; 	/* bits to bytes */
			if (len == 0)
				len = 1;
			if ( ((d->position + len) / secbytes) != sec) {
				if (bb_insert(cyl, trk, sec+1) == 0) {
					printf("\tmedia defect table full\n");
					break;
				}
			}
		}
	    }
	}
}

#ifndef STANDALONE
bb_readfile(secbytes)
int secbytes;
{
	register int cyl;
	register int trk;
	int sec;
	int len;
	int pos;
	FILE *streamptr;
	char buf[LINESIZE];

	printf("\nname of file? ");
	gets(buf);
	if (*buf == 0) {
		printf("invalid file name\n");
		return;
	}
	if ((streamptr = fopen(buf, "r")) < 0) {
		perror("fopen");
		exit(1);
	}
	while (1) {
		cyl = get_aint(streamptr);
		if (cyl == -1)
			break;
		trk = get_aint(streamptr);
		pos = get_aint(streamptr);
		len = get_aint(streamptr);
		printf("\t%d\t%d\t%d\t%d\n", cyl,trk,pos,len);

		sec = pos / secbytes;
		if (bb_insert(cyl, trk, sec) == 0) {
			printf("\tmedia defect table full\n");
			break;
		}
		len = len >> 3; 	/* bits to bytes */
		if (len == 0)
			len = 1;
		if ( ((pos + len) / secbytes) != sec) {
			if (bb_insert(cyl, trk, sec+1) == 0) {
				printf("\tmedia defect table full\n");
				break;
			}
		}
	}

}

get_aint(streamptr)
FILE *streamptr;
{
	char buf[32];
	char c;
	int i;

	/*
	 * skip leading blanks
	 */
	do {
		if (fread(&c, sizeof(char), 1, streamptr) != 1) {
			return (-1);
		}
	} while ((c == ' ') || (c == ',') || (c == '(') || (c == ')') ||
		(c == '\n') || (c == '\r') || (c == '\t'));
	buf[0] = c;
	for (i=1; i < 32; i++) {
		if (fread(&buf[i], sizeof(char), 1, streamptr) != 1) {
			return (-1);
		}
		if ((buf[i] == ',') || (buf[i] == ')') || (buf[i] == ' ')) {
			buf[i] = '\0';
			break;
		}
	}
	return (atoi(buf));
}
#endif !STANDALONE
/*
 * setup list of known scsi bad blocks
 */
scsibb_input()
{
	struct device_parameters *dp = &Vh.vh_dp;
	char buf[LINESIZE];
	int pba, i;

	printf("\nSCSI defect list manipulation, when prompted\n");
	printf("choose one of (list, add, delete, quit)\n");

	while (1) {
		printf("command? ");
		gets(buf);
		switch (*buf) {

		case 'l':
			scsibb_dump();
			break;

		case 'a':
			printf("enter 'physical block address' for each defect (q to quit)\n");
			for (i = 1; ; i++) {
			    printf("%d\t: ",  i);
			    gets(buf);
			    if (*buf == 'q')
				break;
			    if (*buf == 0)
				continue;
			    if (check_pba(buf, &pba) == 0)
				   continue;
			    if (scsibb_insert(pba) == 0) {
				printf("\tmedia defect table full\n");
				break;
			    }
			}
			break;

		case 'd':
			printf("enter 'physical block address': ");
			gets(buf);
			if (*buf == 0)
				break;
			if (check_pba(buf, &pba) == 0)
					break;
			scsibb_delete(pba);
			break;

		case 'q':
			return;

		default:
			printf("choose one of: add, delete, list, quit\n");
		}
	}
}

/*
 * setup list of known bad blocks
 */
bb_input()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i, choice;
	char buf[LINESIZE];
	int sec;
	int cyl;
	int hd;
	int pos;
	int len;
	int secbytes;

	printf("\nmedia defect list manipulation, when prompted\n");
	printf("choose one of (list, add, delete, quit)\n");

	if (!di_defaults[device].realbps) { /* don't know the real bytes/s */
	    if (dp->dp_flags & DP_SECTSLIP)
		if (dp->dp_runt)	/* we've got a runt now */
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 2);
		else
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 1);
	    else
		secbytes = di_defaults[device].trkbytes / dp->dp_secs;
	} else {
		secbytes = di_defaults[device].realbps;
	}
	
	while (1) {
		choice = 0;
		printf("command? ");
		gets(buf);
		switch (*buf) {

		case 'l':
			bb_dump();
			break;

		case 'a':
			printf("'cyl,head,pos,len' or 'cyl,head,sector' defect entry format can be used\n");
			printf("use 'cyl,head,pos,len' format (y if yes)? ");
			gets(buf);
			if (*buf == 'y') choice = 1;
			if (choice)
			printf("enter 'cyl,head,pos,len' for each defect (q to quit)\n");
			else
			printf("enter 'cyl,head,sector' for each defect (q to quit)\n");
			for (i = 1; ; i++) {
			    printf("%d\t: ",  i);
			    gets(buf);
			    if (*buf == 'q')
				break;
			    if (*buf == 0)
				continue;
			    if (choice) {
			       if (check_pbn(buf, &cyl, &hd, &pos, &len) == 0)
				   continue;
			       sec = pos / secbytes;
			    } else {
			       if (check_pbn1(buf, &cyl, &hd, &sec) == 0)
				   continue;
			    }
			    if (bb_insert(cyl, hd, sec) == 0) {
				printf("\tmedia defect table full\n");
				break;
			    }
			    if (choice) {
			    	len = len >> 3;	/* convert bits to bytes */
			    	if (len == 0)
					len = 1;
			    	if ( ((pos+len)/secbytes) != sec) {
				    if (bb_insert(cyl, hd, sec+1) == 0) {
					printf("\tmedia defect table full\n");
					break;
				    }
			    	}
			    }
			}
			break;

		case 'd':
			printf("'cyl,head,pos,len' or 'cyl,head,sector' defect entry format can be used\n");
			printf("use 'cyl,head,pos,len' format (y if yes)? ");
			gets(buf);
			if (*buf == 'y') choice = 1;
			if (choice)
				printf("enter 'cyl,head,pos,len': ");
			else
				printf("enter 'cyl,head,sector': ");
			gets(buf);
			if (*buf == 0)
				break;
			if (choice) {
				if (check_pbn(buf, &cyl, &hd, &pos, &len) == 0)
					break;
				sec = pos / secbytes;
			} else {
				if (check_pbn1(buf, &cyl, &hd, &sec) == 0)
					break;
			}
			bb_delete(cyl, hd, sec);
			if (choice) {
				len = len >> 3; /* convert bits to bytes */
				if (len == 0)
					len = 1;
				if ( ((pos+len)/secbytes) != sec)
					bb_delete(cyl, hd, sec+1);
			}
			break;

		case 'q':
			return;

		default:
			printf("choose one of: add, delete, list, quit\n");
		}
	}
}

/*
 * scan the disk for bad blocks 
 */
bb_scan()
{
	register struct partition_table *pt;
	struct device_parameters *dp = &Vh.vh_dp;
	int i,c;
	int cs, ch, cc;
	int lbn,track;
	int pass;
	int nseccyl;
	int trkbytes;
	int maxpass;
	char buf[LINESIZE];
	struct io_arg vi;
	int noecc;

	if (totalcyl == INVALID) {	/* haven't formatted */
		printf("\nformatting wasn't done, perform scan anyway (y if yes)? ");
		gets(buf);
		if (*buf != 'y')
			return;
	}
	if (totalcyl == INVALID) {	/* haven't formatted */
	   printf("scan entire disk (y if yes)? ");
	   gets(buf);
	   if (*buf != 'y') {
getpart:
		printf("entry number of partition to scan? ");
		gets(buf);
		for( i=0; *(buf+i) != '\0';i++){
		    if( !isdigit( *(buf+i))){
			printf("invalid partition entry %s\n", buf);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		    }
		}
		atob(buf, &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
		pt = &Vh.vh_pt[i];
		nseccyl = dp->dp_secs * dp->dp_trks0;
		startcyl = pt->pt_firstlbn / nseccyl;
		totalcyl = startcyl + (pt->pt_nblks / nseccyl);
	   } else {
		startcyl = 0;
		totalcyl = dp->dp_cyls;
	   }
	}
#ifndef SABLE
	bzero(wptr, DEV_BSIZE * MAX_NSECS);
#endif SABLE
	nseccyl = dp->dp_trks0 * dp->dp_secs;
	trkbytes = dp->dp_secbytes * dp->dp_secs;

	printf("\nscanning destroys disk data, perform scan (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;

	printf("\nnumber of scans for bad blocks (3 are suggested)? ");
	atob(gets(buf), &maxpass);

	noecc = 1;
	if (ioctl(fd, DIOCNOECC, &noecc) < 0)
		printf("WARNING: couldn't disable ecc correction\n");

	for (pass=0; pass < maxpass; pass++) {

		/* 
		 * init pattern for each sector and copy for each sector
		 */
		init_ptrn(wptr, dp->dp_secbytes, pass % MAX_SCANPASS);
		for (i=dp->dp_secbytes; i < trkbytes; i += dp->dp_secbytes) {
			bcopy(wptr, wptr+i, dp->dp_secbytes);
		}

		printf("\nscanning for defects, pass %d (hit ESCAPE to abort)",
							pass+1);
#ifndef STANDALONE
		fflush(stdout);
#endif STANDALONE


		for (cc = startcyl; cc < totalcyl; cc++) {
		    printf(".");
		    /*
		     * Check for the escape key incase the user wants to quit
		     */
		    ioctl(0,FIOCNBLOCK,1);
		    if( (c = getchar()) == ''){
		        ioctl(0,FIOCNBLOCK,0);
			goto stopscan;
		    }
		    ioctl(0,FIOCNBLOCK,0);
#ifndef STANDALONE
		    fflush(stdout);
#endif STANDALONE

		    for (ch = 0; ch < dp->dp_trks0; ch++) {
			lbn = (cc * nseccyl) + (ch * dp->dp_secs);
			vi.sectst = lbn;
			vi.datasz = dp->dp_secs;
			/* 
			 * check one track at a time 
			 */
			if ((lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
			    (write(fd, wptr, trkbytes) != trkbytes) ||
			    (lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
			    (ioctl(fd, DIOCVFYSEC, &vi) < 0)) {

				printf("\nError on cyl %d, trk %d\n", cc, ch);

				/* 
				 * find out which sector(s) is bad 
				 */
				for (cs = 0; cs < dp->dp_secs; cs++, lbn++) {
				    vi.sectst = lbn;
				    vi.datasz = 1;
				    if ((lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
					(write(fd, wptr, dp->dp_secbytes) 
					    != dp->dp_secbytes) ||
				        (lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
				        (ioctl(fd, DIOCVFYSEC, &vi) < 0)) {
					/*
					    printf("\nFound (%d,%d,%d) bad\n",
							cc, ch, cs);
					 */

				    /*
				     * Must offset by skew amount.  For example,
				     * If the skew for that track is 6, then
				     * sector 5 is really physical sector 11.
				     * And since all of the manufacturer's
				     * defects are from the index pulse, we
				     * must convert these.
				     */
				    if( dp->dp_skew ) {
				        track = (cc * dp->dp_trks0) + ch;
				        if( (cs = map_sec( LOG_TO_PHYS, track, cs )) < 0){
					    /*
					     * bad track, just map out any two
					     * sectors so that the entire track
					     * will be mapped 
					     */
				            if (bb_insert(cc, ch, cs) == 0)
					        printf("bad block list full\n");
					    /*
					     * Set to something different for 
					     * the following insert.
					     */
					    cs = ((cs == 0 ) ? cs - 1 : 1);
				        }

				    }
				    if (bb_insert(cc, ch, cs) == 0)
					printf("bad block list full\n");
				}
			}
		    }
		}
	    }
	}
stopscan:
	printf("\n");
	noecc = 0;
	if (ioctl(fd, DIOCNOECC, &noecc) < 0)
		printf("WARNING: couldn't reenable ecc correction\n");
}

/*
 * scan the SCSI disk for bad blocks 
 */
scsi_scan()
{
	register struct partition_table *pt;
	struct device_parameters *dp = &Vh.vh_dp;
	int i;
	int cs, ch, cc;
	int lbn;
	int pass;
	int trkbytes, secbytes, nseccyl;
	int maxpass;
	char buf[LINESIZE];
	int noecc;

	if (totalcyl == INVALID) {	/* haven't formatted */
		printf("\nformatting wasn't done, perform scan anyway (y if yes)? ");
		gets(buf);
		if (*buf != 'y')
			return;
	}
	pt = &Vh.vh_pt[volpart];
	if (totalcyl == INVALID) {	/* haven't formatted */
	   printf("scan entire disk (y if yes)? ");
	   gets(buf);
	   if (*buf != 'y') {
getpart:
		printf("entry number of partition to scan? ");
		atob(gets(buf), &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
		pt = &Vh.vh_pt[i];
	   }
	}
	startcyl = pt->pt_firstlbn;
	totalcyl = startcyl + pt->pt_nblks;
#ifndef SABLE
	bzero(wptr, DEV_BSIZE * MAX_NSECS);
	bzero(wptr_in, DEV_BSIZE * MAX_NSECS);
#endif SABLE
	nseccyl = dp->dp_trks0 * dp->dp_secs;
	secbytes = dp->dp_secbytes;
	trkbytes = secbytes * dp->dp_secs;

	printf("\nscanning destroys disk data, perform scan (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;

	printf("\nnumber of scans for bad blocks (3 are suggested)? ");
	atob(gets(buf), &maxpass);

	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.sectst = 3;
	    if (ioctl(fd, DIOCNOECC, &io_arg) < 0)
		printf("WARNING: couldn't disable ecc correction\n");
	} else {
	    noecc = 1;
	    if (ioctl(fd, DIOCNOECC, &noecc) < 0)
		printf("WARNING: couldn't disable ecc correction\n");
	}

	for (pass=0; pass < maxpass; pass++) {
		/* 
		 * init pattern for each sector and copy for each sector
		 */
		init_ptrn(wptr, dp->dp_secbytes, pass % MAX_SCANPASS);
		for (i=secbytes; i < trkbytes; i += secbytes) {
			bcopy(wptr, wptr+i, dp->dp_secbytes);
		}

		printf("\nstarting block is %d, ending block is %d\n",
							startcyl, totalcyl-1);
		printf("\nscanning for defects, pass %d ", pass+1);
#ifndef STANDALONE
		fflush(stdout);
#endif STANDALONE


		for (cc = startcyl; cc < totalcyl; cc += nseccyl) {
		    printf(".");
#ifndef STANDALONE
		    fflush(stdout);
#endif STANDALONE

		    /* 
		     * check one track at a time 
		     */
		    for (ch = 0; ch < dp->dp_trks0; ch++) {
			lbn = cc + (ch * dp->dp_secs);
			if ((lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
			    (write(fd, wptr, trkbytes) != trkbytes) ||
			    (lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
			    (read(fd, wptr_in, trkbytes) != trkbytes)) {

				/* 
				 * find out which sector(s) is bad 
				 */
				for (cs = 0; cs < dp->dp_secs; cs++, lbn++) {
				    if ((lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
					(write(fd, wptr, dp->dp_secbytes) 
					    != secbytes) ||
				        (lseek(fd, lbn*DEV_BSIZE, 0) < 0) ||
					(read(fd, wptr_in, dp->dp_secbytes) 
					    != secbytes)) {
					    if (scsibb_insert(lbn) == 0)
						printf("bad block list full\n");
				    }
				}
			}
		    }
		}
	}
	printf("\n");
out:
	if (ctlrtype == (SCSI|ON_BOARD)) {
	    io_arg.sectst = 0;
	    if (ioctl(fd, DIOCNOECC, &io_arg) < 0)
		    printf("WARNING: couldn't reenable ecc correction\n");
	} else {
	    noecc = 0;
	    if (ioctl(fd, DIOCNOECC, &noecc) < 0)
		    printf("WARNING: couldn't reenable ecc correction\n");
	}
}
/*
 * insert a SCSI bad block into list
 */
scsibb_insert(pba)
u_int pba;
{
	register int i;

	/* 
	 * bad block may already be recorded 
	 */
	for (i=0; i <= scsinbad; i++) {
		if (reassign.scsibb[i] == pba) {
		       printf("block %d already on the defect list\n", pba);
#ifndef STANDALONE
			fflush(stdout);
#endif STANDALONE
			return (1);
		}
	}
	if (i >= MAX_SCSIBADBLOCKS) {
		return (0);
	}
	printf("block %d added to the defect list\n", pba);
#ifndef STANDALONE
	fflush(stdout);
#endif STANDALONE
	/* 
	 * grab new entry for bad block 
	 */
	++scsinbad;
	reassign.scsibb[scsinbad] = pba;
	return (1);
}

/*
 * insert a bad block into list
 */
bb_insert(cyl, trk, sec)
u_short cyl;
u_char trk;
u_char sec;
{
	register int i;
	register struct bb_entry *bbp;

	/* 
	 * bad block may already be recorded 
	 */
	for (i=0, bbp = bb; i <= nbad; i++, bbp++) {
		if ((bbp->cyl == cyl)&&(bbp->trk == trk)&&(bbp->sec == sec)) {
			printf("block (%d,%d,%d) already on the defect list\n",
					cyl,trk,sec);
#ifndef STANDALONE
			fflush(stdout);
#endif STANDALONE
			return (1);
		}
	}
	if (i >= MAX_BADBLOCKS) {
		return (0);
	}
	printf("block (%d,%d,%d) added to the defect list\n",
			cyl,trk,sec);
#ifndef STANDALONE
	fflush(stdout);
#endif STANDALONE
	/* 
	 * grab new entry for bad block 
	 */
	++nbad;
	bb[nbad].cyl = cyl;
	bb[nbad].trk = trk;
	bb[nbad].sec = sec;
	return (1);
}

/*
 * delete a SCSI bad block from the list
 */
scsibb_delete(pba)
u_int pba;
{
	register int i;

	if (scsinbad == INVALID)
		return;

	for (i=0; i <= scsinbad; i++) {
	    if (reassign.scsibb[i] == pba) {
		reassign.scsibb[i] = reassign.scsibb[scsinbad];
		scsinbad--;
		return;
	    }
	}
}
/*
 * delete a bad block from the list
 */
bb_delete(cyl, trk, sec)
u_short cyl;
u_char trk;
u_char sec;
{
	register int i;
	register struct bb_entry *bbp;

	if (nbad == INVALID)
		return;

	for (i=0, bbp = bb; i <= nbad; i++, bbp++) {
	    if ((bbp->cyl == cyl) && (bbp->trk == trk) && (bbp->sec == sec)) {
		bb[i].cyl = bb[nbad].cyl;
		bb[i].trk = bb[nbad].trk;
		bb[i].sec = bb[nbad].sec;
		nbad--;
		return;
	    }
	}
}

/* 
 * map scsi bad blocks
 */
scsibb_map()
{
	struct device_parameters *dp = &Vh.vh_dp;
	struct fmt_map_info fmi;
	char buf[LINESIZE];
	
	/* if no defects just return */
	if (scsinbad == INVALID) return;

	fmi.fmi_action = FMI_MAP_TRACK;
	fmi.fmi_addr = (caddr_t) &reassign;
	reassign.length = (scsinbad + 1)*4; /* initialize reassign header */
	if (ctlrtype == (SCSI|ON_BOARD)) {
	    fmi.fmi_intrlv = 4 + (4*(scsinbad+1)); /* total bytes to dma */
	    io_arg.memaddr = (unsigned long)&fmi;
	    io_arg.datasz = (unsigned long)sizeof(fmi);
	    if (ioctl(fd, DIOCFMTMAP, &io_arg) < 0) {
		    printf("reassign blocks failed\n");
	    }
	} else {
	    fmi.fmi_cyl = 4 + (4*(scsinbad+1)); /* total nr of bytes to dma */
	    if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
		    printf("reassign blocks failed\n");
	    }
	}
}


/* 
 * map bad blocks
 */
bb_map()
{
	register struct partition_table *pt;
	struct device_parameters *dp = &Vh.vh_dp;
	struct bst_table *bstp;
	struct bb_entry *curbb;
	struct bb_entry *nxtbb;
	int i;
	int nxt_rpl;
	int nsecscyl;
	struct fmt_map_info fmi;
	char buf[LINESIZE];
	int startentry;
	int lastentry;
	unsigned int lbn;
	int track,hd;
	int begrepl,endrepl;
	int begtrk, trepl;
	struct partition_table *ppt;
	int trkindx,tindx, bbindx;
	
	if (totalcyl == INVALID) {	/* haven't formatted */
		printf("\nformat or scan wasn't done, perform mapping anyway (y if yes)? ");
		gets(buf);
		if (*buf != 'y')
			return;
	}
	if (totalcyl == INVALID) {	/* haven't formatted */
	   printf("map entire disk (y if yes)? ");
	   gets(buf);
	   if (*buf != 'y') {
getpart:
		printf("entry number of partition to map? ");
		atob(gets(buf), &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
		pt = &Vh.vh_pt[i];
		nsecscyl = dp->dp_secs * dp->dp_trks0;
		startcyl = pt->pt_firstlbn / nsecscyl;
		totalcyl = startcyl + (pt->pt_nblks / nsecscyl);
	   } else {
		startcyl = 0;
		totalcyl = dp->dp_cyls;
	   }
	}
	printf("\nmapping destroys disk data, perform map (y if yes)? ");
	gets(buf);
	if (*buf != 'y')
		return;

	if ((dp->dp_flags & (DP_SECTSLIP|DP_TRKFWD)) == 0) {
		printf("No support for track forwarding or sector slipping\n");
		return;
	}

	/* 
	 * get bad blocks within a track together 
	 */
	qsort(bb, nbad+1, sizeof(struct bb_entry), qcompar);

	/*
	 * find out which entries are to be mapped
	 */
	startentry = INVALID;
	lastentry = INVALID;
	for (i=0; i <= nbad; i++) {
		if ((startentry == INVALID) && (bb[i].cyl >= startcyl)
		    && (bb[i].cyl < totalcyl))
			startentry = i;
		if ((lastentry == INVALID) && (bb[i].cyl >= totalcyl)) {
			lastentry = i-1;
			break;
		}
	}
	nsecscyl = dp->dp_secs * dp->dp_trks0;
	if (startentry == INVALID)
		goto update_bst;
	if ((lastentry == INVALID) && (bb[nbad].cyl < totalcyl))
		lastentry = nbad;

	/*
	 * Map bad blocks.  First go through and slip all sectors.  This
	 * must be done first so that if any of the slipped sectors
	 * are in the replacment partition, those tracks can still be
	 * used as mapped tracks.
	 */
	trkindx = 0;
	for (i=startentry; i <= lastentry; i++) {
		curbb = &bb[i];
		if ((dp->dp_flags & DP_TRKFWD) && ((i+1) <= lastentry))
			nxtbb = &bb[i+1];
		else
			nxtbb = (struct bb_entry *)0;

		/* 
		 * slip sectors 
		 */
		if (dp->dp_flags & DP_SECTSLIP) {
			/* 
			 * If the next sector is on the same track, then
			 * forward the whole track, unless of course, the
			 * next sector is the runt sector.  If its the
			 * spare sector we still need to forward the whole
			 * track.
			 */
			if (nxtbb && (nxtbb->trk == curbb->trk) && 
				(nxtbb->cyl == curbb->cyl) && 
				(nxtbb->sec <= dp->dp_secs)) {
		    	    goto trkfwd;
			}
			lbn = (curbb->cyl * nsecscyl)+(curbb->trk * dp->dp_secs)
				+ curbb->sec;
			if( dp->dp_runt && curbb->sec > dp->dp_secs){
			    /*
			     * The runt sector is at the end of the track,
			     * even if there's skewing.
			     */
			    curbb->rpltype = BSTTYPE_RUNTBAD;
			    continue;
			}else if( dp->dp_skew ) {
			    /* 
			     * We must offset by the skew.  For example,
			     * if the physical bad sector is 17, and the
			     * skew offset for this track is 8, then the
			     * actually sector that we must send to the
			     * driver is 9.  BTW trks0 is really # of heads.
			     */

			    track = (curbb->cyl * dp->dp_trks0) + 
					curbb->trk;
			    fmi.fmi_sec = map_sec( PHYS_TO_LOG, track, 
			         curbb->sec);
			    if( fmi.fmi_sec == SLIP_PTRN || 
				fmi.fmi_sec == dp->dp_secs){
			        /*
				 * The slip sector is bad.  This can
				 * occur if this sector has already
				 * been mapped, or if it's the last sector
				 */
			        curbb->rpltype = BSTTYPE_SLIPBAD;
				continue;
			    }else if( fmi.fmi_sec < 0 ){
				/*
				 * Something wrong with the track.  Map
				 * the entire thing.
				 */
				goto trkfwd;
			    }
			}
			if (curbb->sec < dp->dp_secs || dp->dp_skew ) {
			    /*
			     * If there's no skew, then check that it's
			     * not a spare sector.
			     * If there's a skew, then we've already
			     * checked.
			     */
			    fmi.fmi_action = FMI_SLIP_SECTOR;
			    fmi.fmi_cyl = curbb->cyl;
			    fmi.fmi_trk = curbb->trk;
			    /*
			     * Already set up above.
			     */
			    if( !dp->dp_skew )
			        fmi.fmi_sec = curbb->sec;

			    if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
				if (dp->dp_flags & DP_TRKFWD) {
				    printf(", forwarding track\n");
				    goto trkfwd;
				} else {
				    printf(", bad block unhandled\n");
				    continue;
				}
			    }
		    	    curbb->rpltype = BSTTYPE_SLIPSEC;

			} else if (curbb->sec == dp->dp_secs) { 
			    /*
			     * If it's the spare sector, don't map it out,
			     * since its already spare.  However, keep
			     * it on the list.  If another sector goes
			     * bad, the whole track will need to be mapped.
			     */
			    curbb->rpltype = BSTTYPE_SLIPBAD;
			} 

		} else if (dp->dp_flags & DP_TRKFWD) {
trkfwd:
		    /* 
		     * For the first time through, just fill in
		     * the list of tracks to be mapped
		     */
		    if( trkindx > di_defaults[device].mdeftrks ){
		 	printf("format: no more replacement tracks\n");
		        continue;
		    }
		    if( trkindx > 0 && ((curbb->cyl == btrk[trkindx - 1].cyl)
		      && (curbb->trk == btrk[trkindx - 1].hd))){
			continue;
		    }
		    btrk[trkindx].cyl = curbb->cyl;
		    btrk[trkindx].hd = curbb->trk;
		    trkindx++;
		}
	}
	if( !dp->dp_flags & DP_TRKFWD)
	    goto update_bst;
	/*
	 * Find the starting and ending cylinder for the replacement partition.
	 */
	for (i=0, ppt = Vh.vh_pt; i < NPARTAB; i++, ppt++) {
	    if (ppt->pt_type == PTYPE_TRKREPL)
		break;
	}
	if (i == NPARTAB) {
	    printf("no replacment partition found\n");
	    return;
	}
	begrepl = ppt->pt_firstlbn / nsecscyl;
	endrepl = begrepl + (ppt->pt_nblks / nsecscyl);
	begtrk = ppt->pt_firstlbn / dp->dp_secs;

        /*
         * Now go through and map all of the bad tracks.  First
	 * go through and mark the tracks in the replacement partition
	 * as BBMAPPED.
	 */
	for( i = 0; i < trkindx; i++ ){
	    if( btrk[i].cyl >= begrepl && btrk[i].cyl <= endrepl ){
		trepl = ((btrk[i].cyl * dp->dp_trks0) + btrk[i].hd) - begtrk;
		mtrk[trepl] = BBMAPPED;
	    }
	}
	for( i = 0, tindx = 0, bbindx = 0; i < trkindx; i++, tindx++){
	    /*
	     * Get next replacement track
	     */
	    while( mtrk[tindx] != NOTMAPPED ){
		if( tindx >= di_defaults[device].mdeftrks ){
		    printf("format: No more replacement tracks\n");
		    return;
		}
		tindx++;
	    }


	    fmi.fmi_action = FMI_MAP_TRACK;
	    fmi.fmi_cyl = btrk[i].cyl;
	    fmi.fmi_trk = btrk[i].hd;
	    fmi.fmi_rplcyl = begrepl + ( tindx / dp->dp_trks0);
	    fmi.fmi_rpltrk = (begtrk +  tindx) % dp->dp_trks0;
	    if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
		printf("format: ERROR during track forwarding of\n");
		printf("cyl %d trk %d to replacement cyl %d trk %d\n",curbb->cyl,curbb->trk,fmi.fmi_rplcyl,fmi.fmi_rpltrk);
		continue;
	    }
	    /*
	     * Need to also find this entry in the bb list, so that we
	     * can mark it correctly
	     */
	    while( (bb[bbindx].cyl != btrk[i].cyl) ||
		   (bb[bbindx].trk != btrk[i].hd) )
		bbindx++;

	    /* 
	     * Update this entry, and any more that are on the same track.
	     */
    	    do {
		bb[bbindx].rpltype = BSTTYPE_TRKFWD;
		bb[bbindx].rpllbn = begtrk + tindx;
		bbindx++;
	    } while ((bb[bbindx].cyl == btrk[i].cyl) && 
				(bb[bbindx].trk == btrk[i].hd));
	}

	/*
	 * update bad sector table
	 */
update_bst:
	for (i=0, bstp = bsttab, curbb = bb; i <= nbad; i++, curbb++, bstp++) {
		switch (curbb->rpltype) {
		case BSTTYPE_SLIPSEC:
			bstp->bt_rpltype = BSTTYPE_SLIPSEC;
			bstp->bt_badlbn = (curbb->cyl * nsecscyl) + 
					  (curbb->trk * dp->dp_secs) + 
					  curbb->sec;
			break;
		case BSTTYPE_TRKFWD:
			bstp->bt_rpltype = BSTTYPE_TRKFWD;
			bstp->bt_badlbn = (curbb->cyl * nsecscyl) + 
					  (curbb->trk * dp->dp_secs) + 
					  curbb->sec;
			bstp->bt_rpllbn = curbb->rpllbn;
			break;
		case BSTTYPE_SLIPBAD:
			bstp->bt_rpltype = BSTTYPE_SLIPBAD;
			bstp->bt_badlbn = (curbb->cyl * nsecscyl) + 
					  (curbb->trk * dp->dp_secs) +
					  (curbb->sec - 1);
			break;
		case BSTTYPE_RUNTBAD:
			bstp->bt_rpltype = BSTTYPE_RUNTBAD;
			bstp->bt_badlbn = (curbb->cyl * nsecscyl) + 
					  (curbb->trk * dp->dp_secs) + 
					  curbb->sec;
			break;
		default:
			bstp->bt_rpltype = BSTTYPE_NOTREPL;
			bstp->bt_badlbn = (curbb->cyl * nsecscyl) + 
					  (curbb->trk * dp->dp_secs) + 
					  curbb->sec;
			break;
		}
	}
}

/*
 * dump list of SCSI bad blocks
 */
scsibb_dump()
{
	register int i;
	register unsigned int lbn;
	register struct device_parameters *dp = &Vh.vh_dp;

	if (scsinbad == INVALID) {
		printf("\tno defects in list\n");
		return;
	}
	printf("\tphysical block address\n");
	for (i=0; i <= scsinbad; i++) {
		printf("\t0x%x (%d)\n",reassign.scsibb[i],reassign.scsibb[i]); 
	}
}
/*
 * dump list of bad blocks
 */
bb_dump()
{
	register int i;
	register unsigned int lbn;
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct bb_entry *bbp;
	register int nsecscyl;
	register int secbytes;

	if (nbad == INVALID) {
		printf("\tno defects in list\n");
		return;
	}
	if (!di_defaults[device].realbps) { /* don't know the real bytes/s */
	    if (dp->dp_flags & DP_SECTSLIP)
		if (dp->dp_runt)	/* we've got a runt now */
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 2);
		else
		   secbytes = di_defaults[device].trkbytes / (dp->dp_secs + 1);
	    else
		secbytes = di_defaults[device].trkbytes / dp->dp_secs;
	} else {
		secbytes = di_defaults[device].realbps;
	}
	printf("\tlbn\t(cyl, head, sec, bytes from index)\n");
	nsecscyl = dp->dp_trks0 * dp->dp_secs;
	for (i=0, bbp = bb; i <= nbad; i++, bbp++) {
		lbn = (bbp->cyl * nsecscyl) + (bbp->trk * dp->dp_secs) 
				+ bbp->sec;
		if (bbp->rpltype == BSTTYPE_SLIPBAD)
			printf("\t%s\t(%d, %d, %d, %d)\n", 
				"slip", bbp->cyl, bbp->trk, bbp->sec,bbp->sec*secbytes);
		else
			printf("\t%d\t(%d, %d, %d, %d)\n", 
				lbn, bbp->cyl, bbp->trk, bbp->sec,bbp->sec*secbytes);
	}
}

/*
 * parse and check physical block number string 
 */
check_pbn(str, cylp, hdp, posp, lenp)
char *str;
int *cylp, *hdp, *posp, *lenp;
{
	char *cp;
	struct device_parameters *dp = &Vh.vh_dp;

	*cylp = *hdp = *posp = *lenp = 0;
	cp = str;
	if (*cp == '(')
		cp++;

	if (*cp != ')') {
		while (isspace(*cp))
			cp++;
		cp = atob(cp, cylp);
		if (*cp == ',') {
			cp++;
			while (isspace(*cp))
				cp++;
			cp = atob(cp, hdp);
			if (*cp == ',') {
				cp++;
				while (isspace(*cp))
					cp++;
				cp = atob(cp, posp);
				if (*cp == ',') {
					cp++;
					while (isspace(*cp))
						cp++;
					cp = atob(cp, lenp);
				}
			}
		}
	}

	/* 
	 * check for valid physical bad block number 
	 */
	if ((*cylp < 0) || (*cylp >= dp->dp_cyls)) {
		printf("invalid cylinder %d, range is 0 thru %d\n", 
		    *cylp, dp->dp_cyls-1);
		return (0);
	}
	if ((*hdp < 0) || (*hdp >= dp->dp_trks0)) {
		printf("invalid head %d, range is 0 thru %d\n", 
		    *hdp, dp->dp_trks0-1);
		return (0);
	}
	if ((*posp < 0) || (*posp >= di_defaults[device].trkbytes)) {
		printf("invalid position %d, range is 0 thru %d\n",
		    *posp, di_defaults[device].trkbytes-1);
		return (0);
	}
	if ((*lenp <= 0) || (*lenp > di_defaults[device].length)) {
		printf("invalid length %d, range is 1 thru %d\n", 
		    *lenp, di_defaults[device].length);
		return (0);
	}
	return (1);
}
/*
 * parse and check scsi physical block address 
 */
check_pba(str, pba)
char *str;
int *pba;
{
	char *cp;
	struct device_parameters *dp = &Vh.vh_dp;
	int max;

	*pba = 0;
	cp = str;
	if (*cp == '(')
		cp++;

	if (*cp != ')') {
		while (isspace(*cp))
			cp++;
		cp = atob(cp, pba);
	}
	/* 
	 * check for valid physical bad block number 
	 */
	max = dp->dp_cyls * dp->dp_secs * dp->dp_trks0;
	if ((*pba < 0) || (*pba >= max)) {
	      printf("invalid physical bock address %d, range is 0 thru %d\n",
		    *pba, max-1);
	      return (0);
	}
	return (1);
}
/*
 * parse and check physical block number string 
 */
check_pbn1(str, cylp, hdp, secp)
char *str;
int *cylp, *hdp, *secp;
{
	char *cp;
	struct device_parameters *dp = &Vh.vh_dp;

	*cylp = *hdp = *secp = 0;
	cp = str;
	if (*cp == '(')
		cp++;

	if (*cp != ')') {
		while (isspace(*cp))
			cp++;
		cp = atob(cp, cylp);
		if (*cp == ',') {
			cp++;
			while (isspace(*cp))
				cp++;
			cp = atob(cp, hdp);
			if (*cp == ',') {
				cp++;
				while (isspace(*cp))
					cp++;
				cp = atob(cp, secp);
			}
		}
	}

	/* 
	 * check for valid physical bad block number 
	 */
	if ((*cylp < 0) || (*cylp >= dp->dp_cyls)) {
		printf("invalid cylinder %d, range is 0 thru %d\n", 
		    *cylp, dp->dp_cyls-1);
		return (0);
	}
	if ((*hdp < 0) || (*hdp >= dp->dp_trks0)) {
		printf("invalid head %d, range is 0 thru %d\n", 
		    *hdp, dp->dp_trks0-1);
		return (0);
	}
	/*
	 * Have to allow it to be equal to secs, since the bad sector
	 * could be the slipped sector 
	 */
	if ((*secp < 0) || (*secp > dp->dp_secs)) {
		printf("invalid sector %d, range is 0 thru %d\n",
		    *secp, dp->dp_secs);
		return (0);
	}
	return (1);
}

/*
 * initialize data pattern used when scanning for bad blocks
 */
init_ptrn(buf, size, offset)
char buf[];
int size;
int offset;
{
	int i;
	char pattern[3];
	int resid;

	pattern[0] = 0xdb;
	pattern[1] = 0x6d;
	pattern[2] = 0xb6;

	for (i=2; i < size; i+=3) {
		buf[i-2] = pattern[offset % 3];
		buf[i-1] = pattern[(1+offset) % 3];
		buf[i]   = pattern[(2+offset) % 3];
	}
	resid = size % 3;
	if (resid == 2) {
		buf[size-2] = pattern[offset % 3];
		buf[size-1] = pattern[(1+offset) % 3];
	} else if (resid == 1) {
		buf[size-1] = pattern[offset % 3];
	}
}

/*
 * comparison routine used by qsort()
 */
qcompar(l1, l2)
struct bb_entry *l1;
struct bb_entry *l2;
{
	if (l1->cyl < l2->cyl)
		return (-1);
	if (l1->cyl > l2->cyl)
		return (1);

	if (l1->trk < l2->trk)
		return (-1);
	if (l1->trk > l2->trk)
		return (1);

	if (l1->sec < l2->sec)
		return (-1);
	if (l1->sec > l2->sec)
		return (1);
	return (0);
}

static char *
btoa(val)
register int val;
{
	static char buf[3];
	register char *cp;

	if (val < 0 || val > 99)
		return("1");

	cp = buf;
	if (val >= 10) {
		*cp++ = '0' + (val / 10);
		val %= 10;
	}
	*cp++ = '0' + val;
	*cp = 0;
	return(buf);
}

#ifndef STANDALONE
/*
 * atob -- convert ascii to binary.  Accepts all C numeric formats.
 */
char *
atob(cp, iptr)
register char *cp;
int *iptr;
{
	register unsigned base = 10;
	register int value = 0;
	register unsigned d;
	int minus = 0;
	int overflow = 0;

	*iptr = 0;
	if (!cp)
		return(0);

	while (isspace(*cp))
		cp++;

	while (*cp == '-') {
		cp++;
		minus = !minus;
	}

	/*
	 * Determine base by looking at first 2 characters
	 */
	if (*cp == '0') {
		switch (*++cp) {
		case 'X':
		case 'x':
			base = 16;
			cp++;
			break;

		case 'B':	/* a frill: allow binary base */
		case 'b':
			base = 2;
			cp++;
			break;
		
		default:
			base = 8;
			break;
		}
	}

	while ((d = digit(*cp)) < base) {
		if ((unsigned)value > ((unsigned)-1/base))
			overflow++;
		value *= base;
		if ((unsigned)value > (unsigned)(-1 - d))
			overflow++;
		value += d;
		cp++;
	}

	if (overflow || (value == 0x80000000 && minus))
		printf("WARNING: conversion overflow\n");
	if (minus)
		value = -value;

	*iptr = value;
	return(cp);
}

/*
 * is_vh -- decide if we're looking at a reasonable volume_header
 */
is_vh(vhp)
struct volume_header *vhp;
{
	register csum;
	register int *ip;

	if (vhp->vh_magic != VHMAGIC)
		return(0);

	csum = 0;
	for (ip = (int *)vhp; ip < (int *)(vhp + 1); ip++)
		csum += *ip;
	return(csum == 0);
}

/*
 * digit -- convert the ascii representation of a digit to its
 * binary representation
 */
unsigned
digit(c)
char c;
{
	unsigned d;

	if (isdigit(c))
		d = c - '0';
	else if (isalpha(c)) {
		if (isupper(c))
			c = tolower(c);
		d = c - 'a' + 10;
	} else
		d = 999999; /* larger than any base to break callers loop */

	return(d);
}
#endif !STANDALONE

#ifndef STANDALONE

bzero(dest,length)
register char *dest;
register int length;
{
	for(;length;length--)
		*dest++ = 0;
}

bcopy(src,dest,length)
register char *src;
register char *dest;
register int length;
{
	for(;length;length--)
		*dest++ = *src++;
}
#endif STANDALONE

map_sec( whichway, track, sector )
int whichway;
int track;
int sector;
{

    int newsec;
    int i, slipi;
    struct device_parameters *dp = &Vh.vh_dp;
    int hasskew;
    struct fmt_map_info fmi;
    int tried = 0;
    char buf[LINESIZE];

again:
    io_arg.memaddr = (unsigned long)track_id;
    io_arg.sectst = (unsigned long)track;
    bzero(track_id, sizeof(struct track_id) * MAX_NSECS);
    if (ioctl(fd,DIOCTRKID, &io_arg) < 0) {
	printf("Cannot get Track ID\n");
	return(0);
    }

    if( whichway == PHYS_TO_LOG){
	newsec = track_id[sector].sector;
    } else{
	for( i = 0; i < MAX_NSECS; i++)
	    if( track_id[i].sector == sector )
		break;
	    else if( track_id[i].sector == SLIP_PTRN ){
		/* 
		 * This track already has a slipped sector, and this
		 * one better be the one that's slipped. 
		 */
		if( i == dp->dp_secs )
		   slipi = 0;
		else
		   slipi = i+1;
		if( track_id[slipi].sector != sector ){
		    printf("Trying to slip sector %d, but sector %d has already been slipped\n", sector, track_id[slipi].sector);
		    return(-1);
		}
	    }
	if( i == MAX_NSECS ){
	    /*
	     * Format this track again, and then see if it's okay
	     */
	    fmi.fmi_action = FMI_FORMAT_TRACK;
	    fmi.fmi_trk = track_id[0].head;
	    fmi.fmi_cyl = track_id[0].cylno;
retry:
  	    if (ioctl(fd, DIOCFMTMAP, &fmi) < 0) {
		printf("format error(%d) on cyl %d head %d.  Retry [n] ?\n",
					errno, fmi.fmi_cyl, fmi.fmi_trk);
		gets(buf);
		if (buf[0] == 'y')
		    goto retry;
	    }
	    if( !tried ){
	       tried++;
	       goto again;
	    }else{
	       printf("Sector %d is an invalid sector to slip.\n");
	       printf("Remapping entire track.\n");
	       return( -1 );
	    }
	}
	newsec = i;
    }
    return( newsec);
}
