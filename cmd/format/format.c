#ident "$Header: format.c,v 1.2.1.6 91/01/14 19:03:31 beacker Exp $"

/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
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

#define DEV_BSIZE	512
#define NBPP		4096

#include <sys/errno.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/dvh.h>
#include <sys/ioctl.h>
#include <sys/dkio.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <termio.h>
#include <unistd.h>
#include <string.h>

extern int errno;

#define PHYS_TO_LOG 	0
#define LOG_TO_PHYS	1
#define SLIP_PTRN	0xff

#define PAGEALIGN(ptr) ((int)((ptr) + NBPP-1) & ~(NBPP-1))
int qcompar();
unsigned conv_digit();
static char *localatob();
extern char *gets();
struct volume_directory *bst_allocsp();
int catchit();
int nsecscyl;

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
#define LINESIZE	1024
#define MAX_SCANPASS	3
#define MAX_BADBLOCKS	4000
#define MAX_NSECS	84	/* Must include the slip sector 	*/
#define MAX_MTRKS	84	/* This is the maximum number of tracks
				 * in the TRKREPL partition.  There are
				 * 4 cylinders for the 2392, so that means
				 * that the max is 84.  This will change
				 * with larger drives 
				 */
#define MAX_SCSIBADBLOCKS	500	/* what should this be? */
#define INVALID	 	-1	

static int Fd;	/* File descriptor of raw disk to be formatted	*/
static int sngl_fd;	/* Fd when only formatting or scanning one partition */
static int nbad;/* # of bad spots, both on-disk and found during this session */
static int scsinbad;	/* SCSI can only track bad spots during this session  */
static int device;		/* Index into device_parameters array	*/
static int volpart;		/* Partition number of entire volume	*/
static int dpnretries;
static int dpreseek;
static int startcyl = INVALID;
static int totalcyl = INVALID;
static struct io_arg io_arg;	/* Structure to pass to IOCTL		*/
static struct volume_header Vh;
static struct volume_header Savedvh;
static struct ctlr_info ct;
static GEOMETRY_INFO geom;

static char bigbst[ MAX_BADBLOCKS * sizeof(struct bst_table) + NBPP + 1];
static struct bst_table *bsttab;
static struct partition_table *rplpart;
static char linebuf[LINESIZE];
static char buf2[LINESIZE];
static struct track_id *track_id;
static char *track_buf;
static int fcflag = 0;	/* Flag before fcntl is called to allow ESC to be hit */
static int nblkflag;		/* flag with NDELAY set	*/
static int atty = 0;		/* 1 if stdin is a tty 	*/
static struct termio orig_termio;
static struct termio new_termio;
static char devarray[32];


/*
 * bad block list entry (used internally)
 */
static struct bb_entry {
        u_short         cyl;
        u_char          trk;
        u_char          sec;
        int             rpltype;
        int             rpllbn;
} bb[MAX_BADBLOCKS];


static struct reassign {
	char	pad;
	char 	pad1;
	u_short length;
	u_int   scsibb[MAX_SCSIBADBLOCKS];
} reassign;

static char *wptr;
static char wbuf[DEV_BSIZE * MAX_NSECS + NBPP + 1];

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
int formatting;

/*
 * known types of disk drives
 */
#define DEV_F2322       0
#define DEV_F2333       1       /* 63 sectors */
#define DEV_F2333R      2       /* 64 sectors */
#define DEV_F2344       3       /* 63 sectors */
#define DEV_F2344R      4       /* 64 sectors */
#define DEV_F2372       5       /* 63 sectors */
#define DEV_F2372R      6       /* 64 sectors */
#define DEV_F2392       7       /* 83 sectors */
#define DEV_F2392R      8       /* 80 sectors */
#define DEV_OTHER       9
#define MAX_DEVICES    10

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
static struct device_parameters dp_defaults[] = {
/* 
 *sk gp1 gp2 runt cyls h0 trk0 h1 trk1 secs byts il flgs dr nr mpw type sp3 sp4 
 */
  {0, 12, 12,  1,  823, 0, 10, 0,  0,  32,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  0,  823, 0, 10, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  1,  823, 0, 10, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  0,  624, 0, 27, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  1,  624, 0, 27, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  0,  745, 0, 27, 0,  0,  63,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  1,  745, 0, 27, 0,  0,  64,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {4,  9, 10,  0, 1916, 0, 21, 0,  0,  83,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0, 12, 12,  0, 1916, 0, 21, 0,  0,  80,  512, 1, 0x25, 0, 3, 0,  0, 0, 0},
  {0,  0,  0,  0,    0, 0,  0, 0,  0,   0,    0, 0,    0, 0, 0, 0,  -1, 0, 0}
};

/*
 * milliseconds per word on data transfer, used by unix commands like iostat(1).
 * NOTE: order corresponds to DEV_ defines 
 */
static int dev_mspw[] = {
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
static char *dev_names[] = {
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
static struct device_info {
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
static struct dev_info dev_attribs[] = {
	/* value		name 					*/
	{  DP_SECTSLIP,		"sector slipping"			},
	{  DP_SECTFWD,		"sector forwarding"			},
	{  DP_TRKFWD,		"track forwarding"			},
	{  DP_MULTIVOL,		"multiple volumes"			},
	{  DP_IGNOREERRORS,	"transfering data regardless of errors"	},
	{  DP_RESEEK,		"recalibrate as last resort"		}
};

static char attr_on[MAX_ATTRIBUTES];
/*
 * names corresponding to types of partitions
 */
#define NUM_PTYPES	7
static char *part_types[] = {
	"volume header  ",
	"track replace  ",
	"sector replace ",
	"raw data       ",
	"BSD file sys   ",
	"SysV file sys  ",
	"entire volume  "
};

extern off_t lseek();
extern void perror();
extern void exit();
extern void qsort();

/*
 * main routine
 */
main()
{

	int stat_ret;
	struct stat buf;
	char *tmp;

	Fd = INVALID;
	sngl_fd = INVALID;
	nbad = INVALID;
	scsinbad = INVALID;
	device = INVALID;
	rplpart = NULL;
	bsttab = (struct bst_table *)PAGEALIGN(bigbst);
	bsttab[0].bt_rpltype = BSTTYPE_EMPTY;
	formatting = 0;
	setbuf(stdout, NULL);

	if( atty = isatty(0)){
	    if( (fcflag = fcntl(0,F_GETFL,0)) < 0 ){
		perror("F_GETFL");
		exit(1);
	    }
	    if( ioctl( 0, TCGETA, &orig_termio ) == -1 ){
		perror("TCGETA");
		exit(1);
	    }
	    new_termio = orig_termio;
            new_termio.c_iflag &= ~(ICRNL|INLCR|IGNCR|BRKINT);
            new_termio.c_lflag &= ~(ICANON|ECHO);
            new_termio.c_cc[VTIME] = 1;
            new_termio.c_cc[VMIN] = 1;
	}

	printf("\nMIPS Format Utility\n");
	printf("\n\nWARNING: Please refer to the Technical Reference Manual\n");
	printf("\t for this machine before running this program.\n");
	printf("\t Certain responses to many of the questions result in\n");
	printf("\t the permanent loss of data from the disks on this machine.\n\n");

	sigset(SIGINT, catchit);
	sigset(SIGTERM, catchit);
	/*
	 * initialize volume header
	 */
again:
	printf("\nEnter full path to /dev entry for entire volume? ");
	if( fgets(devarray, 32, stdin) == NULL )
	    goto again;
	if (*devarray == '\n')
		goto again;
	tmp = devarray;
	while( *tmp != '\n' && *tmp != '\0')
	    tmp++;
	*tmp = '\0';

	/*
	 * Make sure it's the raw device.
	 */

	if( (stat_ret = stat( devarray, &buf)) < 0 ){
		fprintf(stderr, "cannot stat %s\n", devarray);
		perror("stat");
		goto again;
	}
	if( (buf.st_mode & S_IFMT) != S_IFCHR){
	    fprintf(stderr, "Must enter a character device name.\n");
	    goto again;
	}

	if( (buf.st_rdev & 0xf) != PTNUM_VOLUME){
	    fprintf(stderr, "Warning: Device is not partition %d, and may not contain entire volume\n", PTNUM_VOLUME);
	}

	if ((Fd = open(devarray, O_RDWR)) < 0) {
		fprintf(stderr, "cannot open %s\n", devarray);
		perror("open");
		goto again;
	}

	io_arg.memaddr = (unsigned long)&ct;
	io_arg.datasz = (unsigned long)sizeof(ct);
	if (ioctl(Fd, DIOCGETCTLR, &io_arg) < 0) {
		pioerr("DIOCGETCTLR");
		exit(1);
	}

	/*
	 * do formatting and bad block remapping
	 */
	if ( ct.ci_flags & DP_SCSI ) {
		if (scsi_initvh() == 0)
		    exit(1);
		scsi_format();
		scsi_scan();
		scsibb_input();
		scsibb_map();
	 	scsi_wrapup(0);
	}else if (ct .ci_flags & DP_SMD ){
		if (smd_initvh() == 0)
		    exit(1);
                bb_read();
                smd_format();
                bb_scan();
                bb_input();
                bb_map();

                printf("\ndump bad sector table (y if yes)? ");
                if( (fgets(linebuf, LINESIZE,stdin)) != NULL && *linebuf == 'y')
                    bst_dump();

		printf("\nwrite defects to file (y if yes)? " );
	        if( fgets(linebuf, LINESIZE, stdin) != NULL ){
	            if(*linebuf == 'y') {
		        bb_writefile();
	            }
	        }
		smd_wrapup(0);
	} else {
		fprintf(stderr,"Controller type not supported\n");
		exit(1);
	}
	exit(0);
}

/*
 * initial setup of the volume header information
 */
scsi_initvh()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i;
	register int *ip, csum;
	struct partition_table *ppt;
	struct dev_info *dptr;
	int ntrys;

	io_arg.memaddr = (unsigned long)&geom;
	io_arg.datasz = (unsigned long)sizeof(geom);
	ntrys = 4;
	while (ntrys-- ){
	    if (ioctl(Fd, DIOCDISKGEOM, &io_arg) < 0) {
		fprintf(stderr,"Cannot get disk geometry info. Retrying...\n");
		continue;
	    }
	    if( !geom.geom_cyl || !geom.geom_head || !geom.geom_spt ||
		!geom.geom_bps || !geom.geom_tpz){
		fprintf(stderr,"Invalid disk geometry info. Retrying...\n");
		continue;
	    }
	    goto geom_ok;
	}
	pioerr("DIOCDISKGEOM");
	if( io_arg.retval == DIOC_DISKBUSY)
	    exit(3);
	return(0);
geom_ok:

	io_arg.memaddr = (unsigned long)&Vh;
	io_arg.datasz = (unsigned long)sizeof(Vh);
        if (ioctl(Fd, DIOCGETVH, &io_arg) < 0) {
		memset(&Vh, sizeof(struct volume_header),0);

		/*
		 * initialize volume header with defaults
		 */
		scsidp_init();
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
	} else {
		/*
		 * Set attribute array
		 */
		for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES;i++,dptr++) {
			if (dp->dp_flags & dptr->di_value)
				attr_on[i] = 1;
			else 
				attr_on[i] = 0;
		}
		dp->dp_flags |= DP_SCSI;
		if (dp->dp_mspw == 0)
			dp->dp_mspw = 260;
	}

	/*
	 * verify device parameters 
	 */
	while (1) {
		printf("\ndump device parameters (y if yes)? ");
		if( fgets(buf2, LINESIZE, stdin) != NULL )
		    if (*buf2 == 'y')
			scsidp_dump();
		printf("modify device parameters (y if yes)? ");
		if( fgets(buf2, LINESIZE, stdin) != NULL ){
		    if (*buf2 == 'y') {
			scsidp_get();
		    } else
			break;
		}
	}

	nsecscyl = dp->dp_secs * dp->dp_trks0;
	if( nsecscyl == 0 ){
	    fprintf("Number of sectors per track can't be zero.\n");
	    exit(2);
	}

	
	/*
	 * verify partition table information
	 */
parttab:
	while (1) {
		printf("\ndump partition table (y if yes)? ");
		if( fgets(buf2, LINESIZE, stdin) != NULL ){
		    if (*buf2 == 'y')
			pt_dump();
		}
		printf("modify partition table (y if yes)? ");
		if( fgets(buf2, LINESIZE, stdin) != NULL ){
		    if (*buf2 == 'y')
			pt_modif();
		    else
			break;
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
	    fprintf(stderr,"no entire volume partition found, please specify one\n");
	    goto parttab;
	}


	volpart = i;
	/* 
	 * set volume header 
	 */
	Vh.vh_magic = VHMAGIC;
	Vh.vh_csum = 0;
	for (csum = 0, ip = (int *)&Vh; ip < (int *)(&Vh+1); ip++) {
		csum += *ip;
	}
	Vh.vh_csum = -csum;
	io_arg.memaddr = (unsigned long)&Vh;
	io_arg.datasz = (unsigned long)sizeof(Vh);
	if (ioctl(Fd, DIOCSETVH, &io_arg) < 0) {
		pioerr("DIOCSETVH");
		if( io_arg.retval == DIOC_DISKBUSY)
   	            exit(3);
		return(0);
	}

	return (1);
}

/*
 * write volume header and bad sector table
 */
scsi_wrapup( caught )
int caught;
{
	int i;
	struct device_parameters *dp = &Vh.vh_dp;
	int *ip;
	int csum;

tryagain:
	fflush(stdin);
	if( caught )
	    printf("\nwrite new volume header? [y or n] ");
	else
	    printf("\nwrite new volume header? (n if no) ");
	if( fgets(linebuf,LINESIZE, stdin) == NULL || *linebuf == 'n') {
		close(Fd);
		return;
	}
	if( caught && *linebuf != 'y' )
	    goto tryagain;
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

	/* 
	 * write volume header in the first sector of each track of cylinder 0 
	 */
	printf("\nwriting volume header... ");
	for (i=0; i < dp->dp_trks0; i++) {
		if (lseek(Fd, (off_t)(i * dp->dp_secs * DEV_BSIZE), 0) < 0) {
			fprintf(stderr,"lseek for volhdr failed\n");
			continue;
		}
		if (write(Fd, (char *)&Vh, DEV_BSIZE) != DEV_BSIZE) {
			fprintf(stderr, "write of volhdr at trk %d failed\n", i);
		}
	}
	printf("\n");
	io_arg.memaddr = (unsigned long)&Vh;
	io_arg.datasz = (unsigned long)sizeof(Vh);
	if (ioctl(Fd, DIOCSETVH, &io_arg) < 0) {
		pioerr("DIOCSETVH");
		if( io_arg.retval == DIOC_DISKBUSY)
   	            exit(3);
		return;
	}
	close(Fd);
}

/*
 * format a SCSI disk
 */
scsi_format()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	struct fmt_map_info fmi;
	int child_pid, error;

	printf("\nformatting destroys ALL SCSI disk data, perform format (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
		return;

	startcyl = 0;
	totalcyl = dp->dp_cyls;		/* needed for scan */

	/*
	 * Need to fork here so that we can print out dots.
	 */
	child_pid = fork();
	if( child_pid == -1 ){
	    /* Can't create child process for some reason, so
	     * just can't print dots 
	     */
	    printf("\nformatting (This can take up to 45 minutes)...");
	}
	if (child_pid == 0 ){
	    formatting = 1;
	    printf("\nformatting ");
	    fflush(stdout);
	    while(1){
		sleep(2);
		printf(".");
		fflush(stdout);
	    }
	}else{
	   
	    fmi.fmi_action = FMI_FORMAT_TRACK;
	    fmi.fmi_intrlv = dp->dp_interleave;
	    if( !dp->dp_tpz )
	        dp->dp_tpz = 1;
	    fmi.fmi_tpz = dp->dp_tpz;		/* tracks per zone */
	    if( !dp->dp_aspz )
	        dp->dp_aspz = 1;
	    fmi.fmi_aspz = dp->dp_aspz;		/* alt sectors per zone */
	    if(!dp->dp_atpv)
	        dp->dp_atpv = dp->dp_trks0 * 2;
	    fmi.fmi_atpv = dp->dp_atpv;		/* alt tracks per volume */
	    io_arg.memaddr = (unsigned long)&fmi;
	    io_arg.datasz = (unsigned long)sizeof(fmi);
	    if (ioctl(Fd, DIOCFMTMAP, &io_arg) < 0)
		error = errno;
	    else
		error = 0;
	    if( kill(child_pid, SIGKILL) == -1 && errno != ESRCH){
		fprintf(stderr,"Could not kill child process %d\n", child_pid);
	        perror("");
	    }
	    formatting = 0;
	    if( error ){
		errno = error;
	        pioerr("DIOCFMTMAP");
	        exit(1);
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
	int fstype, tmp, tmp1, tmpcyls;
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
	if( (dp->dp_flags & DP_SCSI) == 0 ){
            if (dp->dp_flags & DP_TRKFWD) {
                pt->pt_type = PTYPE_TRKREPL;
                cyls = (di_defaults[device].mdeftrks + dp->dp_trks0 - 1) /
                    dp->dp_trks0;
            } else {
                pt->pt_type = PTYPE_SECREPL;
                cyls = (di_defaults[device].mdefects + nsecscyl - 1) / nsecscyl;
            }
	} else {
	    pt->pt_type = PTYPE_SECREPL;
	    cyls = 0;
	}

	pt->pt_nblks = cyls * nsecscyl;
	pt->pt_firstlbn = Vh.vh_pt[PTNUM_VOLHDR].pt_nblks;
	replend = pt->pt_nblks + pt->pt_firstlbn;

	/*
	 * initialize Unix file system partitions
	 */
	printf("The Unix file system partitions may be either ffs (BSD) or System V\n");
	printf("do you desire fast file system (BSD) partitions (n if no)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf == 'n')
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
			    /* last cyl group under 3 Meg? */
			    if (tmp < MIN_SIZE) {
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
	int i;
	register int j;

	printf("\npartition table manipulation\n");
	printf("choose one of (list, add, delete, quit, init, modify, replace)\n");
	while (1) {
		printf("command? ");
		if( fgets(linebuf, LINESIZE, stdin) == NULL )
		    continue;
		switch (*linebuf) {

		case 'i':
			pt_init();
			break;

		case 'l':
			pt_dump();
			break;

		case 'r':
			printf("\ttable entry number? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			pt = &Vh.vh_pt[i];
get_pttype0:
			printf("\tvalid partition types are:\n");
			for (j=0; j < NUM_PTYPES; j++) {
				printf("\t\t(%d) %s\n", j, part_types[j]);
			}
			printf("\tenter number for type? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_pttype0;
			if (i >= NUM_PTYPES) {
				printf("invalid partition type %d\n", i);
				goto get_pttype0;
			}
			pt->pt_type = i;
get_stcyl0:
			printf("\tstarting cylinder? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_stcyl0;
			if (i >= dp->dp_cyls) {
				printf("\tbad cylinder number (0 thru %d)\n",
					dp->dp_cyls-1);
				goto get_stcyl0;
			}
			pt->pt_firstlbn = i * dp->dp_secs * dp->dp_trks0;

get_numcyls:
			printf("\tnumber of cylinders? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_numcyls;
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
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_pttype;
			if (i >= NUM_PTYPES) {
				printf("invalid partition type %d\n", i);
				goto get_pttype;
			}
			pt->pt_type = i;

get_stcyl:
			printf("\tstarting cylinder? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_stcyl;
			if (i >= dp->dp_cyls) {
				printf("\tbad cylinder number (0 thru %d)\n",
					dp->dp_cyls-1);
				goto get_stcyl;
			}
			pt->pt_firstlbn = i * dp->dp_secs * dp->dp_trks0;

get_numcyls1:
			printf("\tnumber of cylinders? ");
			if( *linebuf == '\n' )
			    goto get_numcyls1;
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			pt->pt_nblks = i * dp->dp_secs * dp->dp_trks0;
			break;

		case 'd':
get_entnum:
			printf("\tentry number? ");
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if( *linebuf == '\n' )
			    goto get_entnum;
			if (i >= NPARTAB)
				printf("invalid partition entry %d\n", i);
			else
				Vh.vh_pt[i].pt_nblks = 0;
			break;

		case 'm':
			printf("default bootfile name? ");
			strncpy(Vh.vh_bootfile, gets(linebuf), BFNAMESIZE);
			printf("entry number of root (currently %d)? ", 
				Vh.vh_rootpt);
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
			if (i >= NPARTAB)
				printf("invalid partition entry %d\n", i);
			else
				Vh.vh_rootpt = i;
			printf("entry number of swap (currently %d)? ",
				Vh.vh_swappt);
			localatob(fgets(linebuf, LINESIZE, stdin), &i);
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
	int cyls;

	dp = &Vh.vh_dp;
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

/*
 * set device parameters
 */
scsidp_init()
{
    int readcap[2];
    register struct device_parameters *dp = &Vh.vh_dp;

    io_arg.memaddr = (unsigned long)readcap;
    io_arg.datasz = sizeof(readcap);
    if (ioctl(Fd,DIOCRDCAP,&io_arg) < 0) {
	pioerr("DIOCRDCAP");
	exit(1);
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
    dp->dp_aspz = geom.geom_aspz;		/* alt sectors per zone */
    dp->dp_atpv = geom.geom_atpv;		/* alt tracks per volume */
    if (Vh.vh_dp.dp_mspw == 0)
	Vh.vh_dp.dp_mspw = 260;
}

/*
 * get SCSI device specific parameters
 */
scsidp_get()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int tmp;

	/* 
	 * get device parameters 
	 */
	printf("enter device information: \n");

	while (1) {
	    printf("\tnumber of cylinders (currently %d): ", dp->dp_cyls);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_cyls = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber heads (currently %d): ", dp->dp_trks0);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_trks0 = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber sectors per track (currently %d): ", dp->dp_secs);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_secs = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\tnumber bytes per sector (currently %d): ", dp->dp_secbytes);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_secbytes = (u_short)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	printf("\tsector interleave (currently %d): ", dp->dp_interleave);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_interleave = (u_short)tmp;

	printf("\tmilliseconds per word (currently %d): ", dp->dp_mspw);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_mspw = (u_int)tmp;

	printf("\tdata rate (currently %d): ", dp->dp_datarate);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
	    dp->dp_datarate = (u_int)tmp;

	printf("\tnumber of retries (currently %d): ", dp->dp_nretries);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
	    dp->dp_nretries = (u_int)tmp;

	while (1) {
	    printf("\ttracks per zone (currently %d): ", dp->dp_tpz);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_tpz = (u_int)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\talternate sectors per zone (currently %d): ",
		dp->dp_aspz);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
	        dp->dp_aspz = (u_int)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}

	while (1) {
	    printf("\talternate tracks per volume (currently %d): ",
		dp->dp_atpv);
	    localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	    if (*linebuf == '\n') break;
	    if (tmp) {
		dp->dp_atpv = (u_int)tmp;
		break;
	    }
	    else printf("invalid value.\n");
	}
}


/*
 * dump device parameters 
 */
scsidp_dump()
{
	struct device_parameters *dp = &Vh.vh_dp;

	printf("\tnumber cylinders = %d\n", dp->dp_cyls);
	printf("\tnumber heads = %d\n", dp->dp_trks0);
	printf("\tnumber sectors per track = %d\n", dp->dp_secs);
	printf("\tnumber bytes per sector = %d\n", dp->dp_secbytes);
	printf("\tsector interleave = %d\n", dp->dp_interleave);
	printf("\tmilliseconds per word = %d\n", dp->dp_mspw);
	printf("\tdata rate = %d\n", dp->dp_datarate);
	printf("\tnumber of retries = %d\n", dp->dp_nretries);
	printf("\ttracks per zone = %d\n", dp->dp_tpz);
	printf("\talternate sectors per zone = %d\n", dp->dp_aspz);
	printf("\talternate tracks per volume = %d\n", dp->dp_atpv);
}

/*
 * setup list of known scsi bad blocks
 */
scsibb_input()
{
	u_int pba;
	int i;

	printf("\nSCSI defect list manipulation, when prompted\n");
	printf("choose one of (list, add, delete, quit)\n");

	while (1) {
		printf("command? ");
		if( fgets(linebuf, LINESIZE, stdin) == NULL )
		    continue;
		switch (*linebuf) {

		case 'l':
			scsibb_dump();
			break;

		case 'a':
			printf("enter 'logical block address' for each defect (q to quit)\n");
			for (i = 1; ; i++) {
			    printf("%d\t: ",  i);
		            if( fgets(linebuf, LINESIZE, stdin) == NULL )
				continue;
			    if (*linebuf == 'q')
				break;
			    if (*linebuf == '\n')
				continue;
			    if (check_pba(linebuf, &pba) == 0)
				   continue;
			    if (scsibb_insert(pba) == 0) {
				printf("\tmedia defect table full\n");
				break;
			    }
			}
			break;

		case 'd':
			printf("enter 'logical block address': ");
			if( fgets(linebuf,LINESIZE, stdin) == NULL || 
						*linebuf == '\n')
				break;
			if (check_pba(linebuf, &pba) == 0)
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
    int c;
    char sngl_partition[32];
    char *tmp;
    char slice[3];
    int whichfd;

    if (totalcyl == INVALID) {	/* haven't formatted */
	printf("\nformatting wasn't done, perform scan anyway (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
	    return;
    }
    pt = &Vh.vh_pt[volpart];
    sngl_fd = INVALID;
    whichfd = Fd;
    if (totalcyl == INVALID) {	/* haven't formatted */
	printf("scan entire disk (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y'){
getpart:
	    printf("entry number of partition to scan? ");
	    localatob(fgets(linebuf, LINESIZE, stdin), &i);
	    if ((i >= NPARTAB) || *linebuf == '\n') {
		printf("invalid partition entry %d\n", i);
		printf("valid range is (0 - %d)\n", NPARTAB);
		goto getpart;
	    }
	    strcpy( sngl_partition, devarray);
	    tmp = &sngl_partition[ strlen(sngl_partition) - 1];
	    while( isdigit(*tmp)) 
	        tmp--;
	    sprintf(slice, "%d", i);
	    strcpy( tmp+1, slice);
    
	    if( (sngl_fd = open( sngl_partition, O_RDWR)) < 0 ){
	       perror( sngl_partition );
	       goto getpart;
	    }
	    whichfd = sngl_fd;
	    pt = &Vh.vh_pt[i];
	}
    }
    startcyl = pt->pt_firstlbn;
    totalcyl = startcyl + pt->pt_nblks;
    wptr = (char *)PAGEALIGN(wbuf);
    memset(wptr, DEV_BSIZE * MAX_NSECS,0);
    nseccyl = dp->dp_secs * dp->dp_trks0;
    secbytes = dp->dp_secbytes;
    trkbytes = secbytes * dp->dp_secs;

    printf("\nscanning destroys disk data, perform scan (y if yes)? ");
    if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y'){
	    goto reterr;
    }

    printf("\nnumber of scans for bad blocks (3 are suggested)? ");
    localatob(fgets(linebuf, LINESIZE, stdin), &maxpass);
    if( *linebuf == '\n' )
	maxpass = 3;

    io_arg.sectst = 0x3;
    if (ioctl(whichfd, DIOCNOECC, &io_arg) < 0){
	if( io_arg.retval == DIOC_DISKBUSY){
	    pioerr("DIOCNOECC");
   	    exit(3);
	}
	printf("WARNING: couldn't disable ecc correction\n");
    }


    for (pass=0; pass < maxpass; pass++) {
	/* 
	 * init pattern for each sector and copy for each sector
	 */
	init_ptrn(wptr, (int)dp->dp_secbytes, pass % MAX_SCANPASS);
	for (i=secbytes; i < trkbytes; i += secbytes) {
	    memcpy(wptr+i, wptr, secbytes);
	}

	printf("\nscanning for defects, pass %d ", pass+1);

        if( atty ){
	    nblkflag = fcflag | O_NDELAY;
	    if(fcntl( 0, F_SETFL, nblkflag)){
	        perror("F_SETFL");
	        goto reterr;
	    }
	    if( ioctl( 0, TCSETA, &new_termio ) < 0 ){
	        perror("TCSETA");
	        goto reterr;
	    }
        }

	for (cc = startcyl; cc < totalcyl; cc += nseccyl ) {
	    /*
	     * Check for the escape key, in case the user wants to
	     * abort the scan phase
	     */
	    printf(".");
	    if( atty  ){
		if( (c = getchar()) == '' )
		    goto stopscan;
		}
	    /* 
	     * check one track at a time 
	     */
	    for (ch = 0; ch < dp->dp_trks0; ch++) {
		lbn = cc + (ch * dp->dp_secs);
		if ((lseek(Fd, (off_t)(lbn*DEV_BSIZE), 0) < 0) ||
		    (write(Fd, wptr, (unsigned)trkbytes) != trkbytes) ||
		    (lseek(Fd, (off_t)(lbn*DEV_BSIZE), 0) < 0) ||
		    (read(Fd, wptr, (unsigned)trkbytes) != trkbytes)) {

		    /* 
		     * find out which sector(s) is bad 
		     */
		    for (cs = 0; cs < dp->dp_secs; cs++, lbn++) {
			if ((lseek(Fd, (off_t)(lbn*DEV_BSIZE), 0) < 0) ||
			    (write(Fd,wptr,(unsigned)secbytes) != secbytes) ||
			    (lseek(Fd,(off_t)(lbn*DEV_BSIZE),0) < 0) ||
			    (read(Fd,wptr,(unsigned)secbytes) != secbytes)) {

			    if (scsibb_insert(lbn) == 0)
				printf("bad block list full\n");
			}
		    }
		}
	    }
	}
    }
stopscan:
    if( atty){
	if( fcntl( 0, F_SETFL, fcflag)){
	    perror("F_SETFL");
	    goto reterr;
	}
	if( ioctl( 0, TCSETA, &orig_termio ) < 0 ){
	    perror("TCSETA");
	    goto reterr;
	}
	nblkflag = 0;
    }
    printf("\n");
    io_arg.sectst = 0;
    if (ioctl(whichfd, DIOCNOECC, &io_arg) < 0)
	    printf("WARNING: couldn't reenable ecc correction\n");
reterr:
    if( sngl_fd != INVALID ){
        close(sngl_fd);
	sngl_fd = INVALID;
    }
}

/*
 * insert a SCSI bad block into list
 */
scsibb_insert(lbn)
int lbn;
{
    register int i;

    /* 
     * bad block may already be recorded 
     */
    for (i=0; i <= scsinbad; i++) {
	if (reassign.scsibb[i] == lbn) {
	    printf("block %d already on the defect list\n", lbn);
	    return (1);
	}
    }
    if (i >= MAX_SCSIBADBLOCKS) {
	return (0);
    }
    printf("block %d added to the defect list\n", lbn);
    /* 
     * grab new entry for bad block 
     */
    ++scsinbad;
    reassign.scsibb[scsinbad] = lbn;
    printf("block %d bad\n", lbn);
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
 * map scsi bad blocks
 */
scsibb_map()
{
	struct fmt_map_info fmi;
	
	/* if no defects just return */
	if (scsinbad == INVALID) return;

	fmi.fmi_action = FMI_MAP_TRACK;
	fmi.fmi_addr = (caddr_t) &reassign;
	fmi.fmi_intrlv = 4 + (4*(scsinbad+1)); /* total # of bytes to dma */
	reassign.length = (scsinbad + 1)*4; /* initialize reassign header */

	io_arg.memaddr = (unsigned long)&fmi;
	io_arg.datasz = (unsigned long)sizeof(fmi);
	if (ioctl(Fd, DIOCFMTMAP, &io_arg) < 0) {
	    pioerr("DIOCFMTMAP");
	    if( io_arg.retval == DIOC_DISKBUSY)
		exit(3);
	}
}

/*
 * dump list of SCSI bad blocks
 */
scsibb_dump()
{
	register int i;

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
		cp = localatob(cp, pba);
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
 * localatob -- convert ascii to binary.  Accepts all C numeric formats.
 */
static char *
localatob(cp, iptr)
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

	while (isdigit(*cp) ){
	        if((d = conv_digit(*cp)) >= base)
		    break;
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
 * conv_digit -- convert the ascii representation of a digit to its
 * binary representation
 */
static unsigned
conv_digit(c)
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

/*
 * read defect information off the drive
 */
bb_read()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct bb_entry *bbp;
	struct media_defect md;
	register struct defects *d;
	register int i;
	u_short cyl;
	u_char trk;
	u_char sec;
	int secbytes;
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

	printf("\nIf the drive is directly from the factory defects can be");
	printf("\nread from it ONLY ONCE before it is formatted.");
	printf("\nread factory defects from the drive (y if yes)? ");
	if( fgets(linebuf,LINESIZE, stdin) == NULL || *linebuf != 'y')
		goto rfile;


	/*
	 * read media defect information off each track of the drive
	 */
	for (cyl = 0; cyl < dp->dp_cyls; cyl++) {
	    md.md_cyl = cyl;
	    for (trk = 0; trk < dp->dp_trks0; trk++) {
		md.md_trk = trk;
	        io_arg.memaddr = (unsigned long)&md;
	        io_arg.datasz = (unsigned long)sizeof(md);
		if (ioctl(Fd, DIOCRDEFECTS, &io_arg) < 0) {
			printf("can't read defects on cyl=%d, trk=%d (%d)\n",
				cyl, trk, errno);
			continue;
		}
		if (md.md_entry.sync != SYNC_PATTERN) {
			printf("bb_read; bad sync pattern = 0x%x\n", 
				md.md_entry.sync);
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
rfile:
	printf("\nread defects from file (y if yes)? " );
	if( fgets(linebuf, LINESIZE, stdin) != NULL ){
	    if(*linebuf == 'y') {
		if (nbad >= 0) { /* we must have read defects from disk */
			printf("\npurge defects read from disk (y if yes)? " );
			if( fgets(linebuf,LINESIZE, stdin) != NULL ){
			    if (*linebuf == 'y') {
				for (i=0, bbp = bb; i <= nbad; i++, bbp++)
					bbp->cyl = bbp->trk = bbp->sec = 0;
				nbad = INVALID;
			    }
			
			}
		}
		bb_readfile(secbytes);
		return;
	    }
	}
}

bb_readfile(secbytes)
int secbytes;
{
	u_short cyl;
	u_char trk;
	u_char sec;
	int len;
	int pos;
	FILE *streamptr;
	int choice = -1;
	int last;
	char *tmp;

enteragain:
	printf("\nname of file? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf == '\n') {
		printf("invalid file name\n");
		if( atty )
		    goto enteragain;
		else
		    exit(1);
	}
	tmp = linebuf;
	while( *tmp != '\0' && *tmp != '\n')
	    tmp++;
	*tmp = '\0';
	
	if ((streamptr = fopen(linebuf, "r")) == NULL) {
		perror(linebuf);
		if( atty )
		    goto enteragain;
		else
		    exit(1);
	}
	while (1) {
		cyl = get_aint(streamptr, &last);
		if ((short)cyl == -1)
			break;
		trk = get_aint(streamptr, &last);
		if( choice == -1 ){
		    /* 
		     * First time through, check if c,h,s or c,h,p,l
		     */
		    pos = get_aint(streamptr, &last);
		    if( last ){
			/*
			 * c,h,s, so sector number is really what we
			 * thought might be the pos field.
			 */
			choice = 0;
			sec = pos;
		    }else{
			choice = 1;
		        len = get_aint(streamptr, &last);
		        printf("\t%d\t%d\t%d\t%d\n", cyl,trk,pos,len);

		        sec = pos / secbytes;
		    }
		}else if( choice == 1){
		    pos = get_aint(streamptr, &last);
		    len = get_aint(streamptr, &last);
		    printf("\t%d\t%d\t%d\t%d\n", cyl,trk,pos,len);

		    sec = pos / secbytes;
		}else
		    sec = get_aint(streamptr, &last);

		if (bb_insert(cyl, trk, sec) == 0) {
			printf("\tmedia defect table full\n");
			break;
		}
		if( choice ){
		    /* 
		     * If length is given, then check if it runs over
		     * a sector boundary.
		     */
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
}
bb_writefile()
{
	register struct bb_entry *bbp;
	FILE *streamptr;
	int i;
	char *tmp;

enteragain:
	printf("\nname of file? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf == '\n') {
		printf("invalid file name\n");
		if( atty )
		    goto enteragain;
		else
		    exit(1);
	}
	tmp = linebuf;
	while( *tmp != '\0' && *tmp != '\n')
	    tmp++;
	*tmp = '\0';
	
	if ((streamptr = fopen(linebuf, "w")) == NULL) {
		perror(linebuf);
		if( atty )
		    goto enteragain;
		else
		    exit(1);
	}

	for( i = 0, bbp = bb; i <= nbad; i++, bbp++)
	    fprintf( streamptr, "%d, %d, %d\n", bbp->cyl, bbp->trk, bbp->sec);

	fclose(streamptr);


}

get_aint(streamptr, last)
FILE *streamptr;
int *last;
{
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

	linebuf[0] = c;
	*last = 0;
	for (i=1; i < 32; i++) {
		if (fread(&linebuf[i], sizeof(char), 1, streamptr) != 1) {
			return (-1);
		}
		if ((linebuf[i] == ',') || (linebuf[i] == ')') || 
		     linebuf[i] == '\n' || (linebuf[i] == ' ')) {
			if( linebuf[i] == '\n')
			   *last = 1;
			linebuf[i] = '\0';
			break;
		}
	}
	return (atoi(linebuf));
}
smd_initvh()
{
        struct device_parameters *dp = &Vh.vh_dp;
        int i;
        register int *ip, csum;
        struct partition_table *ppt;
        struct dev_info *dptr;
	char *malloc();
	int modvh;

	modvh = 0;
	io_arg.memaddr = (unsigned long)&Vh;
	io_arg.datasz = (unsigned long)sizeof(Vh);
        if (ioctl(Fd, DIOCGETVH, &io_arg) < 0) {
	    Vh.vh_magic = INVALID;
	    pioerr("DIOCGETVH");
	    if( io_arg.retval == DIOC_DISKBUSY)
		exit(3);
        }

	/*
	 * Align buffer for reading in sector headers
	 */
	track_buf = malloc( (MAX_NSECS * sizeof(struct track_id)) + 512);
	track_id = (struct track_id *)(((int)track_buf+511 ) & ~511);

        if (is_vh(&Vh)) {

	    nsecscyl = dp->dp_secs * dp->dp_trks0;
	    if( nsecscyl == 0 ){
	        fprintf("Number of sectors per track can't be zero.\n");
	        exit(2);
	    }

            bst_readin();
            /*
             * Set attribute array
             */
            for (i = 0, dptr = dev_attribs; i < MAX_ATTRIBUTES;i++,dptr++) {
                if (dp->dp_flags & dptr->di_value)
                    attr_on[i] = 1;
                else
                    attr_on[i] = 0;
            }
            for (i=0; i < MAX_DEVICES; i++) {
                if (bcmp(&dp_defaults[i], dp,
                      sizeof(struct device_parameters)) == 0) {
                    device = i;
                    break;
                }
		/*
		 * Just compare key fields, runt, cyl, trks0, secs 
		 */
		if( (dp_defaults[i].dp_runt == dp->dp_runt) &&
		    (dp_defaults[i].dp_cyls == dp->dp_cyls) &&
		    (dp_defaults[i].dp_trks0 == dp->dp_trks0) &&
		    (dp_defaults[i].dp_secs == dp->dp_secs) &&
		    (dp_defaults[i].dp_secs == dp->dp_secs) ){
		    device = i;
		    break;
		}
            }
            if (i == MAX_DEVICES) {
                device = DEV_OTHER;
                printf("\ndevice parameters from disk don't match table entries!");
                printf("\nchoose new device parameters (y if yes)? ");
                if( fgets(buf2, LINESIZE, stdin) != NULL && *buf2 == 'y')
                    goto setit;
            } else {
                printf("\nchoose new drive parameters (y if yes)? ");
                if( fgets(buf2, LINESIZE,stdin) != NULL && *buf2 == 'y')
                    goto setit;
            }
            if (dp->dp_mspw == 0)
                dp->dp_mspw = dev_mspw[device];
	}else{
setit:
	    printf("No valid volume header found.\n");
            memset(&Vh, sizeof(struct volume_header),0);

            /*
             * initialize volume header with defaults
             */
            smddp_init();
	    nsecscyl = dp->dp_secs * dp->dp_trks0;
	    if( nsecscyl == 0 ){
	        fprintf("Number of sectors per track can't be zero.\n");
	        exit(2);
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
                vd_init();
                pt_init();
		modvh++;
        }

        if (dp->dp_secs > MAX_NSECS || dp->dp_secs == 0) {
            printf("\nFormat must be rebuilt with MAX_NSECS adjusted\n");
            printf("\tThis drive has %d sectors per track, and MAX_NSECS = %d\n", dp->dp_secs, MAX_NSECS);
            return(0);
        }


        if (di_defaults[device].mdefects > MAX_BADBLOCKS) {
            printf("\nFormat must be rebuilt with MAX_BADBLOCKS adjusted\n");
            printf("\tThis drive has %d max num defects, and MAX_BADBLOCKS = %d\n", di_defaults[device].mdefects, MAX_BADBLOCKS);
            return(0);
        }        

        /*
         * verify device parameters
         */
        while (1) {
            printf("\ndump device parameters (y if yes)? ");
            if( fgets(buf2, LINESIZE, stdin) != NULL && *buf2 == 'y')
                smddp_dump();
            printf("modify device parameters (y if yes)? ");
            if( fgets(buf2, LINESIZE, stdin) != NULL && *buf2 == 'y'){
                dp_get();
                dp_definfo();
		modvh++;
	    }else
     		break;

	}
        /*
         * disable any error recovery that could be performed
         * by the controller.  Save away volume header so that
	 * it can be restored.
         */
	Savedvh = Vh;
        dpreseek = dp->dp_flags & DP_RESEEK;
        dp->dp_flags &= ~DP_RESEEK;
        dpnretries = dp->dp_nretries;
        dp->dp_nretries = 0;

        /*
         * verify partition table information
         */
parttab:
        while (1) {
            printf("\ndump partition table (y if yes)? ");
            if( fgets(buf2, LINESIZE, stdin) != NULL && *buf2 == 'y')
                pt_dump();
            printf("modify partition table (y if yes)? ");
            if( fgets(buf2, LINESIZE, stdin) != NULL && *buf2 == 'y'){
                pt_modif();
		modvh++;
            }else
                break;
        }

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
        } else if (dp->dp_flags) {  /* XXX */
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

        /*
         * set volume header
         */
        Vh.vh_magic = VHMAGIC;
        Vh.vh_csum = 0;
        for (csum = 0, ip = (int *)&Vh; ip < (int *)(&Vh+1); ip++)
            csum += *ip;
        Vh.vh_csum = -csum;

	if( modvh ){
            io_arg.memaddr = (unsigned long)&Vh;
            io_arg.datasz = sizeof(Vh);
            if (ioctl(Fd, DIOCRECONFIG, &io_arg) < 0) {
	        pioerr("DIOCRECONFIG");
	        if( io_arg.retval == DIOC_DISKBUSY)
		    exit(3);
                return (0);
            }
	}
    return(1);
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
	int numentries;

	for (i=0, vd = Vh.vh_vd; i < NVDIR; i++, vd++) {
		if ((strncmp(vd->vd_name, BSTTAB_NAME, VDNAMESIZE) == 0) 
		    && (vd->vd_lbn > 0) && (vd->vd_nbytes > 0)) {
			break;
		}
	}
	if (i < NVDIR) {
		if (lseek(Fd, (off_t)(vd->vd_lbn * DEV_BSIZE), 0) < 0) {
			printf("bst_readin: seek error\n");
			return;
		}
		nbytes = ((vd->vd_nbytes+DEV_BSIZE-1) / DEV_BSIZE) * DEV_BSIZE;
		if (read(Fd, bsttab, (unsigned)nbytes) != nbytes) {
			printf("can't read bad sector table\n");
			return;
		}

		/*
		 * initialize bad block table from bad sector table
		 * on disk
		 */
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
	}
	printf("read in %d defects from 'on disk' bad sector table\n" ,nbad+1);
}

/*
 * print out bad sector table
 */
bst_dump()
{
	int i;
	struct bst_table *bst;

	printf("\nbad sector table:\n");
	for (i=0, bst = bsttab; i <= nbad; i++, bst++) {
		switch (bst->bt_rpltype) {
		case BSTTYPE_SLIPSEC:
			printf("\tbad block %d slipped\n", bst->bt_badlbn);
			break;

		case BSTTYPE_SECFWD:
			printf("\tbad block %d forwarded to %d\n",
				bst->bt_badlbn, bst->bt_rpllbn);
			break;

		case BSTTYPE_TRKFWD:
			printf("\tbad track containing %d forwarded to track starting at block %d\n",
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
 * insert a bad block into list
 */
bb_insert(cyl, trk, sec)
u_short cyl;
u_char trk;
u_char sec;
{
	register int i;
	register struct bb_entry *bbp;
	register struct partition_table *ppt;
	struct device_parameters *dp = &Vh.vh_dp;
	int begrepl,endrepl,begtrk;
	int trepl;

	/* 
	 * bad block may already be recorded 
	 */
	for (i=0, bbp = bb; i <= nbad; i++, bbp++) {
		if ((bbp->cyl == cyl)&&(bbp->trk == trk)&&(bbp->sec == sec)) {
			printf("block (%d,%d,%d) already on the defect list\n",
					cyl,trk,sec);
			return (1);
		}
	}

	/*
	 * If in the replacement partition, mark it as used.
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
	if( cyl >= begrepl && cyl <= endrepl ){
	    trepl = ((cyl * dp->dp_trks0) + trk) - begtrk;
	    mtrk[trepl] = BBMAPPED;
	}

	if (i >= MAX_BADBLOCKS) {
		return (0);
	}
	printf("block (%d,%d,%d) added to the defect list\n",
			cyl,trk,sec);
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
 * get device specific parameters
 */
dp_get()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i;
	char *cp;
	int tmp;
	struct dev_info *dptr;

	/* 
	 * get device parameters 
	 */
	printf("enter device information: \n");

	printf("\tspiral skew (currently %d): ", dp->dp_skew);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if( *linebuf != '\n')
	    dp->dp_skew = (u_char)tmp;
	printf("\tnumber words in gap1 (currently %d): ", dp->dp_gap1);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_gap1 = (u_char)tmp;
	printf("\tnumber words in gap2 (currently %d): ", dp->dp_gap2);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_gap2 = (u_char)tmp;
	printf("\tnumber of cylinders (currently %d): ", dp->dp_cyls);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_cyls = (u_short)tmp;
	printf("\tvol 0 starting head (currently %d): ", dp->dp_shd0);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_shd0 = (u_short)tmp;
	printf("\tnumber heads in vol 0 (currently %d): ", dp->dp_trks0);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_trks0 = (u_short)tmp;
	printf("\tvol 1 starting head (currently %d): ", dp->dp_shd1);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_shd1 = (u_short)tmp;
	printf("\tnumber heads in vol 1 (currently %d): ", dp->dp_trks1);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_trks1 = (u_short)tmp;
	printf("\tnumber sectors per track (currently %d): ", dp->dp_secs);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_secs = (u_short)tmp;
	printf("\tnumber bytes per sector (currently %d): ", dp->dp_secbytes);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_secbytes = (u_short)tmp;
	printf("\tsector interleave (currently %d): ", dp->dp_interleave);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_interleave = (u_short)tmp;
	printf("\tnumber retries on error (currently %d): ", dp->dp_nretries);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		dp->dp_nretries = (u_short)tmp;
	printf("\tmilliseconds per word (currently %d): ", dp->dp_mspw);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
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
		if( *cp != '\n' ){
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
 * get device defect information
 */
dp_definfo()
{
	struct device_info *di;
	int tmp;

	printf("enter device defect information:\n");
	di = &di_defaults[device];
	printf("\ttotal bytes per track (currently %d): ", di->trkbytes);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		di->trkbytes = tmp;
	printf("\tmaximum defect length in bits (currently %d): ", di->length);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		di->length = tmp;
	printf("\tmaximum num defective tracks (currently %d): ", di->mdeftrks);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		di->mdeftrks = tmp;
	printf("\tmaximum num defects (currently %d): ", di->mdefects);
	localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
	if (*linebuf != '\n')
		di->mdefects = tmp;
	if (di->realbps) {
		printf("\ttotal bytes per sector (currently %d): ", di->realbps);
		localatob(fgets(linebuf, LINESIZE, stdin), &tmp);
		if (*linebuf != '\n')
			di->realbps = tmp;
	}
}
smddp_init()
{
	int i;
getname:
        printf("device parameters are known for: \n");
        for (i=0; i < MAX_DEVICES; i++) {
	if( dp_defaults[i].dp_type == -1 )
	    break;
            printf("\t(%d) %s\n", i, dev_names[i]);
        }
        printf("enter number for one of the above? ");
	localatob(fgets(linebuf, LINESIZE, stdin), &i);
        if (i >= MAX_DEVICES || *linebuf == '\n') {
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

smd_format()
{
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct partition_table *pt;
	register int ccyl;
	register int chd;
	struct fmt_map_info fmi;
	int i;
	char sngl_partition[32];
	char slice[3];
	char *tmp;
	int whichfd;

	sngl_fd = INVALID;
	whichfd = Fd;
	printf("\nformatting destroys disk data, perform format (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
		return;
	printf("format entire disk (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) != NULL && *linebuf != 'y'){
getpart:
		printf("entry number of partition to format? ");

		/* 
		 * make sure that all characters are numbers.  We don't
		 * want people to type in garbage and have localatob return
		 * a zero, and partition zero will be formatted
		 */
		if( fgets(linebuf, LINESIZE, stdin) == NULL )
		    goto getpart;
		for( i=0; *(linebuf+i) != '\n';i++){
		    if( !isdigit( *(linebuf+i))){
			printf("invalid partition entry %s\n", linebuf);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		    }
		}

		localatob(linebuf, &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
		/*
		 * First try to open this partion and make sure that 
		 * it isn't in use.
		 */
		strcpy( sngl_partition, devarray);
		tmp = &sngl_partition[ strlen(sngl_partition) - 1];
		while( isdigit(*tmp)) 
		    tmp--;
		sprintf(slice, "%d", i);
		strcpy( tmp+1, slice);

		if( (sngl_fd = open( sngl_partition, O_RDWR)) < 0 ){
		   perror( sngl_partition );
		   goto getpart;
		}
		whichfd = sngl_fd;

		pt = &Vh.vh_pt[i];
		startcyl = pt->pt_firstlbn / nsecscyl;
		totalcyl = startcyl + (pt->pt_nblks / nsecscyl);
	} else {
	    startcyl = 0;
	    totalcyl = dp->dp_cyls;
	}

	printf("\nformatting ");
	fmi.fmi_action = FMI_FORMAT_TRACK;
	for (ccyl = startcyl; ccyl < totalcyl; ccyl++) {
		fmi.fmi_cyl = ccyl;
		printf(".");

		for (chd = 0; chd < dp->dp_trks0; chd++) {
			fmi.fmi_trk = chd;
retry:
			io_arg.memaddr = (unsigned long)&fmi;
			io_arg.datasz = (unsigned long)sizeof(fmi);
			if (ioctl(whichfd, DIOCFMTMAP, &io_arg) < 0) {
			    if( io_arg.retval == DIOC_DISKBUSY){
				pioerr("DIOCFMTMAP");
				exit(3);
			    }
				printf("format error(%d) on cyl %d head %d\n",
					errno, ccyl, chd);
				printf("retry format (y if yes)? ");
				if( fgets(linebuf, LINESIZE, stdin) == NULL || 
						linebuf[0] == 'y')
					goto retry;
			}
		}
	}
	printf("\n");
reterr:
	if( sngl_fd != INVALID )
	    close(sngl_fd);
}
/*
 * scan the disk for bad blocks 
 */
bb_scan()
{
	register struct partition_table *pt;
	struct device_parameters *dp = &Vh.vh_dp;
	int i,c;
	u_short  cc;
	u_char cs, ch;
	u_char insertcs;
	int lbn;
	int pass;
	int nseccyl;
	int trkbytes;
	int maxpass;
	DEFECT_ENTRY dentry;
        char sngl_partition[32];
        char *tmp;
        char slice[3];
	int whichfd;

	sngl_fd = INVALID;
	whichfd = Fd;
	if (totalcyl == INVALID) {	/* haven't formatted */
		printf("\nformatting wasn't done, perform scan anyway (y if yes)? ");
		if( gets(linebuf) == NULL || *linebuf != 'y')
			return;
	}
	if (totalcyl == INVALID) {	/* haven't formatted */
	   printf("scan entire disk (y if yes)? ");
	   if( fgets(linebuf, LINESIZE, stdin) != NULL && *linebuf != 'y') {
getpart:
		printf("entry number of partition to scan? ");
		if( fgets(linebuf,LINESIZE, stdin) == NULL || *linebuf == '\n')
		    goto getpart;
		for( i=0; *(linebuf+i) != '\n';i++){
		    if( !isdigit( *(linebuf+i))){
			printf("invalid partition entry %s\n", linebuf);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		    }
		}
		localatob(linebuf, &i);
		if ((i >= NPARTAB) || (i < 0)) {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
	        strcpy( sngl_partition, devarray);
	        tmp = &sngl_partition[ strlen(sngl_partition) - 1];
	        while( isdigit(*tmp)) 
	            tmp--;
	        sprintf(slice, "%d", i);
	        strcpy( tmp+1, slice);
    
	        if( (sngl_fd = open( sngl_partition, O_RDWR)) < 0 ){
	           perror( sngl_partition );
	           goto getpart;
	        }
		whichfd = sngl_fd;
		pt = &Vh.vh_pt[i];
		nseccyl = dp->dp_secs * dp->dp_trks0;
		startcyl = pt->pt_firstlbn / nseccyl;
		totalcyl = startcyl + (pt->pt_nblks / nseccyl);
	   } else {
		startcyl = 0;
		totalcyl = dp->dp_cyls;
	   }
	}
        wptr = (char *)PAGEALIGN(wbuf);
#ifndef SABLE
	memset(wptr, DEV_BSIZE * MAX_NSECS,0);
#endif SABLE
	nseccyl = dp->dp_trks0 * dp->dp_secs;
	trkbytes = dp->dp_secbytes * dp->dp_secs;

	printf("\nscanning destroys disk data, perform scan (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
		goto reterr;

	printf("\nnumber of scans for bad blocks (3 are suggested)? ");
	localatob(fgets(linebuf, LINESIZE, stdin), &maxpass);
	if( *linebuf == '\n' )
	    maxpass = 3;

	io_arg.sectst = 0x3;
	if (ioctl(whichfd, DIOCNOECC, &io_arg) < 0){
	    if( io_arg.retval == DIOC_DISKBUSY){
		pioerr("DIOCNOECC");
		exit(3);
	    }
	    printf("WARNING: couldn't disable ecc correction\n");
	}

	if( atty ){
	    nblkflag = fcflag | O_NDELAY;
	    if(fcntl( 0, F_SETFL, nblkflag)){
		perror("F_SETFL");
		goto reterr;
	    }
	    if( ioctl( 0, TCSETA, &new_termio ) < 0 ){
	        perror("TCSETA");
		goto reterr;
	    }
	}

	for (pass=0; pass < maxpass; pass++) {

		/* 
		 * init pattern for each sector and copy for each sector
		 */
		init_ptrn(wptr, dp->dp_secbytes, pass % MAX_SCANPASS);
		for (i=dp->dp_secbytes; i < trkbytes; i += dp->dp_secbytes) {
			memcpy(wptr+i, wptr, (int)dp->dp_secbytes);
		}

		printf("\nscanning for defects, pass %d (hit ESCAPE to abort)",
							pass+1);
		for (cc = startcyl; cc < totalcyl; cc++) {
		    printf(".");
		    /*
		     * Check for the escape key incase the user wants to quit
		     * (Need non-blocking i/o to do this)
		     */
		    if( atty){
		        if( (c = getchar()) == '' )
			    goto stopscan;
		    }

		    for (ch = 0; ch < dp->dp_trks0; ch++) {
			lbn = (cc * nseccyl) + (ch * dp->dp_secs);
			io_arg.sectst = lbn;
			io_arg.datasz = dp->dp_secs;
			/* 
			 * check one track at a time 
			 */
			if ((lseek(Fd, (off_t)(lbn*DEV_BSIZE), 0) < 0) ||
			    (write(Fd, wptr, (unsigned)trkbytes) != trkbytes) ||
			    (lseek(Fd, (off_t)(lbn*DEV_BSIZE), 0) < 0) ||
			    (ioctl(Fd, DIOCVFYSEC, &io_arg) < 0)) {

				printf("\nError on cyl %d, trk %d\n", cc, ch);

				/* 
				 * find out which sector(s) is bad 
				 */
				for (cs = 0; cs < dp->dp_secs; cs++, lbn++) {
			            io_arg.sectst = lbn + cs;
			            io_arg.datasz = 1;
				    if((lseek(Fd,(off_t)(lbn*DEV_BSIZE),0)<0) ||
				      (write(Fd,wptr,(unsigned)dp->dp_secbytes) 
					    != dp->dp_secbytes) ||
				       (lseek(Fd,(off_t)(lbn*DEV_BSIZE),0)<0) ||
				       (ioctl(Fd, DIOCVFYSEC, &io_arg) < 0)) {
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
				        if( (int)(insertcs = map_sec( LOG_TO_PHYS, cc, ch, cs )) < 0){
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
					    insertcs =
						((insertcs == 0 ) ? 
						  insertcs - 1 : 1);
				        }
				    }else
					insertcs = cs;
				    if (bb_insert(cc, ch, insertcs) == 0)
					printf("bad block list full\n");
				}
			}
		    }
		}
	    }
	}
stopscan:
	if( atty){
	    if( fcntl( 0, F_SETFL, fcflag)){
		perror("F_SETFL");
		goto reterr;
	    }
	    if( ioctl( 0, TCSETA, &orig_termio ) < 0 ){
	        perror("TCSETA");
	    }
	    nblkflag = 0;
	}
	printf("\n");
	io_arg.sectst = 0;
	if (ioctl(whichfd, DIOCNOECC, &io_arg) < 0)
		printf("WARNING: couldn't reenable ecc correction\n");
reterr:
	if( sngl_fd != INVALID ){
	    close(sngl_fd);
	}
}

map_sec( whichway, cyl, head, sector )
int whichway;
u_short cyl;
u_char head;
u_char sector;
{

    int newsec;
    int i, slipi;
    struct device_parameters *dp = &Vh.vh_dp;
    struct fmt_map_info fmi;
    int tried = 0;

    if( sector > MAX_NSECS )
	printf("sector out of range c/h/s : (%d/%d/%d)\n", cyl,head,sector);
again:
    io_arg.memaddr = (unsigned long)track_id;
    io_arg.datasz = (unsigned long)(sizeof(struct track_id ) * MAX_NSECS);
    memset(track_id, sizeof(struct track_id) * MAX_NSECS, 0);
    track_id[0].cylno = cyl;
    track_id[0].head = head;
    if (ioctl(Fd,DIOCTRKID, &io_arg) < 0) {
	pioerr("DIOCTRKID");
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
	    io_arg.memaddr = (unsigned long)&fmi;
	    io_arg.datasz = (unsigned long)sizeof(fmi);
retry:
  	    if (ioctl(Fd, DIOCFMTMAP, &io_arg) < 0) {
		printf("format error(%d) on cyl %d head %d.  Retry [n] ?\n",
					errno, fmi.fmi_cyl, fmi.fmi_trk);
		if( fgets(linebuf, LINESIZE, stdin) == NULL || linebuf[0] == 'y')
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
/*
 * setup list of known bad blocks
 */
bb_input()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i, choice;
	u_short cyl;
	u_char hd;
	u_char sec;
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
		if( fgets(linebuf, LINESIZE, stdin) == NULL )
		    continue;
		switch (*linebuf) {

		case 'l':
			bb_dump();
			break;

		case 'a':
			printf("'cyl,head,pos,len' or 'cyl,head,sector' defect entry format can be used\n");
			printf("use 'cyl,head,pos,len' format (y if yes)? ");
			if( fgets(linebuf, LINESIZE, stdin) != NULL && 
				*linebuf == 'y') {
			    choice = 1;
			    printf("enter 'cyl,head,pos,len' for each defect (q to quit)\n");
			}else
			    printf("enter 'cyl,head,sector' for each defect (q to quit)\n");
			for (i = 1; ; i++) {
			    printf("%d\t: ",  i);
			    if( fgets(linebuf, LINESIZE, stdin) == NULL || 
					*linebuf == 'q')
				break;
			    if (*linebuf == '\n')
				continue;
			    if (choice) {
			       if (check_pbn(linebuf, &cyl, &hd, &pos, &len) == 0)
				   continue;
			       sec = pos / secbytes;
			    } else {
			       if (check_pbn1(linebuf, &cyl, &hd, &sec) == 0)
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
			if( fgets(linebuf, LINESIZE, stdin) == NULL || 
					*linebuf == 'y'){
			    choice = 1;
			    printf("enter 'cyl,head,pos,len': ");
			} else
				printf("enter 'cyl,head,sector': ");
			if( fgets(linebuf, LINESIZE, stdin) == NULL || 
					*linebuf == '\n')
				break;
			if (choice) {
				if (check_pbn(linebuf, &cyl, &hd, &pos, &len) == 0)
					break;
				sec = pos / secbytes;
			} else {
				if (check_pbn1(linebuf, &cyl, &hd, &sec) == 0)
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
 * parse and check physical block number string 
 */
check_pbn(str, cylp, hdp, posp, lenp)
char *str;
u_short *cylp;
u_char *hdp;
int *posp, *lenp;
{
	char *cp;
	struct device_parameters *dp = &Vh.vh_dp;
	int tmp;

	*cylp = *hdp = *posp = *lenp = 0;
	cp = str;
	if (*cp == '(')
		cp++;

	if (*cp != ')') {
		while (isspace(*cp))
			cp++;
		cp = localatob(cp, &tmp);
		*cylp = (u_short)tmp;
		if (*cp == ',') {
			cp++;
			while (isspace(*cp))
				cp++;
			cp = localatob(cp, &tmp);
			*hdp = (u_char)tmp;
			if (*cp == ',') {
				cp++;
				while (isspace(*cp))
					cp++;
				cp = localatob(cp, posp);
				if (*cp == ',') {
					cp++;
					while (isspace(*cp))
						cp++;
					cp = localatob(cp, lenp);
				}
			}
		}
	}

	/* 
	 * check for valid physical bad block number 
	 */
	if (((int)*cylp < 0) || (*cylp >= dp->dp_cyls)) {
		printf("invalid cylinder %d, range is 0 thru %d\n", 
		    *cylp, dp->dp_cyls-1);
		return (0);
	}
	if (((int)*hdp < 0) || (*hdp >= dp->dp_trks0)) {
		printf("invalid head %d, range is 0 thru %d\n", 
		    *hdp, dp->dp_trks0-1);
		return (0);
	}
	if (((int)*posp < 0) || (*posp >= di_defaults[device].trkbytes)) {
		printf("invalid position %d, range is 0 thru %d\n",
		    *posp, di_defaults[device].trkbytes-1);
		return (0);
	}
	if (((int)*lenp <= 0) || (*lenp > di_defaults[device].length)) {
		printf("invalid length %d, range is 1 thru %d\n", 
		    *lenp, di_defaults[device].length);
		return (0);
	}
	return (1);
}
/*
 * parse and check physical block number string 
 */
check_pbn1(str, cylp, hdp, secp)
char *str;
u_short *cylp; 
u_char *hdp, *secp;
{
	char *cp;
	struct device_parameters *dp = &Vh.vh_dp;
	int iptr;

	*cylp = *hdp = *secp = 0;
	cp = str;
	if (*cp == '(')
		cp++;

	if (*cp != ')') {
		while (isspace(*cp))
			cp++;
		cp = localatob(cp, &iptr);
		*cylp = (short)iptr;
		if (*cp == ',') {
			cp++;
			while (isspace(*cp))
				cp++;
			cp = localatob(cp, &iptr);
			*hdp = (char)iptr;
			if (*cp == ',') {
				cp++;
				while (isspace(*cp))
					cp++;
				cp = localatob(cp, &iptr);
				*secp = (char)iptr;
			}
		}
	}

	/* 
	 * check for valid physical bad block number 
	 */
	if (((int)*cylp < 0) || (*cylp >= dp->dp_cyls)) {
		printf("invalid cylinder %d, range is 0 thru %d\n", 
		    *cylp, dp->dp_cyls-1);
		return (0);
	}
	if (((int)*hdp < 0) || (*hdp >= dp->dp_trks0)) {
		printf("invalid head %d, range is 0 thru %d\n", 
		    *hdp, dp->dp_trks0-1);
		return (0);
	}
	/*
	 * Have to allow it to be equal to secs, since the bad sector
	 * could be the slipped sector 
	 */
	if (((int)*secp < 0) || (*secp > dp->dp_secs)) {
		printf("invalid sector %d, range is 0 thru %d\n",
		    *secp, dp->dp_secs);
		return (0);
	}
	return (1);
}

bb_dump()
{
	register int i;
	register unsigned int lbn;
	register struct device_parameters *dp = &Vh.vh_dp;
	register struct bb_entry *bbp;
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
	struct fmt_map_info fmi;
	int startentry;
	int lastentry;
	int begrepl,endrepl;
	int begtrk, trepl;
	struct partition_table *ppt;
	int trkindx, tindx, bbindx;
        char sngl_partition[32];
        char *tmp;
        int whichfd;
        char slice[3];
	
	sngl_fd = INVALID;
	whichfd = Fd;
	if (totalcyl == INVALID) {	/* haven't formatted */
		printf("\nformat or scan wasn't done, perform mapping anyway (y if yes)? ");
		if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
			return;
	}
	if (totalcyl == INVALID) {	/* haven't formatted */
	   printf("map entire disk (y if yes)? ");
	   if( fgets(linebuf, LINESIZE, stdin) != NULL && *linebuf != 'y') {
getpart:
		printf("entry number of partition to map? ");
		localatob(fgets(linebuf, LINESIZE, stdin), &i);
		if ((i >= NPARTAB) || *linebuf == '\n') {
			printf("invalid partition entry %d\n", i);
			printf("valid range is (0 - %d)\n", NPARTAB);
			goto getpart;
		}
	        strcpy( sngl_partition, devarray);
	        tmp = &sngl_partition[ strlen(sngl_partition) - 1];
	        while( isdigit(*tmp)) 
	            tmp--;
	        sprintf(slice, "%d", i);
	        strcpy( tmp+1, slice);
    
	        if( (sngl_fd = open( sngl_partition, O_RDWR)) < 0 ){
	           perror( sngl_partition );
	           goto getpart;
	        }
		whichfd = sngl_fd;

		pt = &Vh.vh_pt[i];
		startcyl = pt->pt_firstlbn / nsecscyl;
		totalcyl = startcyl + (pt->pt_nblks / nsecscyl);
	   } else {
		startcyl = 0;
		totalcyl = dp->dp_cyls;
	   }
	}
	printf("\nmapping destroys disk data, perform map (y if yes)? ");
	if( fgets(linebuf, LINESIZE, stdin) == NULL || *linebuf != 'y')
		goto reterr;

	if ((dp->dp_flags & (DP_SECTSLIP|DP_TRKFWD)) == 0) {
		printf("No support for track forwarding or sector slipping\n");
		goto reterr;
	}

	/* 
	 * get bad blocks within a track together 
	 */
	qsort(bb, (unsigned)(nbad+1), sizeof(struct bb_entry), qcompar);

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
	for (i=startentry; i <= lastentry; i++ ) {
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
			    curbb = nxtbb;
		    	    goto trkfwd;
			}
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
			    fmi.fmi_sec = map_sec( PHYS_TO_LOG, curbb->cyl, 
				curbb->trk, curbb->sec);
			    if( fmi.fmi_sec == SLIP_PTRN || 
				fmi.fmi_sec == dp->dp_secs){
			        /*
				 * The slip sector is bad.  This can
				 * occur if this sector has already
				 * been mapped, or if it's the last sector
				 */
			        curbb->rpltype = BSTTYPE_SLIPBAD;
				continue;
			    }else if( fmi.fmi_sec > MAX_NSECS ){
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
			    io_arg.memaddr = (unsigned long )&fmi;
			    io_arg.datasz = sizeof(fmi);
			    fmi.fmi_action = FMI_SLIP_SECTOR;
			    fmi.fmi_cyl = curbb->cyl;
			    fmi.fmi_trk = curbb->trk;
			    /*
			     * Already set up above.
			     */
			    if( !dp->dp_skew )
			        fmi.fmi_sec = curbb->sec;

			    if (ioctl(whichfd, DIOCFMTMAP, &io_arg) < 0) {
				if( io_arg.retval == DIOC_DISKBUSY){
				    pioerr("DIOCFMTMAP");
				    exit(3);
				}
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
                        printf("format: no more replacement tracks.\n");
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
	    goto reterr;
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
		    goto reterr;
		}
		tindx++;
	    }


	    io_arg.memaddr = (unsigned long )&fmi;
	    io_arg.datasz = sizeof(fmi);
	    fmi.fmi_action = FMI_MAP_TRACK;
	    fmi.fmi_cyl = btrk[i].cyl;
	    fmi.fmi_trk = btrk[i].hd;
	    fmi.fmi_rplcyl = begrepl + ( tindx / dp->dp_trks0);
	    fmi.fmi_rpltrk = (begtrk +  tindx) % dp->dp_trks0;
	    if (ioctl(whichfd, DIOCFMTMAP, &io_arg) < 0) {
		if( io_arg.retval == DIOC_DISKBUSY ){
		    pioerr("DIOCFMTMAP");
		    exit(3);
		}
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
		bb[bbindx].rpllbn = (begtrk + tindx) * dp->dp_secs;
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
reterr:
    if( sngl_fd != INVALID )
       	close(sngl_fd);
}

/*
 * write volume header and bad sector table
 */
smd_wrapup( caught)
int caught;
{
	int i;
	struct device_parameters *dp = &Vh.vh_dp;
	int *ip;
	int csum;
	struct volume_directory *vd;
	unsigned nbytes;
	int failed;

tryagain:
	fflush(stdin);
	if( caught )
	    printf("\nwrite new volume header? [y or n] ");
	else
	    printf("\nwrite new volume header? (n if no) ");
	if( fgets(linebuf, LINESIZE, stdin) != NULL && *linebuf == 'n') {
		close(Fd);
		return;
	}
	if( caught && *linebuf != 'y' )
	    goto tryagain;

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
		failed = 0;
		if (lseek(Fd, (off_t)(vd->vd_lbn * DEV_BSIZE), 0) < 0) {
			printf("seek failed, ");
			failed = 1;
		} else if (write(Fd, bsttab, (unsigned)nbytes) != nbytes) {
			printf("write failed, ");
			failed = 1;
		}
		if (failed) {
			printf("retry? ");
			if( fgets(linebuf, LINESIZE, stdin) != NULL && 
					*linebuf == 'y')
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

	/* 
	 * write volume header in the 1st sector of each track of cyl 0 
	 */
	printf("\nwriting volume header... ");
	for (i=0; i < dp->dp_trks0; i++) {
	    if (lseek(Fd, (off_t)(i * dp->dp_secs * DEV_BSIZE), 0) < 0) {
		printf("lseek for volhdr failed\n");
		continue;
	    }
	    if (write(Fd, &Vh, DEV_BSIZE) != DEV_BSIZE) {
		printf("write of volhdr at trk %d failed\n", i);
	    }
	}
	printf("\n");

	io_arg.memaddr = (unsigned long)&Vh;
	io_arg.datasz = (unsigned long)sizeof(Vh);
	if (ioctl(Fd, DIOCSETVH, &io_arg) < 0) {
	    pioerr("DIOCSETVH");
	    return;
	}
	close(Fd);
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
 * comparison routine used by qsort()
 */
static
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

/*
 * dump SMD device parameters 
 */
smddp_dump()
{
	struct device_parameters *dp = &Vh.vh_dp;
	int i;
	struct dev_info *dptr;
	struct device_info *iptr;

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
}
catchit()
{
    int tmpfd;

	if( formatting){
	    fprintf( stderr, "\nSIGINT encountered.  Will abort after format\n");
	    return;
	} else
	    fprintf( stderr, "\nSIGINT encountered.  Cleaning up.\n");

	if( nblkflag && fcflag && atty){
	    if(fcntl( 0, F_SETFL, fcflag) < 0 ){
		perror("F_SETFL");
		exit(1);
	    }
	    if( ioctl( 0, TCSETA, &orig_termio ) < 0 ){
	        perror("TCSETA");
		exit(1);
	    }
	}

	/*
	 * See if we've successully opened the volume
	 */
	if( Fd == INVALID )
	    exit(1);

	tmpfd = Fd;
	    
	if( is_vh(&Savedvh)){
	    io_arg.memaddr = (unsigned long)&Savedvh;
	    io_arg.datasz = (unsigned long)sizeof(Savedvh);
	    if( sngl_fd != INVALID )
		tmpfd = sngl_fd;
	    if (ioctl(tmpfd, DIOCSETVH, &io_arg) < 0) {
	        pioerr("DIOCSETVH");
	        exit(1);
	    }
	}
	fflush(stdin);
	if ( ct.ci_flags & DP_SMD )
	    smd_wrapup(1);
	else
	    scsi_wrapup(1);

	exit(1);
}

pioerr( op)
char *op;
{
    char tbuf[120];

    strcpy( tbuf, op );
    switch (io_arg.retval) {
	case DIOC_BADSIZE:
	    strcat( tbuf, " Size of structure passed to ioctl is incorrect");
	    break;
	case DIOC_NOTVOLHDR:
	    strcat( tbuf, " Not a valid volume header");
	    break;
	case DIOC_DISKBUSY:
	    strcat( tbuf, " Disk is busy");
	    break;
	case DIOC_EFAULT:
	    strcat( tbuf, " Memory fault");
	    break;
	case DIOC_OPERR:
	    strcat( tbuf, " Error during I/O operation");
	    break;
	case DIOC_EINVAL:
	    strcat( tbuf, " Illegal Parameter");
	    break;
	}
    perror(tbuf);
}
