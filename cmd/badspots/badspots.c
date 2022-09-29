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
 * badspots.c - routines for mapping out bad spots on disks.
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

#define CYLS_PER_GROUP	16
/* 
 * default settings for file system independent partitions 
 */
#define PTNUM_VOLHDR		8		/* volume header partition */
#define PTNUM_REPL		9		/* trk/sec replacement part */
#define PTNUM_VOLUME		10		/* entire volume partition */
#define PTSIZE_VHDR		1000000		/* 1 Megabyte for volhdr */

/*
 * misc defines 
 */
#define MAX_BADBLOCKS	4000
#define MAX_NSECS	84	/* Must include the slip sector 	*/
#define MAX_MTRKS	84	/* This is the maximum number of tracks
				 * in the TRKREPL partition.  There are
				 * 4 cylinders for the 2392, so that means
				 * that the max is 84.  This will change
				 * with larger drives 
				 */
#define MAX_SCSIBADBLOCKS	1024	/* what should this be? */
#define INVALID	 	-1	
#define CYL( blkno, dp ) ((blkno) / (dp->dp_trks0 * dp->dp_secs))
#define HEAD( blkno, dp) ( ((blkno) / dp->dp_secs) % dp->dp_trks0)
#define SECTOR( blkno, dp) ( (blkno) % dp->dp_secs)
#define TRACK( blkno, dp ) ( (CYL(blkno,dp) * dp->dp_trks0) + HEAD(blkno,dp))
#define SLIPIT 0
#define MAPIT 1
#define PAGEALIGN(ptr) ((int)((ptr) + (NBPP-1)) & ~(NBPP-1))

/*
 * mtrk - contains an entry for every track in the replacment partition.
 * During formatting, it is set to one of the following 3 values.
 * btrk - a list of track numbers of bad tracks to map.
 */
char mtrk[MAX_MTRKS];			
#define NOTMAPPED	0
#define MAPPED		1
#define BBMAPPED	2
#define NOTMAPPEDW_SLIP 3
struct btrk {
    u_short	cyl;
    u_char	hd;
} btrk[MAX_MTRKS];
int formatting;

extern off_t lseek();
extern void perror();
extern void exit();
extern void qsort();
extern char *malloc();

FILE *fd;			/* fd of file specified with -c flag */
char *scanfile;			/* name of file specified with -c flag */
int maxretries;			/* # of retries with -m flag (or 5) */
int range;			/* range to scan with -r flag (or 16) */
int npasses;			/* # of passes to scan	*/
int blocks[MAX_BADBLOCKS];	/* List of bad blocks (used for input) */
int nblocks;			/* Number of entries in blocks */
struct {
    short cyl;
    char hd, sec;
}chs[MAX_BADBLOCKS];		/* List of bad blocks (used to input c/h/s ) */
int nchs;			/* Number of entries in chs */
int slashes;			/* Set if chs is used instead of blocks */
int base;			/* Base of number */
char buffer[1024];		/* Input buffer	*/
struct volume_header vh;	/* Volume header */
struct io_arg io_arg;		/* Structure to pass to ioctl	*/
struct ctlr_info ct;		/* Controller information	*/
int devfd;			/* fd of entire volume partition */
char devarray[32];		/* name of entire volume partition */
int maxmtrks;			/* Max mapped tracks for this device */
int nbps;			/* Number of bytes per sector */
int ftrk;			/* First track of the replacement partition */
int catchit();


/*
 * SCSI defect list
 */
char deftbl[sizeof(struct defect_header) + (MAX_BADBLOCKS * 
	sizeof(struct defect_entry)) + NBPP + 1];
struct defectlist {
    struct defect_header header;
    struct defect_entry entry[MAX_SCSIBADBLOCKS];
} *defptr;

struct reassign {
    char pad;
    char pad1;
    u_short length;
    u_int blocks[MAX_SCSIBADBLOCKS];
} reassign;
/*
 * Buffer in which to read entire track.  To make this faster,
 * since we are doing raw i/o.  Make sure that it's page aligned.
 */
char trkbuf[MAX_NSECS * DEV_BSIZE + NBPP + 1];
char *trkptr;
struct track_id track_id[MAX_NSECS];

/*
 * Defect table.
 */
char bstbuf[ (MAX_BADBLOCKS * sizeof(struct bst_table)) + NBPP + 1];
struct bst_table *bsttable;
int bstfound = 0;		/* Set if bsttab was read in */
int nbst;


struct {
    char *type;
    char *key;
} keydev[] = {
    "DKVJ", "physical block address",
    "SCSI", "physical block address",
    "ipc", "chs=",
    0,0
};

#define DKVJ	 0
#define SCSI	 1
#define DKIP	 2
#define NENTRIES 3
int autoflg = 0;	/* Set if -a flag was specified	*/
int intflg = 0;		/* Set if -i flag was specified */
int specflg = 0;	/* Set if -s flag was specified */
int errflg = 0;		/* Set if -e flag was specified */
int blkflg = 0;		/* Set if -b flag was specified */
int logflg = 0;		/* Set if -c flag was specified */
int volflg = 0;		/* Set if -v flag was specified */
int listflg = 0;	/* Set if -l flag was specified */
int passflg = 0;	/* Set if -p flag was specified */
int dispflg = 0;	/* Set if -d flag was specified */
int retryflg = 0;	/* Set of -m flag was specified */


/*
 * main routine
 */
main(argc,argv)
int argc;
char *argv[];
{
    extern char *optarg;
    extern int optind;
    int c;
    int i;
    char *opttmp, *optbeg;
    int opterrflg = 0;		/* Set if error while parsing options */


    scanfile = NULL;
    fd = NULL;
    maxretries = 5;
    range = 16;
    npasses = 3;
    trkptr = (char *)PAGEALIGN(trkbuf);
    bsttable = (struct bst_table *)PAGEALIGN(bstbuf);
    defptr = (struct defectlist *)PAGEALIGN(deftbl);
    slashes = 0;

    if( argc == 1 )
	intflg++;
    else{
        while((c = getopt( argc, argv, "asilc:deb:m:r:v:p:")) != -1 ){
	    switch(c){
	        case 'a':
		    autoflg++;
		    break;
	        case 'i':
		    intflg++;
		    break;
	        case 's':
		    specflg++;
		    break;
		case 'l':
		    listflg++;
		    break;
		case 'c':
		    logflg++;
		    if( !autoflg ){
		        usage();
		        exit(2);
		    } else{
		        scanfile = optarg;
		        break;
		    }
		case 'd':
		    if( intflg || listflg ){
			usage();
			exit(2);
		    }else{
			dispflg++;
			break;
		    }
		case 'e':
		    if( intflg ){
			usage();
			exit(2);
		    }else{
			errflg++;
			break;
		    }
		case 'b':
		    blkflg++;
		    if( specflg ){
			/* 
			 * list of blocks (or just one) separated by commas
			 * they can also be of the form c/h/s, so check for
			 * '/'s
			 */
			opttmp = optarg;
			slashes = 0;
			while (*opttmp != '\0'){
			    if( *opttmp == '/' ){
				slashes = 1;
				break;
			    }
			    opttmp++;
			}
			opttmp = optarg;

			if( slashes ){
			    for( i = 0; i < MAX_BADBLOCKS; i++){
				chs[i].cyl = (short)strtol(opttmp, &opttmp, 10);
				if( *opttmp++ != '/'){
				    fprintf(stderr, "c/h/s format incorrect\n");
				    usage();
				    exit(2);
				}
				chs[i].hd = (char)strtol(opttmp, &opttmp, 10);
				if( *opttmp++ != '/'){
				    fprintf(stderr, "c/h/s format incorrect\n");
				    usage();
				    exit(2);
				}
				chs[i].sec = (char)strtol(opttmp, &opttmp, 10);
				if( *opttmp == ',' )
				    continue;
				else
				    break;
			    }
			    nchs = i+1;
			}else{
			    for( i = 0; i < MAX_BADBLOCKS; i++){
			        optbeg = opttmp;
			        base = 10;
			        if( *opttmp == '0' ){
			            ++opttmp;
			            if( *opttmp == 'x' || *opttmp == 'X'){
				        base = 16;
				        opttmp++;
			            }else
				        base = 8;
			        }
			        while( isdigit(*opttmp))
			            opttmp++;
			        if( *opttmp != ',' ){
			            if( *opttmp == '\0'){
			                blocks[i] = strtol( optbeg, NULL, base);
				        break;
				    }else{
			                usage();
			                exit(2);
				    }
			        }else{
			            blocks[i] = strtol( optbeg, NULL, base);
				    opttmp++;
			        }
			    }
			}
			nblocks = i+1;
		    }else{
		       usage();
		       exit(2);
		    }
		    break;
		case 'm':
		    retryflg++;
		    if( intflg ){
			usage();
			exit(2);
		    } else{
			maxretries = atoi(optarg);
			break;
		    }
		case 'r':
		    if( intflg ){
			usage();
			exit(2);
		    } else{
			range = atoi(optarg);
			break;
		    }
		case 'v':
		    volflg++;
		    if( specflg || listflg){
			strncpy( devarray, optarg, 32);
			break;
		    }else{
			usage();
			exit(2);
		    }
		case 'p':
		    passflg++;
		    if( !autoflg ){
			usage();
			exit(2);
		    } else{
			npasses = atoi(optarg);
			break;
		    }
	        case '?':
		    opterrflg++;
		    break;
	    }
	}
    }
    if( opterrflg){
        usage();
	exit(2);
    }

    /*
     * Do some more flag checking
     */
    if( intflg ){
	if( autoflg || specflg || errflg || blkflg || logflg || volflg || listflg){ 
	    usage();
	    exit(2);
	}
    }else if( specflg ){
	if( autoflg || logflg || listflg){
	    usage();
	    exit(2);
	}
	if( volflg ){
	    if( openvol() == 0 )
		exit(2);
	}else{
	    fprintf(stderr, "Must specify -v flag with -s option\n");
	    if( !blkflg ){
	        fprintf(stderr, "Must specify -b flag with -s option\n");
	    }
	    exit(2);
	}
	if( !blkflg ){
	    fprintf(stderr, "Must specify -b flag with -s option\n");
	    exit(2);
	}
    }else if( autoflg ){
	if( listflg ){
	    usage();
	    exit(2);
	}
	if( !logflg ){
	    fprintf(stderr, "Must specify -c flag with -a option\n");
	    exit(2);
	}
	if( dispflg && ( errflg || retryflg) ){
	   fprintf("Can't specify -d flag with either -e or -m\n");
	   exit(2);
	}
    }else if( listflg ){
	if( volflg ){
	    if(openvol() == 0)
		exit(2);
	}else{
	    fprintf(stderr, "Must specify -v flag with -l option\n");
	    exit(2);
	}
    }else{
	usage();
	exit(2);
    }

    if( intflg)
	manual();
    else if( specflg ){
        generic();
	specific();
    }else if( listflg){
        generic();
	table();
    }else
	auto_matic();

    exit(0);

}
generic()
{
    int i;
    int j;
    int nbytes;
    int thistrk;
    int lasttrk;
    struct device_parameters *dp;

    /*
     * Get the volume header and controller.  Different things need to be
     * done dependent upon whether this is a SCSI or SMD drive
     */
    io_arg.memaddr = (unsigned long)&vh;
    io_arg.datasz = (unsigned long)sizeof(vh);
    if( ioctl(devfd,DIOCGETVH, &io_arg) < 0 ){
	pioerr("DIOCGETVH");
	exit(3);
    }

    io_arg.memaddr = (unsigned long)&ct;
    io_arg.datasz = (unsigned long)sizeof(ct);
    if( ioctl( devfd, DIOCGETCTLR, &io_arg) < 0 ){
	pioerr("DIOCGETCTLR");
	exit(3);
    }
    dp = &vh.vh_dp;
    if(dp->dp_secbytes)
        nbps = dp->dp_secbytes;
    else
	nbps = DEV_BSIZE;
    /*
     * Read in bsttab if SMD drive
     */
    if( ct.ci_flags & DP_SMD ){
        bstfound = 0;
        for( i = 0; i < NVDIR; i++) {
            if( strcmp(vh.vh_vd[i].vd_name, "bsttab") == 0 ){
    	        if(lseek(devfd, (off_t)(vh.vh_vd[i].vd_lbn*nbps),0)<0){
	            fprintf( stderr, "Can't seek to the bad sector table\n");
                    perror( devarray);
		    exit(4);
	        }
	        nbytes = ((vh.vh_vd[i].vd_nbytes+nbps-1) / nbps) * nbps;
    	        if( read( devfd, bsttable, nbytes) < 0 ){
	            fprintf( stderr, "Can't read the bad sector table\n");
		    perror( devarray );
		    exit(4);
                }
	        bstfound = 1;
	        /* Due to bugs in previous versions of format, the number
	         * of bad sectors may be incorrect.  It works fine for
	         * the format program because it just reads them in, and
	         * keeps counting until it gets an EMPTY one.
	         */
	        for( j = 0; j < MAX_BADBLOCKS; j++){
		    if( bsttable[j].bt_rpltype == BSTTYPE_EMPTY ){
		        break;
		    }else
		        nbst++;
	        }
		/*
		 * Make sure that it's not a bizarre number
		 */
		if( nbst > (nbytes / sizeof( struct bst_table)))
		    nbst = nbytes / sizeof( struct bst_table);
	        break;
	    }
        }
	/*
	 * Get the size of the replacement track partition header.
	 */
	maxmtrks = 0;
	if( vh.vh_pt[PTNUM_REPL].pt_nblks == 0 ){
	    printf("Size of replacement partition is zero.\n");
	}
	maxmtrks = (vh.vh_pt[PTNUM_REPL].pt_nblks / dp->dp_secs);
	ftrk = TRACK(vh.vh_pt[PTNUM_REPL].pt_firstlbn, dp);
	j = 0;

	/* 
	 * Set up array of tracks that are already mapped.
	 * Find all of the bad tracks in the replacment partition.
	 */
	for( i = 0; i < nbst; i++){
	    if( (bsttable[i].bt_badlbn > vh.vh_pt[PTNUM_REPL].pt_firstlbn) ){
		if( bsttable[i].bt_badlbn > (vh.vh_pt[PTNUM_REPL].pt_firstlbn 
				+ vh.vh_pt[PTNUM_REPL].pt_nblks))
		    break;
		thistrk = TRACK(bsttable[i].bt_badlbn,dp) - ftrk;
		if( bsttable[i].bt_rpltype == BSTTYPE_TRKFWD ){
		    /*
		     * There should be at least 2 bad sectors per track,
		     * so only subtract one from maxmtrks only once...
		     * well, twice.  Once for the bad track, and once
		     * for the track it it mapped to.
		     */
		    if( mtrk[ thistrk] != MAPPED){
		        mtrk[ thistrk] = MAPPED;
			maxmtrks--;
		        while(mtrk[j] == MAPPED ){
			    j++;
			    if( j == maxmtrks ){
			        fprintf(stderr, "Disk Integrity questionable.\n");
			        fprintf( stderr, "The number of replacement tracks is out of sync.\n");
			        exit(5);
			    }
		        }
		        mtrk[j++] = MAPPED;
			maxmtrks--;
		    }
		}else 
		   mtrk[thistrk] = NOTMAPPEDW_SLIP;
	    }
	}
	/*
	 * Now go back and mark all the bad tracks on the rest of the
	 * disk that are mapped into the replacment partition.
	 */
	lasttrk = -1;
	for( i = 0; i < nbst; i++){
	    if( (bsttable[i].bt_badlbn < vh.vh_pt[PTNUM_REPL].pt_firstlbn) ||
	        (bsttable[i].bt_badlbn > (vh.vh_pt[PTNUM_REPL].pt_firstlbn
		+ vh.vh_pt[PTNUM_REPL].pt_nblks) )){
		/*
		 * Already took care of those in the replacement partition
		 */
		if( bsttable[i].bt_rpltype == BSTTYPE_TRKFWD ){
		    if( TRACK( bsttable[i].bt_badlbn, dp) != lasttrk ){
			while( mtrk[j] == MAPPED ){
			    j++;
			    if( j == maxmtrks ){
			        fprintf(stderr, "Disk Integrity questionable.\n");
			        fprintf( stderr, "The number of replacement tracks is out of sync.\n");
			        exit(5);
			    }
		        }
		        mtrk[j++] = MAPPED;
			maxmtrks--;
			lasttrk = TRACK(bsttable[i].bt_badlbn,dp);
		    }
		}
	    }
	}
    }
}

manual()
{
    char *tbuf;

    while(1){
        while(1){
	    printf("Enter path to /dev entry for entire volume [q to quit] : ");
	    if( fgets( devarray, 32, stdin ) ){
		if( devarray[0] == 'q' )
		    return;
	        tbuf = devarray;
	        while( *tbuf != '\n' && *tbuf != '\0')
		    tbuf++;
	        *tbuf = '\0';
	    }else
	        continue;
	    if( openvol() )
	        break;
        }
        generic();
        while(1){
            printf("(l)ist defect table, (a)dd defects, (s)can for defects, (q)uit [q] : ");
            fgets( buffer, 1024, stdin);
	    switch( buffer[0] ){
	        case 'l':
		    list_defects();
		    break;
	        case 'a':
		    add_defects();
		    break;
	        case 's':
		    scan_for_defects();
		    break;
	        case 'q':
	        default:
		    goto bout;
		    break;
	    }
        }
bout:
    close(devfd);
    }
}

specific()
{
    int i,j;
    int qcompar();
    struct device_parameters *dp = &vh.vh_dp;
    int tlbn;
    int maporslip;
    int maxblk;
    int gotone;
    
    /*
     * if c/h/s format was used, convert it.  We couldn't convert it
     * when it was entered because you need to know the disk geometry.
     */
    if( slashes ){
        if( !(ct.ci_flags & DP_SMD)){ 
	    fprintf(stderr, "c/h/s option of -b flag is only supported on SMD drives\n");
	    usage();
	    exit(2);
        }
	for( i = 0; i < nchs; i++){
	    blocks[i] = ((chs[i].cyl * dp->dp_trks0) + chs[i].hd) 
			* dp->dp_secs + chs[i].sec;
	}
	nblocks = nchs;
    }
    maxblk = dp->dp_secs * dp->dp_trks0 * dp->dp_cyls;
    gotone = 0;
     
    /*
     * Sort the list.
     */
    qsort( blocks, nblocks, sizeof(int), qcompar);

    if( ct.ci_flags & DP_SMD ){
        for( i = 0; i < nblocks; i++){
	    if( blocks[i] >= maxblk){
		fprintf(stderr, "%d is greater than the maximum block number (%d) for this drive\n", blocks[i], maxblk);
		break;
	    }
	    /*
	     * If already in bsttable, continue on.
	     * If another block in the same track is in bsttable,
	     * then remap the track.
	     */
	    if( bstfound ){
	        for( j = 0; j < nbst; j++){
		    if( blocks[i] == bsttable[j].bt_badlbn ){
			printf("%d is already mapped\n", blocks[i]);
			goto outtahere;
		    }else if( TRACK(blocks[i], dp) == TRACK(bsttable[j].bt_badlbn, dp)){
		        printf("%d is in the same track as %d (already in bst).  Map the entire track.\n", blocks[i], bsttable[j].bt_badlbn);
			maporslip = MAPIT;
			gotone++;
			break;
		    }else if( blocks[i] < bsttable[j].bt_badlbn ){
		        /* 
			 * List is sorted, so if number is greater
			 * than this one, then it's not in list.
			 */
		        printf("Slip sector %d\n", blocks[i]);
		        maporslip = SLIPIT;
			gotone++;
		        break;
		    }
		}
		if( j == nbst ){
		    printf("Slip sector %d\n", blocks[i]);
		    maporslip = SLIPIT;
		    gotone++;
		}
	    }else{
		printf("Slip sector %d\n", blocks[i]);
		maporslip = SLIPIT;
		gotone++;
	    }
	    /* 
	     * Verify that we can read the data off the track.
	     */
	    tlbn = blocks[i] - (blocks[i] % dp->dp_secs);
	    for( j = 0; j < maxretries; j++){
	        if( lseek(devfd, (off_t)(tlbn*nbps), 0) < 0 ){
		    perror( devarray);
		    continue;
		} else if( read( devfd, trkptr, dp->dp_secs * nbps) < 0 ){
		    perror( devarray );
		    continue;
	        }else
		    break;
	    }
	    if( !errflg ){
	        if( j >= maxretries ){
	           fprintf( stderr, "Couldn't remap : %d\n", blocks[i]);
	           continue;
	        }
	    }
	    smdmapit( blocks[i], maporslip);
outtahere:;
	}
	/*
	 * Only write out bst if we changed it
	 */
	if( gotone )
	    bst_write();
    }else{
	/*
	 * To assure data integrity, only do one defects at a time.
	 */
	for( i = 0; i < nblocks; i++){
	    if( blocks[i] >= maxblk){
		fprintf(stderr, "%d is greater than the maximum block number (%d) for this drive\n", blocks[i], maxblk);
		break;
	    }
	    /* 
	     * Verify that we can read the data from this block
	     */
	    for( j = 0; j < maxretries; j++){
	        if( lseek(devfd, (off_t)(blocks[i]*nbps), 0) < 0 ){
		    perror( devarray);
		    continue;
		} else if( read( devfd, trkptr, nbps) < 0 ){
		    perror( devarray );
		    continue;
	        }else
		    break;
	    }
	    if( !errflg ){
	        if( j >= maxretries ){
	           fprintf( stderr, "Couldn't remap : %d\n", blocks[i]);
	           continue;
	        }
	    }
	    if( scsimapit(blocks[i] ) ){
	        /*
	         * Write it back out
	         */
	        for( j = 0; j < maxretries; j++){
	            if( lseek(devfd, (off_t)(blocks[i]*nbps), 0) < 0 ){
		        perror( devarray);
		        continue;
		    } else if( read( devfd, trkptr, nbps) < 0 ){
		        perror( devarray );
		        continue;
	            }else
		        break;
		}
		if( j >= maxretries )
		    goto stillbad;
		printf("Block %d was remapped\n", blocks[i]);
	    }else{
stillbad:
		fprintf(stderr, "Despite all efforts, Block %d wasn't mapped\n", blocks[i]);
	    }
	}
    }
}
qcompar(ent1, ent2 )
int *ent1, *ent2;
{
    if( *ent1 < *ent2 )
	return(-1);
    else if( *ent1 > *ent2 )
	return(1);
    else
	return(0);
}
auto_matic()
{
    char *parsetmp, *parsebeg;
    long controller, unit;
    long blkno;
    int i;
    long cyl, hd, sec;
    int begblk, endblk;
    struct device_parameters *dp;

    if( scanfile == NULL ){
	usage();
	exit(2);
    }
    /* 
     * Scan the logfile for a list of blocks
     */
    if( (fd = fopen( scanfile, "r")) == NULL ){
	perror(scanfile);
	exit(3);
    }
    while( fgets( buffer, 1024, fd) ){
	/*
	 * Find one of the key device names
	 */
	for( i = 0; i < NENTRIES; i++ ){
	    if( parsebeg = strstr( buffer, keydev[i].type)){
		/*
		 * Skip over string.
		 */
		parsebeg += strlen(keydev[i].type);

		controller = strtol( parsebeg, &parsebeg, 10);
		/*
		 * Skip over : for DKVJ, L for SCSI, and 
		 * d for dkip
		 */
		parsebeg++;
		unit = strtol( parsebeg, &parsebeg, 10);

		/*
		 * If this was generated by one of the disk
		 * drives, see if it's a block specific
		 * error
		 */
		if( parsetmp = strstr( parsebeg, keydev[i].key)){

		    if( i == DKVJ || i == SCSI ){
			/* 
			 * block number is between (%d)
			 */
		  	while( *parsetmp != '\0'){
			    if( *parsetmp == '('){
				blkno = strtol( parsetmp+1, NULL, 10);
				break;
			    }else
				parsetmp++;
			}

		    }else{
			/* 
			 * XXX: Since the 2GB drives return lots of seek
			 * errors, ignore them.
			 */
			if( strstr( parsebeg, "seek error")){
			    goto getnextline;
			}
			/* 
			 * Parse c/h/s.  parsetmp looks like "chs=cyl/hd/sec"
			 */
			parsetmp += strlen(keydev[i].key);
			cyl = strtol( parsetmp, &parsetmp, 10 );
			parsetmp++;
			hd = strtol( parsetmp, &parsetmp, 10 );
			parsetmp++;
			sec = strtol( parsetmp, &parsetmp, 10 );
			/*
 			 * Since it doesn't always find 0/X/0 when booting, 
			 * and that gets stored in the log file, ignore
			 * them.
			if( cyl == 0 && sec == 0 ){
			    fprintf( stderr, "Warning : ignoring (c/h/s) %d/%d/%d.  Use the '-s' option to enter.\n");
			    goto getnextline;
			}
			/*
			 * Get the volume header information so  that we can 
			 * calculate the block #
			 * Open the entire volume so that we can use it to 
			 * scan from
			 */
			sprintf( devarray, "/dev/rdsk/ipc%dd%ds10", controller, unit);
			if( (devfd = open( devarray, O_RDWR)) < 0 ){
			    perror(devarray);
			    exit(3);
			}
			io_arg.memaddr = (unsigned long)&vh;
			io_arg.datasz = (unsigned long)sizeof(vh);
			if( ioctl(devfd,DIOCGETVH, &io_arg) < 0 ){
			    pioerr("DIOCGETVH");
			    exit(3);
			}
			dp = &vh.vh_dp;

			blkno = ((cyl * vh.vh_dp.dp_trks0) + hd) *
				vh.vh_dp.dp_secs + sec;
		    } 
		    begblk = (blkno - range);
		    endblk = blkno + range;

                    if( begblk < 0 )
                        begblk = 0;
		    if( endblk > (dp->dp_secs * dp->dp_trks0 * dp->dp_cyls))
			endblk = dp->dp_secs * dp->dp_trks0 * dp->dp_cyls;
		    scanlog(begblk, endblk);
		}
	    }
	}
getnextline:
        ;
    }
}

table()
{
    int i;
    struct bst_table *bst;
    DEFECT_ENTRY *de;
    int blkno;

    if( ct.ci_flags & DP_SMD ){
        printf("\nbad sector table:\n");
        if( nbst == 0 ){
	    printf("\tNo defects found.\n");
	    return;
        }
        for (i=0, bst = bsttable; i < nbst; i++, bst++) {
	    switch (bst->bt_rpltype) {
	    case BSTTYPE_SLIPSEC:
   	        printf("\tbad block %d slipped\n", bst->bt_badlbn);
	        break;
    
	    case BSTTYPE_SECFWD:
	        printf("\tbad block %d forwarded to sector %d\n",
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
    }else{
	printf("\nDefective Sector List:\n");
	io_arg.memaddr = (unsigned long)defptr;
	io_arg.datasz =  sizeof( struct defectlist);
	io_arg.sectst = PRIMARY_DEFECTS;
	if( ioctl( devfd, DIOCRDEFECTS, &io_arg) < 0 ){
	    pioerr("DIOCRDEFECTS");
	    return;
	}
	printf("PRIMARY DEFECT LIST\n");
	if( (defptr->header.list_size / 8  )== 0 )
	    printf("\tNo Primary Defects\n");
	else{
	    de = &defptr->entry[0];
	    for( i = 0; i < defptr->header.list_size / 8; i++, de++){
	        blkno = ((de->def_cyl * vh.vh_dp.dp_trks0) + de->def_head) *
				    vh.vh_dp.dp_secs + de->def_sect;
	        printf("\t%d\n", blkno);
	    }
	}
	io_arg.memaddr = (unsigned long)defptr;
	io_arg.datasz =  sizeof( struct defectlist);
	io_arg.sectst = GROWTH_DEFECTS;
	if( ioctl( devfd, DIOCRDEFECTS, &io_arg) < 0 ){
	    pioerr("DIOCRDEFECTS");
	    return;
	}
	printf("GROWTH DEFECT LIST\n");
	if( (defptr->header.list_size / 8  ) == 0 )
	    printf("\tNo Growth Defects\n");
	else{
	    de = &defptr->entry[0];
	    for( i = 0; i < defptr->header.list_size / 8; i++, de++){
	        blkno = ((de->def_cyl * vh.vh_dp.dp_trks0) + de->def_head) *
				    vh.vh_dp.dp_secs + de->def_sect;
	        printf("\t%d\n", blkno);
	    }
        }
    }
}

pioerr(cp)
char *cp;
{
    char tbuf[120];


    strcpy( tbuf, devarray);
    strcat( tbuf, ": ");
    strcat( tbuf, cp);
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
	default:
	    perror(tbuf);
	    return;
	}
    strcat(tbuf, "\n");
    fputs(tbuf, stderr);
}

smdmapit( blkno, maporslip )
int blkno;
int maporslip;
{
    int i,j;
    int repltrk;
    char *dataout,*tdataout;
    char *datain,*tdatain;
    int firstlbn,repllbn;
    struct fmt_map_info fmi;
    struct device_parameters *dp = &vh.vh_dp;

    /*
     * First thing to do is to make sure that the next track to be
     * mapped is good.  Even if we want to slip a sector, if the
     * sector to slip to is also bad, we're going to have to map
     * the entire track.  We want to make sure that it's possible
     * to write the data somewhere before reformatting the track.
     */
    if( maxmtrks < 1 ){
	fprintf(stderr, "No mapped tracks left.  Can't remap %d\n", blkno);
	return(0);
    }
    for(i = 0; i < MAX_MTRKS; i++){
        if( mtrk[i] != MAPPED )
	    break;
    }
    if( i == MAX_MTRKS ){
	fprintf(stderr, "No mapped tracks left.  Can't remap %d\n", blkno);
	return(0);
    }
    repltrk = ftrk + i;
    repllbn = repltrk * dp->dp_secs;
    tdataout = malloc( dp->dp_secs * nbps + NBPP + 1);
    tdatain = malloc( dp->dp_secs * nbps + NBPP + 1);
    dataout = (char *)PAGEALIGN(tdataout);
    datain = (char *)PAGEALIGN(tdatain);
#ifdef DEBUGGING
track_id[0].cylno = CYL( (repltrk * dp->dp_secs), dp);
track_id[0].head = HEAD( (repltrk * dp->dp_secs), dp);
io_arg.memaddr = (unsigned long)track_id;
io_arg.datasz = sizeof(struct track_id) * dp->dp_secs;
if( ioctl(devfd,DIOCTRKID, &io_arg) < 0 ){
    pioerr("DIOCTRKID");
}
for( k = 0; k < dp->dp_secs; k++)
    printf("c/h/s : (%d/%d/%d) dup h/s (%d/%d)\n", track_id[k].cylno,
    track_id[k].head, track_id[k].sector, track_id[k].duphead, 
    track_id[k].dupsector);
#endif
    for( j = 0; j < maxretries; j++){

	io_arg.sectst = repltrk * dp->dp_secs;
    	io_arg.datasz = dp->dp_secs;
        if( ioctl(devfd,DIOCVFYSEC, &io_arg) < 0 ){
	    pioerr("DIOCGETVH");
	    exit(3);
        }

	/*
	 * Read data off of that track.
	 */
	firstlbn = TRACK(blkno, dp) * dp->dp_secs;
        if( lseek(devfd, (off_t)(firstlbn * nbps), 0) < 0 ){
            perror( "lseek");
	    continue;
 	} 
	else if( read( devfd, dataout, dp->dp_secs * nbps) != 
		dp->dp_secs * nbps ){
	    perror( "read");
	    continue;
	}else
	    break;
    }
    if( !errflg ){
	/*
	 * This test is separate because if we can't read it, we don't
	 * want to write anything.
	 */
        if( j >= maxretries ){
	    fprintf( stderr, "Couldn't remap : %d\n", blkno);
	    return(0);
        }
    }
    for( j = 0; j < maxretries; j++){
	/*
	 * Write it to new track
	 */
        if( lseek(devfd, (off_t)(repltrk * dp->dp_secs * nbps) , 0) < 0 ){
            perror( "lseek");
	    continue;
 	} 
	if( write( devfd, dataout, dp->dp_secs * nbps) != dp->dp_secs * nbps ){
	    perror( "write");
	    continue;
	}
	/*
	 * Read it back in 
	 */
        if( lseek(devfd, (off_t)(repltrk * dp->dp_secs * nbps), 0) < 0 ){
            perror( "lseek");
	    continue;
 	} 
	if( read( devfd, datain, dp->dp_secs * nbps) != dp->dp_secs * nbps ){
	    perror( "read");
	    continue;
	}
	/*
	 * Compare
	 */
	if( bcmp( datain, dataout, dp->dp_secs * nbps) != 0 ){
	    fprintf( stderr, "Data miscompare when writing out to new track\n");
	    continue;
	}else
	    break;
    }
    if( !errflg ){
        if( j >= maxretries ){
	    fprintf( stderr, "Couldn't remap : %d\n", blkno);
	    return(0);
        }
    }

    /*
     * Now that we know that it's "safe", see if we want to slip a
     * sector or remap a track.
     */
    if( maporslip == SLIPIT ){
	firstlbn = TRACK(blkno, dp) * dp->dp_secs;
        for( j = 0; j < maxretries; j++){

	    /*
	     * Slip the sector 
	     */
	    io_arg.memaddr = (unsigned long )&fmi;
    	    io_arg.datasz = sizeof(fmi);
	    fmi.fmi_action = FMI_SLIP_SECTOR;
	    fmi.fmi_cyl = CYL(blkno,dp);
	    fmi.fmi_trk = HEAD(blkno,dp);
	    fmi.fmi_sec = SECTOR(blkno,dp);

            if( ioctl(devfd,DIOCFMTMAP, &io_arg) < 0 ){
	        pioerr("FMI_SLIP_SECTOR");
	        exit(3);
            }
	    /*
	     * Now write the data back out
	     */
            if( lseek(devfd, (off_t)(firstlbn * nbps), 0) < 0 ){
                perror( "lseek");
	        continue;
 	    } 
	    if( write( devfd, dataout, dp->dp_secs * nbps) != 
				dp->dp_secs * nbps ){
	        perror( "read");
	        continue;
	    }
	    /*
	     * Read it back in and compare, just to be sure
	     */
            if( lseek(devfd, (off_t)(firstlbn * nbps), 0) < 0 ){
                perror( "lseek");
	        continue;
 	    } 
	    if( read( devfd, datain, dp->dp_secs * nbps) != 
				dp->dp_secs * nbps ){
	        perror( "read");
	        continue;
	    }
	    if( bcmp( datain, dataout, dp->dp_secs * nbps) != 0 ){
	        fprintf( stderr, "Data miscompare when writing out to new track\n");
	        continue;
	    }else
		break;
	}
	/*
	 * Even if errflg was specified (map even on errors),
	 * Be nice and map the whole track since we know that
	 * works.
	 */ 
	if( j >= maxretries ){
	    /*
	     * map the entire track instead to the one that we
	     * already verified was okay
	     */
	     goto maptrack;
	}
    }else{
maptrack:
	for( j = 0; j < maxretries; j++){
	    /*
	     * Map the track
	     */
	    io_arg.memaddr = (unsigned long )&fmi;
    	    io_arg.datasz = sizeof(fmi);
	    fmi.fmi_action = FMI_MAP_TRACK;
	    fmi.fmi_cyl = CYL(blkno,dp);
	    fmi.fmi_trk = HEAD(blkno,dp);
	    fmi.fmi_rplcyl = CYL(repllbn,dp);
	    fmi.fmi_rpltrk = HEAD(repllbn,dp);

            if( ioctl(devfd,DIOCFMTMAP, &io_arg) < 0 ){
	        pioerr("FMI_MAP_TRACK");
	        exit(3);
            }
	    /*
	     * Mark this track in mtrk
	     */
	    mtrk[repltrk - ftrk] = MAPPED;

	    /*
	     * Now write the data back out
	     */
            if( lseek(devfd, (off_t)(firstlbn * nbps), 0) < 0 ){
                perror( "lseek");
	        continue;
 	    } 
	    if( write( devfd, dataout, dp->dp_secs * nbps) != 
				dp->dp_secs * nbps ){
	        perror( "read");
	        continue;
	    }
	    /*
	     * Read it back in and compare, just to be sure
	     */
            if( lseek(devfd, (off_t)(firstlbn * nbps), 0) < 0 ){
                perror( "lseek");
	        continue;
 	    } 
	    if( read( devfd, datain, dp->dp_secs * nbps) != 
				dp->dp_secs * nbps ){
	        perror( "read");
	        continue;
	    }
	    if( bcmp( datain, dataout, dp->dp_secs * nbps) != 0 ){
	        fprintf( stderr, "Data miscompare when writing out to new track\n");
	        continue;
	    }else
		break;

	}
        if( j >= maxretries ){
	     /*
	      * It should never get here, but one never knows.
	      */
	     fprintf(stderr, "Despite all efforts, the data was corrupted.\n");
	     exit(666);
	}
    }

	  
    bstinsert( blkno, maporslip,repltrk * dp->dp_secs);
    free(dataout);
    free(datain);
    return(1);

}
scsimapit( blkno )
int blkno;
{
    int i;
    struct fmt_map_info fmi;

    for( i = 0; i < maxretries; i++){
	fmi.fmi_action = FMI_MAP_TRACK;
	fmi.fmi_addr = (caddr_t)&reassign;
	fmi.fmi_intrlv = 4 + 4;
	reassign.length = 4;
	reassign.blocks[0] = blkno;

	io_arg.memaddr = (unsigned long)&fmi;
	io_arg.datasz = (unsigned long)sizeof( struct fmt_map_info);

	if( ioctl(devfd, DIOCFMTMAP, &io_arg) < 0 ){
	    pioerr("DIOCFMTMAP");
	}else
	    return(1);
    }
    return(0);
}
bstinsert( blkno, maporslip, repltrk)
int blkno;
int maporslip;
int repltrk;
{
    int i,j;
    struct device_parameters *dp = &vh.vh_dp;

    for (i=0; i < nbst; i++) {
        if( blkno < bsttable[i].bt_badlbn){
            /*
	     * This is where to insert
	     */
	    for( j = nbst; j > i; j--)
		bsttable[j] = bsttable[j-1];
	    bsttable[i].bt_badlbn = blkno;
	    if( maporslip == SLIPIT ){
		bsttable[i].bt_rpltype = BSTTYPE_SLIPSEC;
	    }else{
		bsttable[i].bt_rpltype = BSTTYPE_TRKFWD;
		bsttable[i].bt_rpllbn = repltrk;
	    }
	    break;
	}
    }
    if( i == nbst ){
        /*
	 * Add to the end
	 */
	bsttable[i].bt_badlbn = blkno;
	if( maporslip == SLIPIT ){
	    bsttable[i].bt_rpltype = BSTTYPE_SLIPSEC;
	}else{
	    bsttable[i].bt_rpltype = BSTTYPE_TRKFWD;
	    bsttable[i].bt_rpllbn = repltrk;
	}
    }
    nbst++;
    /*
     * If it was a track forward.  See if the adjoining entries
     * Need to be updated
     */
    for( j = i - 1; j >= 0; j-- ){
	if( TRACK( bsttable[j].bt_badlbn, dp) == TRACK( bsttable[i].bt_badlbn, dp)){
	    bsttable[j].bt_rpltype = BSTTYPE_TRKFWD;
	    bsttable[j].bt_rpllbn = repltrk;
	}else
	    break;
    }
    for( j = i + 1; j < nbst; j++ ){
	if( TRACK( bsttable[j].bt_badlbn, dp) == TRACK( bsttable[i].bt_badlbn, dp)){
	    bsttable[j].bt_rpltype = BSTTYPE_TRKFWD;
	    bsttable[j].bt_rpllbn = repltrk;
	}else
	    break;
    }
}

bst_write()
{
    int i,j;
    struct device_parameters *dp = &vh.vh_dp;
    int *ip;
    int csum;
    struct volume_directory *vd;
    unsigned nbytes;
    FILE *dumpfd;
    struct volume_directory *bst_allocsp();
    char *tbuf;

    /* 
     * write out bad sector table 
     */
    if ((vd = bst_allocsp()) == NULL) {
	printf("\nAllocation for bad sector table failed\n");
	printf("Write defects to a file? [y] ");
        if( fgets( buffer, 1024, stdin) ){
	    if( buffer[0] == 'n' || buffer[0] == 'N' ){
		return(0);
	    }else{
tryagain:
	        printf("Name of file : ");
                if( fgets( buffer, 1024, stdin) ){
	  	    /*
		     * Get rid of newline (use fgets so that the
		     * user doesn't overrun "buffer"
		     */
		    tbuf = buffer;
		    while( *tbuf != '\n' )
			tbuf++;
		    *tbuf = '\0';
		    if( (dumpfd = fopen( buffer, "w")) == NULL ){
			perror(buffer);
			goto tryagain;
		    }else{
			bst_dump( dumpfd);
			fclose(dumpfd);
			return(0);
		    }
		}else
		    goto tryagain;
	    }
	}
    }

	    

    if ((vd->vd_nbytes != 0) && (vd->vd_lbn != INVALID)) {
	nbytes = ((vd->vd_nbytes + nbps-1) / nbps) * nbps;
        for( i = 0; i < maxretries; i++){
	    if (lseek(devfd, (off_t)(vd->vd_lbn * nbps), 0) < 0) {
	        perror("lseek");
	    } else if (write(devfd, bsttable, (unsigned)nbytes) != nbytes) {
	        perror("write");
	    }else
		break;

	}
        if( i >= maxretries ){
	    printf("\nCouldn't write bad sector table after %d retries.\n", maxretries);
	    printf("Write defects to a file? [y] ");
            if( fgets( buffer, 1024, stdin)){
	        if( buffer[0] == 'n' || buffer[0] == 'N' ){
		    return(0);
	        }else{
tryonceagain:
	            printf("Name of file : ");
                    if( fgets( buffer, 1024, stdin)){
			/*
			 * Get rid of newline (use fgets so that the
			 * user doesn't overrun "buffer"
			 */
			tbuf = buffer;
			while( *tbuf != '\n' )
			    tbuf++;
			*tbuf = '\0';
		        if( (dumpfd = fopen( buffer, "w")) == NULL ){
			    perror(buffer);
			    goto tryonceagain;
		        }else{
			    bst_dump(dumpfd);
			    fclose(dumpfd);
			    return(0);
		        }
		    }else
		        goto tryonceagain;
	        }
	    }
        }
    }
    /*
     * If the bsttab was written out successfully.  The volume header
     * must be rewritten since the volume directory was changed, and
     * a new checksum must be calculated.
     */
    vh.vh_magic = VHMAGIC;
    vh.vh_csum = 0;
    for (csum = 0, ip = (int *)&vh; ip < (int *)(&vh+1); ip++) {
	csum += *ip;
    }
    vh.vh_csum = -csum;

    /* 
     * write volume header in the 1st sector of each track of cyl 0 
     */
    for (i=0; i < dp->dp_trks0; i++) {
	for( j = 0; j < maxretries; j++){
	    if (lseek(devfd, (off_t)(i * dp->dp_secs * nbps), 0) < 0) {
	        perror("lseek");
	        continue;
            }
	    if (write(devfd, &vh, nbps) != nbps) {
		perror("write");
		continue;
	    }else
		break;
	}
	if( j >= maxretries){
	    fprintf( stderr, "write of volhdr at trk %d failed\n", i);
	}
    }

    io_arg.memaddr = (unsigned long)&vh;
    io_arg.datasz = (unsigned long)sizeof(vh);
    if (ioctl(devfd, DIOCSETVH, &io_arg) < 0) {
        pioerr("DIOCSETVH");
	return(0);
    }
    return(1);
}

/*
 * find a location in the volume header partition for the
 * bad sector table
 */
struct volume_directory *
bst_allocsp()
{
    struct device_parameters *dp = &vh.vh_dp;
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
    for (i=0, tvd = vh.vh_vd; i < NVDIR; i++, tvd++) {
	if ((vd == NULL) && 
		(strncmp(tvd->vd_name, "bsttab", VDNAMESIZE) == 0)) {
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
	    vd = &vh.vh_vd[empty];
	    strcpy(vd->vd_name, "bsttab");
	}
    }

    /*
     * Recalculate the size and offsets
     */
    oldnblks = (vd->vd_nbytes + dp->dp_secbytes - 1) / dp->dp_secbytes;
    vd->vd_nbytes = (nbst+1) * sizeof(struct bst_table);
    nblks = (vd->vd_nbytes + dp->dp_secbytes - 1) / dp->dp_secbytes;
    if ((nblks <= oldnblks) && (vd->vd_lbn > 0))
	return (vd);

    vd->vd_lbn = INVALID;
    for (i=0, pt = vh.vh_pt; i < NPARTAB; i++, pt++) {
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

bst_dump( dumpfd )
FILE *dumpfd;
{
    int i;
    struct device_parameters *dp = &vh.vh_dp;

    for( i = 0 ; i < nbst; i++)
	fprintf( dumpfd, "%d, %d, %d\n", CYL( bsttable[i].bt_badlbn,dp),
	    HEAD( bsttable[i].bt_badlbn,dp), SECTOR( bsttable[i].bt_badlbn,dp));
}

scanlog( begblk, endblk)
int begblk;
int endblk;
{

    int tmpblk;
    int j,k;
    int nseeks;

    /*
     * Disable ECC correction and driver retries  (bits 0 and 1 on)
     * First set a signal catcher since we need to re-enable ECC if
     * somebody ^C's in the midst of this.
     */

    sigset( SIGINT, catchit);
    sigset( SIGTERM, catchit);

    io_arg.sectst = 3;
    if( ioctl( devfd, DIOCNOECC, &io_arg) < 0 ){
	pioerr("DIOCNOECC");
	exit(3);
    }
    nblocks = 0;

    for( j = 0; j < npasses; j++){
	if( intflg )
	    printf("Pass # %d...\n", j);
	for( tmpblk = begblk; tmpblk <= endblk; tmpblk++){
	    if (lseek(devfd, (off_t)(tmpblk * nbps), 0) < 0) {
	        perror("lseek");
	        continue;
            }
	    /*
	     * read one sector at a time, until we get an error 
	     */
	    if ( read(devfd, trkptr, nbps) != nbps) {
	        for( k = 0; k < nblocks; k++) {
	            /*
	             * Make sure it wasn't added on a previous pass
		     */
		    if( blocks[k] == tmpblk ) 
		        break;
	        }
	        if( k == nblocks )
		    blocks[nblocks++] = tmpblk;
	    }
	}
    }
    /*
     * Enable ECC correction and driver retries  (bits 0 and 1 off)
     */
    io_arg.sectst = 0;
    if( ioctl( devfd, DIOCNOECC, &io_arg) < 0 ){
	pioerr("DIOCNOECC");
	exit(3);
    }
    sigset( SIGINT, SIG_DFL);
    sigset( SIGTERM, SIG_DFL);
    /*
     * Either display them or map them.
     */

    if( dispflg ){
	if( !intflg )
	    printf("\n--- %s", buffer);
	if( nblocks == 0 ){
	    printf("\n\tNo Defective Sectors after %d scan passes from block %d to block %d\n", 
		npasses, begblk, endblk);
	}else{
	    printf("\n\tDefective Sectors from block %d to block %d\n", 
		begblk, endblk);
	    for( j = 0; j < nblocks; j++){
	        printf("\t\t%d\n", blocks[j]);
	    }
	}
    
    }else{
	/*
	 * Just use the routines for the '-s' flag.  It will do an
	 * additional GETVH, but since 'devarray' and the 'blocks' array 
	 * is set up, these routines will work just fine.
	 */
	generic();
	specific();
    }
}

openvol()
{
    struct stat sbuf;

    /*
     * Make sure it's a raw device
     */
    if( stat( devarray, &sbuf) < 0 ){
	fprintf( stderr, "Cannot stat %s\n", devarray);
	perror("stat");
	return(0);
    }

    if( (sbuf.st_mode & S_IFMT) != S_IFCHR){
	fprintf(stderr, "%s: Must be a character device\n", devarray);
	return(0);
    }

/*
    if( (sbuf.st_rdev & 0xf) != PTNUM_VOLUME ){
	fprintf( stderr, "%s: Device must be entire volume, which is partition %d\n", devarray, PTNUM_VOLUME );
	return(0);
    }
*/
    if( (devfd = open( devarray, O_RDWR)) < 0 ){
	perror(devarray);
	return(0);
    }
    return(1);
}
add_defects()
{
    struct device_parameters *dp = &vh.vh_dp;
    int i;
    int c,h,s;
    char *tmp;
    int maxblk;
    int cylenter;

    if( ct.ci_flags & DP_SMD ){
        printf("Defects can be entered either by block number or by cylinder/head/sector\n");
        printf("(b)lock number or (c)/h/s [b] : ");
        cylenter = 0;
        if( fgets( buffer, 1024, stdin) )
	    if( buffer[0] == 'c' )
	        cylenter = 1;
    }else
	cylenter = 0;
    
    if( cylenter ){
	for( i = 0; i < MAX_BADBLOCKS; i++){
tryagain:
	    printf("Enter c/h/s (q to quit): ");
            fgets( buffer, 1024, stdin);
	    if( buffer[0] == 'q' )
		break;
	    tmp = buffer;

	    c = (short)strtol(tmp, &tmp, 10);
	    if( *tmp++ != '/'){
		fprintf(stderr, "c/h/s format incorrect\n");
		goto tryagain;
	    }
	    if( c < 0 || c >= dp->dp_cyls ){
	       printf("cylinder number %d out of range [0-%d]\n",
				c,dp->dp_cyls - 1);
	       goto tryagain;
	    }
	    h = (char)strtol(tmp, &tmp, 10);
	    if( h < 0 || h >= dp->dp_trks0 ){
	       printf("head number %d out of range [0-%d]\n", 
				h, dp->dp_trks0 - 1);
	       goto tryagain;
	    }
	    if( *tmp++ != '/'){
		fprintf(stderr, "c/h/s format incorrect\n");
		goto tryagain;
	    }
	    s = (char)strtol(tmp, &tmp, 10);
	    if( s < 0 || s >= dp->dp_secs ){
	       printf("sector number %d out of range [0-%d]\n", 
				s, dp->dp_secs);
	       goto tryagain;
	    }
	    blocks[i] = ((c * dp->dp_trks0) + h) * dp->dp_secs + s;
	}
	nblocks = i;
    }else{
        maxblk = dp->dp_secs * dp->dp_trks0 * dp->dp_cyls;
        for( i = 0; i < MAX_BADBLOCKS; i++){
tryblkagain:
	    printf("Enter block number (q to quit): ");
	    fgets( buffer, 1024, stdin);
	    if( buffer[0] == 'q' )
		break;
	    tmp = buffer;

	    base = 10;
	    if( *tmp == '0' ){
	        ++tmp;
	        if( *tmp == 'x' || *tmp == 'X'){
		    base = 16;
		    tmp++;
	        }else
		    base = 8;
		}
	    blocks[i] = strtol( buffer, NULL, base);
	    if( blocks[i] >= maxblk ){
		printf("Block number %d out of range [0-%d]\n",
				blocks[i], maxblk);
		goto tryblkagain;
	    } 
	}
        nblocks = i;
    }
    specific();
}
scan_for_defects()
{
    int i; 
    int begblk, endblk;
    int maxblk;
    struct device_parameters *dp = &vh.vh_dp;

    printf("Enter the number of scans passes [3] : ");
    npasses = 0;
    if( fgets( buffer, 1024, stdin) )
	npasses = atoi(buffer);
    if( npasses == 0 )
	npasses = 3;

    printf("Enter the beginning block number [0] : ");
    begblk = 0;
    if( fgets( buffer, 1024, stdin) )
	begblk = atoi(buffer);

    maxblk = dp->dp_secs * dp->dp_trks0 * dp->dp_cyls;
    while( 1 ){
        printf("Enter the ending block number [%d] : ", maxblk-1);
        endblk = maxblk-1;
        if( fgets( buffer, 1024, stdin) ){
	    endblk = atoi(buffer);
	    if( !endblk )
                endblk = maxblk-1;
	}
	if( endblk > maxblk-1 )
	    fprintf( stderr, "%d out of range.\n", endblk);
	else if( endblk < begblk )
	    fprintf( stderr, 
	    "Ending block number %d must be greater than %d.\n", 
	    endblk, begblk);
	else
	    break;
    }
    /*
     * Use the 'automatic' routine, but only display the defects
     */
    dispflg++;
    printf("Scanning device %s.  Range %d - %d\n", devarray, begblk, endblk);
    scanlog( begblk, endblk);
    dispflg--;
}
list_defects()
{
    table();
}

catchit()
{
    /*
     * Enable ECC correction and driver retries  (bits 0 and 1 off)
     */
    io_arg.sectst = 0;
    if( ioctl( devfd, DIOCNOECC, &io_arg) < 0 ){
	pioerr("DIOCNOECC");
	exit(3);
    }
    exit(1);
}

usage()
{
    (void)fprintf(stderr, "usage: badspots [-a -clogfile [-mmaxretries] [-pscanpasses] [-d] [-e] [-rrange]  | [-i]  | [-s -vvolume -b(block[,block] | c/h/s[,c/h/s]) [-d] [-e]] |  [-l -vvolume]\n");
}


