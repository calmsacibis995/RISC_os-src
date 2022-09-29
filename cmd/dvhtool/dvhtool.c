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
#ident	"$Header: dvhtool.c,v 1.6.2.4.1.2 90/08/03 11:12:20 hawkes Exp $"

/*
 * dvhtool.c -- tool for modifying disk volume headers
 */

/* 
 * TODO  clp
 * 
 * interactive
 * fprintf stderr
 * copy on each track.
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/dvh.h>
#include <sys/fcntl.h>
#include <sys/dkio.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define	DEFAULT_VOLHDR	"/dev/dsk/ipc0d0vh"

#define	FNLEN		128		/* size of filename buffer */
#define DEV_BSIZE	512		/* one sector */

char Volume[FNLEN] = DEFAULT_VOLHDR;	/* current volume header being edited */

#define	streq(x,y)	(strcmp(x,y)==0)

union uvh {
	struct volume_header vh1;	/* current header being edited */
	char fill[DEV_BSIZE];		/* force to be 1 sector */
} *u_vh;
char Alignbuf[DEV_BSIZE * 2];

/* shorthand */
#define	vh	u_vh->vh1

/*
 * options for volume directory manipulation
 */
#define VD_ASK		0
#define VD_ADD		1
#define VD_DELETE	2
#define VD_LIST		3
#define VD_CREAT	4

/*
 * options for partition table manipulation
 */
#define PT_ASK		0
#define PT_MODIFY	1
#define PT_LIST		2

/*
 * options for device parameters manipulation
 */
#define DP_ASK		0
#define DP_MODIFY	1
#define DP_LIST		2

int got_dvh = 0;	/* 1 if getdvh() was called on current volume */
int changed_vh = 0; 	/* set to one when the volume header is changed so */
			/* that we can tell when to write it back out	   */
int interactive = 0;	/* Most messages only are printed out if dvhtool   */
			/* is being run interactively (argc = 1)	   */
int israw = 0;		/* set to 1 if the device is /dev/rXXX		   */
main(argc, argv)
int argc;
char *argv[];
{
	int cleanup();
	char *unix_file;
	char *dvh_file;
	char buf[FNLEN];
	int nblks;
	int fblk;
	int value;

	u_vh = (union uvh *)( (int)&Alignbuf[DEV_BSIZE] & ~(DEV_BSIZE - 1));
	if (sizeof(*u_vh) != DEV_BSIZE) {
		fprintf( stderr, "volume header size incorrect\n");
		exit(1);
	}
	signal(SIGINT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGTERM, cleanup);
	interactive = 0;
	got_dvh = 0;
	if (argc == 1) {
		interactive = 1;
		for (;;) {
			printf("\nCommand? (read, vd, pt, dp, write, bootfile, or quit): ");
			gets(buf);
			switch (*buf) {
				case 'r':
					getdvh ();
					break;

				case 'v':
					edit_vd (VD_ASK, 0, 0);
					break;

				case 'p':
					edit_pt (PT_ASK, 0, 0);
					break;

				case 'd':
					edit_dp (DP_ASK, 0, 0);
					break;

				case 'w':
					putdvh();
					break;

				case 'b':
					change_bootname(0);
					break;

				case 'q':
					cleanup();
					break;

				default:
					printf( "Unknown command: %s\n", buf);
					break;
			}
		}
	}
	while (--argc > 0) {
		argv++;
		if ((*argv)[0] == '-') {
			switch ((*argv)[1]) {
			case 'f':
				got_dvh = 0;
				changed_vh = 0;

				if (--argc < 1) {
					usage();
					xcleanup(1);
				}
				strcpy(Volume, *++argv);
				/*
				 * Make sure that full path name is specified
				 */
				if( strncmp( "/dev/", Volume, 5) != 0 ){
				    fprintf( stderr, "Volume name must be full path name\n");
				    usage();
				    xcleanup(1);
				}
				if( Volume[5] == 'r' )
				    israw = 1;
				else
				    israw = 0;
				
#ifdef DEBUG
				printf("New volume is %s\n", Volume);
#endif
				break;
			case 'v':
				if (!got_dvh) {
					getdvh();
					got_dvh = 1;
				}
				/*
				 * manipulate volume directory
				 */
				argv++;
				if (--argc <= 0) {
					usage();
					xcleanup(1);
				}
				switch ((*argv)[0]) {
				case 'c':
					/* 
					 * create an entry in the volume
					 * directory
					 */
					if (--argc < 2 || (*argv)[2] == '-' ||
					    (*argv)[3] == '-') {
						usage();
						xcleanup(1);
					}
					unix_file = *++argv;
					dvh_file = *++argv;
					argc--;
					edit_vd(VD_CREAT, unix_file, dvh_file);
					break;
				case 'a':
					/*
					 * add an entry to volume directory
					 */
					if (--argc < 2 || (*argv)[2] == '-' ||
					    (*argv)[3] == '-') {
						usage();
						xcleanup(1);
					}
					unix_file = *++argv;
					dvh_file = *++argv;
					argc--;
					edit_vd(VD_ADD, unix_file, dvh_file);
					break;
				case 'd':
					/*
					 * delete an entry from volume directory
					 */
					if (--argc < 1 || (*argv)[2] == '-') {
						usage();
						xcleanup(1);
					}
					dvh_file = *++argv;
					edit_vd(VD_DELETE, 0, dvh_file);
					break;
				case 'l':
					/*
					 * list volume directory entries
					 */
					edit_vd(VD_LIST, 0, 0);
					break;
				case 'b':
					change_bootname (*(++argv));
					argc--;
					break;
					
				default:
					/*
					 * unknown option
					 */
					usage();
					xcleanup(1);
				}
				break;
			case 'p':
				fprintf( stderr, "-p not supported as a command line option\n");
				usage();
				xcleanup(1);
#ifdef notdef
				if (!got_dvh) {
					getdvh();
					got_dvh = 1;
				}
				/*
				 * manipulate partition table
				 */
				edit_pt(PT_MODIFY, value, nblks, fblk, buf);
				break;
#endif
			case 'd':
				fprintf( stderr, "-d not supported as a command line option\n");
				usage();
				xcleanup(1);
#ifdef notdef
				if (!got_dvh) {
					getdvh();
					got_dvh = 1;
				}
				/*
				 * manipulate device parameters
				 */
				edit_dp(DP_MODIFY, buf, value);
				break;
#endif
			default:
				fprintf( stderr, "unknown option %s\n", *argv);
				usage();
				xcleanup(1);
			}
		}else{
    			usage();
			xcleanup(1);
		}
	}
	if (got_dvh && changed_vh) {
		putdvh();
	}
	cleanup();
}

usage()
{
#ifdef NOTSUPPORTED
	printf("usage: dvhtool [-f volume] [-p [part nblks 1st_blk type] [list]] [-v [add unix_file dvh_file] [creat unix_file dvh_file] [bootfile unix_file] [delete dvh_file] [list]] [-d [name value] [list]] ] \n");
#else
	fprintf( stderr, "usage: dvhtool [-f volume] [-v [add unix_file dvh_file] [creat unix_file dvh_file] [bootfile unix_file] [delete dvh_file] [list]]\n");
#endif
}

getdvh()
{
	register struct volume_directory *vd;
	register int fd,rfd;
	char buf[FNLEN];
	char rbuf[FNLEN];
	char *rname, *rawname();
	struct io_arg io_arg;
	int i, trks, secs;

#ifdef DEBUG
	printf("getdvh - Volume is %s\n", Volume);
	return(0);
#endif

again:
	if (interactive) {
		printf("Volume? ");
		gets(buf);
		if (*buf == 0)
			return(0);
		if ((fd = open(buf, O_RDWR)) < 0) {
			perror(buf);
			goto again;
		}
		strcpy(Volume, buf);
	} else {
		if ((fd = open(Volume, O_RDWR)) < 0) {
			perror(Volume);
			xcleanup(1);
		}
		/*
		 * Keep the name in 'buf' for consistancy.
		 */
		strncpy(buf, Volume, FNLEN);
		buf[FNLEN-1]='\0';
	}
	/*
	 * Check if the specified volume is the raw device.  In some
	 * cases, we may need to call ioctl, and if so, we need the
	 * raw device.
	 */
	if( buf[5] == 'r' )
	    israw = 1;
	else 
	    israw = 0;

	/*
	 * First try to read volume header from track.
	 */
	if (read(fd, &vh, sizeof(vh)) != sizeof(vh)) {

	    /*
	     * Perhaps the first one was bad.  Read from other tracks
	     * However, before we can do that, we have to get the disk
	     * geometry info from the kernel. If we don't have a raw
	     * device, open it so that we can get it.
	     */

	    if( !israw ){
		rname = rawname(buf);

		io_arg.memaddr = (unsigned long) &vh;
		io_arg.datasz = sizeof(struct volume_header);
		if ((rfd = open(rname, O_RDONLY)) < 0) {
			if( interactive )
			    printf( "Couldn't open %s for disk geometry in volume header.\n", rname);
			else
			    fprintf( stderr, "Couldn't open %s for disk geometry in volume header.\n", rname);
			perror(rname);
			close(fd);
			if( interactive )
			    return(0);
			else
			    xcleanup(1);
		}
	    }else
		rfd = fd;

	    if( ioctl( rfd, DIOCGETVH, &io_arg) < 0){
		if( interactive )
		    printf("Couldn't get disk geometry information.\n");
		else
		    printf(stderr, "Couldn't get disk geometry information.\n");
	        perror(rname);
		if( !israw )
		    close(rfd);
		close(fd);
		if( interactive )
		    return(0);
		else
		    xcleanup(1);
	    }

	    if( !israw )
		close(rfd);
	    trks = vh.vh_dp.dp_trks0;
	    secs = vh.vh_dp.dp_secs;
    	    for( i = 0; i < trks; i++){
	        lseek( fd, secs * i, 0);
		if (read(fd, &vh, sizeof(vh)) == sizeof(vh)) 
		    break;
	    }
	    if( i == trks ){
		if( interactive )
		    printf("Couldn't read volume header\n");
		else
		    fprintf(stderr, "Couldn't read volume header\n");
		perror(buf);
		close(fd);
		if( interactive )
		    return(0);
		else
		    xcleanup(1);
	    }
	}
	/*
	 * read in contents of volume directory to local files
	 */
	if (vh.vh_magic != VHMAGIC){
		if( interactive ){
		    printf("Bad magic number for volume header.\n");
		    close(fd);
		    return(0);
		}else{
		    fprintf( stderr, "Bad magic number for volume header.\n");
		    close(fd);
		    xcleanup(1);
		}
        }
	if (vh_checksum(&vh)){
		if( interactive ){
		    printf("Invalid volume header checksum.\n");
		    close(fd);
		    return(0);
		}else{
		    fprintf( stderr, "Invalid volume header checksum.\n");
		    close(fd);
		    xcleanup(1);
		}
	}
	for (vd = vh.vh_vd; vd < &vh.vh_vd[NVDIR]; vd++) {
		if (vd->vd_lbn == -1)
			vd->vd_nbytes = 0;
		if (vd->vd_nbytes)
			if( !copy_file(vd, fd) ){
			    close(fd);
			    if( interactive ){
			        return(0);
			    }else{
			        xcleanup(1);
			    }
			}
	}
	got_dvh = 1;
	close(fd);
	return(1);
}

#define TMPDIR	"/tmp/"

copy_file(vdp, fd)
register struct volume_directory *vdp;
register int fd;
{
	register int bytes;
	register int cc;
	register int tfd;
	char buf[DEV_BSIZE];
	char fn[FNLEN];

	sprintf(fn, "%s%s.%d", TMPDIR, vdp->vd_name, getpid());
	if ((tfd = open(fn, O_RDWR|O_CREAT|O_TRUNC, 0600)) < 0) {
		perror(fn);
		return(0);
	}
	new_tmpfile(fn);
	if (lseek(fd, vdp->vd_lbn * DEV_BSIZE, 0) < 0) {
		fprintf( stderr, "Seek to block %d failed\n", vdp->vd_lbn);
		goto bad;
	}
	for (bytes = vdp->vd_nbytes; bytes > 0;) {
		cc = min(sizeof(buf), bytes);
		if (read(fd, buf, cc) != cc) {
			fprintf( stderr, "Read error on %s\n", vdp->vd_name);
			perror(vdp->vd_name);
			break;
		}
		if( write(tfd, buf, cc) != cc){
			fprintf( stderr, "Write error on tmpfile: %s\n", fn);
			perror(fn);
			break;
		}
		bytes -= cc;
	}
	if( bytes == 0 ){
	        close(tfd);
	        return(1);
	}
bad:
	close(tfd);
	return(0);
}

#define	MAXFILES	20
char *file_list[MAXFILES];

new_tmpfile(fn)
register char *fn;
{
	register int i;
	register char *cp;
	extern char *malloc();

	for (i = 0; file_list[i] && i < MAXFILES; i++)
		continue;
	if (i >= MAXFILES)
		fprintf( stderr, "Too many files, garbage left in tmp\n");
	else {
		cp = malloc(strlen(fn)+1);
		strcpy(cp, fn);
		file_list[i] = cp;
	}
}

cleanup()
{
    xcleanup(0);
}

xcleanup(ecode)
int ecode;
{
	register int i;

	for (i = 0;  i < MAXFILES; i++)
		if (file_list[i])
			unlink(file_list[i]);
	exit(ecode);
}

/*
 * manipulate volume directory
 */
edit_vd(opt, unix_file, dvh_file)
int opt;
register char *unix_file;
register char *dvh_file;
{
	register char *cp;
	char buf[FNLEN];

#ifdef DEBUG
	printf("edit_vd\n");
	return;
#endif

	if( !got_dvh ){
	    if( interactive)
  		printf( "Must have valid volume header.\n");
	    else
  		fprintf( stderr, "Must have valid volume header for -v option\n");
	    return;
	}

	/* 
	 * Only VD_ASK is an interactive option 
	 */
	switch (opt) {
	case VD_CREAT:
		if( !vd_creat(unix_file, dvh_file, 1))
		    xcleanup(1);
		else
		    return;

	case VD_ADD:
		if( !vd_creat(unix_file, dvh_file, 0))
		    xcleanup(1);
		else
		    return;
		return;

	case VD_DELETE:
		vd_delete(dvh_file);
		return;

	case VD_LIST:
		vd_list();
		return;
	case VD_ASK:
		vd_list();
		break;
	default:
		fprintf( stderr, "Unknown command to -v option\n");
		usage();
		xcleanup(1);
	}

	while (1) {
		printf("Command? (d FILE, a UNIX_FILE FILE, c UNIX_FILE FILE, l or q) ");
		gets(buf);
		if (*buf == 0)
			continue;

		switch (buf[0]) {
		case 'l':
			vd_list();
			break;

		case 'd':
			for (cp = &buf[1]; isspace(*cp); cp++)
				continue;
			vd_delete(cp);
			break;

		case 'a':
		case 'c':
			for (cp = &buf[1]; isspace(*cp); cp++)
				continue;
			unix_file = cp;
			while (*cp && !isspace(*cp))
				cp++;
			*cp++ = 0;
			while (isspace(*cp))
				cp++;
			dvh_file = cp;
			if (buf[0] == 'c')
				vd_creat(unix_file, dvh_file, 1);
			else
				vd_creat(unix_file, dvh_file, 0);
			break;

		case 'q':
			return;

		default:
			printf("Illegal command : %s\n", buf);
			break;
		}
	}
}

/*
 * list volume directory contents
 */
vd_list()
{
	register int i;

	printf("\ncurrent contents:\n");
	printf("\tfile\tlen\tlbn\n");
	for (i = 0; i < NVDIR; i++) {
		if (vh.vh_vd[i].vd_nbytes) {
			printf("\t%s:\t%d\t%d\n", vh.vh_vd[i].vd_name,
			    vh.vh_vd[i].vd_nbytes, vh.vh_vd[i].vd_lbn);
		}
	}
}

/*
 * delete a volume directory entry
 */
vd_delete(dvh_file)
register char *dvh_file;
{
	register int i;

	for (i = 0; i < NVDIR; i++) {
		if (vh.vh_vd[i].vd_nbytes
		    && streq(vh.vh_vd[i].vd_name, dvh_file)) {
			vh.vh_vd[i].vd_nbytes = 0;
			changed_vh = 1;
			return;
		}
	}
	if (i >= NVDIR) {
		printf("%s not found\n", dvh_file);
	}
}

/*
 * create an entry in the volume directory
 */
vd_creat(unix_file, dvh_file, creat)
register char *unix_file;
register char *dvh_file;
int creat;
{
	register int i;
	register int empty;
	register int bytes;

	if (!*unix_file || !*dvh_file) {
	    if( interactive){
		if (creat)
			printf("syntax: c UNIX_FILE DVH_FILE\n");
		else
			printf("syntax: a UNIX_FILE DVH_FILE\n");
	    }else
		usage();
	    return(0);
	}
	if (strlen(dvh_file) >= VDNAMESIZE) {
		fprintf( stderr, "Volume directory filenames must be < %d characters\n",
			VDNAMESIZE);
		return(0);
	}
	empty = -1;
	for (i = 0; i < NVDIR; i++) {
		if (empty < 0 && vh.vh_vd[i].vd_nbytes == 0)
			empty = i;
		if (vh.vh_vd[i].vd_nbytes
		    && streq(vh.vh_vd[i].vd_name, dvh_file)) {
			if (creat) {
				empty = i;
				break;
			} else {
				fprintf( stderr, "%s already exists in volume directory\n", dvh_file);
				return(0);
			}
		}
	}
	if (empty == -1) {
		fprintf( stderr, "Volume directory full\n");
		return(0);
	}
	if ((bytes = new_copy(unix_file, dvh_file)) <= 0) {
		return(0);
	}
	strcpy(vh.vh_vd[empty].vd_name, dvh_file);
	vh.vh_vd[empty].vd_nbytes = bytes;
	vh.vh_vd[empty].vd_lbn = -1;
	changed_vh = 1;
	return(1);
}

new_copy(unix_file, dvh_file)
register char *unix_file;
register char *dvh_file;
{
	register int bytes;
	register int ufd, dfd;
	register int cc;
	char buf[DEV_BSIZE];
	char fn[FNLEN];

	if ((ufd = open(unix_file, O_RDONLY)) < 0) {
		perror(unix_file);
		return(-1);
	}
	sprintf(fn, "%s%s.%d", TMPDIR, dvh_file, getpid());
	if ((dfd = open(fn, O_RDWR|O_CREAT|O_TRUNC, 0600)) < 0) {
		perror(fn);
		close(ufd);
		return(-1);
	}
	new_tmpfile(fn);
	bytes = 0;
	while ((cc = read(ufd, buf, sizeof(buf))) > 0) {
		if (write(dfd, buf, cc) != cc) {
			fprintf( stderr, "Write error on tmp file : %s\n", fn);
			perror(fn);
			return(-1);
		}
		bytes += cc;
	}
	if (cc < 0) {
		fprintf( stderr, "Read error on %s\n", unix_file);
		close(dfd);
		close(ufd);
		return(-1);
	}
	close(dfd);
	close(ufd);
	return(bytes);
}

struct sn_table {
	char *string;
	int num;
} pt_types[] = {
	{ "volhdr",	PTYPE_VOLHDR },
	{ "trkrepl",	PTYPE_TRKREPL },
	{ "secrepl",	PTYPE_SECREPL },
	{ "raw",	PTYPE_RAW },
	{ "bsd",	PTYPE_BSD42 },
	{ "sysv",	PTYPE_SYSV },
	{ "volume",	PTYPE_VOLUME },
	{ 0,		0 }
};

/*
 * manipulate partition table
 */
edit_pt(opt, part, blks, first, name)
int opt;
int part;
int blks;
int first;
char *name;
{
	char buf[FNLEN];
	char type[FNLEN];

#ifdef DEBUG
	printf("edit_pt\n");
	return;
#endif

	if( !got_dvh ){
  		printf("Must have a valid volume header.\n");
		return;
	}
	switch (opt) {
	case PT_MODIFY:
		pt_modify(part, blks, first, name);
		return;
	case PT_LIST:
		pt_list();
		return;
	case PT_ASK:
	        pt_list();
		break;
	}

	while (1) {
		printf("Command? (part nblks 1stblk type, l or q) ");
		gets(buf);
		if (*buf == 0)
			continue;
		if (*buf == 'q')
			return;
		if (*buf == 'l') {
			pt_list();
		} else {
			if (sscanf(buf,"%d %d %d %s", &part,&blks,&first,type) 
			    != 4) {
				printf("couldn't scan input\n");
			} else {
				pt_modify(part, blks, first, type);
			}
		}
	}
}

/*
 * modify a partition table entry
 */
pt_modify(i, blks, first, type)
int i;
int blks;
int first;
char *type;
{
	char *num_to_string();
	register int ntype;

	if ((ntype = string_to_num(type, pt_types)) < 0) {
		printf("%s is not a type\n");
		printf("Known types are:\n");
		for (i = 0; num_to_string(i, pt_types); i++)
			printf("\t%s\n", num_to_string(i, pt_types));
		return;
	}
	if (i >= NPARTAB) {
		printf("partition number must be less than %d\n", NPARTAB);
		return;
	}
	vh.vh_pt[i].pt_nblks = blks;
	vh.vh_pt[i].pt_firstlbn = first;
	vh.vh_pt[i].pt_type = ntype;
	changed_vh = 1;
}

/*
 * list partition table 
 */
pt_list()
{
	register int i;

	printf("\ncurrent contents:\n");
	printf("\tpart\tn_blks\t1st_blk\ttype\n");
	for (i = 0; i < NPARTAB; i++) {
		if (vh.vh_pt[i].pt_nblks) {
			printf("\t%d:\t%d\t%d\t%s\n", i, vh.vh_pt[i].pt_nblks,
			    vh.vh_pt[i].pt_firstlbn,
			    num_to_string(vh.vh_pt[i].pt_type, pt_types));
		}
	}
}

string_to_num(cp, sn)
register char *cp;
register struct sn_table *sn;
{
	for (; *sn->string; sn++)
		if (streq(sn->string, cp))
			return(sn->num);
	return(-1);
}

char *
num_to_string(num, sn)
register int num;
register struct sn_table *sn;
{
	for (; sn->string; sn++)
		if (sn->num == num)
			return(sn->string);
	return((char *)0);
}

/*
 * gross kludge -- should do something better
 */
struct dp_table {
	char *dp_string;
	int dp_offset;
	int dp_width;
} dp_table[] = {
	/* chars */
	{ "dp_skew", 		0,	1 },
	{ "dp_gap1", 		1,	1 },
	{ "dp_gap2", 		2,	1 },
	{ "dp_spare0", 		3,	1 },
	/* shorts */
	{ "dp_cyls", 		4,	2 },
	{ "dp_shd0", 		6,	2 },
	{ "dp_trks0", 		8,	2 },
	{ "dp_shd1", 		10,	2 },
	{ "dp_trks1", 		12,	2 },
	{ "dp_secs", 		14,	2 },
	{ "dp_secbytes",	16,	2 },
	{ "dp_interleave", 	18,	2 },
	/* ints */
	{ "dp_flags", 		20,	4 },
	{ "dp_datarate",	24,	4 },
	{ "dp_mspw",		28,	4 },
	{ 0,			0,	0 },
};

/*
 * manipulate device parameters
 */
edit_dp(opt, name, value)
int opt;
char *name;
int value;
{
	register struct dp_table *dp;
	register char *cp;
	char buf[FNLEN];
	char *atob();

#ifdef DEBUG
	printf("edit_dp\n");
	return;
#endif

	if( !got_dvh ){
  		printf("Must have a valid volume header.\n");
		return;
	}
	switch (opt) {
	case DP_MODIFY:
		for (dp = dp_table; dp->dp_string; dp++)
			if (streq(name, dp->dp_string))
				break;
		if (!dp->dp_string)
			printf("unknown field %s\n", name);
		else
			set_dp(dp, value);
		return;

	case DP_LIST:
		dp_list();
		return;
	case DP_ASK:
		dp_list();
		break;
	}

	while (1) {
		printf("Command? (name value, l or q) ");
		gets(buf);
		if (*buf == 0 )
			continue;
		if (*buf == 'q')
			return;
		if (*buf == 'l') {
			dp_list();
		} else {
			for (cp = buf; !isspace(*cp); cp++)
				continue;
			*cp++ = 0;
			while (*cp && isspace(*cp))
				cp++;
			if (*atob(cp, &value)) {
				printf("bad value\n");
			} else {
				for (dp = dp_table; dp->dp_string; dp++)
					if (streq(buf, dp->dp_string))
						break;
				if (!dp->dp_string)
					printf("unknown field %s\n", name);
				else
					set_dp(dp, value);
			}
		}
	}
}

dp_list()
{
	register struct dp_table *dp;

	printf("\ncurrent contents:\n");
	for (dp = dp_table; dp->dp_string; dp++)
		printf("\t%s:\t%d\n", dp->dp_string, get_dp(dp));
}

get_dp(dp)
register struct dp_table *dp;
{
	return(get_memory(((unsigned)&vh.vh_dp)+dp->dp_offset, dp->dp_width));
}

set_dp(dp, value)
register struct dp_table *dp;
register unsigned value;
{
	set_memory(((unsigned)&vh.vh_dp)+dp->dp_offset, dp->dp_width, value);
}

get_memory(addr, width)
register unsigned addr;
register unsigned width;
{
	switch (width) {
	case sizeof(unsigned char):
		return (*(unsigned char *)addr);
	case sizeof(unsigned short):
		return (*(unsigned short *)addr);
	case sizeof(unsigned):
		return (*(unsigned *)addr);
	default:
		printf("Internal error: get_memory\n");
		xcleanup(1);
	}
}

set_memory(addr, width, value)
register unsigned addr;
register unsigned width;
register unsigned value;
{
	switch (width) {
	case sizeof(unsigned char):
		*(unsigned char *)addr = value;
		break;
	case sizeof(unsigned short):
		*(unsigned short *)addr = value;
		break;
	case sizeof(unsigned):
		*(unsigned *)addr = value;
		break;
	default:
		printf("Internal error: set_memory\n");
		xcleanup(1);
	}
	changed_vh = 1;
}

putdvh()
{
	register int i;
	register struct partition_table *pt;
	register struct volume_directory *vd;
	register struct partition_table *vhpt;
	register int errflg;
	register int fd;
	register int tfd;
	register int bytes;
	register int wroteit;
	char buf[DEV_BSIZE];

#ifdef DEBUG
	printf("putdvh - Volume is %s\n", Volume);
	return(1);
#endif

	/*
	 * validate consistency of volume header
	 *	partition table should have entry for volume header
	 */
	errflg = 0;
	vhpt = 0;
	if(!got_dvh){
	    fprintf( stderr, "No volume header was read in, so one isn't being written.\n");
	    return(0);
	}
	for (i = 0; i < NPARTAB; i++) {
		pt = &vh.vh_pt[i];
		if (!vhpt && pt->pt_nblks && pt->pt_type == PTYPE_VOLHDR)
			vhpt = pt;
		if (pt->pt_firstlbn % (vh.vh_dp.dp_secs * vh.vh_dp.dp_trks0)) {
		    if (pt->pt_firstlbn != -1)
			fprintf( stderr, "warning: partition %d not cylinder aligned\n",
			    i);
		}
	}
	if (!vhpt) {
		fprintf( stderr, "partition table: no entry for volume header\n");
		return(0);
	}
	
	/*
	 * assign space to files within volume directory, check that
	 * enough space exists in partition
	 */
	i = vh.vh_dp.dp_trks0 * vh.vh_dp.dp_secs;
	initvdspace(i, vhpt->pt_nblks - i);
	for (vd = vh.vh_vd; vd < &vh.vh_vd[NVDIR]; vd++) {
		if (vd->vd_nbytes) {
			vd->vd_lbn = getvdspace(vd->vd_nbytes);
			if (vd->vd_lbn == -1) {
				fprintf( stderr, "no space for %s\n", vd->vd_name);
				errflg++;
			}
		}
	}
	if (errflg ){
		if( interactive) {
		    printf("write bad volume header? [n] ");
		    gets(buf);
		    if (*buf != 'y')
			return(0);
		}else{
		    fprintf( stderr, "Not writing out the bad volume header.\n");
		    return(0);
		}
	}

	/*
	 * write volume header out to each track of cyl 0
	 */
	if (interactive) {
		if(!changed_vh){
		    printf("Volume header wasn't modified.\n");
		    printf("Do you still want to write it out? [n] ");
		    gets(buf);
		    if( *buf != 'Y' && *buf != 'y')
			return(0);
		}
again:
		printf("Volume? (%s)", Volume);
		gets(buf);
		if (*buf)
			strcpy(Volume, buf);
	    if ((fd = open(Volume, O_RDWR|O_CREAT|O_TRUNC)) < 0) {
		perror(Volume);
		goto again;
	    }
	}else{
	    if ((fd = open(Volume, O_RDWR|O_CREAT|O_TRUNC)) < 0) {
		perror(Volume);
		xcleanup(1);
	    }
	}


	/*
	 * write out contents of files referenced volume directory
	 */
	for (vd = vh.vh_vd; vd < &vh.vh_vd[NVDIR]; vd++) {
		char fn[FNLEN];

		if (!vd->vd_nbytes || vd->vd_lbn == -1)
			continue;
		if (lseek(fd, vd->vd_lbn * DEV_BSIZE, 0) < 0) {
			fprintf( stderr, "Seek to sector %d failed.  Can't write out %s.\n", vd->vd_lbn, vd->vd_name);
			continue;
		}
		sprintf(fn, "%s%s.%d", TMPDIR, vd->vd_name, getpid());
		if ((tfd = open(fn, O_RDONLY)) < 0) {
			perror(fn);
			continue;
		}
		bytes = 0;
		while ((i = read(tfd, buf, sizeof(buf))) > 0){
			if (write(fd,  buf, i) != i){
				fprintf( stderr, "Write error of %s at %d\n", fn, bytes);
				close(tfd);
				continue;
			}else
				bytes += i;
		}
		if (i != 0){
			fprintf( stderr, "Read error on %s\n", fn);
			perror(fn);
		}
		if (bytes != vd->vd_nbytes) {
			fprintf( stderr, "%s changed length. Was  0x%x bytes, is now 0x%x\n", vd->vd_name, vd->vd_nbytes, bytes);
			vd->vd_nbytes = bytes;
		}
		close(tfd);
	}
	/*
	 * calculate vh_csum
	 */
	vh.vh_magic = VHMAGIC;
	vh.vh_csum = 0;
	vh.vh_csum = vh_checksum(&vh);
	/*
	 * write out volume header to first sector of each track of cyl 0
	 */
	wroteit = 0;
	for (i = 0; i < vh.vh_dp.dp_trks0; i++) {
	    if (lseek(fd, i * vh.vh_dp.dp_secs * DEV_BSIZE, 0) < 0) {
		fprintf( stderr, "Seek to track %d failed while writing out volume header.\n", i);
		continue;
	    }
	    if (write(fd, &vh, DEV_BSIZE) != DEV_BSIZE)
		printf("Write to track %d failed while writing out volume header\n", i);
	    else
		wroteit++;
	}
	if( wroteit && interactive)
    	  printf("Volume header successfully written to %d tracks\n", wroteit);
	changed_vh = 0;
	close(fd);
}

vh_checksum(vhp)
register struct volume_header *vhp;
{
	register int csum;
	register int *ip;

	csum = 0;
	for (ip = (int *)vhp; ip < (int *)(vhp+1); ip++)
		csum += *ip;
	return(-csum);
}

#define NVDMAP	50

struct vd_map {
	int vm_first;	/* first free item */
	int vm_cnt;	/* # of contiguous free elements, 0 => end of list */
} vd_map[NVDMAP];

initvdspace(first, cnt)
{
	/*
	 * this should scan the bst_table and mark busy all defective
	 * sectors
	 *
	 * for now, just free everything after cyl 0 in the volume header
	 * partition
	 */
	if (cnt < 0)
		cnt = 0;
	vd_map[0].vm_first = first;
	vd_map[0].vm_cnt = cnt;
	vd_map[1].vm_cnt = 0;
}

getvdspace(bytes)
{
	register struct vd_map *vm, *vm1;
	register int blks;
	register int first;

	blks = (bytes+DEV_BSIZE-1)/DEV_BSIZE;
	for (vm = vd_map; vm->vm_cnt; vm++)
		if (blks <= vm->vm_cnt) {
			first = vm->vm_first;
			vm->vm_first += blks;
			vm->vm_cnt -= blks;
			if (vm->vm_cnt == 0)
				for (vm1 = vm+1; vm1->vm_cnt; vm1++, vm++)
					vm = vm1;
			return(first);
		}
	return(-1);
}

min(a,b)
{
	return(a<b?a:b);
}

unsigned digit();

/*
 * atob -- convert ascii to binary.  Accepts all C numeric formats.
 */
char *
atob(cp, iptr)
register char *cp;
register int *iptr;
{
	register unsigned d;
	register int value = 0;
	register unsigned base = 10;
	register int minus = 0;

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
		value *= base;
		value += d;
		cp++;
	}

	if (minus)
		value = -value;

	*iptr = value;
	return(cp);
}

/*
 * digit -- convert the ascii representation of a digit to its
 * binary representation
 */
unsigned
digit(c)
register char c;
{
	register unsigned d;

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

change_bootname (p)
	register char *p;
{
	char stk_buf[80];

#ifdef DEBUG
	if (p == 0) {
		printf("get_bootname - interactive\n");
	} else {
		printf("get_bootname - given %s\n", p);
	}
	return;
#endif

	if(!got_dvh ){
	    if( interactive ){
	        printf("Must have a valid volume header before changing the boot file name.\n");
	        return;
	    }else{
	       fprintf( stderr, "Can't change boot file name since volume header was invalid.\n");
	       xcleanup(1);
	    }

	}
	if ( interactive ) {
		printf ("Current Boot file name is %s\n", 
			vh.vh_bootfile[0] == 0 ? "NULL" : vh.vh_bootfile);
		printf ("Enter name up to %d characters: ", BFNAMESIZE);
		gets (stk_buf);
		if (*stk_buf != 0) {
			strncpy (vh.vh_bootfile, stk_buf, BFNAMESIZE);
		}
	}
	else {
		strncpy (vh.vh_bootfile, p, BFNAMESIZE);
	}
	changed_vh = 1;
}
char *
rawname(cp)
	char *cp;
{
	static char rawbuf[MAXPATHLEN];
	char *dp2, *dp = strchr(cp, '/');

	if (dp == 0)
		return (0);
	if (dp == cp) {					/* initial / */
		if ((dp2 = strchr(dp+1, '/')) != 0)
			dp = dp2;
	}
	*dp = 0;
	(void)strcpy(rawbuf, cp);
	*dp = '/';
	(void)strcat(rawbuf, "/r");
	(void)strcat(rawbuf, dp+1);
	return (rawbuf);
}
