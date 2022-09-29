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
#ident	"$Header: diskmap.c,v 1.6.3.3.1.4 90/07/12 14:31:37 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dvh.h>
#include <errno.h>
#include <sys/sysmacros.h>
#include <sys/dkio.h>
#include <sun/mntent.h>
#include <sys/param.h>

/*
 * Definitions.
 */
#define	reg	register		/* Convenience */
#define	uint	unsigned int		/* Convenience */
#define	ulong	unsigned long		/* Convenience */
#define	ushort	unsigned short		/* Convenience */
#define	FSTABSZ	1024			/* Fstab size limit (plus one) */

/*
 * External functions.
 */
void	exit();
char	*strcat();
char	*strchr();
char	*strcpy();
char	*strrchr();
char	*strtok();

/*
 * Internal functions.
 */
int	getopt();
int	prtvtoc();
void	puttable();
void	usage();

/*
 * External variables.
 */
extern int	errno;			/* System error code */
extern	stat();


/*
 * Static variables.
 */
static char	*fstab = "/etc/fstab";	/* Fstab pathname */
static char	*myname;		/* Last qualifier of arg0 */
static int	optind = 1;		/* Argument index */
static char	*optarg;		/* Option argument */

char	bptr[128];
char	rptr[128];
char	nptr[128];
struct	stat	statbuf;
struct	stat	sb;

int	root_dev;			/* device associated with /dev/root */
int	usr_dev;			/* device associated with /dev/usr */

int	part_flag[NPARTAB];		/* hit flags */
int	hit_flag[NPARTAB];
int	skip_flag[NPARTAB];		/* skip display flags */
int	show_map = 0;
int	avail_only = 0;
int	num_avail = 0;
int	size_only = 0;
int	ignore = 0;

char	str_array[NPARTAB][128];	/* string building arrays */

/*
 *
 *	MAIN
 *
 */

main(ac, av)
int		ac;
reg char	**av;
{
	reg int		idx;

	if (myname = strrchr(av[0], '/'))
		++myname;
	else
		myname = av[0];

	while ((idx = getopt(ac, av, "adims:t:")) != -1)
		switch (idx) {
		case 'a':
			avail_only = 1;
			break;
		case 'd':
			size_only = 1;
			break;
		case 'i':
			ignore = 1;
			break;
		case 'm':
			show_map = 1;
			break;
		case 's':
			skip_flag[atoi(optarg)] = 1;
			break;
		case 't':
			fstab = optarg;
			break;
		default:
			usage();
		}

	/* check if no name was supplied and fabricate one */
	if(optind >= ac)
	{
		if((idx = open("/dev/rdsk/ipc0d0s0",0)) > 0)
		{
			close(idx);
			exit(prtvtoc("/dev/rdsk/ipc0d0s0"));
		}

		if((idx = open("/dev/rdsk/isc0d0s0",0)) > 0)
		{
			close(idx);
			exit(prtvtoc("/dev/rdsk/isc0d0s0"));
		}

		if((idx = open("/dev/rdsk/ips0d0s0",0)) > 0)
		{
			close(idx);
			exit(prtvtoc("/dev/rdsk/ips0d0s0"));
		}

		if((idx = open("/dev/rdsk/sdc0d0s0",0)) > 0)
		{
			close(idx);
			exit(prtvtoc("/dev/rdsk/sdc0d0s0"));
		}

		usage();
	}
	idx = 0;
	while (optind < ac)
		idx |= prtvtoc(av[optind++]);

	fflush(stdout);
	fflush(stdout);
	exit(idx);
	/* NOTREACHED */
}

/*
 * getopt()
 *
 * Parse options. Stolen from libc, with changes
 * to avoid standard I/O.
 */
static int
getopt(ac, av, options)
int		ac;
reg char	**av;
char		*options;
{
	reg int		c;
	reg char	*cp;
	static int	sp = 1;

	if (sp == 1)
		if (optind >= ac
		    || av[optind][0] != '-' || av[optind][1] == '\0')
			return (-1);
		else if (strcmp(av[optind], "--") == 0) {
			optind++;
			return (-1);
		}
	c = av[optind][sp];
	if (c == ':' || (cp = strchr(options, c)) == 0)
		usage();
	if (*++cp == ':') {
		if (av[optind][sp+1] != '\0')
			optarg = &av[optind++][sp+1];
		else if (++optind >= ac)
			usage();
		else
			optarg = av[optind++];
		sp = 1;
	} else {
		if (av[optind][++sp] == '\0') {
			sp = 1;
			optind++;
		}
		optarg = 0;
	}
	return (c);
}

/*
 * prtvtoc()
 *
 * Read and print a VTOC.
 */
static int
prtvtoc (name)
char		*name;
{
	reg int		fd;
	reg int		idx;
	struct volume_header vh;
	struct io_arg	ia;

	/* check if device exists */
	if (stat(name, &sb) < 0) return (perror(name));

	/* open the raw device */
	if ((fd = open(name, O_RDONLY)) < 0) return (perror(name));

	/* get the volume header */
	ia.retval = ia.sectst = 0;
	ia.memaddr = (unsigned long)&vh;
	ia.datasz = sizeof(vh);
	if (ioctl(fd, DIOCGETVH, &ia) < 0)
	{
		(void) close(fd);
		return (fprintf(stderr,"%s : Can't read volume header",name));
	}

	(void) close(fd);

	/* validate the header */
	if (vh.vh_magic != VHMAGIC)
	{
		return (fprintf(stderr,
		"%s : Does not contain a valid volume header",name));
	}

	/* process the table */
	puttable(&vh, name);
	fflush(stdout);
	fflush(stdout);

	/* if available flag the return num avail */
	if(avail_only)
		return(num_avail);
	else
		return (0);
}


/*
 * determine if n is within t
 */

within(nb,ne,tb,te)
unsigned long nb,ne,tb,te;
{
	/* check if same begining */
	if(nb == tb) return(1);

	/* check if same end */
	if(ne == te) return(1);

	/* check if within partition */
	if((nb > tb) && (nb < te)) return(1);
	if((ne > tb) && (ne < te)) return(1);

	if((tb > nb) && (tb < ne)) return(1);
	if((te > nb) && (te < ne)) return(1);

	return(0);	/* default is miss */
}


/*
 * check partition collisions
 */

collide(vh,num)
	register struct volume_header *vh;
	int num;
{
	unsigned long num_begin, tst_begin;
	unsigned long num_end, tst_end;
	register struct partition_table *part;
	register int	idxx;
	int ret = -1;

	/* determine begin and end of our partition */
	part = &vh->vh_pt[num];
	num_begin = (part->pt_firstlbn * 512);
	num_end   = (num_begin + (part->pt_nblks * 512));

	/* now look for a collision */
	for(idxx = 0,part = vh->vh_pt; part < &vh->vh_pt[NPARTAB];part++,idxx++)
	{
		/* ignore self */
		if(idxx == num) continue;

		/* ignore empty partitions */
		if(part->pt_nblks <= 0) continue;

		/* ignore partitions that are un-occupied */
		if(part_flag[idxx] < 1) continue;

		/* get begin and end of test partition */
		tst_begin = (part->pt_firstlbn * 512);
		tst_end   = (tst_begin + (part->pt_nblks * 512));

                /* printf("test %d vs. %d\n",num,idxx); */

		/* test for collisions */
		if(within(num_begin,num_end,tst_begin,tst_end))
		{
                        /* printf("collide\n"); */
			hit_flag[num] = 1;
		}
	}
}


/*
 * puttable()
 *
 * Print a human-readable VTOC.
 */
static void
puttable (vh, name)
	register struct volume_header *vh;
	char *name;
{
	register struct partition_table *part;
	register int	idx;
	register int	col;
	int 		i;
	int		len;

	/* convert the raw device name to a block dev name */
	len = strlen(name); name[len -1] = 0;
	strcpy(nptr,name);
	for(idx=5; idx < len; idx++) name[idx] = name[idx+1];

	/* look for device name aliases here */
	/* might need to look for some more also */
	root_dev = getdevnum("/dev/root");
	usr_dev  = getdevnum("/dev/usr");

	for(idx = 0,part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; part++,idx++)
	{
		if(part->pt_nblks <= 0) continue;

		/* never use partitions */
		if(idx == 8) continue;
		if(idx == 9) continue;
		if(idx == 10) continue;

		/* process only user partitions and sizes */
		if(part->pt_type == 4)
		{
		    /* print partition name and size in megs */
		    sprintf(&str_array[idx][0],"%-2d  %s%-2d\t%3d\t",
		    idx,name,idx,(((part->pt_nblks * 512) / 1048576) +1));

		    /* determine if a file system is on this partition */
		    sprintf(bptr,"%s%d",name,idx);
		    sprintf(rptr,"%s%d",nptr,idx);
		    if(find_entry(bptr,rptr,idx))
			part_flag[idx] = 0;	/* not found */
		    else
			part_flag[idx] = 1;	/* found */
		}
	}

	/* scan the hit map for partition conflicts */
	for(idx=0; idx < NPARTAB; idx++)
	{
		/* clear hit flag */
		hit_flag[idx] = 0;

		/* check this partition for a collision with another */
		/* mark it available if no hits */
		collide(vh,idx);
	}

	/* display the partition mapping */
	if(show_map)
	{
		printf ("-----------------------------------------------------------------------\n");
		showmap();
	}

	printf ("-----------------------------------------------------------------------\n");
	printf ("Partition\t\tMegs\tMounted File System or Partition Usage\n");
	printf ("-----------------------------------------------------------------------\n");

	/* check if should skip over used ones and just print available */
	if(size_only) goto szonly;
	if(avail_only) goto skip_used;

	/* print partitions being used and or designated */
	for(idx=0; idx < NPARTAB; idx++)
	{
		/* check if this one has a file system or is in use */
		if(skip_flag[idx]) continue;
		if(part_flag[idx]) printf("%s\n",&str_array[idx][0]);
	}

skip_used:

	/* print available partitions */
	for(idx=0; idx < NPARTAB; idx++)
	{
		if(skip_flag[idx]) continue;
		/* show this if it is really available */
		if((part_flag[idx] == 0) && (hit_flag[idx] == 0)
		 && (strlen(&str_array[idx][0]) > 1))
		{
			printf("%s -**** Available Partition ****-\n",
			&str_array[idx][0]);
			num_avail++;
		}
	}

szonly:

	printf ("-----------------------------------------------------------------------\n");
	printf ("Disk Device %s Megabytes Total Size\n",&str_array[2][1]);
	printf ("-----------------------------------------------------------------------\n");
	fflush(stdout);
	fflush(stdout);
}

/*
 * usage()
 *
 * Print a helpful message and exit.
 */
static void
usage()
{
    fprintf (stderr,
    "Usage: %s [- a][-d][-i][-m][-s skip_part_num][-t fstab] rawdisk ...\n",
    myname);

    exit(1);
}

/*
 *
 *	get the device number for this path
 *
 */

getdevnum(name)
char *name;
{
int err;

	/* get some stats */
	err = stat(name,&statbuf);

	if(err < 0)
		return(-1);
	else
		return(statbuf.st_rdev);
}


/*
 *
 *	map partition number to file system if exists
 *
 */

find_entry(xnptr,xrptr,idx)
char *xnptr, *xrptr;
int idx;
{
int miss, len;
char *endptr;

	FILE *mtabp;
	struct mntent *mntp;
	char *colon;	/* end of hostname: in NFS mount */

	if((mtabp = setmntent("/etc/fstab","r")) == NULL)
	{
		perror("/etc/fstab"); exit(-1);
	}

	miss = 1;

	/* get the end of the string printing buffer */
	len = strlen(&str_array[idx][0]);
	endptr = &str_array[idx][len];

	/* look at every entry in /etc/fstab file */
	while ((mntp = getmntent(mtabp)) != NULL)
	{
		/* check is this entry matches anything */
		if(strcmp(xnptr,mntp->mnt_fsname) == 0)
	    	{
			/* look for swap partition entries and note */
			if(!strncmp("none",mntp->mnt_dir,4))
				sprintf(endptr,"Alternate Swap Partition");
			else
				/* show the file system if any found */
				sprintf(endptr,"%s",mntp->mnt_dir);

			miss = 0;
			break;
		}
	}
	endmntent(mtabp);

	/* check for special cases */
	if(miss)
	{
		if(getdevnum(xrptr) == 0x401)
		{
			sprintf(endptr,"Primary Swap Partition");
			miss = 0;
		}

		if(root_dev == getdevnum(xrptr))
		{
			sprintf(endptr,"/dev/root");
			miss = 0;
		}

		if(usr_dev == getdevnum(xrptr))
		{
		    if(ignore == 0)
		    {
			sprintf(endptr,"/dev/usr");
			miss = 0;
		    }
		}
	}

	return(miss);
}


showmap()
{
printf("-------------- Valid File System Partition Combinations ---------------\n");
#ifdef XXX
printf("[======================= Partition 2 =============================]\n");
#endif
printf("[  ROOT ]*************************************************[ SWAP  ]\n");
printf("          Potential /usr or other file system partitions\n");
printf("[== 0 ==][============== Partition 3 ====================][== 1 ==]\n");
printf("[== 0 ==][===== Part 4 ======][== Part 5 ==][== Part 7 ==][== 1 ==]\n");
printf("[== 0 ==][========== Partition 6 ==========][== Part 7 ==][== 1 ==]\n");
#ifdef XXX
printf("[======= Partition 11 =======][========== Partition 12 ===========]\n");
printf("[== Part 14 =][== Part 15 ===][== Part 5 ==][====== Part 13 ======]\n");
#endif
}

