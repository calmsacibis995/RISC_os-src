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
#ident	"$Header: du.c,v 1.12.1.2 90/05/09 15:46:36 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	du	COMPILE:	cc -O du.c -s -i -o du	*/

/*
**	du -- summarize disk usage
**		du [-Lars] [name ...]
*/

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/param.h>
#include 	<dirent.h>
#include	<sys/statfs.h>

# define c_stat(f,p)	(!Lflag?lstat(f,p):stat(f,p))

#define EQ(x,y)	(strcmp(x,y)==0)
#define BUFSIZE	512	/* logical block */
#define DIRECT	12	/* Number of direct blocks */

struct	stat	Statb;

char	path[1025];

int	aflag = 0;
int	rflag = 0;
int	sflag = 0;
int	Lflag = 0;
long	descend();

main(argc, argv)
char **argv;
{
	long blocks = 0;
	extern int optind;
	int c;

#ifdef STANDALONE
	if (argv[0][0] == '\0')
		argc = getargv("du", &argv, 0);
#endif
	while ((c = getopt(argc, argv, "Lars")) != EOF) {
		switch (c) {
		case 'L':
			Lflag++;
			continue;
		case 'a':
			aflag = 1;
			sflag = 0;
			continue;

		case 'r':
			rflag++;
			continue;

		case 's':
			sflag = 1;
			aflag = 0;
			continue;

		default:
			fprintf(stderr, "usage: du [-L] [-a] [-r] [-s] [name ...]\n");
			exit(2);
		}
	}
	if(argc == optind) {
		path[0] = '.';
		path[1] = '\0';
		blocks = descend(path);
		if(sflag) {
			printf("%ld	%s\n", blocks, path);
		}
	} else {
		while(optind < argc) {
			strcpy(path, argv[optind]);
			blocks = descend(path);
			if(sflag) {
				printf("%ld	%s\n", blocks, path);
			}
			optind++;
		}
	}
#ifdef PRINTTAB
	printtab();
#endif
	exit(0);
}

long descend(name)
char *name;
{
	register DIR 	*dirf;		/* open directory */
	register struct dirent 	*dp;	/* current directory */
	register char	*c1, *c2;
	long blocks = 0;
	int	dir = 0;		/* open directory */
	long	offset, dirsize;
	int	dsize, entries, i;
	char	*endofname;
	struct	statfs fst;
	long nblock();

	if(c_stat(name,&Statb)<0) {
		if(rflag)
			fprintf(stderr, "du: bad status < %s >\n", name);
		return(0);
	}
	if(Statb.st_nlink>1 && (Statb.st_mode&S_IFMT)!=S_IFDIR) {
		if (findlink(Statb.st_dev, Statb.st_ino)) {
			return 0;
		}
	}
	if (statfs(name,&fst,sizeof(fst),0) < 0) {
		if(rflag)
			fprintf(stderr, "du: bad status < %s >\n", name);
		return(0);
	};		
	blocks = nblock(Statb.st_size,fst.f_bsize);

	if((Statb.st_mode&S_IFMT)!=S_IFDIR) {
		if(aflag)
			printf("%ld	%s\n", blocks, name);
		return(blocks);
	}

	for(c1 = name; *c1; ++c1);
	endofname = c1;
#ifdef 0
	if(Statb.st_size > 32000)
		fprintf(stderr, "Huge directory < %s >--call administrator\n", name);
#endif
	if((dirf = opendir(name)) == NULL) {
		if(rflag)
			fprintf(stderr, "du: cannot open < %s >\n", name);
		return(0);
	}
	offset = 0;

	/*
	|| Foreach entry in the current directory ...
	*/
	while((dp = readdir(dirf)) != NULL) {
		
		if(dp->d_ino==0
		   || EQ(dp->d_name, ".")
		   || EQ(dp->d_name, ".."))
			continue;
		if (dp->d_ino == -1)
			continue;
		c1 = endofname;
		if (c1[-1] != '/')
			*c1++ = '/';
		c2 = dp->d_name;
		for(i=0; i<dp->d_reclen; i++)
			if(*c2)
				*c1++ = *c2++;
			else
				break;
		*c1 = '\0';
		if(i == 0) { /* null length name */
			fprintf(stderr,"bad dir entry <%s>\n",dp->d_name);
			return(0);
		}
		/*
		|| Recursively call 'descend' for this entry.  If the
		|| recursion is getting too deep, close the directory
		|| and reopen it upon return to prevent running out
		|| of file descriptors.
		*/
		if(dirf->dd_fd > 10) {
			offset = telldir(dirf);	/* remember current position */
			closedir(dirf);
			dirf = NULL;
		}
		blocks += descend(name);
		if(dirf == NULL) {
			*endofname = '\0';
			if((dirf = opendir(name)) == NULL) {
				if(rflag)
					fprintf(stderr, "du: cannot open < %s >\n", name);
				return(0);
			}
			if(offset) {
				seekdir(dirf, (long)offset);
				offset = 0;
			}
		}
	}
	if(dirf)
		closedir(dirf);
	*endofname = '\0';
	if(!sflag)
		printf("%ld	%s\n", blocks, name);
	return(blocks);
}



long nblock(size,bsize)
long size;
long bsize;
{
	long blocks, tot;
	long nindir = (bsize / sizeof(daddr_t));

	blocks = tot = (size + (BUFSIZE - 1)) / BUFSIZE;
	if(blocks > DIRECT)
		tot += ((blocks - DIRECT - 1) / nindir) + 1;
	if(blocks > DIRECT + nindir)
		tot += ((blocks - DIRECT - nindir - 1) / (nindir * 2)) + 1;
	if(blocks > DIRECT + nindir + nindir*nindir)
		tot++;
	return(tot);
}

/*
 * The link table is implemented as follows:
 *
 * The table consists of HTSIZE hash buckets.  Each hash bucket
 * is a sorted linked list of entries, sorted by device number.
 * Each entry contains NINO inode numbers.  Entries are added to
 * the beginning of the list segment for that device number, because
 * it is likely that links will appear in the same directory.
 *
 * As an example, a bucket might look like this:
 *
 *	Device 1234; Inodes: 300 2467 8813
 *	Device 8893; Inodes: 12 86 100 ... 153
 *	Device 8893; Inodes: 12345 87
 *	...
 *
 * Note: Inode 0 is not expected to be a valid inode number.  This is
 *       usually true, but if it isn't, new entries will need to
 *       be filled with -1s and the code changed to check for that
 *       value.
 */

#define NINO	20	/* Number of inodes per entry */
#define HTSIZE	197	/* Number of hash table entries */

struct linktab {
	dev_t	lt_dev;
	ino_t	lt_ino[NINO];
	struct linktab *next;
};

struct linktab *Link_heads[HTSIZE];
static struct linktab *new_ent();

int
findlink(ldev, lino)
	dev_t ldev;
	ino_t lino;
{
	struct linktab *prev;
	struct linktab *cur;
	struct linktab *new;
	int htent;
	int count;

	/*
	 * Hash function.  Simple, but effective.
	 */

	htent = (ldev + lino) % HTSIZE;

	/*
	 * First case: List is empty or first part of list is for the
	 * given device.
	 */

	cur = Link_heads[htent];
	if (cur == NULL || ldev <= cur->lt_dev) {
		while (cur && ldev == cur->lt_dev) {
			count = 0;
			while (count < NINO) {
				if (cur->lt_ino[count] == 0) {
					cur->lt_ino[count] = lino;
					return 0;
				}
				if (cur->lt_ino[count] == lino) {
					return 1;
				}
				count++;
			}
			cur = cur->next;
		}
		new = new_ent();
		new->lt_dev = ldev;
		new->lt_ino[0] = lino;
		new->next = Link_heads[htent];
		Link_heads[htent] = new;
		return 0;
	}

	/*
	 * Second case: Have to search down the list until we find
	 * the end of the set of entries before the proper device.
	 */

	prev = cur;
	cur = cur->next;
	while (cur && ldev > cur->lt_dev) {
		prev = cur;
		cur = cur->next;
	}
	while (cur && ldev == cur->lt_dev) {
		count = 0;
		while (count < NINO) {
			if (cur->lt_ino[count] == 0) {
				cur->lt_ino[count] = lino;
				return 0;
			}
			if (cur->lt_ino[count] == lino) {
				return 1;
			}
			count++;
		}
		cur = cur->next;
	}
	new = new_ent();
	new->lt_dev = ldev;
	new->lt_ino[0] = lino;
	new->next = prev->next;
	prev->next = new;
	return 0;
}

/*
 * Allocate a new link table entry.
 */

static struct linktab *
new_ent()
{
	struct linktab *new;

	new = (struct linktab *)calloc(1, sizeof(struct linktab));
	if (new == NULL) {
		fprintf(stderr, "du: Out of memory\n");
		exit(1);
	}
	return new;
}

#ifdef PRINTTAB
/*
 * Print out the contents of the link table.
 */

printtab()
{
	int tent;
	int count;
	struct linktab *cur;

	printf("\t--- Link table contents ---\n");
	tent = 0;
	while (tent < HTSIZE) {
		printf("List %d\n", tent);
		cur = Link_heads[tent];
		while (cur) {
			printf("\tDevice: %d\n\tInodes: ", cur->lt_dev);
			count = 0;
			while (count < NINO && cur->lt_ino[count]) {
				printf("%d ", cur->lt_ino[count]);
				count++;
			}
			printf("\n");
			cur = cur->next;
		}
		tent++;
	}
}
#endif
