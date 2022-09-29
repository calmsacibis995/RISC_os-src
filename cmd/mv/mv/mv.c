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
#ident	"$Header: mv.c,v 1.13.2.3 90/05/09 16:52:31 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Combined mv/cp/ln command:
 *	mv file1 file2
 *	mv dir1 dir2
 *	mv file1 ... filen dir1
 */



#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<errno.h>

#define FTYPE(A)	(A.st_mode)
#define FMODE(A)	(A.st_mode)
#define	IDENTICAL(A,B)	(A.st_dev==B.st_dev && A.st_ino==B.st_ino)
#define ISBLK(A)	((A.st_mode & S_IFMT) == S_IFBLK)
#define ISCHR(A)	((A.st_mode & S_IFMT) == S_IFCHR)
#define ISDIR(A)	((A.st_mode & S_IFMT) == S_IFDIR)
#define ISFIFO(A)	((A.st_mode & S_IFMT) == S_IFIFO)
#define ISREG(A)	((A.st_mode & S_IFMT) == S_IFREG)

#define BLKSIZE	4096
#define	DOT	"."
#define	DELIM	'/'
#define EQ(x,y)	!strcmp(x,y)
#define	FALSE	0
#define MODEBITS 07777
#define TRUE 1

extern void perror();
extern int errno;

char	*malloc();
char	*dname();
char	*strrchr();
int	utime();
extern	int errno;
extern  char *optarg;
extern	int optind, opterr;
struct stat s1, s2;
int cpy = FALSE;	
int mve = FALSE;	
int lnk = FALSE;	
int symlnk = FALSE;
int	dopipe = FALSE;
char	*cmd;
int	silent = 0;

char Errbuf[1024];	/* for building error messages */

main(argc, argv)
register char *argv[];
{
	register int c, i, r, errflg = 0;
	
	/*
	 * Determine command invoked (mv, cp, or ln) 
	 */
	
	if (cmd = strrchr(argv[0], '/')) {
		++cmd;
	} else {
		cmd = argv[0];
	}
	
	/*
	 * Set flags based on command.
	 */
	 
	if (EQ(cmd, "mv")) {
		mve = TRUE;
	} else if (EQ(cmd, "ln"))  {
		lnk = TRUE;
	} else {
		cpy = TRUE;   /* default */
	}
	
	/*
	 * Check for options:
	 * 	cp [-p] file1 [file2 ...] target
	 *	ln [-f] file1 [file2 ...] target
	 *	mv [-fp] file1 [file2 ...] target
	 *	mv [-fp] dir1 target
	 */
	 
	if (cpy) {
		while ((c = getopt(argc, argv,"-p")) != EOF) 
	 	switch(c) {
			case 'p':
				dopipe = TRUE;
				break;

			default:
				errflg++;
		}
	} else {

		while ((c = getopt(argc, argv,"fsp")) != EOF) 
	 	switch(c) {
			case 'f':
				silent++;
				break;
			case 's':
				if (! lnk) {
					errflg++;
					break;
				}
				symlnk = TRUE;
				break;
			case 'p':
				if (lnk) {
					errflg++;
					break;
				}
				dopipe = TRUE;
				break;

			default:
				errflg++;
		}
	}
	
	/*
	 * Check for sufficient arguments 
	 * or a usage error.
	 */

	argc -= optind;	 
	argv  = &argv[optind];
	
	if (argc < 2) {
		fprintf(stderr,"%s: Insufficient arguments (%d)\n",cmd,argc);
		usage();
	}
	
	if (errflg != 0)
		usage();
	/*
	 * If there is more than a source and target,
	 * the last argument (the target) must be a directory
	 * which really exists.
	 */
	 
	if (argc > 2) {
		if (stat(argv[argc-1], &s2) < 0) {
			sprintf(Errbuf, "%s: %s", cmd, argv[argc-1]);
			perror(Errbuf);
			exit(2);
		}
		
		if (!ISDIR(s2)) {
			fprintf(stderr,"%s: Target must be directory\n",cmd);
			usage();
		}
	}	
	
	/*
	 * Perform a multiple argument mv|cp|ln by
	 * multiple invocations of move().
	 */
	 
	r = 0;
	for (i=0; i<argc-1; i++)
		r += move(argv[i], argv[argc-1]);
	
	/* 
	 * Show errors by nonzero exit code.
	 */
	 
	 exit(r?2:0);
}

move(source, target)
char *source, *target;
{
	register last, c, i;
	char	*buf = (char *)NULL;
	int from, to, ct, oflg;
	int docpy;
	char fbuf[BLKSIZE];
	
	struct	utimbuf	{
		time_t	actime;
		time_t	modtime;
		};
	struct utimbuf *times;

	/* 
	 * While source or target have trailing 
	 * DELIM (/), remove them (unless only "/")
	 */

	while (((last = strlen(source)) > 1)
	    &&  (source[last-1] == DELIM))
		 source[last-1]=NULL;
	
	while (((last = strlen(target)) > 1)
	    &&  (target[last-1] == DELIM))
		 target[last-1]=NULL;
	
	/*
	 * Make sure source file exists.  Not needed for symlinks.
	 */
	if (!symlnk && stat(source, &s1) < 0) {
		sprintf(Errbuf, "%s: %s", cmd, source);
		perror(Errbuf);
		return(1);
	}

	/*
	 * FIFO files are a problem, so unless -p is given, ignore
	 * any FIFO files.
	 */

	if (!lnk && !dopipe) {
		if (ISFIFO(s1)) {
			fprintf(stderr, "%s : <%s> FIFO file\n", cmd, source);
			return(1);
		}
	}

	/* 
         * Make sure source file is not a directory,
	 * we don't move() directories...
	 */
	 /* Unless making symbolic link, or mv */

	 if (!(symlnk || mve)) {
	    if (ISDIR(s1)) {
		fprintf(stderr, "%s : <%s> directory\n", cmd, source);
		return(1);
	    }
	}
	
	/*
	 * If it is a move command and we don't have write access 
	 * to the source's parent then goto s_unlink for the error
	 * message.
	 */

	if (mve) {
		struct stat spd;	/* stat of source's parent */
		int uid;		/* real uid */
		if (accs_parent(source, 2, &spd) == -1)
			goto s_unlink;
	
		/*
		 * If sticky bit set on source's parent, then move only when :
		 * superuser, source's owner, parent's owner, or source is writable.
		 * Otherwise, we won't be able to unlink source later and will be
		 * left with two links and an error exit from mv.
		 */
		if (spd.st_mode&S_ISVTX && (uid=getuid()) != 0 &&
		   s1.st_uid != uid && spd.st_uid != uid &&
		   access(source, 2)<0) {
			errno = EACCES;
			goto s_unlink;
		}
	}
	
	/*
	 * If stat fails, then the target doesn't exist,
	 * we will create a new target with default file type of regular.
 	 */	
	
	FTYPE(s2) = S_IFREG;
	
	if (stat(target, &s2) >= 0) {
		
		/*
		 * If target is a directory,
		 * make complete name of new file
		 * within that directory.
		 */

		if (ISDIR(s2)) {
			if ((buf = malloc(strlen(target) + strlen(dname(source)) + 4)) == NULL) {
				fprintf(stderr,"%s: Insufficient memory to %s %s\n ",cmd,cmd,source);
				exit(3);
			}
			sprintf(buf, "%s/%s", target, dname(source));
			target = buf;
		}
		
		/*
		 * If filenames for the source and target are 
		 * the same and the inodes are the same, it is
		 * an error.
		 */
		
		if (stat(target, &s2) >= 0) {
			if (IDENTICAL(s1,s2)) {
				fprintf(stderr, "%s: %s and %s are identical\n", cmd, source, target);
				if (buf != NULL)
					free(buf);
				return(1);
			}
			
			/*
			 * Because copy doesn't unlink target,
			 * treat it separately.
			 */
			
			if(cpy)
				goto skip;
			
			/* 
			 * Prevent super-user from overwriting a directory
			 * structure with file of same name.
			 */
			 
			 if (mve && ISDIR(s2)) {
				fprintf(stderr, "%s: Cannot overwrite directory %s\n", cmd, target);
				if (buf != NULL)
					free(buf);
				return(1);
			}
			
			/*
			 * If the user does not have access to
			 * the target, ask him----if it is not
			 * silent and user invoked command 
			 * interactively.
			 */
			
			if (access(target, 2) < 0 
			 && isatty(fileno(stdin))
			 && !silent) {
				fprintf(stderr, "%s: %s: %o mode? ", cmd, target,
					FMODE(s2) & MODEBITS);
			
				/* Get response from user. Based on
				 * first character, make decision.
				 * Discard rest of line.
				 */
				
				i = c = getchar();
				while (c != '\n' && c != EOF)
					c = getchar();
				if (i != 'y') {
					if (buf != NULL)
						free(buf);
					return(1);
				}
			}
			
			/*
			 * Attempt to unlink the target unless we are moving,
			 * or if the target is a directory.
			 */
			 if (mve || !ISDIR(s2)) {
				if (unlink(target) < 0) {
					sprintf(Errbuf, "%s: %s", cmd, target);
					perror(Errbuf);
					if (buf != NULL)
						free(buf);
					return(1);
				}
			}
		}
	}
skip:
	/* 
	 * Either the target doesn't exist,
	 * or this is a copy command ...
	 */
	 
	docpy = TRUE;
	if (symlnk){
	    if ( symlink(source, target) < 0) {
		sprintf(Errbuf, "%s: Symbolic link %s to %s", cmd, source,
			target);
		perror(Errbuf);
		return(1);
	    }
	    else
		docpy = FALSE;
	}

	if (mve)
            /* can't rename directories across devices, for example 
	     * but we want to fall through and do the copy for a 
	     * simple file
	     */
	    if (rename (source, target)< 0) {
		if (errno == EXDEV) {
		    if (ISDIR(s1)) {
			fprintf(stderr,"%s: different file system\n",cmd);
			return (1);
		    }
	        }
		else {
		    sprintf(Errbuf, "%s: rename %s to %s", cmd, source, target);
		    perror(Errbuf);
		    return (1);
		}
	    }
	    else 
		docpy = FALSE;
	if (lnk && !symlnk){
	    if (link(source,target) < 0) {
		if(errno == EXDEV) {
			fprintf(stderr, "%s: different file system\n", cmd);
		} else {

			sprintf(Errbuf, "%s: link %s to %s", cmd, source, target);
			perror(Errbuf);
		}
		if (buf != NULL)
			free(buf);
		return(1);
	    }
	    else
		docpy = FALSE;
	}


	if (docpy || cpy ) {

		/*
		 * If link failed, and command was 'ln'
		 * send out appropriate error message.
		 */
		 
		/* 
		 * Attempt to open source for copy.
		 */
		 
		if((from = open(source, 0)) < 0) {
			sprintf(Errbuf, "%s: %s", cmd, source);
			perror(Errbuf);
			if (buf != NULL)
				free(buf);
			return (1);
		}
		
		/* 
		 * Save a flag to indicate target existed.
		 */
		
		oflg = access(target, 0) == -1;
		
		/* 
		 * Attempt to create a target.
		 */
		
		if((to = creat (target, 0666)) < 0) {
			sprintf(Errbuf, "%s: %s", cmd, target);
			perror(Errbuf);
			if (buf != NULL)
				free(buf);
                        close(from);
			return (1);
		}
		
		/*
		 * Block copy from source to target.
		 */
		 
		/*
		 * If we created a target,
		 * set its permissions to the source
		 * before any copying so that any partially copied
		 * file will have the source's permissions (at most)
		 * or umask permissions whichever is the most restrictive.
		 */
		 
		if (oflg)
			chmod(target, FMODE(s1));
		
		while((ct = read(from, fbuf, BLKSIZE)) != 0)
			if(ct < 0 || write(to, fbuf, ct) != ct) {
				sprintf(Errbuf, "%s: could not write %s", cmd, target);
				perror(Errbuf);
				/*
				 * If target is a regular file,
				 * unlink the bad file.
				 */
				 
				if (ISREG(s2))
					unlink(target);
				if (buf != NULL)
					free(buf);
				return (1);
			}
		
		/*
		 * If it was a move, leave times alone.
		 */
		if (mve) {
			times = (struct utimbuf *) malloc((unsigned) sizeof(struct utimbuf) + 2);
			times->actime = s1.st_atime;
			times->modtime = s1.st_mtime;
			utime(target, times);
			free(times);
		}
		close(from), close(to);
		
	}
	
	/* 
	 * If it is a copy or a link,
	 * we don't have to remove the source.
	 */
	 
	if (!mve) {
		if (buf != NULL)
			free(buf);
		return (0);
	}
	
	/* 
	 * Attempt to unlink the source.
	 */
	/* unlink source iff rename failed and we decided to copy */
	if (docpy && unlink(source) < 0) {
		/*
		 * If we can't unlink the source, assume we lack permission.
		 * Remove the target, as we may have copied it erroneously:
		 *  (1)	from an NFS filesystem, running as root.  In this case
		 *	the call to accs_parent(source) succeeds based on the
		 *	client credentials, but unlink(source) has just failed
		 *	because the server uid for client root is usually -2.
		 *  (2) because after the accs_parent(source) succeeded, we
		 *	lost a race to someone who removed write permission
		 *	from the source directory.
		 */
		if (stat(source, &s1) == 0)	
			(void) unlink(target);

s_unlink:
		sprintf(Errbuf, "%s: cannot remove %s", cmd, source);
		perror(Errbuf);
		if (buf != NULL)
			free(buf);
		return(1);
	}
	if (buf != NULL)
		free(buf);
	return(0);
}

/* In addition to checking write access on parent dir, do a stat on it */
accs_parent(name, amode, sdirp)
register char *name;
register int amode;
register struct stat *sdirp;
{
	register c;
	register char *p, *q;
	char *buf;

	/*
	 * Allocate a buffer for parent.
	 */
	
	if ((buf = malloc(strlen(name) + 2)) == NULL) {
		fprintf(stderr,"%s: Insufficient memory space.\n",cmd);
		exit(3);
	}
	p = q = buf;
	
	/* 
	 * Copy name into buf and set 'q' to point to the last
	 * delimiter within name.
	 */
	 
	while (c = *p++ = *name++)
		if (c == DELIM)
			q = p-1;
	
	/*
	 * If the name had no '\' or was "\" then leave it alone,
	 * otherwise null the name at the last delimiter.
	 */
	 
	if (q == buf && *q == DELIM)
		q++;
	*q = NULL;
	
	/*
	 * Find the access of the parent.
	 * If no parent specified, use dot.
	 */

	if ((c = access(buf[0] ? buf : DOT,amode)) == -1) {
	/* No write access to directory : no need to check sticky bit */
		free(buf);
		return(c);
	}

	/*
	 * Stat the parent : needed for sticky bit check.
	 * If stat fails, move() should fail the access, 
	 * since we cannot proceed anyway.
	 */
	c = stat(buf[0] ? buf : DOT, sdirp);
	free(buf);
	return(c);
	 
}

char *
dname(name)
register char *name;
{
	register char *p;

	/* 
	 * Return just the file name given the complete path.
	 * Like basename(1).
	 */
	 
	p = name;
	
	/*
	 * While there are characters left,
	 * set name to start after last
	 * delimiter.
	 */
	 
	while (*p)
		if (*p++ == DELIM && *p)
			name = p;
	return name;
}

usage()
{
	
	/*
	 * Display usage message.
	 *
	 * This used to be pretty complicated. I decided it was better
	 * just to have separate messages. -- dce
	 */

	if (cpy) {
		fprintf(stderr,
		   "Usage: %s [-p] file1 file2\n       %s [-p] file1 [file2...] directory\n",
		   cmd, cmd);
		exit(2);
	}
	if (mve) {
		fprintf(stderr,
		   "Usage: %s [-fp] file1 file2\n       %s [-fp] file1 [file2...] directory\n",
		   cmd, cmd);
		fprintf(stderr, "       mv [-fp] directory1 directory2\n");
		exit(2);
	}
	fprintf(stderr,
	   "Usage: %s [-sf] file1 file2\n       %s [-sf] file1 [file2...] directory\n",
	   cmd, cmd);
	exit(2);
}
