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
#ident	"$Header: newstat.c,v 1.2.2.2 90/05/09 15:24:42 wje Exp $"

#include <malloc.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "newstat.h"
#include "errmsg.h"
#define  FATAL  -1

/*  new_stat.c
 *	This function was added to cpio when the inode was resized
 *	from 16 bits to 32 bits.  All system and library calls 
 *	in cpio.c to obtain status information about a file were
 * 	changed to call this routine.  THE PROBLEM:  65K files can
 *	now have the same low 16-bits (i.e. - SAME inode value).
 *	If the link count is greater than 1, then cpio doesn't
 *	know which of those 65K files to which the link actually
 *	belongs, and, accordingly, links all files with the same
 *	inode/dev pair together.  THE SOLUTION HERE:  the low
 *	16 bits of the inode along with the device number are
 *	hashed into a small table.  A new inode/dev pair is
 *	generated for each file (i.e., each 48-bit - and possibly
 *	future 64-bit dev/inode pair shrinks into a 32-bit dev/
 *	inode pair), which makes cpio happy.  If two files have
 *	the same original device/inode numbers, then the files
 *	are links and are given the same new inode/dev number.
 *	These new numbers are absolutely bogus, and I feel sorry 
 *	for the poor sucker looking at the headers for meaningful 
 *	information, but cpio only uses this information to tell 
 *	which files are linked.  This disgusting code guarantees 
 *	REAL links.  The initial new inode pair starts at 3, and 
 *	continues up to	2^32-1, so if you're looking for something
 *	redeeming, maybe you can use that little gem of information.
 *
 */

static struct i_node *HASHTBL[HASH];    /* assumes initial NULLs   */
static unsigned long next_ino = 3L;  	/* initial inode value -
					   inode values will increase
					   from this value on      */

int
new_stat(type,severity,path,buf)
char type;		/* type of status call made from cpio.c */
int  severity;		/* severity level for lstat calls	*/
char *path;		/* path name of file			*/
struct stat *buf;	/* stat.h buffer			*/
{

    unsigned long hashval;	/* index into hashtable		*/
    struct i_node *iptr;	/* info struct for linked files */
    struct i_node *oldptr;

#ifdef DEBUG
fprintf(stderr,"new_stat: type = %c, sev = %d, path = %s\n",type,severity,path);
#endif

    switch (type) {
	case 'z':
	    if (zstat(severity,path,buf) == FATAL)  {
#ifdef DEBUG
fprintf(stderr,"new_stat: zstat failed\n");
#endif
		return(FATAL);
	    }
	    break;

   	case 'f':
	    if (fstat(severity,buf) == FATAL) {
#ifdef DEBUG
fprintf(stderr,"new_stat: fstat failed\n");
#endif
		return(FATAL);
	    }
	    break;

	case 'l':
	    if (lstat(path,buf) == FATAL)  {
#ifdef DEBUG
fprintf(stderr,"new_stat: lstat failed\n");
#endif
		return(FATAL);
	    }
	    break;

	default:
	    if (stat(path,buf) == FATAL)   {
#ifdef DEBUG
fprintf(stderr,"new_stat: stat failed\n");
#endif
		return(FATAL);
	    }
    }

#ifdef DEBUG
fprintf(stderr,"new_stat: inode = %ld, dev = %d, rdev = %d, nlinks = %d\n",buf->st_ino,buf->st_dev,buf->st_rdev,buf->st_nlink);
#endif


    if (buf->st_nlink < 2)  {		/* no links to file exist */

#ifdef DEBUG
fprintf(stderr,"new_stat: old_ino = %ld, old_dev = %d, ",buf->st_ino,buf->st_dev);
#endif

	buf->st_ino = next_ino & 0xffffL;
	buf->st_dev = (next_ino >> 16) & 0xffffL;

#ifdef DEBUG
fprintf(stderr,"new_stat: new_ino = %ld, new_dev = %d, ",buf->st_ino,buf->st_dev);
#endif

	next_ino++;
	return;
    }
    else  {		/*  possible linked files	*/

	hashval = ((buf->st_ino&0xffL << 8) + (buf->st_ino&0xff00L >> 8)
		  + buf->st_dev) % (HASH - 1);

/* 
    Need to turn the dev/inode pair into two shorts for cpio's
    data structures - cpio compares the dev/inode pair to see if
    the files are linked - the two shorts generated here are
    bogus, but serve the purpose of identifying files that really
    are linked for cpio's use
 */

	if ((iptr = HASHTBL[hashval]) == NULL)  {   /* no collision  */
	    if ((iptr=(struct i_node *)zmalloc(EWARN,sizeof(struct i_node)))==NULL) {
	        return(FATAL);
	    }
	    HASHTBL[hashval] = iptr;
   	    iptr->next = NULL;
	    iptr->ino = next_ino & 0xffffL;
	    iptr->dev = (next_ino >> 16) & 0xffffL;
	    iptr->old_ino = buf->st_ino;
	    iptr->old_dev = buf->st_dev;
	    iptr->old_rdev = buf->st_rdev;
	    buf->st_ino = iptr->ino;
	    buf->st_dev = iptr->dev;
	    next_ino++;
#ifdef DEBUG
fprintf(stderr,"new_stat: *** First entry into hashtable ***\n");
fprintf(stderr,"new_stat: ino = %d\n",iptr->ino);
fprintf(stderr,"new_stat: dev = %d\n",iptr->dev);
fprintf(stderr,"new_stat: old_ino = %d\n",iptr->old_ino);
fprintf(stderr,"new_stat: old_dev = %d\n",iptr->old_dev);
fprintf(stderr,"new_stat: old_rdev = %d\n",iptr->old_rdev);
#endif
	    return;
        }
        else  {		/* collision - need to verify link or not */
	    do {
	        if ((iptr->old_ino == buf->st_ino) &&  
		    (iptr->old_dev == buf->st_dev) &&
		    (iptr->old_rdev== buf->st_rdev)) {	 /* a link */
			buf->st_ino = iptr->ino;
			buf->st_dev = iptr->dev;
#ifdef DEBUG
fprintf(stderr,"new_stat: *** Linked file entries ***\n");
fprintf(stderr,"new_stat: ino = %d\n",iptr->ino);
fprintf(stderr,"new_stat: dev = %d\n",iptr->dev);
fprintf(stderr,"new_stat: old_ino = %d\n",iptr->old_ino);
fprintf(stderr,"new_stat: old_dev = %d\n",iptr->old_dev);
fprintf(stderr,"new_stat: old_rdev = %d\n",iptr->old_rdev);
#endif
			return;	
		}
		oldptr = iptr;
		iptr = iptr->next;
	    }
	    while (iptr != NULL);

/* 
    didn't find a matching linked file, so must create a new
    entry into the hash table
 */
	    if ((iptr=(struct i_node *)zmalloc(EWARN,sizeof(struct i_node))) == NULL) {
    	        return(FATAL);
	    }
	    oldptr->next = iptr;
	    iptr->next = NULL;
	    iptr->ino = next_ino & 0xffffL;
	    iptr->dev = (next_ino >> 16) & 0xffffL;
	    iptr->old_ino = buf->st_ino;
	    iptr->old_dev = buf->st_dev;
	    iptr->old_rdev = buf->st_rdev;
	    buf->st_ino = iptr->ino;
	    buf->st_dev = iptr->dev;
	    next_ino++;
#ifdef DEBUG
fprintf(stderr,"new_stat: *** New table entry ***\n");
fprintf(stderr,"new_stat: ino = %d\n",iptr->ino);
fprintf(stderr,"new_stat: dev = %d\n",iptr->dev);
fprintf(stderr,"new_stat: old_ino = %d\n",iptr->old_ino);
fprintf(stderr,"new_stat: old_dev = %d\n",iptr->old_dev);
fprintf(stderr,"new_stat: old_rdev = %d\n",iptr->old_rdev);
#endif
	    return;
	}
    }
}
