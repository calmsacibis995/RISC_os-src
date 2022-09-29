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
#ident	"$Header: cleannew.c,v 1.2.2.2 90/05/10 03:43:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char cleanname[128];
char oldname[128];
char newname[128];

char *nptr;
char *eptr;

FILE *fopen();
FILE *fd;

int fdold, fdnew;
char och, nch;

struct	stat	stbuf;

extern	int errno;


/********************************
 *
 *	compare file sizes
 *
 ********************************/

sizecomp(o,n)
int o,n;
{
long osz;
long nsz;

	/* get the sizes of the two files */
	fstat(o,&stbuf); osz = stbuf.st_size;
	fstat(n,&stbuf); nsz = stbuf.st_size;

	/* determine if they are the same size */
	if(osz == nsz)	return(0);	/* yes */
	else		return(1);	/* no */
}

/*************************************************************
 *
 *	conduct a byte for byte compare of old and new file
 *
 *	return 0 if the same
 *	return 1 if different
 *
 ************************************************************/

process(oldptr,newptr)
char *oldptr, *newptr;
{
int same;

	same = 1;	/* assume same */

	/* open the two files */
	errno = 0;
	fdold = open(oldptr,0);
	if(errno != 0)
	{
		perror(oldptr);
		same = 0;
		goto bail;
	}

	errno = 0;
	fdnew = open(newptr,0);
	if(errno != 0)
	{
		same = 0;
		goto bail;
	}

	/* check if files are same size */
	if(sizecomp(fdold,fdnew))
	{
		/* NOPE !! */
		same = 0;
		goto bail;
	}

	/* files are the same size so must be */
	/* checked byte for byte */
	while( read(fdold,&och,1) > 0)
	{
		read(fdnew,&nch,1);
		if(och != nch)
		{
			same = 0;
			break;
		}
	}

bail:
	/* close the two files */
	close(fdold);
	close(fdnew);

#ifdef DEBUG
	printf("%s <==> %s\n",oldptr,newptr);
#endif
	return(same);
}

/*************************************************************
 *
 *	cleannew preserve.log revision base-directory
 *	argv[0]  argv[1]      argv[2]  argv[3]
 *
 *************************************************************/

main(argc,argv)
int argc;
char * argv[];
{
	/* open the preserve.log file and use as the check file */
	fd = fopen(argv[1],"r");

	/* read a line entry from the file */
	while(fgets(cleanname,sizeof(cleanname),fd))
	{
		nptr = &cleanname[0];

		/* check if it is a preserve entry */
		if(!(strncmp(cleanname,"preserve",8)))
		{
			/* it is so just extract the absolute file name */
			while(*nptr != '.') nptr++; nptr++;
			eptr = nptr;
			while(*eptr != 0x0A) eptr++;
			*eptr = 0;

			/* build composite oldname */
			if(argc > 3)
			{
				strcpy(oldname,argv[3]);
				strcat(oldname,nptr);
			}
			else
			{
				strcpy(oldname,nptr);
			}

			/* build composite newname */
			if(argc > 3)
			{
				strcpy(newname,argv[3]);
				strcat(newname,nptr);
			}
			else
			{
				strcpy(newname,nptr);
			}

			if(argc > 2 )
			{
				strcat(newname,":");
				strcat(newname,argv[2]);
				strcat(newname,"+");
			}

			/* check if the new is the same as the old */
			/* if it is then nuke the new copy */
			if(process(oldname,newname))
			{
				/* new is the same as old */
				printf("Removing Duplicate File %s\n",newname);
				unlink(newname);
			}
		}
	}

	/* close the preserve.log file */
	fclose(fd);
}

