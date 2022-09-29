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
#ident	"$Header: filtfstab.c,v 1.2.2.2 90/05/10 03:47:39 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	filter BSD4.3 fstab file into a Sys V fstab file
 */

#include <stdio.h>

char	buffer[256];
char	rbuff[256];

main(argc,argv)
int argc;
char *argv[];
{
	while(fgets(buffer,255,stdin))
	{
		process(buffer); printf("%s",buffer); clearbuff();
	}
}

clearbuff()
{
int i;

	for(i=0; i < 256; i++) buffer[i] = rbuff[i] = 0;
}

process(bptr)
char *bptr;
{
	/* check for existing SYSV device files */
	if(strncmp(bptr,"/dev/dsk",8) == 0) return;

	/* check for a local device name to convert */
	if(strncmp(bptr,"/dev",4) != 0)
	{
		if(strncmp(bptr,"#/dev",5) != 0)
		{
			/* neither type so don't process */
			return;
		}

		/* step over the # */
		bptr++;
	}

	/* adjust ptr to look at variant area */
	bptr += 7;

	/* prepend dsk */
	strcpy(rbuff,"dsk/ip");

	/* translate the device name */
	switch(*bptr)
	{
		case '0': strcat(rbuff,"c0d0"); break;
		case '1': strcat(rbuff,"c0d1"); break;
		case '2': strcat(rbuff,"c1d0"); break;
		case '3': strcat(rbuff,"c1d1"); break;
	}
	bptr++;
	switch(*bptr)
	{
		case 'a': strcat(rbuff,"s0"); break;
		case 'b': strcat(rbuff,"s1"); break;
		case 'c': strcat(rbuff,"s2"); break;
		case 'd': strcat(rbuff,"s3"); break;
		case 'e': strcat(rbuff,"s4"); break;
		case 'f': strcat(rbuff,"s5"); break;
		case 'g': strcat(rbuff,"s6"); break;
		case 'h': strcat(rbuff,"s7"); break;
		case 'i': strcat(rbuff,"s8"); break;
		case 'j': strcat(rbuff,"s9"); break;
	}
	bptr-=3;

	/* delete the BSD 4.3 name */
	delete(bptr,4);

	/* replace with the SYS V name */
	insert(bptr,rbuff,12);
}


insert(hptr,iptr,cnt)
char *hptr;
char *iptr;
int cnt;
{
char *eptr;
char *sptr;
int len;

	/* find end of string */
	len = strlen(hptr);
	eptr = hptr; while(*eptr != 0) eptr++;

	/* determine offset to new end */
	sptr = eptr; sptr += cnt;

	/* copy backwards */
	while(len--)
	{
		*sptr = *eptr; sptr--; eptr--;
	}

	/* clear the space */
	cnt++; while(cnt--) *eptr++ = 0x20;

	/* insert string */
	while(*iptr != 0) *hptr++ = *iptr++;
}

delete(hptr,n)
char *hptr;
int n;
{
char *tptr;

	tptr = hptr; tptr += n;
	while(*tptr != 0) *hptr++ = *tptr++;
	*hptr = 0;
}

