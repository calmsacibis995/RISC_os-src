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
#ident	"$Header: convttys.c,v 1.2.2.2 90/05/10 03:45:11 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	filter BSD4.3 fstab file into a Sys V fstab file
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

char	inbuff[256];
char	otbuff[256];
char	erbuff[256];
char	ttybuff[32];
char	getbuff[32];
char	spdbuff[32];
char	typbuff[32];

main(argc,argv)
int argc;
char *argv[];
{
	while(fgets(inbuff,255,stdin))
	{
		if(process(inbuff,otbuff,erbuff))
		{
			fprintf(stdout,"%s",otbuff);
			fprintf(stderr,"%s",erbuff);
		}
		clearbuff();
	}
}

clearbuff()
{
int i;

	for(i=0; i < 256; i++) inbuff[i] = otbuff[i] = 0;
}

process(iptr,optr,eptr)
char *iptr;
char *optr;
char *eptr;
{
char *tptr;

	/* check for a tty entry */
	if(strncmp(iptr,"tty",3) != 0)
	{
		/* check for comments */
		if(*iptr != '#') return(0); /* NOPE */

		/* copy comment and return */
		strcpy(optr,iptr);
		return(1);
	}

	/* check if this is a null entry */
	tptr = strtok(iptr," \t");
	tptr = strtok((char *)NULL," \t");

	/* if we find a none ignore entry */
	if(strncmp(tptr,"none",4) == 0) return(0);

	/* save tty name */
	strcpy(ttybuff,iptr);

	/* check if quote and step over */
	if(*tptr == '"') tptr++;

	/* save getty name */
	strcpy(getbuff,tptr);

	/* look for speed value */
	tptr = strtok((char *)NULL," \t");
	while(!isdigit(*tptr)) tptr++;

	/* copy speed number */
	strcpy(spdbuff,tptr);
	if(spdbuff[(strlen(spdbuff) - 1)] == '"')
		spdbuff[(strlen(spdbuff) - 1)] = 0;

	/* get the terminal type */
	tptr = strtok((char *)NULL," \t");
	strcpy(typbuff,tptr);

	/* get the state */
	tptr = strtok((char *)NULL," \t");

	/* compose a new inittab line entry */
	strncpy(optr,&ttybuff[(strlen(ttybuff) - 2)],2);
	strcat(optr,":234:");
	if(strncmp(tptr,"on",2))
		strcat(optr,"off:");
	else
		strcat(optr,"respawn:");
	strcat(optr,getbuff);
	strcat(optr," ");
	strcat(optr,ttybuff);
	strcat(optr," dx_");
	strcat(optr,spdbuff);
	strcat(optr," none ");
	strcat(optr,"LDISCO");

	/* get comment if any */
	while(*tptr != 0x0A)
	{
		/* look for trailing comment */
		if(*tptr == '#')
		{
			/* got it so copy and bail */
			strcat(optr,"\t");
			strcat(optr,tptr);
			goto bail;
		}
		tptr++;
	}
	/* end of an inittab entry */
	strcat(optr,"\n");

bail:
	/* compose a ttytype entry */
	strcpy(eptr,typbuff);
	strcat(eptr,"\t");
	strcat(eptr,ttybuff);
	strcat(eptr,"\n");

	return(1);
}

