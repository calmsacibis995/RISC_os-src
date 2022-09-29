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
#ident	"$Header: permit.c,v 1.5.2.2 90/05/09 15:29:25 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include "cron.h"

struct stat globstat;
#define	exists(file)	(stat(file,&globstat) == 0)
#define ROOT	"root"

int per_errno;	/* status info from getuser */


/****************/
char *getuser(uid)
/****************/
int uid;
{
	struct passwd *nptr,*getpwuid();

	if ((nptr=getpwuid(uid)) == NULL) {
		per_errno=1;
		return(NULL); }
	if ((strcmp(nptr->pw_shell,SHELL)!=0) &&
	    (strcmp(nptr->pw_shell,"")!=0)) {
		per_errno=2;
		/* return NULL if you want crontab and at to abort
		   when the users login shell is not /bin/sh otherwise
		   return pw_name
		*/
		return(nptr->pw_name);
	}
	return(nptr->pw_name);
}


/**********************/
allowed(user,allow,deny)
/**********************/
char *user,*allow,*deny;
{
	if ( exists(allow) ) {
		if ( within(user,allow) ) return(1);
		else return(0); }
	else if ( exists(deny) ) {
		if ( within(user,deny) ) return(0);
		else return(1); }
	else if ( strcmp(user,ROOT)==0 ) return(1);
		else return(0);
}


/************************/
within(username,filename)
/************************/
char *username,*filename;
{
	char line[UNAMESIZE];
	FILE *cap;
	int i;

	if((cap = fopen(filename,"r")) == NULL)
		return(0);
	while ( fgets(line,UNAMESIZE,cap) != NULL ) {
		for ( i=0 ; line[i] != '\0' ; i++ ) {
			if ( isspace(line[i]) ) {
				line[i] = '\0';
				break; }
		}
		if ( strcmp(line,username)==0 ) {
			fclose(cap);
			return(1); }
	}
	fclose(cap);
	return(0);
}
