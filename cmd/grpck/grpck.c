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
#ident	"$Header: grpck.c,v 1.1.2.2.1.3 90/08/13 17:14:38 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>

#define ERROR1 "Too many/few fields"
#define ERROR1A "Line too long"
#define ERROR2a "No group name"
#define ERROR2b "Bad character(s) in group name"
#define ERROR3  "Invalid GID"
#define ERROR4a "Null login name"
#define ERROR4b "Logname not found in password file"

int eflag, badchar, baddigit,badlognam,colons,len,i;

#define MYBUFSIZE	512	/* max line length including newline and null */

char buf[MYBUFSIZE];
char tmpbuf[MYBUFSIZE];

struct passwd *getpwnam();
char *strchr();
char *nptr;
int setpwent();
char *cptr;
FILE *fptr;
int delim[MYBUFSIZE];
long gid;
int error();

main (argc,argv)
int argc;
char *argv[];
{
  if ( argc == 1)
    argv[1] = "/etc/group";
  else if ( argc != 2 )
       {
	 printf ("\nusage: %s filename\n\n",*argv);
	 exit(1);
       }

  if ( ( fptr = fopen (argv[1],"r" ) ) == NULL )
  { 
	printf ("\ncannot open file %s\n\n",argv[1]);
	exit(1);
  }

  while(fgets(buf,MYBUFSIZE,fptr) != NULL )
  {
	if ( buf[0] == '\n' )    /* blank lines are ignored */
          continue;

#ifdef RISCOS
	/* special case for login name of "+" and "-", which means
	 * skip to YP or other services
	 */
	if ( buf[0] == '+' || buf[0] == '-' )
	{
	    fprintf(stderr,"\n%s\tNIS entry.  You should be running ypbind\n",buf);
	    continue;	/* ignore the rest of the line */
	}
#endif
	i = strlen(buf);
	if ( (i == (MYBUFSIZE-1)) && (buf[i-1] != '\n') ) {  /* line too long */
		buf[i-1] = '\n';	/* add newline for printing */
		error(ERROR1A);
		while(fgets(tmpbuf,MYBUFSIZE,fptr) != NULL )  {
			i = strlen(tmpbuf);
			if ( (i == (MYBUFSIZE-1)) && (tmpbuf[i-1] != '\n') ) 
				/* another long line */
				continue;
			else
				break;
		}
		/* done reading continuation line(s) */

		strcpy(tmpbuf, buf);
	} else {
		strcpy(tmpbuf, buf);
		tmpbuf[i-1] = ',';	/* change newline to comma for strchr */
	}

	colons=0;
	eflag=0;
	badchar=0;
	baddigit=0;
	badlognam=0;
	gid=0l;

    /*	Check number of fields	*/

	for (i=0 ; buf[i]!=NULL ; i++)
	{
	  if (buf[i]==':')
          {
            delim[colons]=i;
            ++colons;
          }
	}
	if (colons != 3 )
	{
	  error(ERROR1);
	  continue;
	}

    /*	check to see that group name is at least 1 character	*/
    /*		and that all characters are printable.		*/
 
	if ( buf[0] == ':' )
	  error(ERROR2a);
	else
	{
	  for ( i=0; buf[i] != ':'; i++ )
	  {
	    if ( ! ( isprint(buf[i])))
		badchar++;
	  }
	  if ( badchar > 0 )
	    error(ERROR2b);
	}

    /*	check that GID is numeric and <= 65535	*/

	len = ( delim[2] - delim[1] ) - 1;

	if ( len > 5 || len == 0 )
	  error(ERROR3);
	else
	{
	  for ( i=(delim[1]+1); i < delim[2]; i++ )
	  {
	    if ( ! (isdigit(buf[i])))
	      baddigit++;
	    else if ( baddigit == 0 )
		gid=gid * 10 + (buf[i]) - '0';    /* converts ascii */
                                                  /* GID to decimal */
	  }
	  if ( baddigit > 0 )
	    error(ERROR3);
	  else if ( gid > 65535l || gid < 0l )
	      error(ERROR3);
	}

     /*  check that logname appears in the passwd file  */

	nptr = &tmpbuf[delim[2]];
	nptr++;
	while ( ( cptr = strchr(nptr,',') ) != NULL )
	{
	  *cptr=NULL;
	  if ( *nptr == NULL )
	  {
	    error(ERROR4a);
	    nptr++;
	    continue;
	  }
	  if (  getpwnam(nptr) == NULL )
	  {
	    badlognam=1;
	    error(ERROR4b);
	  }
	  nptr = ++cptr;
	  setpwent();
	}
	
  }
}

    /*	Error printing routine	*/

error(msg)

char *msg;
{
	if ( eflag==0 )
	{
	  fprintf(stderr,"\n\n%s",buf);
	  eflag=1;
	}

	if ( badchar != 0 )
	{
	  fprintf (stderr,"\t%d %s\n",badchar,msg);
	  badchar=0;
	  return;
	}
	else if ( baddigit != 0 )
	     {
		fprintf (stderr,"\t%s\n",msg);
		baddigit=0;
		return;
	     }
	     else if ( badlognam != 0 )
		  {
		     fprintf (stderr,"\t%s - %s\n",nptr,msg);
		     badlognam=0;
		     return;
		  }
		  else
		  {
		    fprintf (stderr,"\t%s\n",msg);
		    return;
		  }
}
