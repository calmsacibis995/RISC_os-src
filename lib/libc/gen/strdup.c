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
#ident	"$Header: strdup.c,v 1.6.2.2 90/05/10 01:38:29 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/* string duplication
   returns pointer to a new string which is the duplicate of string
   pointed to by s1
   NULL is returned if new string can't be created
*/

#include <string.h>
#ifndef NULL
#define NULL	0
#endif

extern int strlen();
extern char *malloc();

char *
strdup(s1) 

   char * s1;

{  
   char * s2;

   s2 = malloc((unsigned) strlen(s1)+1) ;
   return(s2==NULL ? NULL : strcpy(s2,s1) );
}
