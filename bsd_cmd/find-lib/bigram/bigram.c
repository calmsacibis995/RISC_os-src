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
#ident	"$Header: bigram.c,v 1.2.1.2 90/05/07 18:32:36 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)bigram.c	4.2	(Berkeley)	7/21/83";
#endif not lint

/*
 *  bigram < text > bigrams
 * 
 * List bigrams for 'updatedb' script.
 * Use 'code' to encode a file using this output.
 */

#include <stdio.h>

#define MAXPATH	1024		/* maximum pathname length */

char path[MAXPATH];
char oldpath[MAXPATH] = " ";	

main ( )
{
  	register int count, j;

     	while ( gets ( path ) != NULL ) {

		count = prefix_length ( oldpath, path );
		/*
		   output post-residue bigrams only
		*/
		for ( j = count; path[j] != NULL; j += 2 ) {
			if ( path[j + 1] == NULL ) 
				break;
			putchar ( path[j] );
			putchar ( path[j + 1] );
			putchar ( '\n' );
		}
		strcpy ( oldpath, path );
   	}
}

prefix_length ( s1, s2 )	/* return length of longest common prefix */
	char *s1, *s2;		/* ... of strings s1 and s2 */
{
	register char *start;

    	for ( start = s1; *s1 == *s2; s1++, s2++ )	
		if ( *s1 == NULL )		
	    		break;
    	return ( s1 - start );
}
