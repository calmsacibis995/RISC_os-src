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
#ident	"$Header: strdup.c,v 1.2.1.3 90/05/09 19:10:14 wje Exp $"
/*
 * @(#)strdup.c	1.1 88/05/18 4.0NFSSRC Copyr 1988 Sun Micro
 *
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 */

#define NULL 0

char *strdup(s1)
char *s1;
{
    char *s2;
    extern char *malloc(), *strcpy();

    s2 = malloc(strlen(s1)+1);
    if (s2 != NULL)
        s2 = strcpy(s2, s1);
    return(s2);
}
