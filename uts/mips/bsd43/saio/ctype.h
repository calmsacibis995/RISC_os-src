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
/* $Header: ctype.h,v 1.6.3.2 90/05/10 04:45:55 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define	BSD43__U	01
#define	BSD43__L	02
#define	BSD43__N	04
#define	BSD43__S	010
#define BSD43__P	020
#define BSD43__C	040
#define BSD43__X	0100
#define BSD43__B	0200

extern	char	bsd43_(_ctype_)[];

#define	bsd43_isalpha(c)	((bsd43_(_ctype_)+1)[c]&(BSD43__U|BSD43__L))
#define	bsd43_isupper(c)	((bsd43_(_ctype_)+1)[c]&BSD43__U)
#define	bsd43_islower(c)	((bsd43_(_ctype_)+1)[c]&BSD43__L)
#define	bsd43_isdigit(c)	((bsd43_(_ctype_)+1)[c]&BSD43__N)
#define	bsd43_isxdigit(c)	((bsd43_(_ctype_)+1)[c]&(BSD43__N|BSD43__X))
#define	bsd43_isspace(c)	((bsd43_(_ctype_)+1)[c]&BSD43__S)
#define bsd43_ispunct(c)	((bsd43_(_ctype_)+1)[c]&BSD43__P)
#define bsd43_isalnum(c)	((bsd43_(_ctype_)+1)[c]&(BSD43__U|BSD43__L|BSD43__N))
#define bsd43_isprint(c)	((bsd43_(_ctype_)+1)[c]&(BSD43__P|BSD43__U|BSD43__L|BSD43__N|BSD43__B))
#define bsd43_isgraph(c)	((bsd43_(_ctype_)+1)[c]&(BSD43__P|BSD43__U|BSD43__L|BSD43__N))
#define bsd43_iscntrl(c)	((bsd43_(_ctype_)+1)[c]&BSD43__C)
#define bsd43_isascii(c)	((unsigned)(c)<=0177)
#define bsd43_toupper(c)	((c)-'a'+'A')
#define bsd43_tolower(c)	((c)-'A'+'a')
#define bsd43_toascii(c)	((c)&0177)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define _B BSD43__B
#   define _C BSD43__C
#   define _L BSD43__L
#   define _N BSD43__N
#   define _P BSD43__P
#   define _S BSD43__S
#   define _U BSD43__U
#   define _X BSD43__X
#   define isalnum bsd43_isalnum
#   define isalpha bsd43_isalpha
#   define isascii bsd43_isascii
#   define iscntrl bsd43_iscntrl
#   define isdigit bsd43_isdigit
#   define isgraph bsd43_isgraph
#   define islower bsd43_islower
#   define isprint bsd43_isprint
#   define ispunct bsd43_ispunct
#   define isspace bsd43_isspace
#   define isupper bsd43_isupper
#   define isxdigit bsd43_isxdigit
#   define toascii bsd43_toascii
#   define tolower bsd43_tolower
#   define toupper bsd43_toupper
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


