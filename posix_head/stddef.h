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
/* $Header: stddef.h,v 1.1.1.2 90/05/10 04:09:17 wje Exp $ */

#ifndef	_POSIX_STDDEF_
#define	_POSIX_STDDEF_	1

#ifndef	NULL
#define	NULL	0
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

#ifndef _PTRDIFF_T
#define _PTRDIFF_T
typedef int 		ptrdiff_t;
#endif

#ifndef _WCHAR_T
#define _WCHAR_T
typedef	long		wchar_t;
#endif

#define offsetof(type, member) ((size_t)(((type *) 0)->member - (char *) 0))

#endif	/* _POSIX_STDDEF_ */
