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
/* $Header: name_mapping.h,v 1.2.1.6 90/05/10 04:16:33 wje Exp $ */

#include "../../../lib/libc/include/name_mapping.h"

#define getpass __getpass
#define bcmp __bcmp
#define tempnam __tempnam
#define mktemp __mktemp
#define	fgetpwent	_fgetpwent


#ifdef SYSTYPE_SYSV
#define _SIZE_T
#define _U_CHAR
#ifdef LANGUAGE_C
typedef unsigned long u_long;
#include <sys/types.h>

#define _UNISTD_
#include <unistd.h>
#undef _UNISTD_
#define __getdtablesize() sysconf(_SC_OPEN_MAX)
#endif LANGUAGE_C
#endif SYSTYPE_SYSV
