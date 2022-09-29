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
/* $Header: system.h,v 1.1.1.2 90/05/09 18:04:44 wje Exp $ */

/*
 * System-dependent definitions
 *
 * Author: Peter J. Nicklin
 */
#define FILEXIST(file)		(access(file,0) ? 0 : 1)
#define FILEWRITE(file)		(access(file,6) ? 0 : 1)

#ifdef V4BSD
#include <sys/file.h>
#define CREATE(name,flags,mode)	open(name,flags|FCREAT,mode)
#define FORK()			vfork()
#define OPEN(name,flags,mode)	open(name,flags,mode)
#define RENAME(from,to)		rename(from,to)
#else
#define NBBY 8            
#define O_RDONLY		000
#define O_WRONLY		001
#define O_RDWR			002
#define CREATE(name,flags,mode)	creat(name,mode)
#define FORK()			fork()
#define OPEN(name,flags,mode)	open(name,flags)
#define RENAME(from,to)		unlink(to); link(from,to); unlink(from)
#endif
