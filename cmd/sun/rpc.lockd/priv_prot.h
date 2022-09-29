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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: priv_prot.h,v 1.1.1.2.1.1.1.2 90/11/17 11:51:29 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/priv_prot.h 1.1 */
/*
 * This file consists of all rpc information for lock manager
 * interface to status monitor
 */

#define PRIV_PROG 100021
#define PRIV_VERS 2
#define PRIV_CRASH 1
#define PRIV_RECOVERY 2

