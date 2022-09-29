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
/* $Header: newstat.h,v 1.2.2.2 90/05/09 15:24:48 wje Exp $ */

#define HASH 8*1024	/* This number MUST be smaller than 65K
			   (i.e., 16 bits) - this is the size of 
			   the hash table that stores the inode
	 	 	   values */

struct i_node		/* dynamically allocated new inode value
			   structure */
{
    unsigned short ino;		/* lower half of new inode value */
    short          dev;		/* upper half of new inode value */
    short          old_rdev;	/* save value of special device  */
    short          old_dev;	/* save value of original device */
    unsigned long  old_ino;	/* save original inode value     */
    struct i_node *next;	/* next in collision chain       */
};

extern struct i_node *HASHTBL[]; /* hash table - pointers to struct */
extern int new_stat();
