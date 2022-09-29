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
/* $Header: ik_ioctl.h,v 1.4.4.2 90/05/10 06:22:52 wje Exp $ */

#ifndef	_SYS_IK_IOCTL_
#define	_SYS_IK_IOCTL_	1


/* ioctls for the ikc driver */
# ifndef ikiocode
# define ikiocode(n)	('k'<<8|(n))

# define IKIOPEEK	ikiocode('a')
# define IKIOPOKE	ikiocode('b')
# define IKIOPIO	ikiocode('c')
# define IKIORESET	ikiocode('d')
# define IKIOSETVSTATE	ikiocode('e')
# define IKIOGETVSTATE	ikiocode('f')
# define IKIODEBUG	ikiocode('g')

struct poke
{
	int f, v;
};

struct vstate
{
	int f;
	int timo;
	int dummy[1];
};
# endif ikiocode

#endif	_SYS_IK_IOCTL_
