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
/* $Header: xm_ioctl.h,v 1.4.4.2 90/05/10 06:45:18 wje Exp $ */

#ifndef	_SYS_XM_IOCTL_
#define	_SYS_XM_IOCTL_	1


# define XMIOCODE(c)	('x'<<8|(c))

# define XMIODEBUG	XMIOCODE(30)
# define XMIODRESET	XMIOCODE(32)
# define XMIOCRESET	XMIOCODE(33)
# define XMIOLOAD	XMIOCODE(34)
# define XMIOUNLOAD	XMIOCODE(35)
# define XMIOREWIND	XMIOCODE(36)
# define XMIOGCPARAMS	XMIOCODE(37)
# define XMIOGDPARAMS	XMIOCODE(38)
# define XMIOSCPARAMS	XMIOCODE(39)
# define XMIOSDPARAMS	XMIOCODE(40)

#endif	_SYS_XM_IOCTL_
