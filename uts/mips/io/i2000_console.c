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
#ident	"$Header: i2000_console.c,v 1.4.2.5 90/05/30 23:50:22 wje Exp $"
/*
 * $Header
 */
/*
 * du_init for i2000 so we don't need stuff in startup.c
 * Project: Jupiter Workstation
 * Date: 09-Feb-1988
 */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"

console_init()
{
	/*showconfig = 1;	/* My preference during debug */
	iop_initialize();
	stopclocks();
	early_uartinit();
	grafprobe();
} /* console_init */

console_exit()
{
}

video_console_init()
{
}

console_getc()
{
	return grafgetc();
} /* console_getc */


console_putc(c)
{
	return grafputc(c,1);
} /* console_getc */
