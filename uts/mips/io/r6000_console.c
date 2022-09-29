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
#ident	"$Header: r6000_console.c,v 1.5.1.3 90/05/10 05:27:24 wje Exp $"


console_init()
{
	stopclocks();
	du_init();		/* initialize the duart port */
	io_init_r6000();	/* an early, convenient place to do this */
}

console_exit()
{
}

video_console_init()
{
}

console_getc()
{
	return dugetchar();
}
console_putc(c)
{
	return duputchar(c);
}
