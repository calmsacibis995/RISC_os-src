/* Copyright(C) 1988, MIPS Computer Systems */
/* $Header: rb3125_console.c,v 1.1.2.1.1.1 90/06/28 15:35:46 root Exp $ */

#ident	"$Header: rb3125_console.c,v 1.1.2.1.1.1 90/06/28 15:35:46 root Exp $"

#include <sys/types.h>

console_init()
{
	stopclocks();
	scc_init();
}

console_getc()
{
	return sccgetc(0);
}

console_putc(c)
{
	return sccputc(0, c);
}

console_exit()
{
}

video_console_init()
{
}

