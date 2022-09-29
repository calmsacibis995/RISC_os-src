/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: sprintf.c,v 1.4 89/11/20 16:20:20 menna Exp $ */

#include	"sys/stdio.h"
#include 	"varargs.h"

char *sprintf(str, fmt, va_alist)
char *str, *fmt;
va_dcl
{
	FILE _strbuf;
	va_list ap;

	va_start(ap);
/*	_strbuf._flag = _IOWRT+_IOSTRG; */
	_strbuf._ptr = str;
	_strbuf._cnt = 32767;
	_doprnt(fmt, ap, &_strbuf);
	stdio_putc('\0', &_strbuf);
	return(str);
}
