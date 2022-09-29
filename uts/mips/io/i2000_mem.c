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
#ident	"$Header: i2000_mem.c,v 1.3.2.2 90/05/10 05:20:54 wje Exp $"

/*
 * I2000 CPU - memory board control / ECC error handler.
 */


/* This driver currently just supplies stubs
 * for ecc handler for R2300 kernels.
 */

void
lmem_setmax()
{
	extern char	*mem_test_max;
	mem_test_max = (char *) min((int)mem_test_max,0x1000000);
}


void
lmem_err_scan()
{
}
