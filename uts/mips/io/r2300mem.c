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
#ident	"$Header: r2300mem.c,v 1.2.3.2 90/05/10 05:25:40 wje Exp $"

/*
 * R2300 CPU - memory board control / ECC error handler.
 */


/* This driver currently just supplies stubs
 * for ecc handler for R2300 kernels.
 */

void
lmem_setmax()
{
}

void
r2300memedtinit(e)
/*struct edt *e;*/
{
}

void
lmem_err_scan()
{
}


/*
 * ECC error interrupt handler
 */
void
r3200memintr()
{
	lmem_err_scan();
}
