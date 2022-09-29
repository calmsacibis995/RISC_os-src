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
#ident	"$Header: bzero.c,v 1.1.1.2 90/05/10 03:40:33 wje Exp $"
/*
 *  Fast, sleazy, and ugly zero function.
 *
 *  Note that this will only work on a VAX, but it is real easy to write a
 *  similar function for whatever machine you may need.  If nothing else,
 *  just a simple loop in C will suffice.
 *
 *  Dave Johnson, Rice University.
 *
 *  Enhanced by William LeFebvre of Rice University to handle zeroing more
 *  than 64K.
 */

# define   K	1024

/*
 *  bzero(memory, amount) - set "amount" bytes starting at "memory" to the
 *			    value 0.
 */

bzero(memory, amount)

char *memory;
int  amount;

{
    while (amount >= 64*K)
    {
	_bzero64(memory, 64*K-1);
	memory += 64*K-1;
	amount -= 64*K-1;
    }
    _bzero64(memory, amount);
}

_bzero64(memory, amount)

char *memory;
int  amount;

{
    asm("	movc5	$0, (sp), $0, 8(ap), *4(ap)");
}
