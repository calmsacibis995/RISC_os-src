#ident "$Header: exec_stub.c,v 1.2 90/01/16 16:44:03 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

#ifndef PROM
/*
 * exec_stub.c -- link promexec to prom entry point
 */

#include "sys/types.h"
#include "prom/entrypt.h"

promexec(pap)
struct promexec_args *pap;
{
	_prom_exec(pap);
}
#endif
