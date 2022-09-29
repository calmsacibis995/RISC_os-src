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
#ident	"$Header: xdr_reference.c,v 1.3.1.2 90/05/07 21:04:38 wje Exp $"
/*
 * @(#)xdr_reference.c 1.1 86/09/24 Copyr 1984 Sun Micro
 *
 * xdr_reference.c, Generic XDR routines impelmentation.
 *
 * These are the "non-trivial" xdr primitives used to serialize and de-serialize
 * "pointers".  See xdr.h for more info on the interface to xdr.
 */

#ifdef KERNEL
#include "sys/types.h"
#include "sys/param.h"
#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include <bsd43/sys/syslog.h>
#define rpc_perror(x) log(BSD43_LOG_ERR,"%s\n",x)
#else
#include <stdio.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#endif

#define LASTUNSIGNED	((u_int)0-1)

/*
 * XDR an indirect pointer
 * xdr_reference is for recursively translating a structure that is
 * referenced by a pointer inside the structure that is currently being
 * translated.  pp references a pointer to storage. If *pp is null
 * the  necessary storage is allocated.
 * size is the sizeof the referneced structure.
 * proc is the routine to handle the referenced structure.
 */
bool_t
xdr_reference(xdrs, pp, size, proc)
	register XDR *xdrs;
	caddr_t *pp;		/* the pointer to work on */
	u_int size;		/* size of the object pointed to */
	xdrproc_t proc;		/* xdr routine to handle the object */
{
	register caddr_t loc = *pp;
	register bool_t stat;

	if (loc == NULL)
		switch (xdrs->x_op) {
		case XDR_FREE:
			return (TRUE);

		case XDR_DECODE:
			*pp = loc = (caddr_t) mem_alloc(size);
#ifndef KERNEL
			if (loc == NULL) {
				rpc_perror("xdr_reference: out of memory");
				return (FALSE);
			}
#endif
			bzero(loc, (int)size);
			break;
	}

	stat = (*proc)(xdrs, loc, LASTUNSIGNED);

	if (xdrs->x_op == XDR_FREE) {
		mem_free(loc, size);
		*pp = NULL;
	}
	return (stat);
}


#ifndef KERNEL
/*
 * xdr_pointer():
 *
 * XDR a pointer to a possibly recursive data structure. This
 * differs with xdr_reference in that it can serialize/deserialiaze
 * trees correctly.
 *
 *  What's sent is actually a union:
 *
 *  union object_pointer switch (boolean b) {
 *  case TRUE: object_data data;
 *  case FALSE: void nothing;
 *  }
 *
 * > objpp: Pointer to the pointer to the object.
 * > obj_size: size of the object.
 * > xdr_obj: routine to XDR an object.
 *
 */
bool_t
xdr_pointer(xdrs,objpp,obj_size,xdr_obj)
	register XDR *xdrs;
	char **objpp;
	u_int obj_size;
	xdrproc_t xdr_obj;
{

	bool_t more_data;

	more_data = (*objpp != NULL);
	if (! xdr_bool(xdrs,&more_data)) {
		return(FALSE);
	}
	if (! more_data) {
		*objpp = NULL;
		return(TRUE);
	}
	return(xdr_reference(xdrs,objpp,obj_size,xdr_obj));
}
#endif /* ! KERNEL */
