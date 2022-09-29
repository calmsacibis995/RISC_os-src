#ident "$Header: protoio.h,v 1.2 90/01/16 17:40:16 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * protoio.h (prom version) -- protocol io interface file
 * used to adapt protocol code to prom or unix environment
 */
typedef	int	pdev_t;

#define	PUTC(c, fd)	putc(c, fd)
#define	GETC(fd)	getc(fd)
#define	PUTFLUSH(fd)
#define	PINIT(fd)
