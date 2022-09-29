#ident "$Header: mem.c,v 1.2 90/01/16 17:20:39 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * mem.c -- memory pseudo-device
 * Add mem(addr,width,am) device
 */

#include "sys/param.h"
#include "sys/file.h"
#include "sys/errno.h"
#include "machine/cpu.h"
#include "mipsvme/vmereg.h"
#include "saio/saio.h"
#include "saio/setjmp.h"

struct am_table {
	unsigned at_am;		/* address modifier */
	unsigned at_base;	/* physical base address */
	unsigned at_maxaddr;	/* max address in region, 0 => eot */
} am_table[] = {
	{ 0,		0,		0xffffffff	}, /* default */
	{ VME_A32NPAMOD,K1BASE,		0x17ffffff	}, /* sys bus mem */
	{ VME_A16NPAMOD,VME_A16NPBASE,	0xffff		}, /* A16 NP */
	{ VME_A16SAMOD,	VME_A16SBASE,	0xffff		}, /* A16 S */
	{ VME_A24SAMOD,	VME_A24SBASE,	0xffffff	}, /* A24 S */
	{ 0,		0,		0		}
};

struct am_table *find_at();

_memopen(io)
register struct iob *io;
{
	register struct am_table *at;
	register width;

	at = find_at(io->i_part);
	if (at == NULL) {
		printf("unsupported address modifier\n");
		goto bad;
	}
	width = io->i_unit;
	if (width == 0)
		io->i_unit = width = sizeof(int);
	if (width != sizeof(char) && width != sizeof(short)
	    && width != sizeof(int)) {
		printf("illegal width\n");
		goto bad;
	}
	if (io->i_ctlr > at->at_maxaddr) {
		printf("base address outside region\n");
		goto bad;
	}
	io->i_devaddr = (unsigned)at;
	return(0);

bad:
	io->i_errno = ENXIO;
	return(-1);
}

_memstrategy(io, func)
register struct iob *io;
{
	register struct am_table *at;
	int cc;

	at = (struct am_table *)io->i_devaddr;
	if ((unsigned)io->i_ctlr + io->i_cc > at->at_maxaddr) {
		io->i_errno = EIO;
		return(-1);
	}

	if (func == READ)
		cc = mem_copy(io->i_ctlr + at->at_base, io->i_ma,
		    io->i_cc, io->i_unit);
	else if (func == WRITE)
		cc = mem_copy(io->i_ma, io->i_ctlr + at->at_base,
		    io->i_cc, io->i_unit);
	else
		_io_abort("memstrategy: bad function");
	if (cc > 0)
		io->i_ctlr += cc;
	return(cc);
}

static
mem_copy(src, dst, bcnt, width)
register unsigned src;
register unsigned dst;
register unsigned bcnt;
int width;
{
	jmp_buf fault_buf;
	volatile unsigned bytes = bcnt;

	if (setjmp(fault_buf)) {
		show_fault();	/* also clears WBE's */
		sa_spl();
		return(bcnt - bytes);
	}

	switch (width) {

	case sizeof(int):
		if (((dst ^ src) & (sizeof(int)-1)) == 0) { /* alignable */
			while (bytes && (src & (sizeof(int)-1))) {
				*(char *)dst = *(char *)src;
				wbflush();
				src += sizeof(char);
				dst += sizeof(char);
				bytes--;
			}
			while (bytes >= sizeof(int)) {
				*(int *)dst = *(int *)src;
				wbflush();
				src += sizeof(int);
				dst += sizeof(int);
				bytes -= sizeof(int);
			}
		}
		/* fall into */

	case sizeof(short):
		if (((dst ^ src) & (sizeof(short)-1)) == 0) {
			while (bytes && (src & (sizeof(short)-1))) {
				*(char *)dst = *(char *)src;
				wbflush();
				src += sizeof(char);
				dst += sizeof(char);
				bytes--;
			}
			while (bytes >= sizeof(short)) {
				*(short *)dst = *(short *)src;
				wbflush();
				src += sizeof(short);
				dst += sizeof(short);
				bytes -= sizeof(short);
			}
		}
		/* fall into */

	case sizeof(char):
		while (bytes) {
			*(char *)dst = *(char *)src;
			wbflush();
			src += sizeof(char);
			dst += sizeof(char);
			bytes--;
		}
		break;

	default:
		_io_abort("mem_copy bad size");
	}
	return(bcnt);
}

struct am_table *
find_at(am)
{
	register struct am_table *at;

	for (at = am_table; at->at_maxaddr; at++)
		if (at->at_am == am)
			return(at);
	return(NULL);
}
