/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: kmem.c,v 1.1 90/07/18 12:49:34 kris Exp $ */

/* 	The routines in this file are MIPS specific.  The file provides
 *	an environment to compile system V specific routines.  The 
 *	symbols required to access the kernel page tables are sysv
 *	specific.
 */

#include <sysv/sys/types.h>
#include <sysv/sys/immu.h>
#include <sysv/sys/sbd.h>
#include <stdio.h>

/* Generalized modifications of sys/immu.h defines */

#define Ctob(x)		((uint)(x) << pfn_to_byte_shift)
#define Pnum(x)		((uint)(x) >> pfn_to_byte_shift)
#define Kvtokptbl(X)	(&kptbl[Pnum((uint)(X) - (uint)K2SEG)])
#define	pte_to_pfn(x)	((x) >> pte_to_pfn_shift)

pde_t	*Sysmap;
int	syssegsz;

int	pfn_to_byte_shift;
int	pte_to_pfn_shift;
int	pde_pg_vr;
int	pagesize;

/*	initkmem -- initialize for kernel page table lookup.
 *	     If memory is a core file, then read up copies of the
 *	page table.  If memory is /dev/kmem, this routine should not
 *	be called.
 */

initkmem(memf, p_syssegsz, p_kptbl, p_pagesize)

int	memf;		/* file descriptor for memory image */
int	p_syssegsz;	/* Address of size of the K2 Seg page table */
int	p_kptbl;	/* Address of pointer to page table */
int	p_pagesize;	/* Address of Kernel variable containing page size */
{
	int	off;

	p_syssegsz &= 0x7fffffff;
	if(lseek(memf, p_syssegsz, 0) == -1 ||
	   read(memf, &syssegsz, sizeof(int)) != sizeof(int)) {
		fprintf(stderr, "Can't read syssegsz from core file\n");
		exit(1);
	}
	if ((Sysmap = (pde_t *)malloc(syssegsz * sizeof(pde_t))) == 0) {
		perror("Sysmap");
		exit(1);
	}
	p_kptbl &= 0x7fffffff;
	if(lseek(memf, p_kptbl, 0) == -1 ||
	   read(memf, &off, sizeof(off)) != sizeof(off)) {
		fprintf(stderr, "Can't read kptbl from core file\n");
		exit(1);
	}
	off &= 0x7fffffff;
	if(lseek(memf, off, 0) == -1 ||
	   read(memf, Sysmap, syssegsz * sizeof(pde_t))
	   != syssegsz * sizeof(pde_t)) {
		fprintf(stderr, "Can't read complete Sysmap\n");
		exit(1);
	}

	/* See crash/init.c as model for the following */

	if ((p_pagesize &= 0x7fffffff) == 0 ||
	   lseek(memf, p_pagesize, 0) == -1 ||
	   read(memf, &pagesize, sizeof(int)) != sizeof(int) || pagesize == 4096) {
		pagesize = 4096;
		pfn_to_byte_shift = 12;
		pte_to_pfn_shift = 12;
		pde_pg_vr = PG_VR;
	} else {
		pagesize = 16384;
		pfn_to_byte_shift = 14;
		pte_to_pfn_shift = 10;
		pde_pg_vr = 0x2;
	}
}


/*
 * Seek into the kernel for a value.
 */
kmem_lseek(fd, base, off, kflag)
	int fd, base, off, kflag;
{
	int	page;

	if (kflag) {	

		if(IS_KSEG0(base) || IS_KSEG1(base)) {
			base &= 0x7fffffff;
		} else if(IS_KSEG2(base)) {
			pde_t *pde;

			page = (base - K2SEG) >> pfn_to_byte_shift;
			if(page > syssegsz) {
				fprintf(stderr,
				"kernel address %x out of range\n", base);
				exit(1);
			}
			pde = &Sysmap[page];
			if(!(pde->pgi.pg_pde & pde_pg_vr)) {
				fprintf(stderr,
				"kernel address %x is invalid\n", base);
				exit(1);
			}
			base = Ctob(pte_to_pfn(pde->pgi.pg_pde)) +
				(base & (pagesize-1));
		}
	}
	lseek(fd, base, off);
}
