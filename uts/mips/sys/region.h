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
/* $Header: region.h,v 1.14.1.3 90/05/10 06:34:38 wje Exp $ */

#ifndef	_SYS_REGION_
#define	_SYS_REGION_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	Uregion 
 *
 *	List entry for list of uses of a region.
 */

#define UREGION_PROC_NULL ((short) -1)

typedef struct uregion {
		short	ur_proc_index; /* index in proc table to proc */
	} ureg_t;

/*
 * Per region descriptor.  One is allocated for
 * every active region in the system.
 */

typedef	struct	region	{
	short	r_flags;	/* Various flags defined below. */
	short	r_usesz;	/* number of entries allocated to r_use */
	int	r_listsz;	/* Number of page tables allocated */
				/* to r_list.	*/
	int	r_pgsz;		/* size in pages */
	int	r_pgoff;	/* offset in pages to first page in region */
				/* this is used for regions that grow down */
	int	r_nvalid;	/* number of valid pages in region */
	short	r_refcnt;	/* number of users pointing at region */
	short	r_type;		/* type of region */
	short	r_waitcnt;	/* number of processes waiting for load */
	short	r_noswapcnt;	/* Count of nbr of processes which have	*/
				/* requested that this region not be	*/
				/* swapped out.				*/
	int	r_filesz;	/* Size in bytes of section of file	*/
				/* from which this region is loaded.	*/
	pde_t	**r_list;	/* Pointer to a list of pointers to	*/
				/* page tables and dbd's.		*/
	struct vnode   *r_vptr;	/* vnode pointer where the blocks are */
	long           nodeid;	/* nodeid from vp attributes */
	struct region  *r_forw;	/* forward link */
	struct region  *r_back;	/* backward link */
	ureg_t	*r_use;		/* first entry in list of uses */
} reg_t;

/*
 * Region flags
 */
#define	RG_NOFREE	0x0001	/* Don't free region on last detach */
#define	RG_DONE		0x0002	/* Region is initialized */
#define	RG_NOSHARE	0x0004	/* Don't share the region. */
#define RG_LOCK		0x0008	/* Region is locked */
#define RG_WANTED	0x0010	/* Wakeup rp after clearing RG_LOCK */
#define RG_WAITING	0x0020	/* Wakeup rp->r_flags when RG_DONE is set */
#define XREMOTE		0x0040	/* remote region entry */

#define	RG_MMAP		0x1000	/* Has some mmapped pages.	*/

/*
 * Region types
 */
#define	RT_UNUSED	0	/* Region not being used.	*/
#define	RT_PRIVATE	1	/* Private (non-shared) region. */
#define RT_STEXT	2	/* Shared text region */
#define RT_SHMEM	3	/* Shared memory region */

/*	Each process has a number of pregions which describe the
 *	regions which are attached to the process.
 */

typedef struct pregion {
	struct region	*p_reg;		/* Pointer to the region.	*/
	caddr_t		p_regva;	/* User virtual address of	*/
					/* region.			*/
	short		p_flags;	/* Flags.			*/
	short		p_type;		/* Type.			*/
} preg_t;

/*	Pregion flags.
 */

#define	PF_RDONLY	0x0001		/* Read-only attach.		*/

/*	Pregion types.
 */

#define	PT_UNUSED	0x00		/* Unused region.		*/
#define	PT_TEXT		0x01		/* Text region.			*/
#define	PT_DATA		0x02		/* Data region.			*/
#define	PT_STACK	0x03		/* Stack region.		*/
#define	PT_SHMEM	0x04		/* Shared memory region.	*/
					/* Space was Double mapped memory.*/
#define	PT_LIBTXT	0x06		/* Shared library text region.	*/
#define	PT_LIBDAT	0x07		/* Shared library data region.	*/
#define PT_GR 		0x08		/* Graphics region 		*/

/*
 *	Kernel global variables
 */
extern preg_t	nullpregion;		/* A null preg_t. */
extern int	rlist_lock;		/* Lock for the region list.	*/
extern int	pregpp;			/* Number of pregions per	*/
					/* process including null one.	*/

extern reg_t	region[];	/* Global array of regions */
extern reg_t	ractive;	/* List of active regions */
extern int	ractive_count;	/* Number of active regions */
extern reg_t	rfree;		/* List of free regions */
extern reg_t	sysreg;		/* System region. */
extern reg_t	nullregion;	/* A null region. */

reg_t		*allocreg();	/* region allocator */
void		freereg();	/* region free routine */
preg_t		*attachreg();	/* Attach region to process. */
void		detachreg();	/* Detach region from process. */
reg_t		*dupreg();	/* Duplicate region (fork). */
int		growreg();	/* Grow region. */
int		loadreg();	/* Load region from file. */
int		mapreg();	/* Map region to 413 file. */
preg_t		*findreg();	/* Find region from virtual address. */
preg_t		*findpreg();	/* Find pregion of given type. */
void		chgprot();	/* Change protection for region. */
void		reginit();	/* Initialize the region table. */

/*	The minimum number of regions for a process.  Including one region
**	each for the text, data, and stack plus one for extra (e.g. graphics)
**	plus one null region pointer to indicate the end of the pregion list.
*/
#define	MINPREGPP	(3 + 1 + 1)


#endif	_SYS_REGION_
