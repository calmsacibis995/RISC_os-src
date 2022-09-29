#ident "$Header: fstbl.c,v 1.3 90/03/27 13:17:34 beacker Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * fstbl.c -- file structure tables
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"

#ifdef EFS
extern int _efs_init(), _efs_open(), _efs_read(), _efs_write(),
	_efs_close(), _efs_checkfs();
#endif EFS

extern int _nulldev();
extern int _nodev();

#ifdef BSD_CODE
extern int _bfsopen(), _bfsread();
extern int _bootpinit(), _bootpopen(), _bootpread(), _bootpclose();
extern int _nfsinit(), _nfsopen(), _nfsread(), _nfsclose();
#endif
extern int _dvhopen(), _dvhread(), _dvhclose();
extern int _tpdopen(), _tpdread(), _tpdclose();
#ifndef PROM
#ifdef BSD_CODE
extern int _bsd42init(), _bsd42open(), _bsd42read(), _bsd42close();
#endif
#ifdef SYSV
extern int _att52init(), _att52open(), _att52read(), _att52write(),
	_att52close();
#endif
#endif
#ifdef NCP
extern int _ncpinit(), _ncpopen(), _ncpread(), _ncpwrite(), _ncpioctl(),
	_ncpclose();
#endif NCP

/*
 * _fs_table -- file structure routine switch
 *
 * W A R N I N G :
 * ---------------
 * this table is indexed by DTFS_* constants in saio.h, any
 * changes here must be reflected in the DTFS_* constants
 *
 * read and write routines should return number characters transferred
 */
struct fs_table _fs_table[] = {
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#ifdef BSD_CODE
	{	/* DTFS_BFS */
		_nulldev,	_bfsopen,	_bfsread,
		_nulldev,	_nulldev,	_nulldev,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif BSD_CODE
	{	/* DTFS_DVH */
		_nulldev,	_dvhopen,	_dvhread,
		_nodev,		_nulldev,	_dvhclose,
#ifdef EFS
		_nodev
#endif EFS
	},
	{	/* DTFS_TPD */
		_nulldev,	_tpdopen,	_tpdread,
		_nodev,		_nulldev,	_tpdclose,
#ifdef EFS
		_nodev
#endif EFS
	},
#ifdef NCP
	{	/* DTFS_NCP */
		_ncpinit,	_ncpopen,	_ncpread,
		_ncpwrite,	_ncpioctl,	_ncpclose,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif NCP
#if defined (BSD_CODE) && !defined (PROM)
	{	/* DTFS_BSD42 */
		_bsd42init,	_bsd42open,	_bsd42read,
		_nodev,		_nulldev,	_bsd42close,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif BSD_CODE
#if defined (SYSV) && !defined (PROM)
	{	/* DTFS_SYSV */
		_att52init,	_att52open,	_att52read,
		_att52write,	_nulldev,	_att52close,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif SYSV
#ifdef BSD_CODE
	{	/* DTFS_BOOTP */
		_bootpinit,	_bootpopen,	_bootpread,
		_nulldev,	_nulldev,	_bootpclose,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif BSD_CODE
#ifdef EFS
	{	/* DTFS_EFS */
		_efs_init,	_efs_open,	_efs_read,
		_efs_write,	_nulldev,	_efs_close,
		_efs_checkfs
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev
	},
#endif
#ifdef BSD_CODE
	{	/* DTFS_NFS */
		_nfsinit,	_nfsopen,	_nfsread,
		_nulldev,	_nulldev,	_nfsclose,
#ifdef EFS
		_nodev
#endif EFS
	},
#else
	{	/* DTFS_NONE */
		_nulldev,	_nodev,		_nodev,
		_nodev,		_nodev,		_nodev,
#ifdef EFS
		_nodev
#endif EFS
	},
#endif BSD_CODE
};

int _nfstypes[] = {(sizeof(_fs_table)/sizeof(_fs_table[0]))};
