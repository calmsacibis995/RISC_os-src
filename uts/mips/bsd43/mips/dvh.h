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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: dvh.h,v 1.7.1.2.1.1.1.2 90/10/23 13:45:26 beacker Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * dvh.h -- disk volume header
 */

/*
 * Format for volume header information
 *
 * The volume header is a block located at the beginning of all disk
 * media.  It contains information pertaining to physical device parameters
 * and logical partition information.
 *
 * The volume header is manipulated by disk formatters/verifiers, 
 * partition builders (e.g. newfs/mkfs), and device drivers.
 *
 * A copy of the volume header is located at sector 0 of each track
 * of cylinder 0.  The volume header is constrained to be less than 512
 * bytes long.  A particular copy is assumed valid if no drive errors
 * are detected, the magic number is correct, and the 32 bit 2's complement
 * of the volume header is correct.  The checksum is calculated by initially
 * zeroing vh_csum, summing the entire structure and then storing the
 * 2's complement of the sum.  Thus a checksum to verify the volume header
 * should be 0.
 *
 * The error summary table, bad sector replacement table, and boot blocks are
 * located by searching the volume directory within the volume header.
 *
 * Tables are sized simply by the integral number of table records that
 * will fit in the space indicated by the directory entry.
 *
 * The amount of space allocated to the volume header, replacement blocks,
 * and other tables is user defined when the device is formatted.
 */

/*
 * device parameters are in the volume header to determine mapping
 * from logical block numbers to physical device addresses
 */
struct bsd43_(device_parameters) {
	u_char	dp_skew;		/* spiral addressing skew */
	u_char	dp_gap1;		/* words of 0 before header */
	u_char	dp_gap2;		/* words of 0 between hdr and data */
	u_char	dp_spare0;		/* spare space */
	u_short	dp_cyls;		/* number of cylinders */
	u_short	dp_shd0;		/* starting head vol 0 */
	u_short	dp_trks0;		/* number of tracks vol 0 */
	u_short	dp_shd1;		/* starting head vol 1 */
	u_short	dp_trks1;		/* number of tracks vol 1 */
	u_short	dp_secs;		/* number of sectors/track */
	u_short	dp_secbytes;		/* length of sector in bytes */
	u_short	dp_interleave;		/* sector interleave */
	int	dp_flags;		/* controller characteristics */
	int	dp_datarate;		/* bytes/sec for kernel stats */
	int	dp_nretries;		/* max num retries on data error */
	int	bsd43_(dp_mspw);		/* ms per word to xfer, for iostat(1) */
	int	dp_spare1;		/* spare entries */
	int	dp_spare2;
	int	dp_spare3;
};

/*
 * Device characterization flags
 * (dp_flags)
 */
#define	BSD43_DP_SECTSLIP	0x00000001	/* sector slip to spare sector */
#define	BSD43_DP_SECTFWD	0x00000002	/* forward to replacement sector */
#define	BSD43_DP_TRKFWD	0x00000004	/* forward to replacement track */
#define	BSD43_DP_MULTIVOL	0x00000008	/* multiple volumes per spindle */
#define	BSD43_DP_IGNOREERRORS	0x00000010	/* transfer data regardless of errors */
#define BSD43_DP_RESEEK	0x00000020	/* recalibrate as last resort */

/*
 * Boot blocks, bad sector tables, and the error summary table, are located
 * via the volume_directory.
 */
#define BSD43_VDNAMESIZE	8

struct bsd43_(volume_directory) {
	char	vd_name[BSD43_VDNAMESIZE];	/* name */
	int	vd_lbn;			/* logical block number */
	int	vd_nbytes;		/* file length in bytes */
};

/*
 * partition table describes logical device partitions
 * (device drivers examine this to determine mapping from logical units
 * to cylinder groups, device formatters/verifiers examine this to determine
 * location of replacement tracks/sectors, etc)
 *
 * NOTE: pt_firstlbn SHOULD BE CYLINDER ALIGNED
 */
struct bsd43_(partition_table) {		/* one per logical partition */
	int	pt_nblks;		/* # of logical blks in partition */
	int	pt_firstlbn;		/* first lbn of partition */
	int	pt_type;		/* use of partition */
};

#define	BSD43_PTYPE_VOLHDR	0		/* partition is volume header */
#define	BSD43_PTYPE_TRKREPL	1		/* partition is used for repl trks */
#define	BSD43_PTYPE_SECREPL	2		/* partition is used for repl secs */
#define	BSD43_PTYPE_RAW	3		/* partition is used for data */
#define	BSD43_PTYPE_BSD	4		/* partition is BSD file system */
#define	BSD43_PTYPE_SYSV	5		/* partition is SysV file system */
#define	BSD43_PTYPE_VOLUME	6		/* partition is entire volume */

#define	BSD43_VHMAGIC		0xbe5a941	/* randomly chosen value */
#define	BSD43_NPARTAB		16		/* 16 unix partitions */
#define	BSD43_NVDIR		15		/* max of 15 directory entries */
#define BSD43_BFNAMESIZE	16		/* max 16 chars in boot file name */

struct bsd43_(volume_header) {
	int vh_magic;				/* identifies volume header */
	short vh_rootpt;			/* root partition number */
	short vh_swappt;			/* swap partition number */
	char vh_bootfile[BSD43_BFNAMESIZE];		/* name of file to boot */
	struct bsd43_(device_parameters) vh_dp;		/* device parameters */
	struct bsd43_(volume_directory) vh_vd[BSD43_NVDIR];	/* other vol hdr contents */
	struct bsd43_(partition_table) vh_pt[BSD43_NPARTAB];	/* device partition layout */
	int vh_csum;				/* volume header checksum */
};

/*
 * The following tables are located via the volume directory
 */

/*
 * error table records media defects
 * allows for "automatic" replacement of bad blocks, and
 * better error logging
 */
#define	BSD43_ERR_SECC	0	/* soft ecc */
#define	BSD43_ERR_HECC	1	/* hard ecc */
#define	BSD43_ERR_HCSUM	2	/* header checksum */
#define	BSD43_ERR_SOTHER	3	/* any other soft errors */
#define	BSD43_ERR_HOTHER	4	/* any other hard errors */

#define	BSD43_NERRTYPES	5	/* Total number of error types */

struct bsd43_(error_table) {		/* one per defective logical block */
	int	et_lbn;		/* defective block number */
	int	et_errcount[BSD43_NERRTYPES];	/* counts for each error type */
};

/*
 * bad sector table
 * The bad sector table is used to map from bad sectors/tracks to replacement
 * sector/tracks.
 *
 * To identify available replacement sectors/tracks, allocate replacements
 * in increasing block number from a replacement partition.  When a new
 * replacement sector/track is needed scan bad sector table to determine current
 * highest replacement sector/track block number and then scan device from next
 * block until a defect free replacement sector/track is found or the end of
 * replacement partition is reached.
 *
 * If bt_rpltype == BSTTYPE_TRKFWD, then bt_badlbn refers to the bad logical
 * block within the bad track, and bt_rpllbn refers to the first sector of 
 * the replacement track.
 *
 * If bt_rpltype == BSTTYPE_SLIPSEC or bt_rpltype == BSTTYPE_SLIPBAD, then
 * bt_rpllbn has no meaning.
 */
struct bsd43_(bst_table) {
	int	bt_badlbn;	/* bad logical block */
	int	bt_rpllbn;	/* replacement logical block */
	int	bt_rpltype;	/* replacement method */
};

/*
 * replacement types
 */
#define	BSD43_BSTTYPE_EMPTY	0	/* slot unused */
#define	BSD43_BSTTYPE_SLIPSEC	1	/* sector slipped to next sector */
#define	BSD43_BSTTYPE_SECFWD	2	/* sector forwarded to replacment sector */
#define	BSD43_BSTTYPE_TRKFWD	3	/* track forwarded to replacement track */
#define BSD43_BSTTYPE_SLIPBAD	4	/* sector reserved for slipping has defect */
#define BSD43_BSTTYPE_NOTREPL	5	/* bad sector not handled */
#define BSD43_BSTTYPE_RUNTBAD	6	/* runt sector has defect */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BFNAMESIZE BSD43_BFNAMESIZE
#   define BSTTYPE_EMPTY BSD43_BSTTYPE_EMPTY
#   define BSTTYPE_NOTREPL BSD43_BSTTYPE_NOTREPL
#   define BSTTYPE_SECFWD BSD43_BSTTYPE_SECFWD
#   define BSTTYPE_SLIPBAD BSD43_BSTTYPE_SLIPBAD
#   define BSTTYPE_SLIPSEC BSD43_BSTTYPE_SLIPSEC
#   define BSTTYPE_TRKFWD BSD43_BSTTYPE_TRKFWD
#   define DP_IGNOREERRORS BSD43_DP_IGNOREERRORS
#   define DP_MULTIVOL BSD43_DP_MULTIVOL
#   define DP_RESEEK BSD43_DP_RESEEK
#   define DP_SECTFWD BSD43_DP_SECTFWD
#   define DP_SECTSLIP BSD43_DP_SECTSLIP
#   define DP_TRKFWD BSD43_DP_TRKFWD
#   define ERR_HCSUM BSD43_ERR_HCSUM
#   define ERR_HECC BSD43_ERR_HECC
#   define ERR_HOTHER BSD43_ERR_HOTHER
#   define ERR_SECC BSD43_ERR_SECC
#   define ERR_SOTHER BSD43_ERR_SOTHER
#   define NERRTYPES BSD43_NERRTYPES
#   define NPARTAB BSD43_NPARTAB
#   define NVDIR BSD43_NVDIR
#   define PTYPE_BSD BSD43_PTYPE_BSD
#   define PTYPE_RAW BSD43_PTYPE_RAW
#   define PTYPE_SECREPL BSD43_PTYPE_SECREPL
#   define PTYPE_SYSV BSD43_PTYPE_SYSV
#   define PTYPE_TRKREPL BSD43_PTYPE_TRKREPL
#   define PTYPE_VOLHDR BSD43_PTYPE_VOLHDR
#   define PTYPE_VOLUME BSD43_PTYPE_VOLUME
#   define VDNAMESIZE BSD43_VDNAMESIZE
#   define VHMAGIC BSD43_VHMAGIC
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


