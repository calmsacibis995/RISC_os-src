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
/* $Header: dkio.h,v 1.11.4.6.1.3.1.6 90/12/20 19:31:05 beacker Exp $ */

#ifndef	_SYS_DKIO_
#define	_SYS_DKIO_	1


/*	dkio.h	6.1	83/07/29	*/
/*
 * Structures and definitions for disk io control commands
 */


/* disk io control commands */
#define _DIOC_(x)	(('d'<<8) | x)
#define DIOCFMTTRK	_DIOC_(1)	/* Format and Map */
#define DIOCVFYSEC	_DIOC_(2)	/* Verify sectors */
#define DIOCGETCTLR	_DIOC_(3)	/* Get ctlr info */
#define DIOCDIAG	_DIOC_(4)	/* Perform diag */
#define DIOCSETDP	_DIOC_(5)	/* Set devparams */
#define DIOCGETVH	_DIOC_(6)	/* Get volume header */

/* BSD compatibility for now */
#define DIOCFMTMAP	_DIOC_(1)	/* Format and Map */
#define DIOCNOECC	_DIOC_(20)	/* Disable/Enable ECC */
#define DIOCRDEFECTS	_DIOC_(21)	/* Read meadia defects */
#define DIOCINITVH	_DIOC_(22)	/* Initialize volume header */

#define DIOCRECONFIG	_DIOC_(23)	/* SETVH, *and* reconfigure drive */
#define DIOCSOFTCNT     _DIOC_(24)      /* get/set soft error count */
#define DIOCSEEK        _DIOC_(25)      /* seek disk */
#define DIOCWRTVFY      _DIOC_(26)      /* turn on/off write verify flag */
#define DIOCREMOVE      _DIOC_(27)      /* setup for disk removal */
#define DIOCDISKCACHE   _DIOC_(28)      /* enable/disable disk cache */
#define DIOCDISKGEOM    _DIOC_(29)      /* get disk geometry */

/* vdisk io control commands */
#define DIOCGETDKTAB	_DIOC_(30)	/* Get dktab */
#define DIOCADDDKTAB	_DIOC_(31)	/* Add dktab */
#define DIOCDELDKTAB	_DIOC_(32)	/* Del dktab */
#define DIOCSETATTR	_DIOC_(33)	/* Set virtual disk attributes */

#define DIOCTRKID	_DIOC_(34)	/* Read track information */
#define DIOCGETSIZE	_DIOC_(35)	/* Get size of partition in blocks */
#define DIOCPARTITION    _DIOC_(36)      /* get partition status */

#define DIOCGETATTR	_DIOC_(37)	/* Get virtual disk attributes */
#define DIOCVDINIT	_DIOC_(38)	/* InitiGet virtual disk attributes */

/* Reserved for SGI. */
#define DIOCSETVH	_DIOC_(7)	/* SGI - SETVH */
/* #define DIOCRESERVED	_DIOC_(8)	/* SGI - DRIVETYPE */
#define DIOCTEST	_DIOC_(9)	/* SGI - TEST  */
/* #define DIOCRESERVED	_DIOC_(10)	/* SGI - FORMAT */
/* #define DIOCRESERVED	_DIOC_(11)	/* SGI - SENSE */
/* #define DIOCRESERVED	_DIOC_(12)	/* SGI - SELECT */
#define DIOCRDCAP	_DIOC_(13)	/* SGI - READCAPACITY */
/* #define DIOCRESERVED	_DIOC_(14)	/* SGI - RDEFECTS */
/* #define DIOCRESERVED	_DIOC_(15)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(16)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(17)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(18)	/* SGI - unused */
/* #define DIOCRESERVED	_DIOC_(19)	/* SGI - unused */

/*
 * Added from System 5.3:
 *	Ioctls to disk drivers will pass the address to this
 *	structure which the driver handle as appropriate.
 *
 */
struct io_arg {
	int retval;
	unsigned long sectst;
	unsigned long memaddr;
	unsigned long datasz;
};

/*
 * return values returned in retval when EIO returned
 */
#define DIOC_BADSIZE	1
#define DIOC_NOTVOLHDR	2
#define DIOC_DISKBUSY	3
#define DIOC_OPERR	4
#define DIOC_EFAULT	5
#define DIOC_EINVAL	6

/*
 * driver ioctl() commands not supported
 */
#define VIOC			('V'<<8)
#define V_PREAD			(VIOC|1)	/* Physical Read */
#define V_PWRITE		(VIOC|2)	/* Physical Write */
#define V_PDREAD		(VIOC|3)	/* Read of Physical 
						 * Description Area */
#define V_PDWRITE		(VIOC|4)	/* Write of Physical 
						 * Description Area */
#define V_GETSSZ		(VIOC|5)	/* Get the sector size of media */

struct  devtable {
        int     nmajr;          /* Number of major numbers for this device */
        int     ncpermjr;       /* number of controllers per major number */
        int     ndrives;        /* number of drives per controller */
        int     *mult;          /* Array of multiple major numbers */
};

struct bootdevtbl {
        char    *name;          /* name that is typed in        */
	int	(*has_func)();	/* function to determine if module is there */
	
};

/*
 * The following structs are parameters to various driver ioctls
 * for disk formatting, etc.
 */

/* defines for SCSI read defects DIOCRDEFECTS */

typedef struct defect_header {
        unsigned short reserved;
        unsigned short list_size;
} DEFECT_HEADER;

typedef struct defect_entry {
        unsigned long   def_cyl         : 24;
        unsigned long   def_head        : 8;
        unsigned long   def_sect;
} DEFECT_ENTRY;

#define HEADER_DEFECTS          0
#define GROWTH_DEFECTS          1
#define PRIMARY_DEFECTS         2
#define ALL_DEFECTS             3

/* DIOCDISKGEOM structure */

typedef struct geometry_info {
        unsigned long   geom_cyl;	/* nr. of cylinders */
        unsigned short   geom_head;	/* nr. of heads */
        unsigned short   geom_spt;	/* nr. of sectors per track */
        unsigned short   geom_bps;	/* nr. of bytes per sector */
        unsigned short   geom_tpz;	/* nr. of tracks per zone */
        unsigned short   geom_aspz;	/* nr. of alternate sectors per zone */
        unsigned short   geom_atpz;	/* nr. of alternate tracks per zone */
        unsigned short   geom_atpv;	/* nr. of alternate tracks per volume */
        unsigned short   geom_ilv;	/* interleave */
        unsigned short   geom_tsf;	/* track skew factor */
        unsigned short   geom_csf;	/* cylinder skew factor */
} GEOMETRY_INFO;

/*
 * controller information struct
 * returned via DIOCGETCTLR
 * mostly to determine appropriate method for bad block handling
 */
#define	CITYPESIZE	128

struct ctlr_info {
	int	ci_flags;		/* same as DP_* flags */
	char	ci_type[CITYPESIZE];	/* controller model and manuf. */
};

/*
 * information necessary to perform one of the following actions:
 * 	format a track
 *	    fmi_cyl and fmi_trk identify track to format
 *	map a track
 *	    fmi_cyl and fmi_trk identify defective track
 *	    fmi_rplcyl and fmi_rpltrk identify replacement track
 *	map a sector
 *	    fmi_cyl, fmi_trk, and fmi_sec identify defective sector
 *	    fmi_rplcyl, fmi_rpltrk, and fmi_rplsec identify replacement sector
 *	slip a sector
 *	    fmi_cyl, fmi_trk, and fmi_sec identify defective sector
 */
#define FMI_FORMAT_TRACK	1	/* format a track */
#define FMI_MAP_TRACK		2	/* map a track */
#define FMI_MAP_SECTOR		3	/* map a sector */
#define FMI_SLIP_SECTOR		4	/* slip a sector */
#define FMI_FORMAT_TRACK_WDATA	5	/* format a track with data */

struct fmt_map_info {
	int	fmi_action;		/* action desired, see FMI_ above */
	union {
	    struct {
		unsigned short ocyl;
		unsigned char otrk;
		unsigned char osec;
		unsigned short rcyl;
		unsigned char rtrk;
		unsigned char rsec;
	    } nonscsi;
	    struct {
		unsigned short intrlv;
		unsigned short tpz;
		unsigned short aspz;
		unsigned short atpv;
	    } scsidsk;
	} info;
	char *fmi_addr;
};

#define	fmi_cyl    info.nonscsi.ocyl	/* cylinder with defect or one with */
					/* track to format */
#define	fmi_trk    info.nonscsi.otrk	/* track with defect or one to format */
#define	fmi_sec    info.nonscsi.osec	/* sector with defect */
#define	fmi_rplcyl info.nonscsi.rcyl	/* replacement cylinder */
#define	fmi_rpltrk info.nonscsi.rtrk	/* replacement track */
#define	fmi_rplsec info.nonscsi.rtrk	/* replacement sector */

#define	fmi_intrlv info.scsidsk.intrlv	/* interleave factor */
#define	fmi_tpz    info.scsidsk.tpz	/* tracks per zone */
#define	fmi_aspz   info.scsidsk.aspz	/* alternate sectors per zone */
#define	fmi_atpv   info.scsidsk.atpv	/* alternate tracks per volume */

struct sdformat {
	u_char		sf_fmtdata;		/* format with defect data */
	u_char		sf_cmplst;		/* complete defect list */
	u_char		sf_lstfmt;		/* defect list format */
	u_char		sf_pattern;		/* format pattern */
	u_short		sf_intleave;		/* interleave factor */
	u_char		sf_deflen;		/* len of defect list */
	u_short		*sf_defptr;		/* pointer to defect data */
};
#define SDMAXDEFECT	256			/* max number of defect */

/*
 * information concerning media defect information placed on drive.
 * Used by the DIOCRDEFECTS ioctl
 */
#define DEFECTIVE_TRACK         0x8000
#define SYNC_PATTERN            0x1919
#define TB_PATTERH              0xf0
#define MAX_DEFECTS             4

struct md_entry {
        u_short         sync;                   /* should be SYNC_PATTERH */
        u_short         cylinder;
        u_char          head;
        u_char          unused;
        struct  defects {
                u_short         position;
                u_short         length;
        } defects[MAX_DEFECTS];
        u_char          tb;                     /* should be TB_PATTERN */
        u_char          zeros[3];
};

struct media_defect {
        u_short         md_cyl;         /* cylinder for read */
        u_short         md_trk;         /* track for read */
        struct md_entry md_entry;       /* media defect information */
};

/*
 * Contents of header found at the beginning of each sector.  Used for
 * the DIOCTRKID ioctl
 */

struct track_id{
    int sync_field:5;
    int cylno:11;
    u_char head;
    u_char sector;
    u_char duphead;
    u_char dupsector;
    u_short zeros;
};
#endif	_SYS_DKIO_
