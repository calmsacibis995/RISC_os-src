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
/* $Header: dkio.h,v 1.7.1.2.1.1.1.2 90/10/23 13:45:01 beacker Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Structures and definitions for disk io control commands
 */

/* 
 * disk io control commands  : all must be IOWR since it copies status
 * in and size out.
 */
#define BSD43_DIOCFMTMAP	BSD43__IOWR(d, 1, struct bsd43_(io_arg))
#define BSD43_DIOCVFYSEC	BSD43__IOWR(d, 2, struct bsd43_(io_arg))
#define BSD43_DIOCGETCTLR	BSD43__IOWR(d, 3, struct bsd43_(io_arg))
#define BSD43_DIOCDIAG		BSD43__IOWR(d, 4, struct bsd43_(io_arg))
#define BSD43_DIOCSETDP		BSD43__IOWR(d, 5, struct bsd43_(io_arg))
#define BSD43_DIOCGETVH		BSD43__IOWR(d, 6, struct bsd43_(io_arg))
#define BSD43_DIOCSETVH		BSD43__IOWR(d, 7, struct bsd43_(io_arg))
/* 8 - 19 reserved for SGI */
#define BSD43_DIOCNOECC		BSD43__IOWR(d, 20, struct bsd43_(io_arg))
#define BSD43_DIOCRDEFECTS	BSD43__IOWR(d, 21, struct bsd43_(io_arg))
#define BSD43_DIOCINITVH	BSD43__IOWR(d, 22, struct bsd43_(io_arg))
#define BSD43_DIOCRECONFIG	BSD43__IOWR(d, 23, struct bsd43_(io_arg))
#define BSD43_DIOCSOFTCNT	BSD43__IOWR(d, 24, struct bsd43_(io_arg))
#define BSD43_DIOCSEEK		BSD43__IOWR(d, 25, struct bsd43_(io_arg))
#define BSD43_DIOCWRTVFY	BSD43__IOWR(d, 26, struct bsd43_(io_arg))
#define BSD43_DIOCREMOVE	BSD43__IOWR(d, 27, struct bsd43_(io_arg))
#define BSD43_DIOCDISKCACHE	BSD43__IOWR(d, 28, struct bsd43_(io_arg))
#define BSD43_DIOCDISKGEOM	BSD43__IOWR(d, 29, struct bsd43_(io_arg))
#define BSD43_DIOCGETDKTAB	BSD43__IOWR(d, 30, struct bsd43_(io_arg))
#define BSD43_DIOCADDDKTAB	BSD43__IOWR(d, 31, struct bsd43_(io_arg))
#define BSD43_DIOCDELDKTAB	BSD43__IOWR(d, 32, struct bsd43_(io_arg))
#define BSD43_DIOCSETATTR	BSD43__IOWR(d, 33, struct bsd43_(io_arg))
#define BSD43_DIOCTRKID		BSD43__IOWR(d, 34, struct bsd43_(io_arg))
#define BSD43_DIOCGETSIZE	BSD43__IOWR(d, 35, struct bsd43_(io_arg))


/*
 * The following structs are parameters to various driver ioctls
 * for disk formatting, etc.
 */

struct bsd43_(io_arg) {
	int retval;
	unsigned long sectst;
	unsigned long memaddr;
	unsigned long datasz;
};

/*
 * return values returned in retval when EIO returned
 */
#define BSD43_DIOCBADSIZE    1
#define BSD43_DIOCNOTVOLHDR  2
#define BSD43_DIOCDISKBUSY   3
#define BSD43_DIOCOPERR      4
#define BSD43_DIOCEFAULT     5
#define BSD43_DIOCEINVAL     6

struct  bsd43_(devtable) {
        int     nmajr;          /* Number of major numbers for this device */
        int     ncpermjr;       /* number of controllers per major number */
        int     ndrives;        /* number of drives per controller */
        int     *mult;          /* Array of multiple major numbers */
};

struct bsd43_(bootdevtbl) {
        char    *name;          /* name that is typed in        */
        int     (*has_func)();  /* function to determine if module is there */

};

/* defines for SCSI read defects DIOCRDEFECTS */

struct bsd43_(defect_header) {
        unsigned short reserved;
        unsigned short list_size;
};

struct bsd43_(defect_entry) {
        unsigned long   def_cyl         : 24;
        unsigned long   def_head        : 8;
        unsigned long   def_sect;
};

#define BSD43_HEADER_DEFECTS          0
#define BSD43_GROWTH_DEFECTS          1
#define BSD43_PRIMARY_DEFECTS         2
#define BSD43_ALL_DEFECTS             3

/* DIOCDISKGEOM structure */

struct bsd43_(geometry_info) {
        unsigned long   geom_cyl;       /* nr. of cylinders */
        unsigned short   geom_head;     /* nr. of heads */
        unsigned short   geom_spt;      /* nr. of sectors per track */
        unsigned short   geom_bps;      /* nr. of bytes per sector */
        unsigned short   geom_tpz;      /* nr. of tracks per zone */
        unsigned short   geom_aspz;     /* nr. of alternate sectors per zone */
        unsigned short   geom_atpz;     /* nr. of alternate tracks per zone */
        unsigned short   geom_atpv;	/* nr. of alternate tracks per volume */
        unsigned short   geom_ilv;      /* interleave */
        unsigned short   geom_tsf;      /* track skew factor */
        unsigned short   geom_csf;      /* cylinder skew factor */
};



/*
 * controller information struct
 * returned via DIOCGETCTLR
 * mostly to determine appropriate method for bad block handling
 */
#define	BSD43_CITYPESIZE	128

struct bsd43_(ctlr_info) {
	int	ci_flags;		/* same as DP_* flags */
	char	ci_type[BSD43_CITYPESIZE];	/* controller model and manuf. */
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
#define BSD43_FMI_FORMAT_TRACK		1	/* format a track */
#define BSD43_FMI_MAP_TRACK		2	/* map a track */
#define BSD43_FMI_MAP_SECTOR		3	/* map a sector */
#define BSD43_FMI_SLIP_SECTOR		4	/* slip a sector */
#define BSD43_FMI_FORMAT_TRACK_WDATA	5	/* format a track with data */

struct bsd43_(fmt_map_info) {
        int     fmi_action;             /* action desired, see FMI_ above */
        union {
            struct {
                unsigned short ocyl;
                unsigned char otrk;
                unsigned char osec;
                unsigned short rcyl;
                unsigned char rtrk;
                unsigned char rsec;
            } bsd43_(nonscsi);
            struct {
                unsigned short intrlv;
                unsigned short tpz;
                unsigned short aspz;
                unsigned short atpv;
            } bsd43_(scsidsk);
    	} info;
        char *fmi_addr;
};

#define bsd43_fmi_cyl    info.bsd43_(nonscsi).ocyl   /* cyl with defect or */
					        /* one with track to format */
#define bsd43_fmi_trk    info.bsd43_(nonscsi).otrk    /* trk w/defect */
#define bsd43_fmi_sec    info.bsd43_(nonscsi).osec    /* sector with defect */
#define bsd43_fmi_rplcyl info.bsd43_(nonscsi).rcyl    /* replacement cyl */
#define bsd43_fmi_rpltrk info.bsd43_(nonscsi).rtrk    /* replacement track */
#define bsd43_fmi_rplsec info.bsd43_(nonscsi).rtrk    /* replacement sector */

#define bsd43_fmi_intrlv info.bsd43_(scsidsk).intrlv  /* interleave factor */
#define bsd43_fmi_tpz    info.bsd43_(scsidsk).tpz     /* tracks per zone */
#define bsd43_fmi_aspz   info.bsd43_(scsidsk).aspz    /* alt sectors/zone */
#define bsd43_fmi_atpv   info.bsd43_(scsidsk).atpv    /* alt tracks/volume */

struct bsd43_(sdformat) {
        u_char          sf_fmtdata;             /* format with defect data */
        u_char          sf_cmplst;              /* complete defect list */
        u_char          sf_lstfmt;              /* defect list format */
        u_char          sf_pattern;             /* format pattern */
        u_short         sf_intleave;            /* interleave factor */
        u_char          sf_deflen;              /* len of defect list */
        u_short         *sf_defptr;             /* pointer to defect data */
};
#define BSD43_SDMAXDEFECT     256                     /* max number of defect */

/*
 * information concerning media defect information placed on drive.
 */
#define BSD43_DEFECTIVE_TRACK		0x8000
#define BSD43_SYNC_PATTERN		0x1919
#define BSD43_TB_PATTERH		0xf0
#define BSD43_MAX_DEFECTS		4

struct bsd43_(md_entry) {
	u_short		sync;			/* should be SYNC_PATTERH */
	u_short		cylinder;
	u_char		head;
	u_char		unused;
	struct	bsd43_(defects)	{
		u_short		position;
		u_short		length;
	} bsd43_(defects)[BSD43_MAX_DEFECTS];
	u_char		tb;			/* should be TB_PATTERN */
	u_char		zeros[3];
};

struct bsd43_(media_defect) {
	u_short		md_cyl;			/* cylinder for read */
	u_short		md_trk;			/* track for read */
	struct bsd43_(md_entry)	bsd43_(md_entry);		/* media defect information */
};


/*
 * Contents of header found at the beginning of each sector.  Used for
 * the DIOCTRKID ioctl
 */

struct bsd43_(track_id){
    int sync_field:5;
    int cylno:11;
    u_char head;
    u_char sector;
    u_char duphead;
    u_char dupsector;
    u_short zeros;
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DIOCFMTMAP BSD43_DIOCFMTMAP
#   define DIOCVFYSEC BSD43_DIOCVFYSEC
#   define DIOCGETCTLR BSD43_DIOCGETCTLR
#   define DIOCDIAG BSD43_DIOCDIAG
#   define DIOCSETDP BSD43_DIOCSETDP
#   define DIOCGETVH BSD43_DIOCGETVH
#   define DIOCSETVH BSD43_DIOCSETVH
#   define DIOCNOECC BSD43_DIOCNOECC
#   define DIOCRDEFECTS BSD43_DIOCRDEFECTS
#   define DIOCINITVH BSD43_DIOCINITVH
#   define DIOCRECONFIG BSD43_DIOCRECONFIG
#   define DIOCSOFTCNT BSD43_DIOCSOFTCNT
#   define DIOCSEEK BSD43_DIOCSEEK
#   define DIOCWRTVFY BSD43_DIOCWRTVFY
#   define DIOCREMOVE BSD43_DIOCREMOVE
#   define DIOCDISKCACHE BSD43_DIOCDISKCACHE
#   define DIOCDISKGEOM BSD43_DIOCDISKGEOM
#   define DIOCGETDKTAB BSD43_DIOCGETDKTAB
#   define DIOCADDDKTAB BSD43_DIOCADDDKTAB
#   define DIOCDELDKTAB BSD43_DIOCDELDKTAB
#   define DIOCSETATTR BSD43_DIOCSETATTR
#   define DIOCTRKID BSD43_DIOCTRKID
#   define DIOCGETSIZE BSD43_DIOCGETSIZE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


