#ident "$Header: bfs.h,v 1.3 90/01/16 15:20:34 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Definitions for boot file server
 */

#define	BFSREV		1		/* protocol revision */

/*
 * NOTE: BFS_MAXPATH+ BFS_MAXDATA + sizeof(struct bfshdr) must
 * be less than ETHERMTU (1500 bytes)
 */
#define	BFS_MAXPATH	255		/* max bfs pathname length */
#define	BFS_MAXDATA	1024		/* max data in a bfs packet */
#define	BFS_MAXSERVER	15		/* max length of server name */
#define	BFS_MAXTRIES	8		/* max attempts to read a block */
#ifdef DEBUG
/* need some time to play around when debugging protocol */
#define	BFS_REXMIT	400		/* timeout period */
#else
#define	BFS_REXMIT	3		/* timeout period */
#endif

struct bfshdr {
	char	bh_rev;				/* protocol revision */
	char	bh_type;			/* packet type */
	u_short bh_pathlen;			/* pathname length */
	short	bh_datalen;			/* data length */
	short	bh_pad;				/* pad to int boundry */
	u_int	bh_offset;			/* file offset */
	int	bh_flags;			/* open flags to use */
	char	bh_server[BFS_MAXSERVER+1];	/* responding server */
};

/*
 * bfs packet types
 * (writes currently aren't fully implemented)
 */
#define	BFSPT_ENQUIRE	1		/* enquire for file availability */
#define	BFSPT_ENQRPY	2		/* reply to enquire */
#define	BFSPT_READ	3		/* request for data read */
#define	BFSPT_RDRPY	4		/* reply to data read request */
#define	BFSPT_WRITE	5		/* request to write data */
#define	BFSPT_WTRPY	6		/* reply to write data request */
#define	BFSPT_ERROR	7		/* error message reply */
#define	BFSPT_ENQFWD	8		/* gateway'ed ENQUIRE request */

/*
 * bfs udp ports
 * (ports less than IPPORT_RESERVED require bfsd server to run setuid root)
 */
#define	BFSPORT_PROM	2200
#define	BFSPORT_SERVER	2201
