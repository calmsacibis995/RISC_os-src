#ident "$Header: bootp.h,v 1.3 90/02/28 22:07:33 chungc Exp $"
/* $Copyright$ */

/*
 * Bootstrap Protocol (BOOTP).  RFC 951.
 */

struct bootp {
	u_char	bp_op;		/* packet opcode type */
#define	BOOTREQUEST	1
#define	BOOTREPLY	2
	u_char	bp_htype;	/* hardware addr type */
	u_char	bp_hlen;	/* hardware addr length */
	u_char	bp_hops;	/* gateway hops */
	u_long	bp_xid;		/* transaction ID */
	u_short	bp_secs;	/* seconds since boot began */	
	u_short	bp_unused;
	iaddr_t	bp_ciaddr;	/* client IP address */
	iaddr_t	bp_yiaddr;	/* 'your' IP address */
	iaddr_t	bp_siaddr;	/* server IP address */
	iaddr_t	bp_giaddr;	/* gateway IP address */
	u_char	bp_chaddr[16];	/* client hardware address */
	u_char	bp_sname[64];	/* server host name */
	u_char	bp_file[128];	/* boot file name */
	u_char	bp_vend[64];	/* vendor-specific area */
};

/*
 * UDP port numbers, server and client.
 */
#define	IPPORT_BOOTPS		67
#define	IPPORT_BOOTPC		68

/*
 * "vendor" data permitted for Stanford boot clients.
 */
struct vend {
	u_char	v_magic[4];	/* magic number */
	u_long	v_flags;	/* flags/opcodes, etc. */
	u_char	v_unused[56];	/* currently unused */
};

#define	VM_STANFORD	"STAN"	/* v_magic for Stanford */

/* v_flags values */
#define	VF_PCBOOT	1	/* an IBMPC or Mac wants environment info */
#define	VF_HELP		2	/* help me, I'm not registered */

/* TFTP connection state */

struct tftp_state {
	int ts_blkno;		/* Last ACKed block for current TFTP conn */
	int ts_eof;		/* Final block has been recvd from TFTP */
	char *ts_data;		/* Pointer to TFTP data buffer */
	int ts_offset;		/* offset from ts_data to first unread byte */
	int ts_count;		/* Count of unread bytes in TFTP buffer */
	short ts_srvport;	/* Server UDP port number for TFTP */
	short ts_myport;	/* Client UDP port number for TFTP */
	struct tp_entry ts_ent;	/* tpd entry (if TPD format file) */
};

struct tftpdmsg {
	struct	tftphdr th;		/* Header */
	char	data[SEGSIZE-1];	/* TFTP SEGSIZE = 512 */
};
