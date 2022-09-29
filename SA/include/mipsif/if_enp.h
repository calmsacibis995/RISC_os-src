#ident "$Header: if_enp.h,v 1.4 90/01/23 13:16:33 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 *	Copyright (c) 1984 by Communication Machinery Corporation
 *
 *	This file contains material which is proprietary to
 *	Communication Machinery Corporation (CMC) and which
 *	may not be divulged without the written permission
 *	of CMC.
 *
 *	ENP-10 Ram Definition
 *
 *	3/15/85 Jon Phares
 *	Update 7/10/85 S. Holmgren
 *	ENP-10 update 7/21/85 J. Mullen
 *	ENP-20 update 8/11/85 J. Mullen
 * 
 */

#define K		*1024

#define ENPSIZE		(124 K)		/* VME bus space allocated to enp */
#define MINPKTSIZE	60		/* minimum ethernet packet size */

/*
 * Note: paged window (4 K) is identity mapped by ENP kernel to provide
 * 124 K contiguous RAM (as reflected in RAM_SIZE
 */
#define RAM_WINDOW	(128 K)
#define IOACCESS_WINDOW (4 K)
#define FIXED_WINDOW	(RAM_WINDOW - IOACCESS_WINDOW)
#define RAMROM_SWAP	(4 K)
#define RAM_SIZE	(FIXED_WINDOW - RAMROM_SWAP)

#define HOST_RAMSIZE	(48 K)
#define ENP_RAMSIZE	(20 K)

/*
 * Definitions to force 16 bit references
 */
#ifdef MIPSEB
struct uint { unsigned short	hi; unsigned short lo; };
#endif MIPSEB
#ifdef MIPSEL
struct uint { unsigned short	lo; unsigned short hi; };
#endif MIPSEL
#define	UINT	struct uint

#define	UINT_uint(lhs, rhs) \
	{ \
		register volatile UINT *lhsp = &(lhs); \
		register unsigned rhstmp = (unsigned)(rhs); \
		lhsp->hi = HI16(rhstmp); \
		wbflush(); \
		lhsp->lo = LO16(rhstmp); \
		wbflush(); \
	}

#define	uint_UINT(type, lhs, rhs) \
	{ \
		register volatile UINT *rhsp = &(rhs); \
		(lhs) = (type)((rhsp->hi << 16) | rhsp->lo); \
	}

#define	UINT_UINT(lhs, rhs) \
	{ \
		register volatile UINT *lhsp = &(lhs); \
		register volatile UINT *rhsp = &(rhs); \
		lhsp->hi = rhsp->hi; \
		wbflush(); \
		lhsp->lo = rhsp->lo; \
		wbflush(); \
	}

/* ...top of 4K local i/o space for ENP */

struct iow10 {
	char	pad1[0x81];
	/*
	 * on write:
	 *	causes an interrupt on the host at the vector written
   	 * on read:
	 *	returns the most significant 8 bits of the slave address
	 */
	char	vector;
	char	pad2[0x1F];
	char	csrarr[0x1E];
	char	pad3[2];
	char	ier;		/* intr. enable reg., 0x80 == enable,0 == off*/
	char	pad4[1];
	char	tir;		/* transmit intr. (Level 4 INP autovector) */
	char	pad5[1];
	char	rir;		/* receive intr. (Level 5 INP autovector) */
	char	pad6[1];
	char	uir;		/* utility intr. (Level 1 INP autovector) */
	char	pad7[7];
	char	mapfirst4k;	/* bit 7 set means ram, clear means rom */
	char	pad8[0x11];
	char	exr;		/* exception register, see bit defines above */
	char	pad9[0xD1F];
	char	hst2enp_interrupt;	/* R or W interrupts ENP */
	char	pad10[0xFF+0xFE];
	char	hst2enp_reset;	/* R or W resets ENP */
};

struct ether_addr {
	u_char	ea_addr[6];
};
#define ETHADDR		struct ether_addr

struct ethlist
{
	UINT	e_listsize;		/* active addr entries */
	ETHADDR	e_baseaddr;		/* addr lance is working with */
	ETHADDR e_addrs[16];		/* possible addresses */
};
#define	ETHLIST	struct ethlist

struct enpstat
{
	UINT e_xmit_successful;		/* Successful transmissions */
	UINT e_mult_retry;		/* multiple retries on xmit */
	UINT e_one_retry;		/* single retries */
	UINT e_fail_retry;		/* too many retries */
	UINT e_deferrals;		/* xmit delayed due to active medium */
	UINT e_xmit_buff_err;		/* xmit chaining failed, can't happen */
	UINT e_silo_underrun;		/* transmit data fetch failed */
	UINT e_late_coll;		/* collision after xmit */
	UINT e_lost_carrier;
	UINT e_babble;			/* xmit length > 1518 */
	UINT e_no_heartbeat;		/* transceiver mismatch, not an error */
	UINT e_xmit_mem_err;
	UINT e_rcv_successful;		/* good receptions */
	UINT e_rcv_missed;		/* no recv buff available */
	UINT e_crc_err;			/* checksum failed */
	UINT e_frame_err;		/* crc error && data len != 0 mod 8 */
	UINT e_rcv_buff_err;		/* rcv chaining failed, can't happen */
	UINT e_silo_overrun;		/* receive data store failed */
	UINT e_rcv_mem_err;
};
#define	ENPSTAT	struct enpstat

struct ring
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;
	UINT	r_slot[1];
};
#define	RING	struct ring

struct ring32
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;			/* to make VAXen happy */
	UINT	r_slot[32];
};
#define	RING32	struct ring32

/*
 * 	ENP Ram data layout
 *
 *	If you don't put it here - it isn't there
 *
 */
struct enpdevice {
	char	enp_ram_rom[4 K];
	union {
		char	all_ram[RAM_SIZE];
		struct {
			unsigned short t_go;
			unsigned short t_csr;
			UINT t_pstart;
		} t;
		struct {
			char	nram[RAM_SIZE - (HOST_RAMSIZE + ENP_RAMSIZE)];
			char	hram[HOST_RAMSIZE];
			char	kram[ENP_RAMSIZE];
		} u_ram;
		struct {
			char	pad7[ 0x100 ];	/* starts 0x1100 - 0x2000 */
			short	e_enpstate;		
			short	e_enpmode;		
			UINT	e_enpbase;		
			UINT	e_enprun;

			RING32	h_toenp;		
			RING32	h_hostfree;		
			RING32	e_tohost;		
			RING32 	e_enpfree;		

			ENPSTAT	e_stat;
			ETHLIST	e_netaddr;		
		} iface;
	} enp_u;
	struct iow10	enp_iow;
};

#define	ENPDEVICE	struct enpdevice

/*
 * enp csr bit defs
 */
#define	ENPCSR_ONLINE	0x2		/* entered application firmware */
#define	ENPCSR_RDY	0x4		/* reset complete */
#define	ENPCSR_OVPROM	0x8		/* don't transfer to prom on reset */
#define	ENPCSR_IE	0x40		/* interrupt enable for bus debug */
#define	ENPCSR_ERR	0x8000		/* enp detected error */

#define	enp_ram		enp_u.all_ram
#define	enp_nram	enp_u.u_ram.nram
#define	enp_hram	enp_u.u_ram.hram
#define	enp_kram	enp_u.u_ram.kram
#define	enp_go		enp_u.t.t_go
#define	enp_csr		enp_u.t.t_csr
#define	enp_prog_start	enp_u.t.t_pstart
#define enp_state	enp_u.iface.e_enpstate
#define enp_mode	enp_u.iface.e_enpmode
#define enp_base	enp_u.iface.e_enpbase
#define enp_enprun	enp_u.iface.e_enprun
#define enp_toenp	enp_u.iface.h_toenp
#define enp_hostfree	enp_u.iface.h_hostfree
#define enp_tohost	enp_u.iface.e_tohost
#define enp_enpfree	enp_u.iface.e_enpfree
#define enp_stat	enp_u.iface.e_stat
#define enp_addr	enp_u.iface.e_netaddr

#define ENPVAL		0xff	/* poke in enp_iow.hst2enp_interrupt */
#define RESETVAL	0x00	/* poke in enp_iow.enp2hst_clear_intr */

#define INTR_ENP(addr)		addr->enp_iow.hst2enp_interrupt = ENPVAL

/*
 * state bits
 */
#define S_ENPRESET	01		/* enp is in reset state */
#define S_ENPRUN	02		/* enp is in run state */

/*
 * mode bits
 */
#define E_SWAP16		0x1		/* swap two octets within 16 */
#define E_SWAP32		0x2		/* swap 16s within 32 */
#define E_SWAPRD		0x4		/* swap on read */
#define E_SWAPWRT		0x8		/* swap on write */
#define E_DMA			0x10		/* enp does data moving */

#define E_EXAM_LIST		0x80000000	/* enp should examine addr list */

/*
 * 	Download ioctl definitions
 */

#define mkioctl(type,value) (0x20000000|('type'<<8)|value)

#define ENPIOGO		mkioctl( S,1 )		/* start the enp */
#define ENPIORESET	mkioctl( S,2 )		/* reset the enp */

/*
 * 	The ENP Data Buffer Structure
 */
struct bcb
{
	struct BCB *b_link;
	short	b_stat;
	short	b_len;
	UINT	b_addr;
	short	b_msglen;
	short	b_reserved;
};
#define	BCB	struct bcb

/*
 * BCB b_stat field defines
 */
#define	BCB_DONE	0x8000		/* buffer complete */
#define	BCB_ERR		0x4000		/* error summary */
#define	BCB_FRAME	0x2000		/* framing error */
#define	BCB_OFLO	0x1000		/* silo overflow */
#define	BCB_CRC		0x0800		/* crc error */
#define	BCB_RXBUF	0x0400		/* rx buffer err */
#define	BCB_STP		0x0200		/* start of packet */
#define	BCB_ENP		0x0100		/* end of packet */
#define	BCB_MEMERR	0x0002		/* memory error */
#define	BCB_MISSED	0x0001		/* missed packet */
