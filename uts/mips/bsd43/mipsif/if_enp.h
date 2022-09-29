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
/* $Header: if_enp.h,v 1.7.1.2 90/05/10 04:43:41 wje Exp $ */
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
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define BSD43_K		*1024

#define BSD43_ENPSIZE		(124 BSD43_K)		/* VME bus space allocated to enp */
#define BSD43_MINPKTSIZE	60		/* minimum ethernet packet size */

/*
 * Note: paged window (4 K) is identity mapped by ENP kernel to provide
 * 124 K contiguous RAM (as reflected in RAM_SIZE
 */
#define BSD43_RAM_WINDOW	(128 BSD43_K)
#define BSD43_IOACCESS_WINDOW (4 BSD43_K)
#define BSD43_FIXED_WINDOW	(BSD43_RAM_WINDOW - BSD43_IOACCESS_WINDOW)
#define BSD43_RAMROM_SWAP	(4 BSD43_K)
#define BSD43_RAM_SIZE	(BSD43_FIXED_WINDOW - BSD43_RAMROM_SWAP)

#define BSD43_HOST_RAMSIZE	(48 BSD43_K)
#define BSD43_ENP_RAMSIZE	(20 BSD43_K)

/*
 * Definitions to force 16 bit references
 */
#ifdef MIPSEB
struct bsd43_(uint) { unsigned short	hi; unsigned short lo; };
#endif MIPSEB
#ifdef MIPSEL
struct bsd43_(uint) { unsigned short	lo; unsigned short hi; };
#endif MIPSEL
#define	BSD43_UINT	struct bsd43_(uint)

#define	BSD43_HI16(x)	((unsigned)(x)>>16)
#define	BSD43_LO16(x)	(x)
#define	BSD43_UINT_uint(lhs, rhs) \
	{ \
		register volatile BSD43_UINT *bsd43_(lhsp) = &(lhs); \
		register unsigned rhstmp = (rhs); \
		bsd43_(lhsp)->hi = BSD43_HI16(rhstmp); \
		bsd43_(wbflush)(); \
		bsd43_(lhsp)->lo = BSD43_LO16(rhstmp); \
		bsd43_(wbflush)(); \
	}

#define	bsd43_uint_UINT(type, lhs, rhs) \
	{ \
		register volatile BSD43_UINT *bsd43_(rhsp) = &(rhs); \
		(lhs) = (type)((bsd43_(rhsp)->hi << 16) | bsd43_(rhsp)->lo); \
	}

#define	BSD43_UINT_UINT(lhs, rhs) \
	{ \
		register volatile BSD43_UINT *bsd43_(lhsp) = &(lhs); \
		register volatile BSD43_UINT *bsd43_(rhsp) = &(rhs); \
		bsd43_(lhsp)->hi = bsd43_(rhsp)->hi; \
		bsd43_(wbflush)(); \
		bsd43_(lhsp)->lo = bsd43_(rhsp)->lo; \
		bsd43_(wbflush)(); \
	}

/* ...top of 4K local i/o space for ENP */

struct bsd43_(iow10) {
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

struct bsd43_(ether_addr) {
	u_char	ea_addr[6];
};
#define BSD43_ETHADDR		struct bsd43_(ether_addr)

struct bsd43_(ethlist)
{
	BSD43_UINT	e_listsize;		/* active addr entries */
	BSD43_ETHADDR	e_baseaddr;		/* addr lance is working with */
	BSD43_ETHADDR e_addrs[16];		/* possible addresses */
};
#define	BSD43_ETHLIST	struct bsd43_(ethlist)

struct bsd43_(enpstat)
{
	BSD43_UINT e_xmit_successful;		/* Successful transmissions */
	BSD43_UINT e_mult_retry;		/* multiple retries on xmit */
	BSD43_UINT e_one_retry;		/* single retries */
	BSD43_UINT e_fail_retry;		/* too many retries */
	BSD43_UINT e_deferrals;		/* xmit delayed due to active medium */
	BSD43_UINT e_xmit_buff_err;		/* xmit chaining failed, can't happen */
	BSD43_UINT e_silo_underrun;		/* transmit data fetch failed */
	BSD43_UINT e_late_coll;		/* collision after xmit */
	BSD43_UINT e_lost_carrier;
	BSD43_UINT e_babble;			/* xmit length > 1518 */
	BSD43_UINT e_no_heartbeat;		/* transceiver mismatch, not an error */
	BSD43_UINT e_xmit_mem_err;
	BSD43_UINT e_rcv_successful;		/* good receptions */
	BSD43_UINT e_rcv_missed;		/* no recv buff available */
	BSD43_UINT e_crc_err;			/* checksum failed */
	BSD43_UINT e_frame_err;		/* crc error && data len != 0 mod 8 */
	BSD43_UINT e_rcv_buff_err;		/* rcv chaining failed, can't happen */
	BSD43_UINT e_silo_overrun;		/* receive data store failed */
	BSD43_UINT e_rcv_mem_err;
};
#define	BSD43_ENPSTAT	struct bsd43_(enpstat)

struct bsd43_(es_stats) {
	int	ictlrerr;
	int	ibadtrailer;
	int	ibadtrailer2;
	int	inombufs;
	int	iqfull;
	int	onombufs;
};

struct bsd43_(ring)
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;
	BSD43_UINT	r_slot[1];
};
#define	BSD43_RING	struct bsd43_(ring)

struct bsd43_(ring32)
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;			/* to make VAXen happy */
	BSD43_UINT	r_slot[32];
};
#define	BSD43_RING32	struct bsd43_(ring32)

/*
 * 	ENP Ram data layout
 *
 *	If you don't put it here - it isn't there
 *
 */
struct bsd43_(enpdevice) {
	char	enp_ram_rom[4 BSD43_K];
	union {
		char	all_ram[BSD43_RAM_SIZE];
		struct {
			unsigned short t_go;
			unsigned short t_csr;
			BSD43_UINT t_pstart;
		} t;
		struct {
			char	nram[BSD43_RAM_SIZE - (BSD43_HOST_RAMSIZE + BSD43_ENP_RAMSIZE)];
			char	hram[BSD43_HOST_RAMSIZE];
			char	kram[BSD43_ENP_RAMSIZE];
		} u_ram;
		struct {
			char	pad7[ 0x100 ];	/* starts 0x1100 - 0x2000 */
			short	e_enpstate;		
			short	e_enpmode;		
			BSD43_UINT	e_enpbase;		
			BSD43_UINT	e_enprun;

			BSD43_RING32	h_toenp;		
			BSD43_RING32	h_hostfree;		
			BSD43_RING32	e_tohost;		
			BSD43_RING32 	e_enpfree;		

			BSD43_ENPSTAT	e_stat;
			BSD43_ETHLIST	e_netaddr;		
		} iface;
	} enp_u;
	struct bsd43_(iow10)	enp_iow;
};

#define	BSD43_ENPDEVICE	struct bsd43_(enpdevice)

/*
 * enp csr bit defs
 */
#define	BSD43_ENPCSR_ONLINE	0x2		/* entered application firmware */
#define	BSD43_ENPCSR_RDY	0x4		/* reset complete */
#define	BSD43_ENPCSR_OVPROM	0x8		/* don't transfer to prom on reset */
#define	BSD43_ENPCSR_IE	0x40		/* interrupt enable for bus debug */
#define	BSD43_ENPCSR_ERR	0x8000		/* enp detected error */

#define	bsd43_enp_ram		enp_u.all_ram
#define	bsd43_enp_nram	enp_u.u_ram.nram
#define	bsd43_enp_hram	enp_u.u_ram.hram
#define	bsd43_enp_kram	enp_u.u_ram.kram
#define	bsd43_enp_go		enp_u.t.t_go
#define	bsd43_enp_csr		enp_u.t.t_csr
#define	bsd43_enp_prog_start	enp_u.t.t_pstart
#define bsd43_enp_state	enp_u.iface.e_enpstate
#define bsd43_enp_mode	enp_u.iface.e_enpmode
#define bsd43_enp_base	enp_u.iface.e_enpbase
#define bsd43_enp_enprun	enp_u.iface.e_enprun
#define bsd43_enp_toenp	enp_u.iface.h_toenp
#define bsd43_enp_hostfree	enp_u.iface.h_hostfree
#define bsd43_enp_tohost	enp_u.iface.e_tohost
#define bsd43_enp_enpfree	enp_u.iface.e_enpfree
#define bsd43_enp_stat	enp_u.iface.e_stat
#define bsd43_enp_addr	enp_u.iface.e_netaddr

#define BSD43_ENPVAL		0xff	/* poke in enp_iow.hst2enp_interrupt */
#define BSD43_RESETVAL	0x00	/* poke in enp_iow.enp2hst_clear_intr */

#define BSD43_INTR_ENP(addr)		addr->enp_iow.hst2enp_interrupt = BSD43_ENPVAL

/*
 * state bits
 */
#define BSD43_S_ENPRESET	01		/* enp is in reset state */
#define BSD43_S_ENPRUN	02		/* enp is in run state */

/*
 * mode bits
 */
#define BSD43_E_SWAP16		0x1		/* swap two octets within 16 */
#define BSD43_E_SWAP32		0x2		/* swap 16s within 32 */
#define BSD43_E_SWAPRD		0x4		/* swap on read */
#define BSD43_E_SWAPWRT		0x8		/* swap on write */
#define BSD43_E_DMA			0x10		/* enp does data moving */

#define BSD43_E_EXAM_LIST		0x80000000	/* enp should examine addr list */

/*
 * 	Download ioctl definitions
 */

#define bsd43_mkioctl(type,value) (0x20000000|('type'<<8)|value)

#define BSD43_ENPIOGO		bsd43_mkioctl( S,1 )		/* start the enp */
#define BSD43_ENPIORESET	bsd43_mkioctl( S,2 )		/* reset the enp */

/*
 * 	The ENP Data Buffer Structure
 */
struct bsd43_(bcb)
{
	struct BSD43_BCB *b_link;
	short	b_stat;
	short	b_len;
	BSD43_UINT	b_addr;
	short	b_msglen;
	short	b_reserved;
};
#define	BSD43_BCB	struct bsd43_(bcb)

/*
 * BCB b_stat field defines
 */
#define	BSD43_BCB_DONE	0x8000		/* buffer complete */
#define	BSD43_BCB_ERR		0x4000		/* error summary */
#define	BSD43_BCB_FRAME	0x2000		/* framing error */
#define	BSD43_BCB_OFLO	0x1000		/* silo overflow */
#define	BSD43_BCB_CRC		0x0800		/* crc error */
#define	BSD43_BCB_RXBUF	0x0400		/* rx buffer err */
#define	BSD43_BCB_STP		0x0200		/* start of packet */
#define	BSD43_BCB_ENP		0x0100		/* end of packet */
#define	BSD43_BCB_MEMERR	0x0002		/* memory error */
#define	BSD43_BCB_MISSED	0x0001		/* missed packet */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BCB BSD43_BCB
#   define BCB_CRC BSD43_BCB_CRC
#   define BCB_DONE BSD43_BCB_DONE
#   define BCB_ENP BSD43_BCB_ENP
#   define BCB_ERR BSD43_BCB_ERR
#   define BCB_FRAME BSD43_BCB_FRAME
#   define BCB_MEMERR BSD43_BCB_MEMERR
#   define BCB_MISSED BSD43_BCB_MISSED
#   define BCB_OFLO BSD43_BCB_OFLO
#   define BCB_RXBUF BSD43_BCB_RXBUF
#   define BCB_STP BSD43_BCB_STP
#   define ENPCSR_ERR BSD43_ENPCSR_ERR
#   define ENPCSR_IE BSD43_ENPCSR_IE
#   define ENPCSR_ONLINE BSD43_ENPCSR_ONLINE
#   define ENPCSR_OVPROM BSD43_ENPCSR_OVPROM
#   define ENPCSR_RDY BSD43_ENPCSR_RDY
#   define ENPDEVICE BSD43_ENPDEVICE
#   define ENPIOGO BSD43_ENPIOGO
#   define ENPIORESET BSD43_ENPIORESET
#   define ENPSIZE BSD43_ENPSIZE
#   define ENPSTAT BSD43_ENPSTAT
#   define ENPVAL BSD43_ENPVAL
#   define ENP_RAMSIZE BSD43_ENP_RAMSIZE
#   define ETHADDR BSD43_ETHADDR
#   define ETHLIST BSD43_ETHLIST
#   define E_DMA BSD43_E_DMA
#   define E_EXAM_LIST BSD43_E_EXAM_LIST
#   define E_SWAP16 BSD43_E_SWAP16
#   define E_SWAP32 BSD43_E_SWAP32
#   define E_SWAPRD BSD43_E_SWAPRD
#   define E_SWAPWRT BSD43_E_SWAPWRT
#   define FIXED_WINDOW BSD43_FIXED_WINDOW
#   define HI16 BSD43_HI16
#   define HOST_RAMSIZE BSD43_HOST_RAMSIZE
#   define INTR_ENP BSD43_INTR_ENP
#   define IOACCESS_WINDOW BSD43_IOACCESS_WINDOW
#   define K BSD43_K
#   define LO16 BSD43_LO16
#   define MINPKTSIZE BSD43_MINPKTSIZE
#   define RAMROM_SWAP BSD43_RAMROM_SWAP
#   define RAM_SIZE BSD43_RAM_SIZE
#   define RAM_WINDOW BSD43_RAM_WINDOW
#   define RESETVAL BSD43_RESETVAL
#   define RING BSD43_RING
#   define RING32 BSD43_RING32
#   define S_ENPRESET BSD43_S_ENPRESET
#   define S_ENPRUN BSD43_S_ENPRUN
#   define UINT BSD43_UINT
#   define UINT_UINT BSD43_UINT_UINT
#   define UINT_uint BSD43_UINT_uint
#   define enp_addr bsd43_enp_addr
#   define enp_base bsd43_enp_base
#   define enp_csr bsd43_enp_csr
#   define enp_enpfree bsd43_enp_enpfree
#   define enp_enprun bsd43_enp_enprun
#   define enp_go bsd43_enp_go
#   define enp_hostfree bsd43_enp_hostfree
#   define enp_hram bsd43_enp_hram
#   define enp_kram bsd43_enp_kram
#   define enp_mode bsd43_enp_mode
#   define enp_nram bsd43_enp_nram
#   define enp_prog_start bsd43_enp_prog_start
#   define enp_ram bsd43_enp_ram
#   define enp_stat bsd43_enp_stat
#   define enp_state bsd43_enp_state
#   define enp_toenp bsd43_enp_toenp
#   define enp_tohost bsd43_enp_tohost
#   define mkioctl bsd43_mkioctl
#   define uint_UINT bsd43_uint_UINT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


