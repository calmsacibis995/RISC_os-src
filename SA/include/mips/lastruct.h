#ident "$Header: lastruct.h,v 1.6 90/03/27 18:08:18 zach Exp $"
/* $Copyright$ */

/*
 * AMD 7990 "Lance" Ethernet Controller Structures
 */

/*
 * Ethernet software status per interface
 *
 * Each interface is referenced by a network interface structure ls_if, which 
 * the routing code uses to locate the interface.  This structure contains 
 * the output queue for the interface, its address, ...
 */
struct	la_softc
{
	struct arpcom	ls_ac;		/* Common Ethernet structures */
#define	ls_if		ls_ac.ac_if	/* Network-Visible Interface */
#define	ls_enaddr	ls_ac.ac_enaddr	/* Hardware Ethernet address */
	unsigned char	*ls_laf;	/* Address of Logical Address Filter */
	int		ls_rxin;	/* Index of next rcv DRE to fill */
	int		ls_rxout;	/* Index of next rcv DRE to read */
	int		ls_txin;	/* Index of next vacant xmit DRE */
	int		ls_txout;	/* Index of next xmit DRE to complete */
	struct ladevice *ls_addr;	/* our device */
}; 


/* Lance Ethernet controller registers */
struct ladevice
{
	unsigned short	pad;		/* rdp is at base + 2 */
	volatile unsigned short	csr_rdp;	/* Register Data Port */
	unsigned short	pad1;		/* rap is at base + 6 */
	volatile unsigned short	csr_rap;	/* Register Data Port */
};


/* Lance Initialisation Block */
struct la_ib
{
	unsigned short	ib_mode;	/* Mode Register */
	unsigned char	ib_padr[6];	/* Physical (Ethernet Station) Addr */
	unsigned char	ib_ladrf[8];	/* Logical Address Filter */
	unsigned short	ib_rdral;	/* Receive Descriptor Ring Address -
					   Least Significant Part (0 - 15) */
	unsigned char	ib_rlen;	/* Receive Ring Length */
	unsigned char	ib_rdram;	/* Receive Descriptor Ring Address -
					   Most Significant Part (16 - 23) */
	unsigned short	ib_tdral;	/* Transmit Descriptor Ring Address -
					   Least Significant Part (0 - 15) */
	unsigned char	ib_tlen;	/* Transmit Ring Length */
	unsigned char	ib_tdram;	/* Transmit Descriptor Ring Address -
					   Most Significant Part (16 - 23) */
};


/* Lance Descriptor Ring Entry */
struct la_dre
{
	unsigned short	dre_ladr;	/* Low Order Addr of Buffer (0 - 15) */
	unsigned char	dre_stat;	/* Status of Entry */
	unsigned char	dre_hadr;	/* High Order Addr of Buff (16 - 23) */
	short	dre_bcnt;	/* Buffer Byte Count */
	unsigned short	dre_mcnt;	/* Message Byte Count */
};


