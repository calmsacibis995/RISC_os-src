#ident "$Header: ladefs.h,v 1.5 90/01/23 14:15:09 huang Exp $"
/* $Copyright$ */

/*
 * AMD 7990 "Lance" Ethernet Controller Defines
 */

#define	CONVERT(hp,off,type)	((type)(((char *)((hp)+1)+(off))))

/* General Defines */
#define	QUAD		0xFFFFFFF8	/* Quad word boundary mask	*/
#define	WORDSHIFT	16	/* No. of Bits to shift a word		*/
#define	MAXBUFSIZE	1518	/* Maximum Buffer Size			*/
#define	MAXDRE		7	/* Maximum Descriptor Ring Entries -
				   power of two				*/
#define	STATADDRSIZE	6	/* Station Address Size			*/
#define	LAFSIZE		8	/* Logical Address Filter Size		*/
#define	DRESIZE		8	/* Descriptor Ring Entry Size		*/
#define	IBSIZE		0x18	/* Initialisation Block Size		*/
#define	BSTART		0x0E	/* Station Address Byte Offset Start	*/
#define	BEND		0x3D	/* Station Address Byte Offset End	*/

/* Mode Bit Masks */
#define	M_DRX	0x0001		/* Disable Receiver			*/
#define	M_DTX	0x0002		/* Disable Transmitter			*/
#define	M_LOOP	0x0004		/* Loopback				*/
#define M_DTCR	0x0008		/* Disable Transmit CRC			*/
#define M_COLL	0x0010		/* Force Collision			*/
#define M_DRTY	0x0020		/* Disable Retry			*/
#define M_INTL	0x0040		/* Internal Loopback			*/
#define M_PROM	0x8000		/* Promiscuous				*/

/* Descriptor Ring Entry Status Bit Masks */
#define	S_ENP	0x01		/* End of Packet			*/
#define S_STP	0x02		/* Start of Packet			*/
#define S_BUFF	0x04		/* Buffer Error				*/
#define S_DEF	0x04		/* Deferred (busy during transmit)	*/
#define S_CRC	0x08		/* CRC Error				*/
#define S_ONE	0x08		/* One Retry				*/
#define S_OFLO	0x10		/* Overflow Error			*/
#define	S_MORE	0x10		/* More than One Retry			*/
#define	S_FRAM	0x20		/* Frame Error				*/
#define	S_RES	0x20		/* Reserved				*/
#define	S_ERR	0x40		/* Error Summary			*/
#define	S_OWN	0x80		/* Owner				*/

/* Additional Transmit Status Bit Masks */
#define	S_RTRY	0x04		/* Retry Error				*/
#define	S_LCAR	0x08		/* Loss of Carrier			*/
#define	S_LCOL	0x10		/* Late Collision			*/
#define	S_UFLO	0x40		/* Underflow Error			*/
#define	S_TBUFF	0x80		/* Buffer Error				*/

/* Serial Number Port Bit Mask (filtabyte boards only) */
#define	SNP_RENA	0x0200	/* Receiver Enable			*/

/* Control & Status Registers */
#define	CSR0	0x00
#define	CSR1	0x01
#define	CSR2	0x02
#define	CSR3	0x03

/* CSR3 Bit Masks */
#define BCON	0x01		/* Byte Control				*/
#define ACON	0x02		/* ALE Control				*/
#define	BSWP	0x04		/* Byte Swap				*/

/* CSR Bit Masks */
#define	CSR_INIT	0x0001	/* Initialise				*/
#define	CSR_STRT	0x0002	/* Start				*/
#define	CSR_STOP	0x0004	/* Stop					*/
#define	CSR_TDMD	0x0008	/* Transmit Demand			*/
#define	CSR_TXON	0x0010	/* Transmitter On			*/
#define	CSR_RXON	0x0020	/* Receiver On				*/
#define	CSR_INEA	0x0040	/* Interrupt Enable			*/
#define	CSR_INTR	0x0080	/* Interrupt Flag			*/
#define	CSR_IDON	0x0100	/* Initialisation Done			*/
#define	CSR_TINT	0x0200	/* Transmitter Interrupt		*/
#define	CSR_RINT	0x0400	/* Receiver Interrupt			*/
#define	CSR_MERR	0x0800	/* Memory Error				*/
#define	CSR_MISS	0x1000	/* Missed Packet			*/
#define	CSR_CERR	0x2000	/* Collision Error			*/
#define	CSR_BABL	0x4000	/* Babble				*/
#define	CSR_ERR		0x8000	/* Error Summary			*/
#define	CLR_CSR0	(CSR_ERR+CSR_BABL+CSR_CERR+CSR_MISS+CSR_MERR)


/* Configuration Information */
#define	LAPRIORITY	26		/* Sleep/Wakeup Priority	*/
#define	LAFILTER	"\000\000\000\000\000\000\000\000"
					/* Logical Address Filter	*/
#define	RXBUFSIZE	MAXBUFSIZE	/* Receive Buffer Size		*/
#define	TXBUFSIZE	MAXBUFSIZE	/* Transmit Buffer Size		*/
/* XXX these should grow for performance later */
#define	MAX_RXDRE	4		/* Number of Receive Descriptor 
					   Ring Entries			*/
#define	MAX_TXDRE	4		/* Number of Transmit Descriptor 
					   Ring Entries			*/
#define TX		0		/* used in laget_mbuf() */
#define RX		1

#define MINPKTSIZE	60		/* Minimum lance packet size */
