/* $Header: r3250reg.h,v 1.3 90/10/15 11:49:40 lian Exp $ */
/* $Copyright$ */

/*
 * r3250reg.h -- definitions for R3250/R3251 (M2000 system) memory boards
 */

/* R3250 ID PROM format */

/* The ID prom is a series of 32 bytes (mostly unused).
 * The prom is referenced with short (16-bit) reads on short
 * boundries, but only the lower byte of the returned value
 * is meaningful.  Adjacent bytes are accessed at adjacent short addresses.
 * That is, the first byte is at offset 0, the second at offset 2, and so on.
 */

struct mem_idprom {
	ushort	board_type;	/* type of memory board			*/
	ushort	board_size;	/* upper nibble is size, lower nibble unused */
	ushort	board_sn[5];	/* board serial number in ascii		*/
};

#define	TYPE_R3250	1	/* type val for R3250/R3251 memory boards */
#define	TYPE_R3260	2	/* type val for R3264/R3268 memory boards */

#define	SIZE_MASK	0xf0	/* significant bits of board_size byte	*/
#define	SIZE_16MEG	0x10	/* 16M (half populated 32M) 1 in upper nibble */
#define	SIZE_32MEG	0x20	/* 32M board.  2 in upper nibble	*/
#define	SIZE_64MEG	0x30	/* 64M board.  3 in upper nibble	*/
#define	SIZE_128MEG	0x40	/* 128M board. 4 in upper nibble	*/

/* Each memory board has a 256-byte control/status I/O space in
 * the VME A16 I/0 space.
 * This contains the control and status registers for the board
 * as well as the ID PROM for the board.
 * The space must be accessed with short I/O accesses.
 * The base address of the control/status
 * registers for a particular board is determined
 * by the physical slot the board occupies in the private memory bus.
 * The slot numbers range from 0 to 4 in the initial M2000
 * (and one slot must hold the CPU giving a max of 4 memory boards).
 * The boards have 256 bytes of control/status space (8 bits) and the
 * next higher 5 address bits (bits 8-12) are the slot number
 * (i.e. up to 32 slots could exist with this mapping):
 *	000 sssss bbbbbbbb	("sssss" is slot #, "b"s are board offset)
 *
 */

struct r3250_regs {		/* per-board control structures */
	ushort	mem_intr_vec;	/* int. vector (low byte) assigned to board */
	ushort	dummy0;		/* pad. former register unused in M2000	*/
	ushort	mem_cntrl;	/* memory board control register	*/
	ushort	mem_addr;	/* base address assignment register	*/
	ushort	mem_status;	/* memory board status values		*/
	ushort	dummy1[4];	/* padding				*/
	ushort	mem_leds;	/* low-byte is memory leds		*/
	char	dummy2[0x80-0x14]; /* pad out to ID PROM offset of 0x80	*/
	struct mem_idprom mem_idprom;
};

#define	SLOT_SHIFT	8	/* left shift of slot # to get base addr */

/* definitions for control register */

#define	EN_VME		0x0002	/* enable VME accesses			*/
#define	EN_SYN_ERR	0x0008	/* enable syndrome error latches	*/
#define	EN_INT_SERR	0x0020	/* enable interrupts for single bit errors */
#define	EN_INT_DERR	0x0040	/* enable interrupts for double bit errors */
#define	INH_ECC		0x0080	/* if set, inhibit ECC correction 	*/
#define	INH_WRT_DATA	0x0100	/* if set, inhibits writing data bits	*/
#define	INH_WRT_CHK	0x0200	/* if set, inhibits writing check bits	*/
#define	REFRESH_MASK	0x0400	/* mask of bit for refresh rate		*/
#define	SPL_MASK	0x6000	/* mask for the bits setting interrupt level */
#define	EN_A24		0x8000	/* enable A24 accesses (24-bit VME space) */
/* values that fit in "masked" fields */
#define	REFRESH_20	0x0000	/* bit value to set Refresh for 20 MHz	*/
#define	REFRESH_25	0x0400	/* bit value to set Refresh for 25 MHz	*/
#define	SPL_7		0x6000	/* bits to set interrupt level 7	*/
#define	SPL_3		0x4000	/* bits to set interrupt level 3	*/
#define	SPL_1		0x2000	/* bits to set interrupt level 1	*/

/* defines for status bits */

#define	BUS_ERR_ID	0x0001	/* err access bus.  1==private, 0==vme	*/
#define	S_ERR		0x0002	/* single-bit error seen		*/
#define	D_ERR		0x0004	/* double-bit error seen		*/
#define	BANK_MASK	0x0038	/* mask to extract bank decode bits	*/
#define	A24_MASK	0x0020	/* mask to extract A24 bank decode bit 	*/
#define	A23_MASK	0x0010	/* mask to extract A23 bank decode bit	*/
#define	A2_MASK		0x0008	/* mask to extract A2 bank decode bit	*/
#define	BANK_SHIFT	3	/* shift needed to print bank decode	*/
#define	SYNDROME_MASK	0x7f00	/* syndrome bits			*/
#define SYNDROME_SHIFT	8	/* shift needed to print syndrome bits	*/

#define	ECC_ERR_LED	0xa0	/* 1 bits lit on mem leds when ecc err	*/
