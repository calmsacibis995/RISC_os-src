#ident "$Header: pon_cfb.c,v 1.2.1.1 90/07/18 14:30:14 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "pon.h"

#define	DISPATCH(x)		(cached ? (int (*)())K1_TO_K0((x)) : (x))
#define	UNCACHE(x)		((int (*)())K0_TO_K1(x))
#define	CODE_SPACE		0x80080000

#define	COLUMNS			2048		/* actual number of columns */
#define	VISIBLE_COLUMNS		1280
#define	ROWS			1024

#define	GRAF_ADDR		0x10000000	/* frame buffer select */
#define	GRAF_LEN		(COLUMNS * ROWS)

#define	CPU_MAPPING		0xe0000000	/* used by the macros below to preserve */

#define	FIRST_ADDRESS		GRAF_ADDR
#define	LAST_ADDRESS		(GRAF_ADDR + GRAF_LEN - 1)

#define	PACK_VERT		0x0000ffc0
#define	PACK_VERT_SHIFT		5
#define	PACK_HORZ_HI		0x001f0000
#define	PACK_HORZ_HI_SHIFT	10
#define	PACK_HORZ_LO		0x0000003f
#define	PACK_HORZ		(PACK_HORZ_HI | PACK_HORZ_LO)
#define	TO_PACKED(addr)		(((u_int)(addr) & (CPU_MAPPING | GRAF_ADDR)) | (((u_int)(addr) >> PACK_VERT_SHIFT) & PACK_VERT) \
				    | (((u_int)(addr) << PACK_HORZ_HI_SHIFT) & PACK_HORZ_HI) | ((u_int)(addr) & PACK_HORZ_LO))
						/* packed mode address */

#define	UNPACK_VERT		0x01ff8000
#define	UNPACK_VERT_SHIFT	4
#define	UNPACK_HOLE		0
#define	UNPACK_HORZ		0x000007ff
#define	TO_UNPACKED(addr)	(((u_int)(addr) & (CPU_MAPPING | GRAF_ADDR)) | (((u_int)(addr) << UNPACK_VERT_SHIFT) & UNPACK_VERT) \
				    | UNPACK_HOLE | ((u_int)(addr) & UNPACK_HORZ))
						/* unpacked mode address */

#define	BANK_SELECT		(1 << 2)	/* determines which VRAM bank is selected */

/*
 * RAMDAC registers.
 */
#define	RAMDAC_BASE		0x14000000

struct ramdac {
	u_int adrlo;
	char pad1[0x80000 - 4];
	u_int adrhi;
	char pad2[0x80000 - 4];
	u_int cntrl;
	char pad3[0x80000 - 4];
	u_int palet;
};

/*
 * RAMDAC internal registers.
 */
#define	RD_COLOR_PALET		0x0000		/* starting address of color palette */
						/*   requires 3 read/write cycles for RGB */
#define	RD_COLOR_PALET_SIZE	256
#define	RD_OVERLAY_PALET	0x0100		/* starting address of overlay palette */
						/*   requires 3 read/write cycles for RGB */
#define	RD_OVERLAY_PALET_SIZE	16

#define	RD_CURSOR_PALET		0x0181		/* starting address of the cursor color palette */
						/*   requires 3 read/write cycles for RGB */
#define	RD_CURSOR_PALET_SIZE	3

#define	RD_ID			0x0200

#define	RD_COMMAND0		0x0201
#define	RD_COMMAND1		0x0202
#define	RD_COMMAND2		0x0203
#define	RD_PIXEL_READ_MASK	0x0204
#define	RD_RESERVED1		0x0205
#define	RD_PIXEL_BLINK_MASK	0x0206
#define	RD_RESERVED2		0x0207
#define	RD_OVERLAY_READ_MASK	0x0208
#define	RD_OVERLAY_BLINK_MASK	0x0209
#define	RD_INTERLEAVE		0x020a
#define	RD_TEST			0x020b
#define	RD_RED_SIGNATURE	0x020c
#define	RD_GREEN_SIGNATURE	0x020d
#define	RD_BLUE_SIGNATURE	0x020e

#define	RD_CURSOR_COMMAND	0x0300
#define	RD_CURSOR_X_LO		0x0301
#define	RD_CURSOR_X_HI		0x0302
#define	RD_CURSOR_Y_LO		0x0303
#define	RD_CURSOR_Y_HI		0x0304
#define	RD_WINDOW_X_LO		0x0305
#define	RD_WINDOW_X_HI		0x0306
#define	RD_WINDOW_Y_LO		0x0307
#define	RD_WINDOW_Y_HI		0x0308
#define	RD_WINDOW_WIDTH_LO	0x0309
#define	RD_WINDOW_WIDTH_HI	0x030a
#define	RD_WINDOW_HEIGHT_LO	0x030b
#define	RD_WINDOW_HEIGHT_HI	0x030c

#define	RD_CURSOR_RAM		0x0400		/* starting address of cursor RAM */
#define	RD_CURSOR_RAM_SIZE	1024

#define	RD_ADDR_HI(x)		(((x) >> 8) & 0xff)
#define	RD_ADDR_LO(x)		((x) & 0xff)

/*
 * Frame buffer registers and bit positions.
 */
#define	KERNEL_REG		0x16080007	/* R/W byte register */
#define	KR_TYPE			(3 << 6)	/* board type */
#define	KR_VBLANK		(1 << 5)	/* vertical blanking interval */
#define	KR_HBLANK		(1 << 4)	/* horizontal blanking interval */
#define	KR_INTEN		(1 << 0)	/* interrupt enable */

#define	XSERVER_REG		0x16000007	/* R/W byte register */
#define	XR_FILL			(1 << 2)	/* fill mode */
#define	XR_PACKED		(1 << 1)	/* 1 = packed mode, 0 = unpacked mode */
#define	XR_UNBLANK		(1 << 0)	/* 1 = unblank display, 0 = blank display */

#define	WRITEMASK_REG		0x16100000	/* R/W word register */

/*
 * Macros.
 */
#define	BLANK_VIDEO		*xserver_reg = *xserver_reg & ~XR_UNBLANK;
#define	UNBLANK_VIDEO		*xserver_reg = *xserver_reg | XR_UNBLANK;

#define	HI_BYTE(x)		(((x) >> 8) & 0xff)
#define	LO_BYTE(x)		((x) & 0xff)

/*
 * Vertical blanking test.
 */
#define	VBLANK_HI_COUNT		760		/* padding of 10% */
#define	VBLANK_LO_COUNT		24372
#define	VBLANK_TIMEOUT		(VBLANK_LO_COUNT + VBLANK_HI_COUNT)

#define	VBLANK_HI_TCOUNT_SLOW	2273		/* based on 505 us @ 20 MHz - 10% */
#define	VBLANK_HI_TCOUNT_FAST	4166		/* based on 505 us @ 30 MHz + 10% */
#define	VBLANK_LO_TCOUNT_SLOW	72720		/* based on 16.16 ms @ 20 MHz - 10 % */
#define	VBLANK_LO_TCOUNT_FAST	133320		/* based on 16.16 ms @ 30 MHz + 10 % */

/*
 * Write masking test.
 */
#define	WM_TEST_SIZE		64
#define	WM_FIRST_ADDRESS	(u_int *)PHYS_TO_K1(GRAF_ADDR)
#define	WM_LAST_ADDRESS		(u_int *)PHYS_TO_K1(GRAF_ADDR + (4 * WM_TEST_SIZE) - 1)

extern char success[], failure[], skipped[], crlf[];
extern int GetCause();
extern int GetDepend();
extern int GetIntR();
extern int pon_puthex();
extern int pon_puts();
extern int WrtRamdacCntrl13();
extern int WrtRamdacCntrl14();
extern int WrtRamdacCntrl256();
extern u_int *end_of_text();
extern u_int (*move_code())();
extern u_int pon_tmp;
extern u_int rambo_clock_hz;

static volatile u_int *ramdac_adrhi = (u_int *)PHYS_TO_K1(RAMDAC_ADRHI);
static volatile u_int *ramdac_adrlo = (u_int *)PHYS_TO_K1(RAMDAC_ADRLO);
static volatile u_int *ramdac_cntrl = (u_int *)PHYS_TO_K1(RAMDAC_CNTRL);
static volatile u_int *ramdac_palet = (u_int *)PHYS_TO_K1(RAMDAC_PALET);

static volatile u_char *kernel_reg = (u_char *)PHYS_TO_K1(KERNEL_REG);
static volatile u_char *xserver_reg = (u_char *)PHYS_TO_K1(XSERVER_REG);
static volatile u_int *write_mask_reg = (u_int *)PHYS_TO_K1(WRITEMASK_REG);

#define	GET_PACKED_BYTE(addr, data)		{ \
							bptr = (u_char *)TO_PACKED(addr); \
							data = *bptr; \
						}

#define	GET_PACKED_WORD(addr, data)		{ \
							wptr = (u_int *)TO_PACKED(addr); \
							data = *wptr; \
						}

#define	GET_UNPACKED_BYTE(addr, data)		{ \
							bptr = (u_char *)TO_UNPACKED(addr); \
							data = *bptr; \
						}

#define	GET_UNPACKED_WORD(addr, data)		{ \
							wptr = (u_int *)TO_UNPACKED(addr); \
							data = *wptr; \
						}

#define	SET_PACKED_BYTE(addr, pattern)		{ \
							bptr = (u_char *)TO_PACKED(addr); \
							*bptr = (u_char)pattern; \
						}

#define	SET_PACKED_WORD(addr, pattern)		{ \
							wptr = (u_int *)TO_PACKED(addr); \
							*wptr = pattern; \
						}

#define	SET_UNPACKED_BYTE(addr, pattern)		{ \
							bptr = (u_char *)TO_UNPACKED(addr); \
							*bptr = (u_char)pattern; \
						}

#define	SET_UNPACKED_WORD(addr, pattern)		{ \
							wptr = (u_int *)TO_UNPACKED(addr); \
							*wptr = pattern; \
						}


static InVBlank()

{
	register volatile u_char *kernel = (u_char *)kernel_reg;
	register u_int i;

	/*
	 * Start with VBlank low.
	 */
	if (*kernel & KR_VBLANK) {
		i = VBLANK_TIMEOUT;
		while ((*kernel & KR_VBLANK) && --i) {
			;
		}
	}

	if (i == 0) {
#ifdef	DEBUG_T
		pon_puts("InVBlank: Timeout waiting for VBLANK to go low\r\n");
#endif	DEBUG_T
		return(BAD);
	}

	/*
	 * Wait for VBlank to go high.
	 */
	i = VBLANK_LO_COUNT;
	while (!(*kernel & KR_VBLANK) && --i) {
		;
	}

	if (i == 0) {
#ifdef	DEBUG_T
		pon_puts("InVBlank: Timeout waiting for VBLANK to go high\r\n");
#endif	DEBUG_T
		return(BAD);
	}

	return(OK);
}


static int WriteRAMDACCntrl(index, data)

register u_short index;
register u_char data;

{
	*ramdac_adrlo = RD_ADDR_LO(index);
	*ramdac_adrhi = RD_ADDR_HI(index);
	*ramdac_cntrl = data;
}


static RamdacAddrRegs()

{
	register volatile u_int *adrhi = ramdac_adrhi;
	register volatile u_int *adrlo = ramdac_adrlo;
	register int i;
	register int j;
	int error = 0;

	for (i = 0, j = 1; i < 4; i++, j <<= 1) {
		*adrhi = j;
		pon_tmp = 0;
		if (*adrhi != j) {
			error = 1;
		}
	}

	for (i = 0, j = 1; i < 8; i++, j <<= 1) {
		*adrlo = j;
		pon_tmp = 0;
		if (*adrlo != j) {
			error = 1;
		}
	}

	if (error) {
#ifdef	DEBUG
		pon_puts("Failed RamdacAddrRegs\r\n");
#endif	DEBUG
		return(FAIL);
	}

	return(PASS);
}


static Palette()

{
	register volatile u_int *cntrl = ramdac_cntrl;
	register volatile u_int *palet = ramdac_palet;
	register int i;
	register u_char data;
	int ret = PASS;

	/*
	 * Check the cursor color palette.
	 */
	*ramdac_adrhi = RD_ADDR_HI(RD_CURSOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_CURSOR_PALET);
	for (i = 0; i < 3 * RD_CURSOR_PALET_SIZE; i++) {
		*cntrl = i;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_CURSOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_CURSOR_PALET);
	for (i = 0; i < 3 * RD_CURSOR_PALET_SIZE; i++) {
		data = *cntrl;
		if (data != (u_char)i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("cursor color 1: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_CURSOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_CURSOR_PALET);
	for (i = 0; i < 3 * RD_CURSOR_PALET_SIZE; i++) {
		*cntrl = ~i;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_CURSOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_CURSOR_PALET);
	for (i = 0; i < 3 * RD_CURSOR_PALET_SIZE; i++) {
		data = *cntrl;
		if (data != (u_char)~i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("cursor color 2: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(~i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_CURSOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_CURSOR_PALET);
	for (i = 0; i < 3 * RD_CURSOR_PALET_SIZE; i++) {
		*cntrl = 0;
	}

	/*
	 * Check the overlay color palette.
	 */
	*ramdac_adrhi = RD_ADDR_HI(RD_OVERLAY_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_OVERLAY_PALET);
	for (i = 0; i < 3 * RD_OVERLAY_PALET_SIZE; i++) {
		*cntrl = i;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_OVERLAY_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_OVERLAY_PALET);
	for (i = 0; i < 3 * RD_OVERLAY_PALET_SIZE; i++) {
		data = *cntrl;
		if (data != (u_char)i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("overlay 1: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_OVERLAY_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_OVERLAY_PALET);
	for (i = 0; i < 3 * RD_OVERLAY_PALET_SIZE; i++) {
		*cntrl = ~i;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_OVERLAY_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_OVERLAY_PALET);
	for (i = 0; i < 3 * RD_OVERLAY_PALET_SIZE; i++) {
		data = *cntrl;
		if (data != (u_char)~i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("overlay 2: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(~i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_OVERLAY_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_OVERLAY_PALET);
	for (i = 0; i < 3 * RD_OVERLAY_PALET_SIZE; i++) {
		*cntrl = 0;
	}

	/*
	 * Check the color palette.
	 */
	*ramdac_adrhi = RD_ADDR_HI(RD_COLOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_COLOR_PALET);
	for (i = 0; i < 3 * RD_COLOR_PALET_SIZE; ) {
		*palet = i++;
		*palet = i++;
		*palet = i++;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_COLOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_COLOR_PALET);
	for (i = 0; i < 3 * RD_COLOR_PALET_SIZE; i++) {
		data = *palet;
		if (data != (u_char)i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("palette 1: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_COLOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_COLOR_PALET);
	for (i = 0; i < 3 * RD_COLOR_PALET_SIZE; ) {
		*palet = ~i++;
		*palet = ~i++;
		*palet = ~i++;
;
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_COLOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_COLOR_PALET);
	for (i = 0; i < 3 * RD_COLOR_PALET_SIZE; i++) {
		data = *palet;
		if (data != (u_char)~i) {
			ret = FAIL;
#ifdef	DEBUG_T
			(*UNCACHE(pon_puts))("palette 2: actual ");
			(*UNCACHE(pon_puthex))(data);
			(*UNCACHE(pon_puts))(" expect ");
			(*UNCACHE(pon_puthex))(~i);
			(*UNCACHE(pon_puts))(crlf);
#endif	DEBUG_T
		}
	}

	*ramdac_adrhi = RD_ADDR_HI(RD_COLOR_PALET);
	*ramdac_adrlo = RD_ADDR_LO(RD_COLOR_PALET);
	for (i = 0; i < 3 * RD_COLOR_PALET_SIZE; i++) {
		*palet = 0;
	}

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed Palette\r\n");
	}
#endif	DEBUG

	return(ret);
}


static CfbRegs()

{
	register volatile u_char *xserver = xserver_reg;
	register volatile u_int *write_mask = write_mask_reg;
	register int i;
	register u_int j;
	int ret = PASS;

	/*
	 * Check the write mask register.
	 */
	for (i = 0, j = 1; i < 32; i++, j <<= 1) {
		*write_mask = j;
		pon_tmp = 0;
		if (*write_mask != j) {
			ret = FAIL;
		}
	}

	*write_mask = 0xffffffff;

	/*
	 * Check the Xserver register.
	 */
	for (i = 0, j = 1; i < 8; i++, j <<= 1) {
		*xserver = j;
		pon_tmp = 0;
		if (*xserver != j) {
			ret = FAIL;
		}
	}

	*xserver = 0;

	/*
	 * NOTE: The Kerneal register is checked in the blanking test.
	 */

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed CfbRegs\r\n");
	}
#endif	DEBUG

	return(ret);
}


static ByteAinaFill()

{
	register volatile u_char *bptr;
	register volatile u_int *wptr;
	register u_int first_addr;
	register u_int last_addr;
	register u_int last_value_read;
	register u_int ptr;
	int ret = PASS;

	BLANK_VIDEO;				/* should be already blanked */
	*write_mask_reg = 0xffffffff;
	if (!((*UNCACHE(GetDepend))() & PON_FAULT_DCACHE)) {
		first_addr = PHYS_TO_K0(FIRST_ADDRESS);
		last_addr = PHYS_TO_K0(LAST_ADDRESS);
	}
	else {
		first_addr = PHYS_TO_K1(FIRST_ADDRESS);
		last_addr = PHYS_TO_K1(LAST_ADDRESS);
	}

#ifdef	DEBUG
	pon_puts("first ");
	pon_puthex(first_addr);
	pon_puts(" last ");
	pon_puthex(last_addr);
	pon_puts(crlf);
#endif	DEBUG

	/*
	 * Case 1:
	 *	Access:	      Byte
	 *	Write:	      Unpacked with fill
	 *	Read:	      Packed
	 *	Bank to fill: 0
	 */
	*xserver_reg = (*xserver_reg & ~XR_PACKED) | XR_FILL;
						/* set fill and unpacked modes */

	/*
	 * Set all locations to A's.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr++) {
		SET_UNPACKED_BYTE(ptr, 0xaa);
	}

	/*
	 * Address-in-address; ascending stores; ascending check.
	 * Write to bank 0 and have fill set bank 1 in unpacked mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr++) {
		if (!((u_int)ptr & BANK_SELECT)) {
			SET_UNPACKED_BYTE(ptr, ptr);
		}
	}

	*xserver_reg = *xserver_reg | XR_FILL | XR_PACKED;
						/* set fill and packed modes */

	/*
	 * Read back data in packed mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr++) {
		GET_PACKED_BYTE(ptr, last_value_read);
		if (last_value_read != ((u_char)ptr & ~BANK_SELECT)) {
			ret = FAIL;
#ifdef	DEBUG
			pon_puts("case 1\r\nactual ");
			pon_puthex(last_value_read);
			pon_puts(" expect ");
			pon_puthex(ptr & ~BANK_SELECT);
			pon_puts(crlf);
#endif	DEBUG
		}
	}

	/*
	 * Case 2:
	 *	Access:       Word
	 *	Write:	      Packed with fill
	 *	Read:	      Unpacked
	 *	Bank to fill: 1
	 */

	/*
	 * Address-in-address; ascending stores; ascending check.
	 * Write to bank 1 and have fill set bank 0 in packed mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr += 4) {
		if ((u_int)ptr & BANK_SELECT) {
			SET_PACKED_WORD(ptr, ptr);
		}
	}

	*xserver_reg = (*xserver_reg & ~XR_PACKED) | XR_FILL;
						/* set fill and unpacked modes */

	/*
	 * Read back data in unpacked mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr += 4) {
		GET_UNPACKED_WORD(ptr, last_value_read);
		if (last_value_read != (ptr | BANK_SELECT)) {
			ret = FAIL;
#ifdef	DEBUG
			pon_puts("case 2\r\nactual ");
			pon_puthex(last_value_read);
			pon_puts(" expect ");
			pon_puthex(ptr | BANK_SELECT);
			pon_puts(crlf);
#endif	DEBUG
		}
	}

	/*
	 * Case 3:
	 *	Access:       Word
	 *	Write:	      Packed without fill
	 *	Read:	      Packed
	 *	Bank to fill: None
	 */
	*xserver_reg = XR_PACKED;		/* set packed mode */

	/*
	 * Address-in-address; ascending stores; ascending check.
	 * Write to bank 1 and have fill set bank 0 in packed mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr += 4) {
		SET_PACKED_WORD(ptr, ~ptr);
	}

	/*
	 * Read back data in unpacked mode.
	 */
	for (ptr = first_addr; ptr <= last_addr; ptr += 4) {
		GET_PACKED_WORD(ptr, last_value_read);
		if (last_value_read != ~ptr) {
			ret = FAIL;
#ifdef	DEBUG
			pon_puts("case 3\r\nactual ");
			pon_puthex(last_value_read);
			pon_puts(" expect ");
			pon_puthex(~ptr);
			pon_puts(crlf);
#endif	DEBUG
		}
	}

done:

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed ByteAinaFill\r\n");
	}
#endif	DEBUG

	return(ret);
}


/*
 * RAMDAC register masks.
 */
struct register_entry {
	u_short address;
	u_char mask;
	u_char reset_value;
};

static struct register_entry registers2[] = {
	RD_COMMAND0,		0xef,	0x80,
	RD_COMMAND1,		0xef,	0,
	RD_COMMAND2,		0xff,	0,
	RD_PIXEL_READ_MASK,	0xff,	0,
	RD_RESERVED1,		0,	0,
	RD_PIXEL_BLINK_MASK,	0xff,	0,
	RD_RESERVED2,		0,	0,
	RD_OVERLAY_READ_MASK,	0x0f,	0,
	RD_OVERLAY_BLINK_MASK,	0x0f,	0,
	RD_INTERLEAVE,		0xff,	0,
	RD_TEST,		0xf7,	0,
	RD_RED_SIGNATURE,	0xff,	0,
	RD_GREEN_SIGNATURE,	0xff,	0,
	RD_BLUE_SIGNATURE,	0xff,	0,
};

#define	TABLE2_SIZE			(sizeof(registers2) / sizeof(struct register_entry))

static struct register_entry registers3[] = {
	RD_CURSOR_COMMAND,	0xff,	0xfc,
	RD_CURSOR_X_LO,		0xff,	0x01,
	RD_CURSOR_X_HI,		0x0f,	0,
	RD_CURSOR_Y_LO,		0xff,	0x04,
	RD_CURSOR_Y_HI,		0x0f,	0,
	RD_WINDOW_X_LO,		0xff,	0,
	RD_WINDOW_X_HI,		0x0f,	0,
	RD_WINDOW_Y_LO,		0xff,	0x80,
	RD_WINDOW_Y_HI,		0x0f,	0,
	RD_WINDOW_WIDTH_LO,	0xff,	0x02,
	RD_WINDOW_WIDTH_HI,	0x0f,	0,
	RD_WINDOW_HEIGHT_LO,	0xff,	0x15,
	RD_WINDOW_HEIGHT_HI,	0x0f,	0x06,
};

#define	TABLE3_SIZE			(sizeof(registers3) / sizeof(struct register_entry))

static u_char patterns[] = {
	0x8a,
	0x34,
	0x41,
};


static RamdacRegs()

{
	register volatile struct ramdac *rd = (struct ramdac *)PHYS_TO_K1(RAMDAC_BASE);
	register volatile u_char *rp;
	register u_char adrhi;
	register u_char adrlo;
	register u_char k;
	register u_char l;
	register int j;
	int ret = PASS;
	int i;
	u_char read[TABLE2_SIZE];

restart:
	BLANK_VIDEO;

	/*
	 * Check each RAMDAC internal register by using the autoincrement of the address
	 * low register.
	 */
	adrhi = RD_ADDR_HI(registers2[0].address);
	adrlo = RD_ADDR_LO(registers2[0].address);
	for (i = 0; i < 3; i++) {
		(*UNCACHE(WrtRamdacCntrl14))(registers2[0].address, patterns[i % 3], patterns[(i + 1) % 3], patterns[(i + 2) % 3]);
						/* write consecutive data */
		rp = read;
		rd->adrhi = adrhi;
		rd->adrlo = adrlo;
		for (j = 0, k = 1; j < TABLE2_SIZE; j++, k++) {
			l = rd->cntrl;
			*rp++ = l;
		}

		for (j = 0; j < TABLE2_SIZE; j++) {
			k = (j + i) % 3;
			l = patterns[k] & registers2[j].mask;
			if ((read[j] & registers2[j].mask) != l) {
				ret = FAIL;
			}
		}
	}

	rd->adrhi = adrhi;
	rd->adrlo = adrlo;
	for (j = 0; j < TABLE2_SIZE; j++) {
		rd->cntrl = registers2[j].reset_value;
	}

	adrhi = RD_ADDR_HI(registers3[0].address);
	adrlo = RD_ADDR_LO(registers3[0].address);
	for (i = 0; i < 3; i++) {
		(*UNCACHE(WrtRamdacCntrl13))(registers3[0].address, patterns[i % 3], patterns[(i + 1) % 3], patterns[(i + 2) % 3]);
						/* write consecutive data */
		rp = read;
		rd->adrhi = adrhi;
		rd->adrlo = adrlo;
		for (j = 0, k = 1; j < TABLE3_SIZE; j++, k++) {
			l = rd->cntrl;
			*rp++ = l;
		}

		for (j = 0; j < TABLE3_SIZE; j++) {
			k = (j + i) % 3;
			l = patterns[k] & registers3[j].mask;
			if ((read[j] & registers3[j].mask) != l) {
				ret = FAIL;
			}
		}
	}

	rd->adrhi = adrhi;
	rd->adrlo = adrlo;
	for (j = 0; j < TABLE3_SIZE; j++) {
		rd->cntrl = registers3[j].reset_value;
	}

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed RamdacRegs\r\n");
	}
#endif	DEBUG

	return(ret);
}


static BlankInt()

{
	register volatile struct cnt_timer *clock = (struct cnt_timer *)PHYS_TO_K1(TIMER_BASE);
	register volatile u_char *kernel = (u_char *)kernel_reg;
						/* speed up kernel register accesses */
	register u_int high1;
	register u_int high2;
	register u_int low1;
	register u_int low2;
	register u_int i;
	register u_int cause;
	int ret = PASS;

	if (GetDepend() & PON_FAULT_TIMER) {
		return(FAIL);
	}

restart:
	*kernel = KR_INTEN;			/* enable color frame buffer interrupts */

	clock->tbreak = 0x80000000;
	clock->tcount = 0;

	/*
	 * Check the vertical blanking interval.
	 */

	/*
	 * Start test with VBlank low.
	 */
	if (*kernel & KR_VBLANK) {
		i = VBLANK_TIMEOUT;
		while ((*kernel & KR_VBLANK) && --i) {
			;
		}
	}

	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 1 waiting for VBLANK to go low\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for VBlank to go high.
	 */
	i = VBLANK_LO_COUNT;
	while (!(*kernel & KR_VBLANK) && --i) {
		;
	}

	clock->tcount = 0;			/* start the timer */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 2 waiting for VBLANK to go high\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for VBlank to go low.
	 */
	i = VBLANK_HI_COUNT;
	while ((*kernel & KR_VBLANK) && --i) {
		;
	}

	high1 = clock->tcount;			/* mark VBLANK high time */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 3 waiting for VBLANK to go low\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for VBlank to go high again.
	 */
	i = VBLANK_LO_COUNT;
	while (!(*kernel & KR_VBLANK) && --i) {
		;
	}

	low1 = clock->tcount;			/* mark VBLANK low time */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 4 waiting for VBLANK to go high\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for VBlank to go low again.
	 */
	i = VBLANK_HI_COUNT;
	while ((*kernel & KR_VBLANK) && --i) {
		;
	}

	high2 = clock->tcount;			/* mark VBLANK high time again */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 5 waiting for VBLANK to go low\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for VBlank to go high again.
	 */
	i = VBLANK_LO_COUNT;
	while (!(*kernel & KR_VBLANK) && --i) {
		;
	}

	low2 = clock->tcount;			/* mark VBLANK low time again */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 6 waiting for VBLANK to go high\r\n");
#endif	DEBUG
		goto done;
	}

#ifdef	DEBUG_C
	printf("VBlank TCOUNTs: hi %u, lo %u, hi %u, lo %u\n", high1, low1 - high1, high2 - low1, low2 - high2);
	printf("VBlank: hi %d us, lo %d us, hi %d us, lo %d us\n",
	    high1 * 4 / 25, (low1 - high1) * 4 / 25, (high2 - low1) * 4 / 25, (low2 - high2) * 4 / 25);
#endif	DEBUG_C

	if ((high1 < VBLANK_HI_TCOUNT_SLOW) || (high1 > VBLANK_HI_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Vertical blank 1 high outside 10 percent range, was ");
		pon_puthex(high1);
		pon_puts(crlf);
#endif	DEBUG
	}

	i = low1 - high1;
	if ((i < VBLANK_LO_TCOUNT_SLOW) || (i > VBLANK_LO_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Vertical blank 1 low outside 10 percent range, was ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
	}

	i = high2 - low1;
	if ((i < VBLANK_HI_TCOUNT_SLOW) || (i > VBLANK_HI_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Vertical blank 2 high outside 10 percent range, was ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
	}

	i = low2 - high2;
	if ((i < VBLANK_LO_TCOUNT_SLOW) || (i > VBLANK_LO_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Vertical blank 2 low outside 10 percent range, was ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
	}

	/*
	 * Check the interrupt interval.  Should match the vertical blanking interval.
	 */

	/*
	 * Start test with IRQ5 low.
	 */
	if (GetCause() & CAUSE_IP8) {
		i = VBLANK_TIMEOUT;
		while ((GetCause() & CAUSE_IP8) && --i) {
			;
		}
	}

	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 1 waiting for IRQ5 to go low\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for IRQ5 to go high.
	 */
	i = VBLANK_LO_COUNT;
	while (!(GetCause() & CAUSE_IP8) && --i) {
		;
	}

	clock->tcount = 0;			/* start the timer */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 2 waiting for IRQ5 to go high\r\n");
#endif	DEBUG
		goto done;
	}

	high2 = GetIntR();
	low2 = *kernel;

	/*
	 * Wait for IRQ5 to go low.
	 */
	i = VBLANK_HI_COUNT;
	while ((GetCause() & CAUSE_IP8) && --i) {
		;
	}

	high1 = clock->tcount;			/* mark IRQ5 high time */
	if (i == 0) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Timeout 3 waiting for IRQ5 to go low\r\n");
#endif	DEBUG
		goto done;
	}

	/*
	 * Wait for IRQ5 to go high again.
	 */
	i = VBLANK_LO_COUNT;
	while (!(GetCause() & CAUSE_IP8) && --i) {
		;
	}

	low1 = clock->tcount;			/* mark IRQ5 low time */

#ifdef	DEBUG_C
	printf("Interrupt TCOUNTs: hi %u, lo %u\n", high1, low1 - high1);
	printf("Interrupt: hi %d us, lo %d us\n", high1 * 4 / 25, (low1 - high1) * 4 / 25);
	printf("Interrupt: IntR %02x, KernelR %02x\n", high2, low2);
#endif	DEBUG_C

	if ((high1 < VBLANK_HI_TCOUNT_SLOW) || (high1 > VBLANK_HI_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Interrupt high outside 10 percent range, was ");
		pon_puthex(high1);
		pon_puts(crlf);
#endif	DEBUG
	}

	i = low1 - high1;
	if ((i < VBLANK_LO_TCOUNT_SLOW) || (i > VBLANK_LO_TCOUNT_FAST)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Interrupt low outside 10 percent range, was ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
	}

	/*
	 * Verify that slot interrupt is valid.
	 */
	if (high2 & IR_SLOT_INT_B) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Slot interrupt for color frame buffer is not valid, IntR ");
		pon_puthex(high2);
		pon_puts(crlf);
#endif	DEBUG
	}

	/*
	 * Verify that vertical blanking is occurring.
	 */
	if (!(low2 & KR_VBLANK)) {
		ret = FAIL;
#ifdef	DEBUG
		pon_puts("Vertical blanking bit is not valid during interrupt, KernelR ");
		pon_puthex(low2);
		pon_puts(crlf);
#endif	DEBUG
	}

done:
	*kernel = 0;

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed BlankInt\r\n");
	}
#endif	DEBUG

	return(ret);
}


CursorRAM()

{
	register volatile struct ramdac *rd = (struct ramdac *)PHYS_TO_K1(RAMDAC_BASE);
	register volatile u_char *rp;
	register u_char adrhi;
	register u_char adrlo;
	register u_char k;
	register u_char l;
	register int j;
	int ret = PASS;
	int i;
	int m;
	u_char read[RD_CURSOR_RAM_SIZE];

#ifdef	DEBUG
	pon_puts("in cursor ram\r\n");
#endif	DEBUG

restart:
	BLANK_VIDEO;

	/*
	 * Check each cursor RAM location by using the autoincrement of the address
	 * low register.
	 */
	adrhi = RD_ADDR_HI(RD_CURSOR_RAM);
	adrlo = RD_ADDR_LO(RD_CURSOR_RAM);
	for (i = 0; i < 3; i++) {
		WriteRAMDACCntrl(RD_CURSOR_Y_LO, LO_BYTE(1500));
		WriteRAMDACCntrl(RD_CURSOR_Y_HI, HI_BYTE(1500));
		InVBlank();
		WrtRamdacCntrl256(RD_CURSOR_RAM, patterns[i % 3], patterns[(i + 1) % 3], patterns[(i + 2) % 3]);
		WrtRamdacCntrl256(RD_CURSOR_RAM + 256, patterns[(i + 1) % 3], patterns[(i + 2) % 3], patterns[i % 3]);
		WrtRamdacCntrl256(RD_CURSOR_RAM + 512, patterns[(i + 2) % 3], patterns[i % 3], patterns[(i + 1) % 3]);
		WrtRamdacCntrl256(RD_CURSOR_RAM + 768, patterns[i % 3], patterns[(i + 1) % 3], patterns[(i + 2) % 3]);
						/* write consecutive data */
		InVBlank();
		WriteRAMDACCntrl(RD_CURSOR_Y_LO, LO_BYTE(VISIBLE_COLUMNS / 2));
		WriteRAMDACCntrl(RD_CURSOR_Y_HI, HI_BYTE(VISIBLE_COLUMNS / 2));
		rp = read;
		rd->adrhi = adrhi;
		rd->adrlo = adrlo;
		for (j = 0, k = 1; j < RD_CURSOR_RAM_SIZE; j++, k++) {
			l = rd->cntrl;
			*rp++ = l;
		}

		for (j = 0, m = RD_CURSOR_RAM; j < RD_CURSOR_RAM_SIZE; j++, m++) {
			k = (j + i) % 3;
			l = patterns[k];
			if (read[j] != l) {
				ret = FAIL;
#ifdef	DEBUG_T
				pon_puts("RAMDAC register @ ");
				pon_puthex(m);
				pon_puts(" Expect ");
				pon_puthex(l);
				pon_puts(" Actual ");
				pon_puthex(read[j]);
				pon_puts(crlf);
#endif	DEBUG_T
			}
		}
	}

done:
	WriteRAMDACCntrl(RD_CURSOR_Y_LO, LO_BYTE(1500));
	WriteRAMDACCntrl(RD_CURSOR_Y_HI, HI_BYTE(1500));
	InVBlank();
	WrtRamdacCntrl256(RD_CURSOR_RAM, 0, 0, 0);
	WrtRamdacCntrl256(RD_CURSOR_RAM + 256, 0, 0, 0);
	WrtRamdacCntrl256(RD_CURSOR_RAM + 512, 0, 0, 0);
	WrtRamdacCntrl256(RD_CURSOR_RAM + 768, 0, 0, 0);

#ifdef	DEBUG_T
	if (ret == FAIL) {
		pon_puts("Failed CursorRAM\r\n");
	}
#endif	DEBUG_T

	return(ret);
}


static WriteMasking()

{
	register volatile u_int *write_mask = write_mask_reg;
	register volatile u_int *ptr;
	register u_int pattern;
	register u_int inverse_pattern;
	register u_int read_value;
	register int i;
	int ret = PASS;

	/*
	 * Verify that the write mask register does mask bits written to memory.
	 * This is the chicken and egg problem where if the write mask register is
	 * broken, how can memory be tested and vice versa.
	 */
	*xserver_reg = XR_PACKED;

	/*
	 * Case 1:
	 */
	*write_mask = 0xffffffff;
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		*ptr = 0;
	}

	ptr = WM_FIRST_ADDRESS;
	for (i = 0, pattern = 1; i < 32; i++, pattern <<= 1) {
		*write_mask = pattern;
		*ptr++ = 0xffffffff;
		*ptr++ = 0xffffffff;
	}

#ifdef	DEBUG_C
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		printf("(%08x)", *ptr);
	}
#endif	DEBUG_C

	for (ptr = WM_FIRST_ADDRESS, pattern = 1; ptr <= WM_LAST_ADDRESS; pattern <<= 1) {
		if ((read_value = *ptr++) != pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 1's mask with all 1's write to bank 0: Expect %08x, Actual %08x, Xor %08x\n",
			    pattern, read_value, pattern ^ read_value);
#endif	DEBUG_C
		}

		if ((read_value = *ptr++) != pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 1's mask with all 1's write to bank 1: Expect %08x, Actual %08x, Xor %08x\n",
			    pattern, read_value, pattern ^ read_value);
#endif	DEBUG_C
		}
	}

	/*
	 * Case 2:
	 */
	*write_mask = 0xffffffff;
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		*ptr = 0xffffffff;
	}

	ptr = WM_FIRST_ADDRESS;
	for (i = 0, pattern = 1; i < 32; i++, pattern <<= 1) {
		*write_mask = pattern;
		*ptr++ = 0;
		*ptr++ = 0;
	}

#ifdef	DEBUG_C
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		printf("(%08x)", *ptr);
	}
#endif	DEBUG_C

	for (ptr = WM_FIRST_ADDRESS, pattern = 1; ptr <= WM_LAST_ADDRESS; pattern <<= 1) {
		inverse_pattern = ~pattern;
		if ((read_value = *ptr++) != inverse_pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 1's mask with all 0's write to bank 0: Expect %08x, Actual %08x, Xor %08x\n",
			    inverse_pattern, read_value, inverse_pattern ^ read_value);
#endif	DEBUG_C
		}

		if ((read_value = *ptr++) != inverse_pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 1's mask with all 0's write to bank 1: Expect %08x, Actual %08x, Xor %08x\n",
			    inverse_pattern, read_value, inverse_pattern ^ read_value);
#endif	DEBUG_C
		}
	}

	/*
	 * Case 3:
	 */
	*write_mask = 0xffffffff;
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		*ptr = 0;
	}

	ptr = WM_FIRST_ADDRESS;
	for (i = 0, pattern = 1; i < 32; i++, pattern <<= 1) {
		inverse_pattern = ~pattern;
		*write_mask = inverse_pattern;
		*ptr++ = 0xffffffff;
		*ptr++ = 0xffffffff;
	}

#ifdef	DEBUG_C
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		printf("(%08x)", *ptr);
	}
#endif	DEBUG_C

	for (ptr = WM_FIRST_ADDRESS, pattern = 1; ptr <= WM_LAST_ADDRESS; pattern <<= 1) {
		inverse_pattern = ~pattern;
		if ((read_value = *ptr++) != inverse_pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 0's mask with all 1's write to bank 0: Expect %08x, Actual %08x, Xor %08x\n",
			    inverse_pattern, read_value, inverse_pattern ^ read_value);
#endif	DEBUG_C
		}

		if ((read_value = *ptr++) != inverse_pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 0's mask with all 1's write to bank 1: Expect %08x, Actual %08x, Xor %08x\n",
			    inverse_pattern, read_value, inverse_pattern ^ read_value);
#endif	DEBUG_C
		}
	}

	/*
	 * Case 4:
	 */
	*write_mask = 0xffffffff;
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		*ptr = 0xffffffff;
	}

	ptr = WM_FIRST_ADDRESS;
	for (i = 0, pattern = 1; i < 32; i++, pattern <<= 1) {
		inverse_pattern = ~pattern;
		*write_mask = inverse_pattern;
		*ptr++ = 0;
		*ptr++ = 0;
	}

#ifdef	DEBUG_C
	for (ptr = WM_FIRST_ADDRESS; ptr <= WM_LAST_ADDRESS; ptr++) {
		printf("(%08x)", *ptr);
	}
#endif	DEBUG_C

	for (ptr = WM_FIRST_ADDRESS, pattern = 1; ptr <= WM_LAST_ADDRESS; pattern <<= 1) {
		if ((read_value = *ptr++) != pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 0's mask with all 0's write to bank 0: Expect %08x, Actual %08x, Xor %08x\n",
			    pattern, read_value, pattern ^ read_value);
#endif	DEBUG_C
		}

		if ((read_value = *ptr++) != pattern) {
			ret = FAIL;
#ifdef	DEBUG_C
			printf("Walking 0's mask with all 0's write to bank 1: Expect %08x, Actual %08x, Xor %08x\n",
			    pattern, read_value, pattern ^ read_value);
#endif	DEBUG_C
		}
	}

	*write_mask = 0xffffffff;

#ifdef	DEBUG
	if (ret == FAIL) {
		pon_puts("Failed write masking test\r\n");
	}
#endif	DEBUG

	return(ret);
}

#ifdef USED
#define	V_BLANK_BIT	0x20
#define	H_BLANK_BIT	0x10
#define	COLOR_RSV	0xCE

#define	CLOCK_DUR	1562500
#define	CLOCK_SLACK	1000

#define	COLOR_VBLNK_MIN	15
#define	COLOR_VBLNK_MAX	50

#define KREG		0x14000000+0x02080004
#define RAMBO_COUNT	0x1c000000+0xc00

/*
 *  This routine relies on the Kernel color register to follow the
 *  Pizazz spec of 12/89
 *
 *  The algorithm is as follows:
 *	Read the kreg
 *	loop for a certain time of rambo info
 *		if the reserved bit change assume that we have been reading
 *			random information and not a color board
 *		count number of transision of the h_blank signal
 *	if the number of transitions of the hblank is reasonable then
 *		we have a color board
 */
CfbPresent() {
	register volatile unsigned long *time;
	register volatile unsigned long *kreg;
	register	  unsigned long stime, etime, ntime;
	register	  unsigned long	skreg, s1kreg;
	register          unsigned long	v_count, color_ret;
	register	  unsigned long wrap_iminent;

	time = (volatile unsigned long *)PHYS_TO_K1(RAMBO_COUNT);
	kreg = (volatile unsigned long *)PHYS_TO_K1(KREG);
	wrap_iminent = 0;
	stime = *time;
	if ((stime + CLOCK_DUR + CLOCK_SLACK) < stime) {
		wrap_iminent++; 
		etime = stime + CLOCK_DUR + CLOCK_SLACK;
	} else {
		etime = stime + CLOCK_DUR;
	}
	skreg = *kreg;
	v_count = 0;
	color_ret = 1;
	while (color_ret) {
		ntime = *time;
		if ( ((wrap_iminent && (ntime < stime)) | !wrap_iminent) &&
				(ntime > etime) ) {
			break;
		}
		if ((skreg ^ (s1kreg = *kreg)) & COLOR_RSV) {
			color_ret = 0;
			break;
		}
		if ((skreg ^ s1kreg) & V_BLANK_BIT) {
			v_count++;
			skreg = s1kreg;
		}
	}
	if (color_ret) {
		if ((v_count > COLOR_VBLNK_MIN) && (v_count < COLOR_VBLNK_MAX)){
                        *write_mask_reg = 0xffffffff;
			return(1);
		}
	}
	return(0);
}
#endif USED

static struct list {
	int (*proc)();				/* actual routine */
} runlist[] = {
	RamdacAddrRegs,
	RamdacRegs,
	Palette,
	CfbRegs,
	ByteAinaFill,
	WriteMasking,
};

#define	RUN_SIZE		(sizeof(runlist) / sizeof(int))


Pon_Cfb()

{
	register int cached = FALSE;
	register u_int *end_addr;
	register u_int (*func)();
	register int i;
	int error = 0;

	pon_puts("Color Frame Buffer Tests...");

	/*
	 * If CFB is not present, return a good status since CFB is an option.
	 */
	if (!CfbPresent()) {
		pon_puts(skipped);
		return(PASS);
	}

	*xserver_reg = 0;			/* blank the screen */

	if (!(GetDepend() & PON_FAULT_ICACHE)) {
		cached = TRUE;
	}

	for (i = 0; i < RUN_SIZE; i++) {
#ifdef	DEBUG_T
		pon_puthex(i);
		pon_puts(" ");
#endif	DEBUG_T
		if (cached) {
			end_addr = end_of_text(runlist[i]);
			invalidate_cache();
			func = move_code(CODE_SPACE, runlist[i], end_addr);
			error += (*func)();
		}
		else {
			error += (*(int (*)())runlist[i])();
		}

#ifdef	DEBUG_T
		pon_puthex(error);
		pon_puts(crlf);
#endif	DEBUG_T
	}

	invalidate_cache();
	SetSR(GetSR() | SR_SWC);
	error += (*DISPATCH(CursorRAM))();
	SetSR(GetSR() & ~SR_SWC);
#ifdef	DEBUG_T
	pon_puthex(error);
	pon_puts(crlf);
#endif	DEBUG_T

	error += BlankInt();
#ifdef	DEBUG_T
	pon_puthex(error);
	pon_puts(crlf);
#endif	DEBUG_T

	if (error) {
		pon_puts(failure);
		FastFlash(0);
		SetDepend(PON_FAULT_COLORVIDEO);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
}
