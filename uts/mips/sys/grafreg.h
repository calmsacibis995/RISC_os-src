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
/* $Header: grafreg.h,v 1.6.1.5 90/05/10 06:21:03 wje Exp $ */
/*
 * Header file for graf.c and kernel putc defines
 * $Header: grafreg.h,v 1.6.1.5 90/05/10 06:21:03 wje Exp $
 */

#ifndef _SYS_GRAFREG_
#define _SYS_GRAFREG_

#define GBFCKEY 0x47424643	/* color cached */
#define GBUFKEY 0x47425546	/* color uncached */
#define	GBFVKEY	0x47424656	/* color vector uncached */
#define GREGKEY 0x47524547	/* color control registers */
#define GBMONCH	0x47424D4F	/* mono cached */
#define GBMNUNC 0x47424D4E	/* mono uncached */

#ifdef SABLE
# define GRAPHICS_FRAME_ADDR 0x1f004000
# define GRAPHICS_FRAME_SIZE 0x1000
/* 
 * Only the graphics buffer is available to sable.  This is only to be
 * used to check out the graphics region stuff anyway.
 */
# define GRAPHICS_REG_ADDR 0
# define GRAPHICS_REG_SIZE 0
# define GRAPHICS_COLOR_REG 0
# define GRAPHICS_COLOR_DATA 0
# define R3030_GRAPHICS_FRAME_ADDR 0
# define R3030_GRAPHICS_FRAME_SIZE 0
# define R3030_GRAPHICS_REG_ADDR 0
# define R3030_KERNEL_OFFSET 0
# define R3030_GRAPHICS_REG_SIZE 0
# define R3030_GRAPHICS_LINE_SIZE 0
# define R3030_GRAPHICS_VECTOR_FRAME_SIZE 0
# define R3030_GRAPHICS_RAMDAC_ADDR_LO_OFFSET 0
# define R3030_GRAPHICS_RAMDAC_ADDR_HI_OFFSET 0
# define R3030_GRAPHICS_COL_REG_OFFSET 0
# define R3030_GRAPHICS_COL_DATA_OFFSET 0
# define R3030_GRAPHICS_XSERVER_OFFSET 0
# define R3030_WRITE_OFFSET 0
# define R3030_RAMDAC_ADLO_OFFSET 0
# define R3030_RAMDAC_ADHI_OFFSET 0
# define R3030_COL_REG_OFFSET 0
# define R3030_COL_DATA_OFFSET 0
# define R3030_XSERVER_OFFSET 0
#else
# define GRAPHICS_FRAME_ADDR 0x1000000
# define GRAPHICS_FRAME_SIZE 0x200000
# define GRAPHICS_REG_ADDR 0x1ff0000
# define GRAPHICS_REG_SIZE 0x0010000
# define GRAPHICS_COLOR_REG (GRAPHICS_REG_ADDR|0xff03)
# define GRAPHICS_COLOR_DATA (GRAPHICS_REG_ADDR|0xff07)

# define R3030_GRAPHICS_FRAME_ADDR		0x10000000
# define R3030_GRAPHICS_FRAME_SIZE		0x400000
# define R3030_GRAPHICS_REG_ADDR		0x14000000
# define R3030_GRAPHICS_REG_SIZE		0x7000		/* maps using offsets below */

# define R3030_GRAPHICS_LINE_SIZE		0x8000
# define R3030_GRAPHICS_VECTOR_FRAME_SIZE	0x200000

# define R3030_GRAPHICS_COLOR_REG		(R3030_GRAPHICS_REG_ADDR|0x100000)
# define R3030_GRAPHICS_COLOR_DATA		(R3030_GRAPHICS_REG_ADDR|0x180000)

# define R3030_GRAPHICS_RAMDAC_ADDR_LO_OFFSET	(0x0000)
# define R3030_GRAPHICS_RAMDAC_ADDR_HI_OFFSET	(0x1000)
# define R3030_GRAPHICS_COL_REG_OFFSET		(0x2000)
# define R3030_GRAPHICS_COL_DATA_OFFSET		(0x3000)
# define R3030_GRAPHICS_XSERVER_OFFSET		(0x4004)
# define R3030_GRAPHICS_KERNEL_OFFSET		(0x5004)
# define R3030_GRAPHICS_WRITE_MASK		(0x6000)

# define R3030_RAMDAC_ADLO_OFFSET		(0x00000)
# define R3030_RAMDAC_ADHI_OFFSET		(0x80000)
# define R3030_COL_REG_OFFSET			(0x100000)
# define R3030_COL_DATA_OFFSET			(0x180000)
# define R3030_XSERVER_OFFSET			(0x2000004)
# define R3030_KERNEL_OFFSET			(0x2080004)
# define R3030_WRITE_OFFSET			(0x2100000)
#endif
#define GRAPHICS_MAX_COLOR_REG 256

# define MONO_FRAME_SIZE (1152*900/8)		/* 1152x900 pixels */

#ifdef KERNEL
typedef struct {
	int g_type;		/* type of graphics available */
	queue_t *g_wq;		/* slave side of pseudo console */
} JupiterPutc;
#endif /* KERNEL */

#define JP_BOARD	0x00	/* returned by grafprobe when board is avail */
#define JP_NOBOARD	0x01	/* no graphics available use uartputc */
#define JP_STREAM	0x02	/* stream queue is available */

#endif _SYS_GRAFREG_
