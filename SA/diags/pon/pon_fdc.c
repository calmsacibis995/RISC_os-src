#ident "$Header: pon_fdc.c,v 1.2.1.1 90/07/18 14:31:11 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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
#include "mips/cpu_board.h"
#include "mips/cpu.h"
#include "pon.h"

#define DEBUG

/* system address of floppy disk controller */
#define FD_STATUS_REG		0xbe000003
#define FD_DATA_REG		0xbe000007
#define FD_TERMINAL_COUNT	0xbe800003

/* commands for 8272 (only those that are supported) */
#define FD_CMD_MASK		0x1f
#define FD_READ_DATA		0x6
#define FD_READ_DEL_DATA	0xc
#define FD_WRITE_DATA		0x5
#define FD_WRITE_DEL_DATA	0x9
#define FD_FORMAT_TRACK		0xd
#define FD_RECALIBRATE		0x7
#define FD_SENSE_INTR		0x8
#define FD_SPECIFY		0x3
#define FD_CONFIGURE		0x13
#define FD_SENSE_DRIVE		0x4
#define FD_SEEK			0xf
#define FD_NOOP			0x1f
#define FD_READ_ID		0xa
#define FD_DUMPREG		0xe

/* main status register */
#define FD_UNIT0_BUSY		0x1
#define FD_UNIT1_BUSY		0x2
#define FD_UNIT2_BUSY		0x4
#define FD_UNIT3_BUSY		0x8
#define FD_BUSY			0x10
#define FD_NON_DMA		0x20 /* set during exec phase in non-dma mode */
#define FD_TO_CPU		0x40
#define FD_READY		0x80

#define TIMEOUT			5000
#define TIMEOUT0		1000

extern char success[], failure[];

typedef unsigned char un_char;

static struct floppy {
	un_char *fd_status_reg; /* status register */
	un_char *fd_data_reg; /* data register (fifo) */
	un_char *fd_terminal_count; /* termianl count */
} fd0 = {
	(unsigned char *)FD_STATUS_REG,
	(unsigned char *)FD_DATA_REG,
	(unsigned char *)FD_TERMINAL_COUNT
};

static unsigned char init_data[5] = {
	0x65,	/* configure 1 */
	0x48,	/* configure 2 */
	0x00,	/* configure 3 */
	0xdf,	/* specify 1 */
	0x7	/* specify 2 */
};

static unsigned char init_result[5]; /*dumpreg 8, 9, 10, 5, 6*/


void Pon_Delay(count)
int count;
{
int i;
	for (i = 0; i < count; i++) ;
}


Pon_Fdc()
{
register int i;
int error = 0;

	pon_puts("FDC Tests...");

	fdc_init();
	fd_dumpreg();
	for(i = 0; i < 5; i++) {
		if(init_result[i] != init_data[i]) {
#ifdef DEBUG
			printf("Error: i = %d, act = %x, exp = %x\n",
			    init_result[i], init_data[i]);
#endif DEBUG
			error = 1;
			goto done;
		}
	}

	error |= fdc_int_test();
done:
	if (error) {
		pon_puts(failure);
		FastFlash(0);
		SetDepend(PON_FAULT_FDC);
		return(FAIL);
	}
	else {
		pon_puts(success);
		return(PASS);
	}
}


static fdc_init()
{
	*fd0.fd_status_reg = 0x80;
	Pon_Delay(TIMEOUT0);
	*fd0.fd_status_reg = 0x0;
	Pon_Delay(TIMEOUT0);
	fd_sense_drive();
	fd_configure();
	fd_specify();
}


/*
 * This function writes a byte to data fifo.
 */
static void write_fifo(data)
register unsigned data;
{
register unsigned char status;
int count = 0;

	for (count = 0; count < TIMEOUT; count++) {
		status = *fd0.fd_status_reg;
		if ((status & FD_READY) && (!(status & FD_TO_CPU))) {
			*fd0.fd_data_reg = (unsigned char) data;
			wbflush();
			break;
		}
	}

#ifdef DEBUG
	if (count >= TIMEOUT) {
		printf("Write fifo time out.");
		printf("   main status reg = 0x%x\n",status);
	}
#endif DEBUG
}


/*
 * This function reads a byte out of data fifo.
 */
static unsigned char
read_fifo()
{
register unsigned char status;
register unsigned char data;
int count = 0;

	for (count = 0; count < TIMEOUT; count++) {
		status = *fd0.fd_status_reg;
		if ((status & FD_READY) && (status & FD_TO_CPU)) {
			data = *fd0.fd_data_reg;
			return(data);
		}
	}

	if (count >= TIMEOUT) {
#ifdef DEBUG
		printf("Read fifo time out.");
		printf("   main status reg = 0x%x\n",status);
#endif DEBUG
		return(BAD);
	}
}


static fd_sense_drive()
{
	write_fifo(FD_SENSE_DRIVE);
	write_fifo(0x80);
	read_fifo();
}


static fd_specify()
{
	write_fifo(FD_SPECIFY);
	write_fifo(init_data[3]);
	write_fifo(init_data[4]);
}


static fd_configure()
{
	write_fifo(FD_CONFIGURE);
	write_fifo(init_data[0]);
	write_fifo(init_data[1]);
	write_fifo(init_data[2]);
}


static fd_dumpreg()
{
register int i;

	for(i=0; i < 5; i++) {
		init_result[i] = 0;
	}

	write_fifo(FD_DUMPREG);
	read_fifo();
	read_fifo();
	read_fifo();
	read_fifo();
	init_result[3] = read_fifo();
	init_result[4] = read_fifo();
	read_fifo();
	init_result[0] = read_fifo();
	init_result[1] = read_fifo();
	init_result[2] = read_fifo();
}


fd_sense_intr()
{
	write_fifo(FD_SENSE_INTR);
	read_fifo();
	read_fifo();
}


fdc_int_test()
{
register unsigned int cause;
unsigned int count = 0;

	fdc_init();

	write_fifo(FD_RECALIBRATE);
	write_fifo(0);	/* interrupt should happen after sending this command */
	while(!((cause = GetCause()) & CAUSE_IP7)) {
		if (count++ > 350000) {
#ifdef DEBUG
			printf("Count %d, Cause = 0x%x\n", count, cause);
#endif DEBUG
			return(FAIL);
		}
	}

	/* clear interrupt */
	fd_sense_intr();
	return(PASS);
}
