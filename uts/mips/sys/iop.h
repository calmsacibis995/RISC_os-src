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
/* $Header: iop.h,v 1.5.2.2 90/05/10 06:24:43 wje Exp $ */
/*
 * $Header: iop.h,v 1.5.2.2 90/05/10 06:24:43 wje Exp $
 */
/*
 *  IOPB is the communication area between R2000 and IOP.
 */

#define IOPBBASE	0x1000	/* base address of the IOPB */
#define IOPBSIZE	0x2000	/* size of IOPB */
#define IOP_CMDREG_SABLE	0x1f003000
#define IOP_CMDREG	0x2000003
#define IOP_VRTCLEAR	0x1800000
#define PROMCOMM	((IOPBBASE - 0x10)|K1BASE)
#define JUMP_LOC	0
#define VALID_LOC	4
#define GRAPHICS_CLR	8	/* graphics routines will set this
				   memory location when they've clear'd
				   and initialized the video board.  The
				   init(1spp) command will clear this causing
				   the video to be reinitialized */

#define VALID_COOKIE	0x02218503

/*
 * The following are the valid commands for the IOP interface.
 */
#define ClrIRQ0		0
#define ClrIRQ1		1
#define ClrIRQ2		2
#define ClrIRQ4		3
#define SetIOPIRQ	4
/* 5 is unused */
#define LEDOn		6
#define LEDOff		7

/*
 * Reading of the command register returns some status
 */
#define FPU_Not_Present	0x01	/* chip present if bit is zero */
#define DBG_Not_Present	0x02	/* debug board present if bit is zero */
#define Video_Not_Present 0x10	/* video board present if bit is zero */
#define IopIntrReady	0x40	/* If zero the R2000 has issued an intr
				   and the IOP hasn't seen it yet */
#define IopParityErr	0x80	/* If the IOP has received a parity error
				   this bit is zero */

/*
 * iop_alloc will return a K1 address.  If ioc_cmdparm is
 * a pointer to a command block it must be an offset from
 * IOPBBASE.  You can use this macro to convert the K1
 * address from iop_alloc to that offset.
 */
#define K1_TO_IOPB_OFFSET(addr) \
  (K1_TO_PHYS((addr)) - IOPBBASE)

#define NUART 2
#define NSCTARG 8
#define NFLTARG 1
#define NUNUSED 1			/* floppy drive 1 not used */
#define MAXSPARE 3
#define IOPIOCB	0			/* iocb for communication with iop */
#define UARTIOCB	(IOPIOCB+1)	/* iocb for uart, one per channel */
#define NVRAMIOCB	(UARTIOCB+NUART)/* iocb for non-volatile ram */
#define LEDIOCB		(NVRAMIOCB+1)	/* iocb for led */
#define CLOCKIOCB	(LEDIOCB+1)	/* iocb for clock */
#define TODIOCB		(CLOCKIOCB+1)	/* iocb for time of day */
#define NPSCTARG (NFLTARG + NSCTARG)
#define PSCSIIOCB SCSIIOCB /* kernel sees scsi and floppy as pseudo-scsi */
#define SCSIIOCB	(TODIOCB+1)  	/* iocb for scsi, one per target */
#define FLOPPYIOCB	(SCSIIOCB+NSCTARG) /* iocb for floppy, one per drive */
#define LANCEIOCB	(FLOPPYIOCB+NFLTARG+NUNUSED) /* iocb for LANCE */
#define PPIOCB		(LANCEIOCB+1)	/* iocb parallel port */
#define KYBDIOCB	(PPIOCB+1)	/* iocb for keyboard using 8042 chip */
#define MOUSEIOCB	(KYBDIOCB+1)	/* iocb for the on board serial port */
#define BUZZERIOCB	(MOUSEIOCB+1)	/* iocb for the buzzer */
#define MAXIOCB		(BUZZERIOCB+MAXSPARE)

/*
 * For each device there exists an instance of iocb.
 * To perform a operation on a device, R2000 sets up
 * the command parameter and then deposits
 * a non-zero value into the command semaphore of the
 * device's iocb, and interrupts the IOP.
 * Upon notification of the command, the IOP performs the 
 * operation.  At the completion of the operation, the IOP
 * clears the command semaphore, sets up the status parameter, 
 * and deposits a non-zero value in the status semaphore.
 * The R2000 is informed of the completion of the operation by 
 * detecting a non-zero value in the status semaphore.  It
 * process the return status and then clears the status semaphore.
 */

#ifdef LANGUAGE_C
struct iocb {
	u_long ioc_cmdparm;	/* command parameter */
	u_long ioc_statparm;	/* status parameter */
	u_short ioc_cmdsem;	/* command semaphore
				   set by R2000, clear by IOP */
	u_short ioc_statsem;	/* status semaphore
				   set by IOP, clear by R2000*/
	u_long ioc_buf;		/* allocated command block in K1 address */
};

struct iopb {
	struct iocb iop_iocb[MAXIOCB];
	short free_off;		/* First free location in iopb */
	short intr_sem;		/* =0 while V50 writes to interface pal */
	short iopb_pad[6];	
};

# undef IOP_STATS
# ifdef IOP_STATS
struct iop_stats {
    char *s_name;
    u_int s_stat;
};
extern struct iop_stats IopStats[];

#  define IOPSTAT_BUSY 0
#  define IOPSTAT_LOOP 1
#  define IOPSTAT_FREE 2
#  define IOPSTAT_UART_LOSS 3
# endif /* IOP_STATS */
#endif /* LANGUAGE_C */


#define SEMAPHORE_CLEAR 0	/* Guess what this is used for? */
#define SEMAPHORE_SET 0xff	/* value to use when setting a semaphore */

/*
 * Flags that are passed to iop_wait and iop_poke.
 */
#define IOPB_NOWAIT 1		/* iop_poke and iop_wait will return -1
				   if they need to sleep or spin */
#define IOPB_SLEEP 2		/* iop_poke and iop_wait will call
				   sleep if needed. */
#define IOPB_SPIN 3		/* instead of sleeping iop_poke and iop_wait
				   will spin waiting for the semaphore to
				   change. */
#define IOPB_SCAN 4		/* Only needed in the standalone code. Instead
				   of just spinning, _scandevs will be called
				   in the busy loop. */
#define NO_SLEEP_ADDR 0		/* The fourth argument to iop_poke and iop_wait
				   is the address to sleep on if non-zero. */

#ifdef LANGUAGE_C
/*
 * Special IOP commands
 */
typedef struct iop_cblock {
	u_short iop_cmd;
	u_short	iop_status;
	u_short	iop_taskid;
	u_short	iop_offset;
	u_long	iop_page[1];
} IopControl;
#endif /* LANGUAGE_C */

/* iop commands */
#define RELOAD_IPL	1	/* Causes then IOP to reload IPL image */
#define DL		2	/* Down load a new task into IOP */
#define MON_OFF		3	/* disable low-level monitor */
#define MON_ON		4	/* enable low-level monitor */
#define INTR0		5	/* generate IRQ0 */
#define INTR1		6	/* generate IRQ1 */
#define INTR2		7	/* generate IRQ2 */
#define INTR4		8	/* generate IRQ4 */

/*
 * the following commands support peek and poke of arbitrary v50 memory and
 * i/o address.  The data is written from or read to the offset field in the
 * command block and the v50 address is stored in the page field.
 */
#define READ_BYTE	9	/* read 8 bits of information from V50 memory space */
#define READ_HALF	10	/* read 16 bits of information from V50 memory space */
#define WRITE_BYTE	11	/* write 8 bits of information to V50 memory space */
#define WRITE_HALF	12	/* write 16 bits of information to V50 memory space */
#define IREAD_BYTE	13	/* read 8 bits of information from V50 i/o space */
#define IREAD_HALF	14	/* read 16 bits of information from V50 i/o space */
#define IWRITE_BYTE	15	/* write 8 bits of information to V50 i/o space */
#define IWRITE_HALF	16	/* write 16 bits of information to V50 i/o space */

/*
 * get the power on diagnostic results.
 */
#define GET_PON_STATUS	17	/* get the pon status */

/*
 * command to allocate a print buffer in kernel space. this is also used
 * in the statparm field in response to this command.  once a print has
 * happened we must wait for and new buffer to be given before we can
 * print again.  The first short in the print buffer contains the size.
 */
#define GIVE_IOP_BUFFER	18	/* give a print buffer pointer in iop_page */
#define SEND_TEST_STR	19	/* send a test printf */

/* __EOF__ */
