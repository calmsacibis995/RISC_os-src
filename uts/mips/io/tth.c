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
#ident	"$Header: tth.c,v 1.7.1.2 90/05/10 05:37:00 wje Exp $"

/*******************************************************************************
********************************************************************************
****									    ****
****		Copyright (C) Tech-Source Laboratories, Inc.                ****
****                         A Division of Ciprico Inc. 1984.	            ****
****									    ****
****		              Tech-Source Laboratories                      ****
****                          1175 Spring Centre South                      ****
****                        Altamonte Springs, FL 32714                     ****
****                              (305) 788-9100                            ****
****									    ****
****				Ciprico Incorporated			    ****
****				 2405 Annapolis Lane			    ****
****				 Plymouth, MN 55441			    ****
****				   (612) 559-2034			    ****
****									    ****
****	Module Number:	xxxxxxxx                                            ****
****	Module Name:	tth.c						    ****
****	Package:	Tapemaster 3000 Driver for UNIX System 4.2  	    ****
****	Date:		XXXXXXXX                                            ****
****	Module Rev:	(%R%.%L%)					    ****
****									    ****
****	Subroutines:	tthinit, tthattach,                                 ****
****                    tthopen, tthclose, tthstrategy,                     ****
****			tthcmd, tthintr, ttherror,                          ****
****			tthminphys, tthread, tthwrite,		 	    ****
****			tthioctl, getiopb, tthqueue, tthexec, position	    ****
****									    ****
****	Description:							    ****
****		This driver handles UNIX I/O requests for the Ciprico	    ****
****		Tapemaster 3000.					    ****
****									    ****
********************************************************************************
*******************************************************************************/

/*******************************************************************************
*									       *
*				Revision History			       *
*									       *
********************************************************************************

    Revision	Date		Author
	Description of Change

    1.1		11/1/85         Tim Beres
	Initial Release.  This driver based on the system III/V driver by
	D. A. Dickey.

		22 Apr 86	Tony Becker
	This driver was brought to life.  Byte ordering was the only significant
	change.  #ifdef's were added for use on other than 68000 based machines.

		5 May 86	M. Tobias
	Cleaned up error handling, specifically in the case of EOT detection.
	Fixed configuration bug.

		08/29/86	J. Fisher
	Ported to MIPS 4.2.

		11/24/86	R. McNeal
	Ported to MIPS System V.3
	
*******************************************************************************/

/*
	The major points of interest within this file are ordered as follows:
		Include the include files
		Subroutine definitions
		I/O address space
		MIPS driver and controller structs
		TM 3000 Driver internal structs
		tthinit - Probe and existence check for TM 3000
		tthattach - XXXXXXXXX
		tthopen - Open routine
		tthclose - Close routine
		tthstrategy - Strategy routine
		tthcmd - Command execution routine
		tthintr - Interrupt service routine
		ttherror - Routine to print TTPB's
		tthminphys - maximum physio transfer
		tthread - Raw read routine
		tthwrite - Raw write routine
		tthioctl - Ioctl interface routine
		getiopb - Routine to allocate an IOPB
		tthqueue - IOPB queueing routine
		tthexec - Routine to output IOPB address & Channel Attention
		position - positioning routine

********************************************************************************

	The subroutines interact as follows.  The interaction shown is only for
	those subroutines within this driver, unix subroutines are not shown.

	ROUTINE or SYSTEM		CALLS

	5.3 configuration       ==>     tthinit, tthattach
            system

	I/O system		==>	tthopen, tthclose, tthstrategy,
					tthread, tthwrite, tthioctl

	Interrupt Mechanism	==>	tthintr

	tthopen			==>	tthcmd
	tthclose		==>	tthcmd, position
	tthread			==>	tthminphys, tthstrategy (via physio)
	tthwrite		==>	tthminphys, tthstrategy (via physio)
	tthioctl		==>	tthcmd
	tthstrategy		==>	getiopb, tthqueue, position
	tthintr			==>	ttherror, tthexec

	position		==>	tthcmd

	tthcmd			==>	getiopb, tthqueue

	tthqueue		==>	tthexec

	tthinit			==>	tthattach
	tthattach               ==>     (nothing)
	ttherror		==>	(nothing)
	getiopb			==>	(nothing)
	tthexec			==>	(nothing)

********************************************************************************

	- The control flow moves from one of:  tthopen, tthclose, tthstrategy
		to tthqueue.  It then transfers from tthqueue onwards
		to tthexec.
	- The tthexec routine is the actual routine where the address of a
		TTPB is written to the TM 3000 and the Channel Attention
		performed.
	- At interrupt time, tthintr will start execution on an IOPB chain
		that was previously set up by tthqueue.
	- The subroutine tthcmd is used by any routine that has an IOPB
		already built up and ready to execute as a command.  Commands
		are executed one at a time.  A process that calls tthcmd will
		sleep until the command is finished executing.
	- Resources (IOPB's) are allocated via the routine getiopb

********************************************************************************
*/

#ifndef lint
static  char Sccsid[] = "%W%  %D%  Copyright Tech-Source Laboratories, Inc";
#endif


/*
 *  The include file tthu.h is created during the configuration process on
 *  MIPS machines.  Its name itself is dependent on the configuration process.
 *  Basically, what is needed in the include file are two defines:
 *	NTTHU	-  Defines how many TM 3000 Tape Units there are, and
 *	NTTHC	-  Defines how many TM 3000's there are.
 *  Typically these will both be defined to 1.
 */
#define NTTHU 1
#define NTTHC 1

/* If there aren't any drives, don't bother compiling. */


/****** Include whatever system include files are needed here. ******/
#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/vmevar.h"
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/ioctl.h"
#include "sys/mtio.h"
#include "sys/file.h"
#include "sys/tthreg.h"

/*
 * For those routines within the driver, define what they return.
 */

static struct IOPB *getiopb();
void static position();		/* Returns nothing */
static ulong allc();

/*
 *	Define the structures and storage needed for configuration and runtime.
 */

static int tthcsa[NTTHC][NTTHU];	/* Unit - Controller - Slave cross reference */

/*
 * rttbuf are the Buffers for doing Raw I/O.  There is one buffer for each
 * tape drive.
 */

static struct buf rtthbuf[NTTHU];	/* Buffers for Raw I/O */


/*
 *	Now, define and allocate structures used internally by the driver.
 */

/*
 * tth_device is a structure containing information for each tape drive which
 * is solely used by the driver.
 */

static struct TTDEVICE tth_device[NTTHU];

/*
 * tthpblist are the IOPB's themselves.  They are allocated by the routine getiopb().
 */

static	struct IOPB *tthpblist[NIOPB];  

/*
 * Now come some pointers for keeping track of the queued IOPB's.
 *
 * tthaiopb - Points to the first queued IOPB.  The next interrupt should be
 *	for this one.
 * tthclist - Points to the first IOPB in the queued list of IOPB's that haven't
 *	yet been started on the controller.
 * tthcend - Points to the last IOPB in the queued list of IOPB's that haven't
 *	yet been started on the controller.
 * holding - The queued IOPB's for a unit.
 */

static	struct IOPB *tthaiopb = (struct IOPB *) NULL;
static	struct IOPB *tthclist = (struct IOPB *) NULL;
static	struct IOPB *tthcend = (struct IOPB *) NULL;
static	struct LIST holding[NTTHU];

/*
 * openunits - Describes what units are open.  If unit zero is open, bit zero
 *	will be on.  If unit one is not open, bit one will be off.
 */

static	int openunits;			/* If a unit is open, set bit. */

/*
 * niopb - Holds the max count of iopb's allocatable in tthinit and getiopb.
 * The only way it changes is if in getiopb memory for another iopb could
 * not be allocated
 */

static 	int niopb=NIOPB;

/*
 * lblkno is used for positioning of block device I/O.  One value for each
 * tape drive.  It describes the block position of the tape drive upon
 * completion of all queued IOPB's for that drive.  It assumes that there
 * will be no errors; if errors occur, this value will probably not be correct.
 */

static	int lblkno[NTTHU];		/* Used for block devices */

/*
 * unitsignals is used within tthopen.  It describes the signal value for the
 * configure command on a particular unit.  Check your tape drive manual to see
 * what signal settings this table should have.
 */

static int unitsignals[10] = {
	CIPHER,		/* Unit zero - Cipher F880 */
	CIPHER,		/* Unit one - Cipher F880 */
	CIPHER,		/* Unit two - Cipher F880 */
	CIPHER,		/* Unit three - Cipher F880 */
	CIPHER,		/* Unit four - Cipher F880 */
	CIPHER,		/* Unit five - Cipher F880 */
	CIPHER,		/* Unit six - Cipher F880 */
	CIPHER,		/* Unit seven - Cipher F880 */
	CIPHER,		/* Unit eight - Cipher F880 */
	CIPHER		/* Unit nine - Cipher F880 */
};

/*
 * cblock is used for changing the configuration of the TM 3000.  This only
 * happens inside tthioctl() and tthopen().
 */

static	struct CONF_INFO tth_cblock;

/*
 * Address table of controllers.  This should be removed when the driver
 * is coded for lboot.
 */
static struct TTREG *tthstd[] = {
	{ (struct TTREG *)0xbd008500 },
};

/*
 * When a controller has been successfully probed its address will be
 * placed in here.
 */
static volatile struct TTREG *reset[NTTHC];

/* TRACE is a macro the converts to a putchar when the comment markers are
 *	removed or produces nothing when they are left in place.  Generally
 *	speaking, when a routine is entered a capital letter is printed; when
 *	the routine exits, the corresponding small letter is printed.  If the
 *	routine exited in error, the small letter will be preceed by an
 *	asterisk.
 */
int tthprintf;
	/* single string messages */
#define	TRACE(str)	/* printf("%s", str) */
	/* This code is for a verbose routine trace */
#define TTHDEBUG(code)	/* code */


/*****************************************************************************
*									     *
*	Subroutine:	tthinit (use to be tthprobe)		 	     *
*									     *
*	Calling Sequence:	tthinit(reg)				     *
*									     *
*	Called By:	main io_init					     *
*									     *
*	Calling Parameters:	reg - base address of T3000 I/O register     *
*									     *
*	Local Variables:						     *
*			timer - timeout mechanism			     *
*			i     - index into reset list                        *
*									     *
*	Calls SuBbroutines:	none    				     *
*									     *
*	Public/Global Variables:					     *
*			tthclist - Initialized to zero.			     *
*			tthaiopb - Initialized to zero.			     *
*			tthpblist - allocates first IOPB of list             *
*			reset - array of controller register structures      *
*									     *
*	Description:							     *
*		This routine is called exactly once, to test for the existen *
*		of and to reset the TM3000.  It also initializes a few	     *
*		variables for the rest of the driver.			     *
* 									     *
*	Notice:								     *
*		This routine assumes only one controller without any 	     *
*		type of configuration (lboot).				     *
*****************************************************************************/

tthinit()
{
	register int i;
	volatile struct TTREG *reg = tthstd[0];
	int dummy;

	TTHDEBUG(printf("\nProbe Entry  "););
	TRACE("P");

	/*
	 * If the board was already reset once, just return.  What we need to
	 * do is look up in the reset table whether or not this controller
	 * register value exists.  It will exist if the controller was already
	 * reset once successfully.
	 */
	for (i = 0; i < NTTHC && reset[i] != 0; i++)
		if (reset[i] == reg) {	/* Is this already in the table? */
			TRACE("probe: alredy reset\n");
			return(sizeof (struct TTREG));	/* YES */
		}

	/* Not reset yet, so let's go through the procedure. */

	/* Read the reset register. */

	if (badaddr(&reg->reset,sizeof (reg->reset))) {
		/* badaddr returned false.  Means that we got a memory fault,
		 * which means that the board isn't at (reg). */
		TRACE("*probe: Tapemaster board is not alive\n");
		return (0);
	}

	dummy = reg->reset;		/* read the reset register to reset */

	/* Give the board 200 milliseconds to reset itself. */
	DELAY(200000);

	/* Has the first IOPB been allocated? */
	if (tthpblist[0] == (struct IOPB *) NULL) {
		/* No, allocate it. */
		if ((tthpblist[0] = (struct IOPB *)allc()) == 0) {
			printf("tthinit: Can't allocate memory for first IOPB\n");
			TRACE("*probe: can't allocate mem\n");
			return(0);
		}
		tthpblist[0]->status = 0;  /* Don't forget to zero status. */
	}

	/* Everything went ok, put reg into the reset table and return. */
	reset[i] = reg;

	/* Initialize ttclist & tthaiopb */
	tthclist = tthaiopb = (struct IOPB *) NULL;

	/* lboot will give us valid information */
	tthattach (0,0,0);
	
	TTHDEBUG(printf("probe end\n"););
	TRACE("p");
	/* Success! */
	return(sizeof (struct TTREG));
}

/*****************************************************************************
*									     *
*	Subroutine:	tthattach					     *
*									     *
*	Calling Sequence:	tthattach(ctlr, slave, unit)		     *
*									     *
*	Called By:	UNIX Initialization sequence.			     *
*									     *
*	Calling Parameters:						     *
*		device - Pointer to device structure.			     *
*									     *
*	Local Variables:	None.					     *
*									     *
*	Calls Subroutines:	None.					     *
*									     *
*	Public/Global Variables:					     *
*		tthcsa - Controller-Slave Association table.		     *
*									     *
*	Description:							     *
*		This routine makes an entry in the tthcsa table for the      *
*		device it is called with.  The tthcsa table is used 	     *
*		elsewhere in the driver in order to translate from a 	     *
*		controller-slave combination to a unit value.		     *
*		Then it returns one signifying everything is ok.	     *
*									     *
*****************************************************************************/

tthattach(ctlr, slave, unit)
{
	TRACE("A");

	/* Enter the Unit Number in the Controller-Slave association table. */
	tthcsa[ctlr][slave] = unit;

	TRACE("a\n");
	return(1);		/* Always return a one for now. */
}

/*******************************************************************************
*									       *
*	Subroutine:	tthopen						       *
*									       *
*	Calling Sequence:	tthopen(dev, flag)			       *
*									       *
*	Called By:	UNIX I/O System.				       *
*									       *
*	Calling Parameters:						       *
*		dev - Major & minor number of device.			       *
*		flag - specifies what kind of open mode (Read, Write, both..)  *
*									       *
*	Local Variables:						       *
*		unit - Drive unit...from dev.				       *
*		drive - Ptr to drive info.				       *
*		iopb - Pointer to pb.					       *
*		status - Ptr to iopb command status for drive 		       *
*		ps - temporary for processor status.			       *
*		pb - TTPB...used to get drive status.			       *
*               cblock - Ptr to TTH_CBLOCK structure in mem.                   *
*									       *
*	Calls Subroutines:	tthcmd          			       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - local driver information about drive.	       *
*		openunits - exclusive open mechanism			       *
*		lblkno - Tape position for buffered mode.		       *
*               unitsignals - config. signal value for drives                  *
*									       *
*	Description:							       *
*		We first get the drive status to make sure it is online & also *
*		so we know the controller is talking to it.  We then check to  *
*		make sure the tape is writable if that open mode was requested.*
*		We then wait for the tape to enter a state of NOT REWINDING    *
*		or NOT ONLINE.  If it has successfully rewound, we then check  *
*		for an exclusive open.  Setting the bit in openunits if we are *
*		the first open and refusing to open the drive if we aren't the *
*		first.  Then what we do it to configure the unit according to  *
*		the Speed/Density bits in the minor device number and the      *
*		value of unitsignals for the unit.  Lastly initialize	       *
*		tth_device for this unit.				       *
*									       *
*******************************************************************************/

tthopen(dev, flag)
dev_t dev;
int flag;
{
	register int unit = UNIT(dev);
	register struct TTPB *iopb;

	register ps;
	register word status;
	register struct TTH_CBLOCK *cblock;
	register int fmtr;
	struct TTPB pb;

	TTHDEBUG(printf("\nOpen Entry"););
	TRACE("O");

	/* This assumes only one controller (the rest of the driver
	 * implies this many places), but this hard coded check is
	 * the easy way to solve this problem for now.
	 */
	if (reset[0] == 0) {
		u.u_error = ENXIO;
		return;
	}

	/* Does the unit look reasonable? */
	if (unit >= NTTHU) {
		TRACE("*bad drive number\n");
		return(ENXIO);
	}

	/* Clear the garbage out before using */
	bzero (&pb, sizeof(pb));
	/* Point iopb to our TTPB. */
	iopb = &pb;
	/* And stuff what we need into it for a DSTATUS command. */
	iopb->cmd = DSTATUS;
	iopb->control = unit;		/* ????? is control the unit
					 * number
					 */

	iopb->levela = TTH_LEVEL;
	iopb->vectora = TTH_VECTOR;
	iopb->levelb = TTH_LEVEL;
	iopb->vectorb = TTH_VECTOR;

	iopb->lkptram = TTH_ADDMOD;
	iopb->sdptram = TTH_ADDMOD;

	iopb->count = 1;

#ifdef notdef
	/* References to status are really our local tthdevice info */
	status = &tth_device[unit].dsr;
#endif notdef

	/* And execute the command. */
	tthcmd(unit, iopb);

	/* Make the status of command easily accessible */
	/**status = SWAB(iopb->dsr);not this one*/
	status = (iopb->dsr);

	/* Was there an error in the command? */
	if (iopb->gate & G_ERR) {
		tth_device[unit].dsr = 0;	/* YES */
		printf("9-track tape%d:  Can't get drive status, ", unit);
		printf("command error = %x\n", iopb->errco);
		u.u_error = EIO;
		return;
	}

	/* Is the drive online? */
	if (!(status & DS_ONL)) {		/* NO */
		printf("9-track tape%d:  Not online.\n", unit);
		u.u_error = EIO;
		return;
	}

	/* If the drive is file protected and the caller tried to open for a
	 * write, return an error. */
	if ((status & DS_FPT) && (flag & FWRITE)) {
		printf("9-track tape%d: write protected.\n", unit);
		u.u_error = EIO;
		return;
	}

	/* While the drive is rewinding and online... */
	while ((status & DS_RWD) && (status & DS_ONL)) {
	/* Sleep for one-second, sleep/timeout/wakeup */
		delay(HZ); 

		/* Execute the DSTATUS command again. */
		tthcmd(unit, iopb);
		status = (iopb->dsr);

		/* Did it error out? */
		if (iopb->gate & G_ERR) {
			tth_device[unit].dsr = 0;
			printf("9-track tape%d:  Can't get drive status.\n", unit);
			u.u_error = EIO;
			return;
		}
	}

	/* The drive is not rewinding or not online; and the last DSTATUS
	 * command was successful. */
	tth_device[unit].dsr = status;		/* Capture the drive status. */

	/* If the drive is still rewinding or went offline, return an error. */
	if ((status & DS_RWD) || !(status & DS_ONL)) {
		TRACE("*drive rewinding or off-line");
		u.u_error = EIO;
		return;
	}

	ps = spltty();	/* Up the processor priority */

	/* Is the unit already open? */
	if (openunits & (1<<unit)) {
		splx(ps);		/* Restore the priority. */
		TRACE("*unit already open");
		u.u_error = EACCES;
		return;
	}
	openunits |= (1<<unit);		/* NO, mark it open NOW. */
	splx(ps);			/* Restore the priority */

	/* Prepare to configure the unit for the speed & density bits in the
	 * minor device number.  Also, set up the signals according to unit
	 * signals. */

	/* Allocate some memory for the CONFIGURE command. */
	if ((cblock = (struct TTH_CBLOCK *) allc()) != 0) {

		/* The first time through here, initialize some elements of
		 * the TTH_CBLOCK structure. */
		if (!(tth_device[unit].status & INITIALIZED)) {
		   cblock->throttle = DEF_THROTTLE; 	/* The throttle. */
		   cblock->retries = DEF_RETRIES; 	/* The retry count. */
		   cblock->signals = unitsignals[unit];	 /* The signals byte. */
		   cblock->buscntrl = WIDTH32;
		   cblock->tapecn1 = 0;
		   cblock->tapecn2 = 0;
		   tth_device[unit].status |= INITIALIZED;
		} else {
		   /* use old cblock */
		   fmtr = unit/4;
		   *cblock = tth_cblock.fmtr[fmtr];
		}
		/* Set up the TTH_CBLOCK structure for each open. */
		/* If the density bit is on in the minor device number, select HIGHDEN for the tape. */
		if (DENSITY(dev))
		   if ((unit%4) >> 1)
		      cblock->tapecn2 |= (HIGHDEN << ((unit%2) * 4));
		   else
		      cblock->tapecn1 |= (HIGHDEN << ((unit%2) * 4));
		else
		   if ((unit%4) >> 1)
		      cblock->tapecn2 &= ~(HIGHDEN << ((unit%2) * 4));
		   else
		      cblock->tapecn1 &= ~(HIGHDEN << ((unit%2) * 4));
	
		/* If the speed bit is on in the minor device number, select HIGHSPD for the tape. */
		if (SPEED(dev))
		   if ((unit%4) >> 1)
		      cblock->tapecn2 |= (HIGHSPD << ((unit%2) * 4));
		   else
		      cblock->tapecn1 |= (HIGHSPD << ((unit%2) * 4));
		else
		   if ((unit%4) >> 1)
		      cblock->tapecn2 &= ~(HIGHSPD << ((unit%2) * 4));
		   else
		      cblock->tapecn1 &= ~(HIGHSPD << ((unit%2) * 4));
	
		/* If the extend bit is on in the minor device number, select EXTGAP for the tape. */
		if (EXTEND(dev))
		   if ((unit%4) >> 1)
		      cblock->tapecn2 |= (EXTGAP << ((unit%2) * 4));
		   else
		      cblock->tapecn1 |= (EXTGAP << ((unit%2) * 4));
		else
		   if ((unit%4) >> 1)
		      cblock->tapecn2 &= ~(EXTGAP << ((unit%2) * 4));
		   else
		      cblock->tapecn1 &= ~(EXTGAP << ((unit%2) * 4));
	
		/* This IOPB now becomes a CONFIGURE. */
		iopb->cmd = CONFIGURE;
		/* Set the source/destination pointer. */

		iopb->sdptr = (K0_TO_PHYS((int)cblock));

		iopb->sdptram = TTH_ADDMOD;
	
		/* Execute the command. */
		tthcmd(unit, iopb);
	
		/* Set our internal configure info */
		fmtr = unit/4;
		tth_cblock.fmtr[fmtr] = *cblock;

		/* free the TTH_CBLOCK memory */
		free(cblock);

		/* NOTE: errors in configure are ignored */
	} else {
		printf("tth: can't allocate cblock\n");
	}
	/* Initialize tth_device */
	tth_device[unit].status &= ~RAWDEV;
	tth_device[unit].blkno = 0;
	lblkno[unit] = 0;
	if (flag & FWRITE)
		tth_device[unit].lastblkno = 0;
	else
		tth_device[unit].lastblkno = 10000000;	/* Infinity */
	TTHDEBUG(printf("Open Exit\n"););
	TRACE("o");
	return;
}

/*******************************************************************************
*									       *
*	Subroutine:	tthclose					       *
*									       *
*	Calling Sequence:	tthclose(dev, flag)			       *
*									       *
*	Called By:	UNIX I/O System.				       *
*									       *
*	Calling Parameters:						       *
*		dev - Major & Minor of device.				       *
*		flag - same flag as for the open of this device.	       *
*									       *
*	Local Variables:						       *
*		unit - Drive unit...from dev.				       *
*		iopb - Pointer to pb.					       *
*		device - Pointer to the device information for this unit.      *
*		ds - Pointer to local driver information for this unit.	       *
*		ps - temporary for processor status.			       *
*		pb - TTPB for commands during close.			       *
*		status - Ptr to memory for status return                       *
*									       *
*	Calls Subroutines:	position, tthcmd			       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - local driver information			       *
*		openunits - exclusive open mechanism			       *
*									       *
*	Description:							       *
*		When closing a tape device, we want to write two file marks    *
*		if it was opened for writing, zero for reading.  If we took    *
*		the action of writing file marks, back up over one of them if  *
*		we are not supposed to rewind the tape; or simply rewind it.   *
*		If it happened to be open for reading, rewind the tape if we   *
*		are supposed to, or leave it be.  Finally, turn off the open   *
*		bit in openunits.					       *
*									       *
*******************************************************************************/

tthclose(dev, flag)
dev_t dev;
int flag;
{
	register unit = UNIT(dev);
	register struct TTPB *iopb;
	register struct TTDEVICE *ds;
	register ps;
	struct TTPB pb;

	TTHDEBUG(printf("\nClose Entry "););
	TRACE("C");
	
	/* Point ds to the correct unit. */
	ds = &tth_device[unit];

	/* Clear the garbage out before using */
	bzero (&pb, sizeof(pb));
	/* Start preparing our TTPB for commands. */
	iopb = &pb;
	iopb->control = unit;		/* ????? what does control mean in
					 * this case
					 */
	iopb->sdptr = (NULL);
	iopb->levela = TTH_LEVEL;
	iopb->vectora = TTH_VECTOR;
	iopb->levelb = TTH_LEVEL;
	iopb->vectorb = TTH_VECTOR;

	/* Was this unit open for writing? */
	if (flag & FWRITE) {		/* YES */

		/* If this drive became write protected somehow, then we want
		 * to skip writing filemarks which will only cause more errors. */
		if (!(ds->dsr & DS_FPT)) {	/* Not write protected */

			/* Ok, go ahead and set up the write filemark command */
			iopb->cmd = WFM;
			iopb->control |= C_SKP;	/* Ignore EOT if it is hit */

			/* And execute it twice. */
			tthcmd(unit, iopb);
			tthcmd(unit, iopb);

			iopb->control &= ~C_SKP;/* Ignore EOT if it is hit */

			/* Set the C_REVERSE flag so that we will wait for the
			 * formatter to become not busy before issuing either
			 * the REWIND or SFM(-1) */
			iopb->control |= C_REVERSE;
		}

		/* If this was a no rewind device, just backup over ONE of the filemarks. */
		if (NOREWIND(dev)) {
			/* Adjust our TTPB. */
			iopb->cmd = SFM;
			iopb->count = (1);
		} else {
			/* C_REVERSE flag doesn't need to be set for doing the
			 * rewind.  What is going on though is that the process needs
			 * to make it into the test for formatter not busy below. */
			iopb->control |= C_REVERSE;
			iopb->cmd = REWIND;		/* And we really want to do a rewind. */
		}
	} else if (NOREWIND(dev)) {	/* Not open for writing. */
		/* NOTE: this command doesn't get executed.  The check below for != DSTATUS fails and the code is bypassed. */
		iopb->cmd = DSTATUS;
	} else {			/* Not open for writing, but IS a rewind unit. */
		iopb->cmd = REWIND;
		iopb->control |= C_REVERSE;
	}

	if (iopb->cmd != DSTATUS) {	/* Skip this code for DSTATUS commands. */
		/* For doing commands that move the tape backwards, we need to
		 * wait for the formatter to become not busy. */

		if (iopb->control & C_REVERSE) {
			struct TTPB pbtmp;	/* Another temporary TMTPB on the stack. */

			/* Clear the garbage out before using */
			bzero (&pbtmp, sizeof(pbtmp));
			/* Point iopb to the new temp TTPB and set it up. */
			iopb = &pbtmp;
			iopb->cmd = DSTATUS;
			iopb->control = unit;
			iopb->sdptr = (NULL);
			iopb->levela = TTH_LEVEL;
			iopb->vectora = TTH_VECTOR;
			iopb->levelb = TTH_LEVEL;
			iopb->vectorb = TTH_VECTOR;

			/* Execute DSTATUS until formatter ready. */
			do {
				tthcmd(unit, iopb);
				/*ds->dsr = SWAB(iopb->dsr);*/
				ds->dsr = (iopb->dsr);
			} while ((ds->dsr & DS_FBY) && iopb->errco == 0);

			/* Point iopb back to the other TTPB. */
			iopb = &pb;
		}

		/* Execute the REWIND or SFM(-1) */
		tthcmd(unit, iopb);
	}

	/* Set the internal information to signify that this drive has not been written on or moved. */
	ds->status &= ~USED;
	ds->status &= ~INITIALIZED;

	/* Turn off the appropiate bit in openunits, this has to be done at high priority. */
	ps = spltty();

	openunits &= ~(1<<unit);
	/* Restore priority */
	splx(ps);
	TTHDEBUG(printf("Close Exit\n"););
	TRACE("c");
}

/*******************************************************************************
*									       *
*	Subroutine:	tthstrategy					       *
*									       *
*	Calling Sequence:	tthstrategy(bp)				       *
*									       *
*	Called By:	UNIX I/O System.				       *
*									       *
*	Calling Parameters:						       *
*		bp - Pointer to I/O request.				       *
*									       *
*	Local Variables:						       *
*		aiopb - Pointer to actual IOPB used by controller.	       *
*		iopb - Pointer to pb (local).				       *
*		ds - Pointer to drivers local drive information.	       *
*		unit - Drive unit...from bp->b_dev.			       *
*		blocks - Number of blocks in data transfer.		       *
*		ps - temporary for processor status.			       *
*		pb - local TTPB.					       *
*									       *
*	Calls Subroutines:	getiopb, position, tthqueue		       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - Drivers local drive information.		       *
*									       *
*	Description:							       *
*		First thing is to make a few consistency checks:  that the     *
*		drive is sensible, if the request is a write that the drive is *
*		writable and for writing that we aren't trying to read	       *
*		past the last written record.				       *
*		Next we get on with the real work for translating the request  *
*		into either a read or write operation.			       *
*									       *
*******************************************************************************/

tthstrategy(bp)
register struct buf *bp;
{
	register volatile struct IOPB *aiopb;
	register struct TTPB *iopb;
	register struct TTDEVICE *ds;
	register int unit;
	register ps;
	struct TTPB pb;

	TTHDEBUG(printf("\nStrategy Entry "););
	TRACE("S");

	/* Get the drive unit from the buffer. */
	unit = UNIT(bp->b_dev);

	/* Does the drive look valid and alive? */
	if (unit < 0 || unit >= NTTHU) {
		/* NO, set the error flag, finish the transfer and return. */
		bp->b_flags |= B_ERROR;
		iodone(bp);
		TRACE("*invalid drive");
		return(0);
	}

	/* Point ds to our internal table for the drive. */
	ds = &tth_device[unit];

	/* Is there a valid drive status?  Is this a write on a file protected tape? */
	if (ds->dsr == 0 ||
	   ((bp->b_flags & B_READ) == 0) && (ds->dsr & DS_FPT)) {
		/* YES...set the error flag, finish the transfer and return. */
		bp->b_flags |= B_ERROR;
		iodone(bp);
		TRACE("*file protect");
		return(0);
	}

	/* If we are trying to read past the last known block number on the
	 * tape, set the residual count to the transfer count, finish the
	 * transfer and return. */
	if ((ds->lastblkno <= bp->b_blkno) && (bp->b_flags & B_READ)) {
		bp->b_resid = bp->b_bcount;
		iodone(bp);
		TRACE("s");
		return(0);
	}

	/* Ok, something will actually happen to the tape at this point.
	 * Set the internal flag to USED. */
	ds->status |= USED;
	TTHDEBUG(printf("(%c,%d,%d,%d)", (bp->b_flags & B_READ) ? 'R' : 'W', bp->b_blkno, btoc(bp->b_bcount), lblkno[unit]);)

	/* Clear the garbage out before using */
	bzero (&pb, sizeof(pb));
	/* Point iopb at our TTPB and start stuffing it. */
	iopb = &pb;
	iopb->control = unit;
	iopb->levela = TTH_LEVEL;
	iopb->vectora = TTH_VECTOR;
	iopb->levelb = TTH_LEVEL;
	iopb->vectorb = TTH_VECTOR;
	if (bp->b_flags & B_READ)
		iopb->cmd = NORMREAD;
	else
		iopb->cmd = NORMWRITE;

	/* Get an actual IOPB to use from getiopb(). */
	aiopb = getiopb();

	iopb->count = (bp->b_bcount);
	iomap (bp);
	iopb->sdptr = pttob(kvtokptbl(bp->b_dmaaddr)->pgm.pg_pfn) | poff(bp->b_dmaaddr);

	iopb->sdptram = TTH_ADDMOD;

	/* Bump our count up if odd */
	if (bp->b_bcount & 1)
		iopb->count++;
	
	/* Copy the temp TTPB into the actual IOPB. */
	aiopb->pb = pb;
	/* And set the bp. */
	aiopb->bp = bp;

	/* Set the status of IOPB */
	aiopb->status |= TRANSFER;
	aiopb->status |= (bp->b_flags & B_READ) ? TREAD : TWRITE;

	/* Here the fun begins...
		check to see if the transfer crosses page boundrys.
		If it does, split up the command into multiple 'ring buffer'
		commands and use the Ciprico 'ring buffer read scatter' and
		'ring buffer write gather' commands.
	*/

	/* cross page boundarys? */
	if (pnum(bp->b_un.b_addr) != pnum(bp->b_un.b_addr + bp->b_bcount - 2)) {
		ulong	xfer,len,addr;
		int	i;

		TRACE("xfer crosses page bound\n");

		/* change the command */
		aiopb->pb.cmd = (bp->b_flags & B_READ) ? RUR : WUR;

		/* point src/dst pointer to first ring buffer */
		aiopb->pb.sdptr = K0_TO_PHYS(&(aiopb->rb[0]));

		/* now go through and set up all the ring buffers */
		len = bp->b_bcount;
		addr = (ulong)(bp->b_dmaaddr);
		for (i=0;i<9 && len > 0; i++) {
			/* calculate how much to xfer */
			xfer = NBPP - poff(addr);
			if (xfer > len)
				xfer = len;
			aiopb->rb[i].gate = RB_RDY;	/* buffer ready */
			aiopb->rb[i].sdptr = pttob(kvtokptbl(addr)->pgm.pg_pfn) | poff(addr);
			aiopb->rb[i].sdptram = TTH_ADDMOD;	/* addr mod */
			aiopb->rb[i].bcount = xfer;	/* how much */

			if (i > 0) { 	/* link to previous buffers */
				aiopb->rb[i-1].nxptram = TTH_ADDMOD;
				aiopb->rb[i-1].next = 
					K0_TO_PHYS(&(aiopb->rb[i]));
			}
			len -= xfer;	/* this much done */
			addr += xfer;	/* bump the address */
		}
		if (i >= 9 && len != 0) {
			printf("tth: ring buffer overflow\n");
			return;
		}

		/* set the last buffer flag */
		aiopb->rb[i-1].gate |= RB_LST;
	}

	/* Up the priority */
	ps = spltty();

	/* Position the drive */
	position(unit, bp->b_blkno);
	aiopb->end = bp->b_blkno + BTOBB(bp->b_bcount);

	/* Queue the IOPB */
	tthqueue(aiopb);

	/* Restore priority */
	splx(ps);
	TTHDEBUG(printf("Strategy Exit\n"););
	TRACE("s");
}

/*******************************************************************************
*									       *
*	Subroutine:	tthcmd						       *
*									       *
*	Calling Sequence:	tthcmd(unit, uiopb)		       		*
*									       *
*	Called By:	tthopen, tthclose, tthioctl, position		       *
*									       *
*	Calling Parameters:						       *
*		unit - Drive unit that iopb is for.			       *
*		uiopb - Pointer to TTPB of cmd.				       *
*									       *
*	Local Variables:						       *
*		ps - temporary for processor status			       *
*		iopb - IOPB to use for cmd.				       *
*									       *
*	Calls Subroutines:	getiopb, tthqueue			       *
*									       *
*	Public/Global Variables:					       *
*		tthpblist - To wakeup any processes needing an IOPB.	       *
*		lblkno - Position of tape in Buffered mode (vs. Raw) after all *
*			currently queued operations are finished.	       *
*									       *
*	Description:							       *
*		This routine gets an actual IOPB that the controller will read *
*		and stuffs the user iopb into it.  It also sets the IOPB       *
*		status & blkno values.  It then queues the IOPB at high	       *
*		priority to block out interrupts.  Finally, it waits for the   *
*		CMD status bit to drop, signalling completion of the command   *
*		and then copies out the IOPB for the calling routine and       *
*		wakes up any process waiting for an IOPB.		       *
*									       *
*******************************************************************************/

static
tthcmd(unit, uiopb)
int unit;
register struct TTPB *uiopb;
{
	register ps;
	register volatile struct IOPB *iopb;
	int timeout, counter;

	TTHDEBUG(printf("\nCommand Entry "););
	TRACE("CMD ");

	/* Get a real IOPB from getiopb() for the command. */
	iopb = getiopb();

	/* Set the status */
	iopb->status |= CMD;

	/* Copy the pre-setup TTPB into our real one. */
	iopb->pb = *uiopb;

	/* Commands start with the tape at its ending position. */
	iopb->end = lblkno[unit];

	/* Depending on the command being executed, set the status and block number. */
	switch (uiopb->cmd) {
		case WFM:
			iopb->status |= TWRITE;
			iopb->end = 0;
			break;
		case SPACE:
			iopb->end += (unsigned long) (iopb->pb.count) * (iopb->pb.control & C_REVERSE ? -1 : 1);
			iopb->status |= TSEEK;
			break;
		case REWIND:
		case SFM:
			iopb->status |= TSEEK;
			iopb->end = 0;
			break;
	}

	TTHDEBUG(printf("CMD: starting queue"););

	ps = spltty();	/* Up the priority */

	tthqueue(iopb);				/* Queue the real IOPB */

	TTHDEBUG(printf(" CMD: wait for cmd complete"););
	/* While the command bit is on (signifies not finished yet), sleep */

	while (iopb->status & CMD)
		sleep(&iopb->status, PRIBIO);

	TRACE(" CMD: done\n");
	TTHDEBUG(printf(" CMD: done\n"););

	/* Copy out the real iopb for the caller when it finishes. */
	*uiopb = *(struct TTPB *)K0_TO_K1(&iopb->pb);

	/* We must release the real iopb and wake up anyone waiting for it.
	 * tthintr() doesn't do this for commands so we'll be sure to get the
	 * right data when we copy out the iopb. */
	iopb->status &= ~PBINUSE;
	if (tthpblist[0]->status & PBWANTED) {
		tthpblist[0]->status &= ~PBWANTED;
		wakeup(tthpblist[0]);
	}

	splx(ps);		/* Restore the priority and return */
	TTHDEBUG(printf(" Command Exit\n"););
	TRACE(" cmd ");
}

/*******************************************************************************
*									       *
*	Subroutine:	tthintr						       *
*									       *
*	Calling Sequence:	tthintr()				       *
*									       *
*	Called By:	UNIX Interrupt Service Mechanism		       *
*									       *
*	Calling Parameters:	None.					       *
*									       *
*	Local Variables:						       *
*		found - Set to TRUE if interrupt is for use, else FALSE        *
*		ctlr - which controller int was for                            *
*		err - error code returned by controller if an error occurred.  *
*		iopb - Pointer to iopb at start of IOPB chain.		       *
*		bp - Buffer pointer of I/O request for this transfer.	       *
*		reg - Base I/O address of controller space                     *
*		ds - Pointer to local drive information.		       *
*		restart - If non-zero, SPACE this # of records for an error    *
*			recovery attempt.				       *
*									       *
*	Calls Subroutines:	ttherror, tthexec			       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - Driver's local device information.		       *
*		tthpblist - To see if anyone needs an IOPB.		       *
*		tthaiopb - Start of IOPB chain that caused the present int.    *
*		tthclist - Start of IOPB chain waiting to be started.	       *
*									       *
*	Description:							       *
*		This is the interrupt service routine for the TM3000.  What we *
*		do is clear the interrupt and then process the IOPB.	       *
*		A lot of work is done in case of an error, and not much	       *
*		if everything is ok.  The error section consists mostly	       *
*		of trying to guess the position of the tape.		       *
*		Where applicable, bp->b_resid & bp->b_flags are also	       *
*		set.  For the really off the wall errors, we just print an     *
*		error message using ttherror.  For a successful iopb, we update*
*		ds->blkno to its new value (and maybe ds->lastblkno) and set   *
*		bp->b_resid to zero.  For all IOPB's we tell UNIX that the     *
*		transfer is done (via iodone) and release any system resources *
*		we are holding.  Then we wakeup any sleeping processes that    *
*		should be...due to IOPB's free.  If the IOPB was queued via    *
*		tthcmd, zero the CMD bit in status & wakeup whatever needs to  *
*		be.  Finally, after the present IOPB chain is done...queue the *
*		next if one is waiting.					       *
*									       *
*******************************************************************************/

tthintr() 
{
	register struct LIST *hlist;
	register int err, unit, ctlr;
	register volatile struct IOPB *iopb;
	register struct buf *bp;
	struct TTDEVICE *ds;
	int dowakeup = FALSE;
	int dowanted = FALSE;
	int found = FALSE;

	register state;

	TTHDEBUG(printf("\nInterrupt Entry "););
	TRACE("I\n");

	/* find the interrupting controller */
	for (ctlr = 0; ctlr < NTTHC; ctlr++) {

	   /* does controller exist */
	   if (reset[ctlr] == 0)
		continue;			      /* NO; iterate in loop */

	   /* Point iopb to the IOPB we are expecting an interrupt for. */
	   iopb = tthaiopb;
	   TTHDEBUG(printf("\ndsr %x  errco %x  gate %x\n", iopb->pb.dsr, iopb->pb.errco, iopb->pb.gate);)

	   /* is the controller REALLY interrupting? */
	   /* MJT - changed to check G_CMP bit instead of equals */
	   if (!(iopb->pb.gate & G_CMP))
		continue;			      /* NO; iterate loop */

	   /* It is us that is interrupting: set the found flag */
	   found = TRUE;

	   /* Get the unit select from the IOPB and the holding list into hlist. */
	   unit = tthcsa[ctlr][iopb->pb.control & C_UNITSEL];
	   hlist = &holding[unit];
   
	   /* Point ds to our internal table for this drive.  The drive
	    * is calculated from the IOPB */
	   ds = &tth_device[unit];

	   /* Set up bp to point to the appropiate buffer pointer, if applicable */
	   if (iopb->pb.cmd == NORMWRITE || iopb->pb.cmd == NORMREAD 
		|| iopb->pb.cmd == RUR || iopb->pb.cmd == WUR ) {
		bp = (struct buf *)K1_TO_K0(iopb->bp);
	   } else {
		bp = (struct buf *) NULL;	/* Not applicable */
	   }

	   /* Did an error occur? */
	   if ((err = iopb->pb.errco) == 0) {
		/* NO */

		TRACE("iNE\n");

		/* if there was a transfer done */
		if (bp) {
			/* transfer completed successfully */
			bp->b_resid = 0;
			ds->blkno += BTOBB(bp->b_bcount);

			/* did we get to the end of the tape? */
	   		if (iopb->pb.dsr & DS_EOT) {
				/* EOT is a warning */
				bp->b_flags |= B_ERROR;
				bp->b_error = ENXIO;
	   		}
		}

		/* For positioning update the blkno & lastblkno */
		if (iopb->pb.cmd == SPACE) {
			if (iopb->pb.control & C_REVERSE)
				ds->blkno -= (unsigned long) ((iopb->pb.count));
			else
				ds->blkno += (unsigned long) ((iopb->pb.count));
			if (ds->blkno > ds->lastblkno)
				ds->lastblkno = ds->blkno;
		}

		/* Searching to one end of current tape file. */
		if (iopb->pb.cmd == SFM)
			ds->blkno = (iopb->pb.control & C_REVERSE) ? ds->lastblkno : 0;

	   } else {				/* ERRORS!!!! */
		/* YES */

		TRACE("*iERR\n");

		if (bp) {
			if (err != SOFTTERR && err != SMALLER && err != HITFM) {
				bp->b_resid = bp->b_bcount;
				bp->b_flags |= B_ERROR;
				bp->b_error = EIO;
			} else if (err != SMALLER && err != HITFM) {
				/* soft error (corrected) this finished */
				ds->blkno += BTOBB(bp->b_bcount);
				bp->b_resid = 0;
			}
			/* Was it a tape read? */
			if (iopb->status & TREAD) {
				if (err == HITFM) {
					/* Easy one to recover from and quite common. */
					/* Didn't transfer any data. */
					bp->b_resid = bp->b_bcount;
					/* Tape position didn't change block numbers. */
					ds->blkno = bp->b_blkno;
				} else if (err == HITEOT) {
					bp->b_flags |= B_ERROR;
					bp->b_resid = bp->b_bcount;
					ds->blkno = bp->b_blkno;
					/* Just like HITFM, except that we now also know the last
					 * block number. */
					ds->lastblkno = ds->blkno;
				} else if (err == SMALLER) {
					/* Didn't get all the request, set b_resid */
					bp->b_resid = (bp->b_bcount - (unsigned long) (iopb->pb.transfer));
					ds->blkno += (unsigned long) BTOBB(iopb->pb.transfer);
				} else if (err == LARGER || err == SOFTTERR) {
					/* LARGER & SOFTTERR errors are ignored. */
					ds->blkno = bp->b_blkno + bp->b_bcount;
				} else {
					/* Don't know how to recover from other read errors. */
					bp->b_resid = bp->b_bcount;
					bp->b_flags |= B_ERROR;
					ttherror(iopb);
				}
			}
			/* Was it a tape write? */
			else if ((iopb->status & TWRITE)) {
				/* Try to handle some errors. */
				if (err == HITEOT) {
					/* HITEOT, so lastblkno must be whatever we were writing at. */
					ds->lastblkno = ds->blkno;
					bp->b_flags |= B_ERROR;
					bp->b_error = ENXIO;
					TRACE("ERROR EOT");
				} else if (err == PROTECTED) {
					/* This would have to be an extremely unusual case. */
					ds->dsr |= DS_FPT;
				} else if (err == SOFTTERR) {
					/* Don't do anything */
				} else {
					/* Don't know how to recover from other write errors. */
					bp->b_resid = bp->b_bcount;
					bp->b_flags |= B_ERROR;
					ttherror(iopb);
				}
			}
		}
		/* Is a particular location being seeked to? */
		if ((iopb->status & TSEEK)) {
			/* YES */
			TRACE("ERROR SEEK");
			if (err == HITFM || err == REVBOT || err == HITEOT) {
				/* Hopefully, the tape position will be either the first or last
				 * block in the file. */
				ds->blkno = (iopb->pb.control & C_REVERSE) ? 0 : ds->lastblkno;
			} else {
				/* Don't know how to recover from other seek errors. */
				ttherror(iopb);
			}
		}
	   }
	   /* If a buffer was associated with this request, unlink it (whether
	    * errors on not */
	   if (bp) {
		iounmap(bp);
		iodone(bp);
	   }

	   /* Turn off the CMD bit for commands and wakeup tthcmd(). */
	   if (iopb->status & CMD) {
		/* tthcmd must free the IOPB when it finishes copying it back out. */
		iopb->status &= ~CMD;
		wakeup(&iopb->status);
	   } else {
		/* We'll free other IOPB's right here. */
		iopb->status &= ~PBINUSE;
		dowanted = TRUE;
	   }
	   if (dowanted && (tthpblist[0]->status & PBWANTED)) {
		tthpblist[0]->status &= ~PBWANTED;
		wakeup(tthpblist[0]);
	   }

	   /* If there is an IOPB waiting to go start it.  */
	   if (hlist->head)
		tthexec(unit);
	   else
		tthaiopb = (struct IOPB *) NULL;

	}		/* end of controller for loop */
	TTHDEBUG(printf(" Interrupt Exit\n"););
	TRACE("i");
	return(found);	/* tell UNIX if interrupt was really us */
}

/*******************************************************************************
*									       *
*	Subroutine:	ttherror					       *
*									       *
*	Calling Sequence:	ttherror(iopb)				       *
*									       *
*	Called By:	tthintr						       *
*									       *
*	Calling Parameters:						       *
*		iopb - Pointer to IOPB in error...this is the one to print.    *
*									       *
*	Local Variables:						       *
*		pb - Register pointer to TTPB of iopb.			       *
*									       *
*	Calls Subroutines:	None.					       *
*									       *
*	Public/Global Variables:None.					       *
*									       *
*	Description:							       *
*		Just print out the TTPB...				       *
*									       *
*******************************************************************************/

static
ttherror(iopb)
register volatile struct IOPB *iopb;
{
	register volatile struct TTPB *pb;

	pb = &iopb->pb;
	TTHDEBUG(printf("pb@%x\n", pb);)

	printf("tt%d: cmd=%x, cntrl=%x, nor_int=%x:%x, err_int=%x:%x, count=%d, sdptr=%x:%x, pblink=%x:%x, gate=%x, errco=%x, xfer=%d, dsr=%x\n",
		pb->control & C_UNITSEL, pb->cmd, pb->control, pb->levela,
		pb->vectora, pb->levelb,
		pb->vectorb, (pb->count), pb->sdptram,
		(pb->sdptr), pb->lkptram, (pb->pblink), pb->gate, pb->errco, (pb->transfer), (pb->dsr));

}

/*******************************************************************************
*									       *
*	Subroutine:	tthminphys, tthread & tthwrite			       *
*									       *
*	Calling Sequence:	tthread(dev) or tthwrite(dev)		       *
*									       *
*	Called By:	UNIX I/O System...this provides the Raw interface.     *
*									       *
*	Calling Parameters:						       *
*		dev - Major & Minor of device to perform I/O on.	       *
*		uio - User's I/O request                                       *
*									       *
*	Local Variables:						       *
*		rtn - value returned from physio.  Only needed because of TRACE*
*									       *
*	Calls Subroutines:	tthminphys, tthstrategy (via physio)	       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - To set the RAWDEV bit in status.		       *
*		rttbuf - Buffers for RAW I/O.				       *
*									       *
*	Description:							       *
*		We provide the Raw I/O interface for UNIX...still, UNIX does   *
*		practically all of the work for us.  One of the things we do   *
*		is a sanity check on the drive unit.                           *
*									       *
*******************************************************************************/

#define MAX_TTH_BSIZE	(32 * 1024)	/* 32K maximum request to strategy */

tthminphys()
{

	/* limit RAWDEV transfers to MAX_TTH_SIZE */
	if (u.u_count > MAX_TTH_BSIZE)
		u.u_count = MAX_TTH_BSIZE;
}

tthread(dev)
dev_t dev;
{
	register rtn;

	TTHDEBUG(printf("\nRead Entry "););
	TRACE("<");

	if (UNIT(dev) >= NTTHU) {
		TRACE("*<");
		return(ENXIO);
	} else {
		tth_device[UNIT(dev)].status |= RAWDEV;
		tthminphys ();
		rtn = physio(tthstrategy, &rtthbuf[UNIT(dev)], dev, B_READ);
		TRACE("<-");
		return(rtn);
	}
}

tthwrite(dev, uio)
dev_t dev;
struct uio *uio;
{
	register rtn;

	TTHDEBUG(printf("\nWrite Entry "););
	TRACE(">");

	if (UNIT(dev) >= NTTHU) {
		TRACE("*>");
		return(ENXIO);
	} else {
		tth_device[UNIT(dev)].status |= RAWDEV;
		tthminphys ();
		rtn = physio(tthstrategy, &rtthbuf[UNIT(dev)], dev, B_WRITE);
		TRACE("->");
		return(rtn);
	}
}

/*******************************************************************************
*									       *
*	Subroutine:	tthioctl					       *
*									       *
*	Calling Sequence:	tthioctl(dev, cmd, addr, flag)		       *
*									       *
*	Called By:	UNIX I/O System					       *
*									       *
*	Calling Parameters:						       *
*		dev - Major & Minor of device to perform control on.	       *
*		cmd - Command for this routine to do.			       *
*		addr - Address of any arguments passed to this routine.	       *
*		flag - Same flag as given to tthopen.			       *
*									       *
*	Local Variables:						       *
*		unit - Drive unit...from dev.				       *
*		mtstat - Pointer to mtget struct for the MTIOCGET cmd.	       *
*		mtop - Pointer to mtop struct for the MTIOCTOP cmd.	       *
*		iopb - Pointer to pb...used for MTIOCTOP cmd.		       *
*		i - loop control.					       *
*		pb - TTPB to use for calling tthcmd.			       *
*	    NOTE: mtstat & mtop are both derived from addr.		       *
*									       *
*	Calls Subroutines:	tthcmd					       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - To get blkno that tape is positioned to.	       *
*									       *
*	Description:							       *
*		This routine provides the capability of doing operations on the*
*		tape drive that are out of the ordinary.  MTIOCGET returns the *
*		drive status; MTIOCTOP performs various functions of the tape. *
*		MTIOCGCON returns the controller configuration block.	       *
*		MTIOCSCON sets the controller configuration.		       *
*									       *
*******************************************************************************/

tthioctl(dev, cmd, data, flag)
dev_t dev;
caddr_t data;
{
	register short unit = UNIT(dev);
	register struct mtget *mtstat = (struct mtget *) data;
	register struct mtop *mtop = (struct mtop *) data;
	register struct TTPB *iopb;
	register int i;
	register word status;
	struct TTPB pb;

	TTHDEBUG(printf("\nIoctl Entry "););
	TRACE("IOCTL:\n");

#ifdef notdef
	/* You may want to remove this test if you want to allow normal users to do ioctl's. */
	if (!suser())
		return(EPERM);
#endif notdef

	/* Make sure we got a good address as an argument. */
	/* don't bother
	if (peek((short *)addr) == -1)
		return(EFAULT);
	*/

	/* Clear the garbage out before using */
	bzero (&pb, sizeof(pb));
	/* Point iopb to our TMTPB. */
	iopb = &pb;
	iopb->levela = TTH_LEVEL;
	iopb->vectora = TTH_VECTOR;
	iopb->levelb = TTH_LEVEL;
	iopb->vectorb = TTH_VECTOR;

	iopb->lkptram = TTH_ADDMOD;
	iopb->sdptram = TTH_ADDMOD;

	/* Do the various commands... */
	switch (cmd) {
		case MTIOCGET:
			TRACE("MTIOCGET");
			/* Someone somewhere wants status information,
			 * issue a status command to T3000.
			 * Set up IOPB for DSTATUS. */
			iopb->cmd = DSTATUS;
        		iopb->control = unit;
			iopb->sdptr = (NULL);
			do {
				tthcmd(unit, iopb);
				/*status = SWAB(iopb->dsr);*/
				status = (iopb->dsr);

				/* If no errors update our internal status value */
				if (iopb->errco == 0)
					tth_device[unit].dsr = status;
			} while (status & DS_FBY);

			/* Our identity. */
			mtstat->mt_type = MT_ISTM3000;
			/* The Drive status register for the unit. */
			mtstat->mt_dsreg = status;
			/* The Error code for the unit. */
			mtstat->mt_erreg = iopb->errco;
			/* Not implemented. */
			mtstat->mt_resid = 0;
			/* Not implemented. */
			mtstat->mt_fileno = 0;
			/* As close as we can tell... */
			mtstat->mt_blkno = tth_device[unit].blkno;
			TRACE(" ioctl\n");
			return(0);
#ifdef notdef
		case MTIOCGCON:
			TRACE("MTIOCGCON");
			*(struct TTH_CBLOCK *)data = tth_cblock.fmtr[unit/4];
			TRACE(" ioctl\n");
			return(0);
		case MTIOCSCON:
			tth_cblock.fmtr[unit/4] = *(struct TTH_CBLOCK *)data;
			return(0);
#endif notdef
		case MTIOCTOP:
			/* Execute a tape operation... */
			TRACE("MTIOCTOP");

			/* start setting it up. */
			iopb->control = unit;
			switch (mtop->mt_op) {
				case MTWEOF:
					iopb->cmd = WFM;

					/* For each Filemark that should be written, execute the command. */
					for (i = mtop->mt_count; i > 0; i--) {
						tthcmd(unit, iopb);
						/* But if one is in error, stop. */
						if (iopb->errco)
							break;
					}

					/* If there was an error, notify the caller. */
					u.u_error = iopb->errco ? EIO : 0;
					return;
					
				case MTBSF:
					iopb->control |= C_REVERSE;
					/* Fall thru... */
				case MTFSF:
					iopb->cmd = SFM;
					iopb->count = mtop->mt_count;
					break;
				case MTBSR:
					iopb->control |= C_REVERSE;
					/* Fall thru... */
				case MTFSR:
					iopb->cmd = SPACE;
					iopb->count = mtop->mt_count;
					break;
				case MTREW:
					iopb->cmd = REWIND;
					break;
				case MTOFFL:
					iopb->cmd = OFFLINE;
					break;
#ifdef notdef
				case MTERASE:
					iopb->cmd = SECURE;
					break;
#endif notdef
				case MTNOP:
					return(0);
				default:
					/* No such operation implemented. */
					u.u_error = EINVAL;
					return;
			};

			/* Execute the command that was just set up.  WFM's are
			 * executed above, not here. */
			tthcmd(unit, iopb);

			/* Tell the user about any errors. */
			TRACE(" ioctl\n");
			u.u_error = iopb->errco ? EIO : 0;
			return;

		default:
			/* No such command implemented. */
			TRACE(" No ioctl\n");
			u.u_error = EINVAL;
			return;
	};
}

/*******************************************************************************
*									       *
*	Subroutine:	getiopb						       *
*									       *
*	Calling Sequence:	getiopb()				       *
*									       *
*	Called By:	tthstrategy, tthcmd				       *
*									       *
*	Calling Parameters:	     					       *
*									       *
*	Local Variables:						       *
*		i - loop control.					       *
*		ps - temporary for processor status.			       *
*		lastalloc - static variable...index of last allocated IOPB     *
*									       *
*	Calls Subroutines:	None.					       *
*									       *
*	Public/Global Variables:					       *
*		tthpblist - List of IOPB's that controller uses.	       *
*									       *
*	Description:							       *
*		This routine searches tthpblist for a free IOPB in a sequent.  *
*		fashion.  When it finds one, it marks it as being in use &     *
*		returns its address.  When it doesn't find one...it marks the  *
*		PBWANTED bit in the first IOPB's status and goes to sleep,     *
*		waiting for a free IOPB.  				       *
*									       *
*******************************************************************************/

static struct IOPB *
getiopb()
{
	register int i;
	register ps;
	static int lastalloc = 0;

	TTHDEBUG(printf("\nGetiopb Entry"););
	TRACE("PB");

	/* Up the processor priority */
	ps = spltty();

findone:
	/* If the PBWANTED bit is set, all IOPB's are in use, so go to sleep. */
	while (tthpblist[0]->status & PBWANTED)
		sleep(tthpblist[0], PRIBIO);

	/* Start looking at the last IOPB allocated plus one. */
	i = lastalloc + 1;
	/* Repeat this loop until we have cycled through all the IOPB's */
	do {
		/* Wrap the index variable if it is too big. */
		if (i >= niopb)
			i = 0;

		/* Has the IOPB been allocated? */
		if (tthpblist[i] == 0) {		/* Try to allocate another IOPB */
			if ((tthpblist[i] = (struct IOPB *)allc()) == 0) {
				/* Could not allocate it... make a note. */
				printf("Could not allocate all IOPB's for Tapemaster 3000.\nEffective number available changed to %d\n", i);
				/* Drop niopb down to what we currently have available. */
				niopb = i;
				/* And restart the index at the first IOPB. */
				i = 0;
			} else {
				/* For a successful allocation, be sure to zero the status field. */
				tthpblist[i]->status = 0;
			}

			TTHDEBUG(printf("tthpblist[i] = %x <<<<<<\n", tthpblist[i]);)

		}

		/* Is this IOPB being used? */
		if ((tthpblist[i]->status & PBINUSE) == 0) {
			/* NO */
			/* We can use it, so mark it as in use. */
			tthpblist[i]->status |= PBINUSE;
			/* Update lastalloc */
			lastalloc = i;
			/* This IOPB is nothing right now. */
			tthpblist[i]->status &= ~(TSEEK | TWRITE | TREAD);
			tthpblist[i]->next = (struct IOPB *) NULL;
			tthpblist[i]->bp = (struct buf *) NULL;
			/* Restore the priority */
			splx(ps);
			TTHDEBUG(printf(" Getiopb Exit\n"););
			TRACE("pb");
			return(tthpblist[i]);		/* ONLY WAY OUT */
		}
	/* Our index (i) gets incremented here. */
	} while (i++ != lastalloc);

	/* We have cycled through all the IOPB's and didn't find one that was free. */
	tthpblist[0]->status |= PBWANTED;
	goto findone;
	/*NOTREACHED*/
}

/*******************************************************************************
*									       *
*	Subroutine:	tthqueue					       *
*									       *
*	Calling Sequence:	tthqueue(iopb)				       *
*									       *
*	Called By:	tthstrategy, tthcmd				       *
*									       *
*	Calling Parameters:						       *
*		iopb - IOPB to queue.					       *
*									       *
*	Local Variables:						       *
*		unit - Drive unit...from iopb.				       *
*									       *
*	Calls Subroutines:	tthexec					       *
*									       *
*	Public/Global Variables:					       *
*		lblkno - Position of tape in Buffered mode (vs. Raw) after all *
*			operations currently queued have finished.	       *
*		tth_device - Drivers local drive information.		       *
*		tthaiopb - Head of IOPB chain controller is currently working  *
*			on.						       *
*		tthclist - Head of IOPB chain we are queueing things to.       *
*		tthcend - Other end of IOPB chain we are queuing things to.    *
*									       *
*	Description:							       *
*		This is a rather simple routine...First thing to do is update  *
*		lblkno & maybe lastblkno.  Then, if the controller is idle,    *
*		start it working on iopb without queuing anything.  If the     *
*		controller is busy, then we do our real work.  Linking iopb    *
*		into ttclist.						       *
*									       *
*******************************************************************************/

static
tthqueue(iopb)
register struct IOPB *iopb;
{
	register int unit;

	TTHDEBUG(printf("\nQueue Entry"););
	TRACE("Q");

	/* The incoming IOPB does not have IOPB's already linked to it. */
	iopb->next = (struct IOPB *) NULL;

	/* Get the unit the IOPB is for. */
	unit = iopb->pb.control & C_UNITSEL;

	/* If there is nothing on the head, just put it there. */
	if (holding[unit].head == (struct IOPB *) NULL) {
		holding[unit].head = iopb;
		holding[unit].tail = iopb;
	} else {
		/* Otherwise, add iopb onto the tail. */
		holding[unit].tail->next = iopb;
		holding[unit].tail = iopb;
	}

	/* If this is a data transfer update the lblkno with the new end. */
	if (iopb->bp != (struct buf *) NULL) {
		/* Update lblkno with the new expected position. */
		lblkno[unit] = iopb->end;

		/* If it is past the present last block number on the tape, update the new lastblkno */
		if (lblkno[unit] > tth_device[unit].lastblkno)
			tth_device[unit].lastblkno = lblkno[unit];
	}

	/* if there is more than one IOPB on the holding list,  */
	/* and the controller is idle, and the head is not a RBWRITE, start the head. */
	if (tthaiopb == (struct IOPB *) NULL) {
		tthexec(unit);
		TTHDEBUG(printf(" Queue Exit 1\n"););
		TRACE("q1");
		return(0);
	}

	TTHDEBUG(printf(" Queue Exit 2\n"););
	TRACE("q2");
}

/*******************************************************************************
*									       *
*	Subroutine:	tthexec						       *
*									       *
*	Calling Sequence:	tthexec(unit)				       *
*									       *
*	Called By:	tthqueue					       *
*									       *
*	Calling Parameters:						       *
*		unit - Unit to start execution on.			       *
*									       *
*	Local Variables:						       *
*		pb - another representation for iopb.			       *
*		dummy - Dummy variable to read the channel attention data into.*
*									       *
*	Calls Subroutines:	None.					       *
*									       *
*	Public/Global Variables:					       *
*		tthaiopb - This is where ttintr should start processing when  *
*			called.						       *
*									       *
*	Description:							       *
*		This routine sends out the IOPB's address & AM and then kicks  *
*		off the controller.					       *
*									       *
*******************************************************************************/

static
tthexec(unit)
register int unit;
{
	register struct LIST *hlist;
	register struct IOPB *iopb;
	register unsigned long pb;
	register volatile struct TTREG *reg;
	register volatile int dummy;

	TTHDEBUG(printf("\nExec Entry"););
	TRACE("X");


	/* We are going to output the address of iopb to the controller, so the
	 * next interrupt will be caused by an IOPB in the iopb chain.  Set
	 * tthaiopb to point to the start. */
	hlist = &holding[unit];
	tthaiopb = (iopb = hlist->head);

	/* Make the address available as a long so we can shift bits. */
	pb = K1_TO_PHYS((unsigned long) &iopb->pb);

	TTHDEBUG(
		printf("reg->addr0   = %x", ((word) pb));
		printf("\treg->addr2   = %x", ((word) (pb >> 16)));
		printf("\treg->addr4   = %x\n", SWAB((word) (IOPBAM)));

		printf("iopb cmd     = %x", iopb->pb.cmd);
		printf("\tiopb control = %x", iopb->pb.control);
		printf("\tiopb count   = %x\n", iopb->pb.count);

		printf("iopb sdptr   = %x", iopb->pb.sdptr);
		printf("\tiopb sdptram = %x\n", iopb->pb.sdptram);

		printf("iopb pblink  = %x", iopb->pb.pblink);
		printf("\tiopb lkptram = %x\n", iopb->pb.lkptram);

		printf("iopb vectora = %x(%x)", iopb->pb.vectora,
						iopb->pb.levela);
		printf("\tiopb vectorb = %x(%x)\n", iopb->pb.vectorb,
						iopb->pb.levelb);
	)

	/* Point reg at correct controller */
	reg = (struct TTREG *) tthstd[0];

	/* Put out the IOPB's address. */
	reg->addr2 = ((word) (pb >> 16));
	wbflush();
	reg->addr0 = ((word) (pb));
	wbflush();
	reg->addr4 = SWAB((word) (IOPBAM));
	wbflush();

	/* flush the cache so that the program can read updated data */
	flush_cache();

	/* Start the controller. */
	dummy = reg->attention;

	hlist->head = iopb->next;

	if (hlist->head == (struct IOPB *) NULL) {
		hlist->tail = (struct IOPB *) NULL;
	}
	TTHDEBUG(printf(" Exec Exit\n"););
	TRACE("x");
}

/*******************************************************************************
*									       *
*	Subroutine:	position					       *
*									       *
*	Calling Sequence:	position(unit, dest)			       *
*									       *
*	Called By:	tthclose, tthstrategy				       *
*									       *
*	Calling Parameters:						       *
*		unit - Drive unit to position.				       *
*		dest - Destination block # we want to position at.	       *
*									       *
*	Local Variables:						       *
*		iopb - Pointer to pb to use for SPACE cmd.		       *
*		cur - Current block # the unit is positioned to.	       *
*		pb - TTPB to use for commands.				       *
*									       *
*	Calls Subroutines:	tthcmd					       *
*									       *
*	Public/Global Variables:					       *
*		tth_device - Drivers local drive information.		       *
*		lblkno - Position of tape after all queued operations have     *
*			finished (in buffered mode)			       *
*									       *
*	Description:							       *
*		There are two ways to position a tape drive...the more	       *
*		accurate method of after operations are complete, so that we   *
*		can see if they were successful or not; and that of expecting  *
*		everything to be ok.  The raw interface uses the former, while *
*		the buffered mode uses the latter.  This routine first checks  *
*		to see if the tape will be in the position we want, if so, it  *
*		does nothing.  Otherwise, we space the desired number of       *
*		records...obviously this only works for block size records, but*
*		we try anyway.  Send off the SPACE command.		       *
*		Then, since the TM3000 will finish a command before	       *
*		the tape is ready (in an effort to keep streaming); wait for   *
*		the formatter to become ready.  Note that this last operation  *
*		only takes place if we moved the tape backwards.	       *
*									       *
*******************************************************************************/

void static
position(unit, dest)
register int unit;
register daddr_t dest;
{
	register struct TTPB *iopb;
	register daddr_t cur;
	struct TTPB pb;
	register word status;

	TTHDEBUG(printf("\nPosition Entry"););
	TRACE("PO");

	/* There are two ways of positioning, use the correct blkno for Raw or Block devices. */
	if (tth_device[unit].status & RAWDEV)
		cur = tth_device[unit].blkno;	/* Raw device value */
	else
		cur = lblkno[unit];		/* Block device value */

	/* If the tape is already there... */
	if (dest == cur) {
		TTHDEBUG(printf(" Position Exit (tape is there)\n");)
		TRACE("po1");
		return;
	}

	/* Clear the garbage out before using */
	bzero (&pb, sizeof(pb));
	/* Point iopb at our TTPB. */
	iopb = &pb;
	iopb->control = unit;
	iopb->levela = TTH_LEVEL;
	iopb->vectora = TTH_VECTOR;
	iopb->levelb = TTH_LEVEL;
	iopb->vectorb = TTH_VECTOR;

	TTHDEBUG(printf(" dest %x cur %x ", dest, cur);)

	/* If we are going to SPACE backwards, wait until the formatter is ready. */
	if (dest < cur) {

		   /* Set up the IOPB */
		   iopb->cmd = DSTATUS;
		   iopb->sdptr = (NULL);

		   /* Repeat DSTATUS'ing until the formatter is ready. */
		   do {
			tthcmd(unit, iopb);
			/*status = SWAB(iopb->dsr);*/
			status = (iopb->dsr);

			/* If no errors update our internal status value */
			if (iopb->errco == 0)
				tth_device[unit].dsr = status;
		   } while (status & DS_FBY);
	}

	/* Set up the IOPB for the SPACE command. */
	iopb->cmd = SPACE;

	if (cur < dest) {
		iopb->count = dest - cur;
		TTHDEBUG(printf(">%d>", (iopb->count));)
	} else {
		/* If going backwards, we'll need to set C_REVERSE. */
		iopb->control |= C_REVERSE;
		iopb->count = cur - dest;
		TTHDEBUG(printf("<%d<", (iopb->count)););
	}

	/* Execute the SPACE command. */
	tthcmd(unit, iopb);

	/* If we went backwards, wait until the formatter is ready. 
	 * This assumes the next command will be something that moves forwards. */
	if (iopb->control & C_REVERSE) {

		/* Set up IOPB for DSTATUS again. */
		iopb->cmd = DSTATUS;
        	iopb->control = unit;
		iopb->sdptr = (NULL);
		do {
			tthcmd(unit, iopb);
			status = (iopb->dsr);

			/* If no errors update our internal status value */
			if (iopb->errco == 0)
				tth_device[unit].dsr = status;
		} while (status & DS_FBY);
	}

	TTHDEBUG(printf(" Position Exit\n"););
	TRACE("po2");
	return;
}


/*
 *  Instead of 'rmalloc'ing space when needed for IOPBs, reserve space now and
 *  simulate alloc and free calls.
 */

static struct IOPB iopbs[NIOPB+1];
static int used[NIOPB+1];

static ulong
allc() {
	register int i;

	for (i=0;i<NIOPB+1;i++) {
		if (!used[i]) {
			used[i]++;
			return ((ulong)(K0_TO_K1(&iopbs[i])));
		}
	}
	return((ulong)0);
}

static
free(addr) 
ulong	addr;
{
	register int i;

	for (i=0;i<NIOPB+1;i++) {
		if (addr == (ulong)K0_TO_K1(&(iopbs[i]))) {
			used[i] = 0;
			return;
		}
	}
	printf("tth: can't free buffer/n");
}

tthprint (dev,str)
	dev_t dev;
	char * str;
{
	cmn_err (CE_NOTE,"tth: dev %d, %s",dev, str);
}

