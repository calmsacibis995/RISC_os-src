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
#ident	"$Header: ts.c,v 1.25.3.2 90/05/10 05:36:50 wje Exp $"

/*
 * $Header: ts.c,v 1.25.3.2 90/05/10 05:36:50 wje Exp $
 */
/*
 * Driver for the ISI VME-QIC2/X cartridge tape controller.
 *
 * Written by: Kipp Hickman
 *
 * TODO:
 *	o test write off end of medium
 *	o test retension
 *	o test mtops:
 *		x rewind
 *		o fsf (normal, past eot)
 *		o fsr (normal, past eot)
 *		o write filemark (normal, past eot)
 *
 */
#include "sys/debug.h"
#include "sys/sysmacros.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/edt.h"
#include "sys/cmn_err.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/dir.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/file.h"
#include "sys/mtio.h"
#include "sys/tsreg.h"
#include "sys/dump.h"

#define	TRUE	(1==1)
#define	FALSE	(1==0)

/*
 * Minor device macros.
 */
#define	CTLR(dev)		((dev) & 0x03)
#define	NOREWIND(dev)		((dev) & 0x04)
#define Q24DEV(dev)		((dev) & 0x08)	/* qic-24 mode (Archive only) */
#define Q11DEV(dev)		((dev) & 0x10)	/* qic-11 mode (Archive only) */
#define	SWAPDEV(dev)		((dev) & 0x20)
#define RAWDEV(dev)		((dev) & 0x40)
#define RAWSHFT			6 /* take rawdev bit and make it lsb */
#define Q24SHFT			3 /* take q24dev bit and make it lsb */
#define Q11SHFT			4 /* take q11dev but and make it lsb */
#define	ISREWIND(dev)		(NOREWIND(dev) == 0)

#define	HI16(x)		(((ulong) (x)) >> 16)
#define	LO16(x)		((ushort) (x))

#define	CMD(sc) \
	((volatile struct tscmd *) K0_TO_K1((long) &(sc)->sc_cmd))
#define	CHBUF(sc) \
	((volatile struct tschar *) K0_TO_K1((long) &(sc)->sc_char))
#define	STATUS(sc) \
	((volatile struct tsstatus *) K0_TO_K1((long) &(sc)->sc_status))

#ifndef	MIN
#define	MIN(a, b)	(((a) < (b)) ? (a) : (b))
#endif

/*
 * Time constants.  These were measured for a 450 foot tape, then fudged
 * up, then normalized for a 650 foot tape.  Thus, they should be plenty
 * long enough.  The units are seconds.
 */
#define	TIME_FSF	(60 * 17)		/* no more than 17 minutes */
#define	TIME_REWIND	(60 * 2)		/* no more than 2 minutes */
#define	TIME_RDWR	(60 * 2)		/* no more than 2 minutes */
#define	TIME_RESET	(TIME_FSF + TIME_REWIND)
#define	TIME_WAIT	(60 * 3)

/*
 * Patterns used to fill the status buffer when checking for bad dma
 * from the tape controller.  We suspect that the controller some times
 * drops the message packet in a random location.  We store a pattern
 * in the message buffer before a command is issued, then check it when
 * the command completes.  In **all** cases the pattern should be overwritten.
 * If its not, then we panic assuming that system memory has been comprimized.
 */
#define	PAT0	0xFFFF
#define	PAT1	0xFFFF
#define	PAT2	0xFFFF
#define	PAT3	0xFFFF
#define	PAT4	0xFFFF

/*
 * Initialize the tape controller refered to by "e".  First probe it and
 * if exists, reset it.
 */
void
tsedtinit(e)
	register struct edt *e;
{
	register struct tssoftc *sc;
	volatile struct tsdevice *ts;
	int ctlr;

	/* probe device */
	ctlr = e->e_intr_info->v_unit;
	ts = (volatile struct tsdevice *) e->e_base;
	if (badaddr(&ts->tssr, sizeof(ts->tssr))) {
		printf("ts%d: missing\n", ctlr);
		return;
	}

	/* initialize software state - reset controller later */
	sc = &tssoftc[ctlr];
	sc->sc_flags = SC_PROBED;
	sc->sc_ctlr = ctlr;
	sc->sc_device = ts;
}

/*
 * Return a string describing the command in CMD(sc)->cmd
 */
static char *
tscname(sc)
	register struct tssoftc *sc;
{
	register volatile struct tscmd *cmd;

	cmd = CMD(sc);
	switch (cmd->cmd & TSCMD_CMD) {
	  case TSCMD_READ:
		return "read";
	  case TSCMD_WRITECHAR:
		return "write characteristics";
	  case TSCMD_WRITE:
		return "write";
	  case TSCMD_FSR:
		return "forward space record";
	  case TSCMD_FSF:
		return "forward space file";
	  case TSCMD_REWIND:
		return "rewind";
	  case TSCMD_WFMK:
		return "write file mark";
	  case TSCMD_SENSE:
		return "get status";
	  default:
		return "unknown";
	}
}

/*
 * Return a string describing the termination code
 */
static char *
tsmsg(sc)
	register struct tssoftc *sc;
{
	volatile struct tsdevice *ts;

	ts = sc->sc_device;
	switch (ts->tssr & TSSR_TC) {
	  case TSTC_OK:
		return "succesful";
	  case TSTC_ATTN:
		return "attention";
	  case TSTC_ALERT:
		return "tape status alert";
	  case TSTC_REJECT:
		return "function reject";
	  case TSTC_UNREC0:
	  case TSTC_UNREC1:
		return "unrecoverable";
	  case TSTC_UNREC2:
		return "write past physical EOT";
	  case TSTC_FATAL:
		return "fatal";
	  default:
		return "unknown";
	}
}

/*
 * Print a pretty error message describing a tape error
 */
static void
tserr(sc)
	register struct tssoftc *sc;
{
	register volatile struct tsstatus *status;
	register volatile struct tsdevice *ts;

	ts = sc->sc_device;
	status = STATUS(sc);
#ifdef	DEBUG
	cmn_err(CE_CONT, "ts%d: %s failed, sr=%x xs0=%x xs1=%x xs3=%x\n",
			 sc->sc_ctlr, tscname(sc), ts->tssr,
			 status->xs0, status->xs1, status->xs3);
{
	register volatile struct tscmd *cmd;
	cmd = CMD(sc);
	cmn_err(CE_CONT, "ts%d: cmd pkt (%x): %x %x %x %x\n", sc->sc_ctlr,
			 cmd, cmd->cmd, cmd->balo, cmd->bahi, cmd->bytecount);
}
{
	register volatile struct tschar *chbuf;
	chbuf = CHBUF(sc);
	cmn_err(CE_CONT, "ts%d: char pkt (%x): %x %x %x %x\n",
			 sc->sc_ctlr, chbuf, chbuf->balo, chbuf->bahi,
			 chbuf->bytecount, chbuf->mode);
}
	cmn_err(CE_CONT, "ts%d: msg pkt (%x): %x %x %x %x %x %x %x\n",
			 sc->sc_ctlr, status, status->header, status->len,
			 status->resid, status->xs0, status->xs1,
			 status->xs2, status->xs3);
#else
	cmn_err(CE_CONT, "ts%d: %s failed (sr=%x)\n",
			 sc->sc_ctlr, tscname(sc), ts->tssr);
#endif
	if (ts->tssr & TSSR_SC) {
		if (ts->tssr & TSSR_TC)
			cmn_err(CE_CONT, "ts%d: %s error\n",
					 sc->sc_ctlr, tsmsg(sc));
		else
		if (ts->tssr & TSSR_RMR)
			cmn_err(CE_CONT, "ts%d: register mod refused\n",
					 sc->sc_ctlr);
		else
			cmn_err(CE_CONT, "ts%d: strange error\n", sc->sc_ctlr);
	}
	if (ts->tssr & TSSR_NBA)
		return;

	/* look at extended status */
	if (status->xs0 & (XS0_WLE|XS0_NEF|XS0_ILC|XS0_ILA)) {
		cmn_err(CE_CONT, "ts%d: extended status: ", sc->sc_ctlr);
		if (status->xs0 & XS0_WLE)
			cmn_err(CE_CONT, "<write lock error> ");
		if (status->xs0 & XS0_NEF)
			cmn_err(CE_CONT, "<nonexecutable function>");
		if (status->xs0 & XS0_ILC)
			cmn_err(CE_CONT, "<illegal command>");
		if (status->xs0 & XS0_ILA)
			cmn_err(CE_CONT, "<illegal address>");
		cmn_err(CE_CONT, "\n");
	}
	if (status->xs1 & XS1_UNC)
		cmn_err(CE_CONT, "ts%d: hard error\n", sc->sc_ctlr);
}

/*
 * Sleep for a bit
 */
static void
tswait(cansleep)
	int cansleep;
{
	if (cansleep)
		delay(HZ/2);
	else {
		DELAY(500000);
	}
}

/*
 * Start a command on the controller, then poll it for a command completion.
 * Return TRUE if the command completes without error, FALSE otherwise.
 */
static int
tspoll(sc, cansleep)
	register struct tssoftc *sc;
	int cansleep;
{
	register volatile struct tsdevice *ts;
	long physaddr;
	long attempts;

	ts = sc->sc_device;
	physaddr = K0_TO_PHYS((long) &sc->sc_cmd);
	ts->txcpr = HI16(physaddr);
	wbflush();
	ts->tcpr = LO16(physaddr);			/* go */
	wbflush();

	attempts = TIME_FSF*2;
	while (--attempts > 0) {
		tswait(cansleep);
		if (ts->tssr & TSSR_SSR)
			break;
		if (ts->tssr & (TSSR_RMR|TSSR_NXM)) {
			tserr(sc);
			return (FALSE);
		}
	}
	if (attempts == 0) {
		volatile struct tscmd *cmd;

		cmd = CMD(sc);
		cmn_err(CE_CONT, "ts%d: timeout during \"%s\" command (%x)\n",
				 sc->sc_ctlr, tscname(sc), cmd->cmd);
		return (FALSE);
	}

	/* check for special condition (== error) */
	if (ts->tssr & TSSR_SC) {
		tserr(sc);
		return (FALSE);
	}
	return (TRUE);
}

/*
 * Reset the given controller.  If the reset fails, return FALSE, otherwise
 * return TRUE.
 */
static int
tsreset(sc, cansleep, dev)
	register struct tssoftc *sc;
	int cansleep;
{
	register volatile struct tsdevice *ts;
	register volatile struct tscmd *cmd;
	register volatile struct tschar *chbuf;
	long attempts;
	long physaddr;

	/*
	 * Probe controller address to see if controller is still
	 * answering bus cycles.  If its not, then its **really** busted.
	 * This is just trying to dodge a kernel panic.
	 */
	ts = sc->sc_device;
	if (badaddr(&ts->tssr, sizeof(ts->tssr))) {
		cmn_err(CE_CONT, "ts%d: controller is hung\n", sc->sc_ctlr);
		return (FALSE);
	}

	ts->txcpr = TXCPR_RESET;
	wbflush();
	/*
	 * Wait for reset to complete.  Delay a bit and then check the
	 * TSSR_SSR bit.  When the initialize completes, the bit will
	 * be set by the controller.  This may take a while if there
	 * is a tape in the drive.
	 */
	attempts = TIME_RESET * 2;
	while (--attempts > 0) {
		tswait(cansleep);
		if (ts->tssr & TSSR_SSR)
			break;
	}
	if (attempts == 0) {
		cmn_err(CE_CONT, "ts%d: controller hung during reset\n",
				 sc->sc_ctlr);
		return (FALSE);
	}

	/*
	 * Now inform the controller of the location of the status buffer.
	 * Use the write characteristics command.
	 */

	/* fill in command buffer */
	cmd = CMD(sc);
	cmd->cmd = TSCMD_ACK | TSCMD_MUSTBEONE | TSCMD_WRITECHAR;
	physaddr = K0_TO_PHYS((long) &sc->sc_char);
	cmd->balo = LO16(physaddr);
	cmd->bahi = HI16(physaddr);
	cmd->bytecount = sizeof(struct tschar);

	/* fill in characteristics buffer */
	chbuf = CHBUF(sc);
	physaddr = K0_TO_PHYS((long) &sc->sc_status);
	chbuf->balo = LO16(physaddr);
	chbuf->bahi = HI16(physaddr);
	chbuf->bytecount = sizeof(struct tsstatus);
	if (RAWDEV(dev)) {
	    sc->sc_flags |= SC_FORMAT;
	    chbuf->mode = TSCHAR_ESS | TSCHAR_RAW;
	}
	else {
	    sc->sc_flags &= ~SC_FORMAT;
	    chbuf->mode = TSCHAR_ESS;
	}

	if (Q11DEV(dev)) {
		sc->sc_flags &= ~SC_Q24;
		sc->sc_flags |= SC_Q11;
		chbuf->mode |= ( TSCHAR_Q11 | TSCHAR_FMT );
	} else if (Q24DEV(dev)) {
		sc->sc_flags &= ~SC_Q11;
		sc->sc_flags |= SC_Q24;
		chbuf->mode |= TSCHAR_FMT;
	}

	/* clear out status buffer */
	bzero((char *) STATUS(sc), sizeof(struct tsstatus));

	/* start command and poll for completion */
	if (!tspoll(sc, cansleep)) {
		cmn_err(CE_CONT, "ts%d: controller hung during setup\n",
				 sc->sc_ctlr);
		return (FALSE);
	}
	sc->sc_flags &= ~(SC_OPEN);
	sc->sc_flags |= SC_PROBED | SC_INITIALIZED;
	return (TRUE);
}

/*
 * Retension the tape in the tape drive.  Since this operation resets
 * the controller, we have to spin on the TSSR_SSR bit instead of
 * expecting an interrupt.  Use a timeout to do the polling.
 * Provide dev number to tsreset so that it may determine if it should
 * set the RAW bit or not.
 */
static int
tsretension(sc, dev)
	struct tssoftc *sc;
{
	register volatile struct tsdevice *ts;
	register volatile struct tscmd *cmd;
	long physaddr;
	long attempts;

	cmd = CMD(sc);
	cmd->cmd = TSCMD_ACK | TSCMD_MUSTBEONE | TSCMD_RESET;
	cmd->balo = 0;
	cmd->bahi = 0;
	cmd->bytecount = 0;

	/* issue command */
	ts = sc->sc_device;
	physaddr = K0_TO_PHYS((long) &sc->sc_cmd);
	ts->txcpr = HI16(physaddr);
	wbflush();
	ts->tcpr = LO16(physaddr);		/* go */
	wbflush();

	attempts = (TIME_FSF + TIME_REWIND)*2;	/* give it lots of time */
	while (--attempts > 0) {
		delay(HZ/2);
		if (ts->tssr & TSSR_SSR)
			break;
	}
	if (attempts == 0) {
		cmn_err(CE_CONT, "ts%d: controller hung during retension\n",
				 sc->sc_ctlr);
		sc->sc_flags |= SC_BUSTED;
		return EIO;
	}

	/* now really reset the controller so as to be able to use it again */
	if (!tsreset(sc, 1, dev)) {
		sc->sc_flags |= SC_BUSTED;
		return EIO;
	}
	return 0;
}

/*
 * Fill in the command block and then point the registers at it, starting
 * the controller.  This assumes its called at spltape()
 */
static void
tsfill(sc, op, buf, bcount)
	register struct tssoftc *sc;
	int op;
	caddr_t buf;
	int bcount;
{
	register volatile struct tscmd *cmd;
	register volatile struct tsdevice *ts;
	register volatile struct tsstatus *status;
	long physaddr;

	/*
	 * Because we don't trust the tape controller, put a known pattern
	 * into the message buffer.  We check for that pattern when an
	 * interrupt occurs, to insure that the message was delivered to
	 * the correct address.
	 */
	status = STATUS(sc);
	status->resid = PAT0;
	status->xs0 = PAT1;
	status->xs1 = PAT2;
	status->xs2 = PAT3;
	status->xs3 = PAT4;

	switch (op) {
	  case TSCMD_READ:
	  case TSCMD_WRITE:
		ASSERT(buf == (caddr_t)K0_TO_PHYS(&tsbuf[sc->sc_ctlr][0]));
		break;
	  case TSCMD_FSF:
	  case TSCMD_FSR:
		ASSERT(((long)buf & 0xFFFF0000) == 0);
		break;
	  default:
		ASSERT(buf == 0);
		break;
	}

	/* fill in command */
	cmd = CMD(sc);
	cmd->cmd = TSCMD_ACK | TSCMD_MUSTBEONE | TSCMD_IE | op;
	cmd->balo = LO16(buf);
	cmd->bahi = HI16(buf);
	cmd->bytecount = bcount;

	/* start command */
	ts = sc->sc_device;
	physaddr = K0_TO_PHYS((long) &sc->sc_cmd);
	ts->txcpr = HI16(physaddr);
	wbflush();
	ts->tcpr = LO16(physaddr);		/* go */
	wbflush();

	/* update software state */
	sc->sc_flags &= ~SC_ERROR;
	sc->sc_flags |= SC_BUSY;
}

/*
 * Handle a controller timeout.  Tag current command with an error, and
 * wakeup anybody sleeping.
 */
static int
tstimeout(sc)
	register struct tssoftc *sc;
{
	cmn_err(CE_WARN, "ts%d: controller timeout during %s command\n",
			 sc->sc_ctlr, tscname(sc));
	sc->sc_flags |= SC_ERROR | SC_BUSTED;
	sc->sc_flags &=
		~(SC_BUSY | SC_WAITING | SC_REWINDING);
	wakeup((caddr_t) sc);
}

/*
 * Issue a command to the controller.  Wait for completion, and then return
 * TRUE if the command works, FALSE if it fails.
 */
static int
tscmd(sc, op, buf, bcount, wait)
	register struct tssoftc *sc;
	int op;
	caddr_t buf;
	int bcount;
	int wait;
{
	register volatile struct tsdevice *ts;
	int attempts;
	int s;
	int tv;

	/*
	 * Insure that controller is ready for a new command.  Under normal
	 * operation this should never happen.
	 */
	ts = sc->sc_device;
	attempts = TIME_WAIT * 4;
	while (!(ts->tssr & TSSR_SSR) && (--attempts > 0)) {
		/* sleep for approximately 1/4 second */
		delay(HZ/4);
	}
	if (attempts == 0) {
		printf("ts%d: hung waiting for controller\n", sc->sc_ctlr);
		/* timed out waiting for controller */
		sc->sc_flags |= SC_BUSTED;
		return (FALSE);
	}

	/*
	 * Start command going.  Wait for completion, if told to.
	 */
	s = splclock();
	tsfill(sc, op, buf, bcount);
	if (wait) {
		/* setup a timeout in case controller hangs */
		switch (op) {
		  case TSCMD_REWIND:
			tv = HZ * TIME_REWIND;
			break;
		  case TSCMD_READ:
		  case TSCMD_WRITE:
			/*
			 * Normal read/write commands take almost zero time.
			 * However, if they are operating on a bad spot in
			 * the tape, they can take a long time.  This
			 * time is unmeasured, thus it is a guess.
			 */
			tv = HZ * TIME_RDWR;
			break;
		  case TSCMD_FSF:
			tv = HZ * TIME_FSF;
			break;
		  default:
			tv = HZ * (TIME_FSF + TIME_RDWR);
			break;
		}
		sc->sc_id = timeout(tstimeout, sc, tv);

		/* wait for command completion */
		while (sc->sc_flags & SC_BUSY) {
			sc->sc_flags |= SC_WAITING;
			(void) sleep((caddr_t) sc, PRIBIO);
		}

		/* examine command status */
		if (sc->sc_flags & SC_ERROR) {
			splx(s);
			return (FALSE);
		}
	}
	splx(s);
	return (TRUE);
}

/*
 * Issue an operation to the controller.  Return EIO if it fails.
 */
static int
tsop(sc, op, repeat, wait)
	register struct tssoftc *sc;
	int op, repeat, wait;
{
	int amount;
	int rv;

	/*
	 * Force controller to be reset by failing commands
	 * until the device is opened again.
	 */
	if (sc->sc_flags & SC_BUSTED)
		return EIO;

	ASSERT((repeat > 1) ? (wait == 1) : 1);
	while (repeat) {
		switch (op) {
		  case TSCMD_FSF:
		  case TSCMD_FSR:
			/*
			 * For these commands, the command packet is different.
			 * The first word following the command contains the
			 * number of things to skip (files/records).  Stuff
			 * the repeat count into the low 16 bits of the
			 * buffer address argument to tscmd.
			 */
			amount = repeat;
			if (amount > 16384)
				amount = 16384;
			rv = tscmd(sc, op, (caddr_t) repeat, 0, wait);
			repeat -= amount;
			break;
		  default:
			rv = tscmd(sc, op, (caddr_t) 0, 0, wait);
			--repeat;
			break;
		}
		if (!rv) {
			cmn_err(CE_CONT, "ts%d: %s command failed\n",
					 sc->sc_ctlr, tscname(sc));
			return EIO;
		}
	}
	return 0;
}

/*
 * Using dev, open the given tape drive.  For rewinding drives, move tape
 * to BOT.
 */
void
tsopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tssoftc *sc;
	register volatile struct tsstatus *status;
	register volatile struct tsdevice *ts;
	int s;
	/* validate unit and check that controller probed */
	if (CTLR(dev) >= tsctlrs) {
		u.u_error = ENXIO;
		return;
	}
	sc = &tssoftc[CTLR(dev)];
	if ((sc->sc_flags & SC_PROBED) == 0) {
		u.u_error = ENXIO;
		return;
	}
	if (sc->sc_flags & SC_OPEN) {
		/* sorry, only one user at a time */
		u.u_error = EBUSY;
		return;
	}
	ts = sc->sc_device;

	/* initialize drive if necessary */
again:
	s = splclock();
	sc->sc_flags |= SC_OPEN;
	splx(s);
	if (((sc->sc_flags & SC_INITIALIZED) == 0) ||
	    (sc->sc_flags & SC_BUSTED) ||
	    (((sc->sc_flags & SC_FORMAT) >> SC_FORMAT_SHFT)
	     != (RAWDEV(dev) >> RAWSHFT)) ||
	    (((sc->sc_flags & SC_Q24) >> SC_Q24_SHFT)
	     != (Q24DEV(dev) >> Q24SHFT)) ||
	    (((sc->sc_flags & SC_Q11) >> SC_Q11_SHFT)
	     != (Q11DEV(dev) >> Q11SHFT)) ||
	    (ts->tssr & TSSR_NBA)) {
		sc->sc_flags &= ~(SC_BUSTED | SC_INITIALIZED);

#ifdef MDOVE
if (((sc->sc_flags & SC_FORMAT) >> SC_FORMAT_SHFT) != (RAWDEV(dev) >> RAWSHFT))
  printf ("tsopen: changing tape format\n");
if ((((sc->sc_flags & SC_Q24) >> SC_Q24_SHFT) != (Q24DEV(dev) >> Q24SHFT)) ||
   (((sc->sc_flags & SC_Q11) >> SC_Q11_SHFT) != (Q11DEV(dev) >> Q11SHFT))) {
 	printf ("tsopen: changing tape format\n");
	printf ("tsopen: old: ");
	if (sc->sc_flags & SC_Q24) printf("qic24  new:");
  	else if (sc->sc_flags & SC_Q11) printf("qic11  new:");
	else printf("unknown  new:");
	if (Q24DEV(dev)) printf("qic24\n");
	else if (Q11DEV(dev)) printf("qic11\n");
	else printf("unknown\n");
}
#endif
		if (!tsreset(sc, 1, dev)) {
			sc->sc_flags |= SC_BUSTED;
			sc->sc_flags &= ~SC_OPEN;
			u.u_error = EIO;
			return;
		}
		sc->sc_flags |= SC_OPEN;
	}

	/* wait for a previous operation to complete */
	s = splclock();
	while (sc->sc_flags & SC_REWINDING) {
		sc->sc_flags |= SC_WAITING;
		if (sleep((caddr_t) sc, PUSER|PCATCH)) {
			/* user gave up waiting...fail the open */
			sc->sc_flags &= ~(SC_WAITING | SC_OPEN);
			u.u_error = EINTR;
			splx(s);
			return;
		}
	}
	splx(s);
	if (sc->sc_flags & SC_BUSTED) {
		/* drive broke during rewind - try again */
		goto again;
	}

	/* read drive status */
	u.u_error = tsop(sc, TSCMD_SENSE, 1, 1);
	if (u.u_error) {
		/*
		 * The sense command failed.  This can happen when a
		 * new tape is inserted in the drive.
		 */
		if (ts->tssr & TSSR_NBA) {
			/* new tape in drive.  init controller */
			sc->sc_flags &= ~SC_INITIALIZED;
			u.u_error = 0;		/* redundant */
			goto again;
		}
		sc->sc_flags |= SC_BUSTED;
		sc->sc_flags &= ~SC_OPEN;
		cmn_err(CE_CONT, "ts%d: can't sense drive status\n",
				 sc->sc_ctlr);
		return;
	}
	status = STATUS(sc);

	/* there better be a tape in the drive */
	if (!(status->xs0 & XS0_ONL)) {
		/* not on line means no tape in the drive */
		sc->sc_flags &= ~SC_OPEN;
		cmn_err(CE_CONT, "ts%d: no cartridge in drive\n", sc->sc_ctlr);
		u.u_error = EIO;
		return;
	}

	/* it must be writable if we are opening with FWRITE set */
	if ((flag & FWRITE) && (status->xs0 & XS0_WLK)) {
		sc->sc_flags &= ~SC_OPEN;
		cmn_err(CE_CONT, "ts%d: tape is write protected\n",
				 sc->sc_ctlr);
		u.u_error = EIO;
		return;
	}
}

/*
 * Close tape drive, writing file marks as needed.  Rewind drive unless
 * this is the no rewind device.  Don't wait for rewind to complete, let
 * the next open do that.
 */
/*ARGSUSED*/
void
tsclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tssoftc *sc;
	int s;

	sc = &tssoftc[CTLR(dev)];
	if (sc->sc_flags & SC_WRITTEN) {
		if (u.u_error = tsop(sc, TSCMD_WFMK, 1, 1))
			goto out;
	}
	if (ISREWIND(dev)) {
		s = splclock();
		u.u_error = tsop(sc, TSCMD_REWIND, 1, 0);
		if (!u.u_error)
			sc->sc_flags |= SC_REWINDING;
		splx(s);
	}

out:
	s = splclock();
	sc->sc_flags &= ~(SC_OPEN | SC_READ | SC_WRITTEN | SC_FILEMARK | SC_EOT);
	splx(s);
}

/*
 * Do iomove for tape, byte swapping if necessary.
 * Return non-zero for errors, setting u.u_error.
 */
static int
tsiomove(dev, cp, n, flag)
	dev_t dev;
	caddr_t cp;
	unsigned n;
	int flag;
{
	if (n == 0)
		return (0);

#ifdef notdef			/* XXX Should we provide this (mis)feature? */
	if (SWAPDEV(dev))
		swiomove(cp, n, flag);
	else
#endif
		iomove(cp, n, flag);
	if (u.u_error)
		return (1);
	return (0);
}

void
tsread(dev)
	dev_t dev;
{
	register struct tssoftc *sc;
	register int amount;
	caddr_t bufva;
	int s;

	sc = &tssoftc[CTLR(dev)];
	bufva = &tsbuf[sc->sc_ctlr][0];
	if (sc->sc_flags & SC_WRITTEN) {
		/*
		 * QIC-02 tapes are not read&write.  They are one or the other.
		 * Disallow a read if the tape has ever been written.
		 */
		u.u_error = EINVAL;
		return;
	}

#ifdef notdef
	/*
	 * I do not see any reason for this.  Lots of things break if
	 * this condition is put in.
	 */
	if ((long)u.u_count & 1) {
		/*
		 * Only allow even i/o counts
		 */
		u.u_error = EIO;
		return;
	}
#endif

	while (u.u_count) {
		if (sc->sc_flags & SC_FILEMARK) {
			/* sorry, can't read past file mark */
			return;
		}
		if (sc->sc_flags & SC_EOT) {
			u.u_error = ENXIO;
			return;
		}
		amount = u.u_count;
		if (amount > TSBUFSIZE)
			amount = TSBUFSIZE;
		if (amount & 511) {
			/* clear the part not being used */
			bzero((caddr_t) K0_TO_K1(bufva) + (amount & 511),
			      512 - (amount & 511));
		}
		if (amount & 511)
			amount += 512 - (amount & 511);
		if (!tscmd(sc, TSCMD_READ, (caddr_t) K0_TO_PHYS(bufva),
			       amount, 1)) {
			tserr(sc);
			u.u_error = EIO;
			return;
		}
			
		if (STATUS(sc)->xs0 != PAT1 && STATUS(sc)->xs0 & XS0_EOT) {
			sc->sc_flags |= SC_EOT;
			if (!tsreset(sc, 1, dev))
				cmn_err(CE_CONT,"ts: reset failed\n");
			u.u_error = ENXIO;
			return;
		}

		if ( (sc->sc_flags & SC_FILEMARK) ||
		     (STATUS(sc)->xs0 & XS0_RLS) ) {
			volatile struct tsstatus *status;
			/*
			 * Reduce transfer count by the amount that wasn't
			 * transfered due to the file mark.
			 */
			status = STATUS(sc);
			amount -= status->resid;
		}
		if (tsiomove(dev, (caddr_t) K0_TO_K1(bufva),
				  MIN(amount, u.u_count), B_READ))
			break;

		/* If tried to read more than we have, return how much read */
		if (STATUS(sc)->xs0 & XS0_RLS)
			break;
			
	}
	s = splclock();
	sc->sc_flags |= SC_READ;
	splx(s);
}

void
tswrite(dev)
	dev_t dev;
{
	register struct tssoftc *sc;
	register int amount;
	caddr_t bufva;
	int s;

	bufva = &tsbuf[CTLR(dev)][0];
	sc = &tssoftc[CTLR(dev)];
	if (sc->sc_flags & SC_READ) {
		/*
		 * QIC-02 tapes are not read&write.  They are one or the other.
		 * Disallow a write if the tape has ever been read.
		 */
		u.u_error = EINVAL;
		return;
	}
#ifdef notdef
	/*
	 * I do not see any reason for this.  Lots of things break if
	 * this condition is put in.
	 */
	if ((long)u.u_count & 1) {
		/*
		 * Only allow even i/o counts
		 */
		u.u_error = EIO;
		return;
	}
#endif

	while (u.u_count) {
		if (sc->sc_flags & SC_EOT)
			return;
		amount = u.u_count;
		if (amount > TSBUFSIZE)
			amount = TSBUFSIZE;
		if (amount & 511) {
			/* clear the part not being used */
			bzero((caddr_t) K0_TO_K1(bufva) + (amount & 511),
			      512 - (amount & 511));
			if (RAWDEV(dev))
				amount += 512 - (amount & 511);
		}
		if (tsiomove(dev, (caddr_t) K0_TO_K1(bufva),
				  MIN(amount, u.u_count), B_WRITE))
			break;
		if (!tscmd(sc, TSCMD_WRITE, (caddr_t) K0_TO_PHYS(bufva),
			       amount, 1)) {
			if (sc->sc_flags & SC_EOT)
				u.u_error = ENXIO;
			else {
				tserr(sc);
				u.u_error = EIO;
			}
			break;
		}
	}
	s = splclock();
	sc->sc_flags |= SC_WRITTEN;
	splx(s);
}

/*ARGSUSED*/
void
tsioctl(dev, cmd, arg, flag)
	dev_t dev;
	unsigned int cmd;
	caddr_t arg;
	int flag;
{
	register struct tssoftc *sc;
	struct mtop mtop;
	struct mtget mtget;
	int blocksize;

	sc = &tssoftc[CTLR(dev)];
	switch (cmd) {
	  case MTIOCTOP:			/* tape operation */
		if (copyin(arg, (caddr_t) &mtop, sizeof(mtop)) < 0) {
			u.u_error = EFAULT;
			break;
		}
		switch (mtop.mt_op) {
		  case MTWEOF:
			u.u_error = tsop(sc, TSCMD_WFMK, (int)mtop.mt_count, 1);
			break;
		  case MTFSF:
			u.u_error = tsop(sc, TSCMD_FSF, (int)mtop.mt_count, 1);
			break;
		  case MTFSR:
			u.u_error = tsop(sc, TSCMD_FSR, (int)mtop.mt_count, 1);
			break;
		  case MTREW:
			u.u_error = tsop(sc, TSCMD_REWIND, 1, 1);
			break;
		  case MTRET:
			u.u_error = tsretension(sc);
			break;
		  case MTRST:
			if (!tsreset(sc, 1, dev))
				u.u_error = EIO;
			break;
		  case MTNOP:
		  case MTOFFL:
			break;
		  case MTBSF:
		  case MTBSR:
			u.u_error = EOPNOTSUPP;
			return;
		  default:
			u.u_error = EINVAL;
			return;
		}
		break;
	  case MTIOCGET:
	  {
		register volatile struct tsstatus *status;

		if (copyin(arg, (caddr_t) &mtget, sizeof(mtget)) < 0) {
			u.u_error = EFAULT;
			break;
		}
		status = STATUS(sc);
		mtget.mt_dsreg = status->xs0;
		mtget.mt_erreg = status->xs1;
		mtget.mt_resid = 0;
		mtget.mt_type = MT_ISQIC;
		if (copyout((caddr_t) &mtget, arg, sizeof(mtget)) < 0) {
			u.u_error = EFAULT;
			break;
		}
	  }
		break;
#ifdef notdef
	  case MTIOCGETBLKSIZE:
		/* set block size to 400 for iris compatability */
		blocksize = 400;
		if (copyout((caddr_t) &blocksize, arg, sizeof(blocksize)) < 0)
			u.u_error = EFAULT;
		break;
#endif
	  default:
		u.u_error = EINVAL;
		break;
	}
}

/*
 * Interrupt handler.
 */
void
tsintr(ctlr)
	int ctlr;
{
	register struct tssoftc *sc;
	register volatile struct tsdevice *ts;
	register volatile struct tsstatus *status;
	register ushort tssr;
	int s;

	s = splclock();
	sc = &tssoftc[ctlr];
	if (sc->sc_id) {			/* clear timeout, asap */
		untimeout(sc->sc_id);
		sc->sc_id = 0;
	}
	splx(s);
	if (!(sc->sc_flags & SC_BUSY)) {
		cmn_err(CE_CONT, "ts%d: stray interrupt\n", ctlr);
		return;
	}

	ts = sc->sc_device;
	tssr = ts->tssr;
	status = STATUS(sc);

	/*
	 * FIRST.  Check and see if controller dropped the message packet
	 * somewhere else.
	 */
	if ((status->resid == PAT0) &&
	    (status->xs0 == PAT1) &&
	    (status->xs1 == PAT2) &&
	    (status->xs2 == PAT3) &&
	    (status->xs3 == PAT4)) {
		cmn_err(CE_CONT,
			"ts%d: system memory corrupted during tape op\n",
			ctlr);
		sc->sc_flags |= SC_ERROR;
		goto skiperr;
	}

	if (tssr & TSSR_SC) {
		/* decode error information */
		if (tssr & TSSR_RMR) {
			/* bad news */
			sc->sc_flags |= SC_ERROR;
		} else {
			switch (tssr & TSSR_TC) {
			  case TSTC_ALERT:
				/*
				 * This error occurs for the following
				 * known reasons:
				 *	- reading a filemark
				 *	- record length short or long
				 */
				if (status->xs0 & XS0_TMK)
					sc->sc_flags |= SC_FILEMARK;
				else if (!((CMD(sc)->cmd & TSCMD_READ) &&
					(status->xs0 & ( XS0_RLS | XS0_RLL ))))
					sc->sc_flags |= SC_ERROR;
				else if (status->xs0 & XS0_EOT)
					sc->sc_flags |= SC_ERROR|SC_EOT;
				break;
			  case TSTC_UNREC0:
				/*
				 * This error occurs for the following
				 * known reasons:
				 *	- removing the cartridge during a write
				 *	- reading a blank tape
				 *	- forward space file'ing a blank tape
				 *	- forward space record'ing a blank tape
				 */
				sc->sc_flags |= SC_BUSTED|SC_ERROR;
				break;
			  case TSTC_UNREC2:
				/*
				 * This error occurs for the following
				 * known reasons:
				 *	- writing past EOT
				 */
				sc->sc_flags |= SC_BUSTED|SC_ERROR|SC_EOT;
				break;
			  case TSTC_REJECT:
				/*
				 * This error occurs for the following
				 * known reasons:
				 *	- removing the cartridge during a read
				 */
				sc->sc_flags |= SC_BUSTED|SC_ERROR;
				break;
			  default:
				sc->sc_flags |= SC_BUSTED|SC_ERROR;
				break;
			}
		}
	}

skiperr:
	sc->sc_flags &= ~(SC_BUSY | SC_REWINDING);
	if (sc->sc_flags & SC_WAITING) {
		sc->sc_flags &= ~SC_WAITING;
		wakeup((caddr_t) sc);
	}
}

/*
 * Issue a command to the controller, and poll for its completion.
 */
static int
tspollcmd(sc, op, buf, bcount)
	register struct tssoftc *sc;
	int op;
	caddr_t buf;
	int bcount;
{
	tsfill(sc, op, buf, bcount);
	return tspoll(sc, 0);
}

/*
 * Dump kernel memory to tape for crash dumping.
 */
/*ARGSUSED*/
int
tsdump(dev, flag, bn, physaddr, count)
	dev_t dev;
	int flag;
	daddr_t bn;
	caddr_t physaddr;
	int count;
{
	register struct tssoftc *sc;
	int amount;

	/* make sure controller probed */
	if (CTLR(dev) >= tsctlrs)
		return ENXIO;
	sc = &tssoftc[CTLR(dev)];
	if (!(sc->sc_flags & SC_PROBED))
		return ENXIO;

	if (flag == DUMP_OPEN) {
		/* initialize device */
		if (!tsreset(sc, 0, dev))
			return EIO;
		if (!tspollcmd(sc, TSCMD_SENSE, (caddr_t) 0, 0))
			return EIO;
		return 0;
	}
	if (flag == DUMP_CLOSE) {
		/* write closing file mark and rewind */
		if (!tspollcmd(sc, TSCMD_WFMK, (caddr_t) 0, 0))
			return EIO;
		return 0;
	}

	/* write data out */
	count = BBTOB(count);
	while (count) {
		amount = count;
		if (amount > TSBUFSIZE)
			amount = TSBUFSIZE;
		if (!tspollcmd(sc, TSCMD_WRITE, physaddr, amount))
			return EIO;
		count -= amount;
		physaddr += amount;
	}
	return 0;
}

