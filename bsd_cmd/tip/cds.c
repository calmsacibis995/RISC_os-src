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
#ident	"$Header: cds.c,v 1.1.2.4 90/05/28 17:51:39 wje Exp $"

/*
 * Routines for calling up on a CDS 224 Modem in keyboard interactive mode
 *
 * This could be done in program interactive mode, but this way people
 * can use programs that don't understand the CDS dialer by dialing
 * by hand.
 */
#include "tip.h"

#define	MAXRETRY	5	/* Maximum number of sync retries */
#define HITBREAK	3	/* Sync retry at which to hit break */

static	int times_up();
static	int timeout = 0;
static	jmp_buf timeoutbuf;
static	int get_connect();
static	int cds_sync();
static	int is_prefix();

cds_dialer(num, acu)
	register char *num;
	char *acu;
{
	register char *cp;
	register int connected = 0;
	char *index(), line[80];

	/*
	 * Get in synch with a couple of carriage returns
	 */
	if (!cds_sync(FD)) {
		printf("can't synchronize with cds modem\n");
		fflush(stdout);
#ifdef ACULOG
		logent(value(HOST), num, "cds", "can't synch up");
#endif
		return (0);
	}
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	fflush(stdout);
	ioctl(FD, TIOCHPCL, 0);
	write(FD, "D", 1);
	for (cp = num; *cp; cp++) {
		write(FD, cp, 1);
	}
	write(FD, "\r", 1);
	timeout = 0;
	connected = 0;
	if (setjmp(timeoutbuf) == 0) {
		alarm(number(value(DIALTIMEOUT)));
		signal(SIGALRM, times_up);
		connected = get_connect();
	}
	signal(SIGALRM, SIG_DFL);
	ioctl(FD, TIOCFLUSH);
#ifdef ACULOG
	if (timeout) {
		sprintf(line, "%d second dial timeout",
			number(value(DIALTIMEOUT)));
		logent(value(HOST), num, "cds", line);
	}
#endif
	if (timeout && boolean(value(VERBOSE))) {
		printf("failed: %d second dial timeout\r\n",
			number(value(DIALTIMEOUT)));
		fflush(stdout);
	}
	if (timeout)
		cds_disconnect();	/* insurance */
	return (connected);
}

cds_disconnect()
{
#ifdef RISCOS
	ioctl(FD,TIOCCDTR,0);
	sleep(1);
	ioctl(FD,TIOCSDTR,0);
	restore_termio(FD);
#endif RISCOS
	close(FD);
}

cds_abort()
{

	write(FD, "\013", 1);
	cds_disconnect();
}

static int
times_up()
{

	printf("\07timeout waiting for reply\n");
	timeout = 1;
	longjmp(timeoutbuf, 1);
}

#define min(a,b)	((a)>(b)?(b):(a))
#define EXPECTED	"CDS > 224 AUTODIAL\r\nCDS > "

/*
 * The subroutine cds_sync() resets the modem and attempts to
 * get it into command mode.
 */
static int
cds_sync(fd)
{
	int already = 0, nread;
	char buf[60];
	int explen;
	char *start;

	explen = strlen(EXPECTED);

	/*
	 * Toggle DTR to force anyone off that might have left
	 * the modem connected, and insure a consistent state
	 * to start from.
	 */
	ioctl(FD, TIOCCDTR, 0);
	sleep(1);
	ioctl(FD, TIOCSDTR, 0);
	while (already < MAXRETRY) {
		if (already == HITBREAK) {
			ioctl(FD, TIOCSBRK, 0);
			sleep(1);
			ioctl(FD, TIOCCBRK, 0);
		}
		sleep(1);
		already++;
		/*
		 * After reseting the modem, send it two \r's to
		 * autobaud and to get into command mode.
		 */
		write(fd, "\r", 1);
		write(fd, "\r", 1);
		sleep(2);
		if (ioctl(fd, FIONREAD, (caddr_t)&nread) < 0) {
			perror("tip: ioctl");
			continue;
		}
		if (nread <= 0) {
			continue;
		}

		/*
		 * Check for expected sequence. Read the data,
		 * unset all parity bits, skip leading CRs and
		 * LFs, and compare.
		 */

		nread = read(FD, buf, min(nread, sizeof(buf) - 1));
		buf[nread] = '\0';
		if (nread < explen) {
			continue;
		}
		start = buf;
		while (*start) {
			*start = (*start & 0177);
			start++;
		}
		start = buf;
		while (*start == '\r' || *start == '\n') {
			start++;
			nread--;
		}
		if (nread < explen) {
			continue;
		}
		if (strncmp(start, EXPECTED, explen) == 0) {
			return 1;
		}
	}
	return (0);
}

/*
 * The subroutine get_connect() reads data from the modem looking
 * for a specific message. If the message is INITIATING, the connection
 * was made, and 1 is returned. Otherwise, the message is printed
 * (in verbose mode) and 0 is returned.
 *
 * Timeouts are expected to be done outside of this routine.
 */

static char *Messages[] = {
	"INITIATING",		/* Must be first */
	"NO DIAL TONE",
	"BUSY",
	"NO RINGING",
	"NO ANSWER TONE",
	"NO ANSWER",
	"VOICE DETECTED",
	"CALL FAILED",
	0
};

static int
get_connect()
{
	char c;
	int nread;
	char buf[100];
	int blen;
	int ind;

	blen = 0;
	for( ; ; ) {
		nread = read(FD, &c, 1);
		if (nread <= 0) {
			continue;
		}
		c = (c & 0177);
		if (c == '\r' || c == '\n') {
			if (blen == 0) {
				continue;
			}
			buf[blen] = '\0';
			ind = 0;
			while (Messages[ind]) {
				if (is_prefix(buf, Messages[ind])) {
					if (ind == 0) {
						return 1;
					}
					if (boolean(value(VERBOSE))) {
						printf("failed: %s\r\n",
							Messages[ind]);
					}
					return 0;
				}
				ind++;
			}
			blen = 0;
			continue;
		}
		if (blen == 0 && (c == ' ' || c == '\t')) {
			continue;
		}
		buf[blen] = c;
		blen++;
	}
}

/*
 * The subroutine is_prefix(str, prefix) returns 1 if str begins with
 * prefix, and 0 otherwise.
 */

static int
is_prefix(str, prefix)
	char *str;
	char *prefix;
{
	while (*str == *prefix) {
		str++;
		prefix++;
	}
	if (*prefix) {
		return 0;
	}
	return 1;
}
