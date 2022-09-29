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
#ident	"$Header: hayes.c,v 1.1.2.3 90/05/28 17:52:45 wje Exp $"

/*
 * Routines for calling up on a Hayes Modem
 * (based on the old VenTel driver).
 * The modem is expected to be strapped for "echo".
 * Also, the switches enabling the DTR and CD lines
 * must be set correctly.
 * NOTICE:
 * The easy way to hang up a modem is always simply to
 * clear the DTR signal. However, if the +++ sequence
 * (which switches the modem back to local mode) is sent
 * before modem is hung up, removal of the DTR signal
 * has no effect (except that it prevents the modem from
 * recognizing commands).
 * (by Helge Skrivervik, Calma Company, Sunnyvale, CA. 1984) 
 */
/*
 * TODO:
 * It is probably not a good idea to switch the modem
 * state between 'verbose' and terse (status messages).
 * This should be kicked out and we should use verbose 
 * mode only. This would make it consistent with normal
 * interactive use thru the command 'tip dialer'.
 */
#include "tip.h"

#define	min(a,b)	((a < b) ? a : b)

extern int errno;
static	int sigALRM();
static	int timeout = 0;
static	jmp_buf timeoutbuf;
static 	char gobble();
#define DUMBUFLEN	40
static char dumbuf[DUMBUFLEN];

#define	DIALING		1
#define IDLE		2
#define CONNECTED	3
#define	FAILED		4
static	int state = IDLE;

hay_dialer(num, acu)
	register char *num;
	char *acu;
{
	register char *cp;
	register int connected = 0;
	char dummy;
#ifdef ACULOG
	char line[80];
#endif
	if (hay_sync() == 0)		/* make sure we can talk to the modem */
		return(0);
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	fflush(stdout);
	ioctl(FD, TIOCHPCL, 0);
	ioctl(FD, TIOCFLUSH, 0);	/* get rid of garbage */
	write_modem("ATv0\r", 5);	/* tell modem to use short status codes */
	gobble("\r");
	gobble("\r");
	write_modem( "ATTD", 4);	/* send dial command */
	write_modem( num, strlen(num));
	state = DIALING;
	write_modem( "\r", 1);
	connected = 0;
	if (gobble("\r")) {
		if ((dummy = gobble("01234")) != '1')
			error_rep(dummy);
		else
			connected = 1;
	}
	if (connected)
		state = CONNECTED;
	else {
		state = FAILED;
		return (connected);	/* lets get out of here.. */
	}
	ioctl(FD, TIOCFLUSH, 0);
#ifdef ACULOG
	if (timeout) {
		sprintf(line, "%d second dial timeout",
			number(value(DIALTIMEOUT)));
		logent(value(HOST), num, "hayes", line);
	}
#endif
	if (timeout)
		hay_disconnect();	/* insurance */
	return (connected);
}


hay_disconnect()
{
	char c;
	int len, rlen;

	/* first hang up the modem*/
#ifdef DEBUG
	printf("\rdisconnecting modem....\n\r");
#endif
	ioctl(FD, TIOCCDTR, 0);
	sleep(1);
	ioctl(FD, TIOCSDTR, 0);
	goodbye();
}

hay_abort()
{

	char c;

	write_modem( "\r", 1);	/* send anything to abort the call */
	hay_disconnect();
}

static int
sigALRM()
{

	printf("\07timeout waiting for reply\n\r");
	timeout = 1;
	longjmp(timeoutbuf, 1);
}

static char
gobble(match)
	register char *match;
{
	char c;
	int i, status = 0;

	signal(SIGALRM, sigALRM);
	timeout = 0;
#ifdef DEBUG
	printf("\ngobble: waiting for %s\n", match);
#endif
	do {
		if (setjmp(timeoutbuf)) {
			alarm(0);
			signal(SIGALRM, SIG_DFL);
			return (0);
		}
		alarm(number(value(DIALTIMEOUT)));
		read(FD, &c, 1);
		alarm(0);
		c &= 0177;
		debug_write(&c,1);
#ifdef DEBUG
		printf("%c 0x%x ", c, c);
#endif
		for (i = 0; i < strlen(match); i++)
			if (c == match[i])
				status = c;
	} while (status == 0);
	signal(SIGALRM, SIG_DFL);
#ifdef DEBUG
	printf("\n");
#endif
	return (status);
}


write_modem(buf,len)
	char	*buf;
	int	len;
{
	write(FD,buf,len);
	debug_write(buf,len);
}


debug_write(buf,len) 
	char	*buf;
	int	len;
{
#ifdef RISCOS
	int	i;
	char	c;

	if (boolean(value(DEBUG_VAR))) {
		for (i = 0; i < len; i++) {
			c = buf[i];
			printf("%c",c);
			if (c == '\r')
				printf("\n");
			fflush(stdout);
		};
	};
#endif RISCOS
}


error_rep(c)
	register char c;
{
	printf("\n\r");
	switch (c) {

	case '0':
		printf("OK");
		break;

	case '1':
		printf("CONNECT");
		break;
	
	case '2':
		printf("RING");
		break;
	
	case '3':
		printf("NO CARRIER");
		break;
	
	case '4':
		printf("ERROR in input");
		break;
	
	case '5':
		printf("CONNECT 1200");
		break;
	
	default:
		printf("Unknown Modem error: %c (0x%x)", c, c);
	}
	printf("\n\r");
	return;
}

/*
 * set modem back to normal verbose status codes.
 */
goodbye()
{
	int len, rlen;
	char c;

#ifdef RISCOS
	len = FREAD;
	if (ioctl(FD, TIOCFLUSH, &len) == -1) {	/* get rid of trash */
		if (errno != EIO &&
		    errno != ENXIO) {
			perror("modem disconnected");
			printf("\r");
			fflush(stdout);
		};
		goto hangup_and_close;
	};
#else RISCOS
	ioctl(FD, TIOCFLUSH, &len);	/* get rid of trash */
#endif RISCOS
	if (hay_sync()) {
		sleep(1);
#ifndef DEBUG
		ioctl(FD, TIOCFLUSH, 0);
#endif
		write_modem( "ATH0\r", 5);		/* insurance */
#ifndef DEBUG
		c = gobble("03");
		if (c != '0' && c != '3') {
			printf("cannot hang up modem\n\r");
			printf("please use 'tip dialer' to make sure the line is hung up\n\r");
		}
#endif
		sleep(1);
		ioctl(FD, FIONREAD, &len);
#ifdef DEBUG
		printf("goodbye1: len=%d -- ", len);
		rlen = read(FD, dumbuf, min(len, DUMBUFLEN));
		dumbuf[rlen] = '\0';
		printf("read (%d): %s\r\n", rlen, dumbuf);
#endif
		write(FD, "ATv1\r", 5);
		debug_write("ATv1\r",5);
		sleep(1);
#ifdef DEBUG
		ioctl(FD, FIONREAD, &len);
		printf("goodbye2: len=%d -- ", len);
		rlen = read(FD, dumbuf, min(len, DUMBUFLEN));
		dumbuf[rlen] = '\0';
		printf("read (%d): %s\r\n", rlen, dumbuf);
#endif
	}
	ioctl(FD, TIOCFLUSH, 0);	/* clear the input buffer */
#ifdef RISCOS
hangup_and_close:
#endif RISCOS
	ioctl(FD, TIOCCDTR, 0);		/* clear DTR (insurance) */
#ifdef RISCOS
	sleep(1);
	ioctl(FD, TIOCSDTR, 0);
	restore_termio(FD);
#endif RISCOS
	close(FD);
}

#define MAXRETRY	5

hay_sync()
{
	int len, retry = 0;

#ifdef RISCOS
	sleep(1);
	ioctl(FD, TIOCFLUSH, 0);	/* clear the input buffer */
#endif RISCOS
	while (retry++ <= MAXRETRY) {
#ifdef RISCOS
		ioctl(FD,TIOCFLUSH,0);
		write_modem( "ATE1\r", 5);
#else RISCOS
		write_modem( "AT\r", 3);
#endif RISCOS
		sleep(1);
		ioctl(FD, FIONREAD, &len);
		if (len) {
			len = read(FD, dumbuf, min(len, DUMBUFLEN));
			if (len > 0)
				debug_write(dumbuf,len);
			if (index(dumbuf, '0') || 
		   	    (index(dumbuf, 'O') && index(dumbuf, 'K'))) {
#ifdef RISCOS
				ioctl(FD,TIOCFLUSH,0);
				write_modem( "ATX0\r",5);
				sleep(1);
				ioctl(FD, FIONREAD, &len);
				if (len) {
					len = read(FD, dumbuf, min(len, DUMBUFLEN));
					if (len > 0)
						debug_write(dumbuf,len);
				};
				ioctl(FD,TIOCFLUSH,0);
#endif RISCOS
				return(1);
			};
#ifdef DEBUG
			dumbuf[len] = '\0';
			printf("hay_sync: (\"%s\") %d\n\r", dumbuf, retry);
#endif
		}
		ioctl(FD, TIOCCDTR, 0);
#ifdef RISCOS
		sleep(1);
#endif RISCOS
		ioctl(FD, TIOCSDTR, 0);
	}
	printf("Cannot synchronize with hayes...\n\r");
	return(0);
}
