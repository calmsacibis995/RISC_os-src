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
#ident	"$Header: hunt.c,v 1.1.2.5 90/05/22 21:44:36 wje Exp $"

#include "tip.h"

extern char *getremote();
extern char *rindex();

static	jmp_buf deadline;
static	int deadfl;

dead()
{

	deadfl = 1;
	longjmp(deadline, 1);
}

hunt(name)
	char *name;
{
	register char *cp;
	int (*f)();

	f = signal(SIGALRM, dead);
	while (cp = getremote(name)) {
		uucplock = rindex(cp, '/');
		if (uucplock != NULL)
			uucplock++;
		else
			uucplock = cp;
		if (mlock(uucplock) < 0) {
			delock(uucplock);
			continue;
		}
		/*
		 * Straight through call units, such as the BIZCOMP,
		 * VADIC and the DF, must indicate they're hardwired in
		 *  order to get an open file descriptor placed in FD.
		 * Otherwise, as for a DN-11, the open will have to
		 *  be done in the "open" routine.
		 */
		if (!HW)
			break;
		deadfl = 0;
		if (setjmp(deadline) == 0) {
			alarm(10);
#ifdef RISCOS
			/*
			 * Do not wait for carrier 
			 */
			FD = open(cp,O_RDWR | O_NDELAY);
			if (FD >= 0) {
				int	zero = 0;
				int	error;
				extern	int	errno;

				/*
				 * turn off non-blocking I/O
				 */
			  	if (ioctl(FD,FIONBIO,&zero) == -1) {
					error = errno;
					close(FD);
					FD = -1;
					errno = error;
				};
				save_termio(FD);
			}
#else RISCOS			
			FD = open(cp, O_RDWR);
#endif RISCOS
		}
		alarm(0);
		if (FD < 0) {
			perror(cp);
			deadfl = 1;
		}
		if (!deadfl) {
#ifdef RISCOS
		  	int zero = 0;

			record_host_clocal();
			record_host_ixon();
			record_host_rtscts();
			if (CU == NOSTR ||
			    equal(CU,DV))
				change_host_clocal(1); /* turn on CLOCAL */
			change_host_rtscts(1);
			ioctl(FD, TIOCNXCL, &zero);
#else RISCOS
			ioctl(FD, TIOCEXCL, 0);
#endif RISCOS
			ioctl(FD, TIOCHPCL, 0);
			signal(SIGALRM, SIG_DFL);
			return ((int)cp);
		}
		delock(uucplock);
	}
	signal(SIGALRM, f);
	return (deadfl ? -1 : (int)cp);
}


