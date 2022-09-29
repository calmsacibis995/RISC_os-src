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
#ident	"$Header: acu.c,v 1.1.2.4 90/05/28 17:51:22 wje Exp $"

#include "tip.h"

static acu_t *acu = NOACU;
static int conflag;
static int acuabort();
static acu_t *acutype();
static jmp_buf jmpbuf;
/*
 * Establish connection for tip
 *
 * If DU is true, we should dial an ACU whose type is AT.
 * The phone numbers are in PN, and the call unit is in CU.
 *
 * If the PN is an '@', then we consult the PHONES file for
 *   the phone numbers.  This file is /etc/phones, unless overriden
 *   by an exported shell variable.
 *
 * The data base files must be in the format:
 *	host-name[ \t]*phone-number
 *   with the possibility of multiple phone numbers
 *   for a single host acting as a rotary (in the order
 *   found in the file).
 */
char *
connect()
{
	register char *cp = PN;
	char *phnum, string[256];
	FILE *fd;
	int tried = 0;

	if (!DU) {		/* regular connect message */
		if (CM != NOSTR)
			pwrite(FD, CM, size(CM));
		return (NOSTR);
	}
	/*
	 * @ =>'s use data base in PHONES environment variable
	 *        otherwise, use /etc/phones
	 */
	signal(SIGINT, acuabort);
	signal(SIGQUIT, acuabort);
	if (setjmp(jmpbuf)) {
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		printf("\ncall aborted\n");
		logent(value(HOST), "", "", "call aborted");
		if (acu != NOACU) {
			boolean(value(VERBOSE)) = FALSE;
			if (conflag)
				disconnect(NOSTR);
			else
				(*acu->acu_abort)();
		}
		setreuid(uid, euid);
		setregid(gid, egid);
		delock(uucplock);
		exit(1);
	}
	if ((acu = acutype(AT)) == NOACU)
		return ("unknown ACU type");
	if (*cp != '@') {
		while (*cp) {
			for (phnum = cp; *cp && *cp != ','; cp++)
				;
			if (*cp)
				*cp++ = '\0';
			
			if (conflag = (*acu->acu_dialer)(phnum, CU)) {
				logent(value(HOST), phnum, acu->acu_name,
					"call completed");
				return (NOSTR);
			} else
				logent(value(HOST), phnum, acu->acu_name,
					"call failed");
			tried++;
		}
	} else {
		setreuid(uid, euid);
		setregid(gid, egid);
		if ((fd = fopen(PH, "r")) == NOFILE) {
			printf("%s: ", PH);
			setregid(egid, gid);
			setreuid(euid, uid);
			return ("can't open phone number file");
		}
		setregid(egid, gid);
		setreuid(euid, uid);
		while (fgets(string, sizeof(string), fd) != NOSTR) {
			for (cp = string; !any(*cp, " \t\n"); cp++)
				;
			if (*cp == '\n') {
				fclose(fd);
				return ("unrecognizable host name");
			}
			*cp++ = '\0';
			if (strcmp(string, value(HOST)))
				continue;
			while (any(*cp, " \t"))
				cp++;
			if (*cp == '\n') {
				fclose(fd);
				return ("missing phone number");
			}
			for (phnum = cp; *cp && *cp != ',' && *cp != '\n'; cp++)
				;
			if (*cp)
				*cp++ = '\0';
			
			if (conflag = (*acu->acu_dialer)(phnum, CU)) {
				fclose(fd);
				logent(value(HOST), phnum, acu->acu_name,
					"call completed");
				return (NOSTR);
			} else
				logent(value(HOST), phnum, acu->acu_name,
					"call failed");
			tried++;
		}
		fclose(fd);
	}
	if (!tried)
		logent(value(HOST), "", acu->acu_name, "missing phone number");
	else
		(*acu->acu_abort)();
	return (tried ? "call failed" : "missing phone number");
}

disconnect(reason)
	char *reason;
{
	if (!conflag)
		return;
	if (reason == NOSTR) {
		logent(value(HOST), "", acu->acu_name, "call terminated");
		if (boolean(value(VERBOSE)))
			printf("\r\ndisconnecting...");
	} else 
		logent(value(HOST), "", acu->acu_name, reason);
	(*acu->acu_disconnect)();
#ifdef RISCOS
	ioctl(FD,TIOCCDTR,0);
	sleep(1);
	ioctl(FD,TIOCSDTR,0);
	restore_termio(FD);
	close(FD);
#endif RISCOS
}

static int
acuabort(s)
{
	signal(s, SIG_IGN);
	longjmp(jmpbuf, 1);
}

static acu_t *
acutype(s)
	register char *s;
{
	register acu_t *p;
	extern acu_t acutable[];

	for (p = acutable; p->acu_name != '\0'; p++)
		if (!strcmp(s, p->acu_name))
			return (p);
	return (NOACU);
}
