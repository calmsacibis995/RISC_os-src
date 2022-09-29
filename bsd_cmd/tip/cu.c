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
#ident	"$Header: cu.c,v 1.1.2.4 90/05/10 20:10:36 wje Exp $"

#include "tip.h"

int	cleanup();
int	timeout();

/*
 * Botch the interface to look like cu's
 */
cumain(argc, argv)
	char *argv[];
{
	register int i;
	static char sbuf[12];
	static char linebuf[1025];
	static char acubuf[1025];

	if (argc < 2) {
		printf("usage: cu telno [-t] [-s speed] [-a acu] [-l line] [-#]\n");
		exit(8);
	}
	CU = DV = NOSTR;
	for (; argc > 1; argv++, argc--) {
		if (argv[1][0] != '-')
			PN = argv[1];
		else switch (argv[1][1]) {

		case 't':
			HW = 1, DU = -1;
			--argc;
			continue;

		case 'a':
			CU = argv[2]; ++argv; --argc;
#ifdef RISCOS
			if (strncmp(CU,"/dev/",5) &&
			    CU[0] != '/') {
				strcpy(acubuf,"/dev/");
				strncpy(acubuf,CU,1024-5);
				acubuf[1024] = 0;
				CU = acubuf;
			};
#endif RISCOS
			break;

		case 's':
			if (speed(atoi(argv[2])) == 0) {
				fprintf(stderr, "cu: unsupported speed %s\n",
					argv[2]);
				exit(3);
			}
			BR = atoi(argv[2]); ++argv; --argc;
			break;

		case 'l':
			DV = argv[2]; ++argv; --argc;
#ifdef RISCOS
			if (strncmp(DV,"/dev/",5) &&
			    DV[0] != '/') {
				strcpy(linebuf,"/dev/");
				strncpy(linebuf,DV,1024-5);
				linebuf[1024] = 0;
				DV = linebuf;
			};
#endif RISCOS
			break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			if (CU)
				CU[strlen(CU)-1] = argv[1][1];
			if (DV)
				DV[strlen(DV)-1] = argv[1][1];
			break;

		default:
			printf("Bad flag %s", argv[1]);
			break;
		}
	}
	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGTERM, cleanup);

	/*
	 * The "cu" host name is used to define the
	 * attributes of the generic dialer.
	 */
	sprintf(sbuf, "cu%d", BR);
	if ((i = hunt(sbuf)) == 0) {
		printf("all ports busy\n");
		exit(3);
	}
	if (i == -1) {
		printf("link down\n");
		delock(uucplock);
		exit(3);
	}
	setbuf(stdout, NULL);
	loginit();
	gid = getgid();
	egid = getegid();
	uid = getuid();
	euid = geteuid();
	setregid(egid, gid);
	setreuid(euid, uid);
	vinit();
	setparity("none");
	boolean(value(VERBOSE)) = 0;
	if (HW)
		ttysetup(speed(BR));
	if (connect()) {
		printf("Connect failed\n");
		setreuid(uid, euid);
		setregid(gid, egid);
		delock(uucplock);
		exit(1);
	}
	if (!HW)
		ttysetup(speed(BR));
}
