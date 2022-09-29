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
#ident	"$Header: udpublickey.c,v 1.1.1.2 90/05/09 19:31:58 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)udpublickey.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (C) 1986, Sun Microsystems, Inc.
 */

/*
 * YP updater for public key map
 */
#include <stdio.h>
#include <rpc/rpc.h>
#include <rpcsvc/ypclnt.h>
#include <sys/file.h>

extern char *malloc();

main(argc, argv)
	int argc;
	char *argv[];
{
	unsigned op;
	char name[MAXNETNAMELEN + 1];
	char key[256];
	char data[256];
	char line[256];
	unsigned keylen;
	unsigned datalen;
	FILE *rf;
	FILE *wf;
	char *fname;
	char *tmpname;
	int err;


	if (argc !=  3) {
		exit(YPERR_YPERR);
	}
	fname = argv[1];
	tmpname = malloc(strlen(fname) + 4);
	if (tmpname == NULL) {
		exit(YPERR_YPERR);
	}
	sprintf(tmpname, "%s.tmp", fname);
	
	/*
	 * Get input
	 */
	if (! scanf("%s\n", name)) {
		exit(YPERR_YPERR);	
	}
	if (! scanf("%u\n", &op)) {
		exit(YPERR_YPERR);
	}
	if (! scanf("%u\n", &keylen)) {
		exit(YPERR_YPERR);
	}
	if (! fread(key, keylen, 1, stdin)) {
		exit(YPERR_YPERR);
	}
	key[keylen] = 0;
	if (! scanf("%u\n", &datalen)) {
		exit(YPERR_YPERR);
	}
	if (! fread(data, datalen, 1, stdin)) {
		exit(YPERR_YPERR);
	}
	data[datalen] = 0;

	/*
	 * Check permission
	 */
	if (strcmp(name, key) != 0) {
		exit(YPERR_ACCESS);
	}
	if (strcmp(name, "nobody") == 0) {
		/*
		 * Can't change "nobody"s key.
		 */
		exit(YPERR_ACCESS);
	}

	/*
	 * Open files 
	 */
	rf = fopen(fname, "r");
	if (rf == NULL) {
		exit(YPERR_YPERR);	
	}
	wf = fopen(tmpname, "w");
	if (wf == NULL) {
		exit(YPERR_YPERR);
	}
	err = -1;
	while (fgets(line, sizeof(line), rf)) {
		if (err < 0 && match(line, name)) {
			switch (op) {
			case YPOP_INSERT:
				err = YPERR_KEY;
				break;
			case YPOP_STORE:
			case YPOP_CHANGE:
				fprintf(wf, "%s %s\n", key, data);	
				err = 0;
				break;
			case YPOP_DELETE:
				/* do nothing */
				err = 0;
				break;
			}	
		} else {
			fputs(line, wf);
		}
	}
	if (err < 0) {
		switch (op) {
		case YPOP_CHANGE:	
		case YPOP_DELETE:
			err = YPERR_KEY;
			break;
		case YPOP_INSERT:
		case YPOP_STORE:
			err = 0;	
			fprintf(wf, "%s %s\n", key, data);	
			break;
		}
	}
	fclose(wf);
	fclose(rf);
	if (err == 0) {
		if (rename(tmpname, fname) < 0) {
			exit(YPERR_YPERR);	
		}
	} else {
		if (unlink(tmpname) < 0) {
			exit(YPERR_YPERR);
		}
	}
	if (fork() == 0) {
		close(0); close(1); close(2);
		open("/dev/null", O_RDWR, 0);
		dup(0); dup(0);
		execl("/bin/sh", "sh", "-c", argv[2], NULL);
	}
	exit(err);
}


match(line, name)
	char *line;
	char *name;
{
	int len;

	len = strlen(name);
	return(strncmp(line, name, len) == 0 && 
		(line[len] == ' ' || line[len] == '\t'));
}
