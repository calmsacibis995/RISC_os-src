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
/* $Header: sha_errs.h,v 1.2.3.2 90/05/10 06:36:42 wje Exp $ */
/*
 * $Header: sha_errs.h,v 1.2.3.2 90/05/10 06:36:42 wje Exp $
 */
#ifndef	SHA_ASCII
#define	SHA_ASCII
char *ascii_sha_cerr[SHA_CERR_MAX] = {
	"no controller errors",			/* SHA_CERR_OK */
	"undefined error",			/* UNDEF */
	"host sent a bad command",		/* SHA_CERR_CMDERR */
	"error selecting target",		/* SHA_CERR_SELERR */
	"wrong interrupt occured",		/* SHA_CERR_IERR */
	"host adaptor timeout",			/* SHA_CERR_TMOERR */
	"SPC in wrong phase",			/* SHA_CERR_PCERR */
	"undefined error",			/* UNDEF */
	"SPC parity error",			/* SHA_CERR_PARITY */
	"unexpected reconnect",			/* SHA_CERR_RECERR */
};

char	*ascii_sns_7_key[] = {
		"NO SENSE", "RECOVERED", "NOT READY", "MEDIUM ERROR",
		"HARDWARE ERROR", "ILLEGAL REQUEST", "UNIT ATTENTION",
		"DATA PROTECT", "BLANK CHECK", "VENDOR UNIQUE", "COPY ABORTED",
		"ABORT COMMAND", "EQUAL", "VOLUME OVERFLOW", "MISCOMPARE",
		"RESERVED",
};

char	*ascii_pdtype[] = {
		"DISK", "TAPE", "PRINTER", "PROCESSOR", "WORM"
};

#endif	SHA_ASCII
/* __EOF__ */
