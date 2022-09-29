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
/* $Header: pwdnm.h,v 1.2.1.2 90/05/09 14:45:06 wje Exp $ */
/*
 * @(#)pwdnm.h	1.2 88/05/08 4.0NFSSRC SMI
 */

struct pwdnm {
	char *name;
	char *password;
};
typedef struct pwdnm pwdnm;


#define PWDAUTH_PROG 100036
#define PWDAUTH_VERS 1
#define PWDAUTHSRV 1
#define GRPAUTHSRV 2

bool_t xdr_pwdnm();
