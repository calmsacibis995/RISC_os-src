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
/* $Header: svc_auth.h,v 1.2.1.2 90/05/07 21:01:59 wje Exp $ */
/*
 * @(#)svc_auth.h 1.1 88/03/04 4.0NFSSRC SMI
 * @(#)svc_auth.h 1.7 88/02/08 SMI
 *
 * svc_auth.h, Service side of rpc authentication.
 */


/*
 * Server side authenticator
 */
extern enum auth_stat _authenticate();
