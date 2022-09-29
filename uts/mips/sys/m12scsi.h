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
/* $Header: m12scsi.h,v 1.2.1.2 90/05/10 06:27:50 wje Exp $ */
/*
** M12 SCSI related defines
*/
#define NTARGET		8
#define MAX_SGENTRY	17	/* max X 4k => 64k byte transfer + 1 */

extern int scsiexterr, isd_Ntarget, isd_Nlun;
extern struct scsi_unit isd_un[];
extern struct iotime isd_iotime[];
extern struct iobuf isd_tab[];
extern int isd_majors[];
