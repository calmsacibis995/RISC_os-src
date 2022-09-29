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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: utsname.h,v 1.23.1.6.1.3.1.4 91/01/09 19:21:48 beacker Exp $ */

#ifndef	_SYS_UTSNAME_
#define	_SYS_UTSNAME_

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/* NOTE:
 * The MIPS' naming convention for all utsname strings requires the strings
 * to be legitimate cpp "names" (e.g. no '.', '/', etc.).
 *
 * Only "nodename" and "sysname" should be defined by the system administrator.
 * The rest of the strings should be defined at configuration or boot time
 * using the constants defined in this file.
 *
 * "sys/limits.h" must be included because it defines SYS_NMLN
 */

#include <sys/limits.h>	/* Include(s) required before this one. */

struct utsname {
	char	sysname[SYS_NMLN];	/* same as nodename field */
	char	nodename[SYS_NMLN];	/* network id name (user defined) */
	char	release[SYS_NMLN];	/* MIPS OS release name */
	char	version[SYS_NMLN];	/* MIPS OS version number */
	char	machine[SYS_NMLN];	/* MIPS system type */
	char	m_type[SYS_NMLN];	/* MIPS specific machine type */
	char	base_rel[SYS_NMLN];	/* base release for initial port */
	char	reserve5[SYS_NMLN];	/* reserved for future use */
	char	reserve4[SYS_NMLN];	/* reserved for future use */
	char	reserve3[SYS_NMLN];	/* reserved for future use */
	char	reserve2[SYS_NMLN];	/* reserved for future use */
	char	reserve1[SYS_NMLN];	/* reserved for future use */
	char	reserve0[SYS_NMLN]	/* reserved for future use */
};

	/* valid release values */
#define R_2_0		"2_0"
#define R_3_0		"3_0"
#define R_3_1		"3_1"
#define R_3_2		"3_2"
#define R_3_01		"3_01"
#define R_3_02		"3_02"
#define R_3_10		"3_10"
#define R_3_11		"3_11"
#define R_3_20		"3_20"
#define R_4_0_		"4_0-"
#define R_4_0		"4_0"
#define R_4_01		"4_01"
#define R_4_02		"4_02"
#define R_4_10		"4_10"
#define R_4_20		"4_20"
#define R_4_30		"4_30"
#define R_4_50		"4_50"
#define R_4_51		"4_51"
#define R_4_52		"4_52"
#define R_5_0		"5_0"

	/* valid version values */
#define V_UMIPSBSD	"UMIPS_BSD"
#define V_UMIPSV	"UMIPS_V"
#define V_UMIPS		"UMIPS"

	/* machine value is the same as what cpp defines */
#define M_MIPS		"mips"

	/* valid m_type values */
#define MT_M500		"m500"
#define MT_M800		"m800"
#define MT_M1000	"m1000"
#define MT_DT1200_5	"m120-5"
#define MT_DT1200_3	"m120-3"
#define MT_DT1200	"m120"		/* Obsolete - here to not bust stuff */
#define MT_M2000_8	"m2000-8"
#define MT_M2000_6	"m2000-6"
#define MT_M2000	"m2000"		/* Obsolete - here not to bust stuff */
#define MT_I2000	"rc2030"
#define MT_M180		"RC3240"
#define MT_M6000	"RC6280"
#define	MT_R3030	"RC3230"	/* computer server machine */
#define	MT_R3030_S	"RS3230"	/* work statsion version */
#define	MT_R3030_33	"RC3330"	/* computer server machine */
#define	MT_R3030_33_S	"RS3330"	/* work statsion version */
#define MT_RB3125	"m2000-25"
#define MT_RB3133	"m2000-33"
#define MT_R6260	"RC6260"

	/* valid base release values */
#define BR_V30_ATT	"ATT_V3_0"
#define BR_V31_ATT	"ATT_V3_1"
#define BR_43_BSD	"4_3_BSD"

#if defined KERNEL || defined INKERNEL
extern struct utsname	utsname;
extern char		hostname[];
extern short		hostnamelen;
extern char		domainname[];
extern short		domainnamelen;
#endif

#endif	_SYS_UTSNAME_
