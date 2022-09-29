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
/* $Header: utsname.h,v 1.6.3.2 90/05/10 04:58:51 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/* for MIPS_UNAME system call */

#define	BSD43_SYS_NMLN	65	/* # of chars in uname-returned strings */

struct bsd43_(utsname) {
	char	sysname[BSD43_SYS_NMLN];	/* Same as nodename */
	char	nodename[BSD43_SYS_NMLN];	/* hostname */
	char	release[BSD43_SYS_NMLN];	/* MIPS OS release name */
	char	bsd43_(version)[BSD43_SYS_NMLN];	/* MIPS OS release number */
	char	machine[BSD43_SYS_NMLN];	/* MIPS system type */
	char	m_type[BSD43_SYS_NMLN];	/* MIPS Specific Machine Type */
	char	base_rel[BSD43_SYS_NMLN];	/* Base Release of Initial Port */
	char	reserve5[BSD43_SYS_NMLN];	/* reserved for future use */
	char	reserve4[BSD43_SYS_NMLN];	/* reserved for future use */
	char	reserve3[BSD43_SYS_NMLN];	/* reserved for future use */
	char	reserve2[BSD43_SYS_NMLN];	/* reserved for future use */
	char	reserve1[BSD43_SYS_NMLN];	/* reserved for future use */
	char	reserve0[BSD43_SYS_NMLN];	/* reserved for future use */
};
#if defined KERNEL || defined INKERNEL
extern struct bsd43_(utsname)	bsd43_(utsname);
extern char		bsd43_(hostname)[];
extern short		bsd43_(hostnamelen);
extern char		bsd43_(domainname)[];
extern short		bsd43_(domainnamelen);
#endif

/* valid sysname values */
#define BSD43_V_UMIPSBSD	"UMIPS-BSD"
#define BSD43_V_UMIPSV	"UMIPS-V"

/* valid base release values */
#define BSD43_BR_V30_ATT	"ATT_V3_0"
#define BSD43_BR_V31_ATT	"ATT_V3_1"
#define BSD43_BR_43_BSD	"4_3_BSD"

/* valid release values */
#define BSD43_R_2_0		"2_0"
#define BSD43_R_3_0		"3_0"

/* valid machine values */
#define BSD43_MT_M500		"m500"
#define BSD43_MT_M800		"m800"
#define BSD43_MT_M1200		"m1200"
#define BSD43_MT_DT1200	"dt1200"

/* valid machine values, must be what cpp defines */
#define BSD43_M_MIPS		"mips"

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BR_43_BSD BSD43_BR_43_BSD
#   define BR_V30_ATT BSD43_BR_V30_ATT
#   define BR_V31_ATT BSD43_BR_V31_ATT
#   define MT_DT1200 BSD43_MT_DT1200
#   define MT_M1200 BSD43_MT_M1200
#   define MT_M500 BSD43_MT_M500
#   define MT_M800 BSD43_MT_M800
#   define M_MIPS BSD43_M_MIPS
#   define R_2_0 BSD43_R_2_0
#   define R_3_0 BSD43_R_3_0
#   define SYS_NMLN BSD43_SYS_NMLN
#   define V_UMIPSBSD BSD43_V_UMIPSBSD
#   define V_UMIPSV BSD43_V_UMIPSV
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


