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
/* $Header: hwconf.h,v 1.6.3.2 90/05/10 04:41:45 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * hdwconf.h -- hardware specific configuration information
 */

/*
 * revision id for chips
 */
union bsd43_(rev_id) {
	unsigned int	ri_uint;
	struct {
#ifdef MIPSEB
		unsigned int	Ri_fill:16,
				Ri_imp:8,		/* implementation id */
				Ri_majrev:4,		/* major revision */
				Ri_minrev:4;		/* minor revision */
#endif MIPSEB
#ifdef MIPSEL
		unsigned int	Ri_minrev:4,		/* minor revision */
				Ri_majrev:4,		/* major revision */
				Ri_imp:8,		/* implementation id */
				Ri_fill:16;
#endif MIPSEL
	} Ri;
};
#define	bsd43_ri_imp		Ri.Ri_imp
#define	bsd43_ri_majrev	Ri.Ri_majrev
#define	bsd43_ri_minrev	Ri.Ri_minrev

struct bsd43_(imp_tbl) {
	char *it_name;
	unsigned it_imp;
};

/*
 * NVRAM information
 */
#define BSD43_ENV_MAXLEN	32
#define BSD43_ENV_ENTRIES	6
struct bsd43_(promenv) {
	char	bsd43_(name)[BSD43_ENV_MAXLEN];
	char	value[BSD43_ENV_MAXLEN];

};

/*
 * contains configuration information for all hardware in system
 */
struct bsd43_(hw_config) {
	unsigned	icache_size;
	unsigned	dcache_size;
	union bsd43_(rev_id)	cpu_processor;
	union bsd43_(rev_id)	fpu_processor;
	unsigned char	cpubd_type;
	unsigned char	cpubd_rev;
	char		cpubd_snum[5];
	int		cpubd_config;
	struct bsd43_(promenv)	bsd43_(promenv)[BSD43_ENV_ENTRIES];
#ifdef TODO
	add memory board id prom information
#endif TODO
};

/*
 * options to hdwconf() syscall
 */
#define BSD43_HWCONF_GET	0
#define BSD43_HWCONF_SET	1


#ifndef LOCORE
#ifdef KERNEL
extern struct bsd43_(hw_config) bsd43_(hwconf);
#endif KERNEL
#endif !LOCORE

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ENV_ENTRIES BSD43_ENV_ENTRIES
#   define ENV_MAXLEN BSD43_ENV_MAXLEN
#   define HWCONF_GET BSD43_HWCONF_GET
#   define HWCONF_SET BSD43_HWCONF_SET
#   define ri_imp bsd43_ri_imp
#   define ri_majrev bsd43_ri_majrev
#   define ri_minrev bsd43_ri_minrev
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


