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
/* $Header: top.h,v 1.4.1.2.1.2.1.2 90/10/23 13:43:40 beacker Exp $ */
/*
 *  Top - a top users display for Berkeley Unix
 *
 *  General (global) definitions
 */

/* Number of lines of header information on the standard screen */
#define Header_lines	6

/* Number of columns needed for display */
#define Display_width	80

/* Log base 2 of 1024 is 10 (2^10 == 1024) */
#define LOG1024		10

/* Convert clicks (kernel pages) to kbytes ... */
/* If there is no PGSHIFT defined, assume it is 11 */
/* Is this needed for compatability with some old flavor of 4.2 or 4.1? */
#if defined(scs)
	/* the s/375 <machine/param.h> has this as 9, but it's really 10 */
# undef PGSHIFT
# define PGSHIFT 10
#endif scs

#ifdef RISCOS
#define pagetok(size)	((size) << (pfn_to_byte_shift - LOG1024))
#else RISCOS
#ifndef PGSHIFT
# define pagetok(size)	((size) << 1)
#else
# if PGSHIFT>10
#  define pagetok(size)	((size) << (PGSHIFT - LOG1024))
# else
#  define pagetok(size)	((size) >> (LOG1024 - PGSHIFT))
# endif
#endif
#endif RISCOS

extern double logcpu;

double log();
double exp();

extern char (* screenbuf)[Display_width];

#ifdef RISCOS
#define CPUSTATES 5
#define NUM_PROCSTATES 8

struct user_data {
	char ud_psargs[PSARGSZ];
	struct bsd43_rusage ud_ru;
	char ud_valid;
	};
extern struct user_data *proc_user_data;
#else RISCOS
#define NUM_PROCSTATES 7
#endif RISCOS

