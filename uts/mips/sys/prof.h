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
/* $Header: prof.h,v 1.1.3.2 90/05/10 06:32:19 wje Exp $ */
#ifdef PROFILING

/*
 *      histogram counters are unsigned shorts (according to the kernel).
 */
#define HISTCOUNTER     unsigned int

/*
 *      fraction of text space to allocate for histogram counters
 *      here, 1/2
 */
#define HISTFRACTION    2
#ifdef LANGUAGE_C
/*
 * profiling header
 * used by kgmon to determine appropriate form of mon.out file
 */
struct phdr {
    int         proftype;       /* profile type, see below */
    char        *lowpc;         /* low text pc */
    char        *highpc;        /* high text pc */
    char        *pc_buf;        /* address of pc sample buckets */
    int         pc_bytes;       /* size of pc sample buckets in bytes */
    char        *bb_buf;        /* address of bb counters */
    int         bb_bytes;       /* size of invocation/bb counters in bytes */
    char        *tos_buf;       /* address of tos array */
    int         tos_bytes;      /* size of gprof tos array in bytes */
    char        *froms_buf;     /* address of froms array */
    int         froms_bytes;    /* size of gprof froms array in bytes */
    int         sample_hz;      /* frequency of sampling */
};
#endif LANGUAGE_C

/*
 *      general rounding functions.
 */
#define ROUNDDOWN(x,y)  (((x)/(y))*(y))
#define ROUNDUP(x,y)    ((((x)+(y)-1)/(y))*(y))

int s_textsize;
unsigned int s_lowpc;
int _mcountoff;

#endif PROFILING
