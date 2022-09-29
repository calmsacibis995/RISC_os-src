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
/* $Header: style.h,v 1.1.1.2 90/05/07 19:29:34 wje Exp $ */

/*	style.h	4.3	83/08/11	*/
extern int part;
extern int style;
extern int topic;
extern int pastyle;
extern int lstyle;
extern int rstyle;
extern int estyle;
extern int nstyle;
extern int Nstyle;
extern int rthresh;
extern int lthresh;
extern int pstyle;
extern int count;
extern int sleng[50];
extern int numsent;
extern long letnonf;
extern long numnonf;
extern long vowel;
extern long numwds;
extern long twds;
extern long numlet;
extern int maxsent;
extern int maxindex;
extern int minindex;
extern int qcount;
extern int icount;
extern int minsent;
extern int simple;
extern int compound;
extern int compdx;
extern int complex;
extern int nomin;
extern int tobe;
extern int noun, infin, pron, aux, adv;
extern int passive;
extern int beg[15];
extern int prepc;
extern int conjc;
extern int verbc;
extern int tverbc;
extern int adj;
#define MAXPAR 20
extern int leng[];
extern sentno;
