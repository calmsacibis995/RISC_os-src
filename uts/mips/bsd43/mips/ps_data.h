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
/* $Header: ps_data.h,v 1.6.3.2 90/05/10 04:42:11 wje Exp $ */
/*
 *	Each constant in this file is an integer index into the ps_data
 *	array in /vmunix. This array can be read to get the size of
 *	individual structures even if the structure changes given that
 *	it changes at the end of the strucuture.
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/* the entries here need to match the constants in ps_data.c */

#define BSD43_PS_DATA_proc		0
#define BSD43_PS_DATA_pte		1
#define BSD43_PS_DATA_text		2
#define BSD43_PS_DATA_user		3
#define BSD43_PS_DATA_user_u_comm	4
#define BSD43_PS_DATA_user_u_arg	5
#define BSD43_PS_DATA_inode		6
#define BSD43_PS_DATA_tty		7
#define BSD43_PS_DATA_ucred		8
#define BSD43_PS_DATA_rusage		9
#define BSD43_PS_DATA_file		10
#define BSD43_PS_DATA_ucred		11
#define BSD43_PS_DATA_rusage		12
#define BSD43_PS_DATA_map		13
#define BSD43_PS_DATA_swdevt		14
#define BSD43_PS_DATA_vme_device	15
#define BSD43_PS_DATA_vme_driver	16

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define PS_DATA_file BSD43_PS_DATA_file
#   define PS_DATA_inode BSD43_PS_DATA_inode
#   define PS_DATA_map BSD43_PS_DATA_map
#   define PS_DATA_proc BSD43_PS_DATA_proc
#   define PS_DATA_pte BSD43_PS_DATA_pte
#   define PS_DATA_rusage BSD43_PS_DATA_rusage
#   define PS_DATA_swdevt BSD43_PS_DATA_swdevt
#   define PS_DATA_text BSD43_PS_DATA_text
#   define PS_DATA_tty BSD43_PS_DATA_tty
#   define PS_DATA_ucred BSD43_PS_DATA_ucred
#   define PS_DATA_user BSD43_PS_DATA_user
#   define PS_DATA_user_u_arg BSD43_PS_DATA_user_u_arg
#   define PS_DATA_user_u_comm BSD43_PS_DATA_user_u_comm
#   define PS_DATA_vme_device BSD43_PS_DATA_vme_device
#   define PS_DATA_vme_driver BSD43_PS_DATA_vme_driver
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


