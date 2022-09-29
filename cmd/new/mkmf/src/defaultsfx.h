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
/* $Header: defaultsfx.h,v 1.1.1.2 90/05/09 18:00:11 wje Exp $ */
".c", SFXSRC, INCLUDE_C,	/* C */
".e", SFXSRC, INCLUDE_FORTRAN,	/* Efl */
".F", SFXSRC, INCLUDE_FORTRAN,	/* Fortran */
".f", SFXSRC, INCLUDE_FORTRAN,	/* Fortran */
".h", SFXHEAD, INCLUDE_NONE,	/* header */
".i", SFXHEAD, INCLUDE_NONE,	/* Pascal include */
".l", SFXSRC, INCLUDE_C,	/* Lex */
".o", SFXOBJ, INCLUDE_NONE,	/* object */
".p", SFXSRC, INCLUDE_PASCAL,	/* Pascal */
".r", SFXSRC, INCLUDE_FORTRAN,	/* Ratfor */
".s", SFXSRC, INCLUDE_NONE,	/* Assembler */
".y", SFXSRC, INCLUDE_C,	/* Yacc */
