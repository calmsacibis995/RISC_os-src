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
/* $Header: mousereg.h,v 1.2.2.2 90/05/10 06:29:29 wje Exp $ */
/*
 * $Header: mousereg.h,v 1.2.2.2 90/05/10 06:29:29 wje Exp $
 */
struct MouseBrains {
  MouseReg *MouseEyes;		/* Pointer to interface area */
  int Health;
  struct ss_line *Mouse_dpl;
};
typedef struct MouseBrains MouseBody;


#define MOUSE_DEAD	0x0000
#define MOUSE_AVAIL	0x0001
#define MOUSE_OPEN	0x0002
#define MOUSE_STOPPED	0x0004
#define MOUSE_TALKING	0x0008
