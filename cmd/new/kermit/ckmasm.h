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
/* $Header: ckmasm.h,v 1.1.1.2 90/05/09 17:50:03 wje Exp $ */
/*
 * file ckmasm.h
 *
 * Module of Mac Kermit containing code for assembler instructions needed
 *
 */

/*
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained.
*/



/****************************************************************************/
pascal void
loadA0 (params)
char *params;
/****************************************************************************/
extern 0x205F;			/* MOVEA.L (A7)+,A0 */



/****************************************************************************/
pascal void
loadD1 (value)
long value;
/****************************************************************************/
extern 0x221F;			/* MOVE.L (A7)+,D1 */



/****************************************************************************/
pascal void
loadD2 (value)
long value;
/****************************************************************************/
extern 0x241F;			/* MOVE.L (A7)+,D2 */



/****************************************************************************/
pascal void
pushD0 ()
/****************************************************************************/
extern 0x2F00;			/* MOVE.L D0,-(A7) */



/****************************************************************************/
pascal void
poptoA0 ()
/****************************************************************************/
extern 0x209F;			/* MOVE.L (A7)+,(A0) */



/****************************************************************************/
pascal void
saveA0 ()
/****************************************************************************/
extern 0x2F08;			/* MOVEA.L A0,-(A7) */



/****************************************************************************/
pascal void
restoreA0 ()
/****************************************************************************/
extern 0x205F;			/* MOVEA.L (A7)+,A0 */



/****************************************************************************/
pascal void
AllRegs ()
/****************************************************************************/
extern 0xFFFF;			/* D0-D7/A0-A7 (all registers) */



/****************************************************************************/
pascal void
SaveRegs ()
/****************************************************************************/
extern 0x48E7;			/* MOVEM.L <registers>,-(A7) */



/****************************************************************************/
pascal void
RestoreRegs ()
/****************************************************************************/
extern 0x4CDF;			/* MOVEM.L (A7)+,<registers> */



/****************************************************************************/
pascal void
execute ()
/****************************************************************************/
extern 0x4E90;			/* JSR (A0) */



/****************************************************************************/
pascal void
Launch ()
/****************************************************************************/
extern 0xA9F2;			/* Launch Trap */

/**********************************/
pascal Boolean
WaitNextEvent (eventMask, theEvent, sleep, mouseRgn)
short eventMask;
EventRecord *theEvent;
long sleep;
RgnHandle mouseRgn;
/***********************************/
extern 0xA860;			/* it's a toolbox trap... */

#define num_WaitNextEvent	0x60
#define num_UnknownTrap		0x9F

/***********************************/
pascal void
UnknownTrap ()
/***********************************/
extern 0xA89F;
