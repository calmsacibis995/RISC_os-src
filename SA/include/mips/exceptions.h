#ident "$Header: exceptions.h,v 1.2 90/01/23 14:13:11 huang Exp $"
/* $Copyright$ */

/*	%Q%	%I%	%M%	*/

extern unsigned long GetPending();
extern void SetPending();
extern unsigned long GetMask();
extern void SetMask();
extern void EnableInterrupts();
extern void DisableInterrupts();
extern void SetHandler();
extern void SetClearer();

