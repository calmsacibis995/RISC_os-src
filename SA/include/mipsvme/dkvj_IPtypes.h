#ident "$Header: dkvj_IPtypes.h,v 1.5 90/01/23 13:25:20 huang Exp $"
/* $Copyright$ */

/*
 * Types for Interphase
*/


#ifndef W

typedef unsigned char	BYTE;
typedef unsigned char	UBYTE;
typedef unsigned short	USHORT;
typedef short		SHORT;
typedef unsigned short	UWORD;
typedef short		WORD;
typedef unsigned int	UINT;
typedef int		INT;
typedef unsigned long	ULONG;
typedef long		LONG;
typedef unsigned short	Bit;

#define W(x)		((x).U.w)
#define BIT(x)		(1 << (x))
#define WSIZ(x)		(sizeof(x)/sizeof(ushort))
#endif
