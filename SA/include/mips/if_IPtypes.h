#ident "$Header: if_IPtypes.h,v 1.2 90/01/23 14:14:02 huang Exp $"
/* $Copyright$ */

/*
 *	The following types are to be used by all the interphase
 *	drivers. 
 *
 *	WARNING:
 *		Since several drivers share this header be very careful
 *		when changing any of these.
 */

typedef unsigned char	BYTE;
typedef unsigned char	UBYTE;
typedef unsigned short	USHORT;
typedef		 short	SHORT;
typedef unsigned short	UWORD;
typedef		 short	WORD;
typedef unsigned int	UINT;
typedef          int	INT;
typedef unsigned long	ULONG;
typedef          long	LONG;
typedef unsigned short    Bit;

struct longv {
	union {
		struct {
			UWORD	msw;
			UWORD	lsw;
		} v;
		ULONG	l;
	} U;
};
typedef struct longv LONGV;

#define MSW(x)	((x).U.v.msw)
#define LSW(x)	((x).U.v.lsw)
#define L(x)	((x).U.l)

struct wordv {
	union {
		struct {
			UBYTE	msb;
			UBYTE	lsb;
		} v;
		UWORD	w;
	} U;
};
typedef struct wordv WORDV;

#define MSB(x)	((x).U.v.msb)
#define LSB(x)	((x).U.v.lsb)
#define W(x)	((x).U.w)

#define LLV( a, b)		{ MSW( a ) = MSW( b ); LSW( a ) = LSW( b ); }
#define WWV( a, b)		{ MSB( a ) = MSB( b ); LSB( a ) = LSB( b ); }

#define LV( a, b)		/* Long to LongV */ \
	{ \
		LONGV _x; \
		L( _x ) = (ULONG)b; \
		MSW( a ) = MSW( _x ); \
		LSW( a ) = LSW( _x ); \
	}
#define VL( a, b)		/* LongV to long */ \
	{ \
		LONGV _x; \
		MSW( _x ) = MSW( b ); \
		LSW( _x ) = LSW( b ); \
		(ULONG)a = L( _x ); \
	}
#define WV( a, b)		/* Word to WordV */	\
	{ \
		WORDV _x; \
		L( _x ) = (UWORD)b; \
		MSB( a ) = MSB( _x ); \
		LSB( a ) = LSB( _x ); \
	}
#define VW( a, b)		/* WordV to word */ \
	{ \
		WORDV _x; \
		MSB( _x ) = MSB( b ); \
		LSB( _x ) = LSB( b ); \
		(UWORD)a = W( _x ); \
	}

#define BIT(x)		(1 << (x))
