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
/* $Header: lerror.h,v 1.5.2.3 90/05/09 15:20:25 wje Exp $ */
/* defines for lint message buffering scheme 
 * be sure to include lerror.h before lmanifest
 */
# if pdp11
#	define TYSZ 2500
#	define FSZ 80
# else
#	define TYSZ 3500	/* should be 2500 temp fix for mjs */
#	define FSZ  1000		/* used to be 80 */
# endif

/* number of chars in NAME, and filename */
#ifndef FLEXNAMES
#	define LCHNM 8
#	define LFNM  30
#else
#	define LFNM 16		/* Only used for header file msg buffering */
#endif

#define	NUMBUF	24
#define	MAXBUF	500

# define PLAINTY	0
# define STRINGTY	01
# define DBLSTRTY	02
# define CHARTY		04
# define NUMTY		010

# define SIMPL		020
# define WERRTY		0100
# define UERRTY		0

# define TMPDIR	"/usr/tmp"
# define TMPLEN	sizeof( TMPDIR )

# define NOTHING	0
# define ERRMSG	01
# define FATAL		02
# define CCLOSE		04
# define HCLOSE		010

struct hdritem {
	char	hname[ LFNM ];
	char	sname[ LFNM ];
	int	hcount;
};

# define HDRITEM	struct hdritem
# define NUMHDRS	100

struct crecord {
    int	code;
    int	lineno;
    union {
#ifdef FLEXNAMES
	char	*name1;
#else
	char	name1[LCHNM];
#endif
	char	char1;
	int	number;
    } arg1;
#ifdef FLEXNAMES
    char	*name2;
#else
    char	name2[LCHNM];
#endif
};

# define CRECORD	struct crecord
# define CRECSZ		sizeof ( CRECORD )

# define OKFSEEK	0
# define PERMSG		((long) CRECSZ * MAXBUF )

struct hrecord {
    int		msgndx;
    int		code;
    int		lineno;
    union {
#ifdef FLEXNAMES
	char	*name1;
#else
	char	name1[ LCHNM ];
#endif
	char	char1;
	int	number;
    } arg1;
#ifdef FLEXNAMES
    char	*name2;
#else
    char	name2[ LCHNM ];
#endif
};

# define HRECORD	struct hrecord
# define HRECSZ		sizeof( HRECORD )

enum boolean { true, false };

/* for pass2 in particular */

# define NUM2MSGS	12
# define MAX2BUF	500

struct c2record {
#ifdef FLEXNAMES
    char	*name;
#else
    char	name[ LCHNM ];
#endif
    int		number;
    int		file1;
    int		line1;
    int		file2;
    int		line2;
};

# define C2RECORD	struct c2record
# define C2RECSZ	sizeof( C2RECORD )
# define PERC2SZ	((long) C2RECSZ * MAX2BUF )

# define NMONLY	1
# define NMFNLN	2
# define NM2FNLN	3
# define ND2FNLN	4
