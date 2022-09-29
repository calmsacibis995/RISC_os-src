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
/* $Header: mkentry.h,v 1.4.2.2 90/05/09 16:49:24 wje Exp $ */

int basetype,curclass,curtype,nargs,_par_lineno,retval;
int stringsused;
int curnum;
int newstackframe;
int default_signed;

int localspace;

extern int errno;
 
extern char lastname[],tokenbuf[],lasttoken[];

char *ipfnm,*opfnm;
char *progname;
int nfuncs;
int extendfloats,onecase;

#define N_ISWRP 0x42
#define WRAPPING_C 3
#define WRAPPING_FORTRAN 1
#define MAXFNAME 32
#define MAXCNAME 0x100

#ifdef MKF2C
#define SRCLANG_IDMAX MAXFNAME
#define TGTLANG_IDMAX MAXCNAME
#else
#define SRCLANG_IDMAX MAXCNAME
#define TGTLANG_IDMAX MAXFNAME
#endif

struct ftn_s {
	char srcentry[SRCLANG_IDMAX+2];
	char tgtentry[TGTLANG_IDMAX+2];
	int  class,type,nargs;} ;
	
struct ftn_s ftn;

char *strcpy();

#define GENREG(reg) (reg)
#define FLTREG(reg) (-(reg))
#define ISFLTREG(reg) ((reg) < 0)
#define FIRST_ARGGREG 4
#define LAST_ARGGREG 7
#define FIRST_ARGFREG 12
#define LAST_ARGFREG 15
#define NARGREGS (LAST_ARGGREG - FIRST_ARGGREG + 1)
struct loc_s {
	/* parameter's register location, if passed in register */
#ifdef mips
	signed char reg;
#else
	char reg;
#endif
	unsigned char flags;
	/* offset from sp of the parameter's home location */
	unsigned short offset;
};

/* bits for the loc flags word. */
#define L_ARGHOMED 0x80

struct op_s {
	struct loc_s arg;
	/* string_len is for char strings only */
	struct loc_s string_len;
	/* string pointer copy is for char strings (value parameters 
	   (char *)) only.  A copy of the pointer to the space
	   calloc'd is saved so that it can be free'd
	*/
	int strptrcopy_offset;
} ;

typedef struct {
	char name[TGTLANG_IDMAX+2];
	int class,type;
	unsigned flags;
	int relstring;
	struct op_s source;
	struct op_s dest;
	} arg;

#define MAXPARMS 64
arg arglist[MAXPARMS];

/* bit definitions for the flags word */
#define DEREFERENCE 1

