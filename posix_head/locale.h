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
/* $Header: locale.h,v 1.4.1.3 90/05/10 04:08:03 wje Exp $ */

#ifndef	_POSIX_LOCALE_
#define	_POSIX_LOCALE_	1

struct lconv {
 	char *decimal_point;	/* string for decimal point, eg '.' */
	char *thousands_sep;	/* string used to separate groups of */
				/* thousands */
	char *grouping;		/* grouping characteristics for non-monetary */
				/* strings */
	char *int_curr_symbol; 	/* string containing international currency */
				/* symbol for the locale */
	char *currency_symbol;	/* string used to represent currency, eg '$' */
 	char *mon_decimal_point;/* string for monetary decimal point, eg '.' */
	char *mon_thousands_sep;/* string used to separate groups of */
				/* thousands in monetary strings */
	char *mon_grouping;	/* grouping characteristics for monetary */
				/* strings */
	char *positive_sign;	/* string that indicates a positive monetary */
				/* value */
	char *negative_sign;	/* string that indicates a negative monetary */
				/* value */
	char int_frac_digits;	/* number of fractional digits (those after */
				/* the decimal point) to be displayed in an */
				/* internationally formatted monetary 	    */
				/* quantity */
	char frac_digits; 	/* # of fractional digits in monetray string */
	char p_cs_precedes;	/* indicates whether symbol that denotes */
				/* positive monetary value comes before or */
				/* after the numerals of the monetary string */
	char p_sep_by_space;	/* indicates whether a space should appear */
				/* between the symbol that denotes */
				/* positive monetary value and the numerals */
				/* of the monetary string */
	char n_cs_precedes;	/* indicates whether symbol that denotes */
				/* negative monetary value comes before or */
				/* after the numerals of the monetary string */
	char n_sep_by_space;	/* indicates whether a space should appear */
				/* between the symbol that denotes */
				/* negative monetary value and the numerals */
				/* of the monetary string */
	char p_sign_posn;	/* position and formatting of the symbol */
				/* that denotes negatvie monetary value */
	char n_sign_posn;	/* position and formatting of the symbol */
				/* that denotes negatvie monetary value */
};

/* Legal Categories for setlocale() 
 */
#define	LC_ALL		0	/* query/set all info for locale */	

/* The remainder of this list is ordered according to the sequence in 
 * which the standard requires them to be set when the category is LC_ALL
 */
#define	LC_CTYPE	1	/* query/set char-handling info for locale */	
#define	LC_COLLATE	2	/* query/set collation info for locale */	
#define	LC_TIME		3	/* query/set time information for locale */	
#define	LC_NUMERIC	4	/* query/set numeric information for locale */	
#define	LC_MONETARY	5	/* query/set monetary information for locale */	

extern char *setlocale();

#endif	_POSIX_LOCALE_
