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
/* $Header: macro.h,v 1.1.1.2 90/05/09 18:01:47 wje Exp $ */

/*
 * General macro function definitions
 *
 * Author: Peter J. Nicklin
 */

int strcmp();				/* string comparison */

#undef CHDIR
#define CHDIR(d) \
	(chdir(d) == 0)			/* change directory */

#undef DOTDIR
#define DOTDIR(dp) \
	(dp->d_name[0] == '.' && dp->d_name[1] == '\0')
					/* current directory? */
#undef DOTDOTDIR
#define DOTDOTDIR(dp) \
	(dp->d_name[0] == '.' && dp->d_name[1] == '.' && dp->d_name[2] == '\0')
					/* parent directory? */
#undef EQUAL
#define EQUAL(s1,s2) \
	(strcmp(s1,s2) == 0)		/* string comparison */

#undef MIN
#define MIN(a,b) \
	(((a) < (b)) ? (a) : (b))	/* minimum of two values */

#undef MAX
#define MAX(a,b) \
	(((a) > (b)) ? (a) : (b))	/* maximum of two values */

#undef WHITESPACE
#define WHITESPACE(c) \
	(c == ' ' || c == '\t')		/* unseen space in a file */
