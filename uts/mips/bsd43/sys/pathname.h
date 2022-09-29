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
/* $Header: pathname.h,v 1.7.1.2 90/05/10 04:53:42 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Pathname structure.
 * System calls which operate on path names gather the
 * pathname from system call into this structure and reduce
 * it by peeling off translated components.  If a symbolic
 * link is encountered the new pathname to be translated
 * is also assembled in this structure.
 */
struct bsd43_(pathname) {
	char	*pn_buf;		/* underlying storage */
	char	*pn_path;		/* remaining pathname */
	u_int	pn_pathlen;		/* remaining length */
};

#define BSD43_PN_STRIP 0x00		/* Strip next component off pn */
#define BSD43_PN_PEEK	0x01  		/* Only peek at next pn component */
#define pn_peekcomponent(PNP, COMP) (pn_getcomponent(PNP, COMP, BSD43_PN_PEEK))
#define pn_stripcomponent(PNP, COMP) (pn_getcomponent(PNP, COMP, BSD43_PN_STRIP))

#define	bsd43_pn_peekchar(PNP)	((PNP)->pn_pathlen>0?*((PNP)->pn_path):0)
#define bsd43_pn_pathleft(PNP)	((PNP)->pn_pathlen)
#define bsd43_pn_getpath(PNP)		((PNP)->pn_path)
#define bsd43_pn_copy(PNP1, PNP2)	(pn_set(PNP2, bsd43_pn_getpath(PNP1)))

extern int	bsd43_(pn_alloc)();	/* allocat buffer for pathname */
extern int	bsd43_(pn_get)();	/* allocate buf and copy path into it*/
#ifdef notneeded
extern int	bsd43_(pn_getchar)();		/* get next pathname char */
#endif
extern int	bsd43_(pn_set)();		/* set pathname to string */
extern int	bsd43_(pn_combine)();	/* combine to pathnames (for symlink)*/
extern int	pn_getcomponent(); /* get next component of pathname */
extern void	bsd43_(pn_skipslash)();	/* skip over slashes */
extern void	bsd43_(pn_free)();	/* free pathname buffer */
extern int	bsd43_(pn_append)();	/* Append string to pathname */
extern int	bsd43_(pn_getlast)();	/* Get last component of pathname */ 

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define PN_STRIP	BSD43_PN_STRIP
#   define PN_PEEK	BSD43_PN_PEEK
/* #   define pn_peekcomponent	bsd43_pn_peekcomponent */
/* #   define pn_stripcomponent	bsd43_pn_stripcomponent */
#   define pn_peekchar	bsd43_pn_peekchar
#   define pn_pathleft	bsd43_pn_pathleft
#   define pn_getpath	bsd43_pn_getpath
#   define pn_copy	bsd43_pn_copy
#endif SYSTYPE_BSD
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

