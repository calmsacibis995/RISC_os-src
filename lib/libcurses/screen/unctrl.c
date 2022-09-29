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
#ident	"$Header: unctrl.c,v 1.2.1.2 90/05/10 02:26:03 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * define unctrl codes for each character
 *
 */

/* LINTLIBRARY */
char	*_unctrl[]	= {	/* unctrl codes for ttys		*/
	"^@", "^A", "^B", "^C", "^D", "^E", "^F", "^G", "^H", "^I", "^J", "^K",
	"^L", "^M", "^N", "^O", "^P", "^Q", "^R", "^S", "^T", "^U", "^V", "^W",
	"^X", "^Y", "^Z", "^[", "^\\", "^]", "^~", "^_",
	" ", "!", "\"", "#", "$",  "%", "&", "'", "(", ")", "*", "+", ",", "-",
	".", "/", "0",  "1", "2",  "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">",  "?", "@",  "A", "B", "C", "D", "E", "F", "G", "H", "I",
	"J", "K", "L",  "M", "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W",
	"X", "Y", "Z",  "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d", "e",
	"f", "g", "h",  "i", "j",  "k", "l", "m", "n", "o", "p", "q", "r", "s",
	"t", "u", "v",  "w", "x",  "y", "z", "{", "|", "}", "~", "^?"
#ifdef DEBUG
	,
	"M-^@", "M-^A", "M-^B", "M-^C", "M-^D", "M-^E", "M-^F", "M-^G",
	"M-^H", "M-^I", "M-^J", "M-^K", "M-^L", "M-^M", "M-^N", "M-^O",
	"M-^P", "M-^Q", "M-^R", "M-^S", "M-^T", "M-^U", "M-^V", "M-^W",
	"M-^X", "M-^Y", "M-^Z", "M-^[", "M-^\\", "M-^]", "M-^~", "M-^_",
	"M- ", "M-!", "M-\"", "M-#", "M-$", "M-%", "M-&", "M-'", 
	"M-(", "M-)", "M-*", "M-+", "M-,", "M--", "M-.", "M-/",
	"M-0", "M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "M-7",
	"M-8", "M-9", "M-:", "M-;", "M-<", "M-=", "M->", "M-?",
	"M-@", "M-A", "M-B", "M-C", "M-D", "M-E", "M-F", "M-G",
	"M-H", "M-I", "M-J", "M-K", "M-L", "M-M", "M-N", "M-O",
	"M-P", "M-Q", "M-R", "M-S", "M-T", "M-U", "M-V", "M-W",
	"M-X", "M-Y", "M-Z", "M-[", "M-\\", "M-]", "M-^", "M-_",
	"M-`", "M-a", "M-b", "M-c", "M-d", "M-e", "M-f", "M-g",
	"M-h", "M-i", "M-j", "M-k", "M-l", "M-m", "M-n", "M-o",
	"M-p", "M-q", "M-r", "M-s", "M-t", "M-u", "M-v", "M-w",
	"M-x", "M-y", "M-z", "M-{", "M-|", "M-}", "M-~", "M-^?"
#endif /* DEBUG */
};
