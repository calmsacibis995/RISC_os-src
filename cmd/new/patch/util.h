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
/* $Header: util.h,v 1.1.2.2 90/05/09 18:11:17 wje Exp $ */

/* and for those machine that can't handle a variable argument list */

#ifdef CANVARARG

#define say1 say
#define say2 say
#define say3 say
#define say4 say
#define ask1 ask
#define ask2 ask
#define ask3 ask
#define ask4 ask
#define fatal1 fatal
#define fatal2 fatal
#define fatal3 fatal
#define fatal4 fatal

#else /* hope they allow multi-line macro actual arguments */

#ifdef lint

#define say1(a) say(a, 0, 0, 0)
#define say2(a,b) say(a, (b)==(b), 0, 0)
#define say3(a,b,c) say(a, (b)==(b), (c)==(c), 0)
#define say4(a,b,c,d) say(a, (b)==(b), (c)==(c), (d)==(d))
#define ask1(a) ask(a, 0, 0, 0)
#define ask2(a,b) ask(a, (b)==(b), 0, 0)
#define ask3(a,b,c) ask(a, (b)==(b), (c)==(c), 0)
#define ask4(a,b,c,d) ask(a, (b)==(b), (c)==(c), (d)==(d))
#define fatal1(a) fatal(a, 0, 0, 0)
#define fatal2(a,b) fatal(a, (b)==(b), 0, 0)
#define fatal3(a,b,c) fatal(a, (b)==(b), (c)==(c), 0)
#define fatal4(a,b,c,d) fatal(a, (b)==(b), (c)==(c), (d)==(d))

#else /* lint */
    /* if this doesn't work, try defining CANVARARG above */
#define say1(a) say(a, Nullch, Nullch, Nullch)
#define say2(a,b) say(a, b, Nullch, Nullch)
#define say3(a,b,c) say(a, b, c, Nullch)
#define say4 say
#define ask1(a) ask(a, Nullch, Nullch, Nullch)
#define ask2(a,b) ask(a, b, Nullch, Nullch)
#define ask3(a,b,c) ask(a, b, c, Nullch)
#define ask4 ask
#define fatal1(a) fatal(a, Nullch, Nullch, Nullch)
#define fatal2(a,b) fatal(a, b, Nullch, Nullch)
#define fatal3(a,b,c) fatal(a, b, c, Nullch)
#define fatal4 fatal

#endif /* lint */

/* if neither of the above work, join all multi-line macro calls. */
#endif

EXT char serrbuf[BUFSIZ];		/* buffer for stderr */

char *fetchname();
int move_file();
void copy_file();
void say();
void fatal();
void ask();
char *savestr();
void set_signals();
void ignore_signals();
void makedirs();
