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
/* $Header: string.h,v 1.1.2.2 90/05/07 19:55:37 wje Exp $ */

#define STR_DEBUG

char *str_cpy();
char *str_ncpy();
char *str_cat();
char *str_itoa();

#define str_cmp(a, b)	strcmp(a, b)

#ifdef STR_DEBUG
struct string {
	struct string *s_forw;
	struct string *s_back;
	char s_data[1];
};

struct string str_head;

#define str_offset ((unsigned)str_head.s_data - (unsigned)&str_head)
#define str_stos(s) ((struct string *)((unsigned)(s) - str_offset))

char *str_alloc();
int str_free();
#else
#define str_free(s)	free(s)
#define str_alloc(s)	malloc(s)
#endif
