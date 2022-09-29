#ident "$Header: stringlist.c,v 1.2 90/01/17 08:47:38 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * stringlist.c -- string_list manipulation routines
 *	string_lists consist of a list of pointers into an byte array
 *	the list is null terminated
 *	(INVARIANT: strptrs[strcnt] == NULL)
 */

#include "saio/stringlist.h"

void delete_str();

/*
 * init_str -- initialize string_list
 */
void
init_str(slp)
register struct string_list *slp;
{
	slp->strp = slp->strbuf;
	slp->strcnt = 0;
	slp->strptrs[0] = 0;
}

/*
 * replace_str -- replace environment style string in string list
 */
void
replace_str(name, value, slp)
char *name, *value;
struct string_list *slp;
{
	delete_str(name, slp);
	new_str2(name, value, slp);
}

/*
 * delete_str -- delete environment style string from string list
 */
void
delete_str(name, slp)
char *name;
struct string_list *slp;
{
	register char **np;
	register int namelen;

	namelen = strlen(name);
	for (np = slp->strptrs;
	    np < &slp->strptrs[slp->strcnt]; np++)
		if (strncmp(name, *np, namelen) == 0 && (*np)[namelen] == '=') {
			/*
			 * found it, compress list
			 */
			while (*np = *(np+1))
				np++;
			slp->strcnt--;
			return;
		}
	return;	/* no error if its not there */
}

/*
 * find_str -- search environment style strings
 */
char *
find_str(name, slp)
char *name;
struct string_list *slp;
{
	register char **np;
	register int namelen;

	namelen = strlen(name);
	for (np = slp->strptrs; np < &slp->strptrs[slp->strcnt]; np++) 
		if (strncmp(*np, name, namelen) == 0 && (*np)[namelen] == '=')
			return(*np);
	return(0);
}

/*
 * new_str1 -- add new string to string list
 */
new_str1(strp, slp)
char *strp;
register struct string_list *slp;
{
	register int len;

	if (slp->strcnt >= MAXSTRINGS - 1) {
		printf("too many strings\n");
		return(-1);
	}

	len = strlen(strp) + 1;
	if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
		garbage_collect(slp);
		if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
			printf("no space left in string table\n");
			return(-1);
		}
	}

	slp->strptrs[slp->strcnt++] = slp->strp;
	slp->strptrs[slp->strcnt] = 0;
	strcpy(slp->strp, strp);
	slp->strp += len;
	return(0);
}

/*
 * new_str2 -- add environment style string to string list
 */
new_str2(name, value, slp)
char *name;
char *value;
register struct string_list *slp;
{
	register int len;

	if (slp->strcnt >= MAXSTRINGS - 1) {
		printf("too many strings\n");
		return(-1);
	}

	len = strlen(name) + strlen(value) + 2;	/* 2:  '=' and '\0' */
	if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
		garbage_collect(slp);
		if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
			printf("no space left in string table\n");
			return(-1);
		}
	}

	slp->strptrs[slp->strcnt++] = slp->strp;
	slp->strptrs[slp->strcnt] = 0;
	strcpy(slp->strp, name);
	strcat(slp->strp, "=");
	strcat(slp->strp, value);
	slp->strp += len;
	return(0);
}

/*
 * set_str -- set particular string in string table
 */
set_str(strp, index, slp)
char *strp;
int index;
register struct string_list *slp;
{
	register int len;

	if (index >= slp->strcnt) {
		printf("no such string\n");
		return(-1);
	}

	len = strlen(strp) + 1;
	if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
		garbage_collect(slp);
		if (slp->strp + len >= &slp->strbuf[STRINGBYTES]) {
			printf("no space left in string table\n");
			return(-1);
		}
	}

	slp->strptrs[index] = slp->strp;
	strcpy(slp->strp, strp);
	slp->strp += len;
	return(0);
}

delete_strnum(num, slp)
int num;
struct string_list *slp;
{
	register char **np;

	if (num < 0 || num >= slp->strcnt)
		return(1);	/* bad string number */
	for (np = &slp->strptrs[num]; *np = *(np+1); np++)
		continue;
	return(0);
}
	

/*
 * garbage_collect -- compact string_list
 */
garbage_collect(slp)
struct string_list *slp;
{
	struct string_list tmplist;
	register char **np;

	init_str(&tmplist);
	for (np = slp->strptrs; *np; np++)
		new_str1(*np, &tmplist);
	init_str(slp);
	for (np = tmplist.strptrs; *np; np++)
		new_str1(*np, slp);
}

/*
 * csu.mips.s copys argv and environ here so they arent overwitten
 * when doing latter execs
 */
struct string_list _argv_strings;
struct string_list _environ_strings;

char **
_copystrings(wp, slp)
char **wp;
struct string_list *slp;
{
	init_str(slp);
	for (; wp && *wp; wp++)
		new_str1(*wp, slp);
	return(slp->strptrs);
}
