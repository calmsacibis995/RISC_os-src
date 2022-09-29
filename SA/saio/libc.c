#ident "$Header: libc.c,v 1.4 90/01/17 11:50:07 paultong Locked $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * libc.c -- routines extracted from c library
 */

#include "sys/types.h"
#include "netinet/in.h"
#include "saio/ctype.h"

/* @(#)ctype_.c	4.2 (Berkeley) 7/8/83 */

char _ctype_[] = {
	0,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_S,	_S,	_S,	_S,	_S,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_S|_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_N,	_N,	_N,	_N,	_N,	_N,	_N,	_N,
	_N,	_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
	_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C
};

/* @(#)index.c	4.1 (Berkeley) 12/21/80 */
/*
 * Return the ptr in sp at which the character c appears;
 * NULL if not found
 */

#define	NULL	0

char *
index(sp, c)
register char *sp, c;
{
	do {
		if (*sp == c)
			return(sp);
	} while (*sp++);
	return(NULL);
}

/* @(#)strcat.c	4.1 (Berkeley) 12/21/80 */
/*
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * Return s1.
 */

char *
strcat(s1, s2)
register char *s1, *s2;
{
	register char *os1;

	os1 = s1;
	while (*s1++)
		;
	--s1;
	while (*s1++ = *s2++)
		;
	return(os1);
}

#ifdef MOVED_TO_LIBASM
/* @(#)strcmp.c	4.1 (Berkeley) 12/21/80 */
/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strcmp(s1, s2)
register char *s1;
register char *s2;
{

	while (*s1 == *s2++)
		if (*s1++ == '\0')
			return(0);
	return(*s1 - *--s2);
}

/* @(#)strcpy.c	4.1 (Berkeley) 10/5/82 */
/*
 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 */

char *
strcpy(s1, s2)
register char *s1, *s2;
{
	register char *os1;

	os1 = s1;
	while (*s1++ = *s2++)
		;
	return(os1);
}
#endif MOVED_TO_LIBASM

/* @(#)strlen.c	4.1 (Berkeley) 12/21/80 */
/*
 * Returns the number of
 * non-NULL bytes in string argument.
 */

strlen(s)
register char *s;
{
	register n;

	n = 0;
	while (*s++)
		n++;
	return(n);
}

/* @(#)strncmp.c	4.1 (Berkeley) 12/21/80 */
/*
 * Compare strings (at most n bytes):  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strncmp(s1, s2, n)
register char *s1, *s2;
register n;
{

	while (--n >= 0 && *s1 == *s2++)
		if (*s1++ == '\0')
			return(0);
	return(n<0 ? 0 : *s1 - *--s2);
}

/* @(#)strncpy.c	4.1 (Berkeley) 12/21/80 */
/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */

char *
strncpy(s1, s2, n)
register char *s1, *s2;
{
	register i;
	register char *os1;

	os1 = s1;
	for (i = 0; i < n; i++)
		if ((*s1++ = *s2++) == '\0') {
			while (++i < n)
				*s1++ = '\0';
			return(os1);
		}
	return(os1);
}

/* @(#)strncat.c	4.1 (Berkeley) 12/21/80 */
/*
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * At most n characters are moved.
 * Return s1.
 */

char *
strncat(s1, s2, n)
register char *s1, *s2;
register n;
{
	register char *os1;

	os1 = s1;
	while (*s1++)
		;
	--s1;
	while (*s1++ = *s2++)
		if (--n < 0) {
			*--s1 = '\0';
			break;
		}
	return(os1);
}

/*	inet_makeaddr.c	4.3	82/11/14	*/

/*
 * Formulate an Internet address from network + host.  Used in
 * building addresses stored in the ifnet structure.
 */
struct in_addr
inet_makeaddr(net, host)
	register int net, host;
{
	u_long addr;

	if (net < 128)
		addr = (net << IN_CLASSA_NSHIFT) | (host&IN_CLASSA_HOST);
	else if (net < 65536)
		addr = (net << IN_CLASSB_NSHIFT) | (host&IN_CLASSB_HOST);
	else
		addr = (net << IN_CLASSC_NSHIFT) | (host&IN_CLASSC_HOST);
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}

/*
 * Internet network address interpretation routine.
 * The library routines call this routine to interpret
 * network numbers.
 */
u_long
inet_network(cp)
	register char *cp;
{
	register u_long val, base, n;
	register char c;
	u_long parts[4], *pp = parts;
	register int i;

again:
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		if (pp >= parts + 4)
			return (-1);
		*pp++ = val, cp++;
		goto again;
	}
	if (*cp && !isspace(*cp))
		return (-1);
	*pp++ = val;
	n = pp - parts;
	if (n > 4)
		return (-1);
	for (val = 0, i = 0; i < n; i++) {
		val <<= 8;
		val |= parts[i] & 0xff;
	}
	return (val);
}

/*
 * Return the local network address portion of an
 * internet address; handles class a/b/c network
 * number formats.
 */
inet_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i))
		return ((i)&IN_CLASSA_HOST);
	else if (IN_CLASSB(i))
		return ((i)&IN_CLASSB_HOST);
	else
		return ((i)&IN_CLASSC_HOST);
}

/*
 * Internet address interpretation routine.
 * All the network library routines call this
 * routine to interpret entries in the data bases
 * which are expected to be an address.
 * The value returned is in network order.
 */
struct in_addr
inet_addr(cp)
	register char *cp;
{
	register u_long val, base, n;
	register char c;
	u_long parts[4], *pp = parts;
	struct in_addr in_addr;

again:
	/*
	 * Collect number up to ``.''.
	 * Values are specified as for C:
	 * 0x=hex, 0=octal, other=decimal.
	 */
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		/*
		 * Internet format:
		 *	a.b.c.d
		 *	a.b.c	(with c treated as 16-bits)
		 *	a.b	(with b treated as 24 bits)
		 */
		if (pp >= parts + 4) {
			in_addr.s_addr = -1;
			return (in_addr);
		}
		*pp++ = val, cp++;
		goto again;
	}
	/*
	 * Check for trailing characters.
	 */
	if (*cp && !isspace(*cp)) {
		in_addr.s_addr = -1;
		return (in_addr);
	}
	*pp++ = val;
	/*
	 * Concoct the address according to
	 * the number of parts specified.
	 */
	n = pp - parts;
	switch (n) {

	case 1:				/* a -- 32 bits */
		val = parts[0];
		break;

	case 2:				/* a.b -- 8.24 bits */
		val = (parts[0] << 24) | (parts[1] & 0xffffff);
		break;

	case 3:				/* a.b.c -- 8.8.16 bits */
		val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
			(parts[2] & 0xffff);
		break;

	case 4:				/* a.b.c.d -- 8.8.8.8 bits */
		val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
		      ((parts[2] & 0xff) << 8) | (parts[3] & 0xff);
		break;

	default:
		in_addr.s_addr = -1;
		return (in_addr);
	}
	val = htonl(val);
	in_addr.s_addr = val;
	return (in_addr);
}

/*
 * Return the network number from an internet
 * address; handles class a/b/c network #'s.
 */
inet_netof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i))
		return (((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT);
	else if (IN_CLASSB(i))
		return (((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
	else
		return (((i)&IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
}

unsigned _digit();

/*
 * atob -- convert ascii to binary.  Accepts all C numeric formats.
 */
char *
atob(cp, iptr)
register char *cp;
int *iptr;
{
	register unsigned base = 10;
	register int value = 0;
	register unsigned d;
	int minus = 0;
	int overflow = 0;

	*iptr = 0;
	if (!cp)
		return(0);

	while (isspace(*cp))
		cp++;

	while (*cp == '-') {
		cp++;
		minus = !minus;
	}

	/*
	 * Determine base by looking at first 2 characters
	 */
	if (*cp == '0') {
		switch (*++cp) {
		case 'X':
		case 'x':
			base = 16;
			cp++;
			break;

		case 'B':	/* a frill: allow binary base */
		case 'b':
			base = 2;
			cp++;
			break;
		
		default:
			base = 8;
			break;
		}
	}

	while ((d = _digit(*cp)) < base) {
		if ((unsigned)value > ((unsigned)-1/base))
			overflow++;
		value *= base;
		if ((unsigned)value > (unsigned)(-1 - d))
			overflow++;
		value += d;
		cp++;
	}

	if (overflow || (value == 0x80000000 && minus))
		printf("WARNING: conversion overflow\n");
	if (minus)
		value = -value;

	*iptr = value;
	return(cp);
}

/*
 * _digit -- convert the ascii representation of a digit to its
 * binary representation
 */
unsigned
_digit(c)
char c;
{
	unsigned d;

	if (isdigit(c))
		d = c - '0';
	else if (isalpha(c)) {
		if (isupper(c))
			c = tolower(c);
		d = c - 'a' + 10;
	} else
		d = 999999; /* larger than any base to break callers loop */

	return(d);
}

/* @(#)getenv.c	4.1 (Berkeley) 12/21/80 */
/*
 *	getenv(name)
 *	returns ptr to value associated with name, if any, else NULL
 */
#define NULL	0
extern	char **environ;
char	*nvmatch();

char *
getenv(name)
char *name;
{
	register char **p = environ;
	register char *v;

	if (p)
		while (*p != NULL)
			if ((v = nvmatch(name, *p++)) != NULL)
				return(v);
	return(NULL);
}

/*
 *	s1 is either name, or name=value
 *	s2 is name=value
 *	if names match, return value of s2, else NULL
 *	used for environment searching: see getenv
 */

static char *
nvmatch(s1, s2)
register char *s1, *s2;
{

	while (*s1 == *s2++)
		if (*s1++ == '=')
			return(s2);
	if (*s1 == '\0' && *(s2-1) == '=')
		return(s2);
	return(NULL);
}
