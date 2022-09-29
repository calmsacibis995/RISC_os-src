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
#ident	"$Header: kopt.c,v 1.1.2.2 90/05/07 18:43:12 wje Exp $"

#include <stdio.h>
#include <machine/cpu.h>
#include <ctype.h>
#include <machine/debug.h>

extern int errno;

struct string_tab {
	char	*st_str;
	int	st_int;
};

struct string_tab op_tab[] = {
	{ "get",	KOPT_GET	},
	{ "set",	KOPT_SET	},
	{ "bis",	KOPT_BIS	},
	{ "bic",	KOPT_BIC	},
	{ 0,		0	}
};

struct string_tab val_tab[] = {
	{ 0,		0	}
};

char *atob();

main(argc, argv)
char **argv;
{
	int op;
	int val;
	int oldval;
	char *cp;

	if (argc < 3 || argc > 4) {
		fprintf(stderr, "Usage: %s get|set|bis|bic KERNELOPT [VAL]\n",
		    argv[0]);
		exit(1);
	}

	if ((op = str_lookup(op_tab, argv[1])) == -1) {
		fprintf(stderr, "%s: %s bad option\n", argv[0], argv[1]);
		exit(2);
	}

	switch (op) {
	
	case KOPT_GET:
		if (argc != 3) {
			fprintf(stderr, "%s: get doesn't take val\n",
			    argv[0]);
			exit(3);
		}
		break;

	case KOPT_SET:
	case KOPT_BIS:
	case KOPT_BIC:
		if (argc != 4) {
			fprintf(stderr, "%s: %s takes value arg\n",
			    argv[0], argv[1]);
			exit(4);
		}
		if (isalpha(argv[3][0])) {
			if ((val = str_lookup(val_tab, argv[3])) == -1) {
				fprintf(stderr, "%s: %s unknown string val\n",
				    argv[0], argv[3]);
				exit(5);
			}
		} else {
			if (*(cp = atob(argv[3], &val))) {
				fprintf(stderr,
				    "%s: bad char in value \'%c\' (0x%x)\n",
				    argv[0], *cp, *cp);
				exit(1);
			}
			fprintf(stderr, "%s: \'%s\'ing %s to %d 0x%x\n",
			    argv[0], argv[1], argv[2], val, val);
		}
		break;

	default:
		fprintf(stderr, "%s: internal bug\n", argv[0]);
		exit(6);
	}

	errno = 0;
	oldval = kopt(argv[2], val, op);
	if (errno)
		perror("kopt");
	else
		fprintf(stderr, "oldval = %d 0x%x\n", oldval, oldval);
}

str_lookup(stp, str)
struct string_tab *stp;
char *str;
{
	for (; stp->st_str; stp++)
		if (strcmp(stp->st_str, str) == 0)
			return(stp->st_int);
	return(-1);
}

unsigned digit();

/*
 * atob -- convert ascii to binary.  Accepts all C numeric formats.
 */
char *
atob(cp, iptr)
char *cp;
int *iptr;
{
	int minus = 0;
	register int value = 0;
	unsigned base = 10;
	unsigned d;

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

	while ((d = digit(*cp)) < base) {
		value *= base;
		value += d;
		cp++;
	}

	if (minus)
		value = -value;

	*iptr = value;
	return(cp);
}

/*
 * digit -- convert the ascii representation of a digit to its
 * binary representation
 */
unsigned
digit(c)
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
