#ident "$Header: calc.c,v 1.3 90/01/11 13:43:56 huang Exp $"
/* $Copyright$ */

/*
 * $Log:	calc.c,v $
 * Revision 1.3  90/01/11  13:43:56  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/03/12  20:03:59  hawkes
 * Add  return(0)  to nop() to avoid unintended exit of _calc().
 * 
 * Revision 1.1  87/08/18  13:52:22  mdove
 * Initial revision
 * 
 * Revision 1.2  86/06/12  09:54:14  mcneal
 * functionality(sp?) is complete.  should enlarge the stack and write
 * the man page.
 * 
 * Revision 1.1  86/06/11  21:47:26  mcneal
 * Initial revision
 * 
 */
#include "dbgmon/calc.h"

/*
 * Globals
 */
static int cbase = 0x10;
static unsigned int r0;
static unsigned int stack[STKSIZ], *sp;

static int quit(), push(), bin_base(), dec_base(), oct_base(), hex_base(),
	com(), dump(), nop();

static struct func_t func_table[] = {
	{ '?',		dump },
	{ 'q',		quit },
	{ '\r',		nop },
	{ '\n',		nop },
	{ CTRL(b),	bin_base },
	{ CTRL(d),	dec_base },
	{ CTRL(o),	oct_base },
	{ CTRL(h),	hex_base },
	{ '+',		com },
	{ '-',		com },
	{ '*',		com },
	{ '/',		com },
	{ '<',		com },
	{ '>',		com },
	{ '%',		com },
	{ '&',		com },
	{ '|',		com },
	{ '^',		com },
};
static int fentrys = sizeof(func_table) / sizeof(struct func_t);

_calc()
{
	register struct func_t *f;
	register cc, nextclr, lr0, rt;
	int c;

	nextclr = 1;
	sp  = &stack[0];
	while (1) {
		c = getchar ();
		for (f = &func_table[0]; f < &func_table[fentrys]; f++)
			if (f->f_inp == c) {
				if (!nextclr)
					push();
				if ((*f->f_func)(c))
					return (0);
				pbase (r0);
				nextclr = 1;
				break;
			}
		
		/*
		 * If f < &func_table[fentrys] then don't process
		 * the character any further.
		 */
		if (f < &func_table[fentrys])
			continue;
		
		if (nextclr) {
			printf (CLRLINE);
			nextclr = 0;
			r0 = 0;
		}
		if ((c >= '0') && (c <= '9'))
			if (cbase > (c - '0'))
				r0 = r0 * cbase + c - '0';
			else 
				continue;
		if ((c >= 'A') && (c <= 'F'))
			c = c - 'A' + 'a';
		if ((c >= 'a') && (c <= 'f'))
			if (cbase > (c - 'a' + 10))
				r0 = r0 * cbase + c - 'a' + 10;
			else
				continue;
		putchar (c);
	}
}

static
quit()
{
	printf ("\n");
	return (1);
}

static
bin_base()
{
	cbase = 2;
	return (0);
}

static
hex_base()
{
	cbase = 16;
	return (0);
}

static
oct_base()
{
	cbase = 8;
	return (0);
}

static
dec_base()
{
	cbase = 10;
	return (0);
}

static
pbase (r2)
	register unsigned int r2;
{
	unsigned int mask, i, j, pest;
	char buf[128], *bp;

	printf (CLRLINE);
	if (cbase == 2) {
		bp = buf;
		mask = 1 << 31;
		while (mask) {
			*bp++ = (r2 & mask) ? '1' : '0';
			mask >>= 1;
		}
		*bp = '\0';
		printf ("%s\r", buf);
	}
	else {
		switch (cbase) {
			case 10:
				printf ("%u\r", r2);
				break;
			
			case 8:
				printf ("%o\r", r2);
				break;
			
			case 16:
				printf ("%x\r", r2);
				break;
		}
	}
	return (0);
}

static
com(t)
	register t;
{
	register int r1, r2;

	r2 = pop();
	r1 = *sp;
	switch (t) {
		case '+':
			r0 = r1 + r2;
			break;
		case '-':
			r0 = r1 - r2;
			break;
		case '*':
			r0 = r1 * r2;
			break;
		case '/':
			r0 = r1 / r2;
			break;
		case '<':
			r0 = r1 << r2;
			break;
		case '>':
			r0 = r1 >> r2;
			break;
		case '%':
			r0 = r1 % r2;
			break;
		case '&':
			r0 = r1 & r2;
			break;
		case '|':
			r0 = r1 | r2;
			break;
		case '^':
			r0 = r1 ^ r2;
			break;
		default:
			printf ("internal error, unknow operator '%c'\r\n", t);
			return (1);
	}
	*sp = r0;
	return (0);
}

static
push()
{
	if (sp >= &stack[STKSIZ - 1])
		sp = &stack[0];
	*(++sp) = r0;
	return (0);
}

static
pop()
{
	if (sp <= &stack[0])
		return (0);
	else
		return (*sp--);
}

static
dump()
{
	register int i;

	printf ("\r\nbase = 0x%x\r\n", cbase);
	printf ("r0 = 0x%x	&stack = 0x%x, sp = 0x%x\r\n\r\n", 
		r0, stack, sp);
	for (i = 0; i < STKSIZ; i++) {
		if (sp == &stack[i])
			printf ("	sp -->	");
		else
			printf ("		");
		printf ("  %d   = 0x%x\r\n", i, stack[i]);
	}
	printf ("\r\n\r\n");
	return (0);
}

static
nop()
{
	return (0);
}
