#ident "$Header: stdio.c,v 1.8 90/01/17 09:30:44 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * stdio.c -- standalone stdio routines
 */

#include "saio/saioctl.h"
#include "saio/ctype.h"
#include "sys/stdio.h"
#include "sys/cmn_err.h"
#include "varargs.h"

#undef SPRINTF				/* sprintf() and width fields */

#define	CTRL(x)		('x'&0x1f)

#define	DEL		0x7f
#define	INTR		CTRL(C)
#define	BELL		0x7
#define	LINESIZE	128
#define	PROMPT		"? "

void putchar(), puts(), printf(), putc(), showchar();
static void _printf();
#ifdef SPRINTF
void sprintf();

#define PUTCHAR(c)		if (buf) { *(buf+1) = 0; *buf++ = c; } \
                                else putchar(c)

#define PUTS(str)		if (buf) { register int i; \
                                      for (i=0; i<strlen(str); i++) \
				 	   PUTCHAR(str[i]); } else puts(str)

int zeropad, numpad, firsttime;

#else

#define PUTCHAR(c)		putchar(c)
#define PUTS(str)		puts(str)

#endif SPRINTF

#ifdef SPRINTF
static void prf();
static char *printn();
#else
static void prf(), printn();
#endif

/*
 * getchar -- get a single character from enabled console devices
 * blocks until character available, returns 7 bit ascii code
 */
getchar()
{
	char c;

	read(0, &c, 1);
	return(c & 0x7f);
}

/*
 * getc -- get character only from particular device
 * blocks until character available, returns 8 bits
 */
getc(fd)
int fd;
{
	char c;

	if (read(fd, &c, 1) <= 0)
		return(EOF);
	return(c & 0xff);
}

/*
 * gets -- get an input line from user
 * handles basic line editing
 * buf assumed to be LINESIZE bytes
 */
char *
gets(buf)
char *buf;
{
	char c;
	char *bufp;

	bufp = buf;
	for (;;) {
		c = getchar();

		switch (c) {

		case CTRL(V):		/* quote next char */
			c = getchar();
			/*
			 * Make sure there's room for this character
			 * plus a trailing \n and 0 byte
			 */
			if (bufp < &buf[LINESIZE-3]) {
				*bufp++ = c;
				showchar(c);
			} else
				putchar(BELL);
			break;

		case '\n':
		case '\r':
			putchar('\n');
			*bufp = 0;
			return(buf);

		case CTRL(H):
		case DEL:
			/*
			 * Change this to a hardcopy erase????
			 */
			if (bufp > buf) {
				bufp--;
				putchar(CTRL(H));
				putchar(' ');
				putchar(CTRL(H));
			}
			break;

		case CTRL(U):
			if (bufp > buf) {
				printf("^U\n%s", PROMPT);
				bufp = buf;
			}
			break;

		case '\t':
			c = ' ';	/* simplifies CTRL(H) dramatically */
			/* fall through to default case */

		default:
			/*
			 * Make sure there's room for this character
			 * plus a trailing \n and 0 byte
			 */
			if (isprint(c) && bufp < &buf[LINESIZE-3]) {
				*bufp++ = c;
				putchar(c);
			} else
				putchar(BELL);
			break;
		}
	}
}

static int column;	/* current output column for tab processing */

/*
 * putchar -- put a single character to enabled console devices
 * handles simple character mappings
 */
void
putchar(c)
char c;
{
	extern int stdio_init;

	switch (c) {
	case '\n':
		putchar('\r');
		column = 0;
		break;

	case '\t':
		do {
			putchar(' ');
		} while (column % 8);
		return;

	default:
		if (isprint(c))
			column++;
		break;
	}
	if (!stdio_init || write(1, &c, 1) != 1)
		_errputc(c);		/* attempt to print something */
}

/*
 * putc -- put a character only to a single device
 */
void
putc(c, fd)
char c;
int fd;
{
	write(fd, &c, 1);
}

void
puts(s)
register char *s;
{
	register c;

	if (s == NULL)
		s = "<NULL>";
	while (c = *s++)
		putchar(c);
}

void
cmn_err(severity, fmt, va_alist)
int severity;
char *fmt;
va_dcl
{
	va_list ap;

	va_start(ap);
	switch (severity)
	{
		case CE_CONT:
			_printf(fmt,ap);
			break;
		case CE_NOTE:
			printf("\nNOTICE: ");
			_printf(fmt,ap);
			printf("\n");
			break;
		case CE_WARN:
			printf("\nWARNING: ");
			_printf(fmt,ap);
			printf("\n");
			break;
		case CE_PANIC:
			printf("\nPANIC: ");
			_printf(fmt,ap);
			printf("\n");
			while (1) ;
	}
}

/* fake out stdio calls */
void
printf(fmt, va_alist)
char *fmt;
va_dcl
{
	va_list ap;

	va_start(ap);
	_printf(fmt,ap);
} /* printf */

static void
_printf(fmt,ap)
char *fmt;
va_list ap;
{
	FILE _strbuf;
    	char buf[1024], *s, c;

/*	_strbuf._flag = _IOWRT+_IOSTRG; */
	_strbuf._ptr = buf;
	_strbuf._cnt = 32767;
	_doprnt(fmt, ap, &_strbuf);
	stdio_putc('\0', &_strbuf);
	s = &buf[0];
	while (c = *s++) {
		putchar(c);	/* So tabs expand */
	}
}

#ifdef OLD_PRINTF

/*
 * Scaled down version of C Library printf.
 *
 * One additional format: %b is supported to decode error registers.
 * Usage is:
 *	printf("reg=%b\n", regval, "<base><arg>*");
 * Where <base> is the output base expressed as a control character,
 * e.g. \10 gives octal; \20 gives hex.  Each arg is a sequence of
 * characters, the first of which gives the bit number to be inspected
 * (origin 1), and the next characters (up to a control character, i.e.
 * a character <= 32), give the name of the register.  Thus
 *	printf("reg=%b\n", 3, "\10\2BITTWO\1BITONE\n");
 * would produce output:
 *	reg=3<BITTWO,BITONE>
 *
 * See comments in saioctl.h regarding %r and %R formats
 */
/*VARARGS1*/
void
printf(fmt, va_alist)
char *fmt;
va_dcl
{
	va_list ap;

	va_start(ap);
#ifdef SPRINTF
	prf(0,fmt,ap);
#else
	prf(fmt, ap);
#endif
	va_end(ap);
}

#ifdef SPRINTF
void
sprintf(buf, fmt, va_alist)
char *buf, *fmt;
va_dcl
{
	va_list ap;

	va_start(ap);
	prf(buf,fmt,ap);
	va_end(ap);
}
#endif

void
#ifdef SPRINTF
prf(buf,fmt,adx)
register char *buf, *fmt;
#else
prf(fmt, adx)
register char *fmt;
#endif
va_list adx;
{
	int b, c, i;
	char *s;
	int any;

loop:
	while ((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		PUTCHAR(c);
	}
#ifdef SPRINTF
	numpad = zeropad = firsttime = 0;		/* No justify, no pad */
#endif
again:
	c = *fmt++;
	switch (c) {

	case 'x':
		b = 16;
		goto number;

	case 'd':
		b = -10;
		goto number;

	case 'u':
		b = 10;
		goto number;

	case 'o':
		b = 8;
#ifdef SPRINTF
	case '0':
		if (!firsttime) {
			firsttime = 1;
			zeropad = 1;
			goto again;
		}
	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
		numpad = (numpad * 10) + (c - '0');
		goto again;
#endif
		
number:
#ifdef SPRINTF
		buf = printn(va_arg(adx, int), b, buf);
#else
		printn(va_arg(adx, int), b);
#endif
		break;

	case 'c':
		b = va_arg(adx, int);
		PUTCHAR(b & 0x7f);
		break;

	case 'b':
		b = va_arg(adx, int);
		s = va_arg(adx, char *);
#ifdef SPRINTF
		buf = printn((unsigned)b, *s++, buf);
#else
		printn((unsigned)b, *s++);
#endif
		any = 0;
		if (b) {
			PUTCHAR('<');
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					if (any)
						PUTCHAR(',');
					any = 1;
					for (; (c = *s) > 32; s++)
						PUTCHAR(c);
				} else
					for (; *s > 32; s++)
						;
			}
			PUTCHAR('>');
		}
		break;

	case 'r':
	case 'R':
		b = va_arg(adx, int);
		s = va_arg(adx, char *);
		if (c == 'R') {
			PUTS("0x");
#ifdef SPRINTF
			buf = printn(b, 16, buf);
#else
			printn(b, 16);
#endif
		}
		any = 0;
		if (c == 'r' || b) {
			register struct reg_desc *rd;
			register struct reg_values *rv;
			register unsigned field;

			PUTCHAR('<');
			for (rd = (struct reg_desc *)s; rd->rd_mask; rd++) {
				field = b & rd->rd_mask;
				field = (rd->rd_shift > 0)
				    ? field << rd->rd_shift
				    : field >> -rd->rd_shift;
				if (any &&
				      (rd->rd_format || rd->rd_values
				         || (rd->rd_name && field)
				      )
				)
					PUTCHAR(',');
				if (rd->rd_name) {
					if (rd->rd_format || rd->rd_values
					    || field) {
						PUTS(rd->rd_name);
						any = 1;
					}
					if (rd->rd_format || rd->rd_values) {
						PUTCHAR('=');
						any = 1;
					}
				}
				if (rd->rd_format) {
#ifdef SPRINTF
					if (buf) {
						sprintf(buf, rd->rd_format,
							field);
						buf = &buf[strlen(buf)];
					} else
						printf(rd->rd_format, field);
#else
					printf(rd->rd_format, field);
#endif
					any = 1;
					if (rd->rd_values)
						PUTCHAR(':');
				}
				if (rd->rd_values) {
					any = 1;
					for (rv = rd->rd_values;
					    rv->rv_name;
					    rv++) {
						if (field == rv->rv_value) {
							PUTS(rv->rv_name);
							break;
						}
					}
					if (rv->rv_name == NULL)
						PUTS("???");
				}
			}
			PUTCHAR('>');
		}
		break;

	case 's':
		s = va_arg(adx, char *);
		PUTS(s);
		break;

	case '%':
		PUTCHAR('%');
		break;
	}
	goto loop;
}

/*
 * Printn prints a number n in base b.
 */
#ifdef SPRINTF
static char *
printn(n, b, buf)
#else
static void
printn(n, b)
#endif
register unsigned n;
register int b;
#ifdef SPRINTF
register char *buf;
#endif
{
	char prbuf[11];
	register char *cp;
	register int cnt;

	if (b < 0) {
		b = -b;
		if ((int)n < 0) {
			PUTCHAR('-');
			n = (unsigned)(-(int)n);
		}
	}

	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
#ifdef SPRINTF
	if ((zeropad || numpad) && ((cp - prbuf) < numpad)) {
		cnt = numpad - (cp - prbuf);
		while (cnt--)
			if (zeropad)
				*cp++ = '0';
			else
				*cp++ = ' ';
	}
#endif
	do
		PUTCHAR(*--cp);
	while (cp > prbuf);
#ifdef SPRINTF
	return(buf);
#endif
}

#endif OLD_PRINTF

/*
 * showchar -- print character in visible manner
 */
void
showchar(c)
int c;
{
	c &= 0xff;
	if (isprint(c))
		putchar(c);
	else switch (c) {
	case '\b':
		puts("\\b");
		break;
	case '\f':
		puts("\\f");
		break;
	case '\n':
		puts("\\n");
		break;
	case '\r':
		puts("\\r");
		break;
	case '\t':
		puts("\\t");
		break;
	default:
		putchar('\\');
		putchar(((c&0300) >> 6) + '0');
		putchar(((c&070) >> 3) + '0');
		putchar((c&07) + '0');
		break;
	}
}
