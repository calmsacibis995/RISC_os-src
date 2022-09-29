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
#ident	"$Header: perror.c,v 1.6.2.3 90/05/10 01:36:05 wje Exp $"

/*LINTLIBRARY*/

/*
 * perror(s) prints the given text, a : (unless the text is empty), and
 * the system error message that corresponds to the current value of
 * errno.
 *
 * This version can be used to format messages as well. The environment
 * variable PERROR_FMT can be set to indicate how the message should
 * be formatted. The variable may contain any nonempty string which will
 * be printed as given except for the following sequences:
 *
 *	%p The standard message that is printed (text given: system message).
 *	%t The text given by the caller.
 *	%c A colon (:) if text is nonempty; nothing otherwise.
 *	%s A colon and a space (: ) if text is nonempty; nothing otherwise.
 *	%m System message.
 *	%e Symbolic name for the error number.
 *	%n The error number (digits).
 *	%% The character %.
 *
 * The error message is always terminated by a newline.
 *
 * Messages are buffered (BUFLEN bytes) so that writes are atomic unless
 * the messages are large.
 */

#ifdef	SYSTYPE_BSD43
int  errno;
#else
extern int  errno;
#endif
extern int  sys_nerr;
extern char *sys_errlist[];
extern int  sys_nerrno;
extern char *sys_errnolist[];
extern int  strlen();
extern int  write();
extern char *getenv();

static void print_str();
static void print_chr();
static void print_num();
static void pflush();

#define DEF_FMT		"%p"

#define BUFLEN		256
#define INIT		0
#define PRINT		1

static char Print_buf[BUFLEN];
static int  Print_left = 0;
static char *Print_pos = (char *)0;

void
perror(text)
	char	*text;
{
	char *sys_msg;
	char *sys_sym;
	char *fmt;
	char pbuf[3];

	sys_msg = "Unknown error";
	sys_sym = "UNKNOWN";
	if(errno < sys_nerr) {
		sys_msg = sys_errlist[errno];
	}
	if(errno < sys_nerrno) {
		sys_sym = sys_errnolist[errno];
	}

	fmt = getenv("PERROR_FMT");
	if (fmt == (char *)0 || *fmt == '\0') {
		fmt = DEF_FMT;
	}

	pflush(INIT);
	while (*fmt) {
		if (*fmt == '%') {
			fmt++;
			switch (*fmt) {
			case 'p':
				print_str(text);
				if (text && *text) {
					print_str(": ");
				}
				print_str(sys_msg);
				break;

			case 't':
				print_str(text);
				break;

			case 'c':
				if (text && *text) {
					print_chr(':');
				}
				break;

			case 's':
				if (text && *text) {
					print_str(": ");
				}
				break;

			case 'm':
				print_str(sys_msg);
				break;

			case 'e':
				print_str(sys_sym);
				break;

			case 'n':
				print_num(errno);
				break;

			case '%':
				print_chr('%');
				break;

			default:
				print_chr('%');
				if (*fmt) {
					print_chr(*fmt);
				}
				break;
			}
		} else {
			print_chr(*fmt);
		}
		if (*fmt) {
			fmt++;
		}
	}
	print_chr('\n');
	pflush(PRINT);
}

/*
 * The following routines actually do the print buffering and printing.
 * The rules are:
 *
 *	1. pflush(INIT) must be called before any print routines
 *	2. the buffer must have at least 1 character position left after
 *	   the call
 *	3. the variable Print_left must contain the number of characters
 *	   left in the buffer at all times (especially when pflush is called).
 */

static void
print_str(str)
	char *str;
{
	int len;

	if (!str) return;
	len = strlen(str);
	while (len > 0) {
		if (len >= Print_left) {
			strncpy(Print_pos, str, Print_left);
			len -= Print_left;
			str += Print_left;
			Print_left = 0;
			pflush(PRINT);
		} else {
			strcpy(Print_pos, str);
			Print_pos += len;
			Print_left -= len;
			len = 0;
		}
	}
}

static void
print_chr(ch)
	char ch;
{
	*Print_pos = ch;
	Print_pos++;
	Print_left--;

	if (Print_left == 0) {
		pflush(PRINT);
	}
}

static char *Digits = "0123456789";

static void
print_num(num)
	int num;
{
	char nbuf[24];	/* 24 digits should be enough */
	char *place;
	int rem;

	if (num == 0) {
		print_chr('0');
		return;
	}

	place = &nbuf[sizeof (nbuf) - sizeof (char)];
	*place = '\0';

	while (num > 0) {
		place--;
		rem = num % 10;
		num = num / 10;
		*place = Digits[rem];
	}

	print_str(place);
}

static void
pflush(what)
	int what;
{
	int len;

	if (what == PRINT) {
		len = BUFLEN - Print_left;
		(void) write(2, Print_buf, len);
	}
	Print_pos = Print_buf;
	Print_left = BUFLEN;
}
