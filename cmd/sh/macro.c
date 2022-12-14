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
#ident	"$Header: macro.c,v 1.7.2.3 90/05/09 18:58:45 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * UNIX shell
 */

#include	"defs.h"
#include	"sym.h"

static char	quote;	/* used locally */
static char	quoted;	/* used locally */



static void	/* DAG -- bug fix (no value returned) */
copyto(endch, trimflag)
int trimflag;  /* flag to check if argument will be trimmed */
register unsigned char	endch;
{
	register unsigned char	c;
	register unsigned char 	d;

	while ((c = getch(endch, trimflag)) != endch && c)
		if (quote) {
			if (c == '\\') { /* don't interpret next character */
				pushstak(c);
				d = readc();
				if(!escchar(d)) { /* both \ and following
						     character are quoted if next
						     character is not $, `, ", or \*/
					pushstak('\\'); 
					pushstak('\\');
				}
				pushstak(d);
			} else { /* push escapes onto stack to quote characters */
				pushstak('\\');
				pushstak(c);
			}
		} else if (c == '\\') {
			c = readc(); /* get character to be escaped */
			pushstak('\\');
			pushstak(c);
		} else
			pushstak(c);
	zerostak();
	if (c != endch)
		error(badsub);
}

static
skipto(endch)
register unsigned char	endch;
{
	/*
	 * skip chars up to }
	 */
	register unsigned char	c;

	while ((c = readc()) && c != endch)
	{
		switch (c)
		{
		case SQUOTE:
			skipto(SQUOTE);
			break;

		case DQUOTE:
			skipto(DQUOTE);
			break;

		case DOLLAR:
			if (readc() == BRACE)
				skipto('}');
		}
	}
	if (c != endch)
		error(badsub);
}

static
getch(endch, trimflag)
unsigned char	endch;
int trimflag; /* flag to check if an argument is going to be trimmed, here document
		 output is never trimmed
	 */
{
	register unsigned char	d;
	int atflag;  		/* flag to check if $@ has already */
				/* been seen within double quotes */

retry:
	d = readc();
	if (!subchar(d))
		return(d);
#if JOBS
	if (d == PERCENT && (flags&jobflg))
	{
		register int	c;

		peekc = (c = readc()) | MARK;
		if (digchar(c) || c == PERCENT)
		{
			register char	*v;

			if (v = j_macro())	/* %number or %% handled */
				while (c = *v++)
					pushstak(c | quote);
			/* else expands to nothingness */
			goto retry;
		}
	}
	else
#endif
	if (d == DOLLAR)
	{
		unsigned char c;

		if ((c = readc(), dolchar(c)))
		{
			struct namnod *n = (struct namnod *)NIL;
			int		dolg = 0;
			BOOL		bra;
			BOOL		nulflg;
			register char	*argp, *v;
			unsigned char		idb[2];
			unsigned char		*id = idb;

			if (bra = (c == BRACE))
				c = readc();
			if (letter(c))
			{
				argp = (char *)relstak();
				while (alphanum(c))
				{
					pushstak(c);
					c = readc();
				}
				zerostak();
				n = lookup(absstak(argp));
				setstak(argp);
				if (n->namflg & N_FUNCTN)
					error(badsub);
				v = n->namval;
				id = (unsigned char *)n->namid;
				peekc = c | MARK;
			}
			else if (digchar(c))
			{
				*id = c;
				idb[1] = 0;
				if (astchar(c))
				{
					if(c == '@' && !atflag && quote) {
						quoted--;
						atflag = 1;
					}
					dolg = 1;
					c = '1';
				}
				c -= '0';
				v = ((c == 0) ? cmdadr :
					(c <= dolc) ? dolv[c] :
						(char *)(dolg = 0));
			}
			else if (c == '$')
				v = pidadr;
			else if (c == '!')
				v = pcsadr;
			else if (c == '#')
			{
				itos(dolc);
				v = numbuf;
			}
			else if (c == '?')
			{
				itos(retval);
				v = numbuf;
			}
			else if (c == '-')
				v = flagadr;
			else if (bra)
				error(badsub);
			else
				goto retry;
			c = readc();
			if (c == ':' && bra)	/* null and unset fix */
			{
				nulflg = 1;
				c = readc();
			}
			else
				nulflg = 0;
			if (!defchar(c) && bra)
				error(badsub);
			argp = 0;
			if (bra)
			{
				if (c != '}')
				{
					argp = (char *)relstak();
					if ((v == 0 || (nulflg && *v == 0)) ^ (setchar(c)))
						copyto('}', trimflag);
					else
						skipto('}');
					argp = absstak(argp);
				}
			}
			else
			{
				peekc = c | MARK;
				c = 0;
			}
			if (v && (!nulflg || *v))
			{

				if (c != '+')
				{
					for (;;)
					{
						if (*v == 0 && quote) {
							pushstak('\\');
							pushstak('\0');
						}
						else
							while (c = *v++) {
								if(quote || (c == '\\' && trimflag))
									pushstak('\\');
								pushstak(c);
							}

						if (dolg == 0 || (++dolg > dolc))
							break;
						else /* $* and $@ expansion */
						{
							v = dolv[dolg];
							if(*id == '*' && quote)
/* push quoted space so that " $* " will not be broken into separate arguments */
								pushstak('\\');
							pushstak(' ');
						}
					}
				}
			}
			else if (argp)
			{
				if (c == '?') {
					if(trimflag)
						trim(argp);
					failed(id, *argp ? argp : badparam);
				}
				else if (c == '=')
				{
					if (n)
					{
						int strlngth = staktop - stakbot;
						char *savptr = fixstak();
						char *newargp;
					/*
					 * copy word onto stack, trim it, and then
					 * do assignment 
					 */
						usestak();
						while(c = *argp++) {
							if(c == '\\' && trimflag) {
								c = *argp++;
								if(!c)
									continue;
							}
							pushstak(c);
						}
						newargp = fixstak();
						assign(n, newargp);
						tdystak(savptr);
						memcpy(stakbot, savptr, strlngth);
						staktop = stakbot + strlngth;
					}
					else
						error(badsub);
				}
			}
			else if (flags & setflg)
				failed(id, unset);
			goto retry;
		}
		else
			peekc = c | MARK;
	}
	else if (d == endch)
		return(d);
	else if (d == SQUOTE)
	{
		comsubst(trimflag);
		goto retry;
	}
	else if (d == DQUOTE)
	{
		if(!quote) {
			atflag = 0;
			quoted++;
		}
		quote ^= QUOTE;
		goto retry;
	}
	return(d);
}

char *
macro(as)
char	*as;
{
	/*
	 * Strip "" and do $ substitution
	 * Leaves result on top of stack
	 */
	register BOOL	savqu = quoted;
	register char	savq = quote;
	struct filehdr	fb;

#ifdef RISCOS
	if (strcmp (as, "\"$@\"") == 0 && dolc == 0) {
		return(fixstak());
	}
#endif RISCOS
	push(&fb);
	estabf(as);
	usestak();
	quote = 0;
	quoted = 0;
	copyto(0, 1);
	pop();
	if (quoted && (stakbot == staktop)) {
		pushstak('\\');
		pushstak('\0');
/*
 * above is the fix for *'.c' bug
 */
	}
	quote = savq;
	quoted = savqu;
	return(fixstak());
}

/* Save file descriptor for command substitution */
int savpipe = -1;
static
comsubst(trimflag)
int trimflag; /* used to determine if argument will later be trimmed */
{
	/*
	 * command substn
	 */
	struct fileblk	cb;
	register unsigned char	d;
	int strlngth = staktop - stakbot;
	register char *oldstaktop;
	char *savptr = fixstak();

	usestak();
	while ((d = readc()) != SQUOTE && d) {
		if(d == '\\') {
			d = readc();
			if(!escchar(d) || (d == '"' && !quote))
		/* trim quotes for `, \, or " if command substitution is within
		   double quotes */
				pushstak('\\');
		}
		pushstak(d);
	}
	{
		register char	*argc;

		argc = fixstak(); 
		push(&cb);
		estabf(argc);  /* read from string */
	}
#if JOBS
	set_wfence();
#endif
	{
		register struct trenod *t = makefork(FPOU, cmd(EOFSYM, MTFLG | NLFLG));
		int		pv[2];

		/*
		 * this is done like this so that the pipe
		 * is open only when needed
		 */
		chkpipe(pv);
		savpipe = pv[OTPIPE];
		initf(pv[INPIPE]); /* read from pipe */
		execute(t, 0, (int)(flags & errflg), 0, pv);
		close(pv[OTPIPE]);
		savpipe = -1;
	}
	tdystak(savptr);
	memcpy(stakbot, savptr, strlngth);
	oldstaktop = staktop = stakbot + strlngth;
	while (d = readc()) {
		if (staktop >= brkend) {
			setbrk(brkincr);
		}
		if(quote || (d == '\\' && trimflag))
			/* quote output from command subst. if within double 
			   quotes or backslash part of output */
			pushstak('\\');
		pushstak(d);
	}
	await(0, 0);
	while (oldstaktop != staktop)
	{ /* strip off trailing newlines from command substitution only */
		if ((*--staktop) != NL)
		{
			++staktop;
			break;
		} else if(quote)
			staktop--; /* skip past backslashes if quoting */ 
	}
	pop();
}

#define CPYSIZ	512

subst(in, ot)
int	in, ot;
{
	register unsigned char	c;
	struct fileblk	fb;
	register int	count = CPYSIZ;

	push(&fb);
	initf(in);
	/*
	 * DQUOTE used to stop it from quoting
	 */
	while (c = (getch(DQUOTE, 0))) /* read characters from here document 
				       and interpret them */
	{
		if(c == '\\') {
			c = readc(); /* check if character in here document is 
					escaped */
			if(!escchar(c) || c == '"')
				pushstak('\\');
		}
		pushstak(c);
		if (--count == 0)
		{
			flush(ot);
			count = CPYSIZ;
		}
	}
	flush(ot);
	pop();
}

static
flush(ot)
{
	write(ot, stakbot, staktop - stakbot);
	if (flags & execpr)
		write(output, stakbot, staktop - stakbot);
	staktop = stakbot;
}
