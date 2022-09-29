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
#ident	"$Header: expr.c,v 1.6.2.2 90/05/09 15:49:14 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# define A_STRING 258
# define NOARG 259
# define OR 260
# define AND 261
# define EQ 262
# define LT 263
# define GT 264
# define GEQ 265
# define LEQ 266
# define NEQ 267
# define ADD 268
# define SUBT 269
# define MULT 270
# define DIV 271
# define REM 272
# define MCH 273
# define MATCH 274

/* size of array used to compile regular expressions */
#define ESIZE	1024
/* size of subexpression array */
#define MSIZE	128
#define error(c)	errxx()
#define EQL(x,y) !strcmp(x,y)

#define INIT	register char *sp = instring;
#define GETC()		(*sp++)
#define PEEKC()		(*sp)
#define UNGETC(c)	(--sp)
#define RETURN(c)	return
#define ERROR(c)	errxx()
#include  <regexp.h>

long atol();
char *ltoa(), *strcpy(), *strncpy();
void exit();
char	**Av;
char *buf;
int	Ac;
int	Argi;
int noarg;
int paren;
/*	
 *	Array used to store subexpressions in regular expressions
 *	Only one subexpression allowed per regular expression currently 
 */
char Mstring[1][MSIZE];
char *malloc();


char *operator[] = { 
	"|", "&", "+", "-", "*", "/", "%", ":",
	"=", "==", "<", "<=", ">", ">=", "!=",
	"match", "\0" };
int op[] = { 
	OR, AND, ADD,  SUBT, MULT, DIV, REM, MCH,
	EQ, EQ, LT, LEQ, GT, GEQ, NEQ,
	MATCH };
int pri[] = {
	1,2,3,3,3,3,3,3,4,4,5,5,5,6,7};
yylex() {
	register char *p;
	register i;

	if(Argi >= Ac) return NOARG;

	p = Av[Argi];

	if((*p == '(' || *p == ')') && p[1] == '\0' )
		return (int)*p;
	for(i = 0; *operator[i]; ++i)
		if(EQL(operator[i], p))
			return op[i];


	return A_STRING;
}

char *rel(oper, r1, r2) register char *r1, *r2; 
{
	long i;

	if(ematch(r1, "-\\{0,1\\}[0-9]*$") && ematch(r2, "-\\{0,1\\}[0-9]*$"))
		i = atol(r1) - atol(r2);
	else
		i = strcmp(r1, r2);
	switch(oper) {
	case EQ: 
		i = i==0; 
		break;
	case GT: 
		i = i>0; 
		break;
	case GEQ: 
		i = i>=0; 
		break;
	case LT: 
		i = i<0; 
		break;
	case LEQ: 
		i = i<=0; 
		break;
	case NEQ: 
		i = i!=0; 
		break;
	}
	return i? "1": "0";
}

char *arith(oper, r1, r2) char *r1, *r2; 
{
	long i1, i2;
	register char *rv;

	if(!(ematch(r1, "-\\{0,1\\}[0-9]*$") && ematch(r2, "-\\{0,1\\}[0-9]*$")))
		yyerror("non-numeric argument");
	i1 = atol(r1);
	i2 = atol(r2);

	switch(oper) {
	case ADD: 
		i1 = i1 + i2; 
		break;
	case SUBT: 
		i1 = i1 - i2; 
		break;
	case MULT: 
		i1 = i1 * i2; 
		break;
	case DIV: 
		if (i2 == 0)
			yyerror("division by zero");
		i1 = i1 / i2; 
		break;
	case REM: 
		if (i2 == 0)
			yyerror("division by zero");
		i1 = i1 % i2; 
		break;
	}
	rv = malloc(16);
	(void) strcpy(rv, ltoa(i1));
	return rv;
}
char *conj(oper, r1, r2) char *r1, *r2; 
{
	register char *rv;

	switch(oper) {

	case OR:
		if(EQL(r1, "0")
		    || EQL(r1, ""))
			if(EQL(r2, "0")
			    || EQL(r2, ""))
				rv = "0";
			else
				rv = r2;
		else
			rv = r1;
		break;
	case AND:
		if(EQL(r1, "0")
		    || EQL(r1, ""))
			rv = "0";
		else if(EQL(r2, "0")
		    || EQL(r2, ""))
			rv = "0";
		else
			rv = r1;
		break;
	}
	return rv;
}

char *match(s, p)
char *s, *p;
{
	register char *rv;

	(void) strcpy(rv=malloc(8), ltoa((long)ematch(s, p)));
	if(nbra) {
		rv = malloc((unsigned) strlen(Mstring[0]) + 1);
		(void) strcpy(rv, Mstring[0]);
	}
	return rv;
}

ematch(s, p)
char *s;
register char *p;
{
	static char expbuf[ESIZE];
	char *compile();
	register num;
	extern char *braslist[], *braelist[], *loc2;

	compile(p, expbuf, &expbuf[ESIZE], 0);
	if(nbra > 1)
		yyerror("Too many '\\('s");
	if(advance(s, expbuf)) {
		if(nbra == 1) {
			p = braslist[0];
			num = braelist[0] - p;
			if ((num > MSIZE - 1) || (num < 0)) yyerror("Paren problem");
			(void) strncpy(Mstring[0], p, num);
			Mstring[0][num] = '\0';
		}
		return(loc2-s);
	}
	return(0);
}

errxx()
{
	yyerror("RE error");
}

yyerror(s)
char *s;
{
	(void) write(2, "expr: ", 6);
	(void) write(2, s, (unsigned) strlen(s));
	(void) write(2, "\n", 1);
	exit(2);
}

char *ltoa(l)
long l;
{
	static str[20];
	register char *sp = (char *) &str[18];	/*u370*/
	register i;
	register neg = 0;

	if(l == 0x80000000L)
		return "-2147483648";
	if(l < 0)
		++neg, l = -l;
	str[19] = '\0';
	do {
		i = l % 10;
		*sp-- = '0' + i;
		l /= 10;
	} 
	while(l);
	if(neg)
		*sp-- = '-';
	return ++sp;
}
char *expres(prior,par)  int prior, par; 
{
	int ylex, temp, op1;
	char *r1, *ra, *rb;
	ylex = yylex();
	if (ylex >= NOARG && ylex < MATCH) {
		yyerror("syntax error");
	}
	if (ylex == A_STRING) {
		r1 = Av[Argi++];
		temp = Argi;
	}
	else {
		if (ylex == '(') {
			paren++;
			Argi++;
			r1 = expres(0,Argi);
			Argi--;
		}
	}
lop:
	ylex = yylex();
	if (ylex > NOARG && ylex < MATCH) {
		op1 = ylex;
		Argi++;
		if (pri[op1-OR] <= prior ) 
			return r1;
		else {
			switch(op1) {
			case OR:
			case AND:
				r1 = conj(op1,r1,expres(pri[op1-OR],0));
				break;
			case EQ:
			case LT:
			case GT:
			case LEQ:
			case GEQ:
			case NEQ:
				r1=rel(op1,r1,expres(pri[op1-OR],0));
				break;
			case ADD:
			case SUBT:
			case MULT:
			case DIV:
			case REM:
				r1=arith(op1,r1,expres(pri[op1-OR],0));
				break;
			case MCH:
				r1=match(r1,expres(pri[op1-OR],0));
				break;
			}
			if(noarg == 1) {
				return r1;
			}
			Argi--;
			goto lop;
		}
	}
	ylex = yylex();
	if(ylex == ')') {
		if(par == Argi) {
			yyerror("syntax error");
		}
		if(par != 0) {
			paren--;
			Argi++;
		}
		Argi++;
		return r1;
	}
	ylex = yylex();
	if(ylex > MCH && ylex <= MATCH) {
		if (Argi == temp) {
			return r1;
		}
		op1 = ylex;
		Argi++;
		switch(op1) {
		case MATCH:
			rb = expres(pri[op1-OR],0);
			ra = expres(pri[op1-OR],0);
		}
		switch(op1) {
		case MATCH: 
			r1 = match(rb,ra); 
			break;
		}
		if(noarg == 1) {
			return r1;
		}
		Argi--;
		goto lop;
	}
	ylex = yylex();
	if (ylex == NOARG) {
		noarg = 1;
	}
	return r1;
}
main(argc, argv) char **argv; 
{
	Ac = argc;
	Argi = 1;
	noarg = 0;
	paren = 0;
	Av = argv;
	buf = expres(0,1);
	if(Ac != Argi || paren != 0) {
		yyerror("syntax error");
	}
	(void) write(1, buf, (unsigned) strlen(buf));
	(void) write(1, "\n", 1);
	exit((!strcmp(buf, "0") || !buf[0])? 1: 0);
	/* NOTREACHED */
}