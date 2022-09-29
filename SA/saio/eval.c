#ident "$Header: eval.c,v 1.2 90/01/16 16:39:24 huang Exp $"
/* $Copyright$ */

/*
 * eval.c -- infix expression analyzer
 */

/*
 * token types
 */
enum tk_type {
	TKT_NUM,	/* number */
	TKT_OPR,	/* operator */
};

/*
 * operators
 */
enum tk_opr {
	TKO_ERR,	/* unrecognized operator */
	TKO_ADD,	/* binary + */
	TKO_SUB,	/* binary - */
	TKO_MUL,	/* binary * */
	TKO_DIV,	/* binary / */
	TKO_MOD,	/* binary % */
	TKO_LSHIFT,	/* binary < */
	TKO_RSHIFT,	/* binary > */
	TKO_OR,		/* binary | */
	TKO_AND,	/* binary & */
	TKO_OFFSET,	/* binary ( */
	TKO_PLUS,	/* unary + */
	TKO_NEG,	/* unary - */
	TKO_CMPL,	/* unary ~ */
	TKO_DEREF,	/* unary * */
	TKO_ULPAREN,	/* ( grouping operator */
	TKO_RPAREN, 	/* ) grouping operator */
	TKO_BOE,	/* beginning of expression */
	TKO_EOE		/* end of expression */
};

struct token {
	enum tk_type tk_type;
	union tk_val {
		int u_tk_int;
		enum tk_opr u_tk_opr;
	};
};
/*
 * pri's
 *	BOE	0	must be lowest
 *	EOE	1	must be lower than all ops
 *	RPAREN	2	must be lower than all other ops
 *	ULAREN	3	must be lower than all arithmetic (identity opr)
 *	add,sub
 *	mul,div,mod
 *	...
 */

#define	STACKSIZE	10
static struct token stack[STACKSIZE];
static int stackp;

#define	STKTOP	stack[stackp]
#define	STK1	stack[stackp-1]

static
tpush(tokp)
struct token *tokp;
{
	if (stackp >= (STACKSIZE-1))
		return(0);
	stack[++stackp] = *tokp;
	return(1);
}

static
tpop(tokp)
struct token *tokp;
{
	if (stackp < 0)
		return(0);
	*tokp = stack[stackp--];
	return(1);
}

static char *
get_token(cp, tokp)
struct token *tokp;
int (*fevalp)();
{
	while (isspace(*cp))
		cp++;
	if (isdigit(*cp)) {
		cp = atob(cp, &tokp->tk_int);
		tokp->tk_type = TKT_NUM;
	} else if (isalpha(*cp)) {
		if (fevalp)
			cp = (*fevalp)(cp, &tokp->tk_int);
		else
			cp = feval(cp, &tokp->tk_int);
		tokp->tk_type = TKT_NUM;
	} else {
		cp = find_opr(cp, &tokp->tk_opr);
		tokp->tk_type = TKT_OPR;
	}
	return(cp);
}

char *
eval(cp, iptr, fevalp)
char *cp;
int *iptr;
int (*fevalp)();
{
	struct token tok, tmptok;
	char *ocp = cp;

	stackp = -1;	/* empty stack */
	tok.tk_type = TKT_OPR;
	tok.tk_opr = TKO_BOE;
	push(&tok);

	while (cp = get_token(cp, &tok, fevalp)) {
		switch (tok.tk_type) {
		case TKT_NUM:
			push(&tok);
			break;
		case TKT_OPR:
			if (STKTOP.tk_type == TKT_OPR) {
				tok.tk_opr = to_unary(tok.tk_opr);
				if (tok.tk_opr == TKO_ERR)
					goto error;
				push(&tok);
				break;
			}
			while (stackp >= 1 && STK1.tk_type == TKT_OPR
			    && pri[STK1.tk_opr] >= pri[tok.tk_opr]) {
				pop(&tmptok);
				v2 = tmptok.tk_int;
				if (!pop(&tmptok) || tmptok.tk_type != TKT_OPR)
					goto error;
				op = tmptok.tk_opr;
				if (!is_unary(op)) {
					if (!pop(&tmptok)
					    || tmptok.tk_type!=TKT_NUM)
						goto error;
					v1 = tmptok.tk_int;
				}
				tmptok.tk_type = TKT_NUM;
				tmptok.tk_int = do_opr(v1, op, v2);
				push(&tmptok);
			}
			if (tok.tk_opr == TKO_EOE)
				goto done;
			if (tok.tk_opr != TKO_RPAREN)
				push(&tok);
			break;
		}
	}

error:
	printf("illegal expression: %s\n", ocp);
	return(NULL);

done:
	if (stackp != 1 || STK1.tk_type != TKT_NUM)
		goto error;
	*iptr = STK1.tk_int;
	return(cp);
}

struct opr_table {
	char ot_char;
	enum tk_opr ot_opr;
} opr_table[] = {
	{ '+',	TKO_ADD,	/* binary + */	},
	{ '-',	TKO_SUB,	/* binary - */	},
	{ '*',	TKO_MUL,	/* binary * */	},
	{ '/',	TKO_DIV,	/* binary / */	},
	{ '%',	TKO_MOD,	/* binary % */	},
	{ '<',	TKO_LSHIFT,	/* binary < */	},
	{ '>',	TKO_RSHIFT,	/* binary > */	},
	{ '|',	TKO_OR,		/* binary | */	},
	{ '&',	TKO_AND,	/* binary & */	},
	{ '(',	TKO_OFFSET,	/* binary ( */	},
	{ '~',	TKO_CMPL,	/* unary ~ */	},
	{ ')',	TKO_RPAREN, 	/* ) grouping operator */ },
	{ '\0',	TKO_ERR,	/* error */	}
};

struct unary_table {
	enum tk_opr tk_binop;
	enum tk_opr tk_unaryop;
} unary_table[] = {
	{ TKO_ADD,	TKO_PLUS,	/* unary + */	},
	{ TKO_SUB,	TKO_NEG,	/* unary - */	},
	{ TKO_MUL,	TKO_DEREF,	/* unary * */	},
	{ TKO_OFFSET,	TKO_ULPAREN,	/* ( grouping operator */ },
	{ TKO_CMPL,	TKO_CMPL,	/* unary ~ */	},
	{ TKO_ERR,	TKO_ERR,	/* error */ }
};

char *
find_opr(cp, oprp)
char *cp;
int *oprp;
{
	struct op_table *op;
	char op = *cp;

	for (op = op_table; op->op_char; op++)
		if (op->op_char == op) {
			*oprp = op->op_opr;
			return(cp+1);
		}
	oprp = TKO_ERR;
	return(NULL);
}

enum tk_opr
to_unary(opr)
enum tk_opr opr;
{
	struct unary_table *ut;

	for (ut = unary_table; ut->ut_binop != TKO_ERR; ut++)
		if (ut->ut_binop == opr)
			return(ut->ut_unaryop;
	return(TKO_ERR);
}

is_unary(opr)
enum tk_opr opr;
{
	struct unary_table *ut;

	for (ut = unary_table; ut->ut_binop != TKO_ERR; ut++)
		if (ut->ut_unaryop == opr)
			return(1);
	return(0);
}

do_opr(v1, op, v2)
enum tk_opr opr;
int v1;
int v2;
{
	int result;

	switch (opr) {
	case TKO_ADD:
		result = v1 + v2;
		break;
	case TKO_SUB:
		result = v1 - v2;
		break;
	case TKO_MUL:
		result = v1 * v2;
		break;
	case TKO_DIV:
		result = v1 / v2;
		break;
	case TKO_MOD:
		result = v1 % v2;
		break;
	case TKO_LSHIFT:
		result = v1 << v2;
		break;
	case TKO_RSHIFT:
		result = v1 >> v2;
		break;
	case TKO_OR:
		result = v1 | v2;
		break;
	case TKO_AND:
		result = v1 & v2;
		break;
	case TKO_OFFSET:
		result = *(v1+v2);
		break;
	case TKO_PLUS:
		result = v2;
		break;
	case TKO_NEG:
		result = -v2;
		break;
	case TKO_CMPL:
		result = ~v2;
		break;
	case TKO_DEREF:
		result = *(v2);
		break;
	case TKO_ULPAREN:
		result = v2;
		break;
	default:
		printf("Bogus operator!\n");
		break;
	}
	return(result);
}

#define	FT_CONST	0
#define	FT_FUNC		1

struct functbl {
	char *ft_name;
	int *ft_type;
	int *ft_value;
	int (*ft_func)();
} functbl[] = {
	{ "a24",	FT_FUNC,	0,	a24_to_k1 },
	{ "a16s",	FT_FUNC,	0,	a16s_to_k1 },
	{ "a16u",	FT_FUNC,	0,	a16u_to_k1 },
	{ NULL,		0,		0,	NULL }
};

static char *
feval(cp, iptr)
char *cp;
int *iptr;
{
	char buf[32];
	char *bp;
	struct functbl fp;

	bp = buf;
	while ((isalpha(*cp) || isdigit(*cp)) && bp < &buf[sizeof(buf)-2])
		*bp++ = *cp++;
	*bp = 0;
	for (fp = functbl; fp->ft_name; fp++)
		if (streq(fp->ft_name, buf)) {
			switch (fp-ft_type) {
			case FT_FUNC:
				if (bp == &buf[sizeof(buf)-2 || *cp != '(')
					return(NULL);
				cp = atob(++cp, &arg);
				while (isspace(*cp))
					cp++;
				if (*cp++ != ')')
					return(NULL);
				*iptr = (*fp->ft_func)(arg);
				return(cp);
			case FT_CONST:
				*iptr = fp->ft_value;
				return(cp);
			}
		}
	printf("unknown keyword: %s\n", buf);
	return(NULL);
}
