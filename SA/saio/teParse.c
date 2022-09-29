#ident "$Header: teParse.c,v 1.1.9.4 90/12/20 11:03:13 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ANSI Terminal Emulator - parser.
 */

#ifdef STANDALONE
#include "sys/types.h"
#include "mips/cpu.h"
#include "mips/teState.h"
#include "machine/grafreg.h"
#else
#include "sys/types.h"
#include "sys/teState.h"
#endif

#define XREG            (R3030_GRAPHICS_REG_ADDR + R3030_XSERVER_OFFSET)

/*
 * ANSI X3.4, X3.41, X3.64 basic parse table.
 *	Indexed by State and Character Column (high four bits).
 */
static	void	(*teStateTable[TESTATES][8])() = {
 /* State     0x      1x      2x      3x      4x      5x      6x      7x  */
 /* RAW */ { teCtl,  teCtl,  tePut,  tePut,  tePut,  tePut,  tePut,  tePut },
#ifndef STANDALONE /* { */
 /* ES  */ { teCtl,  teCtl,  teDmp,  teESF,  teESF,  teESF,  teESF,  teESF },
 /* CS  */ { teCtl,  teCtl,  teDmp,  teCSP,  teCSF,  teCSF,  teCSF,  teCSF },
 /* CSP */ { teCtl,  teCtl,  teDmp,  teCSP,  teCSF,  teCSF,  teCSF,  teCSF },
#endif /* } ! STANDALONE */
};

#ifdef STANDALONE /* { */
#define teEsc	teDmp
#endif /* } STANDALONE */

/*
 * Control Characters (C0).
 *	Indexed by character value.
 */
static	void	(*teCtlTable[])() = {
 /* 00	NUL	SOH	STX	ETX	EOT	ENQ	ACK	BEL+ */
	teIgn,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teBEL,
 /* 08	BS+	HT+	LF+	VT	FF	CR+	SO	SI  */
	teBS,	teHT,	teLF,	teDmp,	teDmp,	teCR,	teDmp,	teDmp,
 /* 10	DLE	DC1	DC2	DC3	DC4	NAK	SYN	ETB */
	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teIgn,	teDmp,
 /* 18	CAN	EM	SUB	ESC	FS	GS	RS	US  */
	teDmp,	teDmp,	teDmp,	teEsc,	teDmp,	teDmp,	teDmp,	teDmp
};

#ifndef STANDALONE /* { */

/*
 * Escape Sequences (with no Intermediates).
 *	Indexed by Final character value.
 */
#define TE_ESF_FIRST	0x44
#define TE_ESF_LAST	0x63

static	void	(*teESFTable[])() = {
 /* 40					IND	NEL	SSA	ESA */
					teLF,	teDmp,	teDmp,	teDmp,
 /* 48	HTS+	HTJ	VTS	PLD	PLU	RI	SS2	SS3 */
	teHTS,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,
 /* 50	DCS	PU1	PU2	STS	CCH	MW	SPA	EPA */
	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,
 /* 58	fs.	fs.	fs.	CSI	ST	OSC	PM	APC */
	teDmp,	teDmp,	teDmp,	teCSI,	teDmp,	teDmp,	teDmp,	teDmp,
 /* 60	DMI	INT	EMI	RIS */
	teDmp,	teDmp,	teDmp,	teDmp,
};

/*
 * Control Sequences (with no Intermediates).
 *	Indexed by Final character value.
 */
#define TE_CSF_FIRST	0x40
#define TE_CSF_LAST	0x6f

static	void	(*teCSFTable[])() = {
 /* 40	ICH+	CUU+	CUD+	CUF+	CUB+	CNL	CPL	CHA+ */
	teICH,	teCUU,	teCUD,	teCUF,	teCUB,	teDmp,	teDmp,	teHPA,
 /* 48	CUP+	CHT	ED+	EL+	IL+	DL+	EF	EA  */
	teCUP,	teDmp,	teED,	teEL,	teIL,	teDL,	teDmp,	teDmp,
 /* 50	DCH+	SEM	CPR	SU	SD	NP	PP	CTC */
	teDCH,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,
 /* 58	ECH	CVT	CBT+	fs.	fs.	fs.	fs.	fs. */
	teDmp,	teDmp,	teCBT,	teDmp,	teDmp,	teDmp,	teDmp,	teDmp,
 /* 60	HPA	HPR	REP+	DA	VPA+	VPR	HVP	TBC+ */
	teHPA,	teDmp,	teREP,	teDmp,	teVPA,	teDmp,	teDmp,	teTBC,
 /* 68	SM	MC	fs.	fs.	RM	SGR+	DSR	DAQ */
	teTBC,	teDmp,	teDmp,	teDmp,	teTBC,	teSGR,	teDmp,	teDmp,
};

#endif /* } ! STANDALONE */

#define ON		1
#define OFF		0
#define FALSE		0
#define TRUE		1
#define HiBits(c)	((c) >> 4)
#define Strip(c)	((c) & 0x7f)
#define EMPTY		-1

#ifdef STANDALONE
extern caddr_t frm_addr;
extern int nrows;
extern int ncols;
int *paramptr;

init_gsparam()
{
	nrows = 24;
	ncols = 80;
	teState.row = 1;
	teState.col = 1;
	frm_addr = (caddr_t)PHYS_TO_K1(0x800000-132*1024);
}

restore_gsparam()
{
	paramptr = (int *)PHYS_TO_K1(0x500);
	nrows = *paramptr++;
	ncols = *paramptr++;
	teState.row = *paramptr++;
	teState.col = *paramptr++;
	frm_addr = (caddr_t)*paramptr;
}

save_gsparam()
{
	paramptr = (int *)PHYS_TO_K1(0x500);
	*paramptr++ = nrows;
	*paramptr++ = ncols;
	*paramptr++ = teState.row;
	*paramptr++ = teState.col;
	*paramptr = (unsigned int)frm_addr;
}
#endif


#ifndef STANDALONE /* { */

/*
 * Interpret parameters from a parameter character string.
 *	All characters in ps are from { c : '0' [0x30] <= c <= '?' [0x3f] }
 */
int	*
teParms(n, ps)
	int	n;		/* max parms for the current sequence */
	TeState	*ps;
{
static	int	list[TEMAXPARMS+1];	/* max 64 parameters */
	int	*l = &list[1];		/* put parm. count in list[0] */	
	int	m = 0, i = EMPTY;	/* parm. count, integer accumulator */
	u_char	*p;

	for (p = ps->seq; p < ps->pp; p++) {
		if (m >= n) {		/* stop after n parameters */
			break;
		} else if (*p <= '9') {
			if (i == EMPTY)
				i = *p - '0';
			else
				i = i * 10 + *p - '0';
		} else if (*p == ';') {	/* standard separator, continue */
			m++;
			*l++ = (i == EMPTY ? 0 : i);
			i = EMPTY;
		} else {		/* non-std sep., discard remainder */
			break;
		}
	}
	if (i != EMPTY) {
		m++;
		*l++ = i;
	}
	list[0] = m;			/* insert the count */
	return list;
}

#endif /* } ! STANDALONE */

/*
 * Print the sequence and reset the parser.
 */
void
teDmp(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	u_char	*s, *se;		/* sequence buffer, its end */

	/*
	 * Print the contents of the sequence buffer (parms & intermediates).
	 * If the current character is not a Control,
	 *	Print it.
	 * Reset the sequence buffer pointer.
	 * Reset the parser state.
	 */
#ifndef STANDALONE /* { */
	for (s = ps->seq, se = ps->pp; s < se; s++)
		tePut(*s, ps);
#endif /* } ! STANDALONE */
	if (' ' <= c && c <= '~')
		tePut(c, ps);
#ifndef STANDALONE /* { */
	ps->pp = ps->seq;
	ps->state = TES_RAW;
#endif /* } ! STANDALONE */
	return;
}

/*
 * Ignore the sequence.
 */
void
teIgn(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	return;
}

/*
 * Any state, c is a Control character.
 */
void
teCtl(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	void	(*f)() = teCtlTable[(int)c];

	/*
	 * Only pad control characters are allowed outside Raw state.
	 * Set the state to Raw.
	 * Execute the function.
	 */
#ifndef STANDALONE /* { */
	if (ps->state != TES_RAW
	  && (char *)f != (char *)teIgn)  /* won't compile without casts !! */
		f = teDmp;		  /*    until 2.10 compilers */
	ps->prev = c;
	ps->state = TES_RAW;
#endif /* } ! STANDALONE */
	(*f)(c, ps);
	return;
}

#ifndef STANDALONE /* { */

/*
 * Raw state, c is ESC.
 */
void
teEsc(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	/*
	 * Set the state to Escape sequence.
	 */
	ps->state = TES_ES;
	ps->pp = ps->seq;
	return;
}

/*
 * Escape sequence state, c is CSI (actually `[').
 */
void
teCSI(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	/*
	 * Set the state to Escape sequence.
	 */
	ps->state = TES_CS;
	return;
}

/*
 * In an Escape Sequence, c is a Final character.
 *	ESC F
 */
void
teESF(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	/*
	 * If the character is outside the table
	 *	Dump the sequence buffer and return.
	 * Set the state to Raw.
	 * Execute the function.
	 */
	if (c < TE_ESF_FIRST || TE_ESF_LAST < c) {
		teDmp(c, ps);
		return;
	}
	ps->state = TES_RAW;
	(*teESFTable[(int)(c - TE_ESF_FIRST)])(c, ps);
	return;
}

/*
 * In a Control Sequence, c is a Parameter.
 */
void
teCSP(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	/*
	 * If we can handle another Parameter,
	 *	Add it to the Parameter list.
	 * (Extra Parameter characters are discarded.)
	 * Set the state to Control Sequence with Parameter(s).
	 */
	if (ps->pp - ps->seq < TEMAXSEQ)
		*ps->pp++ = c;
	ps->state = TES_CSP;
	return;
}

/*
 * In a Control Sequence, c is a Final character.
 *	CSI P* F   ===   ESC [ P* F
 */
void
teCSF(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	/*
	 * If the character is outside the table,
	 *	Dump the sequence buffer and return.
	 * Set the state to Raw.
	 * Execute the function.
	 */
	if (c < TE_CSF_FIRST || TE_CSF_LAST < c) {
		teDmp(c, ps);
		return;
	}
	ps->state = TES_RAW;
	(*teCSFTable[(int)(c - TE_CSF_FIRST)])(c, ps);
	return;
}

#endif /* } ! STANDALONE */

#ifdef NOTUSED /* { */

/*
 * Put characters after ANSI (3.4,3.41,3.64) interpretation.
 */
void
tePutStr(l, s)
	int	l;			/* length */
	u_char	*s;			/* string */
{
	u_char	c, *se = s + l;		/* current char, end of string */

	/*
	 * Turn the cursor off.
	 * For each character in the input buffer:
	 *	Strip off the high bit.		[TODO: Internationalization ??]
	 *	If the character is an ASCII DEL(Rubout),
	 *		Ignore it (may be used for padding).
	 *	Find the associated function in the table, based on
	 *	  the current parsing state and the "column" of the character.
	 * Turn the cursor back on.
	 */
	teCursor(teState.row, teState.col, OFF);
	while (s < se) {
		if ((c = Strip(*s++)) == ASCII_DEL)
			continue;	/* teIgn */
#ifdef STANDALONE /* { */
		(*teStateTable[TES_RAW][(int)HiBits(c)])(c, &teState);
#else /* } STANDALONE { */
		(*teStateTable[(int)teState.state][(int)HiBits(c)])(c,
		  &teState);
#endif /* } ! STANDALONE */
	}
	teCursor(teState.row, teState.col, ON);
	return;
}

#endif /* } NOTUSED */

extern int screen_blanked;
extern int mono_cons;
extern unsigned long blank_time;

/*
 * Put a character after ANSI (3.4,3.41,3.64) interpretation.
 */
void
tePutChar(c)
	u_char	c;
{
	unsigned long xreg_save;

	/*
	 * If the character is not an ASCII DEL(Rubout),
	 *	Turn the cursor off.
	 *	Strip off the high bit.		[TODO: Internationalization ??]
	 *	Find the associated function in the table, based on
	 *	  the current parsing state and the "column" of the character.
	 *	Turn the cursor back on.
	 */
        xreg_save = *((volatile unsigned long *)PHYS_TO_K1(XREG));
        *((volatile unsigned long *)PHYS_TO_K1(XREG)) = 1;
	if ((c = Strip(c)) != ASCII_DEL) {
		teCursor(teState.row, teState.col, OFF);
#ifdef STANDALONE /* { */
		(*teStateTable[TES_RAW][(int)HiBits(c)])(c, &teState);
#else /* } STANDALONE { */
		(*teStateTable[(int)teState.state][(int)HiBits(c)])(c,
		  &teState);
#endif /* } ! STANDALONE */
		teCursor(teState.row, teState.col, ON);
	}
        *((volatile unsigned long *)PHYS_TO_K1(XREG)) = xreg_save | 0x1;
	if (screen_blanked) {
	    if (mono_cons) mono_blank(0);
	    screen_blanked = 0;
	    blank_time = 0;
	}
	return;
}
