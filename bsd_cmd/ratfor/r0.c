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
#ident	"$Header: r0.c,v 1.1.2.2 90/05/07 19:08:05 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)r0.c	1.2 (Berkeley) 8/11/83";
#endif

#include "r.h"

int	swlevel	= -1;
int	swexit[5];
int	nextcase[5];

swcode() {
	transfer = 0;
	putcom("switch");
	swlevel++;
	if (swlevel >= 5)
		error("Switches nested > 5");
	swexit[swlevel] = yyval = genlab(1);
	outcode("\tI");
	outnum(yyval);
	outcode(" = ");
	balpar();
	outdon();
	nextcase[swlevel] = 0;
	indent++;
}

getcase() {
	int t, lpar;
	char token[100];

	if (nextcase[swlevel] != 0) {
		outgoto(swexit[swlevel]);
		outcont(nextcase[swlevel]);
	}
	indent--;
	outcode("\tif(.not.(");
	do {
		outcode("I");
		outnum(swexit[swlevel]);
		outcode(".eq.(");
		lpar = 0;
		do {
			if ((t=gtok(token)) == ':')
				break;
			if (t == '(')
				lpar++;
			else if (t == ')')
				lpar--;
			else if (t == ',') {
				if (lpar == 0)
					break;
				}
			outcode(token);
		} while (lpar >= 0);
		if (lpar < 0)
			error("Missing left parenthesis in case");
		if (t == ',')
			outcode(").or.");
	} while (t != ':');
	if (lpar != 0)
		error("Missing parenthesis in case");
	outcode(")))");
	nextcase[swlevel] = genlab(1);
	outgoto(nextcase[swlevel]);
	indent++;
}

getdefault() {
	char token[20];
	if (gnbtok(token) != ':')
		error("Missing colon after default");
	outgoto(swexit[swlevel]);
	outcont(nextcase[swlevel]);
	indent--;
	putcom("default");
	indent++;
}

endsw(n, def) {
	if (def == 0)
		outcont(nextcase[swlevel]);
	swlevel--;
	if (swlevel < -1)
		error("Switches unwound too far");
	indent--;
	outcont(n);
}
