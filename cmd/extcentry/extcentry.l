/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: extcentry.l,v 1.2.2.1 89/11/26 20:10:07 wje Exp $ */

	/*  Lex source for "extcentry" command.  */

	/*  Written by Jonathan Bowen, July 1984.  */

	int centry=0;
SPACE	(" "|\t)*
%%
"/*"{SPACE}"CENTRY"{SPACE}"*/"		centry=1;
"/*"{SPACE}"ENDCENTRY"{SPACE}"*/"	centry=0;
.|\n					if (centry) ECHO;
%%
main(argc, argv)
int argc;
char *argv[];
{
	if (argc > 3)
	{
		fprintf(stderr,"extcentry: Too many arguments\n");
		exit(--argc);
	}
	if (--argc > 0)
		if (!(yyin = fopen(*++argv,"r")))
		{
			fprintf(stderr,"extcentry: Can't open %s\n",*argv);
			exit(1);
		}
	if (--argc > 0)
		if (!(yyout = fopen(*++argv,"w")))
		{
			fprintf(stderr,"extcentry: Can't open %s\n",*argv);
			exit(2);
		}
	yylex();
}
