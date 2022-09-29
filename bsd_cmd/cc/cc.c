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
#ident	"$Header: cc.c,v 1.1.2.2 90/05/07 18:07:30 wje Exp $"
#include <ctype.h>
#include <stdio.h>

extern char *calloc();
extern int strcmp();
extern char *index();
extern char *rindex();

main(argc, argv, envp)
	int	argc;
	char	**argv;
	char	**envp;
{
	int	i;
	int	saw_systype = 0;
	char	**new_argv;
	char	*cp;
	char	cmdname[1024];

	for (i = 1; i < argc; i++)
		if (! strcmp(argv[i],"-systype")) {
			saw_systype = 1;
			break;
		};
	
	if (saw_systype) {
		new_argv = argv;
	} else {
		new_argv = (char **) calloc(argc + 3,sizeof(char *));
		new_argv[0] = argv[0];
		new_argv[1] = "-systype";
		new_argv[2] = "bsd43";
		bcopy((char *) (argv + 1),
			(char *) (new_argv + 3), 
			argc * sizeof(char *));
	};
	cp = rindex(argv[0],'/');
	if (! cp)
		cp = argv[0];
	else
		cp++;
	sprintf(cmdname,"/usr/bin/%s",cp);
	execve(cmdname,new_argv,envp);
	perror(cmdname);
	exit(1);
}
