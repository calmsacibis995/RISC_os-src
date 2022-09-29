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
#ident	"$Header: main.c,v 1.10.2.2 90/05/09 16:18:59 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

#include <sys/types.h>
#include <a.out.h>
#include <sys/param.h>
#include "lboot.h"
#include "error.h"


/*
 * LBOOT
 *
 * Modus operandi:
 *
 *	main():
 *		findsystem(): get either name of etc/system file or name of boot program
 *				(check for magic mode)
 *		loadunix():
 *			if etc/system file name known
 *				findrivers(): read /boot directory and build driver[] array
 *				fsystem(): parse etc/system file
 *			if boot program name not known
 *				System(): prompt for boot program
 *			read boot program file header
 *			if etc/system file not known
 *				findrivers(): read /boot directory and build driver[] array
 *				System(): prompt for INCLUDE and EXCLUDE
 *			System(): if necessary, prompt for rootdev, pipedev, ...
 *			flag all drivers to be ignored
 *			flag all drivers to be loaded (follow dependency chain)
 *			compute: number of character and block devices
 *				 number of interrupt routines required
 *				 total number of drivers that are to be loaded
 *				print_configuration(): print the configuration table
 *				build_io_subsys():
 *					allocate MAJOR, MINOR, [cb]devcnt, [cb]devsw
 *					for each driver to be loaded
 *						allocate xx_addr[] array if necessary
 *						set MAJOR[] and MINOR[] entries
 *						generate interrupt routines and PCB's
 *					allocate rootdev, pipedev, ...
 *					get space to build io_init[] and io_start[] arrays
 *				for each driver to be loaded
 *					loadriver():
 *						set [bc]devsw table entries
 *						allocate driver specific data structures
 *						set io_init[] and io_start[] table entries
 *				allocate final io_init[] and io_start[] arrays
 *					routine(): check routine names in unloaded drivers
 *				link objects
 */

char *usage = "%s [-m master_directory] [-b boot_directory] [-s system_file]";

char *root;			/* root path prefix */
char *master_dot_d;		/* master directory */
char *slash_boot;		/* /boot directory name */
char *etcsystem;		/* system spec file */
char *kernel_master;		/* kernel master name */
char *kernel_object;		/* kernel object name */

#define CWDSIZ 1025
char *cwd;			/* current working directory */

char *ccopts;
char *ldopts;

int Debug, Verbose;

main(argc, argv)
char *argv[];
{
	register struct edt *edtp;
	struct stat statbuffer;
	extern char *optarg;
	extern int optind;
	int c;

	if ((cwd = getcwd((char *)NULL, CWDSIZ)) == (char *)NULL) {
		perror("getcwd");
		exit(2);
	}

	while ((c = getopt(argc, argv, "vm:r:b:s:")) != EOF)
		switch (c) {

/*
 * XXX -
 * Debugging code no longer works since we changed the driver structure
		case 'x':
			Debug = TRUE;
			break;
*/
		case 'v':
			Verbose = TRUE;
			break;

		case 'r':
			root = optarg;
			break;

		case 'm':
			if (optarg[0] != '/') {
				master_dot_d = mymalloc(strlen(cwd)+strlen(optarg)+4);
				strcpy(master_dot_d, cwd);
				strcat(master_dot_d, "/");
				strcat(master_dot_d, optarg);
			} else
				master_dot_d = optarg;

			if (stat(master_dot_d, &statbuffer))
				panic("cannot stat master directory");
			if (!((statbuffer.st_mode & S_IFMT) == S_IFDIR))
				panic("master directory isn\'t");

			break;

		case 's':
			if (optarg[0] != '/') {
				etcsystem = mymalloc(strlen(cwd)+strlen(optarg)+4);
				strcpy(etcsystem, cwd);
				strcat(etcsystem, "/");
				strcat(etcsystem, optarg);
			} else
				etcsystem = optarg;

			if (stat(etcsystem, &statbuffer))
				panic("cannot find system file");
			if (!((statbuffer.st_mode & S_IFMT) == S_IFREG))
				panic("system file not regular file");
			break;

		case 'b':
			if (optarg[0] != '/') {
				slash_boot = mymalloc(strlen(cwd)+strlen(optarg)+4);
				strcpy(slash_boot, cwd);
				strcat(slash_boot, "/");
				strcat(slash_boot, optarg);
			} else
				slash_boot = optarg;

			if (stat(slash_boot, &statbuffer))
				panic("cannot stat boot directory");
			if (!((statbuffer.st_mode & S_IFMT) == S_IFDIR))
				panic("boot directory isn\'t");
			break;

		case '?':
			fatal( usage, argv[0] );
		}

	if (!root)
		root = getenv("ROOT");
	if (!root)
		root = "";

	if (!slash_boot) {
		slash_boot = mymalloc(strlen(root) + 6);
		(void) strcat( strcpy(slash_boot,root), "/boot" );
	}

	if (chdir(slash_boot) == -1) {
		/* <slash_boot>: perror() message */
		error(ER7, slash_boot);
		exit(2);
	}

	if (!etcsystem) {
		etcsystem = mymalloc(strlen(root) + 12);
		(void) strcat( strcpy(etcsystem,root), "/etc/system" );
	}

	if (!master_dot_d) {
		master_dot_d = mymalloc(strlen(root) + 14);
		(void) strcat( strcpy(master_dot_d,root), "/etc/master.d" );
	}

	/*
	 * Confirm etcsystem file makes sense
	 */
	findsystem();

	/*
	 * load the boot program
	 */
	loadunix();

}


/*
 * Error(msgno, args ...)
 *
 * Print an error message, and determine the recovery action (if any) to
 * be taken.  The actions are itemized in errortab[] located in errortable.c
 */

extern struct errortab errortab[];

static int error_action();

/*VARARGS1*/
 int
error(msgnum, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
register int msgnum;
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10;
{
	register struct errortab *ep;
	register int code;
	register char *text;

	for (ep=errortab; code = ep->msgnum; ++ep) {
		if (code == msgnum) {
			/*
			 * determine recovery action
			 */
			if (((code = ep->action) & _CODE_) == _DYNAMIC_)
				code = error_action(msgnum);

			if ((text=ep->text) == NULL)
				text = "<NOTEXT>";

			if (code & _PANIC_)
				/*
				 * no deposit, no return
				 */
				panic(text);

			if (! (code & _SILENT_))
				/*
				 * either printf() or perror() will write the message
				 */
				if (code & _PERROR_)
					perror((char *)arg1);
				else {
					printf(text, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
					printf("\n");
				}

			/*
			 * take the recovery action
			 */
			switch (code & _CODE_) {
			/*
			 * the caller will do the recovery
			 */
			case _RETURN_:
			/*
			 * return FALSE
			 */
			case _RETURNFALSE_:
				return(FALSE);
			/*
			 * return TRUE
			 */
			case _RETURNTRUE_:
				return(TRUE);
			/*
			 * better fix errortab[] ...
			 */
			default:
				panic("Illegal error action");
			}
		}
	}

	panic("Unknown error number");
	/*NOTREACHED*/
}


/*
 * error_action(msgno)
 * 
 * This routine must decide what action to take for the error `msgno'.  A
 * valid action code must be returned.
 */
 static
 int
error_action(msg)
register int msg;
{
	switch (msg) {
		/* Configured for more memory than available. */
	case ER77:
		/* Device <name> previously configured on LBE (board code <n>) at ELB board code <n> */
	case ER40:
		/* Device <name> previously configured at board code <n> */
	case ER41:
		return(_RETURNTRUE_);

		/* Configured for less memory than available. */
	case ER78:
		/* Device <name> (board code <n>) not configured */
	case ER42:
		/* Device <name> (LBE <n>, board code <n>) not configured */
	case ER43:
		/* go ahead and load it */
		return(_RETURNFALSE_);
	}

	panic("error_action() failed");
	/*NOTREACHED*/
}


/*
 * Panic(message)
 */
panic(message)
char *message;
{

	printf("lboot error: %s\n\n", message);
	exit(2);

}
