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
#ident	"$Header: subr.c,v 1.14.2.2 90/05/09 16:19:18 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <sys/types.h>
#include <a.out.h>
#include "lboot.h"
#include "boothdr.h"
#include <sys/sysmacros.h>
#include <ctype.h>
#include "error.h"

/*
 * Static function declarations for this file
 */
static char    *do_kernel();
static char    *do_device();
static char    *do_exclude();
static char    *do_include();
static char    *do_swapdevice();
static char    *do_vector();
#ifdef ATBUS
static char    *do_atbus();
#endif
static char    *do_ccopts();
static char    *do_ldopts();
static char    *interpret();
static boolean  numeric();
static int      parse();
static void     prompt();

/* 
 * This struct is used by BumpNCat mainly to consolidate the argument list.
 */
typedef struct {
    char *m_str;
    int m_siz;	/* current length of the string */
    int m_max;	/* number of bytes in the string */
} mem_t;


/*
 * Findsystem()
 *
 * This routine will ask for the name of the /etc/system file if the autoboot
 * switch is not set.  If a directory name is given, then the contents will be
 * listed as though "ls -p | pr -t -5 -w80" was issued.
 */
 void
findsystem()
{
	register char *sp;
	register int fd;
	char path[100];
	struct stat stat_system;
	int state = 0;

	/*
	 * stat /etc/system; if the file does not exist, then set st_ino
	 * to zero in the stat structure (no valid inode number is zero)
	 */
	if (stat(etcsystem,&stat_system) == -1)
		stat_system.st_ino = 0;

	/*
	 * we don't leave this routine until we get either a valid system
	 * file name or a valid boot program name
	 */
	while (TRUE) {
		/*
		 * Set state:	0 if first time here
		 *	      >=1 if third (or later) time here
		 */

		switch (state) {

		/*
		 * use /etc/system to do a full self-config boot if possible
		 */
		case 0:
			state += 1;
			if (stat_system.st_ino == 0) {
				/* <system> does not exist */
				error(ER51, etcsystem);
				continue;
			}
			sp = etcsystem;
			break;

		/*
		 * prompt for system file name
		 */
		default:
			while (TRUE) {
				printf("\nEnter path name of system file: ");

				/*
				 * read input from console
				 */
				if (!gets(path))
					continue;

				if ((sp=strtok(path,"\r\n\t ")) == NULL) {
					ls("/");
					continue;
				}

				break;
			}
		}

		/*
		 * we've got a path name now; it must be examined to determine
		 * what kind of file it is
		 */
		if (stat(sp,&stat_system) == -1) {
			/* <sp>: perror() message */
			error(ER7, sp);
			continue;
		}

		if ((stat_system.st_mode & S_IFMT) == S_IFDIR) {
			ls(sp);
			continue;
		}

		if ((fd=open(sp,O_RDONLY)) == -1) {
			/* <sp>: perror() message */
			error(ER7, sp);
			continue;
		}


		if ((stat_system.st_mode & S_IFMT) == S_IFREG) {
			unsigned short magic;

			if (stat_system.st_size <= sizeof(magic)) {
				/*
				 * assume /etc/system file since its obviously not an object file;
				 * it will probably cause a syntax error later, but that's their
				 * problem
				 */
				if (etcsystem) free(etcsystem);
				etcsystem = mymalloc(strlen(sp)+1);
				strcpy(etcsystem, sp);
				break;
			}

			read_and_check(fd, (char*)&magic, sizeof(magic));

			if (isascii(((char*)&magic)[0]) &&
			    (isprint(((char*)&magic)[0]) || isspace(((char*)&magic)[0])))
				if (isascii(((char*)&magic)[1]) &&
				    (isprint(((char*)&magic)[1]) || isspace(((char*)&magic)[1]))) {
					/*
					 * assume /etc/system file
					 */
					if (etcsystem) free(etcsystem);
					etcsystem = mymalloc(strlen(sp)+1);
					strcpy(etcsystem, sp);
					break;
			}
		}

		/* <sp>: not object file and not ascii text file */
		error(ER52, sp);

		/*
		 * bad file, try again
		 */
	badfile:
		close(fd);
	}

	close(fd);
}

/*
 * Fsystem(stream)
 * System(string)
 *
 * Read the /etc/system file, parse and extract all the necessary information.
 * The stream is either an actual file, or it is a memory image.
 */
 void
System(string)
char *string;
{
	FILE strbuf;

	strbuf._flag = _IOREAD;
	strbuf._ptr = strbuf._base = (unsigned char *) string;
	strbuf._cnt = strlen((char*)strbuf._ptr);
	strbuf._file = -1;
	fsystem(&strbuf);
}

 void
fsystem(stream)
register FILE *stream;
{
	char line[256];
	register char *msg;
	register lineno = 1;

	while ((int) fgets(line,sizeof(line),stream) != NULL) {

		if (strlen(line) == sizeof(line)-1) {
			/* System: line too long */
			error((stream->_flag&_IOMYBUF)? ER68 : ER69, lineno);
			continue;
		}

		if (line[0] != '*' && ! blankline(line))
			if ((msg=interpret(line)) != NULL) {
				if (stream->_flag & _IOMYBUF)
					/* System: line <lineno>; <msg> */
					error(ER53, lineno, msg);
				else
					/* System: <msg> */
					error(ER54, msg);
			}

		++lineno;
	}
}

/*
 * Interpret(line)
 *
 * Interpret the line from the /etc/system file
 */


struct	syntax {
	char	*keyword;
	char	*(*process)();
	char	*argument;
};

static struct syntax syntax[] ={
			{ "KERNEL", do_kernel, 0 },
			{ "EXCLUDE", do_exclude, 0 },
			{ "INCLUDE", do_include, 0 },
			{ "DUMPDEV", do_device, (char*) &dumpdev },
			{ "ROOTDEV", do_device, (char*) &rootdev },
			{ "SWAPDEV", do_swapdevice, 0 },
			{ "PIPEDEV", do_device, (char*) &pipedev },
			{ "VECTOR", do_vector, 0 },
#ifdef ATBUS
			{ "ATBUS", do_atbus, 0 },
#endif
			{ "CCOPTS", do_ccopts, 0 },
			{ "LDOPTS", do_ldopts, 0 },
				0 };

 static
 char *
interpret(line)
char *line;
{
	register int largc;
	char *largv[50];
	register struct syntax *p;

	if ((largc=parse(largv,sizeof(largv)/sizeof(largv[0]),line)) > sizeof(largv)/sizeof(largv[0]))
		return("line too long");

	if (largc == 0)
		return(NULL);

	if (largc == 1 || *largv[1] != ':')
		return("syntax error");

	for (p=syntax; p->keyword; ++p) {
		if (0 == strcmp(*largv,p->keyword)) {
			if (largc == 2)
				return(NULL);
			else
				return((*(p->process))(largc-2, &largv[2], p->argument));
		}
	}

	return("syntax error");
}

/*
 * Parse(_argv, sizeof(_argv), line)
 *
 * Parse a line from the /etc/system file; _argc is returned, and _argv is
 * initialized with pointers to the elements of the line.  The contents
 * of the line are destroyed.
 */
 static
 int
parse(_argv, sizeof_argv, line)
char **_argv;
unsigned sizeof_argv;
register char *line;
{
	register char **argvp = _argv;
	register char **maxvp = _argv + sizeof_argv;
	register c;

	while (c = *line) {
		switch (c) {
		/*
		 * white space
		 */
		case ' ':
		case '\t':
		case '\r':
			*line++ = '\0';
			line += strspn(line," \t\r");
			continue;
		/*
		 * special characters
		 */
		case ':':
			*line = '\0';
			*argvp++ = ":";
			++line;
			break;
		case ',':
			*line = '\0';
			*argvp++ = ",";
			++line;
			break;
		case '(':
			*line = '\0';
			*argvp++ = "(";
			++line;
			break;
		case ')':
			*line = '\0';
			*argvp++ = ")";
			++line;
			break;
		case '?':
			*line = '\0';
			*argvp++ = "?";
			++line;
			break;
		case '=':
			*line = '\0';
			*argvp++ = "=";
			++line;
			break;
		/*
		 * end of line
		 */
		case '\n':
			*line = '\0';
			*argvp = NULL;
			return(argvp - _argv);
		/*
		 * words and numbers
		 */
		default:
			*argvp++ = line;
			line += strcspn(line,":,()?= \t\r\n");
			break;
		}

		/*
		 * don't overflow the argv array
		 */
		if (argvp >= maxvp)
			return(sizeof_argv + 1);
	}

	*argvp = NULL;
	return(argvp - _argv);
}

/*
 * prompt(message)
 */
 static
 void
prompt(message)
char *message;
{
	char input[256];
	register char *sp;

	while (TRUE) {
		printf("%s ? ", strcpy(input,message));

		errno = 0;
		if ((scanf("%s",sp=input+strlen(input))) != EOF) {
			if (errno)
				perror("prompt: scanf");

			if (blankline(sp))
				/* empty line */
				break;

			if ((sp=interpret(input)) == NULL)
				break;
		}

		printf("%s; re-enter ", sp);
	}
}

/*
 * Blankline(line)
 *
 * Return TRUE if the line is entirely blank or null; return FALSE otherwise.
 */
 boolean
blankline(line)
register char *line;
{
	return(strspn(line," \t\r\n") == strlen(line));
}


/*
 * Numeric(assign, string)
 *
 * If the string is a valid numeric string, then set *assign to its numeric
 * value and return TRUE; otherwise return FALSE.
 */
 static
 boolean
numeric(assign, string)
register int *assign;
char *string;
{
	register long value;
	char *next;

	value = strtol(string, &next, 0);

	if (*next)
		/*
		 * bad number
		 */
		return(FALSE);

	*assign = value;

	return(TRUE);
}

/*
 * Do_??????(_argc, _argv, optional)
 *
 * Handle the processing for each type of line in /etc/system
 */


/*
 * KERNEL: master
 */
 static
 char *
do_kernel(_argc, _argv)
int _argc;
char **_argv;
{
	register type;

	if (_argc > 1)
		return("syntax error");

	if (*_argv[0] == '?') {
		prompt("KERNEL:");
		return(NULL);
	}

	if (kernel_master)
		free(kernel_master);

	kernel_master=mymalloc(	strlen(*_argv) + 1);
	(void) strcpy(kernel_master,*_argv);

	if ((kernel->opthdr = mkboot(kernel_master)) == 0)
		return("not a master file");

	if (!(kernel->opthdr->flag & KOBJECT))
		return("not a kernel master file");

	return(NULL);
}


/*
 * EXCLUDE: driver ...
 */
 static
 char *
do_exclude(_argc, _argv)
register int _argc;
register char **_argv;
{
	while (_argc-- > 0) {
		if (*_argv[0] != ',')
			/* not comma */
			if (*_argv[0] == '?')
				prompt("EXCLUDE:");
			else
				exclude(*_argv);

		++_argv;
	}

	return(NULL);
}


/*
 * INCLUDE: driver(optional-number) ...
 */
 static
 char *
do_include(_argc, _argv)
register int _argc;
register char **_argv;
{
	register char *p;
	int n;

	while (_argc > 0) {

		if (*_argv[0] == ',') {
			--_argc;
			++_argv;
			continue;
		}

		if (*_argv[0] == '?') {
			prompt("INCLUDE:");
			--_argc;
			++_argv;
			continue;
		}

		p = *_argv;
		n = 1;

		if (_argc >= 4 && *_argv[1] == '(') {
			if (*_argv[3] != ')')
				return("syntax error");

			if (! numeric(&n, _argv[2]))
				return("count must be numeric");

			_argc -= 3;
			_argv += 3;
		}
		else
			if (*p == '(' || *p == ')')
				return("syntax error");

		include(p, n);

		--_argc;
		++_argv;
	}

	return(NULL);
}


/*
 * DUMPDEV: { path | DEV(number,number) }
 * ROOTDEV: { path | DEV(number,number) }
 * PIPEDEV: { path | DEV(number,number) }
 */
 static
 char *
do_device(_argc, _argv, device)
register int _argc;
register char **_argv;
register dev_t *device;
{
	struct stat statbuf;
	register type;
	int M, m;

	if (_argc == 1) {
		/*
		 * path
		 */
		if (*_argv[0] == '?') {
			char what[10];

			/*
			 * we cheat here; we know that interpret() calls this
			 * routine with _argv+2; therefore, _argv[-2] is the original
			 * statement type
			 */
			strcat(strcpy(what,_argv[-2]), ":");

			prompt(what);
			return(NULL);
		}

		if (stat(*_argv,&statbuf) == -1)
			return("no such file");
		else
			if ((type=statbuf.st_mode&S_IFMT) != S_IFCHR && type != S_IFBLK)
				return("file not BLOCK or CHAR special");

		*device = statbuf.st_rdev;

		return(NULL);
	}

	if (_argc != 6 || 0 != strcmp("DEV",_argv[0]) || *_argv[1] != '(' || *_argv[3] != ',' || *_argv[5] != ')')
		return("syntax error");
	
	/*
	 * DEV(number,number)
	 */

	if (! numeric(&M,_argv[2]) || ! numeric(&m,_argv[4]))
		return("major/minor must be numeric");

	*device = makedev(M, m);

	return(NULL);
}


/*
 * SWAPDEV: { path | DEV(number,number) }  swplo  nswap
 */
 static
 char *
do_swapdevice(_argc, _argv)
register int _argc;
register char **_argv;
{
	register char *p;

	if (_argc == 3) {
		/*
		 * path swplo nswap
		 *
		 * swapdev = path
		 * swplo = number
		 * nswap = number
		 */
		if (*_argv[1] == '=')
			/*
			 * internal prompt response
			 */
			{
			if (0 == strcmp("swapdev",_argv[0]))
				return(do_device(1,&_argv[2],&swapdev));

			if (0 == strcmp("swplo",_argv[0]))
				{
				int temp;

				if (! numeric(&temp,_argv[2]))
					return("must be numeric");

				swplo = temp;
				}

			if (0 == strcmp("nswap",_argv[0]))
				if (! numeric(&nswap,_argv[2]))
					return("must be numeric");

			return(NULL);
			}

		if (*_argv[0] == '?')
			prompt("SWAPDEV: swapdev=");
		else
			if ((p=do_device(1,_argv,&swapdev)) != NULL)
				return(p);

		--_argc;
		++_argv;
	} else if (_argc == 8) {
		/*
		 * DEV(number,number) swplo nswap
		 *
		 * swapdev = DEV(number,number)
		 */
		if (*_argv[1] == '=')
			/*
			 * internal prompt response
			 */
			{
			if (0 == strcmp("swapdev",_argv[0]))
				return(do_device(6,&_argv[2],&swapdev));
			}

		if ((p=do_device(6,_argv,&swapdev)) != NULL)
			return(p);

		_argc -= 6;
		_argv += 6;
	} else
		return("syntax error");

	if (*_argv[0] == '?')
		prompt("SWAPDEV: swplo=");
	else {
		int temp;

		if (! numeric(&temp,_argv[0]))
			return("must be numeric");

		swplo = temp;
	}

	if (*_argv[1] == '?')
		prompt("SWAPDEV: nswap=");
	else
		if (! numeric(&nswap,_argv[1]))
			return("must be numeric");

	return(NULL);
}

struct t_edt {
	paddr_t e_base;
	char *v_ifcn;
	unsigned char v_vec;
	unsigned char v_ipl;
	unsigned char v_unit;
};

#define freet(t) \
	if ((t).v_ifcn) free ((t).v_ifcn);

/*
 * VECTOR: module [intr] [ipl vector unit base] [probe [probe_size]]
 */
 static char *
do_vector(_argc, _argv)
register int _argc;
register char *_argv[];
{
	extern int errno;
	struct driver *dp = 0;
	struct master *mp;
	struct edt *ep;
	struct vme_intrs *vp;
	paddr_t probe_addr = 0;
	int probe_size = 4;
	int fd;
	char pbuf[16];
	struct t_edt t_edt;

	t_edt.e_base = 0;
	t_edt.v_ifcn = 0;
	t_edt.v_vec = 0;
	t_edt.v_ipl = 0;
	t_edt.v_unit = 0;

	while (_argc > 0) {
		if (EQUAL("module",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2) || !(dp=searchdriver(_argv[2]))) {
				freet(t_edt);
				if (! ignoredt(_argv[2]))
					error(ER38, _argv[2]);
				return("VECTOR: module = ???");
			}
			_argc -= 3, _argv += 3;

		} else if (EQUAL("intr",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: intr = ???");
			}
			t_edt.v_ifcn=strcpy(mymalloc(strlen(_argv[2])+1),_argv[2]);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("ipl",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: ipl = ???");
			}
			t_edt.v_ipl = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("vector",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: vector = ???");
			}
			t_edt.v_vec = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("unit",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: unit = ???");
			}
			t_edt.v_unit = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("base",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: base = ???");
			}
			t_edt.e_base = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("probe",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: probe = ???");
			}
			probe_addr = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("probe_size",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("VECTOR: probe_size = ???");
			}
			probe_size = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else {
			freet(t_edt);
			return("VECTOR: unknown specifier");
		}
	}

	if (!dp) {
		freet(t_edt);
		return("VECTOR: missing module name");
	}

	if (probe_addr) {
		fd = open("/dev/kmem", O_RDONLY);
		if (fd == -1)
			panic("Could not read /dev/kmem to probe devices.");

		errno = 0;
		lseek(fd, probe_addr, 0);
		(void) read(fd, pbuf, probe_size);
		if (errno) {
			close(fd);
			freet(t_edt);
			return(NULL);
		}
		close(fd);
	}

	mp = dp->opthdr;
	dp->flag |= INEDT;
	++dp->nctl;

	/*
	 * see if there is edt structure existant to which to attach this
	 */
	for (ep=dp->edtp; ep; ep=ep->e_next) {
		if (t_edt.e_base == ep->e_base) {
#ifdef MULTIPLE_MAJOR
		    /*
		     * The reason we decrement is so that we obtain the
		     * correct number of controllers.  If the base matches
		     * a previous entry, we know we are talking about the
		     * same controller.
		     */
		        --dp->nctl;
#endif
			break;
		    }
	}
	if (!ep) {
		ep = (struct edt *)calloc(sizeof(struct edt),1);
		if (ep == (struct edt *) 0)
			panic ("cannot malloc edt structure");
		ep->e_next = dp->edtp;
		dp->edtp = ep;
		ep->e_base = t_edt.e_base;
		vp = (struct vme_intrs *)ep;
	} else {
		for (vp=(struct vme_intrs *)ep; vp->v_next; vp=vp->v_next)
			;
	}

	/*
	 * funny case -- an edt without a interrupt routine
	 *  we build the edt with a null vme_intrs pointer
	 */
	if (!t_edt.v_vec)
		return(NULL);
	vp->v_next = (struct vme_intrs *)calloc(sizeof(struct vme_intrs),1);
	if (vp->v_next == (struct vme_intrs *) 0)
		panic ("cannot malloc edt structure");
	vp = vp->v_next;
	if (t_edt.v_ifcn)
		vp->v_vname = t_edt.v_ifcn;
	else
		/*
		 * use prefix|"intr" if interrupt function not specified
		 * use tag|"intr" if interrupt function not specified ???
		 */
		vp->v_vname = strcat(strcpy(mymalloc(strlen(mp->prefix)+5),mp->prefix),"intr");
	vp->v_vec = t_edt.v_vec;
	vp->v_brl = t_edt.v_ipl;
	vp->v_unit = t_edt.v_unit;

	return(NULL);
}

#ifdef ATBUS

/*
 * ATBUS: module [intr] irq [unit base]
 */
 static char *
do_atbus(_argc, _argv)
register int _argc;
register char *_argv[];
{
	extern int errno;
	struct driver *dp = 0;
	struct master *mp;
	struct edt *ep;
	struct atbus_intrs *ap;
	paddr_t probe_addr = 0;
	int probe_size = 4;
	int fd;
	char pbuf[16];
	struct t_edt t_edt;

	t_edt.e_base = 0;
	t_edt.v_ifcn = 0;
	t_edt.v_vec = 0;
	t_edt.v_ipl = 0;
	t_edt.v_unit = 0;

	while (_argc > 0) {
		if (EQUAL("module",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2) || !(dp=searchdriver(_argv[2]))) {
				freet(t_edt);
				if (! ignoredt(_argv[2]))
					error(ER38, _argv[2]);
				return("ATBUS: module = ???");
			}
			_argc -= 3, _argv += 3;

		} else if (EQUAL("intr",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("ATBUS: intr = ???");
			}
			t_edt.v_ifcn=strcpy(mymalloc(strlen(_argv[2])+1),_argv[2]);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("irq",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("ATBUS: irq = ???");
			}
			t_edt.v_ipl = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("unit",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("ATBUS: unit = ???");
			}
			t_edt.v_unit = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else if (EQUAL("base",_argv[0])) {
			if (!(EQUAL("=",_argv[1])) || (_argc < 2)) {
				freet(t_edt);
				return("ATBUS: base = ???");
			}
			t_edt.e_base = strtol(_argv[2], (char *)0, 0);
			_argc -= 3, _argv += 3;

		} else {
			freet(t_edt);
			return("ATBUS: unknown specifier");
		}
	}

	if (!dp) {
		freet(t_edt);
		return("VECTOR: missing module name");
	}

	mp = dp->opthdr;
	dp->flag |= INEDT;
	++dp->nctl;

	/*
	 * see if there is edt structure existant to which to attach this
	 */
	for (ep=dp->edtp; ep; ep=ep->e_next) {
		if (t_edt.e_base == ep->e_base)
			break;
	}
	if (!ep) {
		ep = (struct edt *)calloc(sizeof(struct edt),1);
		if (ep == (struct edt *) 0)
			panic ("cannot malloc edt structure");
		ep->e_next = dp->edtp;
		dp->edtp = ep;
		ep->e_base = t_edt.e_base;
		ap = (struct atbus_intrs *)ep;
	} else {
		for (ap=(struct atbus_intrs *)ep; ap->a_next; ap=ap->a_next)
			;
	}

	/*
	 * funny case -- an edt without a interrupt routine
	 *  we build the edt with a null vme_intrs pointer
	 */
	ap->a_next = (struct atbus_intrs *)calloc(sizeof(struct atbus_intrs),1);
	if (ap->a_next == (struct atbus_intrs *) 0)
		panic ("cannot malloc edt structure");
	ap = ap->a_next;
	if (t_edt.v_ifcn)
		ap->a_aname = t_edt.v_ifcn;
	else
		/*
		 * use prefix|"intr" if interrupt function not specified
		 * use tag|"intr" if interrupt function not specified ???
		 */
		ap->a_aname = strcat(strcpy(mymalloc(strlen(mp->prefix)+5),mp->prefix),"intr");
	ap->a_irq = t_edt.v_ipl;
	ap->a_unit = t_edt.v_unit;

	return(NULL);
}
#endif ATBUS

/*
 * CCOPTS: strings...
 *
 * ``deparse'' CCOPTS string into ccopts[]
 */
static mem_t ccopts_mem;
 static char *
do_ccopts(_argc, _argv)
register int _argc;
register char **_argv;
{
	ccopts_mem.m_str = ccopts;

	while (_argc--) {
		BumpNCat (&ccopts_mem, _argv[0]);
		if ( _argc && (_argv[1][0] == '=' ) ) {
			_argv++; _argc--;
			BumpNCat(&ccopts_mem, _argv[0]);
			if (_argc) {
				_argv++; _argc--;
				BumpNCat(&ccopts_mem, _argv[0]);
			}
		}
		_argv++;
		BumpNCat(&ccopts_mem, " ");
	}

	ccopts = ccopts_mem.m_str;
	return(NULL);
}

/*
 * LNOPTS: strings...
 *
 * ``deparse'' LDOPTS string into ldopts[]
 */
static mem_t ldopts_mem;
 static char *
do_ldopts(_argc, _argv)
register int _argc;
register char **_argv;
{
	ldopts_mem.m_str = ldopts;

	while (_argc--) {
		if (EQUAL(_argv[0],"-o") && _argc>0) {
			BumpNCat(&ldopts_mem, _argv[0]);
			BumpNCat(&ldopts_mem, " ");
			_argv++;
			_argc--;
			if (_argv[0][0] != '/') {
				BumpNCat(&ldopts_mem, cwd);
				BumpNCat(&ldopts_mem, "/");
			}
		}
		BumpNCat(&ldopts_mem, _argv[0]);
		BumpNCat(&ldopts_mem, " ");
		_argv++;
	}

	ldopts = ldopts_mem.m_str;
	return(NULL);
}

#define BUMPS_FIRST_MALLOC 1024

/* 
 * Allocate space as neccessary for the 'to' string then copy
 * the from string into place
 */
BumpNCat(mp, from)
    register mem_t *mp;
    register char *from;
{
    int len;
    char *malloc(), *realloc(), *t;

    if (!mp->m_str) {
	mp->m_str = malloc(BUMPS_FIRST_MALLOC);
	if(!mp->m_str)
	    return 1;
	mp->m_max = BUMPS_FIRST_MALLOC;
	mp->m_siz = 0;	/* saftey */
    }
    
    /*
     * At this point if there's not enough space left just realloc
     * the current amount plus the new string length.  This has the
     * side effect of allocating up to n-1 extra bytes.  Most likely
     * the next call will use this extra space.
     */
    len = strlen (from);
    if (len > (mp->m_max - mp->m_siz)) {
	t = realloc(mp->m_str, mp->m_max + len + 1);
	if (!t) {
	    return 1;
	}
	mp->m_str = t;
	mp->m_max += len;
    }

    strcat (mp->m_str, from);
    mp->m_siz = strlen (mp->m_str);

    return 0;
}



/*
 *	Ls(dirname)
 *
 *	This routine prints the names of all allocated files in the directory
 *	named by dirname.  Names are printed in alphabetic order.
 */

#define	SIZELINE	80
#define	SIZENAME	(DIRSIZ+2)

struct	list
	{
	char	name[DIRSIZ+1+1];
	};

 void
ls(dir)
char *dir;
{
	register struct dirent *d;
	register struct list *list, *lp;
	register count;
	DIR *dirp;
	struct stat statbuf;
	char fullname[100+DIRSIZ];
	char line[SIZELINE+1];

	printf("Files in %s are:\n", dir);

	if (strlen(dir)+1+DIRSIZ >= 100) {
		printf("%s: pathname too long\n", dir);
		return;
	}

	if (stat(dir,&statbuf) == -1) {
		/* <dir>: perror() message */
		error(ER7, dir);
		return;
	}

	if ((list=lp=(struct list *)malloc(((unsigned)statbuf.st_size/sizeof(struct dirent))*sizeof(struct list))) == NULL) {
		printf("No memory for directory list\n");
		return;
	}

	if (!(dirp=opendir(dir))) {
		/* <dir>: perror() message */
		error(ER7, dir);
		return;
	}

	if ((d=readdir(dirp))) {
		count = 0;

		do	{
			/*
			 * skip directory items '.', '..' 
			 */
			if (d->d_name[0]=='.') {
				if (d->d_name[1]=='\0')
					continue;
				if (d->d_name[1]=='.' && d->d_name[2]=='\0')
					continue;
			}

			strcat(strcat(strcpy(fullname,dir),"/"), strncat(strcpy(lp->name,""),d->d_name,DIRSIZ));

			if (stat(fullname, &statbuf) == -1) {
				/* <fullname>: perror() message */
				error(ER7, fullname);
				continue;
			}

			if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
				strcat(lp->name, "/");

			++lp;
			++count;

		} while ((d=readdir(dirp)) != NULL);

		closedir(dirp);

		/*
		 * sort the names
		 */
		{
			register i, m;

			for (m=count/2; m>0; m/=2) {
				for (i=0; i<count-m; ++i) {
					lp = &list[i];

					if (strcmp(lp[0].name,lp[m].name) > 0) {
						struct list temp;

						temp = lp[m];

						do	{
							lp[m] = lp[0];
							}
							while ((lp-=m) >= list && strcmp(lp[0].name,temp.name) > 0);

						lp[m] = temp;
					}
				}
			}
		}

		/*
		 * produce the display
		 */
		strcpy(line, "");

		for (lp=list; lp < &list[count]; ++lp) {
			strcat(line, lp->name);
			strncat(line, "              ", SIZENAME-strlen(lp->name));

			if (strlen(line) >= SIZELINE-SIZENAME) {
				printf("%s\n", line);
				strcpy(line, "");
			}
		}

		if (strlen(line) > 0)
			printf("%s\n", line);
	}

	free(list);
}


#ifndef mips
/*
 * Edtscan(base, lba, function)
 *
 * Search the EDT, calling function() for each EDT entry.
 *
 * Function() is called with &edt_entry.
 *
 */
 void
edtscan(function)
register int (*function)();
{
	register struct edt *edtp;
	int i;

	for (i=0, edtp=base; i < NUM_EDT; ++i) {
		elb = edtp->opt_slot;

		(*function)(edtp++, lba, elb);
	}
}
#endif not mips

/*
 * Read and seek routines with error checking.  Condition IOERROR is signalled
 * if an I/O error occurs.
 */

CONDITION IOERROR ={ "IOERROR" };


/*
 * Read with error checking; if an I/O error occurs, condition IOERROR is
 * signalled.
 */
 void
read_and_check(fildes, buf, nbyte)
int fildes;
char *buf;
register unsigned nbyte;
{
	register int rbyte;

	if ((rbyte = read(fildes,buf,nbyte)) == -1) {
		/* <filename>: perror() message */
#ifndef	mips
		error(ER7, filename(fildes));
#else	mips
		error(ER7, "read_and_check");
#endif	mips
		SIGNAL(IOERROR);

	} else if (rbyte != nbyte) {

		/* <filename>: truncated read */
#ifndef	mips
		error(ER49, filename(fildes));
#else	mips
		error(ER49, "read_and_check");
#endif	mips
		SIGNAL(IOERROR);
	}
}


/*
 * Lseek with error checking; if an I/O error occurs, condition IOERROR is
 * signalled.
 */
 void
seek_and_check(fildes, foffset, whence)
int fildes;
long foffset;
int whence;
{
	if (lseek(fildes,foffset,whence) == -1L) {
		/* <filename>: perror() message */
#ifndef	mips
		error(ER7, filename(fildes));
#else	mips
		error(ER7, "seek_and_check");
#endif	mips
		SIGNAL(IOERROR);
	}
}

/*
 * Itoa(number)
 *
 * Convert number to right adjusted ascii string; pointer returned is to
 * beginning of char [15] array, with result right adjusted and left padded
 * with blanks (and an optional minus sign).
 */
 char *
itoa(number)
register int number;
{
	static char buffer[15+1];
	register boolean minus;
	register char *p;

	if (!(minus = number<0))
		/*
		 * compute using negative numbers to avoid problems at MAXINT
		 */
		number = -number;

	*(p=buffer+sizeof(buffer)-1) = '\0';

	do {
		*--p = '0' - (number%10);
	} while ((number/=10) < 0);

	if (minus)
		*--p = '-';

	while (p > buffer)
		*--p = ' ';

	return(buffer);
}

/*
 * Function(prefix, name, symbol)
 *
 * Test whether the symbol is prefix-name
 */
 boolean
function(prefix, name, symbol)
register char *prefix;
char *name;
char *symbol;
{
	register len = strlen(prefix);

	return(0 == strncmp(prefix,symbol,len) &&
	       0 == strcmp(name,&symbol[len]));
}

/*
 * Bzero(addr, size)
 *
 * Set `size' bytes of memory beginning at `addr' to zeros
 */
 void
bzero(addr, size)
register caddr_t addr;
register size;
{
	while (size-- > 0)
		*addr++ = 0;
}

/*
 * Suffix (string, suffix)
 * Given "string" and 'suffix', suffix()
 * returns true if "string" ends with .suffix
 */
suffix(string, suffix)
char *string, *suffix;
{
	char *dotpos;

	if ((dotpos = strrchr(string, '.')) == NULL)
		return(0);
	if (EQUAL(++dotpos, suffix))
		return(1);
	return(0);
}
