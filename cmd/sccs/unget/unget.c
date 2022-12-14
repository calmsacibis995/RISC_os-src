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
#ident	"$Header: unget.c,v 1.7.2.2 90/05/09 18:48:03 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"
# include	"had.h"
# include       "sys/utsname.h"

/*
		Program can be invoked as either "unget" or
		"sact".  Sact simply displays the p-file on the
		standard output.  Unget removes a specified entry
		from the p-file.
*/

struct stat Statbuf;
char Error[128];

int	verbosity;
int	num_files;
int	cmd;
long	Szqfile;
char	had[26];
char	Pfilename[FILESIZE];
char	*auxf();
struct	packet gpkt;
struct	sid sid;
struct utsname un;
char *uuname;

main(argc,argv)
int argc;
char *argv[];
{
	int	i, testmore;
	char	c, *p;
	char	*sid_ab();
	extern	unget();
	extern	int Fcnt;

	Fflags = FTLEXIT | FTLMSG | FTLCLN;

	for(i=1; i<argc; i++)
		if(argv[i][0] == '-' && (c=argv[i][1])) {
			p = &argv[i][2];
			testmore = 0;
			switch (c) {

			case 'r':
				if (!p[0]) {
					fatal("missing SID for -r");
				}
				chksid(sid_ab(p,&sid),&sid);
				break;
			case 'n':
			case 's':
				testmore++;
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (testmore) {
				testmore = 0;
				if (*p) {
					sprintf(Error,
						"value after %c arg (cm7)",c);
					fatal(Error);
				}
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");

	/*	If envoked as "sact", set flag
		otherwise executed as "unget".
	*/
	if (equal(sname(argv[0]),"sact")) {
		cmd = 1;
		HADS = 0;
	}

	if (!HADS)
		verbosity = -1;
	setsig();
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;
	for (i=1; i<argc; i++)
		if (p=argv[i])
			do_file(p,unget);
	exit(Fcnt ? 1 : 0);
}


unget(file)
char *file;
{
	extern	char had_dir, had_standinp;
	extern	char *Sflags[];
	int	i, status;
	char	gfilename[FILESIZE];
	char	str[BUFSIZ];
	char	*sid_ba();
	struct	pfile *pp, *edpfile();

	if (setjmp(Fjmp))
		return;

	/*	Initialize packet, but do not open SCCS file.
	*/
	sinit(&gpkt,file,0);
	gpkt.p_stdout = stdout;
	gpkt.p_verbose = verbosity;

	copy(auxf(gpkt.p_file,'g'),gfilename);
	if (gpkt.p_verbose && (num_files > 1 || had_dir || had_standinp))
		fprintf(gpkt.p_stdout,"\n%s:\n",gpkt.p_file);
	/*	If envoked as "sact", call catpfile() and return.
	*/
	if (cmd) {
		catpfile(&gpkt);
		return;
	}
	uname(&un);
	uuname = un.nodename;
	if (lockit(auxf(gpkt.p_file,'z'),2,getpid(),uuname))
		fatal("cannot create lock file (cm4)");
	pp = edpfile(&gpkt,&sid);
	if (gpkt.p_verbose) {
		sid_ba(&pp->pf_nsid,str);
		fprintf(gpkt.p_stdout,"%s\n",str);
	}

	/*	If the size of the q-file is greater than zero,
		rename the q-file the p-file and remove the
		old p-file; else remove both the q-file and
		the p-file.
	*/
	if (Szqfile)
		rename(auxf(gpkt.p_file,'q'),Pfilename);
	else {
		xunlink(Pfilename);
		xunlink(auxf(gpkt.p_file,'q'));
	}
	ffreeall();
	uname(&un);
	uuname = un.nodename;
	unlockit(auxf(gpkt.p_file,'z'),getpid(),uuname);

	/*	A child is spawned to remove the g-file so that
		the current ID will not be lost.
	*/
	if (!HADN) {
		if ((i = fork()) < 0)
			fatal("cannot fork, try again");
		if (i == 0) {
			setuid(getuid());
			unlink(gfilename);
			exit(0);
		}
		else {
			wait(&status);
		}
	}
}


struct pfile *
edpfile(pkt,sp)
struct packet *pkt;
struct sid *sp;
{
	static	struct pfile goodpf;
	char	*user, *logname();
	char	line[BUFSIZ];
	struct	pfile pf;
	int	cnt, name;
	FILE	*in, *out, *fdfopen();

	cnt = -1;
	name = 0;
	user = logname();
	zero(&goodpf,sizeof(goodpf));
	in = xfopen(auxf(pkt->p_file,'p'),0);
	out = xfcreat(auxf(pkt->p_file,'q'),0644);
	while (fgets(line,sizeof(line),in) != NULL) {
		pf_ab(line,&pf,1);
		if (equal(pf.pf_user,user)) {
			name++;
			if (sp->s_rel == 0) {
				if (++cnt) {
					fclose(out);
					fclose(in);
					fatal("SID must be specified (un1)");
				}
				goodpf = pf;
				continue;
			}
			else if (sp->s_rel == pf.pf_nsid.s_rel &&
				sp->s_lev == pf.pf_nsid.s_lev &&
				sp->s_br == pf.pf_nsid.s_br &&
				sp->s_seq == pf.pf_nsid.s_seq) {
					goodpf = pf;
					continue;
			}
		}
		fputs(line,out);
	}
	fflush(out);
	fstat(fileno(out),&Statbuf);
	Szqfile = Statbuf.st_size;
	copy(auxf(pkt->p_file,'p'),Pfilename);
	fclose(out);
	fclose(in);
	if (!goodpf.pf_user[0])
		if (!name)
			fatal("login name not in p-file (un2)");
		else fatal("specified SID not in p-file (un3)");
	return(&goodpf);
}


/* clean_up() only called from fatal().
*/
clean_up(n)
{
	/*	Lockfile and q-file only removed if lockfile
		was created by this process.
	*/
	uname(&un);
	uuname = un.nodename;
	if (mylock(auxf(gpkt.p_file,'z'),getpid(),uuname)) {
		unlink(auxf(gpkt.p_file,'q'));
		ffreeall();
		unlockit(auxf(gpkt.p_file,'z'),getpid(),uuname);
	}
}


catpfile(pkt)
struct packet *pkt;
{
	int c;
	FILE *in;

	if(!(in = fopen(auxf(pkt->p_file,'p'),"r")))
		fprintf(stderr,"No outstanding deltas for: %s\n",pkt->p_file);
	else {
		while ((c = getc(in)) != EOF)
			putc(c,pkt->p_stdout);
		fclose(in);
	}
}
