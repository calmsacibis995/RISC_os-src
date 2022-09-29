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
#ident	"$Header: pcnfsd.c,v 1.3.1.3 90/05/09 19:15:06 wje Exp $"

/*
 * Copyright (c) 1986 by Sun Microsystems, Inc. 
 */

/*
 * rpc.pcnfsd.c 
 *
 * pcnfsd is intended to remedy the lack of certain critical generic network
 * services by providing an simple, customizable set of RPC-based
 * mechanisms. For this reason, Sun Microsystems Inc. is distributing it
 * in source form as part of the PC-NFS release. 
 *
 * Background: The first NFS networks were composed of systems running
 * derivatives of the 4.2BSD release of Unix (Sun's, VAXes, Goulds and
 * Pyramids). The immediate utility of the resulting networks was derived
 * not only from NFS but also from the availability of a number of TCP/IP
 * based network services derived from 4.2BSD. Furthermore the thorny
 * question of network-wide user authentication, while remaining a
 * security hole, was solved at least in terms of a convenient usage model
 * by the Yellow Pages distributed data base facility, which allows
 * multiple Unix systems to refer to common password and group files. 
 *
 * The PC-NFS Dilemma: When Sun Microsystems Inc. ported NFS to PC's, two
 * things became apparent. First, the memory constraints of the typical PC
 * meant that it would be impossible to incorporate the pervasive TCP/IP
 * based service suite in a resident fashion. Indeed it was not at all
 * clear that the 4.2BSD services would prove sufficient: with the advent
 * of Unix System V and (experimental) VAX-VMS NFS implementations, we had
 * to consider the existence of networks with no BSD-derived Unix hosts.
 * The two key types of functionality we needed to provide were remote
 * login and print spooling. The second critical issue  was that of user
 * authentication. Traditional time-sharing systems such as Unix and VMS
 * have well- established user authentication mechanisms based upon user
 * id's and passwords: by defining appropriate mappings, these could
 * suffice for network-wide authentication provided that appropriate
 * administrative procedures were enforced. The PC, however, is typically
 * a single-user system, and the standard DOS operating environment
 * provides no user authentication mechanisms. While this is acceptable
 * within a single PC, it causes problems when attempting to connect to a
 * heterogeneous network of systems in which access control, file space
 * allocation, and print job accounting and routing may all be based upon
 * a user's identity. The initial (and default) approach is to use the
 * pseudo-identity 'nobody' defined as part of NFS to handle problems such
 * as this. However, taking ease of use into consideration, it became
 * necessary to provide a mechanism for establishing a user's identity. 
 *
 * Initially we felt that we needed to implement two types of functionality:
 * user authentication and print spooling. (Remote login is addressed by
 * the Telnet module.) Since no network services were defined within the
 * NFS architecture to support these, it was decided to implement them in
 * a fairly portable fashion using Sun's Remote Procedure Call protocol.
 * Since these mechanisms will need to be re-implemented ion a variety of
 * software environments, we have tried to define a very general model. 
 *
 * Authentication: NFS adopts the Unix model of using a pair of integers
 * (uid, gid) to define a user's identity. This happens to map tolerably
 * well onto the VMS system. 'pcnfsd' implements a Remote Procedure which
 * is required to map a username and password into a (uid, gid) pair.
 * Since we cannot predict what mapping is to be performed, and since we
 * do not wish to pass clear-text passwords over the net, both the
 * username and the password are mildly scrambled using a simple XOR
 * operation. The intent is not to be secure (the present NFS architecture
 * is inherently insecure) but to defeat "browsers". 
 *
 * The authentication RPC will be invoked when the user enters the PC-NFS
 * command: 
 *
 * NET NAME user [password|*] 
 *
 *
 * Printing: The availability of NFS file operations simplifies the print
 * spooling mechanisms. There are two services which 'pcnfsd' has to
 * provide:
 *   pr_init:	given the name of the client system, return the
 * name of a directory which is exported via NFS and in which the client
 * may create spool files.
 *  pr_start: given a file name, a user name, the printer name, the client
 * system name and an option string, initiate printing of the file
 * on the named printer. The file name is relative to the directory
 * returned by pr_init. pr_start is to be "idempotent": a request to print
 * a file which is already being printed has no effect. 
 *
 * Intent: The first versions of these procedures are implementations for Sun
 * 2.0/3.0 software, which will also run on VAX 4.2BSD systems. The intent
 * is to build up a set of implementations for different architectures
 * (Unix System V, VMS, etc.). Users are encouraged to submit their own
 * variations for redistribution. If you need a particular variation which
 * you don't see here, either code it yourself (and, hopefully, send it to
 * us at Sun) or contact your Customer Support representative. 
 */

#include <sys/types.h>
#include <stdio.h>
#include <rpc/rpc.h>
#include <pwd.h>
#include <sysv/shadow.h>
#include <sys/file.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#ifdef RISCOS
#include <sys/syslog.h>
#else
#include <sys/stream.h>
#endif

#include <sys/tty.h>
#include <netdb.h>

int             buggit = 0;

/*
 * *************** RPC parameters ******************** 
 */
#define	PCNFSDPROG	(long)150001
#define	PCNFSDVERS	(long)1
#define	PCNFSD_AUTH	(long)1
#define	PCNFSD_PR_INIT	(long)2
#define	PCNFSD_PR_START	(long)3

/*
 * ************* Other #define's ********************** 
 */
#ifndef SPOOLDIR
# define SPOOLDIR	"/usr/spool/pcnfs"
#endif SPOOLDIR

#ifndef MAXPATHLEN
# define MAXPATHLEN 1024
#endif
#define	zchar		0x5b

#define assert(ex) {if (!(ex)) \
    {fprintf(stderr,"Assertion failed: line %d of %s: \"%s\"\n", \
    __LINE__, __FILE__, "ex"); \
    sleep (30); exit(1);}}

/*
 * *********** XDR structures, etc. ******************** 
 */
enum arstat {
	AUTH_RES_OK, AUTH_RES_FAKE, AUTH_RES_FAIL
};
enum pirstat {
	PI_RES_OK, PI_RES_NO_SUCH_PRINTER, PI_RES_FAIL
};
enum psrstat {
	PS_RES_OK, PS_RES_ALREADY, PS_RES_NULL, PS_RES_NO_FILE,
	PS_RES_FAIL
};

struct auth_args {
	char           *aa_ident;
	char           *aa_password;
};

struct auth_results {
	enum arstat     ar_stat;
	long            ar_uid;
	long            ar_gid;
};

struct pr_init_args {
	char           *pia_client;
	char           *pia_printername;
};

struct pr_init_results {
	enum pirstat    pir_stat;
	char           *pir_spooldir;
};

struct pr_start_args {
	char           *psa_client;
	char           *psa_printername;
	char           *psa_username;
	char           *psa_filename;	/* within the spooldir */
	char           *psa_options;
};

struct pr_start_results {
	enum psrstat    psr_stat;
};


/*
 * ****************** Misc. ************************ 
 */

char           *authproc();
char           *pr_start();
char           *pr_init();
struct stat     statbuf;

char            pathname[MAXPATHLEN];
char            new_pathname[MAXPATHLEN];
char            spoolname[MAXPATHLEN];

/*
 * ************** Support procedures *********************** 
 */
scramble(s1, s2)
	char           *s1;
	char           *s2;
{
	while (*s1) {
		*s2++ = (*s1 ^ zchar) & 0x7f;
		s1++;
	}
	*s2 = 0;
}

free_child()
{
	int             pid;
	int             pstatus;

	pid = wait(&pstatus);	/* clear exit of child process */

	if (buggit || pstatus)
		fprintf(stderr, "FREE_CHILD: process #%d exited with status 0X%x\n",
			pid, pstatus);
	return;
}

/*
 * *************** XDR procedures ***************** 
 */
bool_t
xdr_auth_args(xdrs, aap)
	XDR            *xdrs;
	struct auth_args *aap;
{
	return (xdr_string(xdrs, &aap->aa_ident, 32) &&
		xdr_string(xdrs, &aap->aa_password, 64));
}

bool_t
xdr_auth_results(xdrs, arp)
	XDR            *xdrs;
	struct auth_results *arp;
{
	return (xdr_enum(xdrs, &arp->ar_stat) &&
		xdr_long(xdrs, &arp->ar_uid) &&
		xdr_long(xdrs, &arp->ar_gid));
}

bool_t
xdr_pr_init_args(xdrs, aap)
	XDR            *xdrs;
	struct pr_init_args *aap;
{
	return (xdr_string(xdrs, &aap->pia_client, 64) &&
		xdr_string(xdrs, &aap->pia_printername, 64));
}

bool_t
xdr_pr_init_results(xdrs, arp)
	XDR            *xdrs;
	struct pr_init_results *arp;
{
	return (xdr_enum(xdrs, &arp->pir_stat) &&
		xdr_string(xdrs, &arp->pir_spooldir, 255));
}

bool_t
xdr_pr_start_args(xdrs, aap)
	XDR            *xdrs;
	struct pr_start_args *aap;
{
	return (xdr_string(xdrs, &aap->psa_client, 64) &&
		xdr_string(xdrs, &aap->psa_printername, 64) &&
		xdr_string(xdrs, &aap->psa_username, 64) &&
		xdr_string(xdrs, &aap->psa_filename, 64) &&
		xdr_string(xdrs, &aap->psa_options, 64));
}

bool_t
xdr_pr_start_results(xdrs, arp)
	XDR            *xdrs;
	struct pr_start_results *arp;
{
	return (xdr_enum(xdrs, &arp->psr_stat));
}



/*
 * ********************** main ********************* 
 */

main (Argc, Argv) int Argc; char * Argv[]; {
    int             f1, f2, f3;
    int             FromInetd;
    SVCXPRT        *TransportHandle;
    extern          xdr_string_array();
    void            Dispatch ();

    struct stat sb;

#ifdef RISCOS
    openlog("pcnfsd", LOG_PID, LOG_DAEMON);
#endif
    strcpy (spoolname, SPOOLDIR);
    setbuf (stderr, NULL);

    /*
     *  If we're called from inetd:
     *     - an open RPC socket is passed as fd 0.
     *     and note that we are already registered with the portmapper.
     *  Otherwise:
     *     - we must parse any command-line arguments which may be present.
     *     - we must create an RPC socket (svcudp_create will do this).
     *     - we are not yet registered with the portmapper, and
     *       must do so.
     */
    FromInetd = issock(0);

#ifdef notdef
	if (FromInetd) {
#ifdef RISCOS
		syslog(LOG_ERR,"running from inetd!\n\n\n\n\n\n");
#else
		fprintf(stderr,"running from inetd!\n\n\n\n\n\n");
#endif
		fflush(stderr);
		exit(1);
	} else {
#ifdef RISCOS
		syslog(LOG_ERR,"NOT running from inetd!\n");
#else
		fprintf(stderr,"NOT running from inetd!\n");
#endif
	}
#endif
	
    if (! FromInetd) {
	while (++Argv, --Argc > 0) {
	    if (strcmp (*Argv, "-d") == 0) {
		++ buggit;
		continue;
	    }
	    if (strcmp (*Argv, "-s") == 0) {
		if (! (++Argv, --Argc > 0)) {
#ifdef RISCOS
		    syslog(LOG_ERR,
"-s option must be followed by a spooling directory path\n");
#else
		    fprintf (stderr,
"pc-nfsd error: -s option must be followed by a spooling directory path\n");
#endif
		    exit (1);
		}
		strcpy (spoolname, *Argv);
		continue;
	    }
	    if (strncmp (*Argv, "-s", 2) == 0) {
		strcpy (spoolname, & (*Argv)[2]);
		continue;
	    }
	}
    }

    if (! FromInetd && ! buggit) {
	switch (fork ()) {
	case  0:
	    break;
	case -1:
#ifdef RISCOS
	    syslog(LOG_ERR,"fork failed");
#else
	    perror ("pc-nfsd: fork failed");
#endif
	    exit (1);
	default:
	    exit (0);
	}
    }

    if (! buggit) {
	/*
	 *  Can't mess with STDIN if invoked from inetd, 'cause our
	 *  incoming RPC request datagram is passed in on STDIN.
	 */
	if (! FromInetd) {
	    if ((f1 = open("/dev/null", O_RDONLY)) == -1) {
#ifdef RISCOS
		syslog(LOG_ERR, "couldn't open /dev/null\n");
#else
		fprintf(stderr, "pc-nfsd: couldn't open /dev/null\n");
#endif
		exit(1);
	    }
	    (void) dup2 (f1, 0);
	    (void) close (f1);
	}

	if ((f2 = open("/dev/console", O_WRONLY)) == -1) {
#ifdef RISCOS
	    syslog(LOG_ERR, "couldn't open /dev/console\n");
#else
	    fprintf(stderr, "pc-nfsd: couldn't open /dev/console\n");
#endif
	    exit(1);
	}
	(void) dup2 (f2, 1);
	(void) close (f2);

	if ((f3 = open("/dev/console", O_WRONLY)) == -1) {
#ifdef RISCOS
	    syslog(LOG_ERR, "couldn't open /dev/console\n");
#else
	    fprintf(stderr, "pc-nfsd: couldn't open /dev/console\n");
#endif
	    exit(1);
	}
	(void) dup2 (f3, 2);
	(void) close (f3);

	/*
	 *  Disconnect ourself from the control tty:
	 */
	if ((f1 = open ("/dev/tty", O_RDONLY)) >= 0) {
	    (void) ioctl (f1, TIOCNOTTY, (char *) 0);
	    (void) close (f1);
	}
    }

#if CHATTY
    buggit = 1;  /* chatty from here on ... */
#endif CHATTY

    /*
     *  Set up our RPC environment:
     */
    if (FromInetd) {
#if CHATTY
#ifdef RISCOS
	syslog(LOG_ERR, "started from inetd\n");
#else
	fprintf (stderr, "pc-nfsd started from inetd\n");
#endif
#endif CHATTY
	assert ((TransportHandle = svcudp_create (0)) != NULL);
	assert (svc_register (TransportHandle, PCNFSDPROG, PCNFSDVERS, Dispatch, 0) != NULL);
    } else {
	assert ((TransportHandle = svcudp_create (RPC_ANYSOCK)) != NULL);
	pmap_unset (PCNFSDPROG, PCNFSDVERS);
	assert (svc_register (TransportHandle, PCNFSDPROG, PCNFSDVERS, Dispatch, IPPROTO_UDP) != NULL);
    }

    mkdir(spoolname); /* just in case, ignoring the result */
    if (stat(spoolname, &statbuf) || !(statbuf.st_mode & S_IFDIR)) {
#ifdef RISCOS
	syslog(LOG_ERR, "invalid spool directory %s\n", spoolname);
#else
	fprintf(stderr, "pc-nfsd: invalid spool directory %s\n", spoolname);
#endif
	exit(1);
    }

    svc_run ();
#ifdef RISCOS
    syslog(LOG_ERR, "error: svc_run returned\n");
#else
    fprintf (stderr, "pc-nfsd: error: svc_run returned\n");
#endif
	sleep (30);   /* just in case inetd wants to fork us again */
	exit (1);
}

/*
 * ******************* RPC procedures ************** 
 */
void Dispatch (ServiceRequest, Transport)
    struct svc_req *ServiceRequest; SVCXPRT *Transport; {
    char * outdata;
    char xdrbuf [UDPMSGSIZE];
    bzero (xdrbuf, sizeof (xdrbuf));	/* said to be required... */

    switch (ServiceRequest -> rq_proc) {
    case NULLPROC:
#if CHATTY
#ifdef RISCOS
	syslog(LOG_ERR, "NULLPROC called\n");
#else
	fprintf (stderr, "NULLPROC called\n");
#endif
#endif CHATTY
	assert (svc_sendreply (Transport, xdr_void, NULL));
	break;
    case PCNFSD_AUTH:
#if CHATTY
#ifdef RISCOS
	syslog(LOG_ERR, "PCNFSD_AUTH called\n");
#else
	fprintf (stderr, "PCNFSD_AUTH called\n");
#endif
#endif CHATTY
	assert (svc_getargs (Transport, xdr_auth_args, xdrbuf));
	outdata = authproc (xdrbuf);
#if CHATTY
	{
#ifdef RISCOS
	syslog(LOG_ERR, "AUTH returns %d %d %d\n",
	    (int) (((struct auth_results*) outdata) -> ar_stat),
	    (int) (((struct auth_results*) outdata) -> ar_uid),
	    (int) (((struct auth_results*) outdata) -> ar_gid));
#else
	fprintf (stderr, "AUTH returns %d %d %d\n",
	    (int) (((struct auth_results*) outdata) -> ar_stat),
	    (int) (((struct auth_results*) outdata) -> ar_uid),
	    (int) (((struct auth_results*) outdata) -> ar_gid));
#endif
	}
#endif CHATTY
	assert (svc_sendreply (Transport, xdr_auth_results, outdata));
	break;
    case PCNFSD_PR_INIT:
#if CHATTY
#ifdef RISCOS
	syslog(LOG_ERR, "PCNFSD_PR_INIT called\n");
#else
	fprintf (stderr, "PCNFSD_PR_INIT called\n");
#endif
#endif CHATTY
	assert (svc_getargs (Transport, xdr_pr_init_args, xdrbuf));
	outdata = pr_init (xdrbuf);
	assert (svc_sendreply (Transport, xdr_pr_init_results, outdata));
	break;
    case PCNFSD_PR_START:
#if CHATTY
#ifdef RISCOS
	syslog(LOG_ERR, "PCNFSD_PR_START called\n");
#else
	fprintf (stderr, "PCNFSD_PR_START called\n");
#endif
#endif CHATTY
	assert (svc_getargs (Transport, xdr_pr_start_args, xdrbuf));
	outdata = pr_start (xdrbuf);
	assert (svc_sendreply (Transport, xdr_pr_start_results, outdata));
	break;
    default:
#ifdef RISCOS
	syslog(LOG_ERR,
	    "unknown function %d called in Dispatch()\n",
	    ServiceRequest -> rq_proc);
#else
	fprintf (stderr,
	    "pc-nfsd error: unknown function %d called in Dispatch()\n",
	    ServiceRequest -> rq_proc);
#endif
	break;
    }
    return;
}    


char           *
authproc(a)
	struct auth_args *a;
{
	static struct auth_results r;
	char            username[32];
	char            password[64];
	int             c1, c2;
	struct passwd  *p;
	struct spwd *sp, *getspnam();

	r.ar_stat = AUTH_RES_FAIL;	/* assume failure */
	p = getpwnam ("nobody");
	r.ar_uid = p ? p -> pw_uid : -2;
	r.ar_gid = p ? p -> pw_gid : -2;

	scramble(a->aa_ident, username);
	scramble(a->aa_password, password);

	if (buggit)
#ifdef RISCOS
		syslog(LOG_ERR, "AUTHPROC username=%s\n", username);
#else
		fprintf(stderr, "AUTHPROC username=%s\n", username);
#endif

	p = getpwnam(username);
	if (p == NULL)
		return ((char *) &r);
	if (access(SHADOW,0)==0) {	/* shadow password file */
		sp = getspnam(username);
		if (sp == NULL)
			return ((char*) &r);
		p->pw_passwd = sp->sp_pwdp;
	}
	c1 = strlen(password);
	c2 = strlen(p->pw_passwd);
	if ((c1 && !c2) || (c2 && !c1) ||
		(strcmp(p->pw_passwd, crypt(password, p->pw_passwd)))) {
		return ((char *) &r);
	}
	r.ar_stat = AUTH_RES_OK;
	r.ar_uid = p->pw_uid;
	r.ar_gid = p->pw_gid;
	return ((char *) &r);
}


char           *
pr_init(pi_arg)
	struct pr_init_args *pi_arg;
{
	int             dir_mode = 0777;
	static struct pr_init_results pi_res;

	mkdir(spoolname); /* just in case, ignoring the result */
	chmod(spoolname, dir_mode);

	/* get pathname of current directory and return to client */
	strcpy(pathname, spoolname);	/* first the spool area */
	strcat(pathname, "/");	/* append a slash */
	strcat(pathname, pi_arg->pia_client);
	/* now the host name */
	mkdir(pathname);	/* ignore the return code */
	if (stat(pathname, &statbuf) || !(statbuf.st_mode & S_IFDIR)) {
#ifdef RISCOS
		syslog(LOG_ERR,
			"unable to create spool directory %s\n",
			pathname);
#else
		fprintf(stderr,
			"pc-nfsd: unable to create spool directory %s\n",
			pathname);
#endif
		pathname[0] = 0;/* null to tell client bad vibes */
		pi_res.pir_stat = PI_RES_FAIL;
	} else {
		pi_res.pir_stat = PI_RES_OK;
	}
	pi_res.pir_spooldir = &pathname[0];
	chmod(pathname, dir_mode);

	if (buggit)
#ifdef RISCOS
		syslog(LOG_ERR, "PR_INIT pathname=%s\n", pathname);
#else
		fprintf(stderr, "PR_INIT pathname=%s\n", pathname);
#endif

	return ((char *) &pi_res);
}

char           *
pr_start(ps_arg)
	struct pr_start_args *ps_arg;
{
	static struct pr_start_results ps_res;
	int             pid;
	int             free_child();
	char            printer_opt[64];
	char            jobname_opt[64];
	char            clientname_opt[64];
	struct passwd  *p;
	long		rnum;
	char		snum[20];


	signal(SIGCHLD, free_child);	/* when child terminates it sends */
	/* a signal which we must get */
	strcpy(pathname, spoolname);	/* build filename */
	strcat(pathname, "/");
	strcat(pathname, ps_arg->psa_client);	/* /spool/host */
	strcat(pathname, "/");	/* /spool/host/ */
	strcat(pathname, ps_arg->psa_filename);	/* /spool/host/file */

	if (buggit) {
#ifdef RISCOS
		syslog(LOG_ERR, "PR_START pathname=%s\n", pathname);
		syslog(LOG_ERR, "PR_START username= %s\n", ps_arg->psa_username);
		syslog(LOG_ERR, "PR_START client= %s\n", ps_arg->psa_client);
#else
		fprintf(stderr, "PR_START pathname=%s\n", pathname);
		fprintf(stderr, "PR_START username= %s\n", ps_arg->psa_username);
		fprintf(stderr, "PR_START client= %s\n", ps_arg->psa_client);
#endif
	}

	if (stat(pathname, &statbuf)) {
		/*
		 * We can't stat the file. Let's try appending '.spl' and
		 * see if it's already in progress. 
		 */

		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...can't stat it.\n");
#else
			fprintf(stderr, "...can't stat it.\n");
#endif

		strcat(pathname, ".spl");
		if (stat(pathname, &statbuf)) {
			/*
			 * It really doesn't exist. 
			 */

			if (buggit)
#ifdef RISCOS
				syslog(LOG_ERR, "...PR_START returns PS_RES_NO_FILE\n");
#else
				fprintf(stderr, "...PR_START returns PS_RES_NO_FILE\n");
#endif

			ps_res.psr_stat = PS_RES_NO_FILE;
			return ((char *) &ps_res);
		}
		/*
		 * It is already on the way. 
		 */

		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...PR_START returns PS_RES_ALREADY\n");
#else
			fprintf(stderr, "...PR_START returns PS_RES_ALREADY\n");
#endif

		ps_res.psr_stat = PS_RES_ALREADY;
		return ((char *) &ps_res);
	}
	if (statbuf.st_size == 0) {
		/*
		 * Null file - don't print it, just kill it. 
		 */
		unlink(pathname);

		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...PR_START returns PS_RES_NULL\n");
#else
			fprintf(stderr, "...PR_START returns PS_RES_NULL\n");
#endif

		ps_res.psr_stat = PS_RES_NULL;
		return ((char *) &ps_res);
	}
	/*
	 * The file is real, has some data, and is not already going out.
	 * We rename it by appending '.spl' and exec "lpr" to do the
	 * actual work. 
	 */
	strcpy(new_pathname, pathname);
	strcat(new_pathname, ".spl");

	if (buggit)
#ifdef RISCOS
		syslog(LOG_ERR, "...renaming %s -> %s\n", pathname, new_pathname);
#else
		fprintf(stderr, "...renaming %s -> %s\n", pathname, new_pathname);
#endif

	/*
	 * See if the new filename exists so as not to overwrite it.
	 */


	if (!stat(new_pathname, &statbuf)){
		strcpy(new_pathname, pathname);  /* rebuild a new name */
		sprintf(snum,"%ld",random());		/* get some number */
		strncat(new_pathname, snum, 3);
		strcat(new_pathname, ".spl");		/* new spool file */
		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...created new spl file -> %s\n", new_pathname);
#else
			fprintf(stderr, "...created new spl file -> %s\n", new_pathname);
#endif

	}
		

	if (rename(pathname, new_pathname)) {
		/*
		 * CAVEAT: Microsoft changed rename for Microsoft C V3.0.
		 * Check this if porting to Xenix. 
		 */
		/*
		 * Should never happen. 
		 */
#ifdef RISCOS
		syslog(LOG_ERR, "spool file rename (%s->%s) failed.\n", pathname, new_pathname);
#else
		fprintf(stderr, "pc-nfsd: spool file rename (%s->%s) failed.\n", pathname, new_pathname);
#endif
		ps_res.psr_stat = PS_RES_FAIL;
		return ((char *) &ps_res);
	}
	pid = fork();
	if (pid == 0) {
		/*
		 *  FLUKE jps 28-jul-86 - Invoke lpr as the requesting user.
		 *
		 *  If possible, invoke lpr under the user-id/group-id of the
		 *  person (apparently) making this RPC request.  Good for
		 *  accounting, proper banner page, etc.  It is not mandatory.
		 */
		struct passwd *pw = getpwnam (ps_arg -> psa_username);
		if(buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "username is %s\n", ps_arg->psa_username);
#else
			fprintf(stderr, "username is %s\n", ps_arg->psa_username);
#endif
		if (pw) {
			if (buggit)
#ifdef RISCOS
				syslog(LOG_ERR, "uid is %d\ngid is %d\n",
					pw->pw_uid, pw->pw_gid);
#else
				fprintf(stderr, "uid is %d\ngid is %d\n",
					pw->pw_uid, pw->pw_gid);
#endif
			setreuid (pw -> pw_uid, pw -> pw_uid);
			setregid (pw -> pw_gid, pw -> pw_gid);

			/*
			 *  PC-NFS doesn't pass us any filename to show on
			 *  the banner page, so we blank this field out.
			 *  That's batter than showing the pseudo-random
			 *  temporary file name used internally (or the
			 *  UNIX-ism "(stdin)").
			 */
			sprintf (printer_opt, "-P%s", ps_arg->psa_printername);
			sprintf (jobname_opt, "-J ");
			sprintf (clientname_opt, "-C%s", ps_arg->psa_client);
		} else {
			/*
			 *  We don't know the user's identity, so the printout
			 *  will end up being enqueued by root.  We do want
			 *  the user's name to appear on the banner page,
			 *  so we slip it in via the -J option.
			 */
			sprintf (printer_opt, "-P%s", ps_arg->psa_printername);
			sprintf (jobname_opt, "-J%s", ps_arg -> psa_username);
			sprintf (clientname_opt, "-C%s", ps_arg->psa_client);
		}
		if (ps_arg->psa_options[1] == 'd') {
			/*
			 * This is a Diablo print stream. Apply the ps630
			 * filter with the appropriate arguments. 
			 */
			if (buggit)
#ifdef RISCOS
				syslog(LOG_ERR, "...run_ps630 invoked\n");
			run_ps630(new_pathname, ps_arg->psa_options);
#else
				fprintf(stderr, "...run_ps630 invoked\n");
			run_ps630(new_pathname, ps_arg->psa_options);
#endif
		}
		execlp("/usr/ucb/lpr",
			"lpr",
			"-s",
			"-r",
			printer_opt,
			jobname_opt,
			clientname_opt,
			new_pathname,
			0);
#ifdef RISCOS
		syslog(LOG_ERR, "exec lpr failed");
#else
		perror("pc-nfsd: exec lpr failed");
#endif
		exit (1);	/* end of child process */
	} else if (pid == -1) {
#ifdef RISCOS
		syslog(LOG_ERR, "fork failed");
#else
		perror("pc-nfsd: fork failed");
#endif

		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...PR_START returns PS_RES_FAIL\n");
#else
			fprintf(stderr, "...PR_START returns PS_RES_FAIL\n");
#endif

		ps_res.psr_stat = PS_RES_FAIL;
		return ((char *) &ps_res);
	} else {

		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...forked child #%d\n", pid);
#else
			fprintf(stderr, "...forked child #%d\n", pid);
#endif


		if (buggit)
#ifdef RISCOS
			syslog(LOG_ERR, "...PR_START returns PS_RES_OK\n");
#else
			fprintf(stderr, "...PR_START returns PS_RES_OK\n");
#endif

		ps_res.psr_stat = PS_RES_OK;
		return ((char *) &ps_res);
	}
}

char           *
mapfont(f, i, b)
	char            f;
	char            i;
	char            b;
{
	static char     fontname[64];

	fontname[0] = 0;	/* clear it out */

	switch (f) {
	case 'c':
		strcpy(fontname, "Courier");
		break;
	case 'h':
		strcpy(fontname, "Helvetica");
		break;
	case 't':
		strcpy(fontname, "Times");
		break;
	default:
		strcpy(fontname, "Times-Roman");
		goto exit;
	}
	if (i != 'o' && b != 'b') {	/* no bold or oblique */
		if (f == 't')	/* special case Times */
			strcat(fontname, "-Roman");
		goto exit;
	}
	strcat(fontname, "-");
	if (b == 'b')
		strcat(fontname, "Bold");
	if (i == 'o')		/* o-blique */
		strcat(fontname, f == 't' ? "Italic" : "Oblique");

exit:	return (&fontname[0]);
}

/*
 * run_ps630 performs the Diablo 630 emulation filtering process. ps630 is
 * currently broken in the Sun release: it will not accept point size or
 * font changes. If your version is fixed, define the symbol
 * PS630_IS_FIXED and rebuild pc-nfsd. 
 */


run_ps630(file, options)
	char           *file;
	char           *options;
{
	char            tmpfile[256];
	char            commbuf[256];
	int             i;

	strcpy(tmpfile, file);
	strcat(tmpfile, "X");	/* intermediate file name */

#ifdef PS630_IS_FIXED
	sprintf(commbuf, "ps630 -s %c%c -p %s -f ",
		options[2], options[3], tmpfile);
	strcat(commbuf, mapfont(options[4], options[5], options[6]));
	strcat(commbuf, " -F ");
	strcat(commbuf, mapfont(options[7], options[8], options[9]));
	strcat(commbuf, "  ");
	strcat(commbuf, file);
#else PS630_IS_FIXED
	/*
	 * The pitch and font features of ps630 appear to be broken at
	 * this time. If you think it's been fixed at your site, define
	 * the compile-time symbol `ps630_is_fixed'. 
	 */
	sprintf(commbuf, "ps630 -p %s %s", tmpfile, file);
#endif PS630_IS_FIXED


	if (i = system(commbuf)) {
		/*
		 * Under (un)certain conditions, ps630 may return -1
		 * even if it worked. Hence the commenting out of this
		 * error report. 
		 */
		 /* fprintf(stderr, "\n\nrun_ps630 rc = %d\n", i) */ ;
		/* exit(1); */
	}
	if (rename(tmpfile, file)) {
		perror("run_ps630: rename");
		exit(1);
	}
}

/*
 * Determine if a descriptor belongs to a socket or not
 */
issock(fd)
        int fd;
{
        struct stat st;

        if (fstat(fd, &st) < 0) {
                return (0);
        }
        /*
         * SunOS returns S_IFIFO for sockets, while 4.3 returns 0 and does not
         * even have an S_IFIFO mode.  Since there is confusion about what the
         * mode is, we check for what it is not instead of what it is.
         */
        switch (st.st_mode & S_IFMT) {
        case S_IFCHR:
        case S_IFREG:
        case S_IFLNK:
        case S_IFDIR:
        case S_IFBLK:
                return (0);
        default:
                return (1);
        }
}
