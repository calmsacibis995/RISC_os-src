#ident "$Header: bfsd.c,v 1.6 90/08/16 11:42:03 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * bfsd -- boot file server daemon
 * This version groks inquiry forward packet and how to deal with them.
 */

#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include <net/if.h>
#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/time.h>
#include "saio/bfs.h"

#define	SIGMASK(x)	(1<<((x)-1))

struct	itimerval age_time = {		/* age open file descriptors */
#ifdef VAXDEBUG
	{ 20, 0 },			/* tick every 20 seconds for debug */
	{ 20, 0 }
#else
	{ 2, 0 },			/* tick every 2 seconds */
	{ 2, 0 }
#endif
};

struct	itimerval off_time = {		/* turn-off interval timer */
	{ 0, 0 },
	{ 0, 0 }
};

int timer_on;			/* non-zero if timer is running */

#define	NDESC	10		/* size of descriptor table */
#define	MAXAGE	5		/* max timer ticks on descriptor before close */

struct desc_table {
	char dt_filename[128];
	int dt_desc;
	int dt_age;
} desc_table[NDESC];

/*
 * bfs_packet -- boot file server packet format
 */
struct bfs_packet {
	struct bfshdr bfs;
	char data[BFS_MAXDATA];
};

struct	interface {
	char	*if_name;
	unsigned long	if_broadaddr;
	unsigned long	if_netmask;
	unsigned long 	if_inaddr;
	} ifs[4];
int	If_count = 0;

struct	sockaddr_in sin = { AF_INET };

extern	errno;

char	myname[32];

struct	servent *sp;
struct	hostent *hp;
char	dns_name[128];
char	aliases[128][128];
int	s;

char *progname;
char *root_dir;
int user_id;
int group_id;

extern char	*strcpy();
extern char	*index();
extern char	*pe_concat();
extern long	lseek();
extern struct	in_addr inet_makeaddr();

int	age_signal();
struct	desc_table *can_handle();
char	*expand();

main(argc, argv)
int argc;
char **argv;
{
	struct sockaddr_in from;
#ifndef SVR3
	struct sigvec age_vec;
#endif SVR3
	struct passwd *pw;
	struct group *gr;
	char if_name[32];
	struct ifreq ifreq;
	char *malloc();
	char **lp;
	int i;

	progname = argv[0];

	while (--argc > 0) {
		if (**++argv == '-') {
			switch ((*argv)[1]) {

			case 'd':
				if (--argc <= 0) {
					fprintf(stderr,
					    "%s: -d takes directory\n",
					    progname);
					exit(1);
				}
				root_dir = *++argv;
				break;
#ifndef SVR3
			case 'u':
				if (--argc <= 0) {
					fprintf(stderr,
					    "%s: -u takes username\n",
					    progname);
					exit(1);
				}
				pw = getpwnam(*++argv);
				if (pw == NULL) {
					fprintf(stderr,"%s: %s user unknown\n",
					    progname, *argv);
					exit(1);
				}
				user_id = pw->pw_uid;
				break;

			case 'g':
				if (--argc <= 0) {
					fprintf(stderr,
					    "%s: -g takes groupname\n",
					    progname);
					exit(1);
				}
				gr = getgrnam(*++argv);
				if (gr == NULL) {
					fprintf(stderr,"%s: %s group unknown\n",
					    progname, *argv);
					exit(1);
				}
				group_id = gr->gr_gid;
				break;
#endif

			case 'i':
				if (--argc <= 0) {
					fprintf(stderr,
					    "%s: -i takes interface name\n",
					    progname);
					exit(1);
				}
				strcpy(if_name, *++argv);
				ifs[If_count].if_name =
					malloc(strlen(if_name) + 1);
				strcpy(ifs[If_count].if_name, if_name);
				ifs[If_count].if_name[strlen(if_name)] = '\0';
				s = socket(AF_INET, SOCK_DGRAM, 0);
				strcpy(ifreq.ifr_name, if_name);
				ioctl(s, SIOCGIFNETMASK, &ifreq);
				ifs[If_count].if_netmask =
				((struct sockaddr_in *)&ifreq.ifr_addr)
					->sin_addr.s_addr;
				ioctl(s, SIOCGIFBRDADDR, &ifreq);
				ifs[If_count].if_broadaddr =
				((struct sockaddr_in *)&ifreq.ifr_addr)
					->sin_addr.s_addr;
				ioctl(s, SIOCGIFADDR, &ifreq);
				close(s);
				ifs[If_count++].if_inaddr =
				((struct sockaddr_in *)&ifreq.ifr_addr)
					->sin_addr.s_addr;

#ifdef DEBUG
				fprintf(stderr, "got interface #%d %s with netmask %x and broadaddr %s inaddr %s\n",
					If_count - 1,
					ifs[If_count - 1].if_name,
					ifs[If_count - 1].if_netmask,
					inet_ntoa(ifs[If_count-1].if_broadaddr),
					inet_ntoa(ifs[If_count-1].if_inaddr));
#endif DEBUG
					
				break;

			default:
				fprintf(stderr, "%s: %s unknown option\n",
				    progname, *argv);
				exit(1);
			}
		} else {
			fprintf(stderr, "%s: %s illegal argument\n",
			    progname, *argv);
			exit(1);
		}
	}
	sp = getservbyname("bfs", "udp");
	if (sp == 0) {
		fprintf(stderr, "%s: udp/bfs: unknown service\n", progname);
		exit(1);
	}
#ifndef DEBUG
	if (fork())
		exit(0);
	{
		int fd;

		for (fd = 0; fd < 10; fd++)
			(void) close(fd);
		(void) open("/", 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		fd = open("/dev/tty", 2);
		if (fd >= 0) {
			ioctl(fd, TIOCNOTTY, 0);
			(void) close(fd);
		}
	}
#endif

#ifndef SVR3
	if (group_id)
		setregid(group_id, group_id);
	if (user_id)
		setreuid(user_id, user_id);
#endif

	if (root_dir)
		if (chdir(root_dir) < 0) {
			perror(pe_concat(progname, root_dir));
			exit(1);
		}

#ifndef SVR3
	age_vec.sv_handler = age_signal;
	age_vec.sv_mask = SIGMASK(SIGALRM);
	age_vec.sv_onstack = 0;
	sigvec(SIGALRM, &age_vec, 0);
#else
	signal(SIGALRM, age_signal);
#endif

	/*
	 * Establish host name as returned by system.
	 */
	if (gethostname(myname, sizeof (myname) - 1) < 0) {
		perror(pe_concat(progname, "gethostname"));
		exit(1);
	}
	hp = gethostbyname(myname);
	if (hp == NULL) {
		fprintf(stderr, "%s: %s: don't know my own name net address\n",
		    progname, myname);
		exit(1);
	}
	strcpy(dns_name, hp->h_name);
	for (lp = hp->h_aliases, i=0; *lp; lp++, i++)
		strcpy(aliases[i], lp);

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror(pe_concat(progname, "socket"));
		exit(1);
	}

	bzero(&sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = sp->s_port;

#ifdef DEBUG
	fprintf(stderr, "%s: binding to %d %s %d\n", progname, sin.sin_family,
	    inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
#endif

	if (bind(s, &sin, sizeof (sin)) < 0) {
		perror(pe_concat(progname, "bind"));
		exit(1);
	}

	for (;;) {
		int cc, len = sizeof (from);
		struct bfs_packet req;
		struct desc_table *dt;
		register i;

		cc = recvfrom(s, (char *)&req, sizeof(req), 0, &from, &len);

		for (i = 0 ; i < If_count ; i++)
			if (ifs[i].if_inaddr == from.sin_addr.s_addr)
				goto cont;

		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror(pe_concat(progname, "recvfrom"));
cont:
			continue;
		}

#ifdef DEBUG
		fprintf(stderr, "%s: received from %d %s %d, cc=%d\n\r",
		    progname, from.sin_family, inet_ntoa(from.sin_addr),
		    ntohs(from.sin_port), cc);
#endif

		if (cc < sizeof(struct bfshdr) + ntohs(req.bfs.bh_pathlen)
		   + (req.bfs.bh_type == BFSPT_READ
		       ? 0 : ntohs(req.bfs.bh_datalen))) {
#ifdef DEBUG
			fprintf(stderr, "%s: received short packet\n\r",
			    progname);
#endif
			continue;
		}

		if (dt = can_handle(&req))
			reply_packet(dt, &req, &from);
		else
			{
#ifdef DEBUG
			fprintf(stderr, "%s: can't reply to %s\n\r", progname,
			    req.data);
#endif
			gateway_reqpacket(&req, &from, s);
			}
	}
}

struct desc_table *
can_handle(rqp)
struct bfs_packet *rqp;
{
	char *filename;
	char **lp;
	char *np;
	struct desc_table *dt, *edt;
	int oldsigs;
	struct hostent *server;
	char *name;

#ifdef DEBUG
	fprintf(stderr, "%s: looking for %s\n\r", progname, rqp->data);
#endif
	if (filename = index(rqp->data, ':')) {
		char host[64];
		unsigned long host_addr;

		/*
		 * temporarily set : to null so we can easily copy
		 * host name out
		 */
		*filename = 0;
		strcpy(host, rqp->data);
		*filename++ = ':';
		/* this is necessary to allow for domain names */
		server = gethostbyname(host);
		/* what if we don't recognize this name? */
		if (server == NULL) {
			/* is it a dot format address? */
			if (isdigit(host[0])) {
				/* this fails if -1 is a valid address */
				host_addr = inet_addr(host);
				if (host_addr == -1)
					return(NULL);
				server = gethostbyaddr(&host_addr,
					sizeof(long), AF_INET);
				if (server == NULL)
					return(NULL);
			} else
				return(NULL);
		}
		/* now check to see if the req name matches ours */
		if (strcmp(dns_name, server->h_name)) {
			for (lp = aliases; *lp; lp++)
				if (strcmp(*lp, server->h_name) == 0) {
					break;
				}
			if (*lp == 0) {
#ifdef DEBUG
				fprintf(stderr, "%s: not for me\n", progname);
#endif
				return(NULL);	/* not for us */
			}
		} 
	} else
		filename = rqp->data;

	edt = NULL;
#ifndef SVR3
	oldsigs = sigblock(SIGMASK(SIGALRM));
#else
	sighold(SIGALRM);
#endif
	for (dt = desc_table; dt < &desc_table[NDESC]; dt++) {
		if (edt == NULL && *dt->dt_filename == 0)
			edt = dt;
		if (strcmp(dt->dt_filename, filename) == 0) {
			dt->dt_age = -1;	/* lock entry */
#ifndef SVR3
			sigsetmask(oldsigs);
#else
			sigrelse(SIGALRM);
#endif
			return(dt);
		}
	}

	if (edt == NULL) {
		edt = desc_table;
		for (dt = &desc_table[1]; dt < &desc_table[NDESC]; dt++)
			if (dt->dt_age > edt->dt_age)
				edt = dt;
		*edt->dt_filename = 0;
		close(edt->dt_desc);
	}

	edt->dt_desc = open(expand(filename), ntohl(rqp->bfs.bh_flags), 0666);
	if (edt->dt_desc < 0) {
#ifndef SVR3
		sigsetmask(oldsigs);
#else
		sigrelse(SIGALRM);
#endif
#ifdef DEBUG
		fprintf(stderr, "%s: can't open %s\n",
		    progname, expand(filename));
#endif
		return(NULL);	/* can't open the file on this system */
	}

#ifdef DEBUG
	fprintf(stderr, "%s: opening %s\n\r", progname, filename);
#endif

	strcpy(edt->dt_filename, filename);
	edt->dt_age = -1;	/* lock entry */
	if (!timer_on) {	/* start timer if not already running */
#ifndef SVR3
		setitimer(ITIMER_REAL, &age_time, 0);
#else
		alarm(age_time);
#endif
#ifdef DEBUG
		fprintf(stderr, "%s: starting timer\n\r", progname);
#endif
	}
#ifndef SVR3
	sigsetmask(oldsigs);
#else
	sigrelse(SIGALRM);
#endif
	return(edt);
}
		
reply_packet(dt, rqp, top)
struct desc_table *dt;
struct bfs_packet *rqp;
struct sockaddr_in *top;
{
	struct bfs_packet rpy;
	char *bufp;
	int maxdata, packetlen, oldsigs, pathlen, datalen, offset;

	/* denuxi request packet */
	pathlen = ntohs(rqp->bfs.bh_pathlen);
	datalen = ntohs(rqp->bfs.bh_datalen);
	offset = ntohl(rqp->bfs.bh_offset);

	/* initialize reply packet */
	rpy.bfs = rqp->bfs;
	bcopy(rqp->data, rpy.data, pathlen);
	bufp = &rpy.data[pathlen];
	maxdata = sizeof(rpy.data) - pathlen;
	maxdata = maxdata < datalen ? maxdata : datalen;
	strncpy(rpy.bfs.bh_server, myname, sizeof(rpy.bfs.bh_server) - 1);
	rpy.bfs.bh_rev = BFSREV;

	switch (rqp->bfs.bh_type) {

	case BFSPT_ENQUIRE:
		rpy.bfs.bh_type = BFSPT_ENQRPY;
		rpy.bfs.bh_datalen = htons(0);
		rpy.bfs.bh_offset = htonl(0);
		break;

	case BFSPT_ENQFWD:
#ifdef DEBUG
		fprintf(stderr, "answering ENQFWD request\n");
#endif
		rpy.bfs.bh_type = BFSPT_ENQRPY;
		rpy.bfs.bh_datalen = htons(0);
		rpy.bfs.bh_offset = htonl(0);
		top->sin_port = htons(BFSPORT_PROM);
		bcopy(rqp->data + htons(rqp->bfs.bh_pathlen),
			&(top->sin_addr.s_addr),
			sizeof(top->sin_addr.s_addr));
		break;

	case BFSPT_READ:
		rpy.bfs.bh_type = BFSPT_RDRPY;
		if (lseek(dt->dt_desc, offset, 0) < 0)
			goto error;
		rpy.bfs.bh_datalen = htons(read(dt->dt_desc, bufp, maxdata));
		break;

	case BFSPT_WRITE:
		rpy.bfs.bh_type = BFSPT_WTRPY;
		if (lseek(dt->dt_desc, offset, 0) < 0)
			goto error;
		/*
		 * should probably send back a short reply
		 */
		maxdata = write(dt->dt_desc, bufp, rqp->bfs.bh_datalen);
		if (maxdata != datalen)
			goto error;
		break;

	case BFSPT_ERROR:
	case BFSPT_ENQRPY:
	case BFSPT_RDRPY:
	case BFSPT_WTRPY:
error:
		rpy.bfs.bh_type = BFSPT_ERROR;
		strcpy(bufp, "bfsd received bad packet from you\n");
		rpy.bfs.bh_datalen = htons(strlen(bufp));
		break;
	}

	packetlen = sizeof(struct bfshdr) + ntohs(rpy.bfs.bh_pathlen)
	    + ntohs(rpy.bfs.bh_datalen);

#ifdef DEBUG
	fprintf(stderr, "%s: replying to %d %s %d type %d offset %d len %d\n\r",
		progname, top->sin_family,
		inet_ntoa(top->sin_addr),
		ntohs(top->sin_port),
		rpy.bfs.bh_type, ntohl(rpy.bfs.bh_offset),
		packetlen);
#endif

	if (sendto(s, &rpy, packetlen, 0,
	    top, sizeof(struct sockaddr_in)) != packetlen)
		perror ("sendto");

#ifdef DEBUG
	fprintf(stderr, "%s: sent reply to %d %s %d\n\r",
		progname, top->sin_family,
		inet_ntoa(top->sin_addr),
		ntohs(top->sin_port));
#endif

#ifndef SVR3
	oldsigs = sigblock(SIGMASK(SIGALRM));
#else
	sighold(SIGALRM);
#endif
	if (dt->dt_age != -1) {
		fprintf(stderr, "%s: desc_table entry unlocked!\n", progname);
		exit(1);
	}
	dt->dt_age = 0; 	/* unlock entry */
#ifndef SVR3
	sigsetmask(oldsigs);
#else
	sigrelse(SIGALRM);
#endif
}

age_signal()
{
	struct desc_table *dt;
	int active = 0;

	for (dt = desc_table; dt < &desc_table[NDESC]; dt++)
		if (*dt->dt_filename) {
			active++;
			if (dt->dt_age >= 0) {
				if (++dt->dt_age > MAXAGE) {
#ifdef DEBUG
					fprintf(stderr, "%s: closing %s\n\r",
					    progname, dt->dt_filename);
#endif
					*dt->dt_filename = 0;
					close(dt->dt_desc);
				}
			}
		}
	if (!active) {
		/* turn-off timer if no file descriptors to age */
#ifndef SVR3
		setitimer(ITIMER_REAL, &off_time, 0);
#endif
		timer_on = 0;
#ifdef DEBUG
		fprintf(stderr, "%s: timer off\n\r", progname);
#endif
	}
#ifdef SVR3
	else {
		alarm(age_time);
	}
	signal(SIGALRM, age_signal);
#endif
}

char *
pe_concat(s1, s2)
char *s1, *s2;
{
	static char buf[128];

	strcpy(buf, s1);
	strcat(buf, ": ");
	strcat(buf, s2);
	return(buf);
}

#define	SHELL	"/bin/csh"

/*
 *	expand a file name if it includes shell meta characters
 */
char *
expand(name)
char name[];
{
	static char xname[BUFSIZ];
	char cmdbuf[BUFSIZ];
	register int pid, l, rc;
	register char *cp;
	int s, pivec[2];

	if (!anyof(name, "~{[*?$`'\"\\"))
		return(name);
	if (pipe(pivec) < 0)
		return(name);
	sprintf(cmdbuf, "echo %s", name);
#ifndef SVR3
	if ((pid = vfork()) == 0) {
#else
	if ((pid = fork()) == 0) {
#endif
		close(pivec[0]);
		close(1);
		dup(pivec[1]);
		close(pivec[1]);
		close(2);
		execl(SHELL, SHELL, "-c", cmdbuf, 0);
		_exit(1);
	}
	if (pid == -1) {
		close(pivec[0]);
		close(pivec[1]);
		return(name);
	}
	close(pivec[1]);
	l = read(pivec[0], xname, BUFSIZ);
	close(pivec[0]);
	while (wait(&s) != pid);
		;
	s &= 0377;
	if ((s != 0 && s != SIGPIPE) || l <= 0 || l == BUFSIZ)
		return(name);
	xname[l] = 0;
	for (cp = &xname[l-1]; *cp == '\n' && cp > xname; cp--)
		;
	*++cp = '\0';
	return(xname);
}

/*
 * Are any of the characters in the two strings the same?
 */
anyof(s1, s2)
	register char *s1, *s2;
{
	register int c;

	while (c = *s1++)
		if (index(s2, c))
			return(1);
	return(0);
}

gateway_reqpacket(req, from, sock)
	struct bfs_packet *req;
	struct sockaddr_in *from;
	int sock;
{
	register i;
	int on, off;

	on = 1;
	off = 0;

	setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &on, sizeof(on));

	if (req->bfs.bh_type != BFSPT_ENQUIRE)
		return;

	for (i = 0 ; i < If_count ; i++)
		{
		if ((ifs[i].if_broadaddr & ifs[i].if_netmask) !=
		       (from->sin_addr.s_addr & ifs[i].if_netmask))
			/* then */
			{
			struct sockaddr_in to;
			long a;
#ifdef DEBUG
			fprintf(stderr,"gate to %s\n", ifs[i].if_name);
#endif DEBUG

			to.sin_addr.s_addr = ifs[i].if_broadaddr;
			to.sin_family = AF_INET;
			to.sin_port = BFSPORT_SERVER;

			req->bfs.bh_type = BFSPT_ENQFWD;
			bcopy(&(from->sin_addr.s_addr),
				&a, sizeof(from->sin_addr.s_addr));
#ifdef DEBUG
			fprintf(stderr,"writing %s\n", inet_ntoa(a));
#endif
			a = ntohl(a);
			bcopy(&a, req->data + htons(req->bfs.bh_pathlen),
				sizeof(from->sin_addr.s_addr));

			if (sendto(sock, req, sizeof(*req), 0, &to,
				sizeof(struct sockaddr_in)) != sizeof(*req))
				perror("sendto");
			} /* if broadaddr */
		} /* for interfaces */
	setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &on, sizeof(on));
}
