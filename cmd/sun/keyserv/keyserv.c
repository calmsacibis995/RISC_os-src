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
#ident	"$Header: keyserv.c,v 1.4.1.4 90/05/09 19:13:05 wje Exp $"

#ifndef lint
static char sccsid[] = 	"@(#)keyserv.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (C) 1986, Sun Microsystems, Inc.
 */

/*
 * Keyserver 
 * Store secret keys per uid. Do public key encryption and decryption
 * operations. Generate "random" keys. Do not talk to anything but a local root
 * process (by checking that the source port < IPPORT_RESERVED and by binding
 * to the loopback address). 
 */

#include <stdio.h>
#include <rpc/rpc.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/time.h>
#ifdef RISCOS
#include <sys/syslog.h>
#endif
#include <pwd.h>
#include <des_crypt.h>
#include <rpc/key_prot.h>


char ROOTKEY[] = "/etc/.rootkey";

extern long random();

extern keystatus pk_setkey();
extern keystatus pk_encrypt();
extern keystatus pk_decrypt();


#ifdef DEBUG
int debugging = 1;
#else
int debugging = 0;
#endif

static void keyprogram();
des_block masterkey;

main(argc, argv)
	int argc;
	char *argv[];

{
	SVCXPRT *transp;
	int nflag;

#ifdef RISCOS
	openlog("keyserv",LOG_PID,LOG_DAEMON);
#endif
	nflag = (argc == 2) && (strcmp(argv[1], "-n") == 0);
	if (!(argc == 1 || nflag)) {
		(void) fprintf(stderr, "usage: %s [-n]\n", argv[0]);
		exit(1);
	}
	/*
	 * Initialize 
	 */
	(void) umask(066);	/* paranoia */
	if (geteuid() != 0) {
#ifdef RISCOS
		syslog(LOG_ERR, "%s must be run as root\n", argv[0]);
#else
		(void) fprintf(stderr, "%s must be run as root\n", argv[0]);
#endif
		exit(1);
	}
	setmodulus(HEXMODULUS);
	openstore();
	getrootkey(&masterkey, nflag);
	readkeys();

	/*
	 * create the service, register it, and run 
	 */
	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,
			       "%s: unable to create udp service\n", argv[0]);
#else
		(void) fprintf(stderr,
			       "%s: unable to create udp service\n", argv[0]);
#endif
		exit(1);
	}
	pmap_unset(KEY_PROG, KEY_VERS);
	if (!svc_register(transp, KEY_PROG, KEY_VERS, keyprogram,
			  IPPROTO_UDP)) {
#ifdef RISCOS
		syslog(LOG_ERR, "%s: unable to register service\n",
			       argv[0]);
#else
		(void) fprintf(stderr, "%s: unable to register service\n",
			       argv[0]);
#endif
		exit(1);
	}
	/*
	 * run 
	 */
	if (!debugging) {
		detachfromtty();
	}
	svc_run();
	abort();
}

/*
 * In the event that we don't get a root password, we try to randomize the
 * master key the best we can 
 */
randomize(master)
	des_block *master;
{
	int i;
	int seed;
	struct timeval tv;
	int shift;

	seed = 0;
	for (i = 0; i < 1024; i++) {
		(void) gettimeofday(&tv, (struct timezone *) NULL);
		shift = i % 8 * sizeof(int);
		seed ^= (tv.tv_usec << shift) | (tv.tv_usec >> (32 - shift));
	}
	srandom(seed);
	master->key.low = random();
	master->key.high = random();
	srandom(seed);
}



/*
 * Try to get root's secret key, by prompting if terminal is a tty, else trying
 * from standard input. 
 */
getrootkey(master, prompt)
	des_block *master;
	int prompt;
{
	char *getpass();
	char *passwd;
	char name[MAXNETNAMELEN + 1];
	char secret[HEXKEYBYTES + 1];
	char *crypt();
	char *strrchr();
	int fd;

	if (!prompt) {
		/*
		 * Read secret key out of $ROOTKEY 
		 */
		fd = open(ROOTKEY, O_RDONLY, 0);
		if (fd < 0) {
			randomize(master);
			return (0);
		}
		if (read(fd, secret, HEXKEYBYTES) < 0) {
#ifdef RISCOS
			syslog(LOG_ERR, "Invalid %s\n", ROOTKEY);
#else
			(void) fprintf(stderr, "Invalid %s\n", ROOTKEY);
#endif
			(void) close(fd);
			return (0);
		}
		(void) close(fd);
		secret[HEXKEYBYTES] = 0;
	} else {
		/*
		 * Decrypt yellow pages entry to get secret key 
		 */
		passwd = getpass("root password:");
		passwd2des(passwd, master);
		getnetname(name);
		if (!getsecretkey(name, secret, passwd)) {
#ifdef RISCOS
			syslog(LOG_ERR,
				       "Can't find %s's secret key\n", name);
#else
			(void) fprintf(stderr,
				       "Can't find %s's secret key\n", name);
#endif
			return (0);
		}
		if (secret[0] == 0) {
#ifdef RISCOS
			syslog(LOG_ERR,
				       "Invalid password for %s\n", name);
#else
			(void) fprintf(stderr,
				       "Invalid password for %s\n", name);
#endif
			return (0);
		}
	}
	(void) pk_setkey(0, secret);
	return (1);
}


/*
 * Procedures to implement RPC service
 */

char *
strstatus(status)
	keystatus status;
{
	switch (status) {
	case KEY_SUCCESS:
		return ("KEY_SUCCESS");
	case KEY_NOSECRET:
		return ("KEY_NOSECRET");
	case KEY_UNKNOWN:
		return ("KEY_UNKNOWN");
	case KEY_SYSTEMERR:
		return ("KEY_SYSTEMERR");
	default:
		return ("(bad result code)");
	}
}

keystatus *
key_set_1(uid, key)
	short uid;
	keybuf key;
{
	static keystatus status;

	if (debugging) {
		(void) fprintf(stderr, "set(%d, %.*s) = ", uid,
			       sizeof(keybuf), key);
	}
	status = pk_setkey(uid, key);
	if (debugging) {
		(void) fprintf(stderr, "%s\n", strstatus(status));
		(void) fflush(stderr);
	}
	return (&status);
}



cryptkeyres *
key_encrypt_1(uid, arg)
	short uid;
	cryptkeyarg *arg;
{
	static cryptkeyres res;
	char name2[256];
	int i;

	/* get the real uid */
	sscanf(arg->remotename,"unix.%d@%s", &i, name2);
	if ( i > 0 && i < 65535 ) {
          uid = (short)i;
	}
	else {
	  uid = 0;
	}
#ifdef RISCOS
		syslog(LOG_ERR, "encrypt(%d, %s, %08x%08x) = ", uid,
			       arg->remotename, arg->deskey.key.high,
			       arg->deskey.key.low);
#else
		(void) fprintf(stderr, "encrypt(%d, %s, %08x%08x) = ", uid,
			       arg->remotename, arg->deskey.key.high,
			       arg->deskey.key.low);
#endif
	res.cryptkeyres_u.deskey = arg->deskey;
	res.status = pk_encrypt(uid, arg->remotename, &res.cryptkeyres_u.deskey);
	if (debugging) {
		if (res.status == KEY_SUCCESS) {
			(void) fprintf(stderr, "%08x%08x\n",
				       res.cryptkeyres_u.deskey.key.high,
				       res.cryptkeyres_u.deskey.key.low);
		} else {
#ifdef RISCOS
			syslog(LOG_ERR,
				       "%s\n", strstatus(res.status));
#else
			(void) fprintf(stderr,
				       "%s\n", strstatus(res.status));
#endif
		}
		(void) fflush(stderr);
	}
	return (&res);
}

cryptkeyres *
key_decrypt_1(uid, arg)
	short uid;
	cryptkeyarg *arg;
{
	static cryptkeyres res;
	char name2[256];
	int i;

	sscanf(arg->remotename,"unix.%d@%s", &i, name2);
	if ( i > 0 && i < 65535 ) {
          uid = (short)i;
	}
	else {
	  uid = 0;
	}
	if (debugging) {
		(void) fprintf(stderr, "decrypt(%d, %s, %08x%08x) = ", uid,
			       arg->remotename, arg->deskey.key.high,
			       arg->deskey.key.low);
	}
	res.cryptkeyres_u.deskey = arg->deskey;
	res.status = pk_decrypt(uid, arg->remotename,
				&res.cryptkeyres_u.deskey);
	if (debugging) {
		if (res.status == KEY_SUCCESS) {
			(void) fprintf(stderr, "%08x%08x\n",
				       res.cryptkeyres_u.deskey.key.high,
				       res.cryptkeyres_u.deskey.key.low);
		} else {
			(void) fprintf(stderr, "%s\n", strstatus(res.status));
		}
		(void) fflush(stderr);
	}
	return (&res);
}

des_block *
key_gen_1()
{
	struct timeval time;
	static des_block keygen;
	static des_block key;

	(void) gettimeofday(&time, (struct timezone *) NULL);
	keygen.key.high += (time.tv_sec ^ time.tv_usec);
	keygen.key.low += (time.tv_sec ^ time.tv_usec);
	ecb_crypt(&masterkey, &keygen, sizeof(keygen), DES_ENCRYPT | DES_HW);
	key = keygen;
	des_setparity(&key);
	if (debugging) {
		(void) fprintf(stderr, "gen() = %08x%08x\n", key.key.high,
			       key.key.low);
		(void) fflush(stderr);
	}
	return (&key);
}

/* ARGSUSED */
getcredres *
key_getcred_1(uid, name)
	short uid;
	netnamestr *name;
{
	static getcredres res;
	static int gids[NGROUPS];
	struct unixcred *cred;

	cred = &res.getcredres_u.cred;
	cred->gids.gids_val = gids;
	if (!netname2user(*name, &cred->uid, &cred->gid,
			  &cred->gids.gids_len, gids)) {
		res.status = KEY_UNKNOWN;
	} else {
		res.status = KEY_SUCCESS;
	}
	if (debugging) {
		(void) fprintf(stderr, "getcred(%s) = ", *name);
		if (res.status == KEY_SUCCESS) {
			(void) fprintf(stderr, "uid=%d,gid=%d,grouplen=%d\n",
				   cred->uid, cred->gid, cred->gids.gids_len);
		} else {
			(void) fprintf(stderr, "%s\n", strstatus(res.status));
		}
		(void) fflush(stderr);
	}
	return (&res);
}


/*
 * RPC boilerplate 
 */
static void
keyprogram(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		keybuf key_set_1_arg;
		cryptkeyarg key_encrypt_1_arg;
		cryptkeyarg key_decrypt_1_arg;
		des_block key_gen_1_arg;
	} argument;
	char *result;

	bool_t(*xdr_argument) (), (*xdr_result) ();
	char *(*local) ();
	struct sockaddr_in remote;
	int uid;
	int check_auth;

	switch (rqstp->rq_proc) {
	case NULLPROC:
		svc_sendreply(transp, xdr_void, (char *) NULL);
		return;

	case KEY_SET:
		xdr_argument = xdr_keybuf;
		xdr_result = xdr_int;
		local = (char *(*)()) key_set_1;
		check_auth = 1;
		break;

	case KEY_ENCRYPT:
		xdr_argument = xdr_cryptkeyarg;
		xdr_result = xdr_cryptkeyres;
		local = (char *(*)()) key_encrypt_1;
		check_auth = 1;
		break;

	case KEY_DECRYPT:
		xdr_argument = xdr_cryptkeyarg;
		xdr_result = xdr_cryptkeyres;
		local = (char *(*)()) key_decrypt_1;
		check_auth = 1;
		break;

	case KEY_GEN:
		xdr_argument = xdr_void;
#ifdef RISCOS
		xdr_result = xdr_deskey;
#else
		xdr_result = xdr_des_block;
#endif
		local = (char *(*)()) key_gen_1;
		check_auth = 0;
		break;

	case KEY_GETCRED:
		xdr_argument = xdr_netnamestr;
		xdr_result = xdr_getcredres;
		local = (char *(*)()) key_getcred_1;
		check_auth = 0;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	if (check_auth) {
		remote = *svc_getcaller(transp);
		if (ntohs(remote.sin_port) >= IPPORT_RESERVED ||
		    ntohl(remote.sin_addr.s_addr) != INADDR_LOOPBACK) {
			if (debugging) {
				(void) fprintf(stderr,
					      "not local privileged process\n");
			}
			svcerr_weakauth(transp);
			return;
		}
		if (rqstp->rq_cred.oa_flavor != AUTH_UNIX) {
			if (debugging) {
				(void) fprintf(stderr,
					       "not unix authentication\n");
			}
			svcerr_weakauth(transp);
			return;
		}
		uid = ((struct authunix_parms *) rqstp->rq_clntcred)->aup_uid;
	}
	bzero((char *) &argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local) (uid, &argument);
	if (!svc_sendreply(transp, xdr_result, (char *) result)) {
#ifdef RISCOS
		syslog(LOG_ERR, "unable to reply\n");
#else
		(void) fprintf(stderr, "unable to reply\n");
#endif
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
#ifdef RISCOS
		syslog(LOG_ERR, "unable to free arguments\n");
#else
		(void) fprintf(stderr, "unable to free arguments\n");
#endif
		exit(1);
	}
}
