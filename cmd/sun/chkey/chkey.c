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
#ident	"$Header: chkey.c,v 1.1.1.2 90/05/09 19:10:57 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)chkey.c	1.1 88/07/14 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.2
 */

/*
 * Command to change one's public key in the public key database
 */
#include <stdio.h>
#include <rpc/rpc.h>
#include <rpc/key_prot.h>
#include <rpcsvc/ypclnt.h>
#include <sys/time.h>
#include <sys/file.h>
#include <pwd.h>
#include <mp.h>

extern char *getpass();
extern char *index();
extern char *crypt();
extern char *sprintf();
extern long random();

static char PKMAP[] = "publickey.byname";
static char *domain;
struct passwd *ypgetpwuid();

main(argc, argv)
	int argc;
	char **argv;
{
	char name[MAXNETNAMELEN+1];
	char public[HEXKEYBYTES + 1];
	char secret[HEXKEYBYTES + 1];
	char crypt1[HEXKEYBYTES + KEYCHECKSUMSIZE + 1];
	char crypt2[HEXKEYBYTES + KEYCHECKSUMSIZE + 1];
	int status;	
	char *pass;
	struct passwd *pw;
	char *master;
	int euid;
	int fd;
	int force;
	char *self;

	self = argv[0];
	force = 0;
	for (argc--, argv++; argc > 0 && **argv == '-'; argc--, argv++) {
		if (argv[0][2] != 0) {
			usage(self);
		}
		switch (argv[0][1]) {
		case 'f':
			force = 1;
			break;
		default:
			usage(self);
		}
	}
	if (argc != 0) {
		usage(self);
	}

	(void)yp_get_default_domain(&domain);
	if (yp_master(domain, PKMAP, &master) != 0) {
		(void)fprintf(stderr, 
			"can't find master of publickey database\n");
		exit(1);
	}

	getnetname(name);
	(void)printf("Generating new key for %s.\n", name);

	euid = geteuid();
	if (euid != 0) {
		pw = ypgetpwuid(euid);
		if (pw == NULL) {
			(void)fprintf(stderr, 
				"No yp password found: can't change key.\n");
			exit(1);
		}
	} else {
		pw = getpwuid(0);
		if (pw == NULL) {
			(void)fprintf(stderr, 
				"No password found: can't change key.\n");
			exit(1);
		}
	}
	pass = getpass("Password:");
	if (!force) {
		if (strcmp(crypt(pass, pw->pw_passwd), pw->pw_passwd) != 0) {
			(void)fprintf(stderr, "Invalid password.\n");
			exit(1);
		}
	}

	genkeys(public, secret, pass);	

	bcopy(secret, crypt1, HEXKEYBYTES);
	bcopy(secret, crypt1 + HEXKEYBYTES, KEYCHECKSUMSIZE);
	crypt1[HEXKEYBYTES + KEYCHECKSUMSIZE] = 0;
	xencrypt(crypt1, pass);

	if (force) {
		bcopy(crypt1, crypt2, HEXKEYBYTES + KEYCHECKSUMSIZE + 1);	
		xdecrypt(crypt2, getpass("Retype password:"));
		if (bcmp(crypt2, crypt2 + HEXKEYBYTES, KEYCHECKSUMSIZE) != 0 ||
	    	    bcmp(crypt2, secret, HEXKEYBYTES) != 0) {	
			(void)fprintf(stderr, "Password incorrect.\n");
			exit(1);
		}
	}

	(void)printf("Sending key change request to %s...\n", master);
	status = setpublicmap(name, public, crypt1);
	if (status != 0) {
		(void)printf("%s: unable to update yp database (%u): %s\n", 
			     self, status, yperr_string(status));
		(void)printf("Perhaps %s is down?\n", master);
		exit(1);
	}
	(void)printf("Done.\n");

	if (key_setsecret(secret) < 0) {
		(void)printf("Unable to login with new secret key.\n");
		exit(1);
	}
}

usage(name)
	char *name;
{
	(void)fprintf(stderr, "usage: %s [-f]\n", name);
	exit(1);
}


/*
 * Generate a seed
 */
getseed(seed, seedsize, pass)
	char *seed;
	int seedsize;
	unsigned char *pass;
{
	int i;
	int rseed;
	struct timeval tv;

	(void)gettimeofday(&tv, (struct timezone *)NULL);
	rseed = tv.tv_sec + tv.tv_usec;
	for (i = 0; i < 8; i++) {
		rseed ^= (rseed << 8) | pass[i];
	}
	srandom(rseed);

	for (i = 0; i < seedsize; i++) {
		seed[i] = (random() & 0xff) ^ pass[i % 8];
	}
}


/*
 * Generate a random public/secret key pair
 */
genkeys(public, secret, pass)
	char *public;
	char *secret;
	char *pass;
{
	int i;
 
#define BASEBITS	(8*sizeof(short) - 1)
#define BASE		(1 << BASEBITS)
 
	MINT *pk = itom(0);
 	MINT *sk = itom(0);
	MINT *tmp;
	MINT *base = itom(BASE);
	MINT *root = itom(PROOT);
	MINT *modulus = xtom(HEXMODULUS);
	short r;
	unsigned short seed[KEYSIZE/BASEBITS + 1];
	char *xkey;

	getseed((char *)seed, sizeof(seed), (unsigned char *)pass);	
	for (i = 0; i < KEYSIZE/BASEBITS + 1; i++) {
		r = seed[i] % BASE;
		tmp = itom(r);
		mult(sk, base, sk);
		madd(sk, tmp, sk);
		mfree(tmp);  
	}
	tmp = itom(0);
	mdiv(sk, modulus, tmp, sk);
	mfree(tmp);
	pow(root, sk, modulus, pk); 
	xkey = mtox(sk);   
	adjust(secret, xkey);
	xkey = mtox(pk);
	adjust(public, xkey);
	mfree(sk);
	mfree(base);
	mfree(pk);
	mfree(root);
	mfree(modulus);
} 

/*
 * Adjust the input key so that it is 0-filled on the left
 */
adjust(keyout, keyin)
	char keyout[HEXKEYBYTES+1];
	char *keyin;
{
	char *p;
	char *s;

	for (p = keyin; *p; p++) 
		;
	for (s = keyout + HEXKEYBYTES; p >= keyin; p--, s--) {
		*s = *p;
	}
	while (s >= keyout) {
		*s-- = '0';
	}
}

/*
 * Set the entry in the public key map
 */
setpublicmap(name, public, secret)
	char *name;
	char *public;
	char *secret;
{
	char pkent[1024];
	u_int rslt;
	
	(void)sprintf(pkent,"%s:%s", public, secret);
	rslt = yp_update(domain, PKMAP, YPOP_STORE, 
		name, strlen(name), pkent, strlen(pkent));
	return (rslt);
}

struct passwd *
ypgetpwuid(uid)
	int uid;
{
	char uidstr[10];
	char *val;
	int vallen;
	static struct passwd pw;
	char *p;

	(void)sprintf(uidstr, "%d", uid);
	if (yp_match(domain, "passwd.byuid", uidstr, strlen(uidstr), 
		     &val, &vallen) != 0) {
		return (NULL);
	}
	p = index(val, ':');
	if (p == NULL) {	
		return (NULL);
	}
	pw.pw_passwd = p + 1;
	p = index(pw.pw_passwd, ':');
	if (p == NULL) {
		return (NULL);
	}
	*p = 0;
	return (&pw);
}
