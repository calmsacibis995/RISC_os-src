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
#ident	"$Header: auth_des.c,v 1.4.1.2 90/05/07 20:55:16 wje Exp $"
/*
 * @(#)auth_des.c 1.8 88/08/15 4.0NFSSRC SMI
 *
 * auth_des.c, client-side implementation of DES authentication
 *
 * Original kernel includes:
 * ../des/des_crypt.h ../rpc/types.h ../rpc/auth.h ../rpc/auth_des.h
 * ../rpc/xdr.h ../netinet/in.h socket.h
 */

#ifdef KERNEL
#include "../des/des_crypt.h"
#include "../rpc/types.h"
#include "../rpc/auth.h"
#include "../rpc/auth_des.h"
#include "../rpc/xdr.h"
#include "bsd/netinet/in.h"	/* XXX: just to get htonl() and ntohl() */
#include "sys/socket.h"
#else
#include <des_crypt.h>
#include <rpc/types.h>
#include <rpc/auth.h>
#include <rpc/auth_des.h>
#include <rpc/xdr.h>
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>	/* XXX: just to get htonl() and ntohl() */
#include <sys/socket.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#include <bsd/sys/socket.h>
#endif
#endif

#define MILLION		1000000L
#define RTIME_TIMEOUT 5		/* seconds to wait for sync */

#define AUTH_PRIVATE(auth)	(struct ad_private *) auth->ah_private
#define ALLOC(object_type)	(object_type *) mem_alloc(sizeof(object_type))
#define FREE(ptr, size)		mem_free((char *)(ptr), (int) size)
#define ATTEMPT(xdr_op)		if (!(xdr_op)) return (FALSE)

#define debug(msg)		 printf("%s\n", msg) 

#ifdef KERNEL
#define gettimeofday(tvp, tzp)	uniqtime(tvp)	/* fake system call */
#endif

/* 
 * DES authenticator operations vector
 */
static void	authdes_nextverf();
static bool_t	authdes_marshal();
static bool_t	authdes_validate();
static bool_t	authdes_refresh();
static void	authdes_destroy();
static struct auth_ops authdes_ops = {
	authdes_nextverf,
	authdes_marshal,
	authdes_validate,
	authdes_refresh,
	authdes_destroy
};


/*
 * This struct is pointed to by the ah_private field of an "AUTH *"
 */
struct ad_private {
	char *ad_fullname; 		/* client's full name */
	u_int ad_fullnamelen;		/* length of name, rounded up */
	char *ad_servername; 		/* server's full name */
	u_int ad_servernamelen;		/* length of name, rounded up */
	u_int ad_window;	  	/* client specified window */
	bool_t ad_dosync;		/* synchronize? */		
	struct sockaddr ad_syncaddr;	/* remote host to synch with */
	struct timeval ad_timediff;	/* server's time - client's time */
	u_long ad_nickname;		/* server's nickname for client */
	struct authdes_cred ad_cred;	/* storage for credential */
	struct authdes_verf ad_verf;	/* storage for verifier */
	struct timeval ad_timestamp;	/* timestamp sent */
	des_block ad_xkey;		/* encrypted conversation key */
};
	

/*
 * Create the client des authentication object
 */	
AUTH *
authdes_create(servername, window, syncaddr, ckey)
	char *servername;		/* network name of server */
	u_int window;			/* time to live */
	struct sockaddr *syncaddr;	/* optional addr of host to sync with */
	des_block *ckey;		/* optional conversation key to use*/
{

	AUTH *auth;
	struct ad_private *ad;
	char namebuf[MAXNETNAMELEN+1];

	/*
 	 * Allocate everything now
	 */
	auth = ALLOC(AUTH);
	ad = ALLOC(struct ad_private);
	(void) getnetname(namebuf);

	ad->ad_fullnamelen = RNDUP(strlen(namebuf));
	ad->ad_fullname = mem_alloc(ad->ad_fullnamelen + 1);

	ad->ad_servernamelen = strlen(servername);
	ad->ad_servername = mem_alloc(ad->ad_servernamelen + 1);

	if (auth == NULL || ad == NULL || ad->ad_fullname == NULL ||
	    ad->ad_servername == NULL) {
		debug("authdes_create: out of memory");
		goto failed;
	}

	/* 
	 * Set up private data
	 */
	bcopy(namebuf, ad->ad_fullname, ad->ad_fullnamelen + 1);
	bcopy(servername, ad->ad_servername, ad->ad_servernamelen + 1);
	if (syncaddr != NULL) {
		ad->ad_syncaddr = *syncaddr;
		ad->ad_dosync = TRUE;
	} else {
		ad->ad_dosync = FALSE;
	}
	ad->ad_window = window;
	if (ckey == NULL) {
		if (key_gendes(&auth->ah_key) < 0) {
			debug("authdes_create: unable to gen conversation key");
			return (NULL);
		}
	} else {
		auth->ah_key = *ckey;
	}

	/*
	 * Set up auth handle
	 */ 
	auth->ah_cred.oa_flavor = AUTH_DES;
	auth->ah_verf.oa_flavor = AUTH_DES;
	auth->ah_ops = &authdes_ops;
	auth->ah_private = (caddr_t)ad;

	if (!authdes_refresh(auth)) {
		goto failed;
	}	
	return (auth);

failed:
	if (auth != NULL) 
		FREE(auth, sizeof(AUTH)); 
	if (ad != NULL) 
		FREE(ad, sizeof(struct ad_private));
	if (ad->ad_fullname != NULL) 
		FREE(ad->ad_fullname, ad->ad_fullnamelen + 1);
	if (ad->ad_servername != NULL) 
		FREE(ad->ad_servername, ad->ad_servernamelen + 1);
	return (NULL);
}

/*
 * Implement the five authentication operations
 */


/*
 * 1. Next Verifier
 */	
/*ARGSUSED*/
static void
authdes_nextverf(auth)
	AUTH *auth;
{
	/* what the heck am I supposed to do??? */
}
		


/*
 * 2. Marshal
 */
static bool_t
authdes_marshal(auth, xdrs)
	AUTH *auth;
	XDR *xdrs;
{
	struct ad_private *ad = AUTH_PRIVATE(auth);
	struct authdes_cred *cred = &ad->ad_cred;
	struct authdes_verf *verf = &ad->ad_verf;
	des_block cryptbuf[2];	
	des_block ivec;
	int status;
	int len;
	register long *ixdr;

	/*
	 * Figure out the "time", accounting for any time difference
	 * with the server if necessary.
	 */
	(void) gettimeofday(&ad->ad_timestamp, (struct timezone *)NULL);
	ad->ad_timestamp.tv_sec += ad->ad_timediff.tv_sec;
	ad->ad_timestamp.tv_usec += ad->ad_timediff.tv_usec;
	if (ad->ad_timestamp.tv_usec >= MILLION) {
		ad->ad_timestamp.tv_usec -= MILLION;
		ad->ad_timestamp.tv_sec += 1;
	}

	/*
	 * XDR the timestamp and possibly some other things, then
	 * encrypt them.
	 */
	ixdr = (long *)cryptbuf;
	IXDR_PUT_LONG(ixdr, ad->ad_timestamp.tv_sec);
	IXDR_PUT_LONG(ixdr, ad->ad_timestamp.tv_usec);
	if (ad->ad_cred.adc_namekind == ADN_FULLNAME) {
		IXDR_PUT_U_LONG(ixdr, ad->ad_window);
		IXDR_PUT_U_LONG(ixdr, ad->ad_window - 1);
		ivec.key.high = ivec.key.low = 0;	
		status = cbc_crypt((char *)&auth->ah_key, (char *)cryptbuf, 
			2*sizeof(des_block), DES_ENCRYPT | DES_HW, (char *)&ivec);
	} else {
		status = ecb_crypt((char *)&auth->ah_key, (char *)cryptbuf, 
			sizeof(des_block), DES_ENCRYPT | DES_HW);
	}
	if (DES_FAILED(status)) {
		debug("authdes_marshal: DES encryption failure");
		return (FALSE);
	}
	ad->ad_verf.adv_xtimestamp = cryptbuf[0];
	if (ad->ad_cred.adc_namekind == ADN_FULLNAME) {
		ad->ad_cred.adc_fullname.window = cryptbuf[1].key.high;
		ad->ad_verf.adv_winverf = cryptbuf[1].key.low;
	} else {
		ad->ad_cred.adc_nickname = ad->ad_nickname;
		ad->ad_verf.adv_winverf = 0;
	}

	/*
	 * Serialize the credential and verifier into opaque
	 * authentication data.
	 */
	if (ad->ad_cred.adc_namekind == ADN_FULLNAME) {
		len = ((1 + 1 + 2 + 1)*BYTES_PER_XDR_UNIT + ad->ad_fullnamelen);
	} else {
		len = (1 + 1)*BYTES_PER_XDR_UNIT;
	}

	if (ixdr = xdr_inline(xdrs, 2*BYTES_PER_XDR_UNIT)) {
		IXDR_PUT_LONG(ixdr, AUTH_DES);
		IXDR_PUT_LONG(ixdr, len);
	} else {
		ATTEMPT(xdr_putlong(xdrs, &auth->ah_cred.oa_flavor)); 
		ATTEMPT(xdr_putlong(xdrs, &len)); 
	}
	ATTEMPT(xdr_authdes_cred(xdrs, cred));

	len = (2 + 1)*BYTES_PER_XDR_UNIT; 
	if (ixdr = xdr_inline(xdrs, 2*BYTES_PER_XDR_UNIT)) {
		IXDR_PUT_LONG(ixdr, AUTH_DES);
		IXDR_PUT_LONG(ixdr, len);
	} else {
		ATTEMPT(xdr_putlong(xdrs, &auth->ah_verf.oa_flavor)); 
		ATTEMPT(xdr_putlong(xdrs, &len)); 
	}
	ATTEMPT(xdr_authdes_verf(xdrs, verf));
	return (TRUE);
}


/*
 * 3. Validate
 */
static bool_t
authdes_validate(auth, rverf)
	AUTH *auth;
	struct opaque_auth *rverf;
{
	struct ad_private *ad = AUTH_PRIVATE(auth);
	struct authdes_verf verf;
	int status;
	register u_long *ixdr;
	XDR xop;

	if (rverf->oa_length != (2 + 1) * BYTES_PER_XDR_UNIT) {
		return (FALSE);
	}
	ixdr = (u_long *)rverf->oa_base;
	verf.adv_xtimestamp.key.high = (u_long)*ixdr++;
	verf.adv_xtimestamp.key.low = (u_long)*ixdr++;
	/*
	 * This is a lot of work just to save 4 miserable bytes !
	 */
	xdrmem_create(&xop,(char *)ixdr++,sizeof(u_long),XDR_DECODE);
	ATTEMPT(xdr_opaque(&xop,(char *)&verf.adv_int_u,sizeof(u_long)));

	/*
	 * Decrypt the timestamp
	 */
	status = ecb_crypt((char *)&auth->ah_key, (char *)&verf.adv_xtimestamp,
		sizeof(des_block), DES_DECRYPT | DES_HW);

	if (DES_FAILED(status)) {
		debug("authdes_validate: DES decryption failure");
		return (FALSE);
	}

	/*
	 * xdr the decrypted timestamp 
	 */
	ixdr = (u_long *)verf.adv_xtimestamp.c;
	verf.adv_timestamp.tv_sec = IXDR_GET_LONG(ixdr) + 1;
	verf.adv_timestamp.tv_usec = IXDR_GET_LONG(ixdr);

	/*
	 * validate
	 */
	if (bcmp((char *)&ad->ad_timestamp, (char *)&verf.adv_timestamp,
		 sizeof(struct timeval)) != 0) {
		debug("authdes_validate: verifier mismatch\n");
		return (FALSE);
	}

	/*
	 * We have a nickname now, let's use it
	 */
	ad->ad_nickname = verf.adv_nickname;	
	ad->ad_cred.adc_namekind = ADN_NICKNAME; 
	return (TRUE);	
}

/*
 * 4. Refresh
 */
static bool_t
authdes_refresh(auth)
	AUTH *auth;
{
	struct ad_private *ad = AUTH_PRIVATE(auth);
	struct authdes_cred *cred = &ad->ad_cred;

	if (ad->ad_dosync && 
			!synchronize(&ad->ad_syncaddr, &ad->ad_timediff)) {
		/*
		 * Hope the clocks are synced!
		 */
		ad->ad_timediff.tv_sec = ad->ad_timediff.tv_usec = 0;
		debug("authdes_refresh: unable to synchronize with server");
	}
	ad->ad_xkey = auth->ah_key;
#ifdef RISCOS
	if (key_encryptsession(ad->ad_fullname, &ad->ad_xkey) < 0) {
#else
	if (key_encryptsession(ad->ad_servername, &ad->ad_xkey) < 0) {
#endif
		debug("authdes_create: unable to encrypt conversation key");
		return (FALSE);
	}
	cred->adc_fullname.key = ad->ad_xkey;
	cred->adc_namekind = ADN_FULLNAME;
	cred->adc_fullname.name = ad->ad_fullname;
	return (TRUE);
}


/*
 * 5. Destroy
 */
static void
authdes_destroy(auth)
	AUTH *auth;
{
	struct ad_private *ad = AUTH_PRIVATE(auth);

	FREE(ad->ad_fullname, ad->ad_fullnamelen + 1);
	FREE(ad->ad_servername, ad->ad_servernamelen + 1);
	FREE(ad, sizeof(struct ad_private));
	FREE(auth, sizeof(AUTH));
}
	


/*
 * Synchronize with the server at the given address, that is,
 * adjust timep to reflect the delta between our clocks
 */
static bool_t
synchronize(syncaddr, timep)
	struct sockaddr *syncaddr;
	struct timeval *timep;
{
	struct timeval mytime;
	struct timeval timeout;

	timeout.tv_sec = RTIME_TIMEOUT;
	timeout.tv_usec = 0;
	if (rtime((struct sockaddr_in *)syncaddr, timep, &timeout) < 0) {
		return (FALSE);
	}
	(void) gettimeofday(&mytime, (struct timezone *)NULL);
	timep->tv_sec -= mytime.tv_sec;
	if (mytime.tv_usec > timep->tv_usec) {
		timep->tv_sec -= 1;
		timep->tv_usec += MILLION;
	}
	timep->tv_usec -= mytime.tv_usec;
	return (TRUE);
}
