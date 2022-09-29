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
/* $Header: auth_unix.h,v 1.2.1.3 90/05/07 20:56:12 wje Exp $ */
/*
 * @(#)auth_unix.h 1.1 86/09/24 Sun Microsystems, Inc.
 *
 * auth_unix.h, Protocol for UNIX style authentication parameters for RPC
 *
 * The system is very weak.  The client uses no encryption for  it
 * credentials and only sends null verifiers.  The server sends backs
 * null verifiers or optionally a verifier that suggests a new short hand
 * for the credentials.
 */

/* The machine name is part of a credential; it may not exceed 255 bytes */
#define MAX_MACHINE_NAME 255

/*
 * gids compose part of a credential
 * The reference port says the limit is 16. The NFS Protocol Specification
 * on page 10, section 3.1.2 implies that the limit is 10
 * How's that for consistency, we follow the reference port to hopefully
 * match the Sun implementation.
 */
#define NGRPS 16

/*
 * Unix style credentials.
 */
struct authunix_parms {
	u_long	 aup_time;
	char	*aup_machname;
	int	 aup_uid;
	int	 aup_gid;
	u_int	 aup_len;
	int	*aup_gids;
};

extern bool_t xdr_authunix_parms();

/* 
 * If a response verifier has flavor AUTH_SHORT, 
 * then the body of the response verifier encapsulates the following structure;
 * again it is serialized in the obvious fashion.
 */
struct short_hand_verf {
	struct opaque_auth new_cred;
};
