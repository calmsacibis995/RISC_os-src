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
#ident	"$Header: klm_lockmgr.c,v 1.2.1.2.1.1.1.4 90/11/20 16:38:20 beacker Exp $"
/*
 * @(#)klm_lockmgr.c 2.1 88/05/24 4.0NFSSRC Copyright Sun Microsystems, Inc.
 *
 * Kernel<->Network Lock-Manager Interface
 *
 * File- and Record-locking requests are forwarded (via RPC) to a
 * Network Lock-Manager running on the local machine.  The protocol
 * for these transactions is defined in /usr/src/protocols/klm_prot.x
 *
 * Original includes: param.h systm.h user.h kernel.h socket.h socketvar.h
 * vfs.h vnode.h proc.h file.h stat.h ../rpc/rpc.h
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/errno.h"
#include "sys/systm.h"

#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"

#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "bsd43/sys/stat.h"

#include "sys/socket.h"
#include "sys/socketvar.h"

#include "../rpc/rpc.h"

#include "../klm/lockmgr.h"
#include "../rpcsvc/klm_prot.h"
#include "bsd/net/if.h"
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"

/* Following includes are required to perform the protocol pcb lookup's */
#include "bsd/net/if.h"
#include "bsd/net/route.h"
#include "bsd/netinet/in_systm.h"
#include "bsd/netinet/in.h"
#include "bsd/netinet/in_pcb.h"
#include "bsd/netinet/ip.h"
#include "bsd/netinet/ip_var.h"
#include "bsd/netinet/udp.h"
#include "bsd/netinet/udp_var.h"

static struct sockaddr_in lm_sa;	/* talk to portmapper & lock-manager */

static local_lm_active();
static int talk_to_lockmgr();

extern struct inpcb *in_pcblookup();
extern int wakeup();

static int klm_debug = 0;

/* Define static parameters for run-time tuning */
static int backoff_timeout = 30; /* seconds to wait on klm_denied_nolocks */

static int first_retry = 0;	/* first attempt if klm port# known */
static int first_timeout = 15;   /* first operation timeout in seconds */

static int normal_retry = 1;	/* attempts after new port# obtained */
static int normal_timeout = 15;	/* timeout in seconds on rpc errors */

static int working_retry = 0;	/* attempts after klm_working */
static int working_timeout = 15; /* timeout in seconds on 'working' return */

/*
 * klm_lockctl - process a lock/unlock/test-lock request
 *
 * Calls (via RPC) the local lock manager to register the request.
 * Lock requests are cancelled if interrupted by signals.
 */
klm_lockctl(lh, bfp, cmd, cred, clid)
	lockhandle_t *lh;
	struct flock *bfp;
	int cmd;
	struct ucred *cred;
	int clid;
{
	register int	error;
	int tid_normal;
	klm_lockargs	args;
	klm_testrply	reply;
	u_long		xdrproc;
	xdrproc_t	xdrargs;
	xdrproc_t	xdrreply;

	/* initialize sockaddr_in used to talk to local processes */
	if (lm_sa.sin_port == 0) {
#ifdef notdef
		struct ifnet	*ifp;

		if ((ifp = if_ifwithafup(AF_INET)) == (struct ifnet *)NULL) {
			panic("klm_lockctl: no inet address");
		}
		lm_sa = *(struct sockaddr_in *) &(ifp->if_addr);
#else
		lm_sa.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		lm_sa.sin_family = AF_INET;
#endif
	}

	args.block = FALSE;
	args.exclusive = FALSE;
	args.alock.fh.n_bytes = (char *)&lh->lh_id;
	args.alock.fh.n_len = sizeof (lh->lh_id);
	args.alock.server_name = lh->lh_servername;
	args.alock.pid = clid;
	args.alock.base = bfp->l_start;
	args.alock.length = bfp->l_len;
	xdrproc = KLM_LOCK;
	xdrargs = (xdrproc_t)xdr_klm_lockargs;
	xdrreply = (xdrproc_t)xdr_klm_stat;

	/* now modify the lock argument structure for specific cases */
	switch (bfp->l_type) {
	case F_WRLCK:
		args.exclusive = TRUE;
		break;
	case F_UNLCK:
		xdrproc = KLM_UNLOCK;
		xdrargs = (xdrproc_t)xdr_klm_unlockargs;
		break;
	}

	switch (cmd) {
	case F_SETLKW:
		args.block = TRUE;
		break;
	case F_GETLK:
		xdrproc = KLM_TEST;
		xdrargs = (xdrproc_t)xdr_klm_testargs;
		xdrreply = (xdrproc_t)xdr_klm_testrply;
		break;
	}

requestloop:
	/* send the request out to the local lock-manager and wait for reply */
	error = talk_to_lockmgr(xdrproc,xdrargs, &args, xdrreply, &reply,
			cred, lh->interruptable);
	if (error == ENOLCK) {
		goto ereturn;	/* no way the request could have gotten out */
	}

	/*
	 * The only other possible return values are:
	 * klm_granted | klm_denied | klm_denied_nolocks | klm_deadlck | EINTR
	 */
	switch (xdrproc) {
	case KLM_LOCK:
		switch (error) {
		case klm_granted:
			error = 0;		/* got the requested lock */
			goto ereturn;
		case klm_denied:
			if (args.block) {
			  printf("klm_lockmgr: blocking lock denied?!\n");
			  goto requestloop;	/* loop forever */
			}
			error = EACCES;		/* EAGAIN?? */
			goto ereturn;
		case klm_denied_nolocks:
			error = ENOLCK;		/* no resources available?! */
			goto ereturn;
		case klm_deadlck:
                        error = EDEADLK;        /* deadlock condition */
                        goto ereturn;
		case EINTR:
			if (args.block || lh->interruptable)
			        /* cancel blocking locks OR if interruptable */
				goto cancel;
			  else                   /* loop forever */
				goto requestloop;
		}

	case KLM_UNLOCK:
		switch (error) {
		case klm_granted:
			error = 0;
			goto ereturn;
		case klm_denied:
		case EINTR:
			printf("klm_lockmgr: unlock denied?!\n");
			error = EINVAL;
			goto ereturn;
		case klm_denied_nolocks:
			goto nolocks_wait;	/* back off; loop forever */
		}

	case KLM_TEST:
		switch (error) {
		case klm_granted:
			bfp->l_type = F_UNLCK;	/* mark lock available */
			error = 0;
			goto ereturn;
		case klm_denied:
                        bfp->l_type = (reply.klm_testrply_u.holder.exclusive) ?
                            F_WRLCK : F_RDLCK;
                        bfp->l_start = reply.klm_testrply_u.holder.base;
                        bfp->l_len = reply.klm_testrply_u.holder.length;
                        bfp->l_pid = reply.klm_testrply_u.holder.pid;
                        bfp->l_sysid = reply.klm_testrply_u.holder.rsys;
			error = 0;
			goto ereturn;
		case klm_denied_nolocks:
			goto nolocks_wait;	/* back off; loop forever */
		case EINTR:
			/* may want to take a longjmp here */
			goto requestloop;	/* loop forever */
		}
	}

/*NOTREACHED*/
nolocks_wait:
	tid_normal = timeout(wakeup, (caddr_t)&lm_sa, (backoff_timeout * HZ));
	(void) sleep((caddr_t)&lm_sa, PZERO|PCATCH);
	untimeout(tid_normal);
	goto requestloop;	/* now try again */

cancel:
	/*
	 * If we get here, a signal interrupted a rqst that must be cancelled.
	 * Change the procedure number to KLM_CANCEL and reissue the exact same
	 * request.  Use the results to decide what return value to give.
	 */
	xdrproc = KLM_CANCEL;
	error = talk_to_lockmgr(xdrproc,xdrargs, &args, xdrreply, &reply,
			cred, lh->interruptable);
	switch (error) {
	case klm_granted:
		error = 0;		/* lock granted */
		break;

	case klm_denied:
		error = EINTR;		/* may want to take a longjmp here */
		break;

	case klm_deadlck:
		error = EDEADLK;        /* deadlock condition */
		break;

	case EINTR:
		printf("klm_lockmgr: EINTR response to cancel request\n");
		/* Changed to NOT loop forever waiting for cancel reply msg's*/
		break;

	case klm_denied_nolocks:
		error = ENOLCK;		/* no resources available?! */
		break;

	case ENOLCK:
		printf("klm_lockctl: ENOLCK on KLM_CANCEL request\n");
		break;
	default:
          printf("klm_lockmgr: Error= %d; on KLM_CANCEL request\n", error);
		break;
	}
ereturn:
	return(error);
}

/*
 * Returns TRUE if a listener pcb is associated with the local IP address
 * and port number; Otherwise FALSE is returned presumably indicating that
 * the local lock manager has died. This is a special validity check even
 * though the port mapper indicates the local lock manager port was registered.
 */
static
local_lm_active (lm_sap)
struct sockaddr_in *lm_sap;
{
struct inpcb *udp_pcb;
int retval;

udp_pcb = in_pcblookup (&udb, INADDR_ANY, 0, lm_sap->sin_addr.s_addr,
			lm_sap->sin_port, INPLOOKUP_WILDCARD);

retval = (udp_pcb != (struct inpcb *)NULL) ? TRUE : FALSE;
return retval;		     

}

/*
 * Send the given request to the local lock-manager.
 * If timeout or error, go back to the portmapper to check the port number.
 * This routine loops forever until one of the following occurs:
 *	1) A legitimate (not 'klm_working') reply is returned (returns 'stat').
 *
 *	2) A signal occurs (returns EINTR).  In this case, at least one try
 *	   has been made to do the RPC; this protects against jamming the
 *	   CPU if a KLM_CANCEL request has yet to go out.
 *
 *	3) A drastic error occurs (e.g., the local lock-manager has never
 *	   been activated OR cannot create a client-handle) (returns ENOLCK).
 */
static
talk_to_lockmgr(xdrproc, xdrargs, args, xdrreply, reply, cred, interruptable)
	u_long xdrproc;
	xdrproc_t xdrargs;
	klm_lockargs *args;
	xdrproc_t xdrreply;
	klm_testrply *reply;
	struct ucred *cred;
	int interruptable;
{
	register CLIENT *client;
	struct timeval tmo;
	register int error, portval;
	int tid_error;

	/* set up a client handle to talk to the local lock manager */
	client = clntkudp_create(&lm_sa, (u_long)KLM_PROG, (u_long)KLM_VERS,
	    first_retry, cred);
	if (client == (CLIENT *) NULL) {
		return(ENOLCK);
	}

	if (interruptable)
	  clntkudp_interruptable (client, TRUE);
	tmo.tv_sec = first_timeout;
	tmo.tv_usec = 0;

	/*
	 * If cached port number, go right to CLNT_CALL().
	 * This works because timeouts go back to the portmapper to
	 * refresh the port number.
	 */
	if (lm_sa.sin_port != 0) {
		goto retryloop;		/* skip first portmapper query */
	}

	for (;;) {
remaploop:
		/*
		 * Go get the port number from the portmapper.
		 * if return 1, signal was received before portmapper answered;
		 * if return -1, the lock-manager is not registered
		 * else, got a port number
		 */
		portval = getport_loop(&lm_sa, (u_long)KLM_PROG,
				(u_long)KLM_VERS, (u_long)KLM_PROTO);

		switch (portval) {
		case 1:
			error = EINTR;		/* signal interrupted things */
			goto out;

		case -1:
			uprintf("fcntl: Local lock-manager not registered\n");
			error = ENOLCK;
			goto out;
		}

		/* Check if the port for the local lock-manager is alive */
		if (!local_lm_active(&lm_sa)) {
			uprintf("fcntl: Local lock-manager not active\n");
			error = ENOLCK;
			goto out;
		}

		/*
		 * If a signal occurred, pop back out to the higher
		 * level to decide what action to take.  If we just
		 * got a port number from the portmapper, the next
		 * call into this subroutine will jump to retryloop.
		 */
		if (issig(u.u_procp)) {
			error = EINTR;
			goto out;
		}

		/* reset the lock-manager client handle */
		(void) clntkudp_init(client, &lm_sa, normal_retry, cred);
		tmo.tv_sec = normal_timeout;

retryloop:
		/* retry the request until completion, timeout, or error */
		for (;;) {
			error = (int) CLNT_CALL(client, xdrproc, xdrargs,
			    (caddr_t)args, xdrreply, (caddr_t)reply, tmo);

			if (klm_debug)
			  printf(
    "klm_lockmgr: pid %d; cmd %d; base %d; len %d; error %d; stat %d\n",
			args->alock.pid, (int)xdrproc, args->alock.base,
			args->alock.length, error, (int)reply->stat);

			switch (error) {
			case RPC_SUCCESS:
				error = (int) reply->stat;
				if (error == (int) klm_working) {
					if (issig(u.u_procp)) {
						error = EINTR;
						goto out;
					}
					/* lock-mgr is up...can wait longer */
					(void) clntkudp_init(client, &lm_sa,
					    working_retry, cred);
					tmo.tv_sec = working_timeout;
					continue;	/* retry */
				}
				goto out;	/* got a legitimate answer */

			case RPC_TIMEDOUT:
				goto remaploop;	/* ask for port# again */

			default:
				printf("klm_lockmgr: RPC error: %s\n",
				    clnt_sperrno((enum clnt_stat) error));

				if (error == RPC_INTR)
				  { /* RPC call interrupted */
				  error = EINTR;
				  goto out;
				  }

				/* on RPC error, wait a bit and try again */
				tid_error = timeout(wakeup, (caddr_t)&lm_sa,
				    (normal_timeout * HZ));
				error = sleep((caddr_t)&lm_sa, PZERO|PCATCH);
				untimeout(tid_error);
				if (error) {
				    error = EINTR;
				    goto out;
				}
				goto remaploop;	/* ask for port# again */

			} /*switch*/

		} /*for*/	/* loop until timeout, error, or completion */
	} /*for*/		/* loop until signal or completion */

out:
	AUTH_DESTROY(client->cl_auth);	/* drop the authenticator */
	CLNT_DESTROY(client);		/* drop the client handle */
	return(error);
}
