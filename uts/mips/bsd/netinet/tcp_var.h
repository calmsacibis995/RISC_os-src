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
/* $Header: tcp_var.h,v 1.11.1.2 90/05/10 04:31:40 wje Exp $ */

#ifndef	_BSD_NETINET_TCP_VAR_
#define	_BSD_NETINET_TCP_VAR_	1

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)tcp_var.h	7.7 (Berkeley) 2/27/88
 */

/*
 * TCP configuration:  This is a half-assed attempt to make TCP
 * self-configure for a few varieties of 4.2 and 4.3-based unixes.
 * If you don't have a) a 4.3bsd vax or b) a 3.x Sun (x<6), check
 * this carefully (it's probably not right).  Please send me mail
 * if you run into configuration problems.
 *  - Van Jacobson (van@lbl-csam.arpa)
 */

#undef BSD
#define BSD 43
#define OLDSTAT	/* set if we have to use old netstat binaries */

#define TCP_COMPAT_42	/* set if we have to interop w/4.2 systems */

#ifndef mips
#ifndef SB_MAX
#ifdef	SB_MAXCOUNT
#define	SB_MAX	SB_MAXCOUNT	/* Sun has to be a little bit different... */
#else
#define SB_MAX	32767		/* XXX */
#endif	SB_MAXCOUNT
#endif	SB_MAX
#endif mips

#ifndef IP_MAXPACKET
#define	IP_MAXPACKET	65535		/* maximum packet size */
#endif

/*
 * Bill Nowicki pointed out that the page size (CLBYTES) has
 * nothing to do with the mbuf cluster size.  So, we followed
 * Sun's lead and made the new define MCLBYTES stand for the mbuf
 * cluster size.  The following define makes up backwards compatible
 * with 4.3 and 4.2.  If CLBYTES is >1024 on your machine, check
 * this against the mbuf cluster definitions in /usr/include/sys/mbuf.h.
 */
#ifndef MCLBYTES
#define	MCLBYTES CLBYTES	/* XXX */
#endif

/*
 * The routine in_localaddr is broken in Sun's 3.4.  We redefine ours
 * (in tcp_input.c) so we use can it but won't have a name conflict.
 */
#ifdef sun
#define in_localaddr tcp_in_localaddr
#endif

/* --------------- end of TCP config ---------------- */

/*
 * Kernel variables for tcp.
 */

/*
 * Tcp control block, one per tcp; fields:
 */
struct tcpcb {
	struct	tcpiphdr *seg_next;	/* sequencing queue */
	struct	tcpiphdr *seg_prev;
	short	t_state;		/* state of this connection */
	short	t_timer[TCPT_NTIMERS];	/* tcp timers */
	short	t_rxtshift;		/* log(2) of rexmt exp. backoff */
	short	t_rxtcur;		/* current retransmit value */
	short	t_dupacks;		/* consecutive dup acks recd */
	u_short	t_maxseg;		/* maximum segment size */
	char	t_force;		/* 1 if forcing out a byte */
	u_char	t_flags;
#define	TF_ACKNOW	0x01		/* ack peer immediately */
#define	TF_DELACK	0x02		/* ack, but try to delay it */
#define	TF_NODELAY	0x04		/* don't delay packets to coalesce */
#define	TF_NOOPT	0x08		/* don't use tcp options */
#define	TF_SENTFIN	0x10		/* have sent FIN */
	struct	tcpiphdr *t_template;	/* skeletal packet for transmit */
	struct	inpcb *t_inpcb;		/* back pointer to internet pcb */
/*
 * The following fields are used as in the protocol specification.
 * See RFC783, Dec. 1981, page 21.
 */
/* send sequence variables */
	tcp_seq	snd_una;		/* send unacknowledged */
	tcp_seq	snd_nxt;		/* send next */
	tcp_seq	snd_up;			/* send urgent pointer */
	tcp_seq	snd_wl1;		/* window update seg seq number */
	tcp_seq	snd_wl2;		/* window update seg ack number */
	tcp_seq	iss;			/* initial send sequence number */
	u_short	snd_wnd;		/* send window */
/* receive sequence variables */
	u_short	rcv_wnd;		/* receive window */
	tcp_seq	rcv_nxt;		/* receive next */
	tcp_seq	rcv_up;			/* receive urgent pointer */
	tcp_seq	irs;			/* initial receive sequence number */
/*
 * Additional variables for this implementation.
 */
/* receive variables */
	tcp_seq	rcv_adv;		/* advertised window */
/* retransmit variables */
	tcp_seq	snd_max;		/* highest sequence number sent
					 * used to recognize retransmits
					 */
/* congestion control (for slow start, source quench, retransmit after loss) */
	u_short	snd_cwnd;		/* congestion-controlled window */
	u_short snd_ssthresh;		/* snd_cwnd size threshhold for
					 * for slow start exponential to
					 * linear switch */
/*
 * transmit timing stuff.
 * srtt and rttvar are stored as fixed point; for convenience in smoothing,
 * srtt has 3 bits to the right of the binary point, rttvar has 2.
 * "Variance" is actually smoothed difference.
 */
	short	t_idle;			/* inactivity time */
	short	t_rtt;			/* round trip time */
	tcp_seq	t_rtseq;		/* sequence number being timed */
	short	t_srtt;			/* smoothed round-trip time */
	short	t_rttvar;		/* variance in round-trip time */
	u_short max_rcvd;		/* most peer has sent into window */
	u_short	max_sndwnd;		/* largest window peer has offered */
/* out-of-band data */
	char	t_oobflags;		/* have some */
	char	t_iobc;			/* input character */
#define	TCPOOB_HAVEDATA	0x01
#define	TCPOOB_HADDATA	0x02
};

#define	intotcpcb(ip)	((struct tcpcb *)(ip)->inp_ppcb)
#define	sototcpcb(so)	(intotcpcb(sotoinpcb(so)))

/*
 * TCP statistics.
 * Many of these should be kept per connection,
 * but that's inconvenient at the moment.
 */
struct	tcpstat {
#ifdef OLDSTAT
	/*
	 * Declare statistics the same as in 4.3
	 * at the start of tcpstat (same size and
	 * position) for netstat.
	 */
	int	tcps_rcvbadsum;
	int	tcps_rcvbadoff;
	int	tcps_rcvshort;
	int	tcps_badsegs;
	int	tcps_unack;
#define	tcps_badsum	tcps_rcvbadsum
#define	tcps_badoff	tcps_rcvbadoff
#define	tcps_hdrops	tcps_rcvshort

#endif OLDSTAT
	u_long	tcps_connattempt;	/* connections initiated */
	u_long	tcps_accepts;		/* connections accepted */
	u_long	tcps_connects;		/* connections established */
	u_long	tcps_drops;		/* connections dropped */
	u_long	tcps_conndrops;		/* embryonic connections dropped */
	u_long	tcps_closed;		/* conn. closed (includes drops) */
	u_long	tcps_segstimed;		/* segs where we tried to get rtt */
	u_long	tcps_rttupdated;	/* times we succeeded */
	u_long	tcps_delack;		/* delayed acks sent */
	u_long	tcps_timeoutdrop;	/* conn. dropped in rxmt timeout */
	u_long	tcps_rexmttimeo;	/* retransmit timeouts */
	u_long	tcps_persisttimeo;	/* persist timeouts */
	u_long	tcps_keeptimeo;		/* keepalive timeouts */
	u_long	tcps_keepprobe;		/* keepalive probes sent */
	u_long	tcps_keepdrops;		/* connections dropped in keepalive */

	u_long	tcps_sndtotal;		/* total packets sent */
	u_long	tcps_sndpack;		/* data packets sent */
	u_long	tcps_sndbyte;		/* data bytes sent */
	u_long	tcps_sndrexmitpack;	/* data packets retransmitted */
	u_long	tcps_sndrexmitbyte;	/* data bytes retransmitted */
	u_long	tcps_sndacks;		/* ack-only packets sent */
	u_long	tcps_sndprobe;		/* window probes sent */
	u_long	tcps_sndurg;		/* packets sent with URG only */
	u_long	tcps_sndwinup;		/* window update-only packets sent */
	u_long	tcps_sndctrl;		/* control (SYN|FIN|RST) packets sent */

	u_long	tcps_rcvtotal;		/* total packets received */
	u_long	tcps_rcvpack;		/* packets received in sequence */
	u_long	tcps_rcvbyte;		/* bytes received in sequence */
#ifndef OLDSTAT
	u_long	tcps_rcvbadsum;		/* packets received with ccksum errs */
	u_long	tcps_rcvbadoff;		/* packets received with bad offset */
	u_long	tcps_rcvshort;		/* packets received too short */
#endif
	u_long	tcps_rcvduppack;	/* duplicate-only packets received */
	u_long	tcps_rcvdupbyte;	/* duplicate-only bytes received */
	u_long	tcps_rcvpartduppack;	/* packets with some duplicate data */
	u_long	tcps_rcvpartdupbyte;	/* dup. bytes in part-dup. packets */
	u_long	tcps_rcvoopack;		/* out-of-order packets received */
	u_long	tcps_rcvoobyte;		/* out-of-order bytes received */
	u_long	tcps_rcvpackafterwin;	/* packets with data after window */
	u_long	tcps_rcvbyteafterwin;	/* bytes rcvd after window */
	u_long	tcps_rcvafterclose;	/* packets rcvd after "close" */
	u_long	tcps_rcvwinprobe;	/* rcvd window probe packets */
	u_long	tcps_rcvdupack;		/* rcvd duplicate acks */
	u_long	tcps_rcvacktoomuch;	/* rcvd acks for unsent data */
	u_long	tcps_rcvackpack;	/* rcvd ack packets */
	u_long	tcps_rcvackbyte;	/* bytes acked by rcvd acks */
	u_long	tcps_rcvwinupd;		/* rcvd window update packets */

	u_long	tcps_activeopen;	/* number of direct transitions to SYN-SENT from CLOSED state */
	u_long	tcps_passiveopen;	/* number of direct transition to SYN_RCVD from LISTEN state */
	u_long	tcps_attemptfail;	/* number of direct transitions to CLOSED from eith SYN_SENT or SYN_RCVD state */
	u_long	tcps_estabreset;	/* number of direct transitions to CLOSED from eith ESTABLISHED or CLOSE-WAIT */
	u_long	tcps_currestab;		/* number of connections either ESTABLISHED or CLOSE-WAIT */

};

#ifdef KERNEL
struct	inpcb tcb;		/* head of queue of active tcpcb's */
struct	tcpstat tcpstat;	/* tcp statistics */
struct	tcpiphdr *tcp_template();
struct	tcpcb *tcp_close(), *tcp_drop();
struct	tcpcb *tcp_timers(), *tcp_disconnect(), *tcp_usrclosed();
#endif

#endif	_BSD_NETINET_TCP_VAR_
