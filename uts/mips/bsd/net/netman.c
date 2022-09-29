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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: netman.c,v 1.8.3.3.1.1.1.2 90/10/05 09:51:20 beacker Exp $"

#include "../tcp-param.h"
#include "sys/param.h"
#include "sys/socket.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "../netinet/in.h"
#include "../net/if.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/ip_icmp.h"
#include "../netinet/icmp_var.h"
#include "sys/mbuf.h"
#include "../net/route.h"
#include "../netinet/in_pcb.h"
#include "../netinet/tcp.h"
#include "../netinet/tcp_timer.h"
#include "../netinet/tcp_var.h"
#include "../netinet/udp.h"
#include "../netinet/udp_var.h"
#include "../netinet/if_ether.h"
#include "../netinet/in_var.h"
#include "sys/protosw.h"
#include "sys/utsname.h"
#include "sys/netman.h"

extern struct arptab arptab[];
extern int arptab_size;

int
addr_to_ifindex (addr)
int addr;
{
	struct ifnet * ifidx;
	struct in_ifaddr * ifaidx;
	int idx;

	for (ifidx = ifnet, idx = 1; ifidx; ifidx = ifidx->if_next) {
		for (ifaidx = (struct in_ifaddr *) ifidx->if_addrlist; ifaidx; ifaidx = ifaidx->ia_next) {
			if ((ifaidx->ia_subnet & addr) == ifaidx->ia_subnet)
				break;
		}
		if (ifaidx != (struct in_ifaddr *) 0)
			break;
		idx++;
	}

	if (ifidx == (struct ifnet *) 0)
		return (0);
	else return (idx);
}


int
ifp_to_ifindex (ifp)
struct ifnet * ifp;
{
	struct ifnet * ifidx;
	int idx;

/* First we will make sure that ifp is in the interfaces list. */
	for (ifidx = ifnet; ifidx && ifidx != ifp; ifidx = ifidx->if_next);

/* Since we add new interfaces to the front of the list, the first
 * element is actually the last interface.
 */
	for (idx = 0; ifidx; ifidx = ifidx->if_next, idx++);

	return (idx);
}

int
get_system_data (data, index, next)
char * data;
unsigned long index;
int next;

{
	struct sysinfo sysent;
	extern struct timeval time;
	extern struct timeval boottime;
	char * strp;
	int len;

	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	strp = sysent.sys_id;

	len = strlen (utsname.sysname);
	bcopy (utsname.sysname, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.nodename);
	bcopy (utsname.nodename, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.release);
	bcopy (utsname.release, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.version);
	bcopy (utsname.version, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.machine);
	bcopy (utsname.machine, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.m_type);
	bcopy (utsname.m_type, strp, len);
	strp[len] = ' ';
	strp += len + 1;
	len = strlen (utsname.base_rel);
	bcopy (utsname.base_rel, strp, len);
	strp += len;
	*strp = '\0';

	sysent.sys_objidlen = 8;
	bcopy ("\53\6\1\4\1\71", sysent.sys_objid, 6);
	sysent.sys_objid[6] = '\1';
	if (strcmp (utsname.m_type, MT_M500) == 0) {
		sysent.sys_objid[7] = '\1';
	} else if (strcmp (utsname.m_type, MT_M800) == 0) {
		sysent.sys_objid[7] = '\2';
	} else if (strcmp (utsname.m_type, MT_M1000) == 0) {
		sysent.sys_objid[7] = '\3';
	} else if (strcmp (utsname.m_type, MT_DT1200_5) == 0) {
		sysent.sys_objid[7] = '\4';
	} else if (strcmp (utsname.m_type, MT_DT1200_3) == 0) {
		sysent.sys_objid[7] = '\5';
	} else if (strcmp (utsname.m_type, MT_DT1200) == 0) {
		sysent.sys_objid[7] = '\6';
	} else if (strcmp (utsname.m_type, MT_M2000_8) == 0) {
		sysent.sys_objid[7] = '\7';
	} else if (strcmp (utsname.m_type, MT_M2000_6) == 0) {
		sysent.sys_objid[7] = '\10';
	} else if (strcmp (utsname.m_type, MT_M2000) == 0) {
		sysent.sys_objid[7] = '\11';
	} else if (strcmp (utsname.m_type, MT_I2000) == 0) {
		sysent.sys_objid[7] = '\12';
	} else if (strcmp (utsname.m_type, MT_M6000) == 0) {
		sysent.sys_objid[7] = '\13';
	} else if (strcmp (utsname.m_type, MT_M180) == 0) {
		sysent.sys_objid[7] = '\14';
	} else if (strcmp (utsname.m_type, MT_R3030) == 0) {
		sysent.sys_objid[7] = '\15';
	} else sysent.sys_objid[7] = '\0';	/* unknown machine type */

	sysent.sys_lastinit = (time.tv_sec - boottime.tv_sec) * 100 + 
				(time.tv_usec - boottime.tv_usec) / 10000;

	if (copyout (&sysent, data, sizeof (sysent)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}

	return (1);
}

int
get_total_interfaces (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct ifnet *ifidx;
	int ifcnt;

	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	for (ifidx = ifnet, ifcnt = 0; ifidx; ifidx = ifidx->if_next, ifcnt++);
	if (copyout (&ifcnt, data, sizeof (ifcnt)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}
	
	return (1);
}

static void
itoa( num, buf )
unsigned short num;
char * buf;
{
	unsigned short i, div, flag = 0;

	if (num == 0) {
		*buf++ = '0';
	} else for (div = 10000; div > 0; div /= 10 ) {
		if ((i = num / div) || flag) {
			*buf++ = '0' + i;
			num -= (i * div);
			flag = 1;
		}
	}
	*buf = 0;
}

int
get_interface_entry (data, index, next)
char * data;
unsigned long index;
int next;
{
	int idx, cnt;
	struct ifnet *ifidx;
	struct ifentry ifent;

	if (data == (char *) 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	
	for (ifidx = ifnet, cnt = 0; ifidx; ifidx = ifidx->if_next, cnt++);

	if (cnt == 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	if (next)
		cnt--;

	if ((cnt = cnt - index + 1) <= 0)
		return (0);

	idx = 1;
	ifidx = ifnet;
	while (idx < cnt) {
		ifidx = ifidx->if_next;
		if (ifidx == (struct ifnet *) 0) break;
		idx++;
	}

	if (ifidx == (struct ifnet *) 0) {
		return (0);
	} else {
		ifent.if_index = index;
		if (ifidx->if_description != (char *) 0) {
			strcpy (ifent.if_descr, ifidx->if_name);
			ifent.if_descrlen = strlen (ifent.if_descr);
			itoa (ifidx->if_unit, &ifent.if_descr[ifent.if_descrlen]);
			ifent.if_descrlen = strlen (ifent.if_descr);
		} else ifent.if_descrlen = 0;
			
		ifent.if_type = ifidx->if_type;
		ifent.if_mtu = ifidx->if_mtu;
		ifent.if_speed = ifidx->if_speed;
		ifent.if_physaddrlen = ifidx->if_physaddrlen;
		bcopy (ifidx->if_physaddr, ifent.if_physaddr, ifidx->if_physaddrlen);

		if (ifidx->if_flags & IFF_UP) {
			ifent.if_adminstat = 1;
			ifent.if_operstat = 1;
		} else {
			ifent.if_adminstat = 0;
			ifent.if_operstat = 0;
		}
		ifent.if_uptime = ifidx->if_lastchange;
		ifent.if_rbyte = ifidx->if_ioctets;
		ifent.if_rdirpkt = ifidx->if_ipackets - ifidx->if_inucastpackets;
		ifent.if_rcastpkt = ifidx->if_inucastpackets;
		ifent.if_rdiscard = ifidx->if_idiscards;
		ifent.if_rerr = ifidx->if_ierrors;
		ifent.if_rbadproto = ifidx->if_iunknownprotos;
		ifent.if_xbyte = ifidx->if_ooctets;
		ifent.if_xdirpkt = ifidx->if_opackets - ifidx->if_onucastpackets;
		ifent.if_xcastpkt = ifidx->if_onucastpackets;
		ifent.if_xdiscard = ifidx->if_odiscards;
		ifent.if_xerr = ifidx->if_oerrors;
		ifent.if_xqlen = ifidx->if_snd.ifq_len;
		if (copyout (&ifent, data, sizeof (struct ifentry)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
		if (next)
			return (index + 1);
		else
			return (index);
	}
}

int
get_attable (data, index, next)
char * data;
unsigned long index;
int next;

{
	struct atentry atent;
	int idx, jdx, kdx;
	unsigned long addr;
	unsigned char *low, *high, *current;

	if (data == (char *) 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	low = (unsigned char *) &index;
	addr = 0xFFFFFFFF;
	high = (unsigned char *) &addr;
	kdx = 0;

	for (idx = 0; idx < arptab_size; idx++) {
		if (next) {
			if (arptab[idx].at_flags != 0) {
				current = (unsigned char *)&arptab[idx].at_iaddr.s_addr;
				for (jdx = 0; jdx < 4; jdx++) {
					if ((current[jdx] < low[jdx]) ||
						(current[jdx] > high[jdx]))
						break;

					if ((current[jdx] > low[jdx]) &&
						(high[jdx] > current[jdx])) {
						addr = arptab[idx].at_iaddr.s_addr;
						kdx = idx;
						break;
					}
				}
			}
		} else {
			if ((arptab[idx].at_flags != 0) &&
				(arptab[idx].at_iaddr.s_addr == index)) {
				kdx = idx;
				break;
			}
		}
	}

	if (kdx) {
		atent.at_index = addr_to_ifindex (arptab[kdx].at_iaddr.s_addr);
		atent.at_physaddr_size = 6;
		bcopy (arptab[kdx].at_enaddr, atent.at_physaddr, 6);
		atent.at_netaddr = arptab[kdx].at_iaddr.s_addr;
		if (copyout (&atent, data, sizeof (atent)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
		return (arptab[kdx].at_iaddr.s_addr);
	} else
		return (0);
}

int
get_ip_data (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct ipinfo ipent;
	struct ifnet * ifidx;
	int idx;

	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	for (ifidx = ifnet, idx = 0; ifidx && idx < 2; ifidx = ifidx->if_next) {
		if ((ifidx->if_flags & IFF_UP) && !(ifidx->if_flags & IFF_LOOPBACK))
			idx++;
	}
	if (idx > 1)
		ipent.ip_forwarding = 1;
	else ipent.ip_forwarding = 0;

	ipent.ip_defttl = MAXTTL;
	ipent.ip_rpkts = ipstat.ips_total;
	ipent.ip_rhdrerr = ipstat.ips_badsum + ipstat.ips_badhlen;
	ipent.ip_raddrerr = ipstat.ips_cantforward;
	ipent.ip_forwarded = ipstat.ips_forward;
	ipent.ip_badproto = ipstat.ips_iunknownproto;
	ipent.ip_rdiscard = ipstat.ips_idiscards;
	ipent.ip_delivers = ipstat.ips_total - ipstat.ips_badsum - ipstat.ips_tooshort - ipstat.ips_toosmall - ipstat.ips_badhlen - ipstat.ips_badlen;
	ipent.ip_xpkts = ipstat.ips_opackets;
	ipent.ip_xdiscard = ipstat.ips_odiscards;
	ipent.ip_noroute = ipstat.ips_noroutes;
	ipent.ip_reasm_timeout = ipstat.ips_fragtimeout;
	ipent.ip_reasm_req = ipstat.ips_fragments;
	ipent.ip_reasm_ok = ipstat.ips_reasmok;
	ipent.ip_reasm_fail = ipstat.ips_fragfails;
	ipent.ip_frag_ok = ipstat.ips_fragok;
	ipent.ip_frag_fail = ipstat.ips_fragfails;
	ipent.ip_frag = ipstat.ips_fragtotal;
	if (copyout (&ipent, data, sizeof (ipent)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}
	return (1);
}

int
get_ipaddrentry (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct in_ifaddr *addridx, *addrjdx;
	struct ipaddrtab addrent;
	struct sockaddr_in *sin;
	int idx, jdx;
unsigned long addr;
unsigned char *low, *high, *current;	

	if (data == (char *) 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	low = (unsigned char *) &index;
	addr = 0xFFFFFFFF;
	high = (unsigned char *) &addr;
	addrjdx = (struct in_ifaddr *) 0;

	for (addridx = in_ifaddr; addridx; addridx = addridx->ia_next) {
		sin = (struct sockaddr_in *) &addridx->ia_ifa.ifa_addr;
		if (next) {
			current = (unsigned char *)&sin->sin_addr.s_addr;
			for (jdx = 0; jdx < 4; jdx++) {
				if ((current[jdx] < low[jdx]) ||
					(current[jdx] > high[jdx]))
					break;

				if ((current[jdx] > low[jdx]) &&
					(high[jdx] > current[jdx])) {
					addr = sin->sin_addr.s_addr;
					addrjdx = addridx;
					break;
				}
			}
		} else if (sin->sin_addr.s_addr == index) {
			addrjdx = addridx;
			break;
		}
	}

	if (addrjdx == (struct in_ifaddr *) 0) {
		return (0);
	} else {
		sin = (struct sockaddr_in *) &addrjdx->ia_ifa.ifa_addr;
		addrent.ipa_addr = sin->sin_addr.s_addr;
		addrent.ipa_ifindex = ifp_to_ifindex (addrjdx->ia_ifa.ifa_ifp);
		addrent.ipa_mask = addrjdx->ia_subnetmask;
		addrent.ipa_bcast = (addrjdx->ia_netbroadcast.s_addr & 1);
		if (copyout (&addrent, data, sizeof (addrent)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
		return (index + 1);
	}
}

int
get_iprouteentry (data, index, next)
char * data;
unsigned long index;
int next;
{
	extern int rthashsize;

	struct rtentry *rp, *rpidx;
	struct mbuf * msg;
	int cnt, idx, jdx, kdx, ldx;
	struct iproutab rtent;
	struct sockaddr_in * sin;
unsigned long addr;
unsigned char *low, *current, *high;

	if (data == (char *) 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	cnt = 0;
	idx = 0;
	jdx = 0;
	low = (unsigned char *) &index;
	addr = 0xFFFFFFFF;
	high = (unsigned char *) &addr;
	rpidx = (struct rtentry *) 0;
	msg = rthost[idx];
	while (1) {
		while (msg == (struct mbuf *) 0) {
			idx++;
			if (idx == rthashsize) {
				idx = 0;
				jdx++;
				if (jdx > 1)
					break;
			}
			if (jdx == 0)
				msg = rthost[idx];
			else msg = rtnet[idx];
		}

		if (msg == (struct mbuf *) 0)
			break;

		rp = mtod (msg, struct rtentry *);
		sin = (struct sockaddr_in *)&rp->rt_dst;
/* XXX */	if (!(rp->rt_ifp->if_flags & IFF_LOOPBACK)) {
			if (next) {
				current = (unsigned char *)&sin->sin_addr.s_addr;
				for (kdx = 0; kdx < 4; kdx++) {
					if ((current[kdx] < low[kdx]) ||
						(current[kdx] > high[kdx]))
						break;

					if ((current[kdx] > low[kdx]) &&
						(high[kdx] > current[kdx])) {
						addr = sin->sin_addr.s_addr;
						rpidx = rp;
						break;
					}
				}
			} else if (sin->sin_addr.s_addr == index) {
				rpidx = rp;
				break;
			}
		}
		msg = msg->m_next;
		cnt++;
	}
	if (rpidx != (struct rtentry *) 0) {
		sin = (struct sockaddr_in *) &rpidx->rt_dst;
		rtent.ipr_dest = sin->sin_addr.s_addr;
		rtent.ipr_ifindex = ifp_to_ifindex (rpidx->rt_ifp);
		if (copyout (&rtent, data, sizeof (rtent)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
		return (rtent.ipr_dest);
	} else {
		return (0);
	}						
}

int
get_icmp_data (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct icmpinfo icmpent;
	int idx;

	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	icmpent.icmp_rpkt = icmpstat.icps_badcode + icmpstat.icps_tooshort + icmpstat.icps_checksum + icmpstat.icps_badlen;
	for (idx = 0; idx < ICMP_MAXTYPE; idx++) {
		icmpent.icmp_rpkt += (Counter)icmpstat.icps_inhist[idx];
	}

	icmpent.icmp_rerr = icmpstat.icps_badcode + icmpstat.icps_tooshort + icmpstat.icps_checksum + icmpstat.icps_badlen;
	icmpent.icmp_rdest_unreach = icmpstat.icps_inhist[ICMP_UNREACH];
	icmpent.icmp_rtime_exceed = icmpstat.icps_inhist[ICMP_TIMXCEED];
	icmpent.icmp_rparm_prob = icmpstat.icps_inhist[ICMP_PARAMPROB];
	icmpent.icmp_rsrc_quench = icmpstat.icps_inhist[ICMP_SOURCEQUENCH];
	icmpent.icmp_rredirect = icmpstat.icps_inhist[ICMP_REDIRECT];
	icmpent.icmp_recho = icmpstat.icps_inhist[ICMP_ECHO];
	icmpent.icmp_recho_rep = icmpstat.icps_inhist[ICMP_ECHOREPLY];
	icmpent.icmp_rtimes = icmpstat.icps_inhist[ICMP_TSTAMP];
	icmpent.icmp_rtimes_rep = icmpstat.icps_inhist[ICMP_TSTAMPREPLY];
	icmpent.icmp_raddrmask = icmpstat.icps_inhist[ICMP_MASKREQ];
	icmpent.icmp_raddrmask_rep = icmpstat.icps_inhist[ICMP_MASKREPLY];

	icmpent.icmp_xpkt = icmpstat.icps_error + icmpstat.icps_oldicmp;
	for (idx = 0; idx < ICMP_MAXTYPE; idx++) {
		icmpent.icmp_xpkt += icmpstat.icps_inhist[idx];
	}
	icmpent.icmp_xerr = icmpstat.icps_error + icmpstat.icps_oldicmp;
	icmpent.icmp_xdest_unreach = icmpstat.icps_outhist[ICMP_UNREACH];
	icmpent.icmp_xtime_exceed = icmpstat.icps_outhist[ICMP_TIMXCEED];
	icmpent.icmp_xparm_prob = icmpstat.icps_outhist[ICMP_PARAMPROB];
	icmpent.icmp_xsrc_quench = icmpstat.icps_outhist[ICMP_SOURCEQUENCH];
	icmpent.icmp_xredirect = icmpstat.icps_outhist[ICMP_REDIRECT];
	icmpent.icmp_xecho = icmpstat.icps_outhist[ICMP_ECHO];
	icmpent.icmp_xecho_rep = icmpstat.icps_outhist[ICMP_ECHOREPLY];
	icmpent.icmp_xtimes = icmpstat.icps_outhist[ICMP_TSTAMP];
	icmpent.icmp_xtimes_rep = icmpstat.icps_outhist[ICMP_TSTAMPREPLY];
	icmpent.icmp_xaddrmask = icmpstat.icps_outhist[ICMP_MASKREQ];
	icmpent.icmp_xaddrmask_rep = icmpstat.icps_outhist[ICMP_MASKREPLY];

	if (copyout (&icmpent, data, sizeof (icmpent)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}

	return (1);
}

int
get_tcp_data (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct tcpinfo tcpent;

	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	tcpent.tcp_rtalg = 4;
	tcpent.tcp_rtmin = TCPTV_MIN;
	tcpent.tcp_rtmax = TCPTV_REXMTMAX;
	tcpent.tcp_maxcon = -1;
	tcpent.tcp_actopen = tcpstat.tcps_activeopen;
	tcpent.tcp_pasopen = tcpstat.tcps_passiveopen;
	tcpent.tcp_fails = tcpstat.tcps_attemptfail;
	tcpent.tcp_est_reset = tcpstat.tcps_estabreset;
	tcpent.tcp_estab = tcpstat.tcps_currestab;
	tcpent.tcp_rpkt = tcpstat.tcps_rcvtotal;
	tcpent.tcp_xpkt = tcpstat.tcps_sndtotal;
	tcpent.tcp_retrans = tcpstat.tcps_sndrexmitpack;

	if (copyout (&tcpent, data, sizeof (tcpent)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}

	return (1);
}

int
get_tcpconnentry (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct tcpcontab tcpent;
	struct tcpcb * tcpidx;
	struct inpcb * inpcbidx;
	char * StateMask = "\001\002\003\004\005\010\006\012\011\007\013";
	int idx;

	if (data == (char *) 0) {
		u.u_error = EINVAL;
		return (-1);
	}

	idx = 1;
	inpcbidx = tcb.inp_next;

	if (next)
		index++;

	while (idx < index) {
		if ((inpcbidx == (struct inpcb *) 0) ||
			(inpcbidx->inp_next == inpcbidx->inp_head))
			break;
		inpcbidx = inpcbidx->inp_next;
		idx++;
	}

	if ((idx < index) ||
		(inpcbidx == (struct inpcb *) 0)) {
		return (0);
	} else {
		tcpidx = (struct tcpcb *) inpcbidx->inp_ppcb;
		tcpent.tcpc_state = StateMask[tcpidx->t_state];
		tcpent.tcpc_laddr = inpcbidx->inp_laddr.s_addr;
		tcpent.tcpc_lport = inpcbidx->inp_lport;
		tcpent.tcpc_raddr = inpcbidx->inp_faddr.s_addr;
		tcpent.tcpc_rport = inpcbidx->inp_fport;
		if (copyout (&tcpent, data, sizeof (tcpent)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
		return (index);
	}
}

int
get_udp_data (data, index, next)
char * data;
unsigned long index;
int next;
{
	struct udpinfo udpent;
	
	if ((index > 0) || (data == (char *) 0) || next) {
		u.u_error = EINVAL;
		return (-1);
	}

	udpent.udp_rpkt = udpstat.udps_indatagrams;
	udpent.udp_noport = udpstat.udps_noport;
	udpent.udp_rerr = udpstat.udps_hdrops + udpstat.udps_badsum + udpstat.udps_badlen;
	udpent.udp_xpkt = udpstat.udps_outdatagrams;

	if (copyout (&udpent, data, sizeof (udpent)) < 0) {
		u.u_error = EFAULT;
		return (-1);
	}

	return (1);
}

int
get_egp_data (data, index, next)
char * data;
unsigned long index;
int next;
{
	u.u_error = EINVAL;
	return (-1);
}

int
get_egpneighentry (data, index, next)
char * data;
unsigned long index;
int next;
{
	u.u_error = EINVAL;
	return (-1);
}

int
get_mib_data (key, data, index, next)
int key;
char * data;
unsigned long index;
long next;

{
	switch (key) {
	case ISO_ORG_DOD_INTERNET_MGMT_MIB_SYSTEM:
		return (get_system_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFNUMBER:
		return (get_total_interfaces (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFTABLE_IFENTRY:
		return (get_interface_entry (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_AT_ATTABLE_ATENTRY:
		return (get_attable (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_IP:
		return (get_ip_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPADDRTABLE_IPADDRENTRY:
		return (get_ipaddrentry (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPROUTINGTABLE_IPROUTEENTRY:
		return (get_iprouteentry (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_ICMP:
		return (get_icmp_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP:
		return (get_tcp_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP_TCPCONNTABLE_TCPCONNENTRY:
		return (get_tcpconnentry (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_UDP:
		return (get_udp_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_EGP:
		return (get_egp_data (data, index, next));

	case ISO_ORG_DOD_INTERNET_MGMT_MIB_EGP_EGPNEIGHTABLE_EGPNEIGHENTRY:
		return (get_egpneighentry (data, index, next));

	default:
		u.u_error = EINVAL;
		return (-1);
	}
}
