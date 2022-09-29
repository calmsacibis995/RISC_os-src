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
#ident	"$Header: if.c,v 1.2.1.2 90/05/09 17:00:13 wje Exp $"
/*
 *   CENTER FOR THEORY AND SIMULATION IN SCIENCE AND ENGINEERING
 *			CORNELL UNIVERSITY
 *
 *      Portions of this software may fall under the following
 *      copyrights: 
 *
 *	Copyright (c) 1983 Regents of the University of California.
 *	All rights reserved.  The Berkeley software License Agreement
 *	specifies the terms and conditions for redistribution.
 *
 *  GATED - based on Kirton's EGP, UC Berkeley's routing daemon (routed),
 *	    and DCN's HELLO routing Protocol.
 */

#ifndef	lint
static char *rcsid = "$Header: if.c,v 1.2.1.2 90/05/09 17:00:13 wje Exp $";
#endif	not lint

/*
 * if.c
 *
 * Functions: if_withnet, if_check, if_print
 */

#include "include.h"

/*
 * Find the interface on the network of the specified address.
 */
struct interface *
if_withnet(withnetaddr)
	register struct sockaddr_in *withnetaddr;
{
  register struct interface *ifp;
  register u_long net1, tmp, net2;

  if (withnetaddr->sin_family != AF_INET)
    return (0);

  /* get network part of withnetaddr */
  tmp = ntohl(withnetaddr->sin_addr.s_addr);
  net1 = gd_inet_wholenetof(withnetaddr->sin_addr);
  for (ifp = ifnet; ifp; ifp = ifp->int_next) {
    if ((ifp->int_netmask & net1) == ifp->int_net) {
      net1 = tmp & ifp->int_subnetmask;
      break;
    }
  }
  /* search for ifp */
  for (ifp = ifnet; ifp; ifp = ifp->int_next) {
    if (ifp->int_flags & IFF_POINTOPOINT) {
      tmp = ntohl(in_addr_ofs(&ifp->int_dstaddr).s_addr);
      net2 = gd_inet_wholenetof(in_addr_ofs(&ifp->int_dstaddr));
      if ((ifp->int_netmask & net2) == ifp->int_net)
         net2 = tmp & ifp->int_subnetmask;
    }
    else {
      tmp = ntohl(in_addr_ofs(&ifp->int_addr).s_addr);
      net2 = gd_inet_wholenetof(in_addr_ofs(&ifp->int_addr));
      if ((ifp->int_netmask & net2) == ifp->int_net)
        net2 = tmp & ifp->int_subnetmask;
    }
    if (net1 == net2)
      break;
  }
  return(ifp);
}

#ifdef notdef
/* used for DEBUGing */
if_print()
{
  register struct interface *ifp;

  for (ifp = ifnet; ifp; ifp = ifp->int_next) {
    if_display("if_print", ifp);
  }
}
#endif	notdef

#ifndef	NSS
/*
 * if_check() checks the current status of all interfaces
 * If any interface has changed status, then the interface values
 * are re-read from the kernel and re-set.
 */

if_check()
{
  register struct interface *ifp;
  struct ifreq ifrequest;
  int  if_change = FALSE;
  struct sockaddr_in *sin;
  u_long a;
  int info_sock;

  if ((info_sock = getsocket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    p_error("if_check: no info socket ");
    return;
  }
  for (ifp = ifnet; ifp != NULL; ifp = ifp->int_next) {
    /* get interface status flags */
    (void) strcpy(ifrequest.ifr_name, ifp->int_name);
    if (ioctl(info_sock, SIOCGIFFLAGS, (char *)&ifrequest)) {
      (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFFLAGS:", ifp->int_name);
      p_error(err_message);
    } else {
      if ((ifrequest.ifr_flags & IFF_UP) != (ifp->int_flags & IFF_UP)) {
        if_change = TRUE;
        if (ifrequest.ifr_flags & IFF_UP) {
          ifp->int_flags = IFF_INTERFACE |
                           (ifrequest.ifr_flags & IFF_MASK) |
                           (ifp->int_flags & IFF_KEEPMASK);

#if	defined(INT_METRIC)
          (void) strcpy(ifrequest.ifr_name, ifp->int_name);
          if (ioctl(info_sock, SIOCGIFMETRIC, (char *)&ifrequest) < 0) {
            (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFMETRIC:", ifp->int_name);
            p_error(err_message);
          } else {
            ifp->int_metric = (ifrequest.ifr_metric >= 0) ?
                                 ifrequest.ifr_metric : 0;
          }
#else	defined(INT_METRIC)
          ifp->int_metric =  0;
#endif	defined(INT_METRIC)
          if (ifp->int_flags & IFF_POINTOPOINT) {
            (void) strcpy(ifrequest.ifr_name, ifp->int_name);
            if (ioctl(info_sock, SIOCGIFDSTADDR, (char *)&ifrequest) < 0) {
              (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFDSTADDR:", ifp->int_name);
              p_error(err_message);
            } else {
              ifp->int_dstaddr = ifrequest.ifr_dstaddr;
            }
          }
          (void) strcpy(ifrequest.ifr_name, ifp->int_name);
          if (ioctl(info_sock, SIOCGIFADDR, (char *)&ifrequest) < 0) {
            (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFADDR:", ifp->int_name);
            p_error(err_message);
          } else {
            ifp->int_addr = ifrequest.ifr_addr;
          }
          if (ifp->int_flags & IFF_BROADCAST) {
#ifdef SIOCGIFBRDADDR
            (void) strcpy(ifrequest.ifr_name, ifp->int_name);
            if (ioctl(info_sock, SIOCGIFBRDADDR, (char *)&ifrequest) < 0) {
              (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFBRDADDR:", ifp->int_name);
              p_error(err_message);
            } else {
#ifdef	SUN3_3PLUS
              ifp->int_broadaddr = ifrequest.ifr_addr;
#else	SUN3_3PLUS
              ifp->int_broadaddr = ifrequest.ifr_broadaddr;
#endif	SUN3_3PLUS
            }
#else !SIOCGIFBRDADDR
            ifp->int_broadaddr = ifp->int_addr;
            sin = (struct sockaddr_in *)&ifp->int_addr;
            a = ntohl(sin->sin_addr.s_addr);
            sin = (struct sockaddr_in *)&ifp->int_broadaddr;
            if (IN_CLASSA(a))
              sin->sin_addr.s_addr = htonl(a & IN_CLASSA_NET);
            else if (IN_CLASSB(a))
              sin->sin_addr.s_addr = htonl(a & IN_CLASSB_NET);
            else
              sin->sin_addr.s_addr = htonl(a & IN_CLASSC_NET);
#endif SIOCGIFBRDADDR
          }
          (void) strcpy(ifrequest.ifr_name, ifp->int_name);
#ifdef	SIOCGIFNETMASK
          if (ioctl(info_sock, SIOCGIFNETMASK, (char *)&ifrequest) < 0) {
            (void) sprintf(err_message,"if_check: %s: ioctl SIOCGIFNETMASK:", ifp->int_name);
            p_error(err_message);
            ifp->int_subnetmask = (u_long) 0;
          } else {
            sin = (struct sockaddr_in *)&ifrequest.ifr_addr;
            ifp->int_subnetmask = ntohl(sin->sin_addr.s_addr);
          }
#else	SIOCGIFNETMASK
          sin = (struct sockaddr_in *)&ifp->int_addr;
          a = ntohl(sin->sin_addr.s_addr);
          if (IN_CLASSA(a)) {
            ifp->int_subnetmask = IN_CLASSA_NET;
          } else if (IN_CLASSB(a)) {
            ifp->int_subnetmask = IN_CLASSB_NET;
         } else {
            ifp->int_subnetmask = IN_CLASSC_NET;
         }
#endif	SIOCGIFNETMASK
          sin = (struct sockaddr_in *)&ifp->int_addr;
          a = ntohl(sin->sin_addr.s_addr);
          if (IN_CLASSA(a)) {
            ifp->int_netmask = IN_CLASSA_NET;
          } else if (IN_CLASSB(a)) {
            ifp->int_netmask = IN_CLASSB_NET;
          } else {
            ifp->int_netmask = IN_CLASSC_NET;
          }
          if (ifp->int_subnetmask == 0) {
            ifp->int_subnetmask = ifp->int_netmask;
          } else if (ifp->int_subnetmask != ifp->int_netmask) {
            ifp->int_flags |= IFF_SUBNET;
          }
          ifp->int_net = a & ifp->int_netmask;
          ifp->int_subnet = a & ifp->int_subnetmask;
          syslog(LOG_NOTICE, "if_check: %s, address %s up",
                             ifp->int_name, inet_ntoa(sock_inaddr(&ifp->int_addr)));
          TRACE_INT("if_check: %s, address %s up at %s",
                             ifp->int_name, inet_ntoa(sock_inaddr(&ifp->int_addr)), strtime);
          if_display("if_check", ifp);
          rt_ifup(ifp);
        } else {
          syslog(LOG_NOTICE, "if_check: %s, address %s down",
                             ifp->int_name, inet_ntoa(sock_inaddr(&ifp->int_addr)));
          TRACE_INT("if_check: %s, address %s down at %s",
                             ifp->int_name, inet_ntoa(sock_inaddr(&ifp->int_addr)), strtime);
          ifp->int_flags = IFF_INTERFACE |
                           (ifrequest.ifr_flags & IFF_MASK) |
                           (ifp->int_flags & IFF_KEEPMASK);
          rt_ifdown(ifp, FALSE);
        }
      }
    }
  }
  if (if_change) {
    register struct rt_entry *rt;
    register struct rthash *rh;

    for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++) 
      for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
        if ((rt->rt_state & RTS_INTERIOR) == 0)
          continue;
        if (rt->rt_ifp->int_flags & IFF_UP)
          rt->rt_flags |= RTF_UP;
        else
          rt->rt_flags &= ~RTF_UP;
      }
  }
  (void) close(info_sock);
}


/*
 *	if_display():
 *		Log the configuration of the interface
 */
if_display(name, ifp)
  char *name;
  struct interface *ifp;
{
  TRACE_INT("%s: interface %s: %s, addr %s, metric %d",
    name, ifp->int_name, (ifp->int_flags & IFF_UP) ? "up" : "down", inet_ntoa(sock_inaddr(&ifp->int_addr)), ifp->int_metric);
  if (ifp->int_flags & IFF_BROADCAST) {
    TRACE_INT(", broadaddr %s, ", inet_ntoa(sock_inaddr(&ifp->int_broadaddr)));
  }
  if (ifp->int_flags & IFF_POINTOPOINT) {
    TRACE_INT(", dstaddr %s, ", inet_ntoa(sock_inaddr(&ifp->int_dstaddr)));
  }
  TRACE_INT("\n%s: interface %s: ", name, ifp->int_name);
  TRACE_INT("net %s, ", gd_inet_ntoa(htonl(ifp->int_net)));
  TRACE_INT("netmask %s, ", gd_inet_ntoa(htonl(ifp->int_netmask)));
  TRACE_INT("\n%s: interface %s: ", name, ifp->int_name);
  TRACE_INT("subnet %s, ", gd_inet_ntoa(htonl(ifp->int_subnet)));
  TRACE_INT("subnetmask %s\n", gd_inet_ntoa(htonl(ifp->int_subnetmask)));
}
#endif	NSS

/*
 * Find the interface with address addr.
 */

struct interface *
if_ifwithaddr(withaddraddr)
	struct sockaddr *withaddraddr;
{
  register struct interface *ifp;
  struct sockaddr_in *addr = (struct sockaddr_in *) withaddraddr;
  struct sockaddr_in *intf_addr;

  for (ifp = ifnet; ifp; ifp = ifp->int_next) {
    if (ifp->int_flags & IFF_REMOTE) {
      continue;
    }
    if (ifp->int_addr.sa_family != withaddraddr->sa_family) {
      continue;
    }
    if (ifp->int_flags & IFF_POINTOPOINT) {
      intf_addr = (struct sockaddr_in *)&ifp->int_dstaddr;
      if (!bcmp((char *)&intf_addr->sin_addr, (char *)&addr->sin_addr, sizeof(struct in_addr))) {
        break;
      } else {
        continue;
      }
    }
    intf_addr = (struct sockaddr_in *)&ifp->int_addr;
    if (!bcmp((char *)&intf_addr->sin_addr, (char *)&addr->sin_addr, sizeof(struct in_addr))) {
      break;
    }
    intf_addr = (struct sockaddr_in *)&ifp->int_broadaddr;
    if (ifp->int_flags & IFF_BROADCAST) {
    }
    if ((ifp->int_flags & IFF_BROADCAST) &&
      !bcmp((char *)&intf_addr->sin_addr, (char *)&addr->sin_addr, sizeof(struct in_addr)))
      break;
  }
  return (ifp);
}

#ifndef	NSS
/*
 * update the active gw list on argument interface.
 */

if_updateactivegw(ifptr, actgw_addr, gw_proto)
	struct interface *ifptr;
	u_long actgw_addr;
	int gw_proto;
{
  struct active_gw *agp, *tmpactgw;
  int found_gw = 0;

  for (agp = ifptr->int_active_gw; agp; agp = agp->next) {
    if (actgw_addr == agp->addr) {
      found_gw++;
      break;
    }
    if (agp->next == NULL)
      break;
  }
  if (found_gw != 0) {
    agp->timer = 0;
    agp->proto |= gw_proto;
    return(agp->proto);
  }
  /*
   * this active gateway wasn't recorded yet!  Add it.
   * we have agp pointing to the last element, so no need to
   * traverse again.
   */
  tmpactgw = (struct active_gw *)malloc((unsigned)sizeof(struct active_gw));
  if (tmpactgw <= (struct active_gw *)0) {
    syslog(LOG_WARNING, "if_updateactivegw: out of memory");
    return(0);
  }
  tmpactgw->proto = gw_proto;
  tmpactgw->addr = actgw_addr;
  tmpactgw->timer = 0;
  if (agp == NULL) {		/* first one */
    ifptr->int_active_gw = tmpactgw;
    tmpactgw->back = ifptr->int_active_gw;
  }
  else {
    agp->next = tmpactgw;
    tmpactgw->back = agp;
  }
  tmpactgw->next = NULL;
  tmpactgw = NULL;		/* just to be safe */
  return (gw_proto);
}
#endif	NSS
