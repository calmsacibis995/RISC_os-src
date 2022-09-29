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
#ident	"$Header: mib.c,v 1.1.1.2 90/05/09 17:29:24 wje Exp $"

/***********************************************************
	Copyright 1988 by Carnegie Mellon University

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of CMU not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>

#define MIN(a, b)   ((a) < (b) ? (a) : (b))

typedef struct tree {
    char *text_name;		/* Text name of this tree */
    int numeric_name;		/* Numeric name of this tree */
    struct tree *subtree[64];	/* Subtrees */
} tree;

tree ifIndex = { "ifIndex", 1, { NULL } };
tree ifDescr = { "ifDescr", 2, { NULL } };
tree ifType = { "ifType", 3, { NULL } };
tree ifMtu = { "ifMtu", 4, { NULL } };
tree ifSpeed = { "ifSpeed", 5, { NULL } };
tree ifPhysAddress = { "ifPhysAddress", 6, { NULL }  };
tree ifAdminStatus = {
    "ifAdminStatus", 7, { NULL } };
tree ifOperStatus = { "ifOperStatus",  8, { NULL } };
tree ifLastChange = { "ifLastChange", 9, { NULL } };
tree ifInOctets = { "ifInOctets", 10, { NULL } };
tree ifInUcastPkts = { "ifInUcastPkts", 11, { NULL } };
tree ifInNUcastPkts = { "ifInNUcastPkts", 12, { NULL } };
tree ifInDiscards = { "ifInDiscards", 13, { NULL } };
tree ifInErrors = { "ifInErrors", 14, { NULL } };
tree ifInUnknownProtos = { "ifInUnknownProtos", 15, { NULL } };
tree ifOutOctets = { "ifOutOctets", 16, { NULL } };
tree ifOutUcastPkts = { "ifOutUcastPkts", 17, { NULL } };
tree ifOutNUcastPkts = { "ifOutNUcastPkts", 18, { NULL } };
tree ifOutDiscards = { "ifOutDiscards", 19, { NULL } };
tree ifOutErrors = { "ifOutErrors", 20, { NULL } };
tree ifOutQLen = { "ifOutQLen", 21, { NULL } };
tree atIfIndex = { "atIfIndex", 1, { NULL } };
tree atPhysAddress = { "atPhysAddress", 2, { NULL } };
tree atNetAddress = { "atNetAddress", 3, { NULL } };
tree ipAdEntAddr = { "ipAdEntAddr", 1, { NULL } };
tree ipAdEntIfIndex = { "ipAdEntIfIndex", 2, { NULL } };
tree ipAdEntNetMask = { "ipAdEntNetMask", 3, { NULL } };
tree ipAdEntBcastAddr = { "ipAdEntBcastAddr", 4, { NULL } };
tree ipRouteDest = { "ipRouteDest", 1, { NULL } };
tree ipRouteIfIndex = { "ipRouteIfIndex", 2, { NULL } };
tree ipRouteMetric1 = { "ipRouteMetric1", 3, { NULL } };
tree ipRouteMetric2 = { "ipRouteMetric2", 4, { NULL } };
tree ipRouteMetric3 = { "ipRouteMetric3", 5, { NULL } };
tree ipRouteMetric4 = { "ipRouteMetric4", 6, { NULL } };
tree ipRouteNextHop = { "ipRouteNextHop", 7, { NULL } };
tree ipRouteType = { "ipRouteType", 8, { NULL } };
tree ipRouteProto = { "ipRouteProto", 9, { NULL } };
tree ipRouteAge = { "ipRouteAge", 10, { NULL } };
tree tcpConnState = { "tcpConnState", 1, { NULL } };
tree tcpConnLocalAddress = { "tcpConnLocalAddress", 2, { NULL } };
tree tcpConnLocalPort = { "tcpConnLocalPort", 3, { NULL } };
tree tcpConnRemAddress = { "tcpConnRemAddress", 4, { NULL } };
tree tcpConnRemPort = { "tcpConnRemPort", 5, { NULL } };
tree egpNeighState = { "egpNeighState", 1, { NULL } };
tree egpNeighAddr = { "egpNeighAddr", 2, { NULL } };

tree ifEntry = {
    "ifEntry", 1, {
	&ifIndex, &ifDescr, &ifType, &ifMtu, &ifSpeed, &ifPhysAddress,
	&ifAdminStatus, &ifOperStatus, &ifLastChange, &ifInOctets,
	&ifInUcastPkts, &ifInNUcastPkts, &ifInDiscards, &ifInErrors,
	&ifInUnknownProtos, &ifOutOctets, &ifOutUcastPkts, &ifOutNUcastPkts,
	&ifOutDiscards, &ifOutErrors, &ifOutQLen, NULL } };
tree atEntry = { "atEntry", 1, {
    &atIfIndex, &atPhysAddress, &atNetAddress, NULL } };
tree ipAddrEntry = { "ipAddrEntry", 1, {
    &ipAdEntAddr, &ipAdEntIfIndex, &ipAdEntNetMask, &ipAdEntBcastAddr,
    NULL } };
tree ipRouteEntry = { "ipRouteEntry", 1, {
    &ipRouteDest, &ipRouteIfIndex, &ipRouteMetric1, &ipRouteMetric2,
    &ipRouteMetric3, &ipRouteMetric4, &ipRouteNextHop, &ipRouteType,
    &ipRouteProto, &ipRouteAge, NULL } };
tree tcpConnEntry = { "tcpConnEntry", 1, {
    &tcpConnState, &tcpConnLocalAddress, &tcpConnLocalPort,
    &tcpConnRemAddress, &tcpConnRemPort, NULL } };
tree egpNeighEntry = {
    "egpNeighEntry", 1, { &egpNeighState, &egpNeighAddr, NULL } };


tree sysDescr = { "sysDescr", 1, { NULL } };
tree sysObjectID = { "sysObjectID", 2, { NULL } };
tree sysUpTime = { "sysUpTime", 3, { NULL } };
tree ifNumber = { "ifNumber", 1, { NULL } };
tree ifTable = { "ifTable", 2, { &ifEntry, NULL } };
tree atTable = { "atTable", 1, { &atEntry, NULL } };
tree ipForwarding = { "ipForwarding", 1, { NULL } };
tree ipDefaultTTL = { "ipDefaultTTL", 2, { NULL } };
tree ipInReceives = { "ipInReceives", 3, { NULL } };
tree ipInHdrErrors = { "ipInHdrErrors", 4, { NULL } };
tree ipInAddrErrors = { "ipInAddrErrors", 5, { NULL } };
tree ipForwDatagrams = { "ipForwDatagrams", 6, { NULL } };
tree ipInUnknownProtos = { "ipInUnknownProtos", 7, { NULL } };
tree ipInDiscards = { "ipInDiscards", 8, { NULL } };
tree ipInDelivers = { "ipInDelivers", 9, { NULL } };
tree ipOutRequests = { "ipOutRequests", 10, { NULL } };
tree ipOutDiscards = { "ipOutDiscards", 11, { NULL } };
tree ipOutNoRoutes = { "ipOutNoRoutes", 12, { NULL } };
tree ipReasmTimeout = { "ipReasmTimeout", 13, { NULL } };
tree ipReasmReqds = { "ipReasmReqds", 14, { NULL } };
tree ipReasmOKs = { "ipReasmOKs", 15, { NULL } };
tree ipReasmFails = { "ipReasmFails", 16, { NULL } };
tree ipFragOKs = { "ipFragOKs", 17, { NULL } };
tree ipFragFails = { "ipFragFails", 18, { NULL } };
tree ipFragCreates = { "ipFragCreates", 19, { NULL } };
tree ipAddrTable = { "ipAddrTable", 20, { &ipAddrEntry, NULL } };
tree ipRoutingTable = { "ipRoutingTable", 21, { &ipRouteEntry, NULL } };
tree icmpInMsgs = { "icmpInMsgs", 1, { NULL } };
tree icmpInErrors = { "icmpInErrors", 2, { NULL } };
tree icmpInDestUnreachs = { "icmpInDestUnreachs", 3, { NULL } };
tree icmpInTimeExcds = { "icmpInTimeExcds", 4, { NULL } };
tree icmpInParmProbs = { "icmpInParmProbs", 5, { NULL } };
tree icmpInSrcQuenchs = { "icmpInSrcQuenchs", 6, { NULL } };
tree icmpInRedirects = { "icmpInRedirects", 7, { NULL} };
tree icmpInEchos = { "icmpInEchos", 8, { NULL} };
tree icmpInEchoReps = { "icmpInEchoReps", 9, { NULL} };
tree icmpInTimestamps = { "icmpInTimestamps", 10, { NULL} };
tree icmpInTimestampReps = { "icmpInTimestampReps", 11, { NULL} };
tree icmpInAddrMasks = { "icmpInAddrMasks", 12, { NULL} };
tree icmpInAddrMaskReps = { "icmpInAddrMaskReps", 13, { NULL} };
tree icmpOutMsgs = { "icmpOutMsgs", 14, { NULL} };
tree icmpOutErrors = { "icmpOutErrors", 15, { NULL} };
tree icmpOutDestUnreachs = { "icmpOutDestUnreachs", 16, { NULL} };
tree icmpOutTimeExcds = { "icmpOutTimeExcds", 17, { NULL} };
tree icmpOutParmProbs = { "icmpOutParmProbs", 18, { NULL} };
tree icmpOutSrcQuenchs = { "icmpOutSrcQuenchs", 19, { NULL} };
tree icmpOutRedirects = { "icmpOutRedirects", 20, { NULL} };
tree icmpOutEchos = { "icmpOutEchos", 21, { NULL} };
tree icmpOutEchoReps = { "icmpOutEchoReps", 22, { NULL} };
tree icmpOutTimestamps = { "icmpOutTimestamps", 23, { NULL} };
tree icmpOutTimestampReps = { "icmpOutTimestampReps", 24, { NULL} };
tree icmpOutAddrMasks = { "icmpOutAddrMasks", 25, { NULL} };
tree icmpOutAddrMaskReps = { "icmpOutAddrMaskReps", 26, { NULL} };
tree tcpRtoAlgorithm = {"tcpRtoAlgorithm", 1, { NULL }};
tree tcpRtoMin = { "tcpRtoMin", 2, { NULL } };
tree tcpRtoMax = { "tcpRtoMax", 3, { NULL } };
tree tcpMaxConn = { "tcpMaxConn", 4, { NULL } };
tree tcpActiveOpens = { "tcpActiveOpens", 5, { NULL } };
tree tcpPassiveOpens = { "tcpPassiveOpens", 6, { NULL } };
tree tcpAttemptFails = { "tcpAttemptFails", 7, { NULL } };
tree tcpEstabResets = { "tcpEstabResets", 8, { NULL } };
tree tcpCurrEstab = { "tcpCurrEstab", 9, { NULL } };
tree tcpInSegs = { "tcpInSegs", 10, { NULL } };
tree tcpOutSegs = { "tcpOutSegs", 11, { NULL } };
tree tcpRetransSegs = { "tcpRetransSegs", 12, { NULL } };
tree tcpConnTable = { "tcpConnTable", 13, { &tcpConnEntry, NULL } };
tree udpInDatagrams = { "udpInDatagrams", 1, { NULL } };
tree udpNoPorts = { "udpNoPorts", 2, { NULL } };
tree udpInErrors = { "udpInErrors", 3, { NULL } };
tree udpOutDatagrams = { "udpOutDatagrams", 4, { NULL } };
tree egpInMsgs = { "egpInMsgs", 1, { NULL } };
tree egpInErrors = { "egpInErrors", 2, { NULL } };
tree egpOutMsgs = { "egpOutMsgs", 3, { NULL } };
tree egpOutErrors = { "egpOutErrors", 4, { NULL } };
tree egpNeighTable = { "egpNeighTable", 5, { &egpNeighEntry, NULL } };

tree system = { "system", 1, { &sysDescr, &sysObjectID, &sysUpTime, NULL } };
tree interfaces = { "interfaces", 2, { &ifNumber, &ifTable, NULL } };
tree at = { "at", 3, { &atTable, NULL } };
tree ip = { "ip", 4, {
    &ipForwarding, &ipDefaultTTL, &ipInReceives, &ipInHdrErrors,
    &ipInAddrErrors, &ipForwDatagrams, &ipInUnknownProtos, &ipInDiscards,
    &ipInDelivers, &ipOutRequests, &ipOutDiscards, &ipOutNoRoutes,
    &ipReasmTimeout, &ipReasmReqds, &ipReasmOKs, &ipReasmFails, &ipFragOKs,
    &ipFragFails, &ipFragCreates, &ipAddrTable, &ipRoutingTable, NULL } };
tree icmp = { "icmp", 5, {
    &icmpInMsgs, &icmpInErrors, &icmpInDestUnreachs, &icmpInTimeExcds,
    &icmpInParmProbs, &icmpInSrcQuenchs, &icmpInRedirects, &icmpInEchos,
    &icmpInEchoReps, &icmpInTimestamps, &icmpInTimestampReps,
    &icmpInAddrMasks, &icmpInAddrMaskReps, &icmpOutMsgs, &icmpOutErrors,
    &icmpOutDestUnreachs, &icmpOutTimeExcds, &icmpOutParmProbs,
    &icmpOutSrcQuenchs, &icmpOutRedirects, &icmpOutEchos, &icmpOutEchoReps,
    &icmpOutTimestamps, &icmpOutTimestampReps, &icmpOutAddrMasks,
    &icmpOutAddrMaskReps, NULL } };
tree tcp = { "tcp", 6, {
    &tcpRtoAlgorithm, &tcpRtoMin, &tcpRtoMax, &tcpMaxConn, &tcpActiveOpens,
    &tcpPassiveOpens, &tcpAttemptFails, &tcpEstabResets, &tcpCurrEstab,
    &tcpInSegs, &tcpOutSegs, &tcpRetransSegs, &tcpConnTable, NULL } };
tree udp = { "udp", 7, {
    &udpInDatagrams, &udpNoPorts, &udpInErrors, &udpOutDatagrams, NULL } };
tree egp = { "egp", 8, {
    &egpInMsgs, &egpInErrors, &egpOutMsgs, &egpOutErrors, &egpNeighTable,
    NULL } };
tree CMU_KIP = { "CMU-KIP", 1, { NULL } };
tree CMU_Router = { "C-Router", 2, { NULL } };
tree sysId = { "sysID", 1, { &CMU_KIP, &CMU_Router, NULL } };
tree CMU_mib = { "CMU-mib", 2, { NULL } };
tree CMU = { "CMU", 3, { &sysId, &CMU_mib, NULL } };

tree mib = { "mib", 1, {
    &system, &interfaces, &at, &ip, &icmp, &tcp, &udp, &egp, NULL } };
tree enterprises = { "enterprises", 1, { &CMU, NULL } };

tree directory = { "directory", 1, { NULL } };
tree mgmt = { "mgmt", 2, { &mib, NULL } };
tree experimental = { "experimental", 3, { NULL } };
tree private = { "private", 4, { &enterprises, NULL } };

tree internet = { "internet", 1, {
    &directory, &mgmt, &experimental, &private, NULL} };

tree dod = { "dod", 6, { &internet, NULL } };

tree org = { "org", 3, { &dod, NULL } };

tree iso = { "iso", 1, { &org, NULL } };
tree ccitt = { "ccitt", 2, { NULL } };
tree joint_iso_ccitt = { "joint-iso-ccitt", 3, { NULL } };

tree *root[] = { &iso, &ccitt, &joint_iso_ccitt, NULL };

tree *rfc1066_mib[] = {
    &system, &interfaces, &at, &ip, &icmp, &tcp, &udp, &egp, NULL };

char *cwd_label = "RFC1066-MIB";
unsigned char RFC1066_MIB[] = { 1, 3, 6, 1, 2, 1 };
unsigned char RFC1066_MIB_text[] = ".iso.org.dod.internet.mgmt.mib";
unsigned char *cwd_value = RFC1066_MIB;

static int
read_objid(input, output, out_len)
    char *input, *output;
    int	*out_len;
{
    tree **subtree_p = root;
    char *op = output;
    int i;

    if (*input == '.')
	input++;
    else {
	subtree_p = rfc1066_mib;
	for (i = 0; i < sizeof (RFC1066_MIB); i++) {
	    if ((*out_len)-- > 0)
		*output++ = RFC1066_MIB[i];
	    else {
		printf("object identifier too long\n");
		return (0);
	    }
	}
    }

    if ((*out_len =
	 parse_subtree(subtree_p, input, output, out_len)) == 0)
	return (0);
    *out_len += output - op;

    return (1);
}

static
parse_subtree(subtree_p, input, output, out_len)
    tree **subtree_p;
    char *input, *output;
    int	*out_len;
{
    char buf[64], *to = buf;
    int subid = 0;

    /*
     * No empty strings.  Can happen if there is a trailing '.' or two '.'s
     * in a row, i.e. "..".
     */
    if ((*input == '\0') ||
	(*input == '.'))
	return (0);

    if (isdigit(*input)) {
	/*
	 * Read the number, then try to find it in the subtree.
	 */
	while (isdigit(*input)) {
	    subid *= 10;
	    subid += *input++ - '0';
	}
	for (; subtree_p && *subtree_p; subtree_p++) {
	    if ((*subtree_p)->numeric_name == subid) {
		subtree_p = (*subtree_p)->subtree;
		goto found;
	    }
	}

	/*
	 * If we didn't find the entry, remember that...
	 */
	if (subtree_p && *subtree_p == NULL)
	    subtree_p = NULL;
    }
    else {
	/*
	 * Read the name into a buffer.
	 */
	while ((*input != '\0') &&
	       (*input != '.')) {
	    *to++ = *input++;
	}
	*to = '\0';

	/*
	 * Find the name in the subtree;
	 */
	for (; subtree_p && *subtree_p; subtree_p++) {
	    if (lc_cmp((*subtree_p)->text_name, buf) == 0) {
		subid = (*subtree_p)->numeric_name;
		subtree_p = (*subtree_p)->subtree;
		goto found;
	    }
	}

	/*
	 * If we didn't find the entry, punt...
	 */
	if (subtree_p == NULL || *subtree_p == NULL) {
	    printf("sub-identifier not found: %s\n", buf);
	    return (0);
	}
    }

found:
    if(subid > 0xFF){
	printf("sub-identifier too large: %s\n", buf);
	return (0);
    }

    if ((*out_len)-- <= 0){
	printf("object identifier too long\n");
	return (0);
    }
    *output++ = subid;

    if (*input != '.')
	return (1);
    if ((*out_len =
	 parse_subtree(subtree_p, ++input, output, out_len)) == 0)
	return (0);
    return (++*out_len);
}

static tree **
get_symbol(objid, objidlen, subtree_p, buf)
    u_char  *objid;
    int	    objidlen;
    tree    **subtree_p;
    char    *buf;
{
    tree    **return_tree = NULL;

    for(; subtree_p && *subtree_p; subtree_p++){
	if (*objid == (*subtree_p)->numeric_name){
	    strcpy(buf, (*subtree_p)->text_name);
	    goto found;
	}
    }

    /* subtree not found */
    while(objidlen--){
	sprintf(buf, "%d.", *objid++);
	while(*buf)
	    buf++;
    }
    *(buf - 1) = '\0'; /* remove trailing dot */
    return NULL;

found:
    if (objidlen > 1){
	while(*buf)
	    buf++;
	*buf++ = '.';
	*buf = '\0';
	return_tree = get_symbol(objid + 1, objidlen - 1, (*subtree_p)->subtree, buf);
    } 
    if (return_tree != NULL)
	return return_tree;
    else
	return subtree_p;
}

static int
lc_cmp(s1, s2)
    char *s1, *s2;
{
    char c1, c2;

    while(*s1 && *s2){
	if (isupper(*s1))
	    c1 = tolower(*s1);
	else
	    c1 = *s1;
	if (isupper(*s2))
	    c2 = tolower(*s2);
	else
	    c2 = *s2;
	if (c1 != c2)
	    return ((c1 - c2) > 0 ? 1 : -1);
	s1++;
	s2++;
    }

    if (*s1)
	return -1;
    if (*s2)
	return 1;
    return 0;
}

int
objid_to_string (objid, objidlen, buf, buflen)
u_char	*objid;
int	objidlen;
u_char	*buf;
int	buflen;
{
	tree	**subtree_p = root;
	
	*buf = '.';
	get_symbol (objid, objidlen, subtree_p, buf + 1);
}

int
string_to_objid (buf, objid, objidlen)
u_char	*buf;
u_char	*objid;
int	*objidlen;
{
	tree	**subtree_p = root;

	(void) read_objid (buf, objid, objidlen);
}
