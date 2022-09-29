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
#ident	"$Header: oid.c,v 1.4.1.2 90/05/09 17:31:24 wje Exp $"

/*
 *	$Header: oid.c,v 1.4.1.2 90/05/09 17:31:24 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include		<stdio.h>
#include		<ctype.h>

#include		<ctypes.h>
#include		<debug.h>
#include		<rdx.h>
#include		<oid.h>

asn_enum ifType_enum[] = {
    "other", 1, "regular1822", 2, "hdh1822", 3, "ddn-x25", 4,
    "rfc877-x25", 5, "ethernet-csmacd", 6, "iso88023-csmacd", 7,
    "iso88024-tokenBus", 8, "iso88025-tokenRing", 9, "iso88026-man", 10,
    "starLan", 11, "proteon-10MBit", 12, "proteon-80MBit", 13,
    "hyperchannel", 14, "fddi", 15, "lapb", 16, "sdlc", 17, "t1-carrier", 18,
    "cept", 19, "basicIsdn", 20, "primaryIsdn", 21,
    "propPointToPointSerial", 22, NULL };
asn_enum ifAdminStatus_enum[] = { "up", 1, "down", 2, "testing", 3, NULL };
asn_enum ifOperStatus_enum[] = { "up", 1, "down", 2, "testing", 3, NULL };
asn_enum ipForwarding_enum[] = { "gateway", 1, "host", 2, NULL };
asn_enum ipRouteType_enum[] = {
    "other", 1, "invalid", 2, "direct", 3, "remote", 4, NULL };
asn_enum ipRouteProto_enum[] = {
    "other", 1, "local", 2, "netmgmt", 3, "icmp", 4, "egp", 5, "ggp", 6,
    "hello", 7, "rip", 8, "is-is", 9, "es-is", 10, "ciscoIgrp", 11,
    "bbnSpfIgp", 12, "oigp", 13, NULL };
asn_enum tcpRtoAlgorithm_enum[] = {
    "other", 1, "constant", 2, "rsre", 3, "vanj", 4, NULL };
asn_enum tcpConnState_enum[] = {
    "closed", 1, "listen", 2, "synSent", 3, "synReceive", 4, "established", 5,
    "finWait1", 6, "finWait2", 7, "closeWait", 8, "lastAck", 9, "closing", 10,
    "timeWait", 11, NULL };
asn_enum egpNeighState_enum[] = {
    "idle[]", 1, "acquisition", 2, "down", 3, "up", 4, "cease", 5, NULL };

tree ifIndex = { "ifIndex", 1, { NULL } };
tree ifDescr = { "ifDescr", 2, { NULL } };
tree ifType = { "ifType", 3, { NULL }, ifType_enum };
tree ifMtu = { "ifMtu", 4, { NULL } };
tree ifSpeed = { "ifSpeed", 5, { NULL } };
tree ifPhysAddress = { "ifPhysAddress", 6, { NULL }  };
tree ifAdminStatus = { "ifAdminStatus", 7, { NULL }, ifAdminStatus_enum };
tree ifOperStatus = { "ifOperStatus",  8, { NULL }, ifOperStatus_enum };
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
tree ipRouteType = { "ipRouteType", 8, { NULL }, ipRouteType_enum };
tree ipRouteProto = { "ipRouteProto", 9, { NULL }, ipRouteProto_enum };
tree ipRouteAge = { "ipRouteAge", 10, { NULL } };
tree tcpConnState = { "tcpConnState", 1, { NULL }, tcpConnState_enum };
tree tcpConnLocalAddress = { "tcpConnLocalAddress", 2, { NULL } };
tree tcpConnLocalPort = { "tcpConnLocalPort", 3, { NULL } };
tree tcpConnRemAddress = { "tcpConnRemAddress", 4, { NULL } };
tree tcpConnRemPort = { "tcpConnRemPort", 5, { NULL } };
tree egpNeighState = { "egpNeighState", 1, { NULL }, egpNeighState_enum };
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
tree ipForwarding = { "ipForwarding", 1, { NULL }, ipForwarding_enum };
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
tree tcpRtoAlgorithm = {"tcpRtoAlgorithm", 1, { NULL }, tcpRtoAlgorithm_enum};
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
tree RESERVED = { "Reserved", 0, { NULL } };
tree PROTEON = { "Proteon", 1, { NULL } };
tree IBM = { "IBM", 2, { NULL } };
tree CMU_KIP = { "KIP", 1, { NULL } };
tree CMU_Router = { "C-Router", 2, { NULL } };
tree CMU_sysid = { "SystemID", 1, { &CMU_KIP, &CMU_Router, NULL } };
tree CMU_mib = { "mib", 2, { NULL } };
tree CMU = { "CMU", 3, { &CMU_sysid, &CMU_mib, NULL } };
tree UNIX = { "Unix", 4, { NULL } };
tree ACC = { "ACC", 5, { NULL } };
tree TWG = { "TWG", 6, { NULL } };
tree CAYMAN = { "CAYMAN", 7, { NULL } };
tree NYSERNET = { "NYSERNET", 8, { NULL } };
tree CISCO = { "cisco", 9, { NULL } };
tree NSC = { "NSC", 10, { NULL } };
tree HP = { "HP", 11, { NULL } };
tree EPILOGUE = { "Epilogue", 12, { NULL } };
tree U_OF_TENNESSEE = { "U_of_Tennessee", 13, { NULL } };
tree BBN = { "BBN", 14, { NULL } };
tree ENCORE = { "Encore", 15, { NULL } };
tree UNISYS = { "Unisys", 16, { NULL } };
tree CANSTAR = { "Canstar", 17, { NULL } };
tree WELLFLEET = { "Wellfleet", 18, { NULL } };
tree TRW = { "TRW", 19, { NULL } };
tree LCS = { "LCS", 1, { NULL } };
tree MIT = { "MIT", 20, { NULL } };
tree EON = { "EON", 21, { NULL } };
tree SPARTACUS = { "Spartacus", 22, { NULL } };
tree EXCELAN = { "Excelan", 23, { NULL } };
tree SPIDER = { "Spider", 24, { NULL } };
tree NSFNET = { "NSFNET", 25, { NULL } };
tree SYTEK = { "Sytek", 26, { NULL } };
tree INTERGRAPH = { "Intergraph", 27, { NULL } };
tree INTERLAN = { "Interlan", 27, { NULL } };
tree VITALINK = { "Vitalink", 29, { NULL } };
tree ULANA = { "Ulana", 30, { NULL } };
tree NSWC = { "NSWC", 31, { NULL } };
tree SANTA_CRUZ_OPERATION = { "Santa_Cruz_Operation", 32, { NULL } };
tree XYPLEX = { "Xyplex", 33, { NULL } };
tree CRAY = { "Cray", 34, { NULL } };
tree BELL_NORTHERN_RESEARCH = { "Bell_Northern_Research", 35, { NULL } };
tree DEC = { "DEC", 36, { NULL } };
tree TOUCH = { "Touch", 37, { NULL } };
tree NETWORK_RESEARCH_CORP = { "Network_Research_Corp", 38, { NULL } };
tree BAYLOR_COLLEGE_OF_MEDICINE = { "Baylor_College_of_Medicine", 39, { NULL } };
tree NMFECC_LLNL = { "NMFECC-LLNL", 40, { NULL } };
tree SRI = { "SRI", 41, { NULL } };
tree SUN = { "Sun", 42, { NULL } };
tree THREE_COM = { "3Com", 43, { NULL } };
tree CMC = { "CMC", 44, { NULL } };
tree SYNOPTICS = { "SynOptics", 45, { NULL } };
tree CHEYENNE_SOFTWARE = { "Cheyenne_Software", 46, { NULL } };
tree PRIME = { "Prime", 47, { NULL } };
tree MCNC_NORTH_CAROLINA_DATA_NETWORK = { "MCNC/North_Carolina_Data_Network", 48, { NULL } };
tree CHIPCOM = { "Chipcom", 49, { NULL } };
tree OPTICAL_DATA_SYSTEMS = { "Optical_Data_Systems", 50, { NULL } };
tree GATED = { "gated", 51, { NULL } };
tree CABLETRON_SYSTEMS = { "Cabletron_Systems", 52, { NULL } };
tree APOLLO = { "Apollo", 53, { NULL } };
tree DESKTALK_SYSTEMS = { "DeskTalk_Systems", 54, { NULL } };
tree SSDS = { "SSDS", 55, { NULL } };
tree CASTLE_ROCK_COMPUTING = { "Castle_Rock_Computing", 56, { NULL } };
tree MIPS_UNKNOWN = {"unknown", 0, { NULL } };
tree MIPS_M500 = { "m500", 1, { NULL } };
tree MIPS_M800 = { "m800", 2, { NULL } };
tree MIPS_M1000 = { "m1000", 3, { NULL } };
tree MIPS_M120_5 = { "m120-5", 4, { NULL } };
tree MIPS_M120_3 = { "m120-3", 5, { NULL } };
tree MIPS_M120 = { "m120", 6, { NULL } };
tree MIPS_M2000_8 = { "m2000-8", 7, { NULL } };
tree MIPS_M2000_6 = { "m2000-6", 8, { NULL } };
tree MIPS_M2000 = { "m2000", 9, { NULL } };
tree MIPS_I2000 = { "rc2030", 10, { NULL } };
tree MIPS_EXCALIBUR = { "Excalibur", 11, { NULL } };
tree MIPS_M180 = { "rc3240", 12, { NULL } };
tree MIPS_SYSID = { "SystemID", 1, { &MIPS_UNKNOWN, &MIPS_M500, &MIPS_M800,
    &MIPS_M1000, &MIPS_M120_5, &MIPS_M120_3, &MIPS_M120, &MIPS_M2000_8,
    &MIPS_M2000_6, &MIPS_M2000, &MIPS_I2000, &MIPS_EXCALIBUR, NULL } };
tree MIPS_MIB = { "mib", 2, { NULL } };
tree MIPS = { "MIPS", 57, { &MIPS_SYSID, &MIPS_MIB } };
tree mib = { "mib", 1, {
    &system, &interfaces, &at, &ip, &icmp, &tcp, &udp, &egp, NULL } };
tree enterprises = { "enterprises", 1, { &RESERVED, &PROTEON, &IBM, &UNIX,
    &CMU,  &ACC, &TWG, &CAYMAN, &NYSERNET, &CISCO, &NSC, &HP, &EPILOGUE,
    &U_OF_TENNESSEE, &BBN, &ENCORE, &UNISYS, &CANSTAR, &WELLFLEET, &TRW,
    &EON, &MIT,  &SPARTACUS, &EXCELAN, &SPIDER, &NSFNET, &SYTEK, &INTERGRAPH,
    &INTERLAN, &VITALINK, &ULANA, &NSWC, &SANTA_CRUZ_OPERATION, &XYPLEX,
    &CRAY, &BELL_NORTHERN_RESEARCH, &DEC, &TOUCH, &NETWORK_RESEARCH_CORP,
    &BAYLOR_COLLEGE_OF_MEDICINE, &NMFECC_LLNL, &SRI, &SUN, &THREE_COM, &CMC,
    &SYNOPTICS, &CHEYENNE_SOFTWARE, &PRIME, &MCNC_NORTH_CAROLINA_DATA_NETWORK,
    &CHIPCOM, &OPTICAL_DATA_SYSTEMS, &GATED, &CABLETRON_SYSTEMS, &APOLLO,
    &DESKTALK_SYSTEMS, &SSDS, &CASTLE_ROCK_COMPUTING, &MIPS, NULL } };

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

CIntfType		oidDecode (result, m, oid, n, enumsp)

CCharPtrType		result;
CIntfType		m;
CBytePtrType		oid;
CIntfType		n;
asn_enum **		enumsp;

{
	CUnslType		val;
	CUnslType		quo;
	CByteType		c;
	CIntfType		k;
	CIntfType		s;

	tree **			subtree_p = root;
	int			found;

	if (enumsp)
		*enumsp = (asn_enum *) 0;
	s = m;
	val = (CUnslType) 0;

	do {
		c = *oid++;
		val = (val << 7) | (CUnslType) (c & (CByteType) 0x7F);
		n--;

	} while (((c & (CByteType) 0x80) != (CByteType) 0) &&
		(n != (CIntfType) 0));

	quo = val / (CUnslType) 40;
	found = 0;
	if (s <= (CIntfType) 0) {
		return ((CIntfType) -1);
	}
	for (; subtree_p && *subtree_p; subtree_p++) {
		if ((*subtree_p)->numeric_name == quo) {
			k = strlen ((*subtree_p)->text_name);
			if (k > s) {
				return ((CIntfType) -1);
			}
			if (enumsp)
				*enumsp = (*subtree_p)->enums;
			strcpy (result, (*subtree_p)->text_name);
			subtree_p = (*subtree_p)->subtree;
			found = 1;
			break;
		}
	}

	if (found == 0) {
		k = rdxEncode10 (result, s, quo);
		if (k < (CIntfType) 0) {
			return (k);
		}
	}

	result += k;
	s -= k;
	*result++ = (CCharType) '.';
	s--;

	found = 0;
	val -= ((CUnslType) 40 * quo);
	if (s <= (CIntfType) 0) {
		return ((CIntfType) -1);
	}
	for (; subtree_p && *subtree_p; subtree_p++) {
		if ((*subtree_p)->numeric_name == val) {
			k = strlen ((*subtree_p)->text_name);
			if (k > s) {
				return ((CIntfType) -1);
			}
			if (enumsp)
				*enumsp = (*subtree_p)->enums;
			strcpy (result, (*subtree_p)->text_name);
			subtree_p = (*subtree_p)->subtree;
			found = 1;
			break;
		}
	}

	if (found == 0) {
		k = rdxEncode10 (result, s, val);
	}

	result += k;
	s -= k;

	while ((n != (CIntfType) 0) && (k >= (CIntfType) 0)) {
		val = (CUnslType) 0;
		do {
			c = *oid++;
			val = (val << 7) | (CUnslType) (c & (CByteType) 0x7F);
			n--;

		} while (((c & (CByteType) 0x80) != (CByteType) 0) &&
			(n != (CIntfType) 0));

		*result++ = (CCharType) '.';
		s--;
		found = 0;
		for (; subtree_p && *subtree_p; subtree_p++) {
			if ((*subtree_p)->numeric_name == val) {
				k = strlen ((*subtree_p)->text_name);
				if (k > s) {
					return ((CIntfType) -1);
				}
				if (enumsp)
					*enumsp = (*subtree_p)->enums;
				strcpy (result, (*subtree_p)->text_name);
				subtree_p = (*subtree_p)->subtree;
				found = 1;
				break;
			}
		}

		if (found == 0) {
			k = rdxEncode10 (result, s, val);
		}

		result += k;
		s -= k;
	}

	*result = (CCharType) 0;
	return ((k < (CIntfType) 0) ? k : m - s);
}


static	CIntfType	oidEncodeSubid (oid, n, val)

CBytePtrType		oid;
CIntfType		n;
CUnslType		val;

{
	CIntfType		k;
	CByteType		buf [ (2 * sizeof (val)) ];
	CBytePtrType		bp;
	CByteType		mask;

	k = (CIntfType) 0;
	mask = (CByteType) 0;
	bp = buf + sizeof (val) + sizeof (val);
	do {
		bp--;
		*bp = (CByteType) (val & (CUnslType) 0x7F) | mask;
		mask = (CByteType) 0x80;
		val >>= 7;
		k++;

	} while (val != (CUnslType) 0);

	if (k < n) {
		n = k;
		while (k-- != 0) {
			*oid++ = *bp++;
		}
	}

	return (n);
}

CIntfType		oidEncode (oid, n, text)

CBytePtrType		oid;
CIntfType		n;
CCharPtrType		text;

{
	CUnslType		val;
	CUnslType		val1;
	CIntfType		state;
	CIntfType		h;
	CIntfType		k;
	CCharType		c;
	CBoolType		done;
	int			found;
	tree **			subtree_p = root;
	int			i;
	char buf[64], *to;

	/*	Handle zero-length OID here	*/
	if ((*text == (CCharType) 0) && (n > (CIntfType) 0)) {
		return ((CIntfType) 0);
	}

	state = (CIntfType) 0;
	h = (CIntfType) 0;
	val = (CUnslType) 0;
	done = FALSE;


	if (*text == '.')
		text++;
	else {
		subtree_p = rfc1066_mib;
		for (i = 0; i < sizeof (RFC1066_MIB); i++) {
			val = RFC1066_MIB[i];
			switch (state) {

			case 0:
				val1 = ((CUnslType) 40 * val);
				val = (CUnslType) 0;
				state = (CIntfType) 1;
				break;

			case 1:
				if (val > (CUnslType) 39) {
					n = (CIntfType) 0;
					done = TRUE;
					break;
				}
				else {
					val += val1;
					/*	fall through	*/
				}

			case 2:
				k = oidEncodeSubid (oid, n, val);
				h += k;
				n -= k;
				oid += k;
				val = (CUnslType) 0;
				state = (CIntfType) 2;
				break;
			}
		}
	}

	while ((! done) && (n > (CIntfType) 0)) {
		if (isdigit (*text)) {
			while (isdigit (*text)) {
				val *= 10;
				val += *text++ - '0';
			}
		}
		else if ((c = *text++) == (CCharType) '.') {
			switch (state) {

			case 0:
				val1 = ((CUnslType) 40 * val);
				val = (CUnslType) 0;
				state = (CIntfType) 1;
				break;

			case 1:
				if (val > (CUnslType) 39) {
					n = (CIntfType) 0;
					done = TRUE;
					break;
				}
				else {
					val += val1;
					/*	fall through	*/
				}

			case 2:
				k = oidEncodeSubid (oid, n, val);
				h += k;
				n -= k;
				oid += k;
				val = (CUnslType) 0;
				state = (CIntfType) 2;
				break;
			}
		}
		else if (c == (CCharType) 0) {
			done = TRUE;
			switch (state) {

			case 0:
				n = (CIntfType) 0;
				break;

			case 1:
				if (val > (CUnslType) 39) {
					n = (CIntfType) 0;
					break;
				}
				else {
					val += val1;
					/*	fall through	*/
				}

			case 2:
				k = oidEncodeSubid (oid, n, val);
				h += k;
				n -= k;
				break;
			}
		}
		else {
			found = 0;
			to = buf;
			*to++ = c;
			while ((*text != '\0') &&
			       (*text != '.')) {
			    *to++ = *text++;
			}
			*to = '\0';

			for (; subtree_p && *subtree_p; subtree_p++) {
				if (lc_cmp ((*subtree_p)->text_name, buf) == 0) {
					val = (*subtree_p)->numeric_name;
					subtree_p = (*subtree_p)->subtree;
					found = 1;
					break;
				}
			}

			if (found == 0) {
				n = (CIntfType) 0;
				done = TRUE;
			}
		}
	}

	return ((n > (CIntfType) 0) ? h : (CIntfType) -1);
}

int
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
