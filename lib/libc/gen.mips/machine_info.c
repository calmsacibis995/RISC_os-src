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
#ident	"$Header: machine_info.c,v 1.1.2.4.1.2.1.3 91/01/09 19:17:43 beacker Exp $"

/*
 * machine_info() returns a value giving information about the
 * running machine.  For now, the only info is based on the m_type
 * field given by uname().  Later, this may be extended.
 */

#include <machine_info.h>
#include <sys/utsname.h>

int
machine_info(cmd)
	int cmd;
{

	static int machine = -1;
	static int machclass = -1;
	struct utsname info;

	if ((cmd == MI_MACHINE || cmd == MI_MACHCLASS) && machine == -1) {
		uname(&info);
		machine = MIT_UNKNOWN;
		machclass = MIC_UNKNOWN;

		if (strcmp(info.m_type, MT_RB3133) == 0) {
			machine = MIT_RB3125;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_RB3125) == 0) {
			machine = MIT_RB3125;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_R6260) == 0) {
			machine = MIT_R6260;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_M500) == 0) {
			machine = MIT_M500;
			machclass = MIC_1;
		} else if (strcmp(info.m_type, MT_M800) == 0) {
			machine = MIT_M800;
			machclass = MIC_1;
		} else if (strcmp(info.m_type, MT_M1000) == 0) {
			machine = MIT_M1000;
			machclass = MIC_1;
		} else if (strcmp(info.m_type, MT_M2000_6) == 0) {
			machine = MIT_M2000_6;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_M2000_8) == 0) {
			machine = MIT_M2000_8;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_DT1200_3) == 0) {
			machine = MIT_M120_3;
			machclass = MIC_3;
		} else if (strcmp(info.m_type, MT_DT1200_5) == 0) {
			machine = MIT_M120_5;
			machclass = MIC_3;
		} else if (strcmp(info.m_type, MT_I2000) == 0) {
			machine = MIT_R2030;
			machclass = MIC_4;
		} else if (strcmp(info.m_type, MT_M180) == 0) {
			machine = MIT_R3240;
			machclass = MIC_3;
		} else if (strcmp(info.m_type, MT_M6000) == 0) {
			machine = MIT_R6280;
			machclass = MIC_2;
		} else if (strcmp(info.m_type, MT_R3030) == 0) {
			machine = MIT_R3030;
			machclass = MIC_4;
		} else if (strcmp(info.m_type, MT_R3030_S) == 0) {
			machine = MIT_R3030;
			machclass = MIC_4;
		} else if (strcmp(info.m_type, MT_R3030_33) == 0) {
			machine = MIT_R3030;
			machclass = MIC_4;
		} else if (strcmp(info.m_type, MT_R3030_33_S) == 0) {
			machine = MIT_R3030;
			machclass = MIC_4;
		}
	}

	switch(cmd) {

		case MI_MACHINE:
			return machine;

		case MI_MACHCLASS:
			return machclass;

		default:
			return -1;

	}
}
