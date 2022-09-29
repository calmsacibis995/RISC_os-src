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
/* $Header: lpreg.h,v 1.2.3.2 90/05/10 06:27:38 wje Exp $ */
/*
 * $Header: lpreg.h,v 1.2.3.2 90/05/10 06:27:38 wje Exp $
 */
/*
 * Local variables for the driver
 */

#define NLP		1
#define LPUNIT(dev)	((dev) & 0x01)
#define LPCANON(dev)	((dev) & 0x06)
#define LPMAXCOL(dev)	((dev) & 0xF8)

#define LP_MAXCOL	80
#define LPMAXIO		NBPG/2
#define NLPPAGES	2

struct  lp_device   {
	u_short	lp_cmd;
#define LP_INIT			0x0001
#define LP_SEND			0x0002
#define LP_ABORT		0x0003
#define LP_IE			0x8000
	u_short	lp_status;
#define LP_STAT_RESP_ERR	0x0001
#define LP_STAT_READY_ERR	0x0002
#define LP_STAT_CABLE_ERR	0x0004
#define LP_STAT_SELECT_ERR	0x0008
#define LP_STAT_PAPER_ERR	0x0010
#define LP_STAT_BADCMD_ERR	0x0020
#define LP_STAT_BUSY_ERR	0x0040
#define LP_STAT_INIT_ERR	0x0080
#define LP_STAT_BUSY		0x0100
	u_short	lp_page[NLPPAGES];
	u_short	lp_offset;
	u_short	lp_count;
	u_short	lp_timeout;
};

struct	lp_unitinfo {
	struct lp_device	*ui_device;
	short			ui_state;
#define	LP_OPEN		0x01
#define	LP_ACTIVE	0x02
#define	LP_ABORTED	0x04
#define	LP_CMD_FAILED	0x08
	short			ui_canon;
#define LP_CANON_RAW	0x02
#define LP_CANON_CAP	0x04
	short			ui_physcol;
	short			ui_logcol;
	short			ui_physline;
	short			ui_maxcol;
	char			*ui_memp[2];
	u_short			ui_memn;
	char			*ui_outp;
	u_short			ui_outn;
};
