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
/* $Header: tsreg.h,v 1.7.1.2 90/05/10 04:44:11 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * TS11 controller registers
 */
struct	bsd43_(tsdevice) {
	u_short	tsdb;		/* data buffer */
	u_short	tssr;		/* status register */
};

#define	BSD43_LO16(x)		((x) & 0xffff)
#define BSD43_HI16(x)		((x) >> 16)

#define	BSD43_TSSR_RESET	0x8000	/* Subsystem Clear */

/*
 * Bits in (unibus) status register
 */
#define	BSD43_TS_SC	0100000		/* special condition (error) */
#define	BSD43_TS_UPE	0040000		/* Unibus parity error */
#define	BSD43_TS_SPE	0020000		/* serial bus parity error */
#define	BSD43_TS_RMR	0010000		/* register modification refused */
#define	BSD43_TS_NXM	0004000		/* nonexistant memory */
#define	BSD43_TS_NBA	0002000		/* need buffer address */
#define	BSD43_TS_XMEM	0001400		/* Unibus xmem bits */
#define	BSD43_TS_SSR	0000200		/* subsytem ready */
#define	BSD43_TS_OFL	0000100		/* off-line */
#define	BSD43_TS_FTC	0000060		/* fatal termination class */
#define	BSD43_TS_TC	0000016		/* termination class */

#define	BSD43_TS_SUCC	000		/* successful termination */
#define	BSD43_TS_ATTN	002		/* attention */
#define	BSD43_TS_ALERT 004		/* tape status alert */
#define	BSD43_TS_REJECT 06		/* function reject */
#define	BSD43_TS_RECOV 010		/* recoverable error */
#define	BSD43_TS_RECNM 012		/* recoverable error, no tape motion */
#define	BSD43_TS_UNREC 014		/* unrecoverable error */
#define	BSD43_TS_FATAL 016		/* fatal error */

#define	BSD43_TSSR_BITS	\
"\20\20SC\17UPE\16SPE\15RMR\14NXM\13NBA\12A17\11A16\10SSR\7OFL\6FC1\5FC0\4TC2\3TC1\2TC0\1-"

#define	bsd43_b_repcnt	b_bcount
#define	bsd43_b_command	b_resid

/* status message */
struct	bsd43_(ts_sts) {
	u_short	s_sts;		/* packet header */
	u_short	s_len;		/* packet length */
	u_short s_rbpcr;	/* residual frame count */
	u_short	s_xs0;		/* extended status 0 - 3 */
	u_short	s_xs1;
	u_short	s_xs2;
	u_short	s_xs3;
	u_short	s_xs4;
};

/* Error codes in xstat 0 */
#define	BSD43_TS_TMK	0100000		/* tape mark detected */
#define	BSD43_TS_RLS	0040000		/* record length short */
#define	BSD43_TS_LET	0020000		/* logical end of tape */
#define	BSD43_TS_RLL	0010000		/* record length long */
#define	BSD43_TS_WLE	0004000		/* write lock error */
#define	BSD43_TS_NEF	0002000		/* non-executable function */
#define	BSD43_TS_ILC	0001000		/* illegal command */
#define	BSD43_TS_ILA	0000400		/* illegal address */
#define	BSD43_TS_MOT	0000200		/* capstan is moving */
#define	BSD43_TS_ONL	0000100		/* on-line */
#define	BSD43_TS_IES	0000040		/* interrupt enable status */
#define	BSD43_TS_VCK	0000020		/* volume check */
#define	BSD43_TS_PED	0000010		/* phase-encoded drive */
#define	BSD43_TS_WLK	0000004		/* write locked */
#define	BSD43_TS_BOT	0000002		/* beginning of tape */
#define	BSD43_TS_EOT	0000001		/* end of tape */

#define	BSD43_TSXS0_BITS	\
"\20\20TMK\17RLS\16LET\15RLL\14WLE\13NEF\12ILC\11ILA\10MOT\7ONL\6IES\5VCK\4PED\3WLK\2BOT\1EOT"

/* Error codes in xstat 1 */
#define	BSD43_TS_DLT	0100000		/* data late */
#define	BSD43_TS_COR	0020000		/* correctable data */
#define	BSD43_TS_CRS	0010000		/* crease detected */
#define	BSD43_TS_TIG	0004000		/* trash in the gap */
#define	BSD43_TS_DBF	0002000		/* deskew buffer full */
#define	BSD43_TS_SCK	0001000		/* speed check */
#define	BSD43_TS_RBP	0000400		/* read bus parity error */
#define	BSD43_TS_IPR	0000200		/* invalid preamble */
#define	BSD43_TS_SYN	0000100		/* synchronization failure */
#define	BSD43_TS_IPO	0000040		/* invalid postamble */
#define	BSD43_TS_IED	0000020		/* invalid end of data */
#define	BSD43_TS_POS	0000010		/* postamble short */
#define	BSD43_TS_POL	0000004		/* postamble long */
#define	BSD43_TS_UNC	0000002		/* uncorrectable data */
#define	BSD43_TS_MTE	0000001		/* multitrack error */

#define	BSD43_TSXS1_BITS	\
"\20\20DLT\17-\16COR\15CRS\14TIG\13DBF\12SCK\11RBP\10IPR\7SYN\6IPO\5IED\4POS\3POL\2UNC\1MTE"

/* Error codes in xstat 2 */
#define	BSD43_TS_OPM	0100000		/* operation in progress */
#define	BSD43_TS_SIP	0040000		/* silo parity error */
#define	BSD43_TS_BPE	0020000		/* serial bus parity error */
#define	BSD43_TS_CAF	0010000		/* capstan acceleration failure */
#define	BSD43_TS_WCF	0002000		/* write card fail */
#define	BSD43_TS_DTP	0000400		/* dead track parity */
#define	BSD43_TS_DT	0000377		/* dead tracks */

#define	BSD43_TSXS2_BITS	\
"\20\20OPM\17SIP\16BPE\15CAF\14-\13WCF\12-\11DTP"

/* Error codes in xstat 3 */
#define	BSD43_TS_MEC	0177400		/* microdiagnostic error code */
#define	BSD43_TS_LMX	0000200		/* limit exceeded */
#define	BSD43_TS_OPI	0000100		/* operation incomplete */
#define	BSD43_TS_REV	0000040		/* reverse */
#define	BSD43_TS_CRF	0000020		/* capstan response fail */
#define	BSD43_TS_DCK	0000010		/* density check */
#define	BSD43_TS_NOI	0000004		/* noise record */
#define	BSD43_TS_LXS	0000002		/* limit exceeded statically */
#define	BSD43_TS_RIB	0000001		/* reverse into BOT */

#define	BSD43_TSXS3_BITS	\
"\20\10LMX\7OPI\6REV\5CRF\4DCK\3NOI\2LXS\1RIB"

/* Error codes in xstat 4 */
#define	BSD43_TS_HSS	0100000		/* high speed status */
#define	BSD43_TS_RCX	0040000		/* retry count exceeded */
#define BSD43_TS_WRC	0000377		/* write retry count */

#define	BSD43_TSXS4_BITS	\
"\20\20HSS\10RCX\7WR7\6WR6\5WR5\4WR4\3WR3\2WR2\1WR1\0WR0"

/* command message */

struct bsd43_(ts_cmd) {
	u_short	c_cmd;		/* command */
	u_short	c_loba;		/* low order buffer address */
#define	bsd43_c_repcnt c_loba		/* repeat count on position commands */
	u_short	c_hiba;		/* high order buffer address */
	u_short	c_size;		/* byte count */
};

/* commands and command bits */

#define	BSD43_TS_ACK		0100000		/* ack - release command packet */
#define	BSD43_TS_CVC		0040000		/* clear volume check */
#define BSD43_TS_SWB		0010000		/* don't swap bytes */
#define	BSD43_TS_IE		0000200

#define	BSD43_TS_RCOM		0000001
#define	BSD43_TS_REREAD	0001001		/* read data retry */
#define	BSD43_TS_SETCHR	0000004		/* set characteristics */
#define	BSD43_TS_WCOM		0000005
#define	BSD43_TS_REWRITE	0001005		/* write data retry */
#define	BSD43_TS_RETRY	0001000		/* retry bit for read and write */
#define	BSD43_TS_SFORW	0000010		/* forward space record */
#define	BSD43_TS_SREV		0000410		/* reverse space record */
#define	BSD43_TS_SFORWF	0001010		/* forward space file */
#define	BSD43_TS_SREVF	0001410		/* reverse space file */
#define	BSD43_TS_REW		0002010		/* rewind */
#define	BSD43_TS_OFFL		0000412		/* unload */
#define	BSD43_TS_WEOF		0000011		/* write tape mark */
#define BSD43_TS_RET		0000013		/* ISI QIC: retention command */
#define	BSD43_TS_SENSE	0000017		/* get status */

/* characteristics data */
struct bsd43_(ts_char) {
	u_short	char_ladr;		/* address of status packet */
	u_short	char_hadr;		/* address of status packet */
	u_short	char_size;		/* its size */
	u_short	char_mode;		/* characteristics */
	u_short	ext_char_mode;		/* extended characteristics */
};


/* characteristics */
#define	BSD43_TS_ESS	0x80		/* enable skip tape marks stop */
#define	BSD43_TS_ENB	0x40		/* enable tape mark stop off BOT */
#define	BSD43_TS_EAI	0x20		/* enable attention interrupts */
#define	BSD43_TS_ERI	0x10		/* enable message buffer release interrupts */
#define BSD43_TS_RAW	0x08		/* enable non-block mode */
#define BSD43_TS_Q11	0x04		/* operate in QIC-11 mode */
#define BSD43_TS_FMT	0x02		/* enable set format command */

/* extended characteristics */

#define BSD43_TS_XRL	0170000		/* extended inter-record gap retries */
#define BSD43_TS_NRL	0007400		/* non-extended inter-record gap retries */
#define BSD43_TS_RTY	0200		/* custom retry algorithm */
#define BSD43_TS_LGP	0100		/* long gap &/or read threshold 2 */
#define BSD43_TS_HSP	0040		/* high speed select */
#define	BSD43_TS_RDB	0020		/* read buffering enable */
#define BSD43_TS_WRB	0010		/* write buffering enable */
#define BSD43_TS_SEL	0007		/* unit select */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define HI16 BSD43_HI16
#   define LO16 BSD43_LO16
#   define TSSR_BITS BSD43_TSSR_BITS
#   define TSSR_RESET BSD43_TSSR_RESET
#   define TSXS0_BITS BSD43_TSXS0_BITS
#   define TSXS1_BITS BSD43_TSXS1_BITS
#   define TSXS2_BITS BSD43_TSXS2_BITS
#   define TSXS3_BITS BSD43_TSXS3_BITS
#   define TSXS4_BITS BSD43_TSXS4_BITS
#   define TS_ACK BSD43_TS_ACK
#   define TS_ALERT BSD43_TS_ALERT
#   define TS_ATTN BSD43_TS_ATTN
#   define TS_BOT BSD43_TS_BOT
#   define TS_BPE BSD43_TS_BPE
#   define TS_CAF BSD43_TS_CAF
#   define TS_COR BSD43_TS_COR
#   define TS_CRF BSD43_TS_CRF
#   define TS_CRS BSD43_TS_CRS
#   define TS_CVC BSD43_TS_CVC
#   define TS_DBF BSD43_TS_DBF
#   define TS_DCK BSD43_TS_DCK
#   define TS_DLT BSD43_TS_DLT
#   define TS_DT BSD43_TS_DT
#   define TS_DTP BSD43_TS_DTP
#   define TS_EAI BSD43_TS_EAI
#   define TS_ENB BSD43_TS_ENB
#   define TS_EOT BSD43_TS_EOT
#   define TS_ERI BSD43_TS_ERI
#   define TS_ESS BSD43_TS_ESS
#   define TS_FATAL BSD43_TS_FATAL
#   define TS_FMT BSD43_TS_FMT
#   define TS_FTC BSD43_TS_FTC
#   define TS_HSP BSD43_TS_HSP
#   define TS_HSS BSD43_TS_HSS
#   define TS_IE BSD43_TS_IE
#   define TS_IED BSD43_TS_IED
#   define TS_IES BSD43_TS_IES
#   define TS_ILA BSD43_TS_ILA
#   define TS_ILC BSD43_TS_ILC
#   define TS_IPO BSD43_TS_IPO
#   define TS_IPR BSD43_TS_IPR
#   define TS_LET BSD43_TS_LET
#   define TS_LGP BSD43_TS_LGP
#   define TS_LMX BSD43_TS_LMX
#   define TS_LXS BSD43_TS_LXS
#   define TS_MEC BSD43_TS_MEC
#   define TS_MOT BSD43_TS_MOT
#   define TS_MTE BSD43_TS_MTE
#   define TS_NBA BSD43_TS_NBA
#   define TS_NEF BSD43_TS_NEF
#   define TS_NOI BSD43_TS_NOI
#   define TS_NRL BSD43_TS_NRL
#   define TS_NXM BSD43_TS_NXM
#   define TS_OFFL BSD43_TS_OFFL
#   define TS_OFL BSD43_TS_OFL
#   define TS_ONL BSD43_TS_ONL
#   define TS_OPI BSD43_TS_OPI
#   define TS_OPM BSD43_TS_OPM
#   define TS_PED BSD43_TS_PED
#   define TS_POL BSD43_TS_POL
#   define TS_POS BSD43_TS_POS
#   define TS_Q11 BSD43_TS_Q11
#   define TS_RAW BSD43_TS_RAW
#   define TS_RBP BSD43_TS_RBP
#   define TS_RCOM BSD43_TS_RCOM
#   define TS_RCX BSD43_TS_RCX
#   define TS_RDB BSD43_TS_RDB
#   define TS_RECNM BSD43_TS_RECNM
#   define TS_RECOV BSD43_TS_RECOV
#   define TS_REJECT BSD43_TS_REJECT
#   define TS_REREAD BSD43_TS_REREAD
#   define TS_RET BSD43_TS_RET
#   define TS_RETRY BSD43_TS_RETRY
#   define TS_REV BSD43_TS_REV
#   define TS_REW BSD43_TS_REW
#   define TS_REWRITE BSD43_TS_REWRITE
#   define TS_RIB BSD43_TS_RIB
#   define TS_RLL BSD43_TS_RLL
#   define TS_RLS BSD43_TS_RLS
#   define TS_RMR BSD43_TS_RMR
#   define TS_RTY BSD43_TS_RTY
#   define TS_SC BSD43_TS_SC
#   define TS_SCK BSD43_TS_SCK
#   define TS_SEL BSD43_TS_SEL
#   define TS_SENSE BSD43_TS_SENSE
#   define TS_SETCHR BSD43_TS_SETCHR
#   define TS_SFORW BSD43_TS_SFORW
#   define TS_SFORWF BSD43_TS_SFORWF
#   define TS_SIP BSD43_TS_SIP
#   define TS_SPE BSD43_TS_SPE
#   define TS_SREV BSD43_TS_SREV
#   define TS_SREVF BSD43_TS_SREVF
#   define TS_SSR BSD43_TS_SSR
#   define TS_SUCC BSD43_TS_SUCC
#   define TS_SWB BSD43_TS_SWB
#   define TS_SYN BSD43_TS_SYN
#   define TS_TC BSD43_TS_TC
#   define TS_TIG BSD43_TS_TIG
#   define TS_TMK BSD43_TS_TMK
#   define TS_UNC BSD43_TS_UNC
#   define TS_UNREC BSD43_TS_UNREC
#   define TS_UPE BSD43_TS_UPE
#   define TS_VCK BSD43_TS_VCK
#   define TS_WCF BSD43_TS_WCF
#   define TS_WCOM BSD43_TS_WCOM
#   define TS_WEOF BSD43_TS_WEOF
#   define TS_WLE BSD43_TS_WLE
#   define TS_WLK BSD43_TS_WLK
#   define TS_WRB BSD43_TS_WRB
#   define TS_WRC BSD43_TS_WRC
#   define TS_XMEM BSD43_TS_XMEM
#   define TS_XRL BSD43_TS_XRL
#   define b_command bsd43_b_command
#   define b_repcnt bsd43_b_repcnt
#   define c_repcnt bsd43_c_repcnt
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


