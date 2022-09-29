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
/* $Header: fd_reg.h,v 1.2.1.2 90/05/09 15:51:07 wje Exp $ */
/*
 * $Header: fd_reg.h,v 1.2.1.2 90/05/09 15:51:07 wje Exp $
 */
/*
 * error codes for the extended sense data.
 */
#define SNS_7_ERR_CRC		0x10	/* id CRC error */
#define SNS_7_ERR_MISSADDR_ID	0x12	/* missing id address mark */
#define SNS_7_ERR_MISSADDR_DATA	0x13	/* missing data address mark */
#define SNS_7_ERR_SEEKPOS	0x15	/* seek positioning error */
#define SNS_7_ERR_INVAL_CMD	0x20	/* invalid command code */
#define SNS_7_ERR_INVAL_LBA	0x21	/* invalid logical block addr */
#define SNS_7_ERR_INVAL_CDB	0x24	/* invalid command desc block */
#define SNS_7_ERR_INVAL_LUN	0x25	/* invalid logical unit num */
#define SNS_7_ERR_INVAL_PARAM	0x26	/* invalid field in parm list */
#define SNS_7_ERR_WRITE_PROT	0x27	/* write protect error */
#define SNS_7_ERR_CORRUPT	0x31	/* media format corrupt */
#define SNS_7_ERR_INTERNAL	0x44	/* internal hardware error */
