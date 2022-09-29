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
/* $Header: storclass.h,v 1.1.1.2 90/05/09 15:38:55 wje Exp $ */
/* $Log:	storclass.h,v $
 * Revision 1.1.1.2  90/05/09  15:38:55  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:20:01  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:43:10  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:27  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:21  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:42:20  gb
 * added $Log keyword
 *  */
/*
 */

/*
 *   STORAGE CLASSES
 */

#define  C_EFCN          -1    /* physical end of function */
#define  C_NULL          0
#define  C_AUTO          1     /* automatic variable */
#define  C_EXT           2     /* external symbol */
#define  C_STAT          3     /* static */
#define  C_REG           4     /* register variable */
#define  C_EXTDEF        5     /* external definition */
#define  C_LABEL         6     /* label */
#define  C_ULABEL        7     /* undefined label */
#define  C_MOS           8     /* member of structure */
#define  C_ARG           9     /* function argument */
#define  C_STRTAG        10    /* structure tag */
#define  C_MOU           11    /* member of union */
#define  C_UNTAG         12    /* union tag */
#define  C_TPDEF         13    /* type definition */
#define C_USTATIC	 14    /* undefined static */
#define  C_ENTAG         15    /* enumeration tag */
#define  C_MOE           16    /* member of enumeration */
#define  C_REGPARM	 17    /* register parameter */
#define  C_FIELD         18    /* bit field */
#define  C_BLOCK         100   /* ".bb" or ".eb" */
#define  C_FCN           101   /* ".bf" or ".ef" */
#define  C_EOS           102   /* end of structure */
#define  C_FILE          103   /* file name */

	/*
	 * The following storage class is a "dummy" used only by STS
	 * for line number entries reformatted as symbol table entries
	 */

#define  C_LINE		 104
#define  C_ALIAS	 105   /* duplicate tag */
