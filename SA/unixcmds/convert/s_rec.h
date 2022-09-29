#ident "$Header: s_rec.h,v 1.1.11.1 90/07/18 16:43:28 huang Exp $"
/* $Header: s_rec.h,v 1.1.11.1 90/07/18 16:43:28 huang Exp $ */
/* $Log:	s_rec.h,v $
 * Revision 1.1.11.1  90/07/18  16:43:28  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.1  87/08/18  16:53:01  mdove
 * Initial revision
 *  */

/* Header file for Motorola S record format format output */

extern int S3RecordInitialize();
extern int S2RecordInitialize();
extern int S1RecordInitialize();
extern int SRecordConvert();
extern int SRecordWrite();
extern int SRecordClose();
extern int SRecordTerminate();
