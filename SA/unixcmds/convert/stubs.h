#ident "$Header: stubs.h,v 1.1.11.1 90/07/18 16:43:41 huang Exp $"
/* $Header: stubs.h,v 1.1.11.1 90/07/18 16:43:41 huang Exp $ */
/* $Log:	stubs.h,v $
 * Revision 1.1.11.1  90/07/18  16:43:41  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.1  87/08/18  16:53:04  mdove
 * Initial revision
 *  */

extern int StubObjInitialize();
extern int StubObjRead();
extern int StubObjClose();

extern int StubFmtInitialize();
extern int StubFmtConvert();
extern int StubFmtWrite();
extern int StubFmtClose();
extern int StubFmtTerminate();
