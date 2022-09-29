#ident "$Header: undef.h,v 1.2 90/01/17 09:11:12 huang Exp $"
/* $Copyright$ */

/*
 * This file contains macros and defines that have the
 * same name in System V and BSD 4.2 but different meanings.
 * When it is necessary to include sys/param.h and sysv/param.h
 * you should include this file after each one.  This will prevent
 * anyone from using this symbols until some rational method of 
 * distinguishing which symbol you want to use.
 */
#undef SSIZE
#undef SINCR
#undef NCARGS
#undef PSLEP
#undef PUSER
#undef NBPW
#undef USERMODE
#undef BASEPRI
#undef ctos
#undef stoc
#undef ctod
#undef ctob
#undef btoc
#undef major
#undef minor
#undef makedev
