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
#ident	"$Header: ckmpro.c,v 1.1.1.2 90/05/09 17:50:45 wje Exp $"

/* WARNING -- This C source program generated by Wart preprocessor. */
/* Do not edit this file; edit the Wart-format source file instead, */
/* and then run it through Wart to produce a new C source file.     */

/* Wart Version Info: */
char *wartv = "Wart Version 1A(005) 4 Jul 87";

char *protv = "C-Kermit Protocol Module 4E(031), 31 Jul 87"; /* -*-C-*- */

/* C K C P R O  -- C-Kermit Protocol Module, in Wart preprocessor notation. */
/*
 Authors: Frank da Cruz (SY.FDC@CU20B), Bill Catchings, Jeff Damens;
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained.
*/
#include "ckcdeb.h"
#include "ckcker.h"
/*
 Note -- This file may also be preprocessed by the Unix Lex program, but
 you must indent the above #include statements before using Lex, and then
 restore them to the left margin in the resulting C program before compilation.
 Also, the invocation of the "wart()" function below must be replaced by an
 invocation  of the "yylex()" function.  It might also be necessary to remove
 comments in the %%...%% section.
*/

/* State definitions for Wart (or Lex) */
#define ipkt 1
#define rfile 2
#define rdata 3
#define ssinit 4
#define ssfile 5
#define ssdata 6
#define sseof 7
#define sseot 8
#define serve 9
#define generic 10
#define get 11
#define rgen 12

/* External C-Kermit variable declarations */
  extern char sstate, *versio, *srvtxt, *cmarg, *cmarg2, *rpar();
  extern char data[], filnam[], srvcmd[], ttname[], *srvptr;
  extern int pktnum, timint, nfils, hcflg, xflg, speed, flow, mdmtyp;
  extern int prvpkt, cxseen, czseen, server, local, displa, bctu, bctr, quiet;
#ifdef MAC
  extern int tsecs, parity;
#else
  extern int tsecs, parity, backgrd;
#endif
  extern int putsrv(), puttrm(), putfil(), errpkt();
  extern char *DIRCMD, *DELCMD, *TYPCMD, *SPACMD, *SPACM2, *WHOCMD;
  extern char *rdatap;

/* Local variables */
  static char vstate = 0;  		/* Saved State   */
  static char vcmd = 0;    		/* Saved Command */
  int x;				/* General-purpose integer */
  char *s;				/* General-purpose string pointer */

/* Macros - Note, BEGIN is predefined by Wart (and Lex) */
#define SERVE  tinit(); BEGIN serve
#define RESUME if (server) { SERVE; } else { sleep(2); return; }


#define BEGIN state =

int state = 0;

wart()
{
  int c,actno;
  extern char tbl[];
  while (1) {
	c = input();
	if ((actno = tbl[c + state*128]) != -1)
	  switch(actno) {
case 1:
    { tinit();	    	    	    	/* Do Send command */
    if (sinit()) BEGIN ssinit;
       else RESUME; }
    break;
case 2:
    { tinit(); BEGIN get; }
    break;
case 3:
    { tinit(); vstate = get;  vcmd = 0;   sipkt('I'); BEGIN ipkt; }
    break;
case 4:
    { tinit(); vstate = rgen; vcmd = 'C'; sipkt('I'); BEGIN ipkt; }
    break;
case 5:
    { tinit(); vstate = rgen; vcmd = 'G'; sipkt('I'); BEGIN ipkt; }
    break;
case 6:
    { sleep(1); SERVE; }
    break;
case 7:
    { errpkt("User cancelled transaction"); /* "Abort" -- Tell other side. */
    x = quiet; quiet = 1; 		/* Close files silently. */
    clsif(); clsof(1);
    quiet = x; return(0); }
    break;
case 8:
    { rinit(rdatap); bctu = bctr; /* Get Send-Init */
	   resetc();			/* Reset counters */
    	   rtimer();			/* Reset timer */
	   BEGIN rfile; }
    break;
case 9:
    { spar(rdatap);		/* Get ack for I-packet */
    	   if (vcmd) { scmd(vcmd,cmarg); vcmd = 0; }
    	   if (vstate == get) srinit();
	   BEGIN vstate; }
    break;
case 10:
    { if (vcmd) scmd(vcmd,cmarg);	/* Get E for I-packet (ignore) */
    	   vcmd = 0; if (vstate == get) srinit();
	   BEGIN vstate; }
    break;
case 11:
    { srvptr = srvcmd; decode(rdatap,putsrv); /* Get Receive-Init */
	   cmarg = srvcmd;  nfils = -1;
    	   if (sinit()) BEGIN ssinit; else { SERVE; } }
    break;
case 12:
    { spar(rdatap); ack1(rpar());	           /* Get Init Parameters */
	   pktnum = 0; prvpkt = -1; }
    break;
case 13:
    { srvptr = srvcmd; decode(rdatap,putsrv); /* Get & decode command. */
	   putsrv('\0'); putsrv('\0');
	   sstate = srvcmd[0]; BEGIN generic; }
    break;
case 14:
    { srvptr = srvcmd;		    	 /* Get command for shell */
	   decode(rdatap,putsrv); putsrv('\0');
	   if (syscmd(srvcmd,"")) BEGIN ssinit;
	   else { errpkt("Can't do system command"); SERVE; } }
    break;
case 15:
    { errpkt("Unimplemented server function"); SERVE; }
    break;
case 16:
    { if (!cwd(srvcmd+1)) errpkt("Can't change directory"); /* CWD */
    	     SERVE; }
    break;
case 17:
    { if (syscmd(DIRCMD,srvcmd+2)) BEGIN ssinit;	/* Directory */
    	     else { errpkt("Can't list directory"); SERVE; } }
    break;
case 18:
    { if (syscmd(DELCMD,srvcmd+2)) BEGIN ssinit;	/* Erase */
    	     else { errpkt("Can't remove file"); SERVE; } }
    break;
case 19:
    { ack(); screen(SCR_TC,0,0l,""); return(0); }
    break;
case 20:
    { ack(); ttres(); screen(SCR_TC,0,0l,""); return(zkself()); }
    break;
case 21:
    { if (sndhlp()) BEGIN ssinit;
    	     else { errpkt("Can't send help"); SERVE; } }
    break;
case 22:
    { if (syscmd(TYPCMD,srvcmd+2)) BEGIN ssinit;
    	     else { errpkt("Can't type file"); SERVE; } }
    break;
case 23:
    { x = *(srvcmd+1);			/* Disk Usage query */
    	     x = ((x == '\0') || (x == SP));
	     x = (x ? syscmd(SPACMD,"") : syscmd(SPACM2,srvcmd+2));
    	     if (x) BEGIN ssinit; else { errpkt("Can't check space"); SERVE; }}
    break;
case 24:
    { if (syscmd(WHOCMD,srvcmd+2)) BEGIN ssinit;
    	     else { errpkt("Can't do who command"); SERVE; } }
    break;
case 25:
    { errpkt("Unimplemented generic server function"); SERVE; }
    break;
case 26:
    { decode(rdatap,puttrm); RESUME; }
    break;
case 27:
    { if (rcvfil()) { ack1(filnam); BEGIN rdata; } /* File header */
		else { errpkt("Can't open file"); RESUME; } }
    break;
case 28:
    { opent(); ack(); BEGIN rdata; }
    break;
case 29:
    { ack(); tsecs = gtimer(); reot(); RESUME; }
    break;
case 30:
    { if (cxseen) ack1("X");	/* Got data. */
    	       else if (czseen) ack1("Z");
	       else ack();
	   decode(rdatap,putfil); }
    break;
case 31:
    { if (reof() < 0) {	    	/* Got End Of File */
    	      errpkt("Can't close file"); RESUME;
    	    } else { ack(); BEGIN rfile; } }
    break;
case 32:
    { spar(rdatap); bctu = bctr;	/* Got ACK to Send-Init */
    	    x = sfile(xflg);		/* Send X or F header packet */
	    if (x) { resetc(); rtimer(); BEGIN ssfile; }
	   	else { s = xflg ? "Can't execute command" : "Can't open file";
		    errpkt(s); RESUME; }
          }
    break;
case 33:
    { srvptr = srvcmd;		    	 /* Got ACK to F */
	    decode(rdatap,putsrv); putsrv('\0');
	    if (*srvcmd) tlog(F110," stored as",srvcmd,0);
	    if (sdata() < 0) { clsif(); seof(""); BEGIN sseof; }
    	    	else BEGIN ssdata; }
    break;
case 34:
    { if (canned(rdatap)) { clsif(); seof("D"); BEGIN sseof; }
	    	else if (sdata() < 0) { clsif(); seof(""); BEGIN sseof; } }
    break;
case 35:
    { if (gnfile() > 0) {		/* Got ACK to EOF, get next file */
		if (sfile(xflg)) BEGIN ssdata;
		else { errpkt("Can't open file") ; RESUME; }
	    } else {			/* If no next file, EOT */
		tsecs = gtimer();
		seot();
		BEGIN sseot; }
	  }
    break;
case 36:
    { RESUME; }
    break;
case 37:
    { ermsg(rdatap);			/* Error packet, issue message. */
    x = quiet; quiet = 1;		/* Close files silently, */
    clsif(); clsof(1);			/* discarding any output file. */
    tsecs = gtimer();
    quiet = x;
#ifndef MAC
    if (backgrd && !server) fatal("Protocol error");
#endif
    RESUME; }
    break;
case 38:
    { errpkt("Unknown packet type"); RESUME; }
    break;

	    }
    }
}

char tbl[] = {
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 10, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38,  9, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 29, 38, 38, 37, 27, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 28, 38, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 30, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 31, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 32, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 33, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 34, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 35, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 36, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
-1, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 14, 15, 15, 15, 13, 15, 12, 15, 15, 15, 15, 15, 15,
15, 15, 11,  8, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15,  7, 15,  4, 15, 15, 15,  5, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15,  3,  1, 15, 15,  2, 15,  6, 15, 15, 15, 15, 15, 15, 15,
-1, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25, 25, 16, 17, 18, 19, 25, 21, 25, 25, 25, 20, 25, 25, 25,
25, 25, 25, 25, 22, 23, 25, 24, 25, 25, 25, 25, 25, 25, 25, 25,
25,  7, 25,  4, 25, 25, 25,  5, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25,  3,  1, 25, 25,  2, 25,  6, 25, 25, 25, 25, 25, 25, 25,
-1, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38,  8, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
 0, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38, 38, 38, 37, 27, 38, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38, 38,  8, 38, 38, 38, 38, 28, 26, 38, 38, 38, 38, 38, 38,
38,  7, 38,  4, 38, 38, 38,  5, 38, 38, 38, 38, 38, 38, 38, 38,
38, 38,  3,  1, 38, 38,  2, 38,  6, 38, 38, 38, 38, 38, 38, 38,
};


/*  P R O T O  --  Protocol entry function  */

proto() {

    extern int sigint();
    int x;

    conint(sigint);			/* Enable console interrupts */

/* Set up the communication line for file transfer. */

    if (local && (speed < 0)) {
	screen(SCR_EM,0,0l,"Sorry, you must 'set speed' first");
	return;
    }

    x = -1;
    if (ttopen(ttname,&x,mdmtyp) < 0) {
	debug(F111,"failed: proto ttopen local",ttname,local);
	screen(SCR_EM,0,0l,"Can't open line");
	return;
    }
    if (x > -1) local = x;
    debug(F111,"proto ttopen local",ttname,local);

    x = (local) ? speed : -1;
    if (ttpkt(x,flow,parity) < 0) {	/* Put line in packet mode, */
	screen(SCR_EM,0,0l,"Can't condition line");
	return;
    }
    if (sstate == 'x') {		/* If entering server mode, */
	server = 1;			/* set flag, */
	if (!quiet) {
	    if (!local)			/* and issue appropriate message. */
	    	conol(srvtxt);
	    else {
	    	conol("Entering server mode on ");
		conoll(ttname);
	    }
	}
    } else server = 0;
    if (sstate == 'v' && !local && !quiet)
      conoll("Escape back to your local system and give a SEND command...");
    if (sstate == 's' && !local && !quiet)
      conoll("Escape back to your local system and give a RECEIVE command...");
    sleep(1);
/*
 The 'wart()' function is generated by the wart program.  It gets a
 character from the input() routine and then based on that character and
 the current state, selects the appropriate action, according to the state
 table above, which is transformed by the wart program into a big case
 statement.  The function is active for one transaction.
*/
    wart();				/* Enter the state table switcher. */

    if (server) {			/* Back from packet protocol. */
	server = 0;
    	if (!quiet)  			/* Give appropriate message */
	    conoll("C-Kermit server done");
    }
    ttres();
    screen(SCR_TC,0,0l,"");		/* Transaction complete */
}
