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
#ident	"$Header: passwd.c,v 1.10.2.3.1.2 90/08/22 13:36:48 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * passwd is a program whose sole purpose is to manage 
 * the password file. It allows system administrator
 * to add, change and display password attributes.
 * Non privileged user can change password or display 
 * password attributes which corresponds to their login name.
 */

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <shadow.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef RISCOS
#include <bsd/sys/time.h>
#include <bsd/sys/resource.h>
#endif
#include <time.h>
#include <ctype.h>         /* isalpha(c), isdigit(c), islower(c), toupper(c) */
#include <bsd/syslog.h>
/* flags  indicate password attributes to be modified */

#define PFLAG 0x080		/* change user's password */  
#define LFLAG 0x001		/* lock user's password  */
#define DFLAG 0x002		/* delete user's  password */
#define MFLAG 0x004		/* set max field -- # of days passwd is valid */		 
#define NFLAG 0x008		/* set min field -- # of days between password changes */
#define SFLAG 0x010		/* display password attributes */
#define FFLAG 0x020		/* expire  user's password */
#define AFLAG 0x040		/* display password attributes for all users*/
#define SAFLAG 0x050		/* display password attributes for all users */

/* exit  code */

#define SUCCESS	0	/* succeeded */
#define NOPERM	1	/* No permission */
#define BADSYN	2	/* Incorrect syntax */
#define FMERR	3	/* File manipulation error */
#define FATAL	4	/* Old file can not be recover */
#define FBUSY	5	/* Lock file busy */
#define BADOPT	6	/* Invalid argument to option */
 
/* define error messages */
#define MSG_NP	"Permission denied"
#define MSG_NV  "Invalid argument to option"
#define MSG_BS	"Invalid combination of options"
#define MSG_FE	"Unexpected failure. Password file unchanged."
#define MSG_FF	"Unexpected failure. Password file missing."
#define MSG_FB	"Password file(s) busy. Try again later."

/* return code from ckarg() routine */
#define FAIL 		-1

#define NUMCP	13	/* number of characters for valid password */
#define MINLENGTH 6  	/* for passwords */

#define DAY_WEEK 7	/* days per weeks */
#define DIG_CH	63	/* A character represents upto 64 
			   digits in a radix-64 notation */
#define SEC_WEEK (24 * DAY_WEEK * 60 * 60 )    /* seconds per week */

/*	 Convert base-64 ASCII string to long integer.  *
 * 	 Convert long integer to maxweek, minweeks.     *
 * 	 Convert current time from seconds to weeks.	*
 */ 	
#define CNV_AGE()	{\
	when = (long) a64l(pwd->pw_age);\
	maxweeks = when & 077;\
	minweeks = (when >> 6) & 077;\
	now = time ((long *) 0) / SEC_WEEK;\
}

/* print password status */

#define PRT_PWD(pwdp)	{\
	if (*pwdp == NULL) \
		fprintf(stdout, "NP  ");\
	else if (strlen(pwdp) < NUMCP) \
		(void) fprintf(stdout, "LK  ");\
	else\
		(void) fprintf(stdout, "PS  ");\
}
#define PRT_AGE()	{\
	if (sp->sp_max != -1) { \
		if (sp->sp_lstchg) {\
			lstchg = sp->sp_lstchg * DAY;\
			tmp = gmtime(&lstchg);\
			(void) fprintf(stdout,"%.2d/%.2d/%.2d  ",(tmp->tm_mon + 1),tmp->tm_mday,tmp->tm_year);\
		} else\
			(void) fprintf(stdout,"00/00/00  ");\
		(void) fprintf(stdout, "%d  %d ", sp->sp_min, sp->sp_max);\
	}\
}

extern int optind;
extern struct	passwd *jgetpwent();
extern  struct passwd *jgetpwnam();
struct passwd *pwd;
struct spwd *sp;
char 	lkstring[] = "*LK*";		/*lock string  to lock user's password*/
char	nullstr[] = "";
/* usage message of non privileged user */
char 	usage[]  = "	Usage:\n\tpasswd [-s] [name]\n";
/* usage message of privileged user */
char 	sausage[]  = "	Usage:\n\tpasswd [name]\n\tpasswd  [-l|-d]  [-n min] [-f] [-x max] name\n\tpasswd -s [-a]\n\tpasswd -s [name]\n";
char	opwbuf[10];
char	*pw, *uname, *prognamep;
int 	uid, retval,opwlen;
int 	 mindays, maxdays;		/* password aging information */
int 	when;
long	a64l();
char 	*l64a();
time_t 	now, maxweeks, minweeks;
ushort shadow_exist;

/* for jgetpwent */
static char line[BUFSIZ+1];
static char saveline[BUFSIZ+1];
/*
 * Configuration file info.
 */

#define CONF_FILE	"/etc/passwd.conf"
#define C_LENGTH	6
int Conf_minlength = C_LENGTH;
		/* Minimum length */
#define C_NOSHIFT	1
int Conf_noshift = C_NOSHIFT;
			/* Password can not be circular shift of username */
#define C_DIFFPOS	3
int Conf_diffpos = C_DIFFPOS;
			/* Number of positions new and old must differ by */
#define C_ALPHA		2
int Conf_alpha = C_ALPHA;
			/* Minimum number of alpha characters */
#define C_SPEC		1
int Conf_spec = C_SPEC;
			/* Minimum number of numeric or special characters */
#define C_INSIST	0
int Conf_insist = C_INSIST;
			/* Accept anything if insistent enough */
#define C_NUMINSIST	0
int Conf_numinsist = C_NUMINSIST;
			/* How insistent is enough */
#define C_TRIES		3
int Conf_tries = C_TRIES;
			/* Max number of tries at a good password */
#define C_RETYPES	2
int Conf_retypes = C_RETYPES;
			/* Max number of retries at retyping password */

main (argc, argv)
int argc;
char *argv[];
{
	extern char *crypt();
	extern char *getpass();
	extern char  *getlogin();

	/* passwd calls getpass() to get the password. The getpass() routine
	   returns at most 8 charaters. Therefore, array of 10 chars is
	   sufficient.
	*/
	char	pwbuf[10];			
	char	sampwbuf[10];
	int	numsame;
	char	buf[10];
	char 	*p, *o;
	char 	saltc[2];	 /* crypt() takes 2 char string as a salt */
	 int	count; 		 /* count verifications */
	long 	salt;
	int 	insist;
	int 	flags;
	int	c;
	int 	i, j, k, flag;	/* for triviality checks */
	int 	pwlen;  /* length of old passwords */

	flag = 0;
	insist = 0;		/* # of times the program  prompts 
				 * for valid password */
	count = 0;
	prognamep = argv[0];	
	uid = getuid();		/* get the user id */
	
	/* open the system log for possible complaints about the pw file*/
	openlog("passwd",LOG_PID,LOG_AUTH);
	/* check for shadow existence */
	if ( access(SHADOW,0) == 0)
		shadow_exist = 1; 
	if (argc > 1) 
		/* 
	    	 * ckarg() parses the arguments. In case of an error, 
		 * it sets the retval and returns FAIL (-1). It return
		 * the value which indicate which password attributes
		 * to modified 
		*/

		switch (flag = ckarg( argc, argv)) { 
		case FAIL:		/* failed */
			exit(retval); 
		case SAFLAG:		/* display password attributes */
			if (shadow_exist)
				exit(sp_display((char *) NULL));
			else
				exit(pwd_display((char *) NULL));
		default:	
			break;
		}

	argc -= optind;

	if (argc < 1 ) {
		if ((uname = getlogin()) == NULL) { 
			(void) fprintf(stderr, "%s", uid > 0 ? usage :sausage);
			exit(NOPERM);
		} else if (!flag)	/* if flag set, must be displaying or */
					/* modifying password aging attributes */
			(void) fprintf(stderr,"%s:  Changing password for %s\n",
				       prognamep,  uname);
	} else
		uname = argv[optind];

	if ((pwd = jgetpwnam(uname)) == NULL || 
            (shadow_exist &&  (sp = getspnam(uname)) == NULL)) { 
		(void) fprintf(stderr, "%s:  %s does not exist\n", 
   			       prognamep,  uname);
		exit(NOPERM);
	}

	if (uid != 0 && uid != pwd->pw_uid) { 
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_NP);
		exit(NOPERM);
	}
	/* If the flag is set display/update password attributes.*/

	switch (flag) {
	case SFLAG:		/* display password attributes */
		if (shadow_exist)
			exit(sp_display(uname));
		else
			exit(pwd_display(uname));
	case 0:			/* changing user password */
		break;
	default:		/* changing user password attributes */
		exit(update(flag));
		break;
	}

	if (shadow_exist)
		retval = ck_sppasswd();
	else
		retval = ck_pwdpasswd();
	if (retval)
		exit(retval);

	getconf(uid);
#ifdef DUMPCONF
	dumpconf();
#endif

tryagn:
	if( Conf_insist && numsame >= Conf_numinsist) { 
		goto okpasswd;
	}
	if (insist >= Conf_tries) {	/* three chances to meet triviality standard */
		(void) fprintf(stderr, "Too many failures - try later\n", prognamep);
		exit(NOPERM);
	}
	(void) strcpy (pwbuf, getpass ("New password:"));
	pwlen = strlen (pwbuf);
	if (strcmp(pwbuf, sampwbuf) != 0)  {
		strcpy(sampwbuf,pwbuf);
		numsame = 1;
	}  else  {
		numsame++;
	}

	/* Make sure new password is long enough */

	if (uid != 0 && (pwlen < Conf_minlength ) ) { 
		(void) fprintf(stderr, "Password is too short - must be at least %d digits\n",Conf_minlength);
		insist++;
		goto tryagn;
	}

	/* Check the circular shift of the logonid */
	 
	if( uid != 0 && Conf_noshift && circ(uname, pwbuf) ) {
		(void) fprintf(stderr, "Password cannot be circular shift of logonid\n");
		insist++;
		goto tryagn;
	}

	/* Ensure passwords contain at least Conf_alpha alpha characters */
	/* and Conf_spec numeric or special character */

	if (uid != 0 && (Conf_alpha > 0 || Conf_spec > 0) && !alphaspec(pwbuf)) {
		fprintf(stderr, "Password must contain ");
		if (Conf_alpha > 0) {
			fprintf(stderr, "at least %d alphabetic character%s",
				Conf_alpha, Conf_alpha == 1 ? "" : "s");
		}
		if (Conf_alpha > 0 && Conf_spec > 0) {
			fprintf(stderr, " and\n     ");
		}
		if (Conf_spec > 0) {
			fprintf(stderr, "at least %d numeric or special character%s",
				Conf_spec, Conf_spec == 1 ? "" : "s");
		}
		fprintf(stderr, "\n");
		insist++;
		goto tryagn;
	}

	/* Ensure passwords differ by at least Conf_diffpos positions */

	if ( uid != 0 ) {
		p = pwbuf;
		o = opwbuf;
		if ( pwlen >= opwlen) {
			i = pwlen;
			k = pwlen - opwlen;
		} else {
			i = opwlen;
			k = opwlen - pwlen;
		}
		for ( j = 1; j  <= i; j++ ) 
			if ( *p++ != *o++ ) 
				k++;
		if( Conf_diffpos > 0 && k  <  Conf_diffpos) {
			fprintf(stderr, "Passwords must differ by at least %d positions\n",
				Conf_diffpos);
			insist++;
			goto tryagn;
		}
	}

okpasswd:

	/* Ensure password was typed correctly, user gets Conf_retypes chances */

	(void) strcpy (buf, getpass ("Re-enter new password:"));
	if (strcmp (buf, pwbuf)) {
		if (++count > Conf_retypes) { 
			(void) fprintf(stderr, "%s: Too many tries; try again later\n", prognamep);
			exit(NOPERM);
		} else
			(void) fprintf(stderr, "They don't match; try again.\n");
		goto tryagn;
	}

	/* Construct salt, then encrypt the new password */

	(void) time(&salt);
	salt += getpid();

	saltc[0] = salt & 077;
	saltc[1] = (salt >> 6) & 077;
	for (i=0; i<2; i++) {
		c = saltc[i] + '.';
		if (c>'9') c += 7;
		if (c>'Z') c += 6;
		saltc[i] = c;
	}
	pw = crypt (pwbuf, saltc);
	exit(update(PFLAG));
}

/* ck_pwdpasswd():  Verify user old password. It also check 
 * password aging information to varify that user is authorized
 * to change password.
 */ 
  
int
ck_pwdpasswd()
{
	if (pwd->pw_passwd[0] && uid != 0) {
		(void) strcpy (opwbuf, getpass ("Old password:"));
		opwlen = strlen(opwbuf);       /* get length of old password */
		pw = crypt (opwbuf, pwd->pw_passwd);
		if (strcmp (pw, pwd->pw_passwd) != 0) {
			(void) fprintf(stderr, "Sorry.\n");
			return (NOPERM);
		}
	} else
		opwbuf[0] = '\0';
	/* password age checking applies */
	if (*pwd->pw_age != NULL) {
		CNV_AGE();
		when >>= 12 ;
		if (when <= now) {
			if (uid != 0 && ( now < when + minweeks)) { 
				(void) fprintf(stderr, "%s:  Sorry: < %ld weeks since the last change\n", prognamep, minweeks);
				return (NOPERM);
			}
			if ( minweeks > maxweeks && uid != 0) { 
				(void) fprintf(stderr, "%s: You may not change this password\n", prognamep);
				return (NOPERM);
			}
		}
	}
	return (SUCCESS);
}

/* ck_sppasswd():  Verify user old password. It also check 
 * password aging information to varify that user is authorized
 * to change password.
 */ 

int
ck_sppasswd()
{
	if (sp->sp_pwdp[0] && uid != 0) {
		(void) strcpy (opwbuf, getpass ("Old password:"));
		opwlen = strlen(opwbuf);       /* get length of old password */
		pw = crypt (opwbuf, sp->sp_pwdp);
		if (strcmp (pw, sp->sp_pwdp) != 0) {
			(void) fprintf(stderr, "Sorry.\n");
			return (NOPERM);
		}
	} else
		opwbuf[0] = '\0';
	/* password age checking applies */
	if (sp->sp_max != -1 && sp->sp_lstchg != 0) {
		now  =  DAY_NOW;
		if (sp->sp_lstchg <= now) {
			if (uid != 0 && ( now < sp->sp_lstchg  + sp->sp_min)) { 
				(void) fprintf(stderr, "%s:  Sorry: < %ld days since the last change\n", prognamep, sp->sp_min);
				return (NOPERM);
			}
			if (sp->sp_min > sp->sp_max && uid != 0) { 
				(void) fprintf(stderr, "%s: You may not change this password\n", prognamep);
				return (NOPERM);
			}
		}
	}
	return (SUCCESS);
}

int 
update(flag)
int flag;
{
	register int ret;

	/* lock the password file */
	if (lckpwdf() != 0) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FB);
		return (FBUSY);
	}
	if ( access(SHADOW,0) == 0) {
		if (!shadow_exist) {
			(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
			(void) ulckpwdf();
			return(FMERR);
		} else
			ret = sp_update(flag);

	} else  {
		if (shadow_exist) {
			(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
			(void) ulckpwdf();
			return(FMERR);
		} else
			ret = pwd_update(flag);
	}
	(void) ulckpwdf();
	return (ret);
} 

/************************************************************
 *							    *
 * pwd_update(): updates the password file.	    *
 * It takes "flag" as an argument to determine which        *
 * password attributes to modify. It returns 0 for success  *
 * and  > 0 for failure.			            *
 *							    *
 ***********************************************************/

int
pwd_update(flag)
int flag;
{
	register int i;
	extern int errno;
	struct stat buf;
	FILE *tpfp;

	

	/* ignore all the signals */

	for (i=1; i < NSIG; i++)
		(void) sigset(i, SIG_IGN);

 	/* Clear the errno. It is set because SIGKILL can not be ignored. */

	errno = 0;

	/* Mode  of the passwd file should be 444 or less */

	if (stat(PASSWD, &buf) < 0) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}
	(void) umask(~(buf.st_mode & 0444));

	if ((tpfp = fopen(PASSTEMP, "w")) == NULL) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	/*
	 *	copy passwd  files to temps, replacing matching lines
	 *	with new password attributes.
	 */

	/* entry should be there -- we just checked it */
#ifdef RISCOS
	unlimit(RLIMIT_CPU);
	unlimit(RLIMIT_FSIZE);
#endif

	jsetpwent();
	while ((pwd = jgetpwent()) != NULL) {
	   /* handle bad lines by just passing them through
	    * this lets the YP + and - lines through
	    * Other errors are posted to the syslog
	    */
	   if ( pwd == (struct passwd *)-1)
	   {
		fprintf(tpfp,"%s",saveline);
		continue;
	   }
	   if (strcmp(pwd->pw_name, uname) == 0) { 
		/* LFLAG and DFLAG should be checked before FFLAG.
		   FFLAG clears the lastchg field. We do not
		   want lastchg field to be set if one execute
		   passwd -d -f name or passwd -l -f name.
		*/

		if (flag & LFLAG) {	 /* lock password */
			pwd->pw_passwd = pw;
			if (*pwd->pw_age != NULL) {
				CNV_AGE();
				when = maxweeks + (minweeks << 6) + (now << 12);
				pwd->pw_age = l64a(when);
			}
		} 
		if (flag & DFLAG) {	 /* delete password */
			pwd->pw_passwd = nullstr;
			if (*pwd->pw_age != NULL) {
				CNV_AGE();
				when = maxweeks + (minweeks << 6) + (now << 12);
				pwd->pw_age = l64a(when);
			}
		} 
		if (flag & FFLAG) {	 /* expire password */
			if (*pwd->pw_age != NULL)  {
				CNV_AGE();
				when = maxweeks + (minweeks << 6);
				if ( when == 0)
					pwd->pw_age = ".";
				else
					pwd->pw_age = l64a(when);
			} else
				pwd->pw_age = ".";
		}
		/* MFLAG should be checked before NFLAG.  One 
		 * can not set min field without setting max field.
		 * If aging is off, force to expire a user password --
		 * set lastchg field to 0.
		*/
		if (flag & MFLAG)  { 	/* set max field */
			if ( maxdays == -1) {
				pwd->pw_age = nullstr; 
			} else  {
				if (*pwd->pw_age == NULL) {  
					minweeks  = 0;
					when = 0;
				} else
					CNV_AGE();
				maxweeks = (maxdays + (DAY_WEEK -1)) / DAY_WEEK;
				maxweeks = maxweeks > DIG_CH ? DIG_CH : maxweeks;
				when = maxweeks + (minweeks << 6) + (when & ~0xfff);
				if (when == 0)
					pwd->pw_age = ".";
				else
					pwd->pw_age = l64a(when);
			}
		}
		if (flag & NFLAG) {   /* set min field */
			if (*pwd->pw_age == NULL)  {
				(void) fprintf(stderr,"%s\n %s", MSG_BS, sausage); 
				(void) unlink(PASSTEMP);
				return (BADSYN);
			}
			CNV_AGE();
			minweeks = (mindays + (DAY_WEEK  - 1)) / DAY_WEEK;
			minweeks = minweeks > DIG_CH ? DIG_CH : minweeks;
			when = maxweeks + (minweeks << 6) + (when & ~0xfff);
			if (when == 0)
				pwd->pw_age = ".";
			else
				pwd->pw_age = l64a(when);
		}
		if (flag & PFLAG)  {	/* change password */
			pwd->pw_passwd = pw;
			if (*pwd->pw_age != NULL) {
				CNV_AGE();
				if (maxweeks  == 0) {   /* turn off aging */
					pwd->pw_age = nullstr;
				} else  {
					when = maxweeks + (minweeks << 6) + (now << 12);
					pwd->pw_age = l64a(when);
				}
			}

		}
	   }
		if (putpwent (pwd, tpfp) != 0) { 
			(void) unlink(PASSTEMP);
			(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
			return (FMERR);
		}

	}
	(void) fclose (tpfp);
	return(update_pfile(PASSWD,OPASSWD,PASSTEMP));
}

int 
update_pfile(pfilep, opfilep, tpfilep)
char *pfilep;		/* password file */
char *opfilep;		/* old passwd file */
char *tpfilep;		/* temparory password file */
{
	/*
	 *	Rename temp file back to  appropriate passwd file.
	 */

	/* remove old passwd file */
	if (unlink(opfilep) && access(opfilep, 0) == 0) {
		(void) unlink(tpfilep);
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		(void) ulckpwdf();
		return (FMERR);
	}

	/* line password file to old passwd file */
	if (link(pfilep, opfilep)) {
		(void) unlink(tpfilep);
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	/* unlink passwd file */
	if (unlink(pfilep)) {
		(void) unlink(tpfilep);
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	/* link temparory password file to password file */
	if (link(tpfilep, pfilep)) {
		(void) unlink(pfilep);
		if (link (opfilep, pfilep)) { 
			(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FF);
			return (FATAL);
		}
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	/* unlink temparory password file. If fails, don't care because 
	  password file is already updated */

	(void) unlink(tpfilep);
	return(SUCCESS);
}

/************************************************************
 *							    *
 * sp_update(): updates the shadow password file.	    *
 * It takes "flag" as an argument to determine which        *
 * password attributes to modify. It returns 0 for success  *
 * and  > 0 for failure.			            *
 *							    *
 ***********************************************************/

int
sp_update(flag)
int flag;
{
	register int i;
	extern int errno;
	struct spwd *sp;	/* pointer to a shadow password entry */
	struct stat buf;
	FILE *tsfp;

	

	/* ignore all the signals */

	for (i=1; i < NSIG; i++)
		(void) sigset(i, SIG_IGN);

 	/* Clear the errno. It is set because SIGKILL can not be ignored. */

	errno = 0;

	/* Mode  of the shadow file should be 400 or 000 */

	if (stat(SHADOW, &buf) < 0) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	(void) umask(~(buf.st_mode & 0400));
	if ((tsfp = fopen(SHADTEMP, "w")) == NULL) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
		return (FMERR);
	}

	/*
	 *	copy passwd  files to temps, replacing matching lines
	 *	with new password attributes.
	 */

	/* entry should be there -- we just checked it */

	while ((sp = getspent()) != NULL) {
		if (strcmp(sp->sp_namp, uname) == 0) { 
			/* LFLAG and DFLAG should be checked before FFLAG.
			   FFLAG clears the sp_lstchg field. We do not
			   want sp_lstchg field to be set if one execute
			   passwd -d -f name or passwd -l -f name.
			*/

			if (flag & LFLAG) {	 /* lock password */
				sp->sp_pwdp = pw;
				sp->sp_lstchg = DAY_NOW;
			} 
			if (flag & DFLAG) {	 /* delete password */
				sp->sp_pwdp = nullstr;
				sp->sp_lstchg = DAY_NOW;
			} 
			if (flag & FFLAG)	 /* expire password */
				sp->sp_lstchg = (long) 0;
			if (flag & MFLAG)  { 	/* set max field */
				if (!(flag & NFLAG) && sp->sp_min == -1)
					sp->sp_min = 0;
				if (maxdays == -1) 	/* trun off aging */
					sp->sp_min = -1;
				else if (sp->sp_max == -1)
					sp->sp_lstchg = 0;
				sp->sp_max = maxdays;
			}
			if (flag & NFLAG) {   /* set min field */
				if (sp->sp_max == -1) {
					(void) fprintf(stderr,"%s\n %s", MSG_BS, sausage); 
					(void) unlink(SHADTEMP);
					return (BADSYN);
				}
				sp->sp_min = mindays;
			}
			if (flag & PFLAG)  {	/* change password */
				sp->sp_pwdp = pw;
				/* update the last change field */
				sp->sp_lstchg = DAY_NOW;
				if (sp->sp_max == 0) {   /* turn off aging */
					sp->sp_max = -1;
					sp->sp_min = -1;
				}
			}
		}
		if (putspent (sp, tsfp) != 0) { 
			(void) unlink(SHADTEMP);
			(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FE);
			return (FMERR);
		}

	}
	(void) fclose (tsfp);
	return(update_pfile(SHADOW,OSHADOW,SHADTEMP));
}

#ifdef RISCOS
unlimit(lim)
{
	struct rlimit rlim;

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void) setrlimit(lim, &rlim);
}
#endif RISCOS

circ( s, t )
char *s, *t;
{
	char c, *p, *o, *r, buff[25], ubuff[25], pubuff[25];
	int i, j, k, l, m;
	 
	m = 2;
	i = strlen(s);
	o = &ubuff[0];
	for ( p = s; c = *p++; *o++ = c ) 
		if ( islower(c) )
			 c = toupper(c);
	*o = '\0';
	o = &pubuff[0];
	for ( p = t; c = *p++; *o++ = c ) 
		if ( islower(c) ) 
			c = toupper(c);

	*o = '\0';
	 
	p = &ubuff[0];
	while ( m-- ) {
		for ( k = 0; k  <=  i; k++) {
			c = *p++;
			o = p;
			l = i;
			r = &buff[0];
			while (--l) 
				*r++ = *o++;
			*r++ = c;
			*r = '\0';
			p = &buff[0];
			if ( strcmp( p, pubuff ) == 0 ) 
				return (1);
		}
		p = p + i;
		r = &ubuff[0];;
		j = i;
		while ( j-- ) 
			*--p = *r++;
	}
	return (SUCCESS);
}

/********************************************************
 *							*
 * ckarg(): This function parses and verifies the 	*
 * arguments.  It takes two parameters:			*
 * argc => # of arguments				*
 * argv => pointer to an argument			*
 * In case of an error it prints the appropriate error 	*
 * message, sets the retval and returns FAIL(-1).	* 		
 *							*
 ********************************************************/

int
ckarg(argc, argv)
int argc;
char **argv;
{
	extern char *optarg;
	char *char_p;
	register int c, flag = 0;

	while ((c = getopt(argc, argv, "aldfsx:n:")) != EOF) {
		switch (c) {
		case 'd':		/* delet the password */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag & (LFLAG|SAFLAG)) {
				(void) fprintf(stderr,"%s\n %s", MSG_BS, sausage); 
				retval = BADSYN;
				return (FAIL);
			}
			flag | = DFLAG;
			pw = nullstr;
			break;
		case 'l':		/* lock the password */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag & (DFLAG|SAFLAG))  {
				(void) fprintf(stderr,"%s\n %s", MSG_BS,sausage);
				retval = BADSYN;
				return (FAIL);
			}
			flag | = LFLAG;
			pw = &lkstring[0];
			break;
		case 'x':		/* set the max date */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag & SAFLAG) {
				(void) fprintf(stderr,"%s\n %s", MSG_BS, sausage);
				retval = BADSYN;
				return (FAIL);
			}
			flag | = MFLAG;
			if ((maxdays = (int) strtol(optarg, &char_p, 10)) < -1
			    || *char_p != '\0' || strlen(optarg)  <= 0) {
				(void) fprintf(stderr, "%s: %s -x\n", prognamep, MSG_NV);
				retval = BADOPT;
				return (FAIL);
			}
			break;
		case 'n':		/* set the min date */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag & SAFLAG) { 
				(void) fprintf(stderr,"%s\n %s", MSG_BS, sausage);
				retval = BADSYN;
				return (FAIL);
			}
			flag |= NFLAG;
			if (((mindays = (int) strtol(optarg, &char_p,10)) < 0 
			    || *char_p != '\0') || strlen(optarg)  <= 0) {
				(void) fprintf(stderr, "%s: %s -n\n", prognamep, MSG_NV);
				retval = BADOPT;
				return (FAIL);
			} 
			break;
		case 's':		/* display password attributes */
			if (flag && (flag != AFLAG)) { 
				(void) fprintf(stderr,"%s %\n", MSG_BS, sausage);
				retval = BADSYN;
				return (FAIL);
			} 
			flag |= SFLAG;
			break;
		case 'a':		/* display password attributes */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag && (flag != SFLAG)) { 
				(void) fprintf(stderr,"%s\n %s", MSG_BS,sausage);
				retval = BADSYN;
				return (FAIL);
			} 
			flag |= AFLAG;
			break;
		case 'f':		/* expire password attributes */
			/* Only privileged process can execute this */
			if (ckuid() != 0)
				return (FAIL);
			if (flag & SAFLAG) { 
				(void) fprintf(stderr,"%s\n %s", MSG_BS, uid > 0 ? usage : sausage);
				retval = BADSYN;
				return (FAIL);
			} 
			flag | = FFLAG;
			break;
		case '?':
			(void) fprintf(stderr,"%s", uid > 0 ? usage : sausage);
			retval = BADSYN;
			return (FAIL);
		}
	}

	argc -=  optind;
	if (argc > 1) {
		fprintf(stderr,"%s", uid > 0 ? usage : sausage);
		retval = BADSYN;
		return (FAIL);
	}

	/* If no options are specified or only the show option */
	/* is specified, return because no option error checking */
	/* is needed */
	if (!flag || (flag == SFLAG)) 
		return (flag);

	if (flag == AFLAG) {
		(void) fprintf(stderr,"%s", sausage);
		retval = BADSYN;
		return (FAIL);
	}
	if (flag != SAFLAG && argc < 1) {
		(void) fprintf(stderr,"%s", sausage);
		retval = BADSYN;
		return (FAIL);
	}
	if (flag == SAFLAG && argc >= 1) {
		(void) fprintf(stderr,"%s", sausage);
		retval = BADSYN;
		return (FAIL);
	}
	if ((maxdays == -1) &&  (flag & NFLAG)) {
		(void) fprintf(stderr, "%s: %s -x\n", prognamep, MSG_NV);
		retval = BADOPT;
		return (FAIL);
	}
	return (flag);
}

/******************************************************
 *						      *
 *  pwd_display():  displays password attributes.     * 
 *  It takes user name as a parameter. If the user    *
 *  name is NULL then it displays password attributes *
 *  for all entries on the file. It returns 0 for     *
 * success and positive  number for failure.	      *
 *						      *
 *****************************************************/

int
pwd_display(unamep)
char *unamep;
{
	extern struct tm *gmtime();
	struct tm *tmp;
	int when, maxweeks, minweeks;
	register int found = 0;

	if (unamep != NULL) {
		(void) fprintf(stdout,"%s  ",unamep);
		PRT_PWD(pwd->pw_passwd);
		if (*pwd->pw_age != NULL) {
			CNV_AGE();
			when >>= 12;
			if (when) {
				when = when * SEC_WEEK;
				tmp = gmtime (&when);
				(void) fprintf(stdout,"%.2d/%.2d/%.2d  ",(tmp->tm_mon + 1),tmp->tm_mday,tmp->tm_year);
			} else
				(void) fprintf(stdout,"00/00/00  ");
			(void) fprintf(stdout, "%d  %d ", minweeks * DAY_WEEK, maxweeks * DAY_WEEK);
		}
		(void) fprintf(stdout,"\n");
		return (SUCCESS);
	}
	jsetpwent();
	while ((pwd = jgetpwent()) != NULL) {
		if ( pwd == (struct passwd *)-1)
		{
		    fprintf(stdout,"NIS escape or garbled line:\n%s",saveline);
		    continue;
		}
		found++;
		(void) fprintf(stdout,"%s  ",pwd->pw_name);
		PRT_PWD(pwd->pw_passwd);
		if (*pwd->pw_age != NULL) {
			CNV_AGE();
			when >>= 12;
			if (when) {
				when = when * SEC_WEEK;
				tmp = gmtime (&when);
				(void) fprintf(stdout,"%.2d/%.2d/%.2d  ",(tmp->tm_mon + 1),tmp->tm_mday,tmp->tm_year);
			} else
				(void) fprintf(stdout,"00/00/00  ");
			(void) fprintf(stdout, "%d  %d ", minweeks * DAY_WEEK, maxweeks * DAY_WEEK);
		}
		(void) fprintf(stdout,"\n");
	}
	/* If password files do not have any entries or files are missing, 
	   return fatal error.
	*/

	if (found == 0) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FF);
		return (FATAL);
	}
	return (SUCCESS);
}

/******************************************************
 *						      *
 *  sp_display():  displays password attributes.      * 
 *  It takes user name as a parameter. If the user    *
 *  name is NULL then it displays password attributes *
 *  for all entries on the file. It returns 0 for     *
 * success and positive  number for failure.	      *
 *						      *
 *****************************************************/

int
sp_display(unamep)
char *unamep;
{
	extern struct tm *gmtime();
	struct tm *tmp;
	struct spwd *sp;
	struct passwd  *pwdp;
	register int found = 0;
	long lstchg;

	if (unamep != NULL) {
		jsetpwent();
		if ((jgetpwnam(unamep) != NULL) && ((sp = getspnam(unamep)) != NULL)) {
			(void) fprintf(stdout,"%s  ",unamep);
			PRT_PWD(sp->sp_pwdp);
			PRT_AGE();
			(void) fprintf(stdout,"\n");
			return (SUCCESS);
		} 
		(void) fprintf(stderr, " %s: uname %s does not exist\n", prognamep, unamep);
		return (NOPERM);
	} 
	jsetpwent();
	while ((pwdp = jgetpwent()) != NULL)
		if ( pwdp == (struct passwd *)-1)
		{
		    fprintf(stdout,"NIS escape or garbled line:\n%s",saveline);
		    continue;
		}
		if ((sp = getspnam(pwdp->pw_name)) != NULL) {
			found++;
			(void) fprintf(stdout,"%s  ",pwdp->pw_name);
			PRT_PWD(sp->sp_pwdp);
			PRT_AGE();
			(void) fprintf(stdout,"\n");
		}
	/* If password files do not have any entries or files are missing, 
	   return fatal error.
	*/

	if (found == 0) {
		(void) fprintf(stderr, "%s: %s\n", prognamep, MSG_FF);
		return (FATAL);
	}
	return (SUCCESS);
}
int
ckuid()
{
	if(uid != 0) {
		(void) fprintf(stderr,"%s: %s\n", prognamep, MSG_NP);
		return (retval = NOPERM); 
	} 
	return (SUCCESS);
}

/*
 * Return 1 if the number of alphabetics in the string is at least
 * Conf_alpha and the number of others is at least Conf_spec.
 */

int
alphaspec(str)
	char *str;
{
	int nalpha = 0;
	int nspec = 0;

	while (*str) {
		if (isalpha(*str)) {
			nalpha++;
		} else {
			nspec++;
		}
		if (nalpha >= Conf_alpha && nspec >= Conf_spec) {
			return 1;
		}
		str++;
	}
	return 0;
}

/*
 * The following routines implement a password configuration file
 * parser that allows the administrator to configure the command
 * as desired.  For example, in System V, a password is at least
 * 6 characters long, has at least 2 alphabetics, 1 non-alphabetic,
 * and the rules can not be altered.  In BSD, a password should be
 * 4 or more characters long, but anything will work if the user is
 * insistent enough.
 *
 * If the command is executed by root, any problems encountered will
 * cause a message.  Otherwise, errors are ignored silently.
 */

#define VALSIZE		100

#define TOK_NONE	0
#define TOK_BSD		1
#define TOK_LEN		2
#define TOK_SHIFT	3
#define TOK_DIFF	4
#define TOK_ALPHA	5
#define TOK_SPEC	6
#define TOK_INSIST	7
#define TOK_NINSIST	8
#define TOK_TRIES	9
#define TOK_RETYPES	10

/*
 * getconf() checks to make sure that the configuration file is mode 0644,
 * and owned by 0 and with group 0.  The file is opened, lines parsed,
 * and values set as requested.
 */

getconf(uid)
	int uid;
{
	struct stat statb;
	FILE *fp;
	char value[VALSIZE + 1];
	int token;

	if (stat(CONF_FILE, &statb)) {
		return;
	}
	if ((statb.st_mode & ~S_IFMT) != 0644 || statb.st_uid != 0 ||
	    statb.st_gid != 0) {
		if (uid == 0) {
			fprintf(stderr, "%s has bad mode/owner/group (%s)\n",
				CONF_FILE, "should be 0644, 0, 0");
		}
		return;
	}

	if ((fp = fopen(CONF_FILE, "r")) == NULL) {
		if (uid == 0) {
			fprintf(stderr, "Could not read %s after stat\n",
				CONF_FILE);
		}
		return;
	}

	while ((token = gettok(fp, value, uid)) != EOF) {
		switch (token) {

		case TOK_BSD:
			if (value[0]) {
				if (uid == 0) {
					fprintf(stderr,
						"BSD found with value %s\n",
						value);
				}
				putback();
				fclose(fp);
				return;
			}
			Conf_minlength = 5;
			Conf_noshift = 0;
			Conf_diffpos = 0;
			Conf_alpha = 0;
			Conf_spec = 0;
			Conf_insist = 1;
			Conf_numinsist = 3;
			Conf_tries = 3;
			Conf_retypes = 2;
			break;

		case TOK_LEN:
			Conf_minlength = get_num(value, 1, 8, uid,
				"Minimum password length");
			if (Conf_minlength < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_SHIFT:
			Conf_noshift = !get_yn(value, uid,
				"Circular shift");
			if (Conf_noshift < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_DIFF:
			Conf_diffpos = get_num(value, 0, 7, uid,
				"Position difference");
			if (Conf_diffpos < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_ALPHA:
			Conf_alpha = get_num(value, 0, 8, uid,
				"Number of alphabetics");
			if (Conf_alpha < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_SPEC:
			Conf_spec = get_num(value, 0, 8, uid,
				"Number of non-alphabetics");
			if (Conf_spec < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_INSIST:
			Conf_insist = get_yn(value, uid,
				"Accept insistence");
			if (Conf_insist < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_NINSIST:
			Conf_numinsist = get_num(value, 1, 25, uid,
				"Number of insistence tries");
			if (Conf_numinsist < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_TRIES:
			Conf_tries = get_num(value, 1, 25, uid,
				"Total tries");
			if (Conf_tries < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		case TOK_RETYPES:
			Conf_retypes = get_num(value, 1, 25, uid,
				"Number of retypes");
			if (Conf_retypes < 0) {
				putback();
				fclose(fp);
				return;
			}
			break;

		default:
			putback();
			fclose(fp);
			return;
		}
	}
	if (Conf_diffpos > Conf_minlength) {
		Conf_diffpos = Conf_minlength;
	}
	if (Conf_alpha > (Conf_minlength - Conf_spec)) {
		Conf_alpha = Conf_minlength - Conf_spec;
	}
	if (Conf_spec > (Conf_minlength - Conf_alpha)) {
		Conf_spec = Conf_minlength - Conf_alpha;
	}
	if (Conf_insist && Conf_numinsist < 1) {
		Conf_numinsist = 1;
	}
}

/*
 * putback() re-initializes the values when an error occurs.
 */

putback()
{
	Conf_minlength = C_LENGTH;
	Conf_noshift = C_NOSHIFT;
	Conf_diffpos = C_DIFFPOS;
	Conf_alpha = C_ALPHA;
	Conf_spec = C_SPEC;
	Conf_insist = C_INSIST;
	Conf_numinsist = C_NUMINSIST;
	Conf_tries = C_TRIES;
	Conf_retypes = C_RETYPES;
}

/*
 * gettok() reads lines from the given file, ignoring comments and
 * whitespace, puts the current value (with case lowered) into the
 * space pointed to by val, and returns the token identifier.
 */

int
gettok(fp, val, uid)
	FILE *fp;
	char *val;
	int uid;
{
	
	char tokbuf[20];
	int c;
	int token;
	int place;

	token = TOK_NONE;
	val[0] = '\0';

	/*
	 * Skip whitespace.
	 */

	for ( ; ; ) {
		while ((c = getc(fp)) == ' ' || c == '\t' || c == '\n') {
			;
		}
		if (c == EOF) {
			return EOF;
		}
		if (c == '#') {
			while ((c = getc(fp)) != '\n') {
				if (c == EOF) {
					return EOF;
				}
			}
			continue;
		}
		break;
	}

	/*
	 * Get token.
	 */

	tokbuf[0] = tolower(c);
	place = 1;
	while ((c = getc(fp)) != ' ' && c != '\t' && c != '\n') {
		if (c == EOF) {
			return EOF;
		}
		if (place > 18) {	/* bad token */
			if (uid == 0) {
				tokbuf[19] = '\0';
				fprintf(stderr, "Token too long: begins %s\n",
					tokbuf);
			}
			return;
		}
		tokbuf[place] = tolower(c);
		place++;
	}
	tokbuf[place] = '\0';

	if (strcmp(tokbuf, "bsd") == 0) {
		token = TOK_BSD;
	}
	if (strcmp(tokbuf, "length") == 0 || strcmp(tokbuf, "minimum") == 0) {
		token = TOK_LEN;
	}
	if (strcmp(tokbuf, "shift") == 0) {
		token = TOK_SHIFT;
	}
	if (strcmp(tokbuf, "differ") == 0 || strcmp(tokbuf, "diffpos") == 0) {
		token = TOK_DIFF;
	}
	if (strcmp(tokbuf, "alpha") == 0) {
		token = TOK_ALPHA;
	}
	if (strcmp(tokbuf, "special") == 0 || strcmp(tokbuf, "nonalpha") == 0) {
		token = TOK_SPEC;
	}
	if (strcmp(tokbuf, "insist") == 0) {
		token = TOK_INSIST;
	}
	if (strcmp(tokbuf, "numinsist") == 0) {
		token = TOK_NINSIST;
	}
	if (strcmp(tokbuf, "tries") == 0) {
		token = TOK_TRIES;
	}
	if (strcmp(tokbuf, "retypes") == 0) {
		token = TOK_RETYPES;
	}

	if (token == TOK_NONE) {
		if (uid == 0) {
			fprintf(stderr, "Unknown token: %s\n", tokbuf);
		}
		return token;
	}
	if (c == '\n') {
		return token;
	}

	/*
	 * Skip whitespace.
	 */

	while ((c = getc(fp)) == ' ' || c == '\t') {
		;
	}
	if (c == '\n') {
		return token;
	}
	if (c == EOF) {
		return EOF;
	}
	if (c == '#') {
		while ((c = getc(fp)) != '\n') {
			if (c == EOF) {
				return EOF;
			}
		}
		return token;
	}

	/*
	 * Get value.
	 */

	val[0] = tolower(c);
	place = 1;
	while ((c = getc(fp)) != '\n') {
		if (c == EOF) {
			return EOF;
		}
		if (place > VALSIZE - 1) {	/* bad token */
			if (uid == 0) {
				tokbuf[VALSIZE] = '\0';
				fprintf(stderr, "value too long: begins %s\n",
					val);
			}
			return;
		}
		if (c == '#' &&
		    (val == 0 ||
		     (val[place - 1] == ' ' || val[place - 1] == '\t'))) {
				while ((c = getc(fp)) != '\n') {
					if (c == EOF) {
						return EOF;
					}
				}
				break;
		}
		val[place] = tolower(c);
		place++;
	}

	/*
	 * Remove trailing whitespace.
	 */

	place--;
	while (place >= 0 && val[place] == ' ' || val[place] == '\t') {
		place--;
	}
	place++;
	val[place] = '\0';

	return token;
}

/*
 * get_num() returns the numerical value in the given string.  If it
 * is out of the range (v_min, v_max), or is non-numerical, a -1 is
 * returned to indicate an error.
 */

int
get_num(str, v_min, v_max, uid, title)
	char *str;
	int v_min;
	int v_max;
	int uid;
	char *title;
{
	int accum;

	accum = 0;
	if (*str == 0) {
		if (uid == 0) {
			fprintf(stderr, "No value for %s\n", title);
		}
		return -1;
	}
	while(*str) {
		if (!isdigit(*str)) {
			if (uid == 0) {
				fprintf(stderr, "Bad number %s for %s\n",
					str, title);
			}
			return -1;
		}
		accum *= 10;
		accum += (*str) - '0';
		str++;
	}
	if (accum < v_min) {
		if (uid == 0) {
			fprintf(stderr, "%s too small (%d < %d)\n",
				title, accum, v_min);
		}
		return -1;
	}
	if (accum > v_max) {
		if (uid == 0) {
			fprintf(stderr, "%s too big (%d > %d)\n",
				title, accum, v_max);
		}
		return -1;
	}
	return accum;
}

/*
 * get_yn() returns a 1 if the given string is empty, "1", or "yes",
 * a 0 if it is "0" or "no", and a -1 otherwise.
 */

get_yn(str, uid, title)
	char *str;
	int uid;
	char *title;
{
	
	if (*str == '\0') {
		return 1;
	}
	if (strcmp(str, "yes") == 0 || strcmp(str, "1") == 0) {
		return 1;
	}
	if (strcmp(str, "no") == 0 || strcmp(str, "0") == 0) {
		return 0;
	}
	if (uid == 0) {
		fprintf(stderr, "%s requires yes/no or 1/0; %s invalid\n",
			title, str);
	}
	return -1;
}

#ifdef DUMPCONF

/*
 * Dump the configuration values.
 */

dumpconf()
{
	fprintf(stderr, "Conf_minlength: %d\n", Conf_minlength);
	fprintf(stderr, "Conf_noshift  : %d\n", Conf_noshift);
	fprintf(stderr, "Conf_diffpos  : %d\n", Conf_diffpos);
	fprintf(stderr, "Conf_alpha    : %d\n", Conf_alpha);
	fprintf(stderr, "Conf_spec     : %d\n", Conf_spec);
	fprintf(stderr, "Conf_insist   : %d\n", Conf_insist);
	fprintf(stderr, "Conf_numinsist: %d\n", Conf_numinsist);
	fprintf(stderr, "Conf_tries    : %d\n", Conf_tries);
	fprintf(stderr, "Conf_retypes  : %d\n", Conf_retypes);
}
#endif
/* the following implements the ht_getpw stuff so we don't expand
 * passwd entries for YP ones, and preserve the yp entries in the proper
 * places.  KLUDGE, but it's just like NFS4.0 passwd.c does it.
 * This code is duplicated in bsd_cmd/passwd.c.  It has a one release or
 * or so life (replaced in 5.0 by C2 security code), and this avoids BOM
 * changes.
 */
FILE *pwf=NULL;
struct passwd passwd;
#ifndef MAXUID
#define MAXUID 65536
#endif MAXUID
#ifndef MAXGID
#define MAXGID 65536
#endif MAXGID
static char *
jpwskip(p)
register char *p;
{
	while(*p && *p != ':' && *p != '\n')
		++p;
	if(*p == '\n')
		*p = '\0';
	else if(*p)
		*p++ = '\0';
	return(p);
}

struct passwd *
jgetpwent(a)
	int a;
{

	extern struct passwd *jfgetpwent();

	if(pwf == NULL) {
		if((pwf = fopen(PASSWD, "r")) == NULL)
			return(NULL);
	}
	return (jfgetpwent(pwf));
}

struct passwd *
jfgetpwent(f)
FILE *f;
{
	register char *p;
	char *endp;
	long	x, strtol();
	char *memchr();

	if (f == (FILE *)NULL)
		return(NULL);
	p = fgets(line, BUFSIZ, f);
	if(p == NULL)
		return(NULL);
	strcpy(saveline,line);
	if ( *p == '+' || *p == '-')
	{
	    /* NIS escape, let caller worry about what to do with it*/
	    return((struct passwd *)-1);
	}
	passwd.pw_name = p;
	p = jpwskip(p);
	/* the real fgetpwent gives back the shadow pw here, which
	 * should not be done for the passwd pgm itself
	 * Rather, if the shadow exists, give the passwd as "x", which
	 * is what the pwconv did to passwd.
	 */
	passwd.pw_passwd = (shadow_exist) ? "x":p;
	p = jpwskip(p);
	if (p == NULL || *p == ':')
	{
		/* check for non-null uid */
		syslog(LOG_ALERT,"Bad passwd entry: %s",saveline);
		return ((struct passwd *)-1);
	}
	x = strtol(p, &endp, 10);	
	if (endp != memchr(p, ':', strlen(p)))
	{
		syslog(LOG_ALERT,"Bad passwd entry: %s",saveline);
		/* check for numeric value */
		return ((struct passwd *)-1);
	}
	p = jpwskip(p);
	passwd.pw_uid = (x < 0 || x > MAXUID)? (MAXUID+1): x;
	if ( x < 0 || x > MAXUID)
	{
	    syslog(LOG_ALERT,"Bad uid in passwd entry: %s",saveline);
	}
	if (p == NULL || *p == ':')
	{
		syslog(LOG_ALERT,"Bad passwd entry: %s",saveline);
		/* check for non-null uid */
		return ((struct passwd *)-1);
	}
	x = strtol(p, &endp, 10);	
	if (endp != memchr(p, ':', strlen(p)))
	{
		syslog(LOG_ALERT,"Bad passwd entry: %s",saveline);
		/* check for numeric value */
		return ((struct passwd *)-1);
	}
	p = jpwskip(p);
	passwd.pw_gid = (x < 0 || x > MAXGID)? (MAXGID+1): x;
	if ( x < 0 || x > MAXGID)
	{
	    syslog(LOG_ALERT,"Bad gid in passwd entry: %s",saveline);
	}
	passwd.pw_comment = p;
	passwd.pw_gecos = p;
	p = jpwskip(p);
	passwd.pw_dir = p;
	p = jpwskip(p);
	passwd.pw_shell = p;
	(void) jpwskip(p);

	p = passwd.pw_passwd;
	while(*p && *p != ',')
		p++;
	if(*p)
		*p++ = '\0';
#ifdef SYSTYPE_BSD43
	passwd.pw_quota = 0;
#else SYSTYPE_BSD43
	passwd.pw_age = p;
#endif SYSTYPE_BSD43
	return(&passwd);
}

struct passwd *
jgetpwnam(name)
char * name;
{
	register struct passwd *p;
	
	if(pwf == NULL) {
		if((pwf = fopen(PASSWD, "r")) == NULL)
			return(NULL);
	}
	else
		rewind(pwf);

	while (p = jfgetpwent(pwf)) 
	{
	    if ( p == (struct passwd *)-1)
		continue;  /* ignore YP and other trash */
	    if (strcmp(name, p->pw_name)==0)
		    break;
	}
	if ( p == (struct passwd *)-1)
	    return(NULL);
	else
	    return(p);
}

jsetpwent()
{
	if(pwf == NULL) {
		if((pwf = fopen(PASSWD, "r")) == NULL)
			return(NULL);
	}
	else
		rewind(pwf);
}
