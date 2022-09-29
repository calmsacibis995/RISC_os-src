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
#ident	"$Header: passwd.c,v 1.2.1.3.1.2 90/08/22 13:35:48 hawkes Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)passwd.c	4.32 (Berkeley) 1/21/88";
#endif not lint

/*
 * Modify a field in the password file (either password, login shell, or
 * gecos field).  This program should be suid with an owner with write
 * permission on /etc/passwd.
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <sysv/shadow.h>	/* SVR3.2 shadow capability */
#include <ndbm.h>
#include <errno.h>
#include <strings.h>
#include <ctype.h>
#include <sys/syslog.h>

/*
 * This should be the first thing returned from a getloginshells()
 * but too many programs know that it is /bin/sh.
 */
#define	DEFSHELL	"/bin/sh"

#define	DICT		"/usr/dict/words"

#define	EOS		'\0';

static short shadow_exists = 0;		/* SVR3.2 shadow passwords */
struct passwd *jgetpwent();
struct passwd *jgetpwnam();
struct passwd *jgetpwuid();
/* for jgetpwent */
static char line[BUFSIZ+1];
static char saveline[BUFSIZ+1];

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	errno, optind;
	struct passwd	*pwd;
	FILE	*tf;
	DBM	*dp;
	uid_t	uid, getuid();
	int	ch, fd, dochfn, dochsh;
	int	sfd;			/* SVR3.2 shadow passwords */
	struct spwd	*spass;		/* SVR3.2 shadow passwords */
	FILE	*stf;			/* SVR3.2 shadow passwords */
	char	*cp, *uname, *progname, *umsg,
		*getfingerinfo(), *getloginshell(), *getnewpasswd(), *malloc();

	/* open the system log for possible complaints about the pw file*/
	openlog("passwd",LOG_PID,LOG_AUTH);
	setpwfile(PASSWD);
	if (access(SHADOW,0) == 0) {	/* does shadow passwd file exist? */
		shadow_exists = 0xf;	/* yup */
		setspent();		/* initialize it */
#ifdef DEBUG_SHADOW
fprintf(stderr,"shadow password file opened\n");
#endif
	}
	progname = (cp = rindex(*argv, '/')) ? cp + 1 : *argv;
	dochfn = dochsh = 0;
	if (!strcmp(progname, "chfn")) {
		dochfn = 1;
		umsg = "usage: chfn [username]\n";
	}
	else if (!strcmp(progname, "chsh")) {
		dochsh = 1;
		umsg = "usage: chsh [username]\n";
	}
	else
		umsg = "usage: passwd [-fs] [username]\n";

	while ((ch = getopt(argc, argv, "fs")) != EOF)
		switch((char)ch) {
		case 'f':
			if (dochsh)
				goto usage;
			dochfn = 1;
			break;
		case 's':
			if (dochfn)
				goto usage;
			dochsh = 1;
			break;
		case '?':
		default:
usage:			fputs(umsg, stderr);
			exit(1);
		}

	uid = getuid();
	if (argc - optind < 1) {
		if (!(pwd = jgetpwuid(uid))) {
			fprintf(stderr, "%s: %u: unknown user uid.\n", progname, uid);
			exit(1);
		}
		if (!(uname = malloc((u_int)(strlen(pwd->pw_name) + 1)))) {
			fprintf(stderr, "%s: out of space.\n", progname);
			exit(1);
		}
		(void)strcpy(uname, pwd->pw_name);
	}
	else {
		uname = *(argv + optind);
		if (!(pwd = jgetpwnam(uname))) {
			fprintf(stderr, "%s: %s: unknown user.\n", progname, uname);
			exit(1);
		}
	}
	if (shadow_exists) {
#ifdef DEBUG_SHADOW
fprintf(stderr,"searching for %s in shadow password file\n",uname);
#endif
		if (!(spass = getspnam(uname))) {
			fprintf(stderr, "%s: %s: unknown user.\n", progname, uname);
			endspent();
#ifdef DEBUG_SHADOW
fprintf(stderr,"name not found in shadow password file\n");
#endif
			exit(1);
		}
	}
	if (uid && uid != pwd->pw_uid) {
		fputs("Permission denied.\n", stderr);
		exit(1);
	}
	printf("Changing %s for %s.\n", dochfn ? "finger information" : dochsh ? "login shell" : "password", uname);
	if (dochfn)
		cp = getfingerinfo(pwd);
	else if (dochsh)
		cp = getloginshell(pwd, uid);
	else
#ifdef NOTUSED
		if (shadow_exists)
			cp = getnewpasswd(pwd,spass,-1);
		else
#endif
			cp = getnewpasswd(pwd,spass, uid);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTSTP, SIG_IGN);
	(void) umask(0);
	if ((fd = open(PASSTEMP, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
		if (errno == EEXIST)
			fprintf(stderr, "%s: password file busy - try again.\n", progname);
		else {
			fprintf(stderr, "%s: %s: ", progname, PASSTEMP);
			perror("");
		}
		exit(1);
	}
	if ((tf = fdopen(fd, "w")) == NULL) {
		fprintf(stderr, "%s: fdopen failed.\n", progname);
		exit(1);
	}

/* If shadow password file exists and the old password has been successfully
   retrieved, create the shadow temp file now, and open it.
 */
	if (shadow_exists) {
#ifdef DEBUG_SHADOW
fprintf(stderr,"Creating shadow temp file, /etc/stmp\n");
#endif
		if ((sfd = open(SHADTEMP,O_WRONLY|O_CREAT|O_EXCL,0600)) < 0) {
			if (errno == EEXIST)
				fprintf(stderr,"%s: shadow password file busy - try again.\n",progname);
			else {
				fprintf(stderr,"%s: %s: ",progname,SHADTEMP);
				perror("");
			}
			exit(1);
		}
		if ((stf = fdopen(sfd,"w")) == NULL) {
			fprintf(stderr,"%s: fdopen failed.\n",progname);
			exit(1);
		}
#ifdef DEBUG_SHADOW
fprintf(stderr,"/etc/stmp created successfully\n");
#endif
	}
#if RISCOS
	dp = NULL;
#else
	if ((dp = dbm_open(PASSWD, O_RDWR, 0644)) == NULL) {
		fprintf(stderr, "Warning: dbm_open failed: %s: ", PASSWD);
		perror((char *)NULL);
	}
	else if (flock(dp->dbm_dirf, LOCK_EX) < 0) {
		perror("Warning: lock failed");
		dbm_close(dp);
		dp = NULL;
	}
#endif
	unlimit(RLIMIT_CPU);
	unlimit(RLIMIT_FSIZE);
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new password.
	 */
	jsetpwent();
	while ((pwd = jgetpwent()) != NULL) {
	   /* handle bad lines by just passing them through
	    * this lets the YP + and - lines through
	    * Other errors are posted to the syslog
	    */
	        if ( pwd == (struct passwd *)-1)
	        {
		    fprintf(tf,"%s",saveline);
		    continue;
	        }
		if (!strcmp(pwd->pw_name, uname)) {
			if (uid && uid != pwd->pw_uid) {
				fprintf(stderr, "%s: permission denied.\n", progname);
				goto out;
			}
			if (dochfn)
				pwd->pw_gecos = cp;
			else if (dochsh)
				pwd->pw_shell = cp;
			else  {
				if (shadow_exists) {
#ifdef DEBUG_SHADOW
fprintf(stderr,"Attempting to match entry in shadow password file.\n");
#endif
					while ((spass = getspent()) != NULL ) {
						if (!strcmp(spass->sp_namp,uname)) {
							spass->sp_pwdp = cp;
							spass->sp_lstchg = DAY_NOW;
							if (spass->sp_max == 0) {
								spass->sp_max = -1;
								spass->sp_min = -1;
							}
#ifdef DEBUG_SHADOW
fprintf(stderr,"Attempting to write record into /etc/stmp.\n");
#endif
						}
						if (putspent(spass,stf) != 0 ) {
							fprintf(stderr,"%s: Unexpected failure.  Shadow password file unchanged.\n",progname);
							goto out;
						}
					}
					endspent();
					(void)fclose(stf);
					if (chmod(SHADTEMP,00400) < 0) {
						fprintf(stderr,"%s: Chmod unsuccessful.  Shadow password file unchanged.\n",progname);
						goto out;
					}
					if (rename(SHADTEMP,SHADOW) < 0) {
						fprintf(stderr,"%s: Rename of /etc/stmp to /etc/shadow unsuccessful.\n",progname);
						goto out;
					}	
				}
				else
					pwd->pw_passwd = cp;
			}
			if (pwd->pw_gecos[0] == '*')	/* ??? */
				pwd->pw_gecos++;
			replace(dp, pwd);
		}
		/* handle the age field similarly to sysv putpwent
		 * we are cheating here, and are reusing the comment field
		 * to hold the ageing info.  BSD passwd does not have
		 * the age field.
		 */
		fprintf(tf, "%s:%s", pwd->pw_name, pwd->pw_passwd);
		if ((*pwd->pw_comment) != '\0')
		    (void)fprintf(tf,",%s",pwd->pw_comment);
		fprintf(tf, ":%d:%d:%s:%s:%s\n",
			pwd->pw_uid,
			pwd->pw_gid,
			pwd->pw_gecos,
			pwd->pw_dir,
			pwd->pw_shell);
	}
	endpwent();
	if (dp && dbm_error(dp))
		fputs("Warning: dbm_store failed.\n", stderr);
	(void) fflush(tf);
	if (ferror(tf)) {
		fprintf(stderr, "Warning: %s write error, %s not updated.\n",
		    PASSTEMP, PASSWD);
		goto out;
	}
	(void)fclose(tf);
	if (dp != NULL)
		dbm_close(dp);
	if (rename(PASSTEMP, PASSWD) < 0) {
		perror(progname);
	out:					/* error conditions only */
		(void)unlink(PASSTEMP);
		if (shadow_exists) {
			endspent();		/* close shadow file */
			(void)unlink(SHADTEMP); /* unlink temp shadow file */
		}
		exit(1);
	}
	exit(0);
}

unlimit(lim)
	int	lim;
{
	struct rlimit rlim;

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void)setrlimit(lim, &rlim);
}

/*
 * Replace the password entry in the dbm data base with pwd.
 */
replace(dp, pwd)
	DBM *dp;
	struct passwd *pwd;
{
	datum key, content;
	register char *cp, *tp;
	char buf[BUFSIZ];

	if (dp == NULL)
		return;

	cp = buf;
#define	COMPACT(e)	tp = pwd->e; while (*cp++ = *tp++);
	COMPACT(pw_name);
	COMPACT(pw_passwd);
	bcopy((char *)&pwd->pw_uid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_gid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_quota, cp, sizeof (int));
	cp += sizeof (int);
	COMPACT(pw_comment);
	COMPACT(pw_gecos);
	COMPACT(pw_dir);
	COMPACT(pw_shell);
	content.dptr = buf;
	content.dsize = cp - buf;
	key.dptr = pwd->pw_name;
	key.dsize = strlen(pwd->pw_name);
	dbm_store(dp, key, content, DBM_REPLACE);
	key.dptr = (char *)&pwd->pw_uid;
	key.dsize = sizeof (int);
	dbm_store(dp, key, content, DBM_REPLACE);
}

char *
getnewpasswd(pwd,spass,u)
	register struct passwd *pwd;
	register struct spwd *spass;
	uid_t u;
{
	time_t	salt, time();
	int	c, i, insist;
	char	*pw, pwbuf[10], pwcopy[10], saltc[2],
		*crypt(), *getpass();

	if (shadow_exists)  {
#ifdef DEBUG_SHADOW
fprintf(stderr,"Reading shadow password file now.\n");
#endif
		if (spass->sp_pwdp[0] && u != 0)  {	/* password present */
			(void)strcpy(pwbuf,getpass("Old password:"));
			pw = crypt(pwbuf,spass->sp_pwdp);
			if (strcmp(pw, spass->sp_pwdp) != 0) {
				puts("Sorry!");
				exit(1);
			}
		}
#ifdef DEBUG_SHADOW
fprintf(stderr,"Successful retrieval and decrypt of shadow password.\n");
#endif
	}  else  {
		if (pwd->pw_passwd[0] && u != 0) {
			(void)strcpy(pwbuf, getpass("Old password:"));
			pw = crypt(pwbuf, pwd->pw_passwd);
			if (strcmp(pw, pwd->pw_passwd) != 0) {
				puts("Sorry.");
				exit(1);
			}
		}
	}
	for(;;) {
		(void)strcpy(pwbuf, getpass("New password:"));
		if (!*pwbuf) {
			puts("Password unchanged.");
			exit(1);
		}
		if (strcmp(pwbuf, pwcopy)) {
			insist = 1;
			(void)strcpy(pwcopy, pwbuf);
		}
		else if (++insist == 4)
			break;
		if (strlen(pwbuf) <= 4)
			puts("Please enter a longer password.");
		else {
			for (pw = pwbuf; *pw && islower(*pw); ++pw);
			if (*pw)
				break;
			puts("Please don't use an all-lower case password.\nUnusual capitalization, control characters or digits are suggested.");
		}
	}
	if (strcmp(pwbuf, getpass("Retype new password:"))) {
		puts("Mismatch - password unchanged.");
		exit(1);
	}
	(void)time(&salt);
	salt = 9 * getpid();
	saltc[0] = salt & 077;
	saltc[1] = (salt>>6) & 077;
	for (i = 0; i < 2; i++) {
		c = saltc[i] + '.';
		if (c > '9')
			c += 7;
		if (c > 'Z')
			c += 6;
		saltc[i] = c;
	}
	return(crypt(pwbuf, saltc));
}

char *
getloginshell(pwd, u)
	struct passwd *pwd;
	uid_t u;
{
	static char newshell[BUFSIZ];
	char *cp, *valid, *getusershell();

	if (pwd->pw_shell == 0 || *pwd->pw_shell == '\0')
		pwd->pw_shell = DEFSHELL;
	if (u != 0) {
		do {
			valid = getusershell();
			if (valid == NULL) {
				printf("Cannot change from restricted shell %s\n",
					pwd->pw_shell);
				exit(1);
			}
		} while (strcmp(pwd->pw_shell, valid) != 0);
	}
	printf("Old shell: %s\nNew shell: ", pwd->pw_shell);
	(void)fgets(newshell, sizeof (newshell) - 1, stdin);
	cp = index(newshell, '\n');
	if (cp)
		*cp = '\0';
	if (newshell[0] == 0) {
		puts("Login shell unchanged.");
		exit(1);
	}
	/*
	 * Allow user to give shell name w/o preceding pathname.
	 */
	if (u != 0 || newshell[0] != '/') {
		endusershell();
		do {
			valid = getusershell();
			if (valid == 0) {
				if (u == 0) {
					valid = newshell;
					break;
				}
				printf("%s is unacceptable as a new shell.\n",
					newshell);
				exit(1);
			}
			if (newshell[0] == '/') {
				cp = valid;
			} else {
				cp = rindex(valid, '/');
				if (cp == 0)
					cp = valid;
				else
					cp++;
			}
		} while (strcmp(newshell, cp) != 0);
	}
	else
		valid = newshell;
	if (strcmp(valid, pwd->pw_shell) == 0) {
		puts("Login shell unchanged.");
		exit(1);
	}
	if (access(valid, X_OK) < 0) {
		printf("%s is unavailable.\n", valid);
		exit(1);
	}
	if (strcmp(valid, DEFSHELL) == 0)
		valid[0] = '\0';
	return (valid);
}

struct default_values {
	char *name;
	char *office_num;
	char *office_phone;
	char *home_phone;
};

/*
 * Get name, room number, school phone, and home phone.
 */
char *
getfingerinfo(pwd)
	struct passwd *pwd;
{
	char in_str[BUFSIZ];
	struct default_values *defaults, *get_defaults();
	static char answer[4*BUFSIZ];

	answer[0] = '\0';
	defaults = get_defaults(pwd->pw_gecos);
	puts("Default values are printed inside of '[]'.");
	puts("To accept the default, type <return>.");
	puts("To have a blank entry, type the word 'none'.");
	/*
	 * Get name.
	 */
	do {
		printf("\nName [%s]: ", defaults->name);
		(void) fgets(in_str, BUFSIZ - 1, stdin);
		if (special_case(in_str, defaults->name)) 
			break;
	} while (illegal_input(in_str));
	(void) strcpy(answer, in_str);
	/*
	 * Get room number.
	 */
	do {
		printf("Room number (Exs: 597E or 197C) [%s]: ",
			defaults->office_num);
		(void) fgets(in_str, BUFSIZ - 1, stdin);
		if (special_case(in_str, defaults->office_num))
			break;
	} while (illegal_input(in_str) || illegal_building(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get office phone number.
	 * Remove hyphens.
	 */
	do {
		printf("Office Phone (Ex: 6426000) [%s]: ",
			defaults->office_phone);
		(void) fgets(in_str, BUFSIZ - 1, stdin);
		if (special_case(in_str, defaults->office_phone))
			break;
		remove_hyphens(in_str);
	} while (illegal_input(in_str) || not_all_digits(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get home phone number.
	 * Remove hyphens if present.
	 */
	do {
		printf("Home Phone (Ex: 9875432) [%s]: ", defaults->home_phone);
		(void) fgets(in_str, BUFSIZ - 1, stdin);
		if (special_case(in_str, defaults->home_phone))
			break;
		remove_hyphens(in_str);
	} while (illegal_input(in_str) || not_all_digits(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	if (strcmp(answer, pwd->pw_gecos) == 0) {
		puts("Finger information unchanged.");
		exit(1);
	}
	return (answer);
}

/*
 * Prints an error message if a ':', ',' or a newline is found in the string.
 * A message is also printed if the input string is too long.  The password
 * file uses :'s as separators, and are not allowed in the "gcos" field;
 * commas are used as separators in the gcos field, so are disallowed.
 * Newlines serve as delimiters between users in the password file, and so,
 * those too, are checked for.  (I don't think that it is possible to
 * type them in, but better safe than sorry)
 *
 * Returns '1' if a colon, comma or newline is found or the input line is
 * too long.
 */
illegal_input(input_str)
	char *input_str;
{
	char *ptr;
	int error_flag = 0;
	int length = strlen(input_str);

	if (strpbrk(input_str, ",:")) {
		puts("':' and ',' are not allowed.");
		error_flag = 1;
	}
	if (input_str[length-1] != '\n') {
		/* the newline and the '\0' eat up two characters */
		printf("Maximum number of characters allowed is %d\n",
			BUFSIZ-2);
		/* flush the rest of the input line */
		while (getchar() != '\n')
			/* void */;
		error_flag = 1;
	}
	/*
	 * Delete newline by shortening string by 1.
	 */
	input_str[length-1] = '\0';
	/*
	 * Don't allow control characters, etc in input string.
	 */
	for (ptr = input_str; *ptr; ptr++)
		if (!isprint(*ptr)) {
			puts("Control characters are not allowed.");
			error_flag = 1;
			break;
		}
	return (error_flag);
}

/*
 * Removes '-'s from the input string.
 */
remove_hyphens(str)
	char *str;
{
	char *hyphen;

	while ((hyphen = index(str, '-')) != NULL)
		(void) strcpy(hyphen, hyphen+1);
}

/*
 *  Checks to see if 'str' contains only digits (0-9).  If not, then
 *  an error message is printed and '1' is returned.
 */
not_all_digits(str)
	register char *str;
{
	for (; *str; ++str)
		if (!isdigit(*str)) {
			puts("Phone numbers may only contain digits.");
			return(1);
		}
	return(0);
}

/*
 * Deal with Berkeley buildings.  Abbreviating Cory to C and Evans to E.
 * Correction changes "str".
 *
 * Returns 1 if incorrect room format.
 * 
 * Note: this function assumes that the newline has been removed from str.
 */
illegal_building(str)
	register char *str;
{
	int length = strlen(str);
	register char *ptr;

	/*
	 * If the string is [Ee]vans or [Cc]ory or ends in
	 * [ \t0-9][Ee]vans or [ \t0-9M][Cc]ory, then contract the name
	 * into 'E' or 'C', as the case may be, and delete leading blanks.
	 */
	if (length >= 5 && strcmp(ptr = str + length - 4, "vans") == 0 &&
	    (*--ptr == 'e' || *ptr == 'E') &&
	    (--ptr < str || isspace(*ptr) || isdigit(*ptr))) {
		for (; ptr > str && isspace(*ptr); ptr--)
			;
		ptr++;
		*ptr++ = 'E';
		*ptr = '\0';
	} else
	if (length >= 4 && strcmp(ptr = str + length - 3, "ory") == 0 &&
	    (*--ptr == 'c' || *ptr == 'C') &&
	    (--ptr < str || *ptr == 'M' || isspace(*ptr) || isdigit(*ptr))) {
		for (; ptr > str && isspace(*ptr); ptr--)
			;
		ptr++;
		*ptr++ = 'C';
		*ptr = '\0';
	}
	return (0);
}

/*
 * get_defaults picks apart "str" and returns a structure points.
 * "str" contains up to 4 fields separated by commas.
 * Any field that is missing is set to blank.
 */
struct default_values *
get_defaults(str)
	char *str;
{
	struct default_values *answer;
	char	*malloc();

	answer = (struct default_values *)
		malloc((unsigned)sizeof(struct default_values));
	if (answer == (struct default_values *) NULL) {
		fputs("\nUnable to allocate storage in get_defaults!\n", stderr);
		exit(1);
	}
	/*
	 * Values if no corresponding string in "str".
	 */
	answer->name = str;
	answer->office_num = "";
	answer->office_phone = "";
	answer->home_phone = "";
	str = index(answer->name, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->office_num = str + 1;
	str = index(answer->office_num, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->office_phone = str + 1;
	str = index(answer->office_phone, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->home_phone = str + 1;
	return (answer);
}

/*
 *  special_case returns true when either the default is accepted
 *  (str = '\n'), or when 'none' is typed.  'none' is accepted in
 *  either upper or lower case (or any combination).  'str' is modified
 *  in these two cases.
 */
special_case(str,default_str)
	char *str, *default_str;
{
	static char word[] = "none\n";
	char *ptr, *wordptr;

	/*
	 *  If the default is accepted, then change the old string do the 
	 *  default string.
	 */
	if (*str == '\n') {
		(void) strcpy(str, default_str);
		return (1);
	}
	/*
	 *  Check to see if str is 'none'.  (It is questionable if case
	 *  insensitivity is worth the hair).
	 */
	wordptr = word-1;
	for (ptr = str; *ptr != '\0'; ++ptr) {
		++wordptr;
		if (*wordptr == '\0')	/* then words are different sizes */
			return (0);
		if (*ptr == *wordptr)
			continue;
		if (isupper(*ptr) && (tolower(*ptr) == *wordptr))
			continue;
		/*
		 * At this point we have a mismatch, so we return
		 */
		return (0);
	}
	/*
	 * Make sure that words are the same length.
	 */
	if (*(wordptr+1) != '\0')
		return (0);
	/*
	 * Change 'str' to be the null string
	 */
	*str = '\0';
	return (1);
}
/* the following implements the ht_getpw stuff so we don't expand
 * passwd entries for YP ones, and preserve the yp entries in the proper
 * places.  KLUDGE, but it's just like NFS4.0 passwd.c does it.
 * This code is duplicated in cmd/passwd.c.  It has a one release or
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
	passwd.pw_passwd = (shadow_exists) ? "x":p;
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
	passwd.pw_quota = 0;	/* cheat here, so as not to mess up age info*/
	passwd.pw_comment = p;	
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
struct passwd *
jgetpwuid(uid)
uid_t uid;
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
	    if (uid == p->pw_uid)
		    break;
	}
	if ( p == (struct passwd *)-1)
	    return(NULL);
	else
	    return(p);
}
