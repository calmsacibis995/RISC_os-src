#!/bin/sh
#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: uudemon.clnu.sh,v 1.4.2.3 90/05/10 00:43:13 wje Exp $
# #ident	"@(#)uucp:uudemon.cleanup	2.2"
#
#	This demon cleans up uucp directories.
#	It is started by /usr/lib/crontab;
#	it can be run daily, weekly, whatever depending on the system
#	  uucp load.
#	The log files get large so you may have to up the ulimit.
#	e.g.
#
# 45 23 * * * ulimit 5000; /bin/su uucp -c "/usr/lib/uucp/uudemon.clnu"
#

MAILTO=uucp
MAILDIR=/usr/mail
export PATH
PATH=/bin:/usr/bin:/usr/lib/uucp:/usr/lbin
TMP=/tmp/uu$$

#
#	These are taken from the Makefile.  If changed in Makefile
#	they must be changed here also.
#
PUBDIR=/usr/spool/uucppublic
SPOOL=/usr/spool/uucp
LOCKS=/usr/spool/locks
XQTDIR=/usr/spool/uucp/.Xqtdir
CORRUPT=/usr/spool/uucp/.Corrupt
LOGDIR=/usr/spool/uucp/.Log
SEQDIR=/usr/spool/uucp/.Sequence
STATDIR=/usr/spool/uucp/.Status
WORKDIR=/usr/spool/uucp/.Workspace
ADMIN=/usr/spool/uucp/.Admin

#	OLD is the directory for archiving old admin/log files
OLD=$SPOOL/.Old
O_LOGS=$OLD/Old-Log

if [ -f $ADMIN/xferstats ]
then
	mv $ADMIN/xferstats $OLD/xferstats
fi
if [ -f $ADMIN/audit ]
then
	mv $ADMIN/audit $OLD/audit
fi
if [ -f $ADMIN/errors ]
then
	mv $ADMIN/errors $OLD/errors
fi
if [ -f $ADMIN/Foreign ]
then
	mv $ADMIN/Foreign $OLD/Foreign
fi

> $ADMIN/xferstats
> $ADMIN/audit
> $ADMIN/errors
> $ADMIN/Foreign

#
#	The list in the for controls how many old LOG files are retained.
#	O_LOGS-2 goes to O_LOGS-3, O_LOGS-1 goes to O_LOGS-2.
#	Todays goes to O_LOGS-1
#
for i in  2 1
do
	j=`expr $i + 1`
	if [ -f ${O_LOGS}-$i ]
	then
		mv ${O_LOGS}-$i ${O_LOGS}-$j
	fi
done

#
#	Combine all log files into O_LOGS-1.
#	Add a name separator between each system.
#
> ${O_LOGS}-1
for i in uucico uucp uux uuxqt
do
	if [ -d $LOGDIR/$i ]
	then
		cd $LOGDIR/$i
		for j in *
		do
			if [ "$j" = "*" ]
			then
				break
			fi
			echo "********** $j **********" >> ${O_LOGS}-1
			cat $j >> ${O_LOGS}-1
			rm -f $j
		done
	fi
done

#	Execute the system directory cleanup program
#	See uucleanup.1m for details.
uucleanup -D7 -C7 -X2 -o2 -W1
#	Use the grep instead of the mv to ignore warnings to uucp
# grep -v 'warning message sent to uucp' $ADMIN/uucleanup > $OLD/uucleanup
if [ -f $ADMIN/uucleanup ]
then
	mv $ADMIN/uucleanup $OLD/uucleanup
fi
if [ -s $OLD/uucleanup ]
then
	(echo "Subject: cleanup"; cat $OLD/uucleanup) | mail $MAILTO
fi
>$ADMIN/uucleanup

#  cleanup funny directories that may have been created in the spool areas
for d in $SPOOL/[A-z]*
do
	if [ "$d" -eq "$SPOOL/"'[A-z]*' ]; then
		continue
	fi

	cd $d
	for s in *
	do
		if [ "$s" = "*" ]
		then
			break
		fi
		if [ -d $s ]
		then
			rm -fr $s
		fi
	done

#		if it is now empty, remove it.
	cd ..
	rmdir $d
done 2>&1 >/dev/null

#
#	Find old cores
#
find $SPOOL -name core -print > $TMP
if [ -s $TMP ]
then
	(echo "Subject: cores"; cat $TMP) | mail $MAILTO
fi

#
#	Remove old files and directories
#
if [ -d $PUBDIR ]
then
	find $PUBDIR -type f -mtime +30 -exec rm -f {} \;
fi
if [ "$PUBDIR/*" -ne "$PUBDIR/"'*' ]
then
	find $PUBDIR/* -type d -exec rmdir {} \;
fi
if [ -d $SEQDIR ]
then
	find $SEQDIR -type f -mtime +30 -exec rm -f {} \;
fi
if [ -d $WORKDIR ]
then
	find $WORKDIR -type f -mtime +1 -exec rm -f {} \;
fi
if [ -d $STATDIR ]
then
	find $STATDIR -type f -mtime +2 -exec rm -f {} \;
fi
if [ -d $CORRUPT ]
then
	find $CORRUPT -type f -mtime +10 -exec rm -f {} \;
fi

rm -f $LOCKS/LTMP*
if [ "$SPOOL/[A-z]*" -ne "$SPOOL/"'[A-z]*' ]
then
	rmdir $SPOOL/[A-z]* 2>/dev/null
fi

#
#	Mail a daily summary of status
#
> $TMP
if [ -f ${O_LOGS}-1 ]
then
	grep passwd ${O_LOGS}-1 > $TMP
	grep "REQUEST.*/" ${O_LOGS}-1 >> $TMP
fi
if [ -s $TMP ]
then
	(echo "Subject: uucp requests"; cat $TMP) | mail $MAILTO
fi


> $TMP
if [ -f ${O_LOGS}-1 ]
then
	awk '/(DENIED)/	{print prev}
		{prev = $0}' ${O_LOGS}-1 > $TMP
fi
if [ -s $TMP ]
then
	(echo "Subject: uucp DENIED"; cat $TMP) | mail $MAILTO
fi

uustat -q > $TMP
if [ -s $TMP ]
then
	(echo "Subject: uu-status"; cat $TMP) | mail $MAILTO
fi

> $TMP
if [ -d $CORRUPT ]
then
	ls $CORRUPT > $TMP
fi
if [ -s $TMP ]
then
	(echo "Subject: $CORRUPT"; cat $TMP) | mail $MAILTO
fi

tail $OLD/errors 2>/dev/null > $TMP
tail $OLD/Foreign 2>/dev/null >> $TMP
if [ -s $TMP ]
then
	(echo "Subject: uucp Admin"; cat $TMP) | mail $MAILTO
fi
(echo "Subject: uucleanup ran; $SPOOL du"; du $SPOOL) | mail $MAILTO

#
#	Dispose of mail to uucp and nuucp
#
# rm -f $MAILDIR/uucp $MAILDIR/nuucp $TMP
rm -f $TMP
