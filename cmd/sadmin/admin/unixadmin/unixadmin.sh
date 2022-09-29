#! /bin/sh
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
# $Header: unixadmin.sh,v 1.5.2.2 90/05/09 18:34:07 wje Exp $
#	General purpose hook into administrative logins used as commands,
#	so that the commands can have passwords on them.

set -f
PATH=${PATH:-/bin:/usr/bin}:/usr/lbin
export PATH
cmd=`basename $0`
if  grep "^${cmd}:" /etc/passwd >/dev/null
then
	# Magic here!
	# The problem is that we want the commands to (potentially) have a
	# password, so that they can be restricted if the user likes, and
	# to be within the restricted shell so that the user can not break out
	# and become super-user.  The complication is that we want the command
	# to have arguments, but /bin/su will not permit arguments to something
	# that has /bin/rsh as its login shell and /bin/su clears out the
	# environment when the - argument is used.  So we open another File
	# Descriptor and ship the arguments in via a pipe.
	#	make File Descriptor 4 the same as 0 (standard input)
	exec 4<&0
	#	echo the arguments down a pipe.  On the receiving end,
	#	connect the pipe to File Descriptor 3 and connect the original
	#	standard input, available on FD 4, to FD 0.  Lastly, close FD 4.
	#	See the .profile file (admin profile.dot) for how the arguments
	#	are read.
	echo $*  |  3<&0 0<&4 4<&- /bin/su - ${cmd}
else
	admerr $0 Command ${cmd} does not exist in /etc/passwd.
	exit 1
fi
