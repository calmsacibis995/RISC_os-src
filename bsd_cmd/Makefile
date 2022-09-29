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
# $Header: Makefile,v 1.37.1.7.1.2.1.2 90/10/16 11:45:04 beacker Exp $
#
SHELL	=/bin/sh

SUBDIRS	= ../bsd_lib/diffh ../bsd_lib/dprog ../bsd_lib/spell \
Mail ac accton addbib apply at atl atq atrm atrun awk \
biff build_hlists cat cc \
checkeq checknr chgrp chmod chown clear col colcrt colrm compress cp \
comsat crypt csh \
date dd deroff df diction diff du echo efl enroll eqn error expand \
explain find find-lib fmt fpr from fsplit getNAME \
grep grep_cmds graph groups head indent install_cmd indxbib \
iostat \
kill kopt \
last lastcomm leave lex lint-libs ln lock look lookbib lpc lpd lpf lpq lpr lprm lptest \
ls make \
makewhatis man mesg msgs mv \
neqn nohup nroff od pac pagesize passwd plot pr ps ptx ratfor refer \
renice rev rm roffbib roff-lib \
sa sccs soelim sort sortbib spell spellin spellout spline struct stty \
style style1 style2 style3 sysline \
systat \
tail talk tabs tbl tc tcopy tip tk touch tr troff tsort tty unexpand \
unifdef uniq \
units users vacation vipw vgrind vmstat \
wall w whois whereis which \
window write xget xsend yacc yes

all install clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}
