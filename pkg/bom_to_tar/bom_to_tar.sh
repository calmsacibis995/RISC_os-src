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
# $Header: bom_to_tar.sh,v 1.2.2.2 90/05/10 03:43:12 wje Exp $
#
#   usage: bom_to_tar <bomfile>...
#
#   filter bomfile; ignore comments, print path of all plain files which
#   are not "slave" links. This becomes important in the instance where two
#   mutually exclusive optional subpackages have a file which must share a
#   common link; it allows the common link to be shown in both boms, while
#   keeping whatever happens to be have the name of the common link in the
#   release tree from being put into the archive; the links in the release
#   will be created by comply.
#

awk ' $0 !~ /^#/ { if (substr($2, 1, 1) == "-" && $6 == "") print $1 } ' $@

