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
# $Header: bomit.sh,v 1.3.2.2 90/05/10 03:43:36 wje Exp $
#
#  bomit - build bom for existing directories
# 
#  This version does a much better job of creating boms based
#  on the existing trees. (In fact, it may be too good; we don't
#  want to get complacent about letting bom-building get too
#  automatic!). It will do link targets automatically.
#
#  If a parameter is supplied, it is taken to be the name of the
#  tree for which a bom is to be built, otherwise, the cd is used.
#  
#  The whole first part of the awk script is parsing; we must bend over
#  backwards to accomodate 3-digit link counts, which make the
#  mode/link-count a single field as far as awk in concerned.
#  
#  Thanks to David Elliott for having a file with a large number
#  of links around, without which the three-digit link count problem
#  would still be lurkin around in thar.
#  

Postproc=${Postproc=/bin/cat}

case "$1" in
  -x) Postproc=/usr/pkg/bin/groombom
      shift
      ;;
  -*) echo "$0: usage: bomit [ -x ] dir ..."
      exit 1
      ;;
  esac
  
if [ $# -eq 1 ]
  then
    root=$1
  else
    root="*"
  fi

find $root -exec ls -ldi '{}' \; |\
sed 's/\([0-9][0-9]\),\([0-9][0-9][0-9]\)/\1, \2/' |\
awk '
{ type = substr($2, 1, 1)
  ino = $1
  if (length($2) != 10)
    {
      mode = substr($2, 1, 10)
      nlink = substr($2, 11, length($2)-10)
      owner = $3
      group = $4
      if (index("bc", type) != 0)
        {
          maj = substr($5, 1, length($5)-1)
          min = $6
          path = $10
        }
      else
        { 
          maj = ""
          min = ""
	  path = $9
        }
    }
  else
    {
      mode = $2
      nlink = $3
      owner = $4
      group = $5
      if (index("bc", type) != 0)
        {
          maj = substr($6, 1, length($6)-1)
          min = $7
          path = $11
        }
      else
        { 
          maj = ""
          min = ""
	  path = $10
        }
    }

  ltarg = ""
  if (type == "l") ltarg = $NF
  if ((nlink > 1) && (index("-bc", type) != 0))
    {
      if (linktarg[ino] == "")
	linktarg[ino] = path
      else
	ltarg = linktarg[ino]
    }
 
  printf "%s %s %s %s", path, mode, owner, group
  if (index("bc", type) != 0)
    printf " %s %s", maj, min
  printf " %s", nlink
  if (ltarg != "")
    printf " %s", ltarg
  printf "\n"
}
' | $Postproc

