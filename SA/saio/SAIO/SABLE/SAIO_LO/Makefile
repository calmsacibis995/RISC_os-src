#ident "$Header: Makefile,v 1.4 90/06/06 13:44:56 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/SAIO/SABLE/SAIO_LO/Makefile
#

MACHINE=SAIO
VERSION=SABLE
BINTYPE=LO

DEVSRC= device/sable_alldev.c
DEVOBJ= sable_alldev.o
LIB=	libsa.a
CSU=	csu.mips.o

LIBDIR=	/usr/lib/sable
CCOPTS= -O
ASOPTS=
ENDIAN= -EL
TYPE=$(VERSION) OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
