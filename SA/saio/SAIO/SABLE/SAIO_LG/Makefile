#ident "$Header: Makefile,v 1.4 90/06/06 13:43:50 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/SAIO/SABLE/SAIO_LG/Makefile
#

MACHINE=SAIO
VERSION=SABLE
BINTYPE=LG

DEVSRC= device/sable_alldev.c
DEVOBJ= sable_alldev.o
LIB=	libsa.a
CSU=	csu.mips.o
LIBDIR=	/usr/lib/sable

CCOPTS= -g
ASOPTS= -g
ENDIAN= -EL
TYPE=$(VERSION) DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
