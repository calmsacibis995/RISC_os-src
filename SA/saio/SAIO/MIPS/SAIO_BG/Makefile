#ident "$Header: Makefile,v 1.5 90/06/06 13:34:00 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/SAIO/MIPS/SAIO_BG/Makefile
#

MACHINE=SAIO
VERSION=MIPS
BINTYPE=BG

DEVSRC= device/mips_alldev.c
DEVOBJ= mips_alldev.o
LIB=	libsa.a
CSU=	csu.mips.o
LIBDIR=	/usr/lib/saio

HARDWARE_BUGS_6000= -DR6000_BUG_FLUSH -DR6000_BUG_IDPROM

CCOPTS= -g $(HARDWARE_BUGS_6000)
ASOPTS= -g $(HARDWARE_BUGS_6000)

ENDIAN= -EB
TYPE=$(VERSION) DBG
DEPTH=../../../..

include $(DEPTH)/Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
