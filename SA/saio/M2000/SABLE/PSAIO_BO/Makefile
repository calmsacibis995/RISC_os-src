#ident "$Header: Makefile,v 1.4.2.1 90/07/18 16:23:21 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M2000/SABLE/PSAIO_BO/Makefile
#

MACHINE=M2000
VERSION=SABLE
BINTYPE=BO

DEVSRC= device/sable_3200dev.c
DEVOBJ= sable_3200dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3200_ID)$(M2000_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
