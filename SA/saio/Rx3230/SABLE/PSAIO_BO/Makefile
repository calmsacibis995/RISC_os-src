#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:24:54 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/Rx3230/SABLE/PSAIO_BO/Makefile
#

MACHINE=Rx3230
VERSION=SABLE
BINTYPE=BO

DEVSRC= device/sable_3030dev.c
DEVOBJ= sable_3030dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3030_ID)$(Rx3230_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
