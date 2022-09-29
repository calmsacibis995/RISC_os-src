#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:25:05 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/Rx3230/SABLE/PSAIO_LG/Makefile
#

MACHINE=Rx3230
VERSION=SABLE
BINTYPE=LG

DEVSRC= device/sable_3030dev.c
DEVOBJ= sable_3030dev.o
LIB=	libpsa.a

CCOPTS= -g $(PROMOPTS)
ASOPTS= -g $(PROMOPTS)
ENDIAN= -EL
TYPE=$(VERSION) PROM DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3030_ID)$(Rx3230_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
