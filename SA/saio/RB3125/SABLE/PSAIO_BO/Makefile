#ident "$Header: Makefile,v 1.1.2.1 90/07/18 16:31:44 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/RB3125/SABLE/PSAIO_BO/Makefile
#

MACHINE=RB3125
VERSION=SABLE
BINTYPE=BO

DEVSRC= device/sable_3125dev.c
DEVOBJ= sable_3125dev.o
LIB=	libpsa.a

CCCOPTS= -DRB3125
CASOPTS= -DRB3125

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(RB3125_ID)$(RB3125_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
