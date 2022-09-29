#ident "$Header: Makefile,v 1.1.2.1 90/07/18 16:31:51 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/RB3125/SABLE/PSAIO_LG/Makefile
#

MACHINE=RB3125
VERSION=SABLE
BINTYPE=LG

DEVSRC= device/sable_3125dev.c
DEVOBJ= sable_3125dev.o
LIB=	libpsa.a

CCCOPTS= -DRB3125
CASOPTS= -DRB3125

CCOPTS= -g $(PROMOPTS)
ASOPTS= -g $(PROMOPTS)
ENDIAN= -EL
TYPE=$(VERSION) PROM DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(RB3125_ID)$(RB3125_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
