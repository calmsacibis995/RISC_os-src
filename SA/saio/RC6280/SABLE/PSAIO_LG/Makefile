#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:30:41 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/RC6280/SABLE/PSAIO_LG/Makefile
#

MACHINE=RC6280
VERSION=SABLE
BINTYPE=LG

DEVSRC= device/sable_6300dev.c
DEVOBJ= sable_6300dev.o
LIB=	libpsa.a

CCOPTS= -g $(PROMOPTS)
ASOPTS= -g $(PROMOPTS)
ENDIAN= -EL
TYPE=$(VERSION) PROM DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R6300_ID)$(RC6280_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
