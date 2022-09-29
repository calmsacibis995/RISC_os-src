#ident "$Header: Makefile,v 1.4.2.1 90/07/18 16:20:27 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M1000/SABLE/PSAIO_LG/Makefile
#

MACHINE=M1000
VERSION=SABLE
BINTYPE=LG

DEVSRC= device/sable_2300dev.c
DEVOBJ= sable_2300dev.o
LIB=	libpsa.a

CCOPTS= -g $(PROMOPTS)
ASOPTS= -g $(PROMOPTS)
ENDIAN= -EL
TYPE=$(VERSION) PROM DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R2300_ID)$(M1000_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
