#ident "$Header: Makefile,v 1.5.2.1 90/07/18 15:46:41 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../M1000/SABLE/PROM_LO/Makefile for prom monitor
#

MACHINE=M1000
VERSION=SABLE
BINTYPE=LO

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON=../../../../diags/pon/$(VERSION)_PON_$(BINTYPE)/libpon.a

CCOPTS=	-O
ASOPTS=
ENDIAN=	-EL
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE= $(MAJOR_REV_ID).$(R2300_ID)$(M1000_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
