#ident "$Header: Makefile,v 1.5.4.1 90/03/07 16:48:00 menna Exp $"
#
# $Header: Makefile,v 1.5.4.1 90/03/07 16:48:00 menna Exp $
# ------------------------------------------------------------------
# | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights |
# | Reserved.  This software contains proprietary and confidential |
# | information of MIPS and its suppliers.  Use, disclosure or     |
# | reproduction is prohibited without the prior express written   |
# | consent of MIPS.                                               |
# ------------------------------------------------------------------
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTDIR	The root of the destination directory.
#

include ../../Mkinc.common_defines
include ../../Mkinc.common_targets

WHEREIGO=$(DESTDIR)/usr/spp

SCRIPTS=sacc \
	saas \
	sald \
	saman

all clean clobber remake tags depend generic:

install:
	-if [ ! -d $(DESTDIR)/usr/spp ]; \
	then \
		$(IF_SYSV) \
			mkdir -p $(DESTDIR)/usr/spp; \
		$(ENDIF) \
		$(IF_BSD) \
			mkdir $(DESTDIR)/usr; \
			mkdir $(DESTDIR)/usr/spp; \
		$(ENDIF) \
	fi
	for i in $(SCRIPTS); \
	do \
		$(MAKE) package_x_install SRC=$$i DEST=$(WHEREIGO); \
	done

