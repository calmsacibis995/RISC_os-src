#ident "$Header: Makefile,v 1.9 90/10/24 10:50:59 huang Exp $"
#
# ------------------------------------------------------------------
# | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights |
# | Reserved.  This software contains proprietary and confidential |
# | information of MIPS and its suppliers.  Use, disclosure or     |
# | reproduction is prohibited without the prior express written   |
# | consent of MIPS.                                               |
# ------------------------------------------------------------------
#

include	../Mkinc.common_defines

# SUBDIRS : the subdirectories that contain the unix commands
SUBDIRS = bfsd convert scripts sable cache2000 makepipe sdownload

# UMIPS_DIRS : the subdirectories that contain the commands shipped with umips
UMIPS_DIRS= bfsd

# PROM_DIRS : the commands that the proms are dependant on
PROM_DIRS= convert


all install generic clean clobber depend remake tags:
	@for i in $(SUBDIRS) ; \
	do \
	    if [ -d $$i ]; \
	    then \
	    	echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
	    	(cd $$i; $(MAKE) GENERIC="$(GENERIC)" DESTDIR=$(DESTDIR) $(MAKEARGS) $@;) \
	    fi \
	done

install_umips :
	@for i in $(UMIPS_DIRS) ; \
	do \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" DESTDIR=$(DESTDIR) $(MAKEARGS) $@;) \
	done

clean_umips :
	@for i in $(UMIPS_DIRS) ; \
	do \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" $(MAKEARGS) clean;) \
	done

clobber_umips :
	@for i in $(UMIPS_DIRS) ; \
	do \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" $(MAKEARGS) clobber;) \
	done

proms :
	@for i in $(PROM_DIRS) ; \
	do \
	    if [ -d $$i ]; \
	    then \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" DESTDIR=$(DESTDIR) $(MAKEARGS) all;) \
	    fi \
	done

clean_proms :
	@for i in $(PROM_DIRS) ; \
	do \
	    if [ -d $$i ]; \
	    then \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" $(MAKEARGS) clean;) \
	    fi \
	done

clobber_proms :
	@for i in $(PROM_DIRS) ; \
	do \
	    if [ -d $$i ]; \
	    then \
		echo "XXXXXX Make $@ for unixcmds/$$i XXXXXX"; \
		(cd $$i; $(MAKE) GENERIC="$(GENERIC)" $(MAKEARGS) clobber;) \
	    fi \
	done

