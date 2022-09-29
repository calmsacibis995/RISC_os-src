#
# $Header: Makefile,v 1.1 87/12/29 16:54:49 dce Exp $
# ---------------------------------------------------
# | Copyright (c) 1987 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh

SUBDIRS	=include a_spell compress hashcheck hashmake lists spell spellin \
	spellprog

all install clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}
