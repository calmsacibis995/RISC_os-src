#ident "$Header: Makefile,v 1.28 90/10/24 10:30:55 huang Exp $"
#
# Makefile -- Makefile to build complete standalone system
#
include	Mkinc.release

SHELL= /bin/sh
MAKE= /bin/make
CAT=/bin/cat

MKARGS=	MAKE=$(MAKE) $(MAKEARGS)

# components that comprise the SPP release
SA_COMPONENTS= saio diags unixcmds prom dbgmon stand 

# commands, and standalones included in the UMIPS release
UMIPS_COMPONENTS= saio/SAIO prom/SASH stand unixcmds dbgmon

# sources needed to make a set of proms
PROM_COMPONENTS= unixcmds saio diags prom

# sources needed to make a sash
SASH_COMPONENTS= saio prom

# sources needed to make a format
FORMAT_COMPONENTS= saio stand

# include files that are installed in the SPP tree
INCLUDE_COMPONENTS= saio prom 

# libraries that are install in the SPP tree
LIBRARY_COMPONENTS= saio

# standalone programs that are installed in the SPP tree
ALONE_COMPONENTS= prom stand dbgmon

# UMIPS commands that are installed in the SPP tree
COMMAND_COMPONENTS= unixcmds 

# man pages that are installed in the SPP tree
MAN_COMPONENTS= unixcmds

# components that are never installed, but just built
BUILD_COMPONENTS= diags

# bom files (bill of material)
BOMS= boms
SPP_MASTER_BOM= spp.boms
PROM_SRC_MASTER_BOM= prom_src_master.boms
RISCOS_SA_MASTER_BOM= riscos_sa_master.boms

#
# BUILD TARGETS : these targets build the components of SPP
#

# Mkinc.common_targets : default & package installation  targets
include	Mkinc.common_targets


# all     : build everything in the SPP
# depend  : build depend lists for everything in the SPP
# remake  : build everything in the SPP
# clean   : remove .o files for everything in the SPP
# clobber : remove all object files for everything in the SPP
# tags    : build a ctags file for everything in the SPP
# print   : print the source files for everything in the SPP
all depend remake clean clobber tags print: links
	@for i in $(SA_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo SPP make $@ done


# umips : build the files that are included in the UMIPS release
install_umips: mips links install_umips_depend
	@for i in $(UMIPS_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done

install_umips_depend:
	@for i in $(UMIPS_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) depend DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done

# proms : build a set of proms
proms: 
	@for i in $(PROM_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done


# sash        : build a sash
sash:
	@for i in $(SASH_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done


# format : standalone disk format utility
format:
	@for i in $(FORMAT_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done


# libs  : build the stand alone libraries
libs: 
	@for i in $(LIBRARY_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) all $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo library make done

# alone : build the stand alone programs
alone: 
	@for i in $(ALONE_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) all $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make done

# unixmcds : build all the unix level commands
cmds: 
	@for i in $(COMMAND_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) all DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo unix command make done

#
# INSTALL TARGETS : these targets install into the SPP tree
#

# install : install everythhing into the SPP tree
install: all install_include install_libs install_alone install_cmds install_man

# install_include : install the include files in the SPP tree
install_include: 
	@for i in $(INCLUDE_COMPONENTS); \
	do \
		echo XXXXXX Install for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) install DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo include install done


# install_libs : install the libraries in the SPP tree
install_libs: libs
	@for i in $(LIBRARY_COMPONENTS); \
	do \
		echo XXXXXX Install for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) install DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo library install done

# install_alone : install the standalone programs in the SPP tree
install_alone: alone
	@for i in $(ALONE_COMPONENTS); \
	do \
		echo XXXXXX Install for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) install DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone install done

# install_cmds : install the commands in the SPP tree
install_cmds: cmds
	@for i in $(COMMAND_COMPONENTS); \
	do \
		echo XXXXXX Install for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) install DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo command install done

# install_man : install the man pages in the SPP tree
install_man: 
	@for i in $(MAN_COMPONENTS); \
	do \
		echo XXXXXX Install for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) install DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo man install done

# reinstall : target to clobber, rebuild, and install in one command
reinstall: remake install


#
# CLEAN TARGETS : these targets allow one to "clean" and "clobber" a component
#

# remove the umips installed components
clean_umips clobber_umips: mips
	@for i in $(UMIPS_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done

# remove the prom objects
clean_proms clobber_proms: 
	@for i in $(PROM_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done


# remove the sash objects
clean_sash clobber_sash:
	@for i in $(SASH_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make $@ done


# remove the stand alone io library oobjects
clean_lib: 
	@for i in $(LIBRARY_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clean $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo library make done

clobber_lib: 
	@for i in $(LIBRARY_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clobber $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo library make done

# remove the stand alone program objects
clean_alone : 
	@for i in $(ALONE_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clean $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make done

clobber_alone: 
	@for i in $(ALONE_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clobber $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo standalone make done

# remove the unix commands
clean_cmds: 
	@for i in $(COMMAND_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clean DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo unix command make done

clobber_cmds: 
	@for i in $(COMMAND_COMPONENTS); \
	do \
		echo XXXXXX make for $$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) clobber DESTDIR=$(DESTROOT) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo unix command make done



#
# HOUSEKEEPING TARGETS : these make life easier by insuring that things work
#

# links : make links to include files, so builds work as expected
links:
	-cd include; if [ ! -d dbgmon ]; then \
		ln -s ../dbgmon dbgmon; fi
	-cd include; if [ ! -d prom ]; then \
		ln -s ../prom prom; fi
	-cd include; if [ ! -d saio ]; then \
		ln -s ../saio saio; fi
	-cd include; if [ ! -d machine ]; then \
		ln -s mips machine; fi
	-cd include; if [ ! -f signal.h ]; then \
		ln -s sys/signal.h signal.h; fi
	-cd include; if [ ! -f ctype.h ]; then \
		ln -s saio/ctype.h ctype.h; fi

#
# The following targets are used internally at MIPS
#

#
# set-up makefiles to expect licensed source
#
mips:
	-rm -f Mkinc.common_defines
	ln -s Mkinc.common_defines_mips Mkinc.common_defines

#
# set-up makefiles to use *.O's for licensed sources
#
spp:
	-rm -f Mkinc.common_defines
	ln -s Mkinc.common_defines_spp Mkinc.common_defines

#
# build a release of prom 
#
prom_release:	install_prom_src_boms

#
# install the boms for a prom release
#
install_prom_src_boms:
	$(MAKE) BOM_FILES="`$(CAT) $(BOMS)/$(PROM_SRC_MASTER_BOM) | xargs`" really_install_prom_src_boms

really_install_prom_src_boms:
	-if [ ! -d $(DESTROOT)/usr/pkg/lib/PROM_Source$(PROM_SRC_RELEASE)/boms ]; \
	then \
	    mkdir -p $(DESTROOT)/usr/pkg/lib/PROM_Source$(PROM_SRC_RELEASE)/boms; \
	fi
	for i in $(BOM_FILES) ; \
	do \
	    if [ -f $(BOMS)/$$i ]; \
	    then \
	        $(MAKE) package_r_install SRC=$(BOMS)/$$i DEST=$(DESTROOT)/usr/pkg/lib/PROM_Source$(PROM_SRC_RELEASE)/boms; \
	    fi \
	done

#
# build a release of RISCOS satnd alone
#
riscos_release:	install_riscos_sa_boms

#
# install the boms for a prom release
#
install_riscos_sa_boms:
	$(MAKE) BOM_FILES="`$(CAT) $(BOMS)/$(RISCOS_SA_MASTER_BOM) | xargs`" really_install_riscos_sa_boms

really_install_riscos_sa_boms:
	-if [ ! -d $(DESTROOT)/usr/pkg/lib/RISCOS_StandAlone$(RISCOS_SA_RELEASE)/boms ]; \
	then \
	    mkdir -p $(DESTROOT)/usr/pkg/lib/RISCOS_StandAlone$(RISCOS_SA_RELEASE)/boms; \
	fi
	for i in $(BOM_FILES) ; \
	do \
	    if [ -f $(BOMS)/$$i ]; \
	    then \
	        $(MAKE) package_r_install SRC=$(BOMS)/$$i DEST=$(DESTROOT)/usr/pkg/lib/RISCOS_StandAlone$(RISCOS_SA_RELEASE)/boms; \
	    fi \
	done

#
# build a release of spp 
#
spp_release:	install spp_install_boms

#
# install the boms for a release
#
spp_install_boms:
	$(MAKE) BOM_FILES="`$(CAT) $(BOMS)/$(SPP_MASTER_BOM) | xargs`" really_spp_install_boms

really_spp_install_boms:
	-if [ ! -d $(DESTROOT)/usr/pkg/lib/SPP$(SPP_RELEASE)/boms ]; \
	then \
	    mkdir -p $(DESTROOT)/usr/pkg/lib/SPP$(SPP_RELEASE)/boms; \
	fi
	for i in $(BOM_FILES) ; \
	do \
	    if [ -f $(BOMS)/$$i ]; \
	    then \
	        $(MAKE) package_r_install SRC=$(BOMS)/$$i DEST=$(DESTROOT)/usr/pkg/lib/SPP$(SPP_RELEASE)/boms; \
	    fi \
	done
