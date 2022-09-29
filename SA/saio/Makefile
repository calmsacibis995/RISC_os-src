#ident "$Header: Makefile,v 1.15 90/10/10 15:57:34 huang Exp $"
#
# ..../saio/Makefile --- makefile to build saio
#

SHELL=/bin/sh
MAKE=/bin/make

include ../Mkinc.release
include ../Mkinc.common_defines

LIST	=	Makefile Mkinc.saio \
		M1000/Makefile \
		M1000/MIPS/Makefile \
		M1000/MIPS/PSAIO_BO/Makefile \
		M1000/MIPS/PSAIO_BG/Makefile \
		M1000/SABLE/Makefile \
		M1000/SABLE/PSAIO_BO/Makefile \
		M1000/SABLE/PSAIO_BG/Makefile \
		M1000/SABLE/PSAIO_LO/Makefile \
		M1000/SABLE/PSAIO_LG/Makefile \

		M120/Makefile \
		M120/MIPS/Makefile \
		M120/MIPS/PSAIO_BO/Makefile \
		M120/MIPS/PSAIO_BG/Makefile \
		M120/SABLE/Makefile \
		M120/SABLE/PSAIO_BO/Makefile \
		M120/SABLE/PSAIO_BG/Makefile \
		M120/SABLE/PSAIO_LO/Makefile \
		M120/SABLE/PSAIO_LG/Makefile \

		M2000/Makefile \
		M2000/MIPS/Makefile \
		M2000/MIPS/PSAIO_BO/Makefile \
		M2000/MIPS/PSAIO_BG/Makefile \
		M2000/SABLE/Makefile \
		M2000/SABLE/PSAIO_BO/Makefile \
		M2000/SABLE/PSAIO_BG/Makefile \
		M2000/SABLE/PSAIO_LO/Makefile \
		M2000/SABLE/PSAIO_LG/Makefile \

		RC6280/Makefile \
		RC6280/MIPS/Makefile \
		RC6280/MIPS/PSAIO_BO/Makefile \
		RC6280/MIPS/PSAIO_BG/Makefile \
		RC6280/SABLE/Makefile \
		RC6280/SABLE/PSAIO_BO/Makefile \
		RC6280/SABLE/PSAIO_BG/Makefile \
		RC6280/SABLE/PSAIO_LO/Makefile \
		RC6280/SABLE/PSAIO_LG/Makefile \

		Rx3230/Makefile \
		Rx3230/MIPS/Makefile \
		Rx3230/MIPS/PSAIO_BO/Makefile \
		Rx3230/MIPS/PSAIO_BG/Makefile \
		Rx3230/SABLE/Makefile \
		Rx3230/SABLE/PSAIO_BO/Makefile \
		Rx3230/SABLE/PSAIO_BG/Makefile \
		Rx3230/SABLE/PSAIO_LO/Makefile \
		Rx3230/SABLE/PSAIO_LG/Makefile \

		SAIO/Makefile \
		SAIO/MIPS/Makefile \
		SAIO/MIPS/SAIO_BO/Makefile \
		SAIO/MIPS/SAIO_BG/Makefile \
		SAIO/SABLE/Makefile \
		SAIO/SABLE/SAIO_BO/Makefile \
		SAIO/SABLE/SAIO_BG/Makefile \
		SAIO/SABLE/SAIO_LO/Makefile \
		SAIO/SABLE/SAIO_LG/Makefile \

		../include/mips/asm.h ../include/mips/i8254clock.h \
		../include/mips/sableclock.h ../include/mips/cpu.h \
		../include/mips/dvh.h ../include/mips/s2681cons.h \
		../include/mips/sablecons.h ../include/mipsif/if_enp.h \
		../include/mipsvme/dkipreg.h ../include/mipsvme/sdreg.h \
		../include/mipsvme/vmereg.h ../include/sys/param.h \
		arp.h bfs.h debug.h mbuf.h \
		ncp.h parser.h protocol.h protoio.h saio.h saioctl.h \
		setjmp.h socket.h stringlist.h csu.mips.s \
		faultasm.s initiate.s libasm.s prom_entry.s \
		../saio_bsd/arp.c ../saio_sysv/att52.c bfs.c \
		../saio_bsd/bsd42.c check_dbg.c cmc.c \
		dkip.c dvh.c exec.c exec_stub.c fault.c gba.c ioa.c \
		libc.c mbuf.c mem.c ncp.c parser.c pcons.c \
		protocol.c regdesc.c s2681cons.c sablecons.c saio.c \
		scc_cons.c asc_poll.c pdkis.c scsi_probe.c common_scsi.c \
		bitmap.c mono.c r3030_ansi.c r3030_font.c video_cons.c \
		sd.c socket.c stdio.c stringlist.c timer.c tpd.c tpqic.c \
		pkbd.c ggetchar.c keymap_r3030.c \
		nfs.c tftp.c errputs.c ../saio_bsd/udpip.c \

		device/mips_2300dev.c device/sable_2300dev.c \
		device/mips_2400dev.c device/sable_2400dev.c \
		device/mips_3200dev.c device/sable_3200dev.c \
		device/mips_6300dev.c device/sable_6300dev.c \
		device/mips_3030dev.c device/sable_3030dev.c \
		device/mips_alldev.c device/sable_alldev.c


default:	all

all remake:
		-if [ $@ = remake ]; \
		then \
			$(MAKE) tags $(MAKEARGS); \
		fi
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for saio/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done

		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) $@ $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi

		@echo saio make $@ done

proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for saio/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) all $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo saio make $@ done


clean_proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for saio/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) clean $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo saio make $@ done


clobber_proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for saio/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) clobber $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo saio make $@ done


install_umips clean_umips clobber_umips:
		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) $@ $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi
		@echo saio make $@ done

sash format:
		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) all $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi
		@echo saio make $@ done


clean_sash:
		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) clean $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi
		@echo saio make $@ done


clobber_sash:
		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) clobber $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi
		@echo saio make $@ done


install:
		@echo XXXXXX Make insatall for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) install DESTDIR=$(DESTDIR) $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi
		@echo saio make $@ done
#		cd MIPS_SAIO_O; $(MAKE) install DESTDIR=$(DESTDIR) $(MAKEARGS)
#		cd SABLE_SAIO_BO; $(MAKE) install DESTDIR=$(DESTDIR) $(MAKEARGS)

tags:		FRC
		-rm -f tags
		P=`pwd`; D=`basename $$P`; ctags ../$$D/*.[csh] >/dev/null 2>&1

clobber clean depend:
		-if [ $@ = clobber ]; \
		then \
			rm -f errs; \
		fi
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for saio/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done

		@echo XXXXXX Make $@ for saio/SAIO XXXXXX
		@if [ -d SAIO ]; \
		then \
			cd SAIO; $(MAKE) $@ $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SAIO does not exist: no action taken; \
		fi

		@echo saio make $@ done


print:
		$(IF_SYSV) \
			pr $(LIST) | lp; \
		$(ENDIF)
		$(IF_BSD) \
			lpr -p -J standalone -C library $(LIST); \
		$(ENDIF)
		@echo saio make print done

FRC:
