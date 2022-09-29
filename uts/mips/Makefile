#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.81.1.21.1.3.1.2 90/10/16 09:57:26 beacker Exp $
#ident	"$Header: Makefile,v 1.81.1.21.1.3.1.2 90/10/16 09:57:26 beacker Exp $"

#
# Makefile to make the kernel
#
# By default, "$ make" builds all possible kernels.
# "$ make install" copies them all to $DESTROOT.
#
# People interested in building only one kernel might consider
# changing the default dependancy to unix.whatever.
#

DEPTH=.
include $(DEPTH)/kcommondefs

ETCDIR= /etc

UNICES_1= unix.r2300_std unix.r2400_std unix.r3200_std unix.i2000_std \
	  unix.r3200_ijc unix.r3030_std unix.rb3125_std
UNICES_2= unix.r6000_std
UNICES= $(UNICES_1) $(UNICES_2)

UNICES_1_SABLE=	unix.r2300_sable unix.r2400_sable unix.r3200_sable \
		unix.r3030_sable
UNICES_2_SABLE=	unix.r6000_sable
UNICES_SABLE=$(UNICES_1_SABLE) $(UNICES_2_SABLE)

EXTRA_UNICES_1= unix.local unix.r2300_local unix.r2400_local unix.r3200_local \
		unix.r3200_ijc_local unix.i2000_local unix.r3030_local
EXTRA_UNICES_2= unix.r6000_local
EXTRA_UNICES= $(EXTRA_UNICES_1) $(EXTRA_UNICES_2)

ALL_UNICES=$(UNICES_1) $(UNICES_2)

default: $(ALL_UNICES)

mark_i:	$(UNICES_1)
mark_ii: $(UNICES_2)
mark_i_sable: $(UNICES_1_SABLE)
mark_ii_sable: $(UNICES_2_SABLE)

all: $(ALL_UNICES)

MARKS= mark_i mark_ii mark_i_sable mark_ii_sable
MAKEMARK= $(MAKE) -f Makefile.mark

WHEREIGO=$(DESTROOT)/usr/include

HEADERDIRS= \
	bsd \
	bsd/mips \
	bsd/net \
	bsd/netinet \
	bsd/netns \
	bsd/sys \
	bsd43 \
	bsd43/mips \
	bsd43/mipsif \
	bsd43/mipsvme \
	bsd43/net \
	bsd43/netimp \
	bsd43/netinet \
	bsd43/netns \
	bsd43/nfs \
	bsd43/prom \
	bsd43/saio \
	bsd43/sys \
	bsd43/ufs \
	posix \
	posix/sys \
	sys \
	sys/fs

HEADERFILES= \
	bsd/mips/if_enp.h \
	bsd/net/af.h \
	bsd/net/if.h \
	bsd/net/if_arp.h \
	bsd/net/netisr.h \
	bsd/net/raw_cb.h \
	bsd/net/route.h \
	bsd/net/soioctl.h \
	bsd/netinet/icmp_var.h \
	bsd/netinet/if_ether.h \
	bsd/netinet/in.h \
	bsd/netinet/in_pcb.h \
	bsd/netinet/in_systm.h \
	bsd/netinet/in_var.h \
	bsd/netinet/ip.h \
	bsd/netinet/ip_icmp.h \
	bsd/netinet/ip_var.h \
	bsd/netinet/tcp.h \
	bsd/netinet/tcp_debug.h \
	bsd/netinet/tcp_fsm.h \
	bsd/netinet/tcp_seq.h \
	bsd/netinet/tcp_timer.h \
	bsd/netinet/tcp_var.h \
	bsd/netinet/tcpip.h \
	bsd/netinet/udp.h \
	bsd/netinet/udp_var.h \
	bsd/netns/idp.h \
	bsd/netns/idp_var.h \
	bsd/netns/ns.h \
	bsd/netns/ns_error.h \
	bsd/netns/ns_if.h \
	bsd/netns/ns_pcb.h \
	bsd/netns/sp.h \
	bsd/netns/spidp.h \
	bsd/netns/spp_debug.h \
	bsd/netns/spp_var.h \
	bsd/sys/dir.h \
	bsd/sys/domain.h \
	bsd/sys/file.h \
	bsd/sys/ioctl.h \
	bsd/sys/mbuf.h \
	bsd/sys/msgbuf.h \
	bsd/sys/param.h \
	bsd/sys/protosw.h \
	bsd/sys/resource.h \
	bsd/sys/signal.h \
	bsd/sys/socket.h \
	bsd/sys/socketvar.h \
	bsd/sys/time.h \
	bsd/sys/timeb.h \
	bsd/sys/ttychars.h \
	bsd/sys/types.h \
	bsd/sys/uio.h \
	bsd/sys/wait.h \
	bsd43/mips/debug.h \
	bsd43/mips/dkio.h \
	bsd43/mips/dvh.h \
	bsd43/mips/entrypt.h \
	bsd43/mips/hwconf.h \
	bsd43/mips/i8254clock.h \
	bsd43/mips/machparam.h \
	bsd43/mips/pcb.h \
	bsd43/mips/ps_data.h \
	bsd43/mips/psl.h \
	bsd43/mips/pte.h \
	bsd43/mips/s2681cons.h \
	bsd43/mips/sableclock.h \
	bsd43/mips/sablecons.h \
	bsd43/mips/softfp.h \
	bsd43/mips/vmparam.h \
	bsd43/mipsif/if_enp.h \
	bsd43/mipsvme/cpreg.h \
	bsd43/mipsvme/dkipreg.h \
	bsd43/mipsvme/sdreg.h \
	bsd43/mipsvme/tsreg.h \
	bsd43/mipsvme/tthreg.h \
	bsd43/mipsvme/vmereg.h \
	bsd43/mipsvme/vmevar.h \
	bsd43/netimp/if_imp.h \
	bsd43/netimp/if_imphost.h \
	bsd43/prom/entrypt.h \
	bsd43/prom/prom.h \
	bsd43/saio/arp.h \
	bsd43/saio/bfs.h \
	bsd43/saio/ctype.h \
	bsd43/saio/debug.h \
	bsd43/saio/ei.h \
	bsd43/saio/mbuf.h \
	bsd43/saio/ncp.h \
	bsd43/saio/parser.h \
	bsd43/saio/protocol.h \
	bsd43/saio/protoio.h \
	bsd43/saio/saio.h \
	bsd43/saio/saioctl.h \
	bsd43/saio/setjmp.h \
	bsd43/saio/socket.h \
	bsd43/saio/stringlist.h \
	bsd43/saio/tpd.h \
	bsd43/saio/undef.h \
	bsd43/sys/acct.h \
	bsd43/sys/bkmac.h \
	bsd43/sys/bootconf.h \
	bsd43/sys/buf.h \
	bsd43/sys/callout.h \
	bsd43/sys/clist.h \
	bsd43/sys/cmap.h \
	bsd43/sys/conf.h \
	bsd43/sys/des.h \
	bsd43/sys/dir.h \
	bsd43/sys/dk.h \
	bsd43/sys/dkbad.h \
	bsd43/sys/dirent.h \
	bsd43/sys/dmap.h \
	bsd43/sys/domain.h \
	bsd43/sys/errno.h \
	bsd43/sys/exec.h \
	bsd43/sys/fcntl.h \
	bsd43/sys/file.h \
	bsd43/sys/fixpoint.h \
	bsd43/sys/gprof.h \
	bsd43/sys/ioctl.h \
	bsd43/sys/kernel.h \
	bsd43/sys/kstamp.h \
	bsd43/sys/map.h \
	bsd43/sys/mman.h \
	bsd43/sys/mount.h \
	bsd43/sys/msgbuf.h \
	bsd43/sys/mtio.h \
	bsd43/sys/namei.h \
	bsd43/sys/param.h \
	bsd43/sys/pathname.h \
	bsd43/sys/proc.h \
	bsd43/sys/prof.h \
	bsd43/sys/protosw.h \
	bsd43/sys/ptrace.h \
	bsd43/sys/quota.h \
	bsd43/sys/reboot.h \
	bsd43/sys/resource.h \
	bsd43/sys/seg.h \
	bsd43/sys/signal.h \
	bsd43/sys/socket.h \
	bsd43/sys/stat.h \
	bsd43/sys/syscall.h \
	bsd43/sys/syslog.h \
	bsd43/sys/sysmips.h \
	bsd43/sys/systm.h \
	bsd43/sys/tablet.h \
	bsd43/sys/text.h \
	bsd43/sys/time.h \
	bsd43/sys/timeb.h \
	bsd43/sys/times.h \
	bsd43/sys/trace.h \
	bsd43/sys/tty.h \
	bsd43/sys/ttychars.h \
	bsd43/sys/ttydev.h \
	bsd43/sys/types.h \
	bsd43/sys/ucred.h \
	bsd43/sys/uio.h \
	bsd43/sys/un.h \
	bsd43/sys/unpcb.h \
	bsd43/sys/user.h \
	bsd43/sys/utsname.h \
	bsd43/sys/vadvise.h \
	bsd43/sys/vcmd.h \
	bsd43/sys/vfs.h \
	bsd43/sys/vfs_stat.h \
	bsd43/sys/vlimit.h \
	bsd43/sys/vm.h \
	bsd43/sys/vmmac.h \
	bsd43/sys/vmmeter.h \
	bsd43/sys/vmparam.h \
	bsd43/sys/vmsystm.h \
	bsd43/sys/vnode.h \
	bsd43/sys/vsio.h \
	bsd43/sys/vtimes.h \
	bsd43/sys/wait.h \
	bsd43/ufs/fs.h \
	bsd43/ufs/fsdir.h \
	bsd43/ufs/inode.h \
	bsd43/ufs/mount.h \
	bsd43/ufs/quota.h \
	posix/sys/asm.h \
	posix/sys/dirent.h \
	posix/sys/errno.h \
	posix/sys/fpu.h \
	posix/sys/limits.h \
	posix/sys/regdef.h \
	posix/sys/signal.h \
	posix/sys/softfp.h \
	posix/sys/stat.h \
	posix/sys/syscall.h \
	posix/sys/sysmips.h \
	posix/sys/termios.h \
	posix/sys/times.h \
	posix/sys/types.h \
	posix/sys/utsname.h \
	posix/sys/wait.h \
	sys/acct.h \
	sys/adaptec_scsi.h \
	sys/adv.h \
	sys/am9516.h \
	sys/asm.h \
	sys/async.h \
	sys/bc.h \
	sys/bitmap.h \
	sys/bitmasks.h \
	sys/boot.h \
	sys/boothdr.h \
	sys/bsd_glue.h \
	sys/buf.h \
	sys/buzzer.h \
	sys/cachectl.h \
	sys/callo.h \
	sys/cdsioreg.h \
	sys/cirmgr.h \
	sys/clock.h \
	sys/cmn_err.h \
	sys/comm.h \
	sys/conf.h \
	sys/cpreg.h \
	sys/cpu_board.h \
	sys/crtctl.h \
	sys/ctlspace.h \
	sys/ctype.h \
	sys/debug.h \
	sys/dir.h \
	sys/dirent.h \
	sys/dkinreg.h \
	sys/dkio.h \
	sys/dkipdebug.h \
	sys/dkipreg.h \
	sys/dkvj_IPtypes.h \
	sys/dkvj_reg.h \
	sys/dkvj_scsi.h \
	sys/dkvj_struct.h \
	sys/dnlc.h \
	sys/du_dep.h \
	sys/dump.h \
	sys/dvh.h \
	sys/edt.h \
	sys/elog.h \
	sys/erec.h \
	sys/err.h \
	sys/errno.h \
	sys/fblk.h \
	sys/fcntl.h \
	sys/fd8272.h \
	sys/file.h \
	sys/filsys.h \
	sys/firmware.h \
	sys/fixpoint.h \
	sys/flio.h \
	sys/flock.h \
	sys/fpu.h \
	sys/fs/bfs_bio.h \
	sys/fs/bfs_dir.h \
	sys/fs/bfs_fs.h \
	sys/fs/bfs_inode.h \
	sys/fs/bfs_magic.h \
	sys/fs/bfs_param.h \
	sys/fs/bfs_yech.h \
	sys/fs/com_inode.h \
	sys/fs/com_pncc.h \
	sys/fs/cs5_dir.h \
	sys/fs/fifo.h \
	sys/fs/nfs.h \
	sys/fs/nfs_clnt.h \
	sys/fs/nfs_compat.h \
	sys/fs/nfs_export.h \
	sys/fs/nfs_rnode.h \
	sys/fs/nfs_stat.h \
	sys/fs/pipe_inode.h \
	sys/fs/prfcntl.h \
	sys/fs/proc_fs.h \
	sys/fs/rnode.h \
	sys/fs/s5dir.h \
	sys/fs/s5fblk.h \
	sys/fs/s5filsys.h \
	sys/fs/s5inode.h \
	sys/fs/s5macros.h \
	sys/fs/s5param.h \
	sys/fs/snode.h \
	sys/fs/unp_fs_inode.h \
	sys/fs/ufs_fs.h \
	sys/fs/ufs_fsdir.h \
	sys/fs/ufs_inode.h \
	sys/fs/ufs_mount.h \
	sys/fs/ufs_quota.h \
	sys/fsid.h \
	sys/fstyp.h \
	sys/gdpstr.h \
	sys/gen_ioctl.h \
	sys/getpages.h \
	sys/grafreg.h \
	sys/hd146818.h \
	sys/hdeioctl.h \
	sys/hdelog.h \
	sys/hetero.h \
	sys/i8042.h \
	sys/i8254clock.h \
	sys/idtab.h \
	sys/if_IPtypes.h \
	sys/if_egl.h \
	sys/if_laiocb.h \
	sys/if_lareg.h \
	sys/ik_ioctl.h \
	sys/ikcreg.h \
	sys/immu.h \
	sys/immu_r6000.h \
	sys/ino.h \
	sys/inode.h \
	sys/inst.h \
	sys/ioa.h \
	sys/iobuf.h \
	sys/ioctl.h \
	sys/iop.h \
	sys/iop_clk.h \
	sys/iopvar.h \
	sys/ipc.h \
	sys/kbd_ioctl.h \
	sys/kbd.h \
	sys/keyboard.h \
	sys/keymap.h \
	sys/kmem.h \
	sys/ladefs.h \
	sys/lastruct.h \
	sys/lihdr.h \
	sys/limits.h \
	sys/lock.h \
	sys/log.h \
	sys/lpreg.h \
	sys/m12scsi.h \
	sys/m120scsi.h \
	sys/map.h \
	sys/mb87030.h \
	sys/mem_board.h \
	sys/message.h \
	sys/mipskopt.h \
	sys/mk48t02.h \
	sys/mman.h \
	sys/mount.h \
	sys/mouse.h \
	sys/mouse_dc.h \
	sys/mousereg.h \
	sys/msg.h \
	sys/mtio.h \
	sys/nami.h \
	sys/ncr53c94.h \
	sys/netid.h \
	sys/netman.h \
	sys/nserve.h \
	sys/numips.h \
	sys/nvram.h \
	sys/open.h \
	sys/param.h \
	sys/pcb.h \
	sys/pfdat.h \
	sys/poll.h \
	sys/proc.h \
	sys/psw.h \
	sys/ptrace.h \
	sys/pty_ioctl.h \
	sys/que.h \
	sys/queue.h \
	sys/r2300mem.h \
	sys/r2350reg.h \
	sys/r3030scsi.h \
	sys/r3200mem.h \
	sys/r3250reg.h \
	sys/rambo.h \
	sys/rdebug.h \
	sys/recover.h \
	sys/reg.h \
	sys/regdef.h \
	sys/region.h \
	sys/rfsys.h \
	sys/sablecons.h \
	sys/sabledsk.h \
	sys/sbd.h \
	sys/scc.h \
	sys/sccsid.h \
	sys/scsi.h \
	sys/sem.h \
	sys/sema.h \
	sys/sg.h \
	sys/sgs.h \
	sys/sha_errs.h \
	sys/shareg.h \
	sys/shm.h \
	sys/signal.h \
	sys/slip.h \
	sys/socket.h \
	sys/softfp.h \
	sys/ss.h \
	sys/stat.h \
	sys/statfs.h \
	sys/stermio.h \
	sys/stream.h \
	sys/strids.h \
	sys/strlog.h \
	sys/stropts.h \
	sys/strstat.h \
	sys/stty_ld.h \
	sys/swap.h \
	sys/syscall.h \
	sys/sysinfo.h \
	sys/sysmacros.h \
	sys/sysmips.h \
	sys/systm.h \
	sys/tcon.h \
	sys/teDevice.h \
	sys/teFont.h \
	sys/teState.h \
	sys/termio.h \
	sys/tihdr.h \
	sys/times.h \
	sys/timod.h \
	sys/tiuser.h \
	sys/tod.h \
	sys/trace.h \
	sys/tsreg.h \
	sys/tthreg.h \
	sys/tuneable.h \
	sys/types.h \
	sys/uadmin.h \
	sys/uart_ioctl.h \
	sys/uartreg.h \
	sys/ucred.h \
	sys/uio.h \
	sys/user.h \
	sys/wait.h \
	sys/ustat.h \
	sys/utsname.h \
	sys/var.h \
	sys/vcmd.h \
	sys/vfs.h \
	sys/vfs_stat.h \
	sys/vmereg.h \
	sys/vmevar.h \
	sys/vnode.h \
	sys/vtoc.h \
	sys/xm_ioctl.h \
	sys/xmreg.h \
	sys/xt.h \
	sys/xtproto.h \
	sys/xyl712reg.h

OPTHEADERFILES= \
	sys/vdisk.h

BSDSOCKETHEADERFILES= \
	bsd/socket/un.h \
	bsd/socket/unpcb.h

BSDENETHEADERFILES= \
	bsd/enet/enet.h

BSDHEADERFILES= \
	sys/mbuf.h \
	sys/socketvar.h \
	net/af.h \
	net/if.h \
	net/if_arp.h \
	net/netisr.h \
	net/raw_cb.h \
	net/route.h \
	netinet/icmp_var.h \
	netinet/if_ether.h \
	netinet/in.h \
	netinet/in_pcb.h \
	netinet/in_systm.h \
	netinet/in_var.h \
	netinet/ip.h \
	netinet/ip_icmp.h \
	netinet/ip_var.h \
	netinet/tcp.h \
	netinet/tcp_debug.h \
	netinet/tcp_fsm.h \
	netinet/tcp_seq.h \
	netinet/tcp_timer.h \
	netinet/tcp_var.h \
	netinet/tcpip.h \
	netinet/udp.h \
	netinet/udp_var.h \
	netns/idp.h \
	netns/idp_var.h \
	netns/ns.h \
	netns/ns_error.h \
	netns/ns_if.h \
	netns/ns_pcb.h \
	netns/sp.h \
	netns/spidp.h \
	netns/spp_debug.h \
	netns/spp_var.h 

FSHEADERFILES= \
	nfs/nfs.h \
	nfs/nfs_clnt.h

SYSHEADERFILES= \
	mips/asm.h \
	mips/cachectl.h \
	mips/cpu_board.h \
	mips/fpu.h \
	mips/reg.h \
	mips/regdef.h \
	mips/inst.h \
	sys/dnlc.h \
	sys/fd8272.h

TAGSSUBDIRS= os fs/ufs fs/proc fs/nfs4 fs/specfs io \
	bsd/mips bsd/net bsd/netinet bsd/netns bsd/sys ml \
	bsd43/glue posix/glue klib sys \
	bsd43/mips bsd43/mipsif bsd43/mipsvme bsd43/netimp \
	bsd43/prom bsd43/sys \
	bsd43/ufs bsd43/saio posix/sys \
	../../head ../../bsd_head ../../posix_head \
	vdisk x25 decnet

TAGSFILES= \
	bsd/tcp-param.h \
	bsd/socket/sys_socket.c \
	bsd/socket/uipc_domain.c \
	bsd/socket/uipc_pipe.c \
	bsd/socket/uipc_proto.c \
	bsd/socket/uipc_socket.c \
	bsd/socket/uipc_socket2.c \
	bsd/socket/uipc_syscalls.c \
	bsd/socket/uipc_usrreq.c \
	bsd/socket/uipc_43.c \
	bsd/socket/unpcb.h \
	bsd/socket/imp.h \
	bsd/socket/un.h
CTAGS=/usr/new/ctags
ETAGS=/usr/new/etags

clean generic depend rules:
	for i in $(MARKS) ; do \
		mk=`echo $$i | sed -e 's/mark_//'` ; \
		[ -d "bootarea_"$$mk ] || mkdir "bootarea_"$$mk ; \
		echo "$(MAKEMARK) MARK="$$mk" $@ ;" ; \
		$(MAKEMARK) MARK=$$mk $@ ; \
	done

rules-mark:
	$(MAKE) $(MAKEARGS) rules
	touch rules-mark

install-unix: all
	@for i in $(ALL_UNICES); \
	do \
		echo 'ROOT="" $(INSTALL) $(INSTOPTS) $$i $(DESTROOT)/;'; \
		$(INSTALL) $(INSTOPTS) $$i $(DESTROOT)/; \
	done

clobber:
	for i in $(MARKS) ; do \
		mk=`echo $$i | sed -e 's/mark_//'` ; \
		[ -d "bootarea_"$$mk ] || mkdir "bootarea_"$$mk ; \
		echo "$(MAKEMARK) MARK="$$mk" $@ ;" ; \
		$(MAKEMARK) MARK=$$mk $@ ; \
	done
	rm -rf $(UNICES) $(EXTRA_UNICES) $(UNICES_SABLE)
	rm -rf link-mark rules-mark

install: install-headers install-unix

install-headers:
	ID=`id +%U` ; \
	[ -r $(WHEREIGO) ] || \
		( ( mkdir -p $(WHEREIGO) || exit 0) ; \
		  [ $$ID != 0 ] || \
		  	( chgrp $(GROUP) $(WHEREIGO) ; \
			  chmod 0775 $(WHEREIGO) ; \
			  chown $(OWNER) $(WHEREIGO) ) ) ; \
	for i in $(HEADERDIRS) ;  \
	{ \
		[ -d $(WHEREIGO)/$$i ] || \
		    ( mkdir -p $(WHEREIGO)/$$i ; \
		      [ $$ID != 0 ] || \
		      ( chmod 0775 $(WHEREIGO)/$$i ; \
			chgrp $(GROUP) $(WHEREIGO)/$$i ; \
			chown $(OWNER) $(WHEREIGO)/$$i ) ) ; \
	} ; 
	for file in $(HEADERFILES) ; \
	{\
		DIRN=`dirname $$file` ; \
		FILEN=`basename $$file` ; \
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/$$DIRN ; \
	} ;
	for file in $(OPTHEADERFILES) ; \
	{\
		if [ -r "$$file" ] ; \
		then \
			DIRN=`dirname $$file` ; \
			FILEN=`basename $$file` ; \
			$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/$$DIRN ; \
		fi ; \
	} ;
	for file in $(BSDSOCKETHEADERFILES) ; \
	{\
		DIRN=`dirname $$file` ; \
		FILEN=`basename $$file` ; \
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/bsd/sys ; \
	} ;
	for file in $(BSDENETHEADERFILES) ; \
	{\
		if [ -r "$$file" ] ; \
		then \
			DIRN=`dirname $$file` ; \
			FILEN=`basename $$file` ; \
			$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/bsd/net ; \
		fi ; \
	} ;
	rm -rf $(WHEREIGO)/bsd/machine ; \
	ln -s mips $(WHEREIGO)/bsd/machine ; \
	for file in $(BSDHEADERFILES) ; \
	{\
		rm -rf $(WHEREIGO)/bsd43/$$file ; \
		ln -s ../sysv/bsd/$$file $(WHEREIGO)/bsd43/$$file ; \
	} ; \
	for file in $(FSHEADERFILES) ; \
	{\
		FILEN=`basename $$file` ; \
		rm -rf $(WHEREIGO)/bsd43/$$file ; \
		ln -s ../sysv/sys/fs/$$FILEN $(WHEREIGO)/bsd43/$$file ; \
	} ; \
	for file in $(SYSHEADERFILES) ; \
	{\
		FILEN=`basename $$file` ; \
		rm -rf $(WHEREIGO)/bsd43/$$file ; \
		ln -s ../sysv/sys/$$FILEN $(WHEREIGO)/bsd43/$$file ; \
	} ; \
	rm -rf $(WHEREIGO)/bsd43/nfs/rnode.h ; \
	ln -s ../sysv/sys/fs/nfs_rnode.h $(WHEREIGO)/bsd43/nfs/rnode.h ; \
	rm -rf $(WHEREIGO)/bsd43/nfs/export.h ; \
	ln -s ../sysv/sys/fs/nfs_export.h $(WHEREIGO)/bsd43/nfs/export.h ; \
	rm -rf $(WHEREIGO)/bsd43/machine ; \
	ln -s mips $(WHEREIGO)/bsd43/machine ; 
	rm -rf $(WHEREIGO)/bsd43/sys/regdef.h ; \
	ln -s ../mips/regdef.h $(WHEREIGO)/bsd43/sys/regdef.h ; \
	rm -rf $(WHEREIGO)/bsd43/mips/cpu.h ; \
	ln -s ../sysv/sys/sbd.h $(WHEREIGO)/bsd43/mips/cpu.h ; \
	rm -rf $(WHEREIGO)/bsd43/mips/r2350mem.h ; \
	ln -s ../sysv/sys/r2300mem.h $(WHEREIGO)/bsd43/mips/r2350mem.h ; \
	rm -rf $(WHEREIGO)/sys/mbuf.h ; \
	ln -s ../bsd/sys/mbuf.h $(WHEREIGO)/sys/mbuf.h ;
	if [ -d x25 ] && [ -r x25/Makefile ] ; \
	then \
		echo "(cd x25 ; $(MAKE) $(MAKEARGS) install-headers ) ; " ; \
		(cd x25 ; $(MAKE) $(MAKEARGS) install-headers ) ; \
	fi

 

$(UNICES) $(EXTRA_UNICES) $(UNICES_SABLE): rules-mark FRC

$(UNICES_1) $(EXTRA_UNICES_1): objects_i
	X=`expr $@ : "unix.\(.*\)"`; \
	$(MAKEMARK) MARK=i $(MAKEARGS) unix_X X=$$X

$(UNICES_1_SABLE): objects_i_sable
	X=`expr $@ : "unix.\(.*\)"`; \
	$(MAKEMARK) MARK=i_sable $(MAKEARGS) unix_X X=$$X

$(UNICES_2) $(EXTRA_UNICES_2): objects_ii
	X=`expr $@ : "unix.\(.*\)"`; \
	$(MAKEMARK) MARK=ii $(MAKEARGS) unix_X X=$$X

$(UNICES_2_SABLE): objects_ii_sable
	X=`expr $@ : "unix.\(.*\)"`; \
	$(MAKEMARK) MARK=ii_sable $(MAKEARGS) unix_X X=$$X

objects_i objects_i_sable objects_ii objects_ii_sable: FRC
	X=`expr $@ : "objects_\(.*\)"`; \
	$(MAKEMARK) MARK=$$X $(MAKEARGS) $(BUILD_TYPE) 

header-links:
	[ -d bootarea_i ] || mkdir bootarea_i ; \
	$(MAKEMARK) MARK=i $(MAKEARGS) $@

sys-header-links:
	if [ $(BUILD_TYPE)x = sourcex ] ; \
	then \
		for file in $(SYSHEADERFILES) ; \
		{\
			FILEN=`basename $$file` ; \
			rm -rf bsd43/$$file ; \
			ln -s ../../sys/$$FILEN bsd43/$$file ; \
		} ; \
	fi ;


FRC:

tags: header-links
	-rm -f tags
	touch tags
	for i in $(TAGSSUBDIRS) ; \
	do \
		find $$i \( -name \*.c -o -name \*.h \) -print | \
			xargs -n100 -t ctags -a ; \
	done
	( [ -r $(BOOTAREA)/master.r2300_std.c ] && \
		$(CTAGS) -a $(BOOTAREA)/master.r2300_std.c ) || true
	$(CTAGS) -a ${TAGSFILES} || true
	mv tags tag.nosort
	sort tag.nosort > tags
	rm -f tag.nosort

TAGS: header-links
	-rm -f TAGS
	touch TAGS
	for i in $(TAGSSUBDIRS) ; \
	do \
		find $$i \( -name \*.c -o -name \*.h \) -print | \
			xargs -n100 -t $(ETAGS) -a ; \
	done
	( [ -r $(BOOTAREA)/master.r2300_std.c  ] && \
		$(ETAGS) -a $(BOOTAREA)/master.r2300_std.c ) || true
	$(ETAGS) -a ${TAGSFILES} || true


