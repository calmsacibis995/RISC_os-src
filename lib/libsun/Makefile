#
# $Header: Makefile,v 1.13.1.10 90/05/09 14:53:50 wje Exp $
# ---------------------------------------------------
# | Copyright (c) 1987 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(GNUM) $(MAXOPT) $(INCLUDE) -DRISCOS -DLIBBSD $(ECFLAGS)
CFLAGS2 =$(GNUM) -O1 $(INCLUDE) -DRISCOS $(ECFLAGS)
ASFLAGS =$(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

BSD43RPCSRCS	=auth_des.c auth_none.c auth_unix.c authdes_prot.c \
	authunix_prot.c clnt_generic.c \
	clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c clnt_udp.c \
	get_myaddress.c key_call.c key_prot.c netname.c \
	openchild.c pmap_clnt.c pmap_getmaps.c pmap_getport.c \
	pmap_prot.c pmap_prot2.c pmap_rmt.c rpc_callmsg.c rpc_prot.c \
	rpccommondata.c rpcdtablesize.c rtime.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_run.c \
	svc_simple.c svc_tcp.c svc_udp.c svcauth_des.c \
	xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_reference.c xdr_stdio.c

BSD43RPCOBJS =$(BSD43RPCSRCS:.c=.o)

BSD43GENSRCS = fstab.c mntent.c

BSD43GENOBJS =$(BSD43GENSRCS:.c=.o)

BSD43NETSRCS = ether_addr.c gethostnamadr.c getnetbyaddr.c getnetbyname.c \
	getnetent.c getnetgrent.c getproto.c getprotoent.c getprotoname.c \
	getrpcent.c getservent.c innetgr.c rcmd.c res_comp.c res_debug.c \
	res_init.c res_mkquery.c res_query.c res_send.c sethostent.c vis.c \
	ruserpass.c

BSD43NETOBJS =$(BSD43NETSRCS:.c=.o)

BSD43NAMEDSRCS = dns_getnamadr.c dns_service.c dns_sethent.c

BSD43NAMEDOBJS =$(BSD43NAMEDSRCS:.c=.o)

BSD43HTSRCS = ht_getgrent.c ht_getgrgid.c ht_getgrnam.c ht_gethent.c \
	ht_getnamadr.c ht_getpent.c ht_getpname.c ht_getproto.c ht_getpwent.c \
	ht_gnbyaddr.c ht_gnbyname.c ht_gnetent.c ht_gngrent.c ht_grpcent.c \
	ht_gservent.c ht_innetgr.c ht_service.c

BSD43HTOBJS=$(BSD43HTSRCS:.c=.o)

BSD43YPSRCS = yp_all.c yp_bind.c yp_enum.c yp_master.c yp_match.c \
	yp_order.c yp_update.c yperr_string.c ypmaint_xdr.c ypprot_err.c \
	ypv1_xdr.c ypxdr.c ypupdate_xdr.c yp_getgrent.c yp_gethostent.c \
	yp_getpent.c yp_getpwent.c yp_getrpcent.c yp_getservent.c yp_gnetent.c \
	yp_gnetgr.c yp_innetgr.c yp_service.c

BSD43YPOBJS=$(BSD43YPSRCS:.c=.o)

LIBBSDSRCS = Ovfork.s flock.s

LIBBSDOBJS = $(LIBBSDSRCS:.s=.o)

BSD43YPHDRS = ypv1_prot.h ypclnt.h yp_prot.h

LCLSRCS = async_d.s exportfs.s getfh.s nfsmount.s nfssvc.s

LCLOBJS = $(LCLSRCS:.s=.o)

ALIASSRCS1 = \
	x_accept.s \
	x_ffs.s \
	x_getgroups.s \
	x_listen.s \
	x_setreuid.s \
	x_setsockopt.s \
	x_vfork.s \
	x_authdes_create.s \
	x_authdes_getucred.s \
	x_authnone_create.s \
	x_authunix_create.s \
	x_authunix_create_default.s \
	x_bcopy.s \
	x_bind.s \
	x_bindresvport.s \
	x_blkclr.s \
	x_bzero.s \
	x_callrpc.s \
	x_clnt_broadcast.s \
	x_clnt_create.s \
	x_clnt_pcreateerror.s \
	x_clnt_perrno.s \
	x_clnt_perror.s \
	x_clnt_spcreateerror.s \
	x_clnt_sperrno.s \
	x_clnt_sperror.s \
	x_clntraw_create.s \
	x_clnttcp_create.s \
	x_clntudp_bufcreate.s \
	x_clntudp_create.s \
	x_clntudp_destroy.s \
	x_closelog.s \
	x_connect.s \
	x_dbm_close.s \
	x_dbm_delete.s \
	x_dbm_fetch.s \
	x_dbm_firstkey.s \
	x_dbm_forder.s \
	x_dbm_nextkey.s \
	x_dbm_open.s \
	x_dbm_store.s \
	x_dn_comp.s \
	x_dn_expand.s \
	x_dn_skipname.s \
	x_dns_endhostent.s \
	x_dns_gethostbyaddr.s \
	x_dns_gethostbyname.s \
	x_dns_service.s \
	x_dns_sethostent.s \
	x_endhostent.s \
	x_endnetent.s \
	x_endnetgrent.s \
	x_endprotoent.s \
	x_endrpcent.s \
	x_endservent.s \
	x_ether_aton.s \
	x_ether_hostton.s \
	x_ether_line.s \
	x_ether_ntoa.s \
	x_ether_ntohost.s \
	x_flock.s \
	x_fp_query.s \
	x_get_myaddress.s \
	x_getdomainname.s \
	x_getdtablesize.s \
	x_gethostbyaddr.s \
	x_gethostbyname.s \
	x_gethostent.s \
	x_gethostname.s \
	x_getnetbyaddr.s \
	x_getnetbyname.s \
	x_getnetent.s \
	x_getnetgrent.s \
	x_getnetname.s \
	x_getprotobyname.s \
	x_getprotobynumber.s \
	x_getprotoent.s \
	x_getrpcbyname.s \
	x_getrpcbynumber.s \
	x_getrpcent.s \
	x_getservbyname.s \
	x_getservbyport.s \
	x_getservent.s \
	x_getsockname.s \
	x_gettimeofday.s \
	x_host2netname.s \
	x_hostalias.s \
	x_ht_endgrent.s \
	x_ht_endhostent.s \
	x_ht_endnetent.s \
	x_ht_endnetgrent.s \
	x_ht_endprotoent.s \
	x_ht_endpwent.s \
	x_ht_endrpcent.s \
	x_ht_endservent.s \
	x_ht_fgetgrent.s \
	x_ht_getgrent.s \
	x_ht_getgrgid.s \
	x_ht_getgrnam.s \
	x_ht_gethostbyaddr.s \
	x_ht_gethostbyname.s \
	x_ht_gethostent.s \
	x_ht_getnetbyaddr.s \
	x_ht_getnetbyname.s \
	x_ht_getnetent.s \
	x_ht_getnetgrent.s \
	x_ht_getprotobyname.s \
	x_ht_getprotobynumber.s \
	x_ht_getprotoent.s \
	x_ht_getpwent.s \
	x_ht_getpwnam.s \
	x_ht_getpwuid.s \
	x_ht_getrpcbyname.s \
	x_ht_getrpcbynumber.s \
	x_ht_getrpcent.s \
	x_ht_getservbyname.s \
	x_ht_getservbyport.s \
	x_ht_getservent.s \
	x_ht_innetgr.s \
	x_ht_service.s \
	x_ht_setgrent.s \
	x_ht_sethostent.s \
	x_ht_setnetent.s \
	x_ht_setnetgrent.s \
	x_ht_setprotoent.s \
	x_ht_setpwent.s \
	x_ht_setrpcent.s \
	x_ht_setservent.s \
	x_htonl.s \
	x_htons.s \
	x_index.s \
	x_inet_addr.s \
	x_inet_makeaddr.s \
	x_inet_network.s \
	x_inet_ntoa.s \
	x_innetgr.s \
	x_key_decryptsession.s \
	x_key_encryptsession.s \
	x_key_gendes.s \
	x_key_setsecret.s \
	x_netname2host.s \
	x_netname2user.s \
	x_no_service.s \
	x_ntohl.s \
	x_ntohs.s \
	x_openlog.s \
	x_ovbcopy.s \
	x_p_cdname.s \
	x_p_class.s \
	x_p_query.s \
	x_p_rr.s \
	x_p_type.s \
	x_pmap_getmaps.s \
	x_pmap_getport.s \
	x_pmap_rmtcall.s \
	x_pmap_set.s \
	x_pmap_unset.s \
	x_putlong.s \
	x_putshort.s \
	x_rcmd.s \
	x_recv.s \
	x_recvfrom.s \
	x_registerrpc.s \
	x_res_init.s \
	x_res_mkquery.s \
	x_res_query.s \
	x_res_querydomain.s \
	x_res_search.s \
	x_res_send.s \
	x_rindex.s \
	x_rpc_perror.s \
	x_rpc_perror1.s \
	x_rpc_perror2.s \
	x_rresvport.s

ALIASSRCS2 = \
	x_rtime.s \
	x_ruserok.s \
	x_select.s \
	x_send.s \
	x_sendto.s \
	x_setgrfile.s \
	x_setgrjunk.s \
	x_sethostent.s \
	x_sethostfile.s \
	x_setlogmask.s \
	x_setnetent.s \
	x_setnetgrent.s \
	x_setprotoent.s \
	x_setpwfile.s \
	x_setrpcent.s \
	x_setservent.s \
	x_socket.s \
	x_strcasecmp.s \
	x_strncasecmp.s \
	x_svc_getreq.s \
	x_svc_getreqset.s \
	x_svc_register.s \
	x_svc_run.s \
	x_svc_sendreply.s \
	x_svc_unregister.s \
	x_svcerr_auth.s \
	x_svcerr_decode.s \
	x_svcerr_noproc.s \
	x_svcerr_noprog.s \
	x_svcerr_progvers.s \
	x_svcerr_systemerr.s \
	x_svcerr_weakauth.s \
	x_svcfd_create.s \
	x_svcraw_create.s \
	x_svctcp_create.s \
	x_svcudp_bufcreate.s \
	x_svcudp_create.s \
	x_svcudp_enablecache.s \
	x_syslog.s \
	x_user2netname.s \
	x_usingypmap.s \
	x_v1prot_dofirst.s \
	x_v1prot_donext.s \
	x_vis_nextserv.s \
	x_xdr_accepted_reply.s \
	x_xdr_array.s \
	x_xdr_authdes_cred.s \
	x_xdr_authdes_verf.s \
	x_xdr_authunix_parms.s \
	x_xdr_bool.s \
	x_xdr_bytes.s \
	x_xdr_callhdr.s \
	x_xdr_callmsg.s \
	x_xdr_char.s \
	x_xdr_cryptkeyarg.s \
	x_xdr_cryptkeyres.s \
	x_xdr_datum.s \
	x_xdr_des_block.s \
	x_xdr_deskey.s \
	x_xdr_double.s \
	x_xdr_enum.s \
	x_xdr_float.s \
	x_xdr_free.s \
	x_xdr_getcredres.s \
	x_xdr_int.s \
	x_xdr_keybuf.s \
	x_xdr_keystatus.s \
	x_xdr_long.s \
	x_xdr_netnamestr.s \
	x_xdr_netobj.s \
	x_xdr_opaque.s \
	x_xdr_opaque_auth.s \
	x_xdr_pmap.s \
	x_xdr_pmaplist.s \
	x_xdr_pointer.s \
	x_xdr_reference.s \
	x_xdr_rejected_reply.s \
	x_xdr_replymsg.s \
	x_xdr_rmtcall_args.s \
	x_xdr_rmtcallres.s \
	x_xdr_short.s \
	x_xdr_string.s \
	x_xdr_u_char.s \
	x_xdr_u_int.s \
	x_xdr_u_long.s \
	x_xdr_u_short.s \
	x_xdr_union.s \
	x_xdr_unixcred.s \
	x_xdr_vector.s \
	x_xdr_void.s \
	x_xdr_wrapstring.s \
	x_xdr_yp_binding.s \
	x_xdr_yp_buf.s \
	x_xdr_yp_inaddr.s \
	x_xdr_ypall.s \
	x_xdr_ypbind_resp.s \
	x_xdr_ypbind_setdom.s \
	x_xdr_ypdelete_args.s \
	x_xdr_ypdomain_wrap_string.s \
	x_xdr_ypmap_parms.s \
	x_xdr_ypmap_wrap_string.s \
	x_xdr_ypmaplist.s \
	x_xdr_ypmaplist_wrap_string.s \
	x_xdr_ypowner_wrap_string.s \
	x_xdr_yppushresp_xfr.s \
	x_xdr_ypreq_key.s \
	x_xdr_ypreq_nokey.s \
	x_xdr_ypreq_xfr.s \
	x_xdr_ypresp_key_val.s \
	x_xdr_ypresp_maplist.s \
	x_xdr_ypresp_master.s \
	x_xdr_ypresp_order.s \
	x_xdr_ypresp_val.s \
	x_xdr_ypupdate_args.s \
	x_xdrmem_create.s \
	x_xdrrec_create.s \
	x_xdrrec_endofrecord.s \
	x_xdrrec_eof.s \
	x_xdrrec_skiprecord.s \
	x_xdrstdio_create.s \
	x_xprt_register.s \
	x_xprt_unregister.s \
	x_yp_all.s \
	x_yp_bind.s \
	x_yp_endgrent.s \
	x_yp_endhostent.s \
	x_yp_endnetent.s \
	x_yp_endnetgrent.s \
	x_yp_endprotoent.s \
	x_yp_endpwent.s \
	x_yp_endrpcent.s \
	x_yp_endservent.s \
	x_yp_fgetgrent.s \
	x_yp_first.s \
	x_yp_get_default_domain.s \
	x_yp_getgrent.s \
	x_yp_getgrgid.s \
	x_yp_getgrnam.s \
	x_yp_gethostbyaddr.s \
	x_yp_gethostbyname.s \
	x_yp_gethostent.s \
	x_yp_getnetbyaddr.s \
	x_yp_getnetbyname.s \
	x_yp_getnetent.s \
	x_yp_getnetgrent.s \
	x_yp_getprotobyname.s \
	x_yp_getprotobynumber.s \
	x_yp_getprotoent.s \
	x_yp_getpwent.s \
	x_yp_getpwnam.s \
	x_yp_getpwuid.s \
	x_yp_getrpcbyname.s \
	x_yp_getrpcbynumber.s \
	x_yp_getrpcent.s \
	x_yp_getservbyname.s \
	x_yp_getservbyport.s \
	x_yp_getservent.s \
	x_yp_innetgr.s \
	x_yp_master.s \
	x_yp_match.s \
	x_yp_next.s \
	x_yp_order.s \
	x_yp_service.s \
	x_yp_setgrent.s \
	x_yp_sethostent.s \
	x_yp_sethostfile.s \
	x_yp_setnetent.s \
	x_yp_setnetgrent.s \
	x_yp_setprotoent.s \
	x_yp_setpwent.s \
	x_yp_setpwfile.s \
	x_yp_setrpcent.s \
	x_yp_setservent.s \
	x_yp_unbind.s \
	x_yp_update.s \
	x_yperr_string.s \
	x_ypprot_err.s

ALIASOBJS1 =$(ALIASSRCS1:.s=.o)

ALIASOBJS2 =$(ALIASSRCS2:.s=.o)

ALIASSRCS = $(ALIASSRCS1) $(ALIASSRCS2)

ALIASOBJS = $(ALIASOBJS1) $(ALIASOBJS2)

REMOTESRCS = \
	$(BSD43RPCSRCS) \
	$(BSD43GENSRCS) \
	$(BSD43NETSRCS) \
	$(BSD43NAMEDSRCS) \
	$(BSD43HTSRCS) \
	$(BSD43YPSRCS) \
	$(LIBBSDSRCS)

REMOTEOBJS = \
	$(BSD43RPCOBJS) \
	$(BSD43GENOBJS) \
	$(BSD43NETOBJS) \
	$(BSD43NAMEDOBJS) \
	$(BSD43HTOBJS) \
	$(BSD43YPOBJS) \
	$(LIBBSDOBJS) 

BASESRCS = $(LCLSRCS) $(REMOTESRCS) 

SRCS = $(BASESRCS) $(ALIASSRCS)

BASEOBJS = $(LCLOBJS) $(REMOTEOBJS)

OBJS = $(BASEOBJS) $(ALIASOBJS)

HDRS	=

LIBSUFFIX=
WHOAMI	=libsun$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) Makefile
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(BASEOBJS)
	LIST= ; N=0 ; \
	for file in $(ALIASOBJS) ; \
	{ \
		N=` expr $$N + 1 ` ; \
		rm -f x_$$N.o ; \
		ln $$file x_$$N.o ; \
		LIST="$$LIST x_$$N.o" ; \
	} ; \
	$(AR) $(ARFLAGS) $(WHOAMI) $$LIST 

clean:
	-rm -f $(OBJS)
	-rm -f $(REMOTESRCS) 
	-rm -f $(ALIASSRCS)
	-rm -f x_[0-9][0-9]*.o x_[0-9].o

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(ALIASSRCS):
	[ ! -r $@ ] || rm -f $@
	NAME=`echo $@ | sed -e 's/\.s$$//' -e 's/^x_//' ` ; \
		echo '#include	<sys/regdef.h>' > $@ ; \
		echo '#include	<sys/asm.h>' >> $@ ; \
		echo "LEAF($$NAME)" >> $@ ; \
		echo '	j	__'"$$NAME" >> $@ ; \
		echo "END($$NAME)" >> $@

$(BSD43RPCSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/rpc/$@"' >> $@

$(BSD43NETSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/net/$@"' >> $@

$(BSD43NAMEDSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/named/$@"' >> $@

$(BSD43HTSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/hosttable/$@"' >> $@

$(BSD43GENSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/gen/$@"' >> $@

$(BSD43YPSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../../bsd_lib/libc/yp/$@"' >> $@

$(BSD43YPOBJS):  
	$(CC) $(CFLAGS) $(ECFLAGS) -I../../bsd_lib/libc/yp -c $<

$(LIBBSDSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "../libc/include/name_mapping.h"' > $@
	echo '#include "../libbsd/$@"' >> $@

$(LCLOBJS): 
	echo '#include "../libc/include/name_mapping.h"' > tmp.s
	echo '#include "$<"' >> tmp.s
	$(CC) $(CFLAGS) $(ECFLAGS) -c -o $@ tmp.s
	@rm -f tmp.s

