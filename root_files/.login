# root's csh login file
# "$Header: .login,v 1.1.2.4 90/05/03 14:31:15 wje Exp $"

setenv	SHELL	/bin/csh

stty line 1 erase '^H' kill '^U' intr '^C' echoe

if (!($?TERM)) then
# IF board is I2000 & there is video & this shell is on /dev/console
# & /dev/console is the graphics tube THEN we are running the ansi
# emulator. We want to set the TERM variable to ansi and make it
# readonly.
	switch (`/etc/hwconf | grep 'Cpu Board' | sed 's/MIPS \(.*\) Cpu.*/\1/'`)
	case *I2000*:
		set st1=0
		/etc/hwconf | grep 'video board present' >& /dev/null
		set st2=$status
		breaksw
	case *3030:
		set st1=0
		/etc/hwconf | grep 'display enabled' >& /dev/null
		set st2=$status
		breaksw
	default:
		set st1=1
		set st2=1
	endsw
        set st3=`ls -l /dev/console | sed 's/.* 37, .*/37/'`
	switch (`tty`)
	case /dev/console:
	case /dev/ttykeybd:
	case /dev/syscon:
	case /dev/systty:
		set st4=0
		breaksw
	default:
		set st4=1
	endsw
        if ($st1 == 0 && $st2 == 0 && "$st3" == 37 && $st4 == 0) then
		set Nterm=ansi
	else
		echo -n "TERM=(dumb) "
		set Nterm = "$<"
		if ("$Nterm" == "") then
			set Nterm = "dumb"
		endif
	endif
	set term=$Nterm
	setenv TERM "$Nterm"
	unset Nterm st1 st2 st3 st4
endif

#
# The prompt is set to contain the hostname.  If there is no hostname
# set, the default is taken from the default hostname file.  If there
# is none there, the default is made quite obvious.
#

set Hostname = ""
if ( -x /usr/net/hostname ) then
	set Hostname = `/usr/net/hostname`
endif

if ( "$Hostname" == "") then
	if (-f /etc/local_hostname) then
		set Hostname = `sed -n '1p' /etc/local_hostname`
	else
		set Hostname = "No default hostname"
	endif
endif

set prompt = "$Hostname [\!]# "
unset Hostname
