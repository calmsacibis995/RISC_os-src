# root's sh profile
# "$Header: .profile,v 1.1.2.4 90/05/03 14:32:11 wje Exp $"

umask 022

#
# Root's path should not contain '.', as this can cause security
# holes.
#
PATH=/usr/lbin:/bin:/etc:/usr/bin:/usr/ucb:/usr/new:/usr/local/bin:/usr/X11/bin
export PATH

stty line 1 erase '^H' kill '^U' intr '^C' echoe 

case "$TERM" in
"")
# IF board is I2000 or RS3230 & there is video & this shell is on /dev/console
# & /dev/console is the graphics tube THEN we are running the ansi
# emulator. We want to set the TERM variable to ansi and make it
# readonly.
	case `/etc/hwconf | grep 'Cpu Board' | sed 's/MIPS \(.*\) Cpu.*/\1/'` in
	*I2000*)
		st1=0
		/etc/hwconf | grep 'video board present' >/dev/null 2>&1
		st2=$?
		;;
	*3030)
		st1=0
		/etc/hwconf | grep 'display enabled' >/dev/null 2>&1
		st2=$?
		;;
	*)
		st1=1
		st2=1
		;;
	esac
	st3=`ls -l /dev/console | sed 's/.* 37, .*/37/'`
	case `tty` in
	/dev/console | /dev/ttykeybd | /dev/syscon | /dev/systty)
		st4=0
		;;
	*)
		st4=1
		;;
	esac
	if [ $st1 -eq 0 -a $st2 -eq 0 -a "$st3" -eq 37 -a $st4 -eq 0 ]
	then
		TERM=ansi
		readonly TERM
	else
		echo "TERM=(dumb) \c"
		read TERM
		TERM=${TERM:=dumb}
	fi
	;;
esac
unset st1 st2 st3 st4
export TERM

#
# The prompt is set to contain the hostname.  If there is no hostname
# set, the default is taken from the default hostname file.  If there
# is none there, the default is made quite obvious.
#

if [ -x /usr/net/hostname ]
then
	Hostname=`/usr/net/hostname 2>/dev/null`
fi

case "$Hostname" in
	"")
		Hostname=`sed -n '1p' /etc/local_hostname 2>&1`
		;;
esac

PS1="$Hostname # "
unset Hostname

#
# Useful functions
#
ls() {
	/bin/ls -C ${1+"$@"}
}

ll() {
	/bin/ls -l ${1+"$@"}
}

sd() {
	cd ${1+"$@"}
}

which() {
    type ${1+"$@"}
}
