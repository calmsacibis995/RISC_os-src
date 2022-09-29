# root's csh settings

# "$Header: .cshrc,v 1.1.2.2 90/05/03 14:30:43 wje Exp $"

umask 022

# stuff for interactive shells

if ($?prompt) then
	set history = 100
	alias ls 'ls -C'
	alias ll 'ls -l'
	alias h history
endif

# stuff for all shells

set path = (/usr/lbin /usr/ucb /usr/bin /bin /etc /usr/new /usr/local/bin )
