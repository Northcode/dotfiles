if test $XDG_VTNR = 1 -a $XDG_SESSION_TYPE = tty -a -z "$SWAYSOCK"
	set PATH $HOME/bin $PATH
	export PATH
	sway
end
