if test "$XDG_VTNR" = '1' -a "$XDG_SESSION_TYPE" = "tty" -a -z "$SWAYSOCK"
	set PATH $HOME/bin $PATH
	set MOZ_ENABLE_WAYLAND 1
	export PATH
	export MOZ_ENABLE_WAYLAND
	sway
end


alias bt=bluetoothctl
alias pa=pulsemixer
alias k=kubectl
alias ec=emacsclient
