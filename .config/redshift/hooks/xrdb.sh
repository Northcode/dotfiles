#!/bin/bash

case $1 in
    period-changed)
	case $3 in
	    night)
		cd ~/.config/X/
		ln -sf themes/spacegray colors
		xrdb resources
		;;
	    day)
		cd ~/.config/X/
		ln -sf themes/tango colors
		xrdb resources
		;;
	esac
	;;
esac

