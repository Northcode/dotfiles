#!/bin/bash

case $1 in
    period-changed)
	case $3 in
	    night)
		emacsclient -e "(enable-theme 'foggy-night)"
		;;
	    day)
		emacsclient -e "(disable-theme 'foggy-night)"
		;;
	esac
	;;
esac

