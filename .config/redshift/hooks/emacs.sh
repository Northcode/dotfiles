#!/bin/bash

case $1 in
    period-changed)
	case $3 in
	    night)
		emacsclient -e "(load-theme 'spacegray)"
		;;
	    day)
		emacsclient -e "(load-theme 'tango-plus)"
		;;
	esac
	;;
esac

