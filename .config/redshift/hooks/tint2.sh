#!/bin/bash

case $1 in
    period-changed)
	case $3 in
	    night)
		cd ~/.config/tint2/
		ln -sf dark tint2rc
		pkill -USR1 tint2
		;;
	    day)
		cd ~/.config/tint2/
		ln -sf light tint2rc
		pkill -USR1 tint2
		;;
	esac
	;;
esac

