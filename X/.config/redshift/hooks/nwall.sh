#!/bin/bash

case $1 in
    period-changed)
	case $3 in
	    night)
		~/bin/nwall 0.0
		;;
	    transition)
		~/bin/nwall 0.4
		;;
	    daytime)
		~/bin/nwall 0.6
		;;
	esac
	;;
esac

