#!/bin/bash

# /etc/X11/xinit/xinitrc
#
# global xinitrc file, used by all X sessions started by xinit (startx)

# invoke global X session script
#. /etc/X11/Xsession

[ -f ~/.xprofile ] && source ~/.xprofile

exec bspwm
