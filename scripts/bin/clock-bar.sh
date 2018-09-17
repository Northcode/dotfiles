#!/bin/bash

FG='#9999A0'
BG='#CC111111'
# FONT='-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*'
FONT='xft:Hack:pixelsize=14:antialias=true'

BAR_X=0
if [ "$(hostname)" == "andreas-d0" ]
then
    BAR_X=1920 # custom offset for 3 screens
fi

while true
do
    clock=$(date +"%H:%M")
    date=$(date +"%d.%m")

    unread_mails=$(mu find 'flag:unread AND NOT maildir:/northcode/Junk' 2> /dev/null | wc -l)

    vpn_active=$(systemctl is-active openvpn-client@Norway.service)
    
    echo '%{r}' "vpn: $vpn_active | unread email: $unread_mails | $clock | $date  "
    sleep 10
done | lemonbar -g 1920x25+$BAR_X+1055 -f $FONT -B $BG -F $FG
