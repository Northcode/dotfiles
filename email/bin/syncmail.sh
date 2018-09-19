#!/bin/bash
davmail -server > /tmp/davmail.log 2>&1 &
sleep 1
mbsync -aV
kill %1
mu index -m ~/.mail

UNREADCOUNT=$(mu find 'flag:unread AND NOT maildir:/northcode/Junk' | wc -l)

if [ $UNREADCOUNT -gt 0 ]; then
    notify-send "New mail!" "$UNREADCOUNT new emails synced!"
fi
