#!/usr/bin/env bash

source ~/.borg-conf 

borg create -v --stats --compression lz4 \
     $REPOSITORY::'{hostname}-{now:%Y-%m-%d@%H:%M}' \
     "$HOME/" \
     "/data/Music" \
     --exclude "$HOME/.cache/" \
     --exclude "$HOME/.ccache/" \
     --exclude "$HOME/.local/share/" \
     --exclude "$HOME/Downloads/" \
     --exclude "$HOME/lost+found" \
     --exclude "$HOME/.wineprefixes/" \
     --exclude "$HOME/.steam/" \
     --exclude "$HOME/Android/" \
     --exclude "$HOME/.gradle/" \
     --exclude "$HOME/.android/" \
     --exclude "$HOME/.AndroidStudio3.1/" \
     --exclude "$HOME/.AndroidStudio3.0/" \
     --exclude "$HOME/.AndroidStudioPreview3.2/" \
     --exclude "$HOME/.virtualenvs/" \
     --exclude "$HOME/.tor-browser/" \
     --exclude "$HOME/.cargo/"

2>&1

if [ "$?" = "1" ]
then
    export BORG_PASSPHRASE=""
    exit 1
fi

borg prune -v $REPOSITORY --prefix '{hostname}-' \
     --keep-hourly=6 \
     --keep-daily=7 \
     --keep-weekly=4 \
     --keep-monthly=6

borg list $REPOSITORY

export BORG_PASSPHRASE=""
exit 0
