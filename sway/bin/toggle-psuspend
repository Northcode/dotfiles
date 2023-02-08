#!/bin/bash

pid=$1

state=$(ps -o state= "$pid")

psignal () {
    pids=$(pstree $2 -npl | grep -oP '(?<=\()[0-9]+(?=\))')
    kill -s $1 $pids
}

psuspend () {
    psignal SIGSTOP $1
}

pcont () {
    psignal SIGCONT $1
}


if [ "$state" = "T" ]; then
    pcont $pid
else
    psuspend $pid
fi
