#!/usr/bin/env bash

emacsclient -n -a "" -e "(if (> (length (frame-list)) 1) 't)" | grep t &>/dev/null
if [ "$?" = "1" ]; then
    emacsclient -c -n -a "" "$@"
else
    if (( $# )); then
        emacsclient -n -a "" "$@"
    fi
fi
