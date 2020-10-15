#! /usr/bin/env sh

/usr/local/bin/emacsclient -s $(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f 8) --no-wait "$1" >/dev/null 2>&1 &
