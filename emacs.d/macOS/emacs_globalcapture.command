#! /usr/bin/env sh

$(which emacsclient) -s $(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f 8) -c -ne "(make-capture-frame)" >/dev/null 2>&1 &

osascript -e 'tell application "Terminal" to close (every window whose name contains "emacs_globalcapture.command")' &
