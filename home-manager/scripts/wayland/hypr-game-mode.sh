#!/usr/bin/env bash

HYPRGAMEMODE=$(hyprctl getoption animations:enabled | awk 'NR==1{print $2}')
if [ "$HYPRGAMEMODE" = 1 ]; then
    hyprctl --batch "\
    keyword animations:enabled 0;\
    keyword decoration:shadow:enabled 0;\
    keyword decoration:blur:enabled 0;\
    keyword decoration:rounding 0"
    systemctl --user stop wlsunset.service
    exit
fi
systemctl --user start wlsunset.service
hyprctl reload
