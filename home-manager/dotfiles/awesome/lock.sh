#!/usr/bin/env sh

BLANK="#24273a"
CLEAR='#ffffff22'
DEFAULT="#a5adcb"
TEXT="#cdd6f4"
WRONG="#ed8796"
VERIFYING="#a6da95"

i3lock-color \
    --insidever-color=$CLEAR \
    --ringver-color=$VERIFYING \
    \
    --insidewrong-color=$CLEAR \
    --ringwrong-color=$WRONG \
    \
    --inside-color=$BLANK \
    --ring-color=$DEFAULT \
    --line-color=$BLANK \
    --separator-color=$DEFAULT \
    \
    --verif-color=$TEXT \
    --wrong-color=$TEXT \
    --time-color=$TEXT \
    --date-color=$TEXT \
    --layout-color=$TEXT \
    --keyhl-color=$VERIFYING \
    --bshl-color=$WRONG \
    \
    --screen 1 \
    --blur 5 \
    --clock \
    --indicator \
    --time-str='%I:%M %P' \
    --time-size=48 \
    --date-str="%A, %m-%d" \
    --date-size=24 \
    --radius 150 \
    --ring-width 12
