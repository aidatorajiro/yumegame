#!/bin/bash

function dae () {
    setsid $@ >/dev/null 2>&1 < /dev/null &
}

sh ./start_ide_ghcup.sh
code .
dae firefox --ProfileManager
xfce4-terminal --command='sh ./start_watch_ghcup.sh' --title='YUME develop: console A'
xfce4-terminal --command='blender' --title='YUME develop: console B'

cat startup.py | xclip -selection clipboard
sleep 5

# 0: Debug / Sub Code
wmctrl -r "YUME develop: console A" -t 0
wmctrl -r "YUME develop: console B" -t 0
wmctrl -r "yumegame - Visual Studio Code" -t 0
# 1: Blender
wmctrl -r "- Blender " -t 1
wmctrl -r "- Blender " -b add,fullscreen
# 2: Main Code
wmctrl -r "yumegamehs - Visual Studio Code" -t 2
wmctrl -r "yumegamehs - Visual Studio Code" -b add,fullscreen
# 3: Browse
wmctrl -r "Firefox" -t 3
