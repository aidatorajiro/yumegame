#!/bin/python3

# Arch Linux: 2024-03-21
# linux: 6.8.1.arch1-1
# util-linux: 2.39.3-2
# wmctrl: 1.07-6
# xclip: 0.13-4
# xfce4-session: 4.18.3-1
# xfce4-terminal: 1.1.3-1
# blender: 17:4.0.2-21
# firefox: 124.0-1

import subprocess
import os
import time

# base path
base_path = os.path.abspath(os.path.dirname(__file__))
os.chdir(base_path)

# settings
TERMINAL = "xfce4-terminal"
WAIT_SECONDS_APPS_OPEN = 10
WAIT_SECONDS_WINDOW_MOVE = 0.1
DESKTOP_CONFIG = [[0, 27, 1910, 1016], [1920, 0, 1910, 1046]] # manually obtained x/y/w/h for a window that covers whole display (DESKTOP_CONFIG[0] will be of left display, DESKTOP_CONFIG[1] will be of right display)

def move_window_workspace(title, desktop, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-t", desktop])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def move_window_position(title, config, wait=True):
    [gravity, pos_x, pos_y, width, height] = config
    arg = ",".join([str(gravity), str(pos_x), str(pos_y), str(width), str(height)])
    subprocess.run(["wmctrl", "-r", title, "-e", arg])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)
    subprocess.run(["wmctrl", "-r", title, "-e", arg])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def make_fullscreen(title, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-b", "add,fullscreen"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_fullscreen(title, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-b", "remove,fullscreen"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def make_sticky(title, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-b", "add,sticky"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_sticky(title, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-b", "remove,sticky"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def calc_config(display_id, numgrids_x, numgrids_y, x, y, w, h):
    [_x, _y, _w, _h] = DESKTOP_CONFIG[display_id]
    grid_w = _w / numgrids_x
    grid_h = _h / numgrids_y
    ret_x = grid_w * x
    ret_y = grid_h * y
    ret_w = grid_w * w
    ret_h = grid_h * h
    return [0, round(_x + ret_x), round(_y + ret_y), round(ret_w), round(ret_h)]

def switch_desktop(s, wait=True):
    subprocess.run(["wmctrl", "-s", s])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_maximized(title, wait=True):
    subprocess.run(["wmctrl", "-r", title, "-b", "remove,maximized_vert"])
    subprocess.run(["wmctrl", "-r", title, "-b", "remove,maximized_horz"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def main_spawn_processes():
    # main ide
    subprocess.Popen(["sh", "start_ide_ghcup.sh"])

    # sub ide
    subprocess.Popen(["code" ,"."])

    # firefox, make sure the process is completely separated from the parent
    subprocess.Popen(["setsid" ,"firefox", "-P", "yumegame develop"], stdout=subprocess.DEVNULL, stdin=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # build log console
    subprocess.Popen([TERMINAL, "--command=sh ./start_watch_ghcup.sh", "--title=YUME develop: console A"])

    # runtime log console
    subprocess.Popen([TERMINAL, "--command=blender", "--title=YUME develop: console B"])

    # copy to clipboard
    with open('startup.py') as f:
        subprocess.run(["xclip", "-selection", "clipboard"], stdin=f)

    # wait for some seconds
    time.sleep(WAIT_SECONDS_APPS_OPEN)

def main_placement(single=False):
    # Workspace 0: Blender   TODO: a little bit of danger of misclassification
    desktop = '0'
    switch_desktop(desktop)

    title = "- Blender 4."
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    if single:
        move_window_position(title, calc_config(0, 1, 1, 0, 0, 1, 1))
    else:
        make_sticky(title)
        move_window_position(title, calc_config(1, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title)

    # Workspace 1: Debug / Sub Code
    desktop = '1'
    switch_desktop(desktop)

    title = "YUME develop: console A"
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    move_window_position(title, calc_config(0, 2, 2, 0, 0, 1, 1))

    title = "YUME develop: console B"
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    move_window_position(title, calc_config(0, 2, 2, 0, 1, 1, 1))

    title = "yumegame - Visual Studio Code"
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    move_window_position(title, calc_config(0, 2, 2, 1, 0, 1, 2))

    # Workspace 2: Main Code
    desktop = '2'
    switch_desktop(desktop)

    title = "yumegamehs - Visual Studio Code"
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    move_window_position(title, calc_config(0, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title)

    # Workspace 3: Browse   TODO: a little bit of danger of misclassification
    desktop = '3'
    switch_desktop(desktop)

    title = "Firefox"
    remove_fullscreen(title)
    remove_maximized(title)
    move_window_workspace(title, desktop)
    move_window_position(title, calc_config(0, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title)

    # Turns out that this can be done using add,sticky...
    """
    if not single:
        returncode = subprocess.run(["gcc", "-I/usr/include/glib-2.0", "-I/usr/lib/glib-2.0/include", "-lXmu", "-lX11", "-lglib-2.0", "display-switch.c", "-o", "display-switch.o"]).returncode

        subprocess.run(["killall", "display-switch.o"])
        subprocess.run(["killall", "display-switch.py"])
        
        desktop = '0'
        switch_desktop(desktop)
        title = "Display Switch Watch"
        remove_fullscreen(title)
        remove_maximized(title)
        move_window_workspace(title, desktop)
        move_window_position(title, calc_config(0, 1, 1, 0, 0, 1, 1))
        
        if returncode == 0:
            subprocess.run(["./display-switch.o"])
        else:
            subprocess.run(["./display-switch.py"])
    """