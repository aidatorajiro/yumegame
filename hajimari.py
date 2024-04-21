#!/bin/python3
"""
Arch Linux: 2024-04-13

pacman -Q | rg "^(linux|util-linux|wmctrl|xclip|xfce4-session|xfce4-terminal|blender|firefox|obs-studio)"

blender 17:4.1.0-3
firefox 124.0.2-1
linux 6.8.5.arch1-1
linux-api-headers 6.7-1
linux-firmware 20240409.1addd7dc-1
linux-firmware-whence 20240409.1addd7dc-1
linux-headers 6.8.5.arch1-1
obs-studio 30.1.1-1
util-linux 2.40-2
util-linux-libs 2.40-2
wmctrl 1.07-6
xclip 0.13-4
xfce4-session 4.18.3-1
xfce4-terminal 1.1.3-1
"""

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

def class_option(classname):
    if classname:
        return ["-x"]
    else:
        return []

def move_window_workspace(title, classname, desktop, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-t", desktop])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def move_window_position(title, classname, config, wait=True):
    [gravity, pos_x, pos_y, width, height] = config
    arg = ",".join([str(gravity), str(pos_x), str(pos_y), str(width), str(height)])
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-e", arg])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-e", arg])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def make_fullscreen(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "add,fullscreen"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def make_above(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "add,above"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_above(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "remove,above"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_fullscreen(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "remove,fullscreen"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def make_sticky(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "add,sticky"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def remove_sticky(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "remove,sticky"])
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

def remove_maximized(title, classname, wait=True):
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "remove,maximized_vert"])
    subprocess.run(["wmctrl"] + class_option(classname) + ["-r", title, "-b", "remove,maximized_horz"])
    if wait:
        time.sleep(WAIT_SECONDS_WINDOW_MOVE)

def main_spawn_processes(single=False):
    os.chdir(base_path)

    # main ide
    subprocess.Popen(["setsid", "obs"], stdout=subprocess.DEVNULL, stdin=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # main ide
    subprocess.Popen(["sh", "start_ide_ghcup.sh"])

    # sub ide
    subprocess.Popen(["code" ,"."])

    # firefox, make sure the process is completely separated from the parent
    subprocess.Popen(["setsid", "firefox", "-P", "yumegame develop"], stdout=subprocess.DEVNULL, stdin=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # build log console
    envcopy = os.environ.copy()
    if single:
        envcopy["DEV_MODE"] = "SINGLE"
    else:
        envcopy["DEV_MODE"] = "MULTI"
    subprocess.Popen([TERMINAL, "--command=sh ./start_watch_ghcup.sh", "--title=YUME develop: console A"], env=envcopy)

    # runtime log console
    subprocess.Popen([TERMINAL, "--command=blender main-develop.blend -P startup.py", "--title=YUME develop: console B"])

    # copy to clipboard
    # with open('startup.py') as f:
    #     subprocess.run(["xclip", "-selection", "clipboard"], stdin=f)

    # wait for some seconds
    time.sleep(WAIT_SECONDS_APPS_OPEN)

def place_blender(desktop, single):
    title = "Blender.Blender"
    classname = True
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 1, 1, 0, 0, 1, 1))
    else:
        make_sticky(title, classname)
        move_window_position(title, classname, calc_config(0, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title, classname)

    # wmctrl -x -r yumegamehs-exe.yumegamehs-exe -b add,above
    title = "yumegamehs-exe.yumegamehs-exe"
    classname = True
    move_window_workspace(title, classname, desktop)
    if not single:
        make_sticky(title, classname)
    make_above(title, classname)

def place_console_a(desktop, single):
    title = "YUME develop: console A"
    classname = False
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 2, 2, 0, 0, 1, 1))
    else:
        move_window_position(title, classname, calc_config(1, 2, 2, 0, 0, 1, 1))

def place_console_b(desktop, single):
    title = "YUME develop: console B"
    classname = False
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 2, 2, 0, 1, 1, 1))
    else:
        move_window_position(title, classname, calc_config(1, 2, 2, 0, 1, 1, 1))

def place_code_devenv(desktop, single):
    title = "yumegame - Visual Studio Code"
    classname = False
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 2, 2, 1, 0, 1, 2))
    else:
        move_window_position(title, classname, calc_config(1, 2, 2, 1, 0, 1, 2))

def place_code_hs(desktop, single):
    title = "yumegamehs - Visual Studio Code"
    classname = False
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 1, 1, 0, 0, 1, 1))
    else:
        move_window_position(title, classname, calc_config(1, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title, classname)

def place_ff(desktop, single):
    title = "Navigator.firefox"
    classname = True
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 1, 1, 0, 0, 1, 1))
    else:
        move_window_position(title, classname, calc_config(1, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title, classname)

def place_obs(desktop, single):
    title = "obs.obs"
    classname = True
    remove_fullscreen(title, classname)
    remove_maximized(title, classname)
    move_window_workspace(title, classname, desktop)
    if single:
        move_window_position(title, classname, calc_config(0, 1, 1, 0, 0, 1, 1))
    else:
        move_window_position(title, classname, calc_config(1, 1, 1, 0, 0, 1, 1))
    make_fullscreen(title, classname)

def main_placement(single=False):
    # Workspace 0: Blender   TODO: a little bit of danger of misclassification
    desktop = '0'
    switch_desktop(desktop)
    place_blender(desktop, single)

    # Workspace 1: Debug / Sub Code
    desktop = '1'
    switch_desktop(desktop)
    place_console_a(desktop, single)
    place_console_b(desktop, single)
    place_code_devenv(desktop, single)

    # Workspace 2: Main Code
    desktop = '2'
    switch_desktop(desktop)
    place_code_hs(desktop, single)

    # Workspace 3: Browse   TODO: a little bit of danger of misclassification
    desktop = '3'
    switch_desktop(desktop)
    place_ff(desktop, single)

    desktop = '4'
    switch_desktop(desktop)
    place_obs(desktop, single)

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