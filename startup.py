# Please copy and paste this script into Blender's console

import bpy
import os

def get_main_area():
    for a in bpy.context.screen.areas:
        if a.spaces.active and a.spaces.active.type == 'VIEW_3D':
            return a

def startup_misc():
    get_main_area().spaces.active.shading.type = "RENDERED"
    bpy.ops.wm.window_fullscreen_toggle()

proj_path = bpy.path.abspath("//")

with open(os.path.join(proj_path, "yumepy", "main.py")) as f:
    exec(f.read())

startup_misc()