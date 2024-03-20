# Please copy and paste this script into Blender's console

import bpy
import os

proj_path = bpy.path.abspath("//")

with open(os.path.join(proj_path, "yumepy", "main.py")) as f:
    exec(f.read())