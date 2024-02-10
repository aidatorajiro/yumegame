import os
import subprocess
import sys

# init project info
proj_path = bpy.path.abspath("//")
venv_path = os.path.join(proj_path, "venv")
package_path = os.path.join(venv_path, "Lib", "site-packages")
python_path = os.path.join(os.path.dirname(bpy.app.binary_path), "%s.%s" % (bpy.app.version[0], bpy. app.version[1]), "python", "bin", "python.exe")

# initialize venv
subprocess.run([python_path, "-m", "venv", venv_path], capture_output=True)
subprocess.run([python_path, "-m", "pip", "install", "--target=" + package_path, "-r", os.path.join(proj_path, "requirements.txt")], capture_output=True)

# initialize module
sys.path.append(package_path)

# setup server


# misc scripts
bpy.ops.screen.screen_full_area(True) # full screen current panel


