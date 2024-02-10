import os
import subprocess
import sys
import glob
import select

# init project info
proj_path = bpy.path.abspath("//")
venv_path = os.path.join(proj_path, "venv")

if sys.platform == "linux":
    # init project info
    package_path = os.path.join(venv_path, "lib", "site-packages")
    orig_python_path = "/usr/bin/python"
    venv_python_path = os.path.join(venv_path, "bin", "python")
    venv_pip_path = os.path.join(venv_path, "bin", "pip")

    # initialize venv
    if not os.path.exists(venv_path):
        subprocess.run([orig_python_path, "-m", "venv", venv_path], capture_output=True)
        subprocess.run([venv_pip_path, "install", "--upgrade", "pip"], capture_output=True)
        subprocess.run([venv_pip_path, "install", "-r", os.path.join(proj_path, "requirements.txt")], capture_output=True)

# initialize module
sys.path.append(package_path)

proc = subprocess.Popen([glob.glob(os.path.join(proj_path, "hs", ".stack-work", "install", "*", "*", "*", "bin", "yumegame-exe"))[0]], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

stdout = proc.stdout
stdin = proc.stdin

select.select([stdout], [], [], 60/1000)

stdout.read