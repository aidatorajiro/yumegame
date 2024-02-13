import os
import subprocess
import glob
import sys
import glob

# init project info
proj_path = bpy.path.abspath("//")

# init venv
venv_path = os.path.join(proj_path, "venv")

# init project info
package_path = glob.glob(os.path.join(venv_path, "lib", "python*", "site-packages"))[0]
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
