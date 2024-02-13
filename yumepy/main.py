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
if not package_path in sys.path:
    sys.path.append(package_path)

"""
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
import logging

logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')

class MyEventHandler(FileSystemEventHandler):
    def on_modified(self, event) -> None:
        if event.src_path.endswith(".py"):
            pass
"""

