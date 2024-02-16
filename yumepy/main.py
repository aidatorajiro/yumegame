import os
import subprocess
import glob
import sys
import glob
import socket
import struct
import time

# init project info
proj_path = bpy.path.abspath("//")
hs_path = os.path.join(proj_path, "hs", "yumegame")

# init venv
venv_path = os.path.join(proj_path, "venv")

# init project info
package_path = glob.glob(os.path.join(venv_path, "lib", "python*", "site-packages"))[0]
orig_python_path = sys.executable
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

if not hs_path in sys.path:
    sys.path.append(hs_path)

def client_inner():
    while True:
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                print("(re-)connecting to the server")
                s.connect(("127.0.0.1", 3170))
                while True:
                    if ev_ask_terminate.is_set():
                        return
                    datatype = s.recv(8)
                    if len(datatype) == 0:
                        break
                    datatype = struct.unpack(">Q", datatype)[0]
                    datalen = s.recv(8)
                    if len(datalen) == 0:
                        break
                    datalen = struct.unpack(">Q", datalen)[0]
                    if datalen < 1000000:
                        data = s.recv(datalen)
                        if len(data) == 0:
                            break
                        if len(data) == datalen:
                            try:
                                if datatype == 1:
                                    exec(data.decode())
                            except Exception as e:
                                print("Error occured during eval\n[Received Script]\n%s\n[Error Message]\n%s" % (data, e))
                        else:
                            raise ValueError("Inconsistent data lengths (%s <-> %s)" % (datalen, len(data)))
                    else:
                        raise ValueError("Too large input (data length: %s)" % datalen)
        except socket.error:
            print("connect lost")
            if ev_ask_terminate.is_set():
                return
            time.sleep(1)

from threading import Thread, Event

try:
    ev_ask_terminate
except NameError:
    ev_ask_terminate = Event()

try:
    client_thread
except NameError:
    client_thread = None

def start():
    global ev_ask_terminate
    global client_thread
    if client_thread is not None:
        print("thread object exists")
        if client_thread.is_alive():
            print("terminating existing thread")
            ev_ask_terminate.set()
            print("waiting...")
            client_thread.join()
        ev_ask_terminate.clear()
    client_thread = Thread(target=client_inner, args=[])
    client_thread.start()

start()