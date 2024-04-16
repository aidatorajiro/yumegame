import os
import subprocess
import glob
import sys
import glob
import socket
import struct
import time
import platform
from threading import Event

# init project info
proj_path = bpy.path.abspath("//")
hs_path = os.path.join(proj_path, "yumegamehs")

# init venv
venv_path = os.path.join(proj_path, "venv")

# init project info
if "Windows" in platform.platform():
    package_path = glob.glob(os.path.join(venv_path, "Lib", "site-packages"))[0]
else:
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

timeframe = 1 / 60

def sock_loop():
    global the_socket
    
    def terminate_check():
        if ev_ask_terminate.is_set():
            print("unregistering timer...")
            try:
                the_socket.close()
            except Exception:
                pass
            ev_ask_terminate.clear()
            ev_retry_connection.set()
            return True
    def socket_end():
        print("connect lost")
        try:
            the_socket.close()
        except Exception:
            pass
        if terminate_check():
            return None
        print("retrying connection...")
        ev_retry_connection.set()
        return 1
    
    def get_stream():
        s = the_socket.recv(4096)
        r = s
        while len(s) == 4096:
            s = the_socket.recv(4096)
            r += s
        return r

    try:
        if terminate_check():
            return None
        if ev_retry_connection.is_set():
            the_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            print("(re-)connecting to the server")
            the_socket.connect(("127.0.0.1", 3170))
            ev_retry_connection.clear()
        stream = get_stream()
        if len(stream) == 0:
            return socket_end()
        while len(stream):
            datatype, stream = stream[:8], stream[8:]
            datatype = struct.unpack(">Q", datatype)[0]
            datalen, stream = stream[:8], stream[8:]
            datalen = struct.unpack(">Q", datalen)[0]
            if datalen < 1000000:
                data, stream = stream[:datalen], stream[datalen:]
                if len(data) == datalen:
                    try:
                        if datatype == 1:
                            exec(data.decode(), globals())
                    except Exception as e:
                        print("Error occured during eval\n[Received Script]\n%s\n[Error Message]\n%s" % (data, e))
                else:
                    the_socket.close()
                    raise ValueError("Inconsistent data lengths (%s <-> %s)" % (datalen, len(data)))
            else:
                the_socket.close()
                raise ValueError("Too large input (data length: %s)" % datalen)
        return timeframe
    except socket.error:
        return socket_end()

try:
    the_socket
except NameError:
    the_socket = None

try:
    ev_ask_terminate
    ev_retry_connection
except NameError:
    ev_ask_terminate = Event()
    ev_retry_connection = Event()
    ev_retry_connection.set()

try:
    client_thread_created
except NameError:
    client_thread_created = False

def start():
    global client_thread_created
    if client_thread_created == True:
        print("terminating existing thread")
        ev_ask_terminate.set()
    bpy.app.timers.register(sock_loop, first_interval=2)
    client_thread_created = True

start()