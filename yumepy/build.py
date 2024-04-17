import socket
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
import time
import logging
import os
import subprocess
from util import calculate_env
import sys
from threading import Event
import queue

job_queue = queue.Queue(1)

job_queue.put(())

current_proc = None

base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
hs_path = os.path.join(base_path, "yumegamehs")

os.chdir(hs_path)

def watch():
    global current_proc

    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')

    class MyEventHandler(FileSystemEventHandler):
        def on_modified(self, event) -> None:

            if event.src_path.endswith(".hs") or event.src_path.endswith("hsfunctions.py"):
                logging.info("Modified: %s" % event.src_path)
                try:
                    job_queue.put((), block=False)
                except queue.Full:
                    logging.info("Job queue is full. Ignoring this event.")

    logging.info(f'start watching directory {hs_path!r}')
    event_handler = MyEventHandler()
    observer = Observer()
    observer.schedule(event_handler, hs_path, recursive=True)
    observer.start()

    try:
        while True:
            time.sleep(0.01)
            if job_queue.full():
                time.sleep(1)
                job_queue.get()
                logging.info("detect modification. running build again...")
                if current_proc is not None:
                    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                        logging.info("Sending shutdown signal")
                        try:
                            s.connect(("127.0.0.1", 3171))
                            s.send(b"shutdown")
                        except socket.error as e:
                            logging.info("Shutdown signal send failed: %s", e)
                    current_proc.wait()
                logging.info("Running build script...")
                retcode = build().wait()
                if retcode == 0:
                    current_proc = run()
                    time.sleep(3)
                    if os.environ.get("SINGLE_MODE") is not None:
                        subprocess.run(["wmctrl", "-x", "-r", "yumegamehs-exe.yumegamehs-exe", "-t", "0"])
                    else:
                        subprocess.run(["wmctrl", "-x", "-r", "yumegamehs-exe.yumegamehs-exe", "-b", "add,sticky"])
                        subprocess.run(["wmctrl", "-x", "-r", "yumegamehs-exe.yumegamehs-exe", "-e", "0,2880,540,300,300"])
                    subprocess.run(["wmctrl", "-x", "-r", "yumegamehs-exe.yumegamehs-exe", "-b", "add,above"])
    finally:
        observer.stop()
        observer.join()

def build():
    if "stack" in os.environ["BUILD_METHOD"]:
        return subprocess.Popen(["stack", "build"], env=calculate_env())

def run():
    if "stack" in os.environ["BUILD_METHOD"]:
        return subprocess.Popen(["stack", "run"], env=calculate_env())

if __name__ == '__main__':
    if sys.argv[1] == 'build':
        build().wait()
    if sys.argv[1] == 'run':
        retcode = build().wait()
        if retcode == 0:
            run().wait()
    if sys.argv[1] == 'watch':
        watch()