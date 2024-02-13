from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
import time
import logging
import os
import subprocess
from util import calculate_env
import sys

modified_flag = True
current_proc = None

base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
hs_path = os.path.join(base_path, "hs", "yumegame")

os.chdir(hs_path)

def watch():
    global current_proc
    global modified_flag

    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')

    class MyEventHandler(FileSystemEventHandler):
        def on_modified(self, event) -> None:
            global modified_flag

            if event.src_path.endswith(".hs"):
                logging.info("Modified: %s" % event.src_path)
                modified_flag = True

    logging.info(f'start watching directory {hs_path!r}')
    event_handler = MyEventHandler()
    observer = Observer()
    observer.schedule(event_handler, hs_path, recursive=True)
    observer.start()

    try:
        while True:
            time.sleep(1)
            if modified_flag:
                modified_flag = False
                if current_proc is not None:
                    current_proc.terminate()
                logging.info("Running build script...")
                retcode = build().wait()
                if retcode == 0:
                    current_proc = run()
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