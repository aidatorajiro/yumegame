from util import calculate_env

import subprocess
import os
import sys

base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
hs_path = os.path.join(base_path, "yumegamehs")

if __name__ == "__main__":
    if sys.argv[1] == "code":
        subprocess.run(["code", hs_path], env=calculate_env())
