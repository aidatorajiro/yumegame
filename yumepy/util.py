import os

base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

def calculate_env():
    newenv = os.environ.copy()

    if "ghcup" in os.environ["BUILD_METHOD"]:
        if "GHCUP_PATH" in os.environ:
            newenv["PATH"] = newenv["GHCUP_PATH"] + os.pathsep + newenv["PATH"]
        else:
            newenv["PATH"] = os.path.join(os.path.expanduser('~'), ".ghcup", "bin") + os.pathsep + newenv["PATH"]
    
    if "nix" in os.environ["BUILD_METHOD"]:
        if not os.path.exists(os.path.join(base_path, "nix_env_export")):
            # TODO some workaround with nix...
            pass
    
    return newenv