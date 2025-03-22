import os

GHCUP_GHC_VERSION = "9.6.6"

base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

def calculate_env():
    newenv = os.environ.copy()

    if "ghcup" in os.environ["BUILD_METHOD"]:
        if "GHCUP_PATH" in os.environ:
            newenv["PATH"] = os.path.join(newenv["GHCUP_PATH"], "ghc", GHCUP_GHC_VERSION, "bin") + os.pathsep + os.path.join(newenv["GHCUP_PATH"], "bin") + os.pathsep + newenv["PATH"]
        else:
            newenv["PATH"] = os.path.join(os.path.expanduser('~'), ".ghcup", "ghc", GHCUP_GHC_VERSION, "bin") + os.pathsep + os.path.join(os.path.expanduser('~'), ".ghcup", "bin") + os.pathsep + newenv["PATH"]
    
    if "nix" in os.environ["BUILD_METHOD"]:
        if not os.path.exists(os.path.join(base_path, "nix_env_export")):
            # TODO some workaround with nix...
            pass
    
    return newenv