import os

def calculate_env():
    newenv = os.environ.copy()

    if "ghcup" in os.environ["BUILD_METHOD"]:
        if "GHCUP_PATH" in os.environ:
            newenv["PATH"] = newenv["GHCUP_PATH"] + os.pathsep + newenv["PATH"]
        else:
            newenv["PATH"] = os.path.join(os.path.expanduser('~'), ".ghcup", "bin") + os.pathsep + newenv["PATH"]
    
    return newenv