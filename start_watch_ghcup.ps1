.\venv\Scripts\Activate.ps1

$env:BUILD_METHOD="ghcup,stack"

python yumepy/build.py watch