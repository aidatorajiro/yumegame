stack exec -- pacman -Syu
stack exec -- pacman -S mingw-w64-x86_64-pkg-config
stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
stack build