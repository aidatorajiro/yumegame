# yume tensei

A game development framework that makes Blender as an asset editor and a game simultaneously.

## Project description

This project consists of these parts:

1. Blender and the startup Python script  
   Location: `startup.py`, `yumepy/main.py`  
   Description: the startup script creates a TCP server within the Blender process. The server evaluates incoming data as a python script.
2. A controller program.
   Location: `yumagamehs/`
   Description: it connects to the TCP server and controls Blender. It is written in Haskell and Yampa to intuitively describe the data transformation regarding with time.
3. Various scripts and assets to help development/exhibition/playing environment.
   Location: `misc-scripts`
   Description: This folder includes a specific version of Windows SDL2. Somehow, currently the latest version of SDL cannot work with Haskell due to some compile error. Thus, you need older version of SDL2 if you want to use Blender.
4. A script to launch the development environment.
   Location: `hajimari.py` `hajimari-multi.py` `hajimari-single.py`
   Description: A script to launch the main app (Blender), the controller app with hot-rebuilding features, the script editors and the debug consoles.
