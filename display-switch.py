#!/bin/python3

from pyglet.window.xlib import xlib
from ctypes import byref, cast, POINTER
from contextlib import contextmanager
from hajimari import *

def check_error(display, event):
    print("Xlib error: {0} {1}".format(display, event))

_error_handler_ptr = xlib.XErrorHandler(check_error)
xlib.XSetErrorHandler(_error_handler_ptr)

xlib.XInitThreads()

@contextmanager
def open_display():
    display = xlib.XOpenDisplay(None)
    yield display
    xlib.XCloseDisplay(display)

def get_workspace(display):
    atom = xlib.XInternAtom(display, b"_NET_CURRENT_DESKTOP", False)
    data = xlib.c_void_p()
    real_type = xlib.c_ulong()
    format = xlib.c_int()
    items = xlib.c_ulong()
    bytes_after = xlib.c_ulong()
    data_ptr = cast(byref(data), POINTER(POINTER(xlib.c_ubyte)))

    xlib.XGetWindowProperty(display, xlib.XDefaultRootWindow(display), atom, 0, 1,
        False, xlib.AnyPropertyType, byref(real_type), byref(format), byref(items),
        byref(bytes_after), data_ptr)

    workspace_id = cast(data, POINTER(xlib.c_ulong)).contents.value

    xlib.XFree(data)

    return workspace_id

def evt_loop(display):
    root_win = xlib.XDefaultRootWindow(display)
    atom = xlib.XInternAtom(display, b"_NET_CURRENT_DESKTOP", False)
    xlib.XSelectInput(display, root_win, xlib.PropertyChangeMask)

    ev = xlib.XEvent()

    while True:
        xlib.XNextEvent(display, byref(ev))
        if ev.type == xlib.PropertyNotify and ev.xproperty.atom == atom:
            workspace = get_workspace(display)
            print("Current workspace id: {0}".format(workspace))
            title = "- Blender 4."
            move_window_workspace(title, str(workspace))
  
def main():
    with open_display() as display:
        evt_loop(display)
  
if __name__ == "__main__":
    main()