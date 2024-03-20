#!/bin/sh

. ./venv/bin/activate

BUILD_METHOD=ghcup,stack python yumepy/build.py watch