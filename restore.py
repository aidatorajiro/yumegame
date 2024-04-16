#!/bin/python

import subprocess

import os

envcpy = os.environ.copy()

os.chdir(os.path.dirname(os.path.abspath(__file__)))

subprocess.run(["restic", "-r", "rclone:ueno:yumegame", "restore", "latest", "--target", ".", '--password-file', 'password-file'], env=envcpy)