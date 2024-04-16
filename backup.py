#!/usr/bin/python3

import subprocess

import os

os.chdir(os.path.dirname(os.path.abspath(__file__)))

subprocess.run(["restic", "-r", "rclone:ueno:yumegame", "backup", ".", "--exclude-file=excludes.txt", '--password-file', 'password-file'])
