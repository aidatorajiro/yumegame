#!/bin/python

import subprocess

import os

os.chdir(os.path.dirname(os.path.abspath(__file__)))

subprocess.run(["restic", "-r", "rclone:ueno:yumegame", "restore", "latest", "--target", ".", '--password-file', 'password-file', '-e', '.git'])