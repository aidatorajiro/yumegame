#!/bin/python3
import os
import subprocess

basepath = os.path.abspath(os.path.dirname(__file__))

# iterate over all .mp4 files in the directory 'movies'
for entry in os.listdir(os.path.join(basepath, 'movies')):
    if entry.endswith('.mp4'):
        # check if [original path].right.mp4 already exists
        if not os.path.exists(os.path.join(basepath, 'movies-left', entry + '.left.mp4')):
            # split the file
            # thanks to https://github.com/nyanshiba/best-ffmpeg-arguments
            nv_h264_settings = '-c:v h264_nvenc -preset:v p7 -profile:v high -rc:v vbr -rc-lookahead 1 -spatial-aq 0 -temporal-aq 1 -cq 20 -weighted_pred 0 -coder cabac -b_ref_mode 1 -dpb_size 4 -multipass 1 -g 120 -bf 2 -pix_fmt yuv420p -color_range tv -color_primaries bt709 -color_trc bt709 -colorspace bt709 -movflags +faststart'

            subprocess.run([
                'ffmpeg', '-i',
                os.path.join(basepath, 'movies', entry), '-acodec', 'copy'] +
                nv_h264_settings.split(' ') + [
                '-filter_complex', 
                '[0]crop=iw/2:ih:0:0[left];[0]crop=iw/2:ih:ow:0[right]', 
                '-map', '[left]', 
                os.path.join(basepath, 'movies-left', entry + '.left.mp4'), 
                '-map', '[right]', 
                os.path.join(basepath, 'movies-right', entry + '.right.mp4')])
            