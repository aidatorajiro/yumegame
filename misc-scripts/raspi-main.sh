#!/bin/bash

# setup:
# 1. run `sudo apt install wmctrl` and switch from Wayland to X11 usinf raspi config
# 2. run `docker run --restart always -d -p 1935:1935 --name nginx-rtmp tiangolo/nginx-rtmp`

sleep 3

killall ffplay
killall vlc

sleep 2

killall -9 ffplay
killall -9 vlc

if [ ! -d /media/pi/d34c487a-24e3-4085-b2ee-3301c757983b ]
then

sleep 10

setsid ffplay -i rtmp://localhost/live/rpi5dayo -vf crop=1920:1080:0:0 1>/dev/null 2>&1 &

res=1

until [ "$res" -eq "0" ]
do

  echo "Waiting until window creation..."

  wmctrl -x -r ffplay.ffplay -b add,fullscreen

  res=$?

  wmctrl -x -r ffplay.ffplay -b add,above

  sleep 1

done

else

setsid vlc --no-video-title-show --intf dummy -LZ "/media/pi/d34c487a-24e3-4085-b2ee-3301c757983b/movies-left" 1>/dev/null 2>&1 &

setsid vlc --no-video-title-show --intf dummy -LZ "/media/pi/d34c487a-24e3-4085-b2ee-3301c757983b/movies-right" 1>/dev/null 2>&1 &

sleep 5

THE_WINDOWS="$(wmctrl -lx | grep vlc.Vlc | awk '{print $1}')"
THE_ARR=($THE_WINDOWS)

wmctrl -i -r ${THE_ARR[0]} -t 0
sleep 1
wmctrl -i -r ${THE_ARR[0]} -b add,fullscreen
wmctrl -i -r ${THE_ARR[0]} -b add,above

sleep 1
wmctrl -i -r ${THE_ARR[1]} -t 1
sleep 1
wmctrl -i -r ${THE_ARR[1]} -b add,fullscreen
wmctrl -i -r ${THE_ARR[0]} -b add,above

fi
