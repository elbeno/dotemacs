#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "dockwindow expects 2 arguments: window-title and left|right"
    exit 1
fi

if [ $2 == "right" ]; then
    # get window width and monitor max x
    w=`wmctrl -l -G | grep $1 | awk '{print $5}'`
    xmax=`xrandr | grep '*' | cut -d'x' -f1 | awk 'BEGIN {max = 0} {if ($1>max) max=$1} END {print max}'`
    wmctrl -r $1 -e 0,`expr $xmax - $w`,-1,-1,-1
fi
if [ $2 == "left" ]; then
    wmctrl -r $1 -e 0,0,-1,-1,-1
fi

wmctrl -r $1 -b remove,maximized_vert
wmctrl -r $1 -b add,maximized_vert
