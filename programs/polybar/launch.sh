#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

polybar top 2>&1 | tee -a /tmp/polybar.log & disown
