#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar right 2>&1 | tee -a /tmp/polybarRight.log & disown
polybar left 2>&1 | tee -a /tmp/polybarLeft.log & disown

echo "Bars launched..."
