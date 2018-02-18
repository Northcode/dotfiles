#!/bin/bash
jack_control start

jack_control ds alsa

jack_control dps device hw:Intel,0
jack_control dps rate 48000
jack_control dps nperiods 2
jack_control dps period 512

sleep 2
pkill a2jmidid
a2jmidid -e &

sleep 1
systemctl --user restart pulseaudio
