#!/bin/bash

handle() {
  local event=$(swaymsg -t SUBSCRIBE "['window']");
  if [[ $(jq .container.app_id <<< $event) == '"waybar"' && $(jq .change <<< $event) == '"new"' ]]; then
    local vert=$(( $(jq .container.geometry.height <<< $event) / 2 + 10))
    local horz=$(( $(jq .container.geometry.width <<< $event) / 2))
    swaymsg move position cursor
    swaymsg move down $vert px
    swaymsg move left $horz px
  fi
}

while handle; do
  sleep 0.1
done
