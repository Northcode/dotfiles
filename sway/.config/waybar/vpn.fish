#!/bin/fish

set me (string sub -s 1 -l 15 (basename (status --current-filename)))

for p in (pgrep $me | grep -v $fish_pid)
    kill $p
end

while true

    set vpn_status (systemctl is-active pia@Norway.service)

    printf '{ "text":"vpn: %s" }\n' $vpn_status

    sleep 10
end
