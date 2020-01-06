#!/bin/fish

set me (string sub -s 1 -l 15 (basename (status --current-filename)))

for p in (pgrep $me | grep -v $fish_pid)
    kill $p
end

while true
    if pgrep spotifyd
	if test (playerctl status) = 'Playing'
	
	    set song (playerctl metadata "xesam:title")
	    set artist (playerctl metadata "xesam:artist")
	    set album (playerctl metadata "xesam:album")

	    printf '{ "text":"%s / %s - %s" }\n' $song $album $artist
	else
	    printf '{ "text":"nothing playing..." }\n'
	end

    else
	printf '{ "text":"" }\n'
    end

    sleep 10
end
