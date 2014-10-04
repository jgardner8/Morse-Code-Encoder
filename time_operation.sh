#!/bin/sh

# A really shitty benchmark!

get_time() {
	timedatectl | grep Local | awk '{printf "%s", $5}'
}

echo $(get_time) && eval "$1" && echo $(get_time)
