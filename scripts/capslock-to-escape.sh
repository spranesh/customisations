#!/bin/bash

# "Remapping caps lock to escape"
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
