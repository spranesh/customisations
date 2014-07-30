#!/bin/bash
wd=$(pwd)
echo "Working directory is" $wd "."
echo "Creating symlinks if they do not already exist."
echo

ln -vsT $wd/zsh/zshrc ~/.zshrc
ln -vsT $wd/rc/screenrc ~/.screenrc
ln -vsT $wd/rc/vimperatorrc ~/.vimperatorrc
ln -vsT $wd/xmonad ~/.xmonad
