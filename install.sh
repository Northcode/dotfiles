#!/bin/bash

BASEDIR=$(readlink -f $(dirname $0))

echo "linking config folders"

for i in $(ls -a .config)
do
    if test $i != "." && test $i != ".."
    then
	rm -r ~/.config/$i
	ln -sf $BASEDIR/.config/$i ~/.config/$i
    fi
done

echo "linking emacs config"
rm ~/.emacs
ln -sf $BASEDIR/.emacs ~/.emacs

echo "Removing legacy ncmpcpp config"
rm -r ~/.ncmpcpp/

echo "Linking user bin folder"
rm ~/bin
ln -sf $BASEDIR/bin ~/bin

echo "linking xinitrc"
rm ~/.xinitrc
ln -sf ~/.config/X/xinitrc ~/.xinitrc 

echo "linking zshrc"
rm ~/.zshrc
ln -sf $BASEDIR/.zshrc ~/.zshrc
