#!/bin/bash

BASEDIR=$(readlink -f $(dirname $0))

echo "linking config folders to $BASEDIR"

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

echo "linking xinitrc"
rm ~/.mbsyncrc
ln -sf $BASEDIR/.mbsyncrc ~/.mbsyncrc 

echo "linking xprofile"
rm ~/.xprofile
ln -sf $BASEDIR/.xprofile ~/.xprofile

echo "linking zshrc"
rm ~/.zshrc
ln -sfv "$BASEDIR/zshrc" ~/.zshrc

rm ~/.zpreztorc 
ln -sf $BASEDIR/zpreztorc ~/.zpreztorc

echo "linking screenrc"
rm ~/.screenrc
ln -sf $BASEDIR/.screenrc ~/.screenrc
