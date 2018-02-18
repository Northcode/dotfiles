#!/bin/bash

PACKAGES="emacs X zsh scripts systemd email music"
TARGET=$HOME
TRAIL=""

usage() {
    echo "Usage:"
    echo "install.sh [-n] [-t|--target target] [-D] [packages]"
    echo "Options:"
    echo "-n : dry-run, no action. Run as normally, but do not modify file system"
    echo '-t : set target, defaults to $HOME, but can be set if you want to install somewhere else'
    echo "-D : uninstall instead of installing"
    echo ""
    echo "Specify packages to install as final arguments, or nothing to install default packages"
    echo "Default packages are: $PACKAGES"
    echo ""
}

# Loop thrugh every option args
while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
	-t|--target)
	    TARGET="$2"
	    shift
	    ;;
	-n|--dry-run)
	    TRAIL="-n"
	    ;;
	-h|--help|-?)
	    usage
	    exit
	    ;;
	*)
	    break
	;;
    esac
    shift
done

# Check if there are positional args left
if [ $# -gt 0 ]
then
    PACKAGES=$@
    shift
fi

if [ ! -x "$(command stow)" ]; then
    echo "Installing packages: $PACKAGES ..."
    stow $TRAIL -t $TARGET -v $PACKAGES
else
    echo "GNU Stow not installed, please install stow for easy management, or symlink the files in the subdirectories manually"
fi
