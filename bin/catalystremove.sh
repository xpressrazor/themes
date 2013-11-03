#!/bin/bash

echo "Removing catalyst driver"
echo ""
echo "1. Make sure you add 'echo OFF > /sys/kernel/debug/vgaswitcheroo/switch' in /etc/rc.local (before exit 0)"
echo "2. Also make sure you have internet access, because open source drivers will be reinstalled"

echo ""
read -p "Do you want to continue (y/n) ?  " -n 1
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]
then
    sudo apt-get remove --purge fglrx fglrx_* fglrx-amdcccle* fglrx-dev*
    sudo mv /etc/X11/xorg.conf /etc/X11/xorg-mybackup-conf
    sudo apt-get install --reinstall xserver-xorg-core xserver-xorg-video-intel libgl1-mesa-glx libgl1-mesa-dri libgl1-mesa-glx:i386 ubuntu-desktop
    sudo dpkg-reconfigure xserver-xorg
    echo "Removal of catalyst driver complete"
else
	exit 0
fi

echo ""
read -p "Do you want to reboot now (y/n) ? " -n 1
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
    sudo reboot -n
fi


echo ""
echo ""
