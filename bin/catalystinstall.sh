#!/bin/bash

echo ""
echo ""
catalystrunfile=`ls amd-*.run | wc -w`
linuxversion=`uname -r`


if [ $catalystrunfile -ne 1 ]; then
	echo "Error: Please run this command in a directory with amd-*.run file"
	echo ""
	exit 1;
fi 
echo "AMD catalyst driver installation program for Linux $linuxversion"
echo ""
echo ""
echo "1. Make sure to comment 'echo OFF > /sys/kernel/debug/vgaswitcheroo/switch' in /etc/rc.local"
echo "2. Run 'echo ON > /sys/kernel/debug/vgaswitcheroo/switch' as root, before running these comands"

echo ""
read -p "Do you want to continue (y/n) ?  " -n 1
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi


debfiles=`ls *.deb | wc -w`

if [ $debfiles -gt 2 ]; then
	sudo dpkg -i fglrx*.deb
	sudo aticonfig --initial -f
	echo "Installation completed successfully"
else
	sudo sh ./amd-*.run --buildpkg Ubuntu/raring
	sudo dpkg -i fglrx*.deb
	sudo aticonfig --initial -f
	echo "Installation completed successfully"
	echo "After reboot, these new drivers will take effect"
fi

read -p "Do you want to reboot now (y/n) ? " -n 1
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
    sudo reboot -n
fi

echo ""
echo ""
