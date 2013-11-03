#!/usr/bin/perl

$checkGraphicsCommand = ` aticonfig --pxl`;

chomp $checkGraphicsCommand;

if ($checkGraphicsCommand eq "PowerXpress: Integrated GPU is active (Power-Saving mode).") {
	# Activate the discrete GPU drivers
	#system "sudo aticonfig --px-dgpu";
	#system "pkexec aticonfig --px-dgpu && pkill gnome-session"
	system "pkexec aticonfig --px-dgpu && pkill lxdm-binary"
} else {
	# Activate the integrated intel 3000HD drivers
	#system "sudo aticonfig --px-igpu";
	#system "pkexec aticonfig --px-igpu && pkill gnome-session"
	system "pkexec aticonfig --px-igpu && pkill lxdm-binary"
}

