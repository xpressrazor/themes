#!/bin/bash
echo 1 | sudo tee /sys/module/snd_hda_intel/parameters/power_save
echo Y | sudo tee /sys/module/snd_hda_intel/parameters/power_save_controller
pkill pulseaudio
