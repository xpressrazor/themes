#!/bin/bash
aticonfig --pxl | while read SPAM_OUT; do notify-send "$SPAM_OUT"; done
