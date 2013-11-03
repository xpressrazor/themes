#!/bin/bash
find ~/.mozilla/firefox/*.default/Cache/*/ -type f -size +2M -printf "cp %p .\n"  > /tmp/findcache &&

sh < /tmp/findcache &&
rm /tmp/findcache




