#!/bin/bash

# This small script makes it easy to download and properly install youtube-dl script for downloading youtube-videos.
# The script is copyright Danijel Orsolic (http://www.nuxified.org) and is released under the GNU GPL.

ROOT_UID=0         # Root has $UID 0.
E_WRONG_USER=65    # Not root?

# This script must be run as root.
if [ "$UID" -ne "$ROOT_UID" ]
then
  echo; echo "You must be root to run this script. Try sudo ./youtube-install.sh"; echo
  exit $E_WRONG_USER
fi

echo  Downloading and installing youtube-dl

cd /usr/local/bin && wget -c http://www.arrakis.es/~rggi3/youtube-dl/youtube-dl && chmod 755 youtube-dl

echo youtube-dl installed
