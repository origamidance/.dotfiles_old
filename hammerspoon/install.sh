#!/bin/bash

echo "Hammerspoon configuring"

HAMMERSPOONDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd ~

ln -Fs "$HAMMERSPOONDIR"
rm -rf .hammerspoon
mv hammerspoon .hammerspoon