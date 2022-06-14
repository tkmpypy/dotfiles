#!/bin/zsh

defaults write com.apple.Finder AppleShowAllFiles true

sudo sysctl -w kern.maxfiles=20480
sudo sysctl -w kern.maxfilesperproc=18000
