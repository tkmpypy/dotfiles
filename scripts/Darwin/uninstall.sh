#!/bin/zsh

# Uninstall Homebrew
brew uninstall --force $(brew list)
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
