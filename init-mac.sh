#!/usr/bin/env bash

#-------------------------------------------------------------------------------
# Instructions

# 1. Install xcode or just command line tools
# 2. Install homebrew, casks, and packages
# 3. Add ssh key to github
# 4. Clone dotfiles
# 5. Set up symlinks
# 6. Install fonts, open apps, customize as needed

#-------------------------------------------------------------------------------
# xcode command line tools

xcode-select -p

#-------------------------------------------------------------------------------
# homebrew

# Go to: https://brew.sh/

# If you want to install specific versions through homebrew:
brew tap homebrew/cask-versions

#-------------------------------------------------------------------------------
# homebrew cask apps

apps="alfred emacs firefox google-chrome iterm2 linearmouse macdown paintbrush
 scroll-reverser sizeup vlc"

for app in $apps; do
  brew install --cask "$app"
done

#-------------------------------------------------------------------------------
# homebrew packages

packages="bash cloc coreutils ctags ffmpeg fzf git haskell-stack htop markdown p7zip
 rbenv rename shellcheck the_silver_searcher thefuck tree ydiff youtube-dl"

for package in $packages; do
  brew install "$package"
done

#-------------------------------------------------------------------------------
# Symlinks

~/dotfiles/init-symlinks.sh

#-------------------------------------------------------------------------------
# java 1.8

# brew tap AdoptOpenJDK/openjdk
# brew cask install adoptopenjdk8

#-------------------------------------------------------------------------------
# zsh

# if ! [ -d ~/.oh-my-zsh ]; then
#   echo "Cloning oh-my-zsh..."
#   git clone git@github.com:robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
# else
#   echo "oh-my-zsh already exists"
# fi

# if ! [ -L ~/.oh-my-zsh/themes/mikenichols.zsh-theme ]; then
#   echo "Linking zsh theme..."
#   ln -s ~/dotfiles/mikenichols.zsh-theme ~/.oh-my-zsh/themes/mikenichols.zsh-theme
# else
#   echo "zsh theme already linked"
# fi

# if [ "$SHELL" = "/usr/local/bin/zsh" ]; then
#   echo "zsh already set as login shell"
# else
#   sudo chsh -s "/usr/local/bin/zsh" "$(whoami)"
# fi

#-------------------------------------------------------------------------------
# Unicorn leap

# if ! [ -e  ~/code/unicornleap ]; then
#   echo "Installing unicornleap..."
#   mkdir -p ~/code
#   cd ~/code
#   git clone git@github.com:jgdavey/unicornleap.git
#   cd unicornleap
#   make && make install
# else
#   echo "unicornleap already installed"
# fi

#-------------------------------------------------------------------------------

## End init-mac.sh
