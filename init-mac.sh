#!/usr/bin/env bash

# 1. Install xcode
# 2. Open xcode and select command line tools under "locations"
# 3. Add ssh key to github
# 4. Clone dotfiles
# 5. Run this script
# 6. Install fonts, open apps

#-------------------------------------------------------------------------------
# xcode command line tools

# if [ "$(xcode-select -p)" = '/Applications/Xcode.app/Contents/Developer' ]; then
#   echo 'xcode or command line tools already installed.'
# else
#   echo 'Please install xcode and add your ssh key to github before running this script.'
#   exit 1
# fi

#-------------------------------------------------------------------------------
# homebrew

# if ! type brew > /dev/null; then
#   echo "Homebrew not found, installing..."
#   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
# else
#   echo "Homebrew already installed"
# fi

# brew tap homebrew/cask-versions

#-------------------------------------------------------------------------------
# homebrew cask apps

apps="alfred emacs firefox flux google-chrome iterm2 macdown paintbrush
 scroll-reverser sizeup vlc yujitach-menumeters"

for app in $apps; do
  brew cask install "$app"
done

#-------------------------------------------------------------------------------
# homebrew packages

packages="bash cloc coreutils ctags ffmpeg fzf git haskell-stack htop markdown p7zip
 rbenv rename shellcheck the_silver_searcher thefuck tree ydiff youtube-dl zsh"

for package in $packages; do
  brew install "$package"
done

#-------------------------------------------------------------------------------
# java 1.8

# brew tap AdoptOpenJDK/openjdk
# brew cask install adoptopenjdk8

#-------------------------------------------------------------------------------
# zsh

if ! [ -d ~/.oh-my-zsh ]; then
  echo "Cloning oh-my-zsh..."
  git clone git@github.com:robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
else
  echo "oh-my-zsh already exists"
fi

if ! [ -L ~/.oh-my-zsh/themes/mikenichols.zsh-theme ]; then
  echo "Linking zsh theme..."
  ln -s ~/dotfiles/mikenichols.zsh-theme ~/.oh-my-zsh/themes/mikenichols.zsh-theme
else
  echo "zsh theme already linked"
fi

if [ "$SHELL" = "/usr/local/bin/zsh" ]; then
  echo "zsh already set as login shell"
else
  sudo chsh -s "/usr/local/bin/zsh" "$(whoami)"
fi

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
# Symlinks

~/dotfiles/init-symlinks.sh

## End init-mac.sh
