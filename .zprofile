##
## Mike's .zprofile file
##

source ~/dotfiles/common.sh

#-------------------------------------------------------------------------------
# oh-my-zsh setup

# Path to your oh-my-zsh configuration.
ZSH=~/dotfiles/oh-my-zsh

# Set name of the theme to load.
# Look in ~/dotfiles/oh-my-zsh/themes/
ZSH_THEME="mikenichols"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git github node osx ruby)

source "$ZSH"/oh-my-zsh.sh

# Custom options have to happen after we load the oh-my-zsh.sh file

# Turn off shared history
unsetopt share_history

# Turn off autocorrect. This is too annoying
unsetopt correct
unsetopt correct_all
DISABLE_CORRECTION="true"

# zsh default aliases this to 'ls -l'
alias ll="ls -hAl"

#-------------------------------------------------------------------------------
# zsh functions

alias resh="source ~/.zprofile && echo '.zprofile reloaded'"

#-------------------------------------------------------------------------------
# path

# Force unique values for the path array (which is tied to $PATH)
typeset -U path

# This was recommended by homebrew when I installed node
path=(/usr/local/share/npm/bin "$path[@]")

# For rabbitmq
path=(/usr/local/sbin "$path[@]")

# For great justice
path=(/usr/local/bin "$path[@]")

# My scripts
path=(~/dotfiles/bin "$path[@]")

# Emacs cask
path=(~/.cask/bin "$path[@]")

#-------------------------------------------------------------------------------
## end .zprofile
