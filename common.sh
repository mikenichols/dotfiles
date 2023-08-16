## common.sh

#-------------------------------------------------------------------------------
# OS

if [ "$(uname)" = "Darwin" ]; then
  export MIKEN_OS="mac"
elif [ "$(uname)" = "linux-gnu" -o "$(uname)" = "Linux" ]; then
  export MIKEN_OS="linux"
else
  export MIKEN_OS="unknown"
fi

#-------------------------------------------------------------------------------
# emacs

# There can be only one
export EDITOR="emacs -q -nw"
alias e="emacsclient -n"

#-------------------------------------------------------------------------------
# functions

# change up a directory
cu() {
  cd ../"$1"
}

fn() {
  local dir="$2"

  if [ -z "$dir" ]; then
    dir='.'
  fi

  find "$dir" -iname "*$1*" -not -path "*/node_modules/*"
}

mkcd() {
  mkdir "$1" && cd "$1"
}

# Rename in current directory
rn() {
  rename -n "s/$1/$2/g" ./*
}

rename_music() {
  rename -n --sanitize --lower-case *
}

hpg() {
  history | grep "$1"
}

hpgw() {
  history | grep "\b$1\b"
}

biggest() {
  if type gsort > /dev/null; then
    du -sh ./* | gsort -h -r | head -15
  else
    du -sh ./* | grep -e "\d\+G" | sort -n -r | head -10
    du -sh ./* | grep -e "\d\+M" | sort -n -r | head -10
  fi
}

make_symlink_safe() {
  if [ -h "$2" ]; then
    echo "Symlink $2 already exists"
  elif [ -f "$2" ]; then
    echo "File $2 already exists"
  elif [ -e "$2" ]; then
    echo "Something $2 already exists"
  else
    echo "Linking $2 as $1"
    ln -s "$1" "$2"
  fi
}

gh() {
  local orig_dir="$(pwd)"

  while ! [ -e .git ]; do
    cd ..

    if [ "$(pwd)" = '/' ]; then
      cd "$orig_dir"
      echo 'Not in a git repo'
      return 1
    fi
  done

  local repo_git_url=$(grep -o -m 1 'git@.*' .git/config)
  local repo_url=${repo_git_url/git\@github\.com\:/https://github.com/}

  cd "$orig_dir"
  open "$repo_url"
}

agi() {
  \ag $(echo "$*" | sed 's/ / --ignore /g')
}

rgi() {
  \rg $(echo "$*" | sed 's/ / -g !/g')
}

rgw() {
  \rg "\b$1\b"
}

# requires node and http-server:
#   npm install http-server -g
serve() {
  local port=1234

  if [ "$1" ]; then
    port="$1"
  fi

  http-server ./ -p ${port}
}

change_shell() {
  local new_shell="$1"

  if ! [ -e "$new_shell" ]; then
    echo 'Please enter a valid shell'
    return 1
  fi

  chsh -s "$new_shell" "$(whoami)"
}

oci() {
  if [ -e "coverage/index.html" ]; then
    open coverage/index.html
  elif [ -e "cover/excoveralls.html" ]; then
    open cover/excoveralls.html
  elif [ -e "coverage/lcov-report/index.html" ]; then
    open coverage/lcov-report/index.html
  else
    echo "Nope."
  fi
}

workfromhome() {
  if grep "\[user\]" .git/config > /dev/null; then
    echo "Already set up"
  else
    echo "Setting work email for this repo..."
    echo "[user]\n  email = mike.nichols@avant.com\n  name = Mike Nichols" >> .git/config
  fi
}

s() {
  local file="$1"

  if [ -z "$file" ]; then
    file='.'
  fi

  bundle exec rspec "$file"

  if [ "$?" -eq '0' ]; then
    unicornleap -s 1.5
  else
    unicornleap -s 1.5 --unicorn sweetjesus.png
  fi
}

trim_end() {
  if [ -e trimmed.mp3 ]; then
    rm trimmed.mp3
  fi

  ffmpeg -i "$1" -ss 0 -to "$2" -c copy trimmed.mp3
}

syncmusic() {
  rsync -vr --size-only --delete /Volumes/home-silver/home/Music/iTunes/ ~/Music/iTunes
}

fco() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

fshow_preview() {
    glNoGraph |
        fzf --no-sort --reverse --tiebreak=index --no-multi \
            --ansi --preview="$_viewGitLogLine" \
                --header "enter to view, alt-y to copy hash" \
                --bind "enter:execute:$_viewGitLogLine   | less -R" \
                --bind "alt-y:execute:$_gitLogLineToHash | xclip"
}

lgi() {
 ls | grep -i "$1"
}

# Replace all occurences of $1 with $2 in files matching $3
# Example: replace_in_place foo bar txt
replace_in_place() {
  local oldtext="$1"
  local newtext="$2"
  local fileglob="*$3*"

  find . -type f -name $fileglob -exec sed -i '' -e "s/$oldtext/$newtext/g" {} \;
}

#-------------------------------------------------------------------------------
# aliases

# I'm really more of a dog person
alias dog="cat"
alias less="bat"

# oh-my-zsh default aliases this to 'ls -l'
alias ll="ls -hAl"

alias o=open

# Shows how much space each directory/file in the current directory is taking up
alias dush="du -sh ./*"

alias sc="shellcheck"

alias underscore2dash='rename "s/_/-/g" ./*'

# nice path
alias np="echo \"\$PATH\" | tr : '\n'"

# homebrew update all
alias hbua="brew update && brew upgrade && brew cleanup"
alias bs="brew services"

# bundler
alias b="bundle check 2>/dev/null || bundle install"
alias bb="bundle check 2>/dev/null || bundle config --local frozen false  && bundle install && bundle config --local frozen true"
alias bi="bundle install"
alias bus="bundle update --source"
alias bu="bundle update"
alias be="bundle exec"
alias ber="bundle exec rake"
alias bers="bundle exec rspec"
alias beru="bundle exec ruby"

# rspec && unicorn
alias ru="bundle exec rspec && unicornleap -s 1.5"
alias rr="bundle exec rubocop -D && bundle exec rspec && unicornleap -s 1.5"

# rails stuff
alias ret="RAILS_ENV=test"
alias red="RAILS_ENV=development"
alias rep="RAILS_ENV=production"

alias taildev="tail -f log/development*"
alias tailprod="tail -f log/production*"

# rake aliases
alias mst="bundle exec rake db:migrate:status"

alias dcl="bundle exec rake db:drop db:create db:schema:load"
alias dcls="bundle exec rake db:drop db:create db:schema:load db:seed"

alias dcm="bundle exec rake db:drop db:create db:migrate"
alias dcms="bundle exec rake db:drop db:create db:migrate db:seed"

# clojure
alias lt="lein test"
alias lr="lein repl"
alias lc="lein clean"
alias lds="lein deps"
alias lfa="lein cljfmt fix && lein how-to-ns fix"

# mix / elixir
alias mc="mix compile"
alias mch="mix coveralls.html"
alias met="MIX_ENV=test"
alias med="MIX_ENV=dev"
alias me="mix espec"
alias mdg="mix deps.get"
alias mps="mix phoenix.server"
alias mpr="mix phoenix.routes"

alias imps="iex -S mix phoenix.server"
alias ie="iex -S mix espec"

alias ee="mix credo --strict && mix espec --cover && unicornleap -s 1.2 --unicorn phoenix.png --sparkle flame.png"
alias eee="mix credo --strict && mix espec --cover && npm test && unicornleap -s 1.2 --unicorn phoenix.png --sparkle flame.png"
alias eov="mix coveralls.html; open cover/excoveralls.html"

# javascript / npm
alias ne="npm run exec -- "
alias tt="npm run lint && CI=true npm run test && unicornleap -s 1.5 --unicorn ~/.unicornleap/narwhal.png"

# emacs
alias ce="cask exec"
alias cee="cask exec ecukes"
alias cer="cask exec ert-runner"

# postgresql
alias pginit="initdb /usr/local/var/postgres -E utf8 -U mike"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias pgstatus="pg_ctl -D /usr/local/var/postgres status"
alias pg="psql -h localhost -d postgres"
alias pgc="pgcli"

# terraform
alias tf="terraform"

# cd aliases
alias cdA="cd /Applications"
alias cdb="cd ~/books"
alias cdc="cd ~/code"
alias cdC="cd ~/code/c"
alias cdcj="cd ~/code/clojure"
alias cdd="cd ~/Downloads"
alias cdde="cd ~/Desktop"
alias cdD="cd ~/Documents"
alias cd.="cd ~/dotfiles"
alias cde="cd ~/thoughts/emacs"
alias cdj="cd ~/code/javascript"
alias cdl="cd ~/code/lisp"
alias cdm="cd ~/code/mikendotio"
alias cdM="cd ~/Movies"
alias cdP="cd ~/Pictures"
alias cdpm="cd ~/code/rails/programmer-mike"
alias cdr="cd ~/code/ruby"
alias cdra="cd ~/code/rails"
alias cdrr="cd ~/code/ruby-refactor"
alias cdrw="cd ~/code/rails/recursewords"
alias cdsq="cd ~/code/sendql"
alias cdt="cd ~/thoughts"

# Java command line
alias vb="javac -cp .:junit-4.11.jar:hamcrest-core-1.3.jar *.java"
alias vt="java -cp .:junit-4.11.jar:hamcrest-core-1.3.jar org.junit.runner.JUnitCore VideoStoreTest"

alias un7zip="7z x"

alias g="git"
alias gx="gitx &"
alias git-prune-merged-remote="git branch --remote --merged | grep -v /main | sed 's/origin\///' | xargs -n 1 git push --delete origin"
alias git-prune-merged-local="git checkout main && git branch --merged | grep -v '* main' | xargs -n 1 git branch --delete"

alias pwdp="pwd -P"

alias killspring="ps aux | egrep 'spring (app|server)' | tr -s ' ' | cut -d' ' -f2 | xargs -n 1 kill -9"
alias killpuma="ps aux | grep -v grep | grep puma | cut -d ' ' -f10 | xargs -n 1 kill"

alias wh="which"

alias tree="\tree -I 'node_modules|coverage|__pycache__' "
alias t="tree"
alias t1="tree -L 1"
alias t2="tree -L 2"
alias t3="tree -L 3"
alias t4="tree -L 4"
alias t5="tree -L 5"
alias t6="tree -L 6"
alias t7="tree -L 7"

# make
alias mt="make test && unicornleap -s 1.5"

# haskell
alias cdh="cd ~/code/haskell"

alias hst="history"

alias rg="rg -S"

#-------------------------------------------------------------------------------
# machine-specific setup

if [ "$(whoami)" = "mike" ]; then
  source ~/dotfiles/home.sh
elif [ "$(whoami)" = "mikenichols" ]; then
  source ~/dotfiles/work.sh
fi

if [ "$MIKEN_OS" = "mac" ]; then
  source ~/dotfiles/common-mac.sh
elif [ "$MIKEN_OS" = "linux" ]; then
  source ~/dotfiles/common-linux.sh
  eval "$(ssh-agent -s)" > /dev/null
fi

#-------------------------------------------------------------------------------
# Docker

alias di="docker images"
alias dp="docker ps -a"

alias dattach='docker attach "$(basename $(pwd)"'
alias dbuild='docker build -t "$(basename $(pwd))" .'
alias dshell='docker exec -it "$(basename $(pwd))" /bin/bash'
alias dshelli='docker run -it "$(basename $(pwd))" /bin/bash'

alias dcleani='docker rmi $(docker images -aq)'
alias dcleanc="docker ps -aq -f status=exited | xargs -n 1 docker rm -v"

drun() {
  docker run "$(basename $(pwd))" "$1"
}

#-------------------------------------------------------------------------------
# Misc.

# Nicer colors for emacs in terminal
export TERM=xterm-256color

if type fuck &> /dev/null; then
  eval "$(thefuck --alias)"
fi

export NVM_DIR="$HOME/.nvm"
if [ -f "/usr/local/opt/nvm/nvm.sh" ]; then
  . "/usr/local/opt/nvm/nvm.sh"
fi

# termcap terminfo
# ks      smkx      make the keypad send commands
# ke      rmkx      make the keypad send digits
# vb      flash     emit visual bell
# mb      blink     start blink
# md      bold      start bold
# me      sgr0      turn off bold, blink and underline
# so      smso      start standout (reverse video)
# se      rmso      stop standout
# us      smul      start underline
# ue      rmul      stop underline

# Add some colour to LESS/MAN pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$(tput bold; tput setaf 4)
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$(tput bold; tput setaf 160; tput setab 11)
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$(tput bold; tput setaf 2)
export LESS_TERMCAP_zzz=$'\E[0m'

# screen -S mike<n> "-e^Ss"

#-------------------------------------------------------------------------------
# rbenv

if type rbenv &> /dev/null; then
  eval "$(rbenv init -)"
fi

# For nokogiri to not shit the bed
export PKG_CONFIG_PATH=/usr/local/opt/libxml2/lib/pkgconfig

#-------------------------------------------------------------------------------
# pyenv

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

#-------------------------------------------------------------------------------
# Homebrew

eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_ANALYTICS=1

#-------------------------------------------------------------------------------

## end common.sh
