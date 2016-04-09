export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-magic"

CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"
plugins=(git)

# User configuration
alias ls="ls -lF"

export PATH="~/pebble-dev/PebbleSDK-current/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin"
export JAVA_HOME="$(/usr/libexec/java_home -v 1.7)"

source $ZSH/oh-my-zsh.sh

setopt interactivecomments
