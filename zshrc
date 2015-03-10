export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-magic"

CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"
plugins=(git)

# User configuration

export PATH="~/pebble-dev/PebbleSDK-current/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin"

source $ZSH/oh-my-zsh.sh

setopt interactivecomments
