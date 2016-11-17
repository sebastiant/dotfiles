export ZSH=$HOME/.oh-my-zsh


ZSH_THEME="af-magic"

CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"
plugins=(git)

# User configuration
alias ls="ls -lF"

export PATH="/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:/usr/local/git/bin:/usr/texbin"
export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"
export EDITOR='vim'
export TERM="xterm-256color"

source $ZSH/oh-my-zsh.sh
source ~/.bin/tmuxinator.zsh

setopt interactivecomments
