export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-magic"

CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"
plugins=(git)

# User configuration
alias ls="ls -lF"

export PATH="/run/current-system/sw/bin:/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:/usr/local/git/bin:/usr/texbin"
export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"
export LC_ALL=sv_SE.UTF-8 
export LANG=sv_SE.UTF-8
export EDITOR='vim'
export TERM="xterm-256color"

source $ZSH/oh-my-zsh.sh
source ~/.bin/tmuxinator.zsh

setopt interactivecomments

# GHC (Haskell)
if which jenv > /dev/null; then eval "$(jenv init -)"; fi
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# fzf
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
  bindkey -r '^T'
  bindkey '^B' fzf-history-widget
fi
