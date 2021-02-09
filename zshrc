export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-no-magic"

CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"
plugins=(git tmuxinator nix-shell)

# User configuration
alias ls="ls -lF"
alias bat="batcat"

export PATH="/run/current-system/sw/bin:/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:/usr/local/git/bin:/usr/texbin:$HOME/.local/bin"
export EDITOR='vim'
export TERM="xterm-256color"

source $ZSH/oh-my-zsh.sh

setopt interactivecomments

# fzf
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
  bindkey -r '^T'
  bindkey '^B' fzf-history-widget
fi

#Nix
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi

# GHC (Haskell)
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"


# direnv
eval "$(direnv hook zsh)"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
