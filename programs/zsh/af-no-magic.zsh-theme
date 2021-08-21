# A stripped version of af-magic.zsh-theme
# Modifications:
# - No separator line before prompt
# - No "right prompt" with username & hostname
# - $ instead of »
# - Displays when in nix-shell
# Repository of original version: https://github.com/andyfleming/oh-my-zsh

if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

eval my_gray='$FG[237]'
eval my_orange='$FG[214]'

nix_shell_info() {
  if [[ -n "$IN_NIX_SHELL" ]]; then
    echo -n "$my_orange(nix-shell)%{$reset_color%}"
  fi
}


# primary prompt
PROMPT='$FG[237]%{$reset_color%}
$FG[032]%~\
$(git_prompt_info)\
$(nix_shell_info)\
$FG[105]%(!.#.$)%{$reset_color%} '
PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
RPS1='${return_code}'

# git settings
ZSH_THEME_GIT_PROMPT_PREFIX="$FG[075](branch:"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="$my_orange*%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="$FG[075])%{$reset_color%}"
