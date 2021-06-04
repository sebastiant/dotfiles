{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    initExtraBeforeCompInit = builtins.readFile ../../zshrc;
  };
  programs.fzf.enableZshIntegration = true;
}
