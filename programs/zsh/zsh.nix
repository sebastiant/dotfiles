{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    initExtraBeforeCompInit = builtins.readFile ./zshrc;
    oh-my-zsh = {
      enable = true;
      theme = "af-no-magic";
      plugins = [
        "git"
        "tmuxinator"
        "nix-shell"
        "direnv"
      ];
    };
  };
  programs.fzf.enableZshIntegration = true;

  home.file.".config/oh-my-zsh/themes/af-no-magic.zsh-theme".source = ./af-no-magic.zsh-theme;
}
