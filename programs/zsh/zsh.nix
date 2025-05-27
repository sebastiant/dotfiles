{ lib, ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    initContent = lib.mkOrder 550 (builtins.readFile ./zshrc);
    oh-my-zsh = {
      enable = true;
      theme = "af-no-magic";
      plugins = [
        "git"
        "nix-shell"
        "direnv"
      ];
    };
  };

  home.file.".config/oh-my-zsh/themes/af-no-magic.zsh-theme".source = ./af-no-magic.zsh-theme;
}
