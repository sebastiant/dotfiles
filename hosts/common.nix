{ config, pkgs, nixpkgs, lib, ... }:
{
  imports =
    [
      ../programs/zsh/zsh.nix
      ../programs/neovim/neovim.nix
      ../programs/git.nix
      ../programs/emacs/emacs.nix
    ];

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "vscode"
    "vscode-with-extensions"
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      nixops
      lorri
      pandoc
      cabal-install
      ghc
      haskell-language-server
      stack
      pyright
      ripgrep
      tree
      zathura
      sqlite
      mypy
      syncthing
    ];
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fzf.enable = true;
  programs.bat = {
    enable = true;
    config.theme = "ansi-dark";
  };

  xdg.configFile."alacritty/alacritty.yml".source = ../programs/alacritty.yml;
  xdg.configFile."oh-my-zsh/plugins/nix-shell".source = pkgs.fetchFromGitHub {
    owner = "chisui";
    repo = "zsh-nix-shell";
    rev = "f8574f27e1d7772629c9509b2116d504798fe30a";
    sha256 = "0svskd09vvbzqk2ziw6iaz1md25xrva6s6dhjfb471nqb13brmjq";
  };
}
