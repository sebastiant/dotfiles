{ config, pkgs, ... }:
{
  imports = 
  [
    ./neovim.nix
    ./git.nix
  ];
  home = {
    username = "sebastian";
    homeDirectory = "/home/sebastian";
    stateVersion = "21.05";
    packages = with pkgs; [
      pandoc
      cabal-install
      ghc
      haskell-language-server
      stack
      ripgrep
      tree
    ];
  };

  programs = {
    home-manager.enable = true;
    bat = {
      enable = true;
      config.theme = "ansi-dark";
    };
  };
}
