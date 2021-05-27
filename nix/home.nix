{ config, pkgs, ... }:
{
  imports = 
  [
    ./neovim.nix
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
      zlib
      xmonad-with-packages
      dmenu
      ripgrep
      tree
      autorandr
      arandr
      picom
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
