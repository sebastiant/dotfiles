{ config, pkgs, ... }:
let
  unstable = import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  };

in {
  programs.home-manager.enable = true;
  imports = [
    ../common.nix
    ../../programs/tmux/tmux.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  programs.emacs.package = unstable.emacsGcc;

  home.packages = with pkgs; [
    zlib
    dmenu
    autorandr
    arandr
    picom
    espeak
    blink1-tool
  ];

  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad.hs;
  xdg.configFile."polybar/config".source = ../../programs/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../../programs/polybar/launch.sh;
}
