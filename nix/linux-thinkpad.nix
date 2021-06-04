{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
    ./programs/tmux.nix
  ];

  home.packages = with pkgs; [
    zlib
    dmenu
    autorandr
    arandr
    picom
  ];

  home.file.".xmonad/xmonad.hs".source = ../x11/xmonad.hs;
  xdg.configFile."polybar/config".source = ../x11/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../x11/polybar/launch.sh;
}
