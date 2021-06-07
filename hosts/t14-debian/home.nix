{ config, pkgs, ... }:
{
  programs.home-manager.enable = true;
  imports = [
    ../../common.nix
    ../../programs/tmux.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    zlib
    dmenu
    autorandr
    arandr
    picom
  ];

  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad.hs;
  xdg.configFile."polybar/config".source = ../../programs/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../../programs/polybar/launch.sh;
}
