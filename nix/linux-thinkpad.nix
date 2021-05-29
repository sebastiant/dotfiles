{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    zlib
    xmonad-with-packages
    dmenu
    autorandr
    arandr
    picom
  ];
}
