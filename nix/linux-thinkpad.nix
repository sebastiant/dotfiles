{ config, pkgs, ... }:
{
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    zlib
    xmonad-with-packages
    dmenu
    autorandr
    arandr
    picom
  ];
}
