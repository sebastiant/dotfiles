{ config, pkgs, ... }:
{
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    zlib
    dmenu
    autorandr
    arandr
    picom
  ];
}
