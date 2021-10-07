{ config, pkgs, ... }:
{
  imports = [
    ../common.nix
    ../common-linux.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    zlib
    dmenu
    arandr
    picom
    espeak
    blink1-tool
    flameshot
  ];
}
