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
}
