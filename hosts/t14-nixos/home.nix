{ config, lib, pkgs, ... }:
{
  imports = [
    ../common.nix
    ../common-linux.nix
    ../../programs/non-free.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  services.emacs = {
    enable = true;
    package = pkgs.emacsNativeCompSebastiant;
  };

  home.packages = with pkgs; [
    zlib
    dmenu
    arandr
    picom
    espeak
    blink1-tool
    flameshot
  ];

  home.file.".xsessionrc".text = ''
    xset r rate 200 50
    '';
  home.file.".background-image/nixos-wallpaper.png".source = ./nixos-wallpaper.png;
}
