{ config, pkgs, ... }:
{
  imports = [
    ../common.nix
    ../common-linux.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  services.emacs.enable = true;

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
}
