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
    package = pkgs.emacsSebastiant;
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
  programs.firefox.package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
    forceWayland = true;
    extraPolicies = {
      ExtensionSettings = {};
    };
  };

  home.file.".xsessionrc".text = ''
    xset r rate 200 50
    '';
  home.file.".background-image/nixos-wallpaper.png".source = ./nixos-wallpaper.png;
  xdg.configFile."sway/config".source = ../../programs/sway/config;
}
