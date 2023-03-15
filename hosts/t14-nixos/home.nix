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
    package = pkgs.emacsPgtkSebastiant;
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
      extraPolicies = {
      ExtensionSettings = {};
    };
  };
  programs.emacs.package = pkgs.emacsPgtkSebastiant;

  home.file.".xsessionrc".text = ''
    xset r rate 200 50
    '';
  home.file.".background-image/nixos-wallpaper.png".source = ./nixos-wallpaper.png;
  xdg.configFile."sway/config".source = ../../programs/sway/config;
  xdg.configFile."waybar/config".source = ../../programs/waybar/config;
}
