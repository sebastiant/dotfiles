{ config, pkgs, nixpkgs, lib, ... }:
let
  nixFlakes = (pkgs.writeScriptBin "nixFlakes" ''
      exec ${pkgs.nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$@"
    '');
in {
  imports = [
    ../common.nix
    ../common-linux.nix
  ];

  programs.zsh.shellAliases.bat = "batcat";

  home.packages = with pkgs; [
    zlib
    dmenu
    autorandr
    arandr
    picom
    espeak
    blink1-tool
    flameshot
    nyxt
    byzanz
    xrectsel
    dunst
    libnotify
    gnome-icon-theme
    _1password
    nixFlakes
    xmonad-with-packages
  ];

  home.file.".xsessionrc".source = ./xsessionrc;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "1password"
    "skypeforlinux"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-with-extensions"
    "zoom"
  ];
}
