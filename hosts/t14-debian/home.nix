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

  services.emacs.enable = true;

  programs.zsh.shellAliases.bat = "batcat";

  home.packages = with pkgs; [
    _1password
    arandr
    autorandr
    blink1-tool
    byzanz
    dmenu
    dunst
    esbuild
    espeak
    flameshot
    gnome-icon-theme
    libnotify
    mysql
    nixFlakes
    nyxt
    pavucontrol
    picom
    sass
    spotify
    vscode
    xmonad-with-packages
    xrectsel
    zlib
  ];

  home.file.".xsessionrc".text = ''
    export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$HOME/.local/bin:$PATH
    xset r rate 200 50
    '';

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "1password"
    "onepassword-password-manager"
    "skypeforlinux"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-with-extensions"
    "zoom"
  ];
}
