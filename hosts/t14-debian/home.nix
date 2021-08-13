{ config, pkgs, ... }:
let
  unstable = import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  };

in {
  programs.home-manager.enable = true;
  imports = [
    ../common.nix
    ../../programs/tmux/tmux.nix
  ];
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        geometry = "600x50-50+65";
        follow = "keyboard";
        transparency = 10;
        frame_color = "#E06C75";
        font = "Iosevka Regular 18";
        padding = 8;
        horizontal_padding = 8;
        frame_width = 3;
        line_height = 4;
        format = "<b>%s</b>\n%b";
        show_age_threshold = 60;
        separator_height = 2;
        separator_color = "frame";
        markup = "full";
        ignore_newline = "no";
        word_wrap = "yes";
        alignment = "left";
      };
      urgency_low = {
        background = "#282C34";
        foreground = "#ABB2BF";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282C34";
        foreground = "#ABB2BF";
        timeout = 10;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#FFFFFF";
        frame_color = "#FF0000";
        timeout = 0;
      };
    };
    iconTheme = {
      package = pkgs.gnome3.adwaita-icon-theme;
      name = "Adwaita";
    };
  };

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
  ];

  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad.hs;
  xdg.configFile."polybar/config".source = ../../programs/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../../programs/polybar/launch.sh;
}
