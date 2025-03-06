{ pkgs, ... }: {
  imports = [ ../programs/tmux/tmux.nix ];
  programs.home-manager.enable = true;
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
        format = ''
          <b>%s</b>
          %b'';
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
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };
  };
  services.blueman-applet.enable = true;
  programs.vscode.profiles.default.extensions =
    [ pkgs.vscode-extensions.ms-vsliveshare.vsliveshare ];

  home.packages = with pkgs; [
    age
    _1password-gui
    comma
    feh
    git-mob
    haskellPackages.yeganesh
    networkmanager
    polybar
    powertop
    volctl
  ];
  programs.firefox = {
    enable = true;
    profiles.default = {
      id = 0;
      name = "Default";
      isDefault = true;
      settings = {
        "app.update.auto" = false;
        "browser.startup.homepage" = "about:blank";
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "gfx.webrender.all" = true;
        "gfx.webrender.enabled" = true;
        "media.av1.enabled" = false;
        "media.ffmpeg.vaapi.enabled" = true;
        "media.hardware-video-decoding.force-enabled" = true;
        "media.navigator.mediadatadecoder_vpx_enabled" = true;
        "signon.rememberSignons" = false;
      };
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
        onepassword-password-manager
        ublock-origin
        vim-vixen
      ];
    };

  };

  home.file.".config/syncorate/config.yaml".source =
    ../programs/syncorate/config.linux.yaml;
  home.file.".xmonad/xmonad.hs".source = ../programs/xmonad.hs;
  xdg.configFile."polybar/config".source = ../programs/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../programs/polybar/launch.sh;
}
