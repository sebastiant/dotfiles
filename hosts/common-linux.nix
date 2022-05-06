{ pkgs, ...}:
{
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

  services.blueman-applet.enable = true;

  home.packages = with pkgs; [
    feh
    mob
    networkmanager
    polybar
    volctl
    zeal
  ];
  programs.firefox = {
    enable = true;
    profiles.default = {
      id = 0;
      name = "Default";
      isDefault = true;
      settings = {
        "app.update.auto" = false;
        "signon.rememberSignons" = false;
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "browser.startup.homepage" = "about:blank";
      };
    };
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      onepassword-password-manager
      ublock-origin
      vim-vixen
    ];
  };

  home.file.".xmonad/xmonad.hs".source = ../programs/xmonad.hs;
  xdg.configFile."polybar/config".source = ../programs/polybar/config;
  xdg.configFile."polybar/launch.sh".source = ../programs/polybar/launch.sh;
}
