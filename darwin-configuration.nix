{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs;
    [ 
      ( python38.withPackages (ps: with ps; [ pip flake8 black pynvim ipython python-language-server.override { pylint = null; } ]) )
      zsh
      vimHugeX
      tmux
      tmuxinator
      httpie
      ffmpeg
      gnupg
      openssl
      yarn
      coreutils
      htop
      httpie
      ripgrep
      fzf
      git
      irssi
      tig
      tree
      wget
      fswatch
      sqlite
      nodejs
      docker
      reattach-to-user-namespace
      sass
      alacritty
    ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ fira-code iosevka];
  };

  programs.zsh.enable = true;

  system.stateVersion = 4;
}
