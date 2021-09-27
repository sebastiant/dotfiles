{ pkgs, nix, nixpkgs, config, lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "skypeforlinux"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-with-extensions"
    "zoom"
  ];

  environment.systemPackages = with pkgs;
    [
      ( python38.withPackages (ps: with ps; [ pip flake8 black pynvim ipython python-language-server.override { pylint = null; } ]) )
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
      irssi
      tig
      tree
      wget
      fswatch
      sqlite
      nodejs
      docker
      reattach-to-user-namespace
      alacritty
      ghc
      cabal-install
      syncthing
    ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      fira-code
      iosevka
      aileron
    ];
  };

  programs.zsh.enable = true;

  system.stateVersion = 4;
  users = {
    users.sebastian = {
      home = /Users/sebastian;
    };
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
