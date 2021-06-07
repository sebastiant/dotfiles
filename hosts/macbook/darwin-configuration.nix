{ config, pkgs, ... }:
{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;
  home-manager.users.sebastian = { pkgs, ... }: {
    imports = [
      ../common.nix
    ];
  };

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
    ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ fira-code iosevka];
  };

  programs.zsh.enable = true;

  system.stateVersion = 4;
}
