{ pkgs, nix, nixpkgs, config, lib, ... }:
{
  imports = [
    ../../programs/non-free.nix
  ];
  environment.systemPackages = with pkgs;
    [
      ( python38.withPackages (ps: with ps; [ pip flake8 black pynvim ipython python-language-server.override { pylint = null; } ]) )
      alacritty
      cabal-install
      coreutils
      docker
      ffmpeg
      fswatch
      fzf
      gnupg
      home-manager
      irssi
      nodejs
      openssl
      reattach-to-user-namespace
      ripgrep
      sqlite
      tig
      tree
      wget
      yarn
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
    distributedBuilds = true;
    buildMachines = [ {
      hostName = "nix-docker";
      sshUser = "root";
      sshKey = "/etc/nix/docker_rsa";
      systems = [ "x86_64-linux" ];
      maxJobs = 2;
    } ];
  };
  services.nix-daemon.enable = true;
}
