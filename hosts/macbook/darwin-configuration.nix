{ pkgs, nix, nixpkgs, config, lib, ... }:
{
  imports = [
    ../../programs/non-free.nix
  ];
  environment.systemPackages = with pkgs;
    [
      alacritty
      cabal-install
      coreutils
      docker
      ffmpeg
      fswatch
      fzf
      gnupg
      home-manager
      nodejs
      openssl
      reattach-to-user-namespace
      sqlite
      tig
      tree
      wget
      yarn
    ];

  fonts = {
    packages = with pkgs; [
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
    nixPath = lib.mkForce [
      "nixpkgs=${nixpkgs}"
    ];
    package = pkgs.nixVersions.latest;
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
