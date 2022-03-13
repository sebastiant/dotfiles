{ config, lib, nixpkgs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../programs/cachix/cachix.nix
    ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  services.xserver.libinput.enable = lib.mkDefault true;

  time.timeZone = "Europe/Stockholm";

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s20f0u2u1.useDHCP = true;
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
    hostName = "t14";
    networkmanager.enable = true;
  };

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
  displayManager.sessionCommands = lib.mkAfter ''
    ${pkgs.xorg.xset}/bin/xset r rate 200 50
    ${pkgs.xorg.xmodmap} ~/.Xmodmap
    '';
  };
  services.autorandr = {
    enable = true;
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sebastian = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "docker"
      "vboxusers"
      "video"
      "audio"
      "disk"
      "networkmanager"
    ];
    home = "/home/sebastian";
    uid = 1000;
    shell = pkgs.zsh;
  };
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    package = pkgs.nixUnstable;
  };

  fonts.fonts = with pkgs; [
      iosevka
      aileron
  ];

  environment.systemPackages = with pkgs; [
    alacritty
    autorandr
    feh
    wget
    pciutils
    killall
    firefox
    vscode-with-extensions
    spotify
    skypeforlinux
    zoom-us
    slack
    docker-compose
    direnv
    pavucontrol
    polybar
    dunst
    cmake
    gcc
    gnumake
    libtool
  ];

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "skypeforlinux"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-with-extensions"
    "zoom"
    "1password"
  ];

  system.stateVersion = "22.05";
}
