{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];
  boot.loader.systemd-boot.enable = true;

  services.xserver.libinput.enable = lib.mkDefault true;
  networking.networkmanager.enable = true;
  networking.hostName = "t14";

  time.timeZone = "Europe/Stockholm";

  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.xserver = {
    enable = true;
    displayManager.defaultSession = "none+xmonad";
    displayManager.gdm.enable = true;
    desktopManager.gnome3.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
  displayManager.sessionCommands = with pkgs; lib.mkAfter
    ''
    xmodmap ~/.Xmodmap
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
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
  };
  nix.allowedUsers = [
    "sebastian"
  ];

  fonts.fonts = with pkgs; [
      iosevka
  ];

  environment.systemPackages = with pkgs; [
    zsh
    oh-my-zsh
    vim
    ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
    ]))
    alacritty
    arandr
    autorandr
    feh
    picom
    dmenu
    jdk14
    python3Full
    python38Packages.pip
    postgresql
    git
    wget
    pciutils
    tree
    ripgrep
    killall
    firefox
    jetbrains.pycharm-community
    vscode-with-extensions
    skypeforlinux
    zoom-us
    slack
    spotify
    docker-compose
    terraform
    ansible
    direnv
    pavucontrol
    polybar
    dunst
    cmake
    gcc
    gnumake
    libtool
    ghc
  ];

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;
  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "python" "man"];
      theme = "af-no-magic";
      custom = "$HOME/.oh-my-zsh-custom";
    };
  };

  system.stateVersion = "20.09";
}

