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
    extraGroups = [ "wheel" ];
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
    emacs
    alacritty
    arandr
    autorandr
    feh
    picom
    xmobar
    dmenu
    stalonetray
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
    docker
    docker-compose
    ansible
    direnv
  ];
    
  nixpkgs.config.allowUnfree = true;
  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "python" "man"];
      theme = "agnoster";
    };
  };

  system.stateVersion = "20.09";
}

