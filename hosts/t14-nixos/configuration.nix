{ config, lib, nixpkgs, pkgs, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ../../programs/cachix/cachix.nix
      ../../programs/non-free.nix
    ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  time.timeZone = "Europe/Stockholm";

  networking = {
    useDHCP = false;
    interfaces = {
      wlp0s20f3.useDHCP = true;
    };
    hostName = "t14";
    networkmanager.enable = true;
  };
  programs.nm-applet.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];
    displayManager.lightdm.greeters.mini = {
      enable = true;
      user = "sebastian";
      extraConfig = ''
        [greeter]
        show-password-label = false
        [greeter-theme]
        window-color = "#A0A0A0"
      '';
    };
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

  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];

  services.autorandr = {
    enable = true;
  };
  services.udev.extraRules = "ATTRS{idVendor}==\"27b8\", ATTRS{idProduct}==\"01ed\", MODE:=\"666\", GROUP=\"plugdev\"\n";

  services.xserver.layout = "us";

  boot.initrd.kernelModules = [ "i915" ];

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
  };
  sound.enable = true;
  hardware.pulseaudio.enable = true;

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

  environment.systemPackages =
    with pkgs; [
    alacritty
    autorandr
    binutils
    feh
    wget
    pciutils
    intel-gpu-tools
    killall
    libva-utils
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
    vdpauinfo
    ];

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  systemd.user.services.dropbox = {
    description = "Dropbox";
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  system.stateVersion = "22.05";
}
