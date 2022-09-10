{ config, lib, nixpkgs, pkgs, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
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

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "performance";
  };

  services.throttled.enable = true;

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
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.dbus.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-wlr
    ];
  };
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export _JAVA_AWT_WM_NONREPARENTING=1
      export MOZ_ENABLE_WAYLAND = "1";
      export XDG_CURRENT_DESKTOP = "sway";
    '';
  };

  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];

  services.autorandr = {
    enable = true;
  };
  services.udev.extraRules = "ATTRS{idVendor}==\"27b8\", ATTRS{idProduct}==\"01ed\", MODE:=\"666\", GROUP=\"plugdev\"\n";


  services.xserver.layout = "us";

  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    settings = {
      paint-on-overlay = true;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
    };
  };

  boot.initrd.kernelModules = [ "i915" ];

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
    LIBVA_DRIVER_NAME = "iHD";
    MOZ_DISABLE_RDD_SANDBOX = "1";
  };
  sound.enable = true;

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
    nixPath = [
        "nixpkgs=${nixpkgs}"
    ];
    registry.nixpkgs.flake = nixpkgs;
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
    ] ++
    [
      sway
      wayland
      glib
      swaylock
      swayidle
      wl-clipboard
      waybar
    ];

  services.cron.enable = true;

  virtualisation = {
    docker = {
      enable = true;
      rootless.enable = true;
    };
    virtualbox.host.enable = true;
  };

  networking.firewall = {
    enable = true;
    trustedInterfaces = [ "docker0" ];
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
