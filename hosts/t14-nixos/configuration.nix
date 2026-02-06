{ lib, nixpkgs, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ../../programs/non-free.nix ];
  nixpkgs.overlays = [ (import ../../programs/wayland-overlay.nix) ];

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

  # Taken from https://gitlab.com/xaverdh/my-nixos-config/-/blob/master/per-box/tux/default.nix
  systemd.timers.suspend-on-low-battery = {
    wantedBy = [ "multi-user.target" ];
    timerConfig = {
      OnUnitActiveSec = "120";
      OnBootSec= "120";
    };
  };
  systemd.services.suspend-on-low-battery =
    let
      battery-level-sufficient = pkgs.writeShellScriptBin
        "battery-level-sufficient" ''
        test "$(cat /sys/class/power_supply/BAT0/status)" != Discharging \
          || test "$(cat /sys/class/power_supply/BAT0/capacity)" -ge 5
      '';
    in
      {
        serviceConfig = { Type = "oneshot"; };
        onFailure = [ "suspend.target" ];
        script = "${lib.getExe battery-level-sufficient}";
      };


  services.throttled.enable = true;

  time.timeZone = "Europe/Stockholm";

  networking = {
    useDHCP = false;
    interfaces = { wlp0s20f3.useDHCP = true; };
    hostName = "t14";
    networkmanager.enable = true;
  };
  programs.nm-applet.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.pulseaudio = {
    package = pkgs.pulseaudioFull;
    extraConfig = ''
    load-module module-udev-detect ignore_dB=1
    load-module module-detect
    load-module module-alsa-card device_id="sofhdadsp" tsched=0
    load-module module-alsa-source device_id="sofhdadsp"
    load-module module-alsa-sink device_id="sofhdadsp"
    set-card-profile alsa_card.sofhdadsp output:analog-stereo+input:analog-stereo
    set-default-sink alsa_output.sofhdadsp.analog-stereo
    options snd_hda_intel power_save=0
  '';
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  services.printing.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  services.dbus.enable = true;
  services.dbus.packages =  [ pkgs.pass-secret-service ];
  services.passSecretService.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
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
      export MOZ_ENABLE_WAYLAND=1;
      export XDG_CURRENT_DESKTOP=sway;
    '';
  };
  programs.waybar.enable = true;
  programs.zsh.enable = true;

  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];

  services.autorandr = { enable = true; };

  services.udev = {
    packages = [ pkgs.android-udev-rules ];
    extraRules = ''
      ATTRS{idVendor}=="27b8", ATTRS{idProduct}=="01ed", MODE:="666", GROUP="plugdev"
    '';
  };

  services.xserver.xkb.layout = "us";

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

  services.tailscale.enable = true;

  boot.initrd.kernelModules = [ "i915" ];

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [ intel-media-driver vaapiVdpau libvdpau-va-gl ];
  };

  environment.variables = {
    SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false"; # fixes issue with systemd hibernate and qemu VMs https://github.com/systemd/systemd/issues/33083
    VDPAU_DRIVER = "va_gl";
    LIBVA_DRIVER_NAME = "iHD";
    MOZ_DISABLE_RDD_SANDBOX = "1";
  };

  users.users.sebastian = {
    isNormalUser = true;
    createHome = true;
    extraGroups =
      [ "wheel" "docker" "video" "audio" "disk" "networkmanager" ];
    home = "/home/sebastian";
    uid = 1000;
    shell = pkgs.zsh;
  };
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = [ "nixpkgs=${nixpkgs}" ];
    registry.nixpkgs.flake = nixpkgs;
    package = pkgs.nixVersions.latest;
    settings = {
      substituters = [
        "https://nix-community.cachix.org/"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      allow-import-from-derivation = "true";
    };

  };

  fonts.packages = with pkgs; [ font-awesome iosevka aileron ];

  environment.systemPackages = with pkgs;
    [
      alacritty
      autorandr
      binutils
      brightnessctl
      cmake
      direnv
      docker-compose
      dunst
      feh
      gcc
      gnumake
      intel-gpu-tools
      killall
      libtool
      libva-utils
      pavucontrol
      pciutils
      polybar
      pulseaudio
      slack
      spotify
      vdpauinfo
      wget
      zoom-us
    ] ++ [
      glib
      grim
      slurp
      sway
      swayidle
      swaylock
      wayland
      wdisplays
      wl-clipboard
    ];

  services.cron.enable = true;

  virtualisation.docker = {
    enable = true;
    rootless.enable = true;
  };

  networking.firewall = {
    enable = true;
    trustedInterfaces = [ "docker0" ];
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  systemd.user.services.dropbox = {
    description = "Dropbox";
    wantedBy = [ "default.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/"
        + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/"
        + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      RestartSec = "3";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  system.stateVersion = "22.05";
}
