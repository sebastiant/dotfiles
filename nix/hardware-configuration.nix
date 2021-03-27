{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];
  boot = {
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "nvme"
        "usb_storage"
        "sd_mod"
        "rtsx_pci_sdmmc"
      ];
      kernelModules = [ ];
    };
    kernelModules = [
      "kvm-intel"
      "acpi_call"
    ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelParams = [ "acpi_backlight=native" ];
    kernelPackages = pkgs.linuxPackages_latest;
    blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
      "ath3k"
    ];
  };
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
  environment.systemPackages = with pkgs; [ alsa-firmware] ;
  services = {
    tlp.enable = lib.mkDefault true;
    xserver.libinput.enable = true;
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2718dc21-5e42-4821-9314-0fc43604bc6f";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/E5BB-8B46";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/e2ce9ad6-6bf1-4ea0-aa11-5a56f73929e3"; }
    ];

  networking = {
    networkmanager.enable = true;
    hostName = "t14";
  };
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
