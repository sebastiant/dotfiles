{ config, pkgs, nixpkgs, ... }:
{
  home-manager.useUserPackages = true;
  home-manager.users.sebastian = { pkgs, ... }: {
    imports = [
      ../common.nix
    ];
    home.packages = with pkgs; [
      vscode-with-extensions
      google-cloud-sdk
    ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "skypeforlinux"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-with-extensions"
    "zoom"
  ];
}
