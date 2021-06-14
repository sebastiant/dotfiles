{ config, pkgs, ... }:
{
  home-manager.useUserPackages = true;
  home-manager.users.sebastian = { pkgs, ... }: {
    imports = [
      ../common.nix
    ];
  };
}
