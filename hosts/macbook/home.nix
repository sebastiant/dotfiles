{ config, pkgs, ... }:
{
  home-manager.useUserPackages = true;
  home-manager.users.sebastian = { pkgs, ... }: {
    imports = [
      ../common.nix
    ];
    home.packages = with pkgs; [
      vscode-with-extensions
    ];
  };
}
