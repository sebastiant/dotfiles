{ config, pkgs, nixpkgs, ... }:
let
  nixFlakes = (pkgs.writeScriptBin "nixFlakes" ''
      exec ${pkgs.nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$@"
    '');
in {
  imports = [
   ../common.nix
 ];
 home.packages = with pkgs; [
   vscode-with-extensions
   google-cloud-sdk
   nixFlakes
 ];
}
