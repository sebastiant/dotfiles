{ pkgs, ... }:
let
  nixFlakes = (pkgs.writeScriptBin "nixFlakes" ''
      exec ${pkgs.nixVersions.latest}/bin/nix --experimental-features "nix-command flakes" "$@"
    '');
in {
  imports = [
   ../common.nix
   ../../programs/non-free.nix
 ];
 home.packages = with pkgs; [
   zlib
   google-cloud-sdk
   nixFlakes
 ];
  home.file.".config/syncorate/config.yaml".source = ../../programs/syncorate/config.macos.yaml;
}
