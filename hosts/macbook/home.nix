{ lib, config, pkgs, nixpkgs, ... }:
let
  nixFlakes = (pkgs.writeScriptBin "nixFlakes" ''
      exec ${pkgs.nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$@"
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
  programs.emacs.package = pkgs.emacsSebastiant;
}
