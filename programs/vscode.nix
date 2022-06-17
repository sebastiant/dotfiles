{ config, pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      ms-vsliveshare.vsliveshare
      ms-python.python
      ms-python.vscode-pylance
      eamodio.gitlens
    ];
  };
 }
