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
    userSettings = {
      "editor.fontFamily" = "Iosevka";
      "editor.fontLigatures" = true;
      "editor.fontSize" = 13;
    };
  };
 }
