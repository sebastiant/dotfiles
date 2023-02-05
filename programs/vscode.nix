{ config, pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      eamodio.gitlens
      haskell.haskell
      justusadam.language-haskell
      ms-python.python
      ms-python.vscode-pylance
      dart-code.flutter
      dart-code.dart-code
    ];
    userSettings = {
      "editor.fontFamily" = "Iosevka";
      "editor.fontLigatures" = true;
      "editor.fontSize" = 13;
      "workbench.colorTheme" = "Solarized Dark";
    };
  };
 }
