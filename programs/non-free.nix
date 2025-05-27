{ lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "1password"
    "1password-cli"
    "1password-gui"
    "discord"
    "dropbox"
    "ngrok"
    "onepassword-password-manager"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "vscode"
    "vscode-extension-MS-python-vscode-pylance"
    "vscode-extension-ms-vsliveshare-vsliveshare"
    "zoom"
  ];
}
