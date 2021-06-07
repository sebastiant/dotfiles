{ config, pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    tmuxinator.enable = true;
    baseIndex = 1;
    keyMode = "vi";
    prefix = "C-b";
    extraConfig = builtins.readFile ./tmux.conf;
  };
}
