{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-colors-solarized
      vim-airline
      vim-airline-themes
      nerdtree
      vim-markdown
      ctrlp
      vim-indent-guides
    ];
    extraConfig = builtins.readFile ./neovimrc;
  };
}
