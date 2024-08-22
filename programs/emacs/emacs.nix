{ pkgs, ... }:
{
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  home.packages = with pkgs; [
    elixir_ls
    jsonnet-language-server
    pyright
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.prettier
    nixd
    sqls
  ];
}
