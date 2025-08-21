{ pkgs, ... }:
{
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  home.packages = with pkgs; [
    (elixir_ls.overrideAttrs (_: {
      postInstall = "rm -f $out/LICENSE $out/README.md";
    }))
    (pyright.overrideAttrs (_: {
      postInstall = "rm -f $out/LICENSE $out/README.md";
    }))
    jsonnet-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.prettier
    nixd
    sqls
  ];
}
