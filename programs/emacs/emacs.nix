{ config, pkgs, ... }:
{
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      alchemist
      beacon
      company
      company-box
      dap-mode
      dired-single
      doom-themes
      editorconfig
      elfeed
      elixir-mode
      envrc
      evil
      evil-collection
      evil-nerd-commenter
      flycheck
      forge
      general
      git-gutter
      haskell-mode
      helpful
      hide-mode-line
      js2-mode
      jsonnet-mode
      lsp-haskell
      lsp-mode
      lsp-pyright
      lsp-treemacs
      lsp-ui
      magit
      minions
      nix-mode
      no-littering
      org
      org-roam
      org-superstar
      org-tree-slide
      persp-projectile
      perspective
      po-mode
      projectile
      py-isort
      python-mode
      pyvenv
      rainbow-delimiters
      restclient
      ripgrep
      terraform-mode
      typescript-mode
      undo-tree
      use-package
      use-package-ensure-system-package
      visual-fill-column
      vterm
      web-mode
      which-key
      yaml-mode
    ];
  };
}
