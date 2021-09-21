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
      counsel
      counsel-projectile
      dap-mode
      dired-single
      doom-themes
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
      ivy
      ivy-prescient
      js2-mode
      lsp-haskell
      lsp-ivy
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
      org-tree-slide
      org-superstar
      hide-mode-line
      perspective
      persp-projectile
      projectile
      py-isort
      python-mode
      pyvenv
      rainbow-delimiters
      typescript-mode
      undo-tree
      use-package
      use-package-ensure-system-package
      web-mode
      which-key
      vterm
      yaml-mode
      editorconfig
      restclient
      jsonnet-mode
      terraform-mode
      po-mode
    ];
  };
}
