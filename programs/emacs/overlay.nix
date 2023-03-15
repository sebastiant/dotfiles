final: prev:
let
  emacs = prev.emacsPackagesFor prev.emacs29;
  emacs-gtk = prev.emacsPackagesFor prev.emacs29-pgtk;
  packages = epkgs:
    (with epkgs; [
      ace-window
      alchemist
      beacon
      corfu
      dap-mode
      dart-mode
      devdocs
      dired-single
      dockerfile-mode
      editorconfig
      elixir-mode
      elm-mode
      embark
      envrc
      flutter
      flycheck
      forge
      git-gutter
      haskell-mode
      helpful
      hide-mode-line
      js2-mode
      jsonnet-mode
      lsp-dart
      lsp-haskell
      lsp-mode
      lsp-pyright
      lsp-treemacs
      lsp-ui
      magit
      marginalia
      minions
      nix-mode
      nov
      no-littering
      orderless
      org
      org-roam
      org-superstar
      org-tree-slide
      pdf-tools
      persp-projectile
      perspective
      po-mode
      popper
      puni
      prettier-js
      prodigy
      projectile
      python-black
      python-isort
      python-mode
      rainbow-delimiters
      restclient
      ripgrep
      swiper
      syncorate-el
      terraform-mode
      typescript-mode
      undo-tree
      use-package
      use-package-ensure-system-package
      vertico
      visual-fill-column
      vterm
      web-mode
      which-key
      yaml-mode
      yasnippet
    ]);
in {
  emacsSebastiant = emacs.emacsWithPackages packages;
  emacsPgtkSebastiant = emacs-gtk.emacsWithPackages packages;
}
