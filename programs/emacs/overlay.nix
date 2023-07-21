final: prev:
let
  packages = epkgs:
    (with epkgs; [
      ace-window
      alchemist
      beacon
      combobulate
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
      jsonnet-mode
      ligature
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
      rainbow-delimiters
      restclient
      ripgrep
      swarm-mode
      swiper
      syncorate-el
      terraform-mode
      treesit-grammars.with-all-grammars
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
  emacsSebastiant = (prev.emacsPackagesFor prev.emacs29).emacsWithPackages packages;
}
