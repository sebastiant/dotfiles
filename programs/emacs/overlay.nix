final: prev:
let
  packages = epkgs:
    (with epkgs; [
      ace-window
      alchemist
      beacon
      combobulate
      consult
      corfu
      dap-mode
      dart-mode
      devdocs
      dockerfile-mode
      eat
      editorconfig
      elixir-mode
      elm-mode
      embark
      envrc
      flutter
      flycheck
      forge
      git-gutter
      haskell-ts-mode
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
      po-mode
      popper
      puni
      prettier-js
      prodigy
      python-black
      python-isort
      rainbow-delimiters
      restclient
      ripgrep
      swarm-mode
      syncorate-el
      terraform-mode
      treesit-grammars.with-all-grammars
      vertico
      vertico-posframe
      visual-fill-column
      web-mode
      which-key
      yaml-mode
      yasnippet
    ]);
in {
  emacsSebastiant = (prev.emacsPackagesFor prev.emacs29).emacsWithPackages packages;
}
