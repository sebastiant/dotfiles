let
  packages = epkgs: (with epkgs; [
    ace-window
    alchemist
    beacon
    consult
    corfu
    dap-mode
    dired-single
    dockerfile-mode
    editorconfig
    elfeed
    elixir-mode
    elm-mode
    embark
    embark-consult
    envrc
    flycheck
    forge
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
    marginalia
    minions
    nix-mode
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
    projectile
    python-black
    python-isort
    python-mode
    rainbow-delimiters
    restclient
    ripgrep
    smartparens
    swiper
    terraform-mode
    tree-sitter
    tree-sitter-langs
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
    zeal-at-point
  ]) ++ (with epkgs.melpaStablePackages; [
    evil
    evil-collection
    evil-nerd-commenter
  ]);
in
prev: final: {
  emacsNativeCompSebastiant = (final.emacsPackagesFor final.emacsNativeComp).emacsWithPackages packages;
}