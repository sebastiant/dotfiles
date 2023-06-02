{ config, pkgs, nixpkgs, lib, ... }: {
  imports = [
    ../programs/zsh/zsh.nix
    ../programs/git.nix
    ../programs/emacs/emacs.nix
    ../programs/vscode.nix
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      cabal-install
      caddy
      dig
      discord
      file
      gawk
      haskellPackages.swarm
      htop
      httpie
      jq
      jsonnet
      k9s
      kubectx
      lazydocker
      lorri
      ngrok
      nix-index
      nixfmt
      openssl
      p7zip
      pandoc
      patchelf
      pstree
      ripgrep
      sqlite
      tree
      unzip
      vim
      watchexec
      whois
    ];
  };

  programs.direnv = {
    enable = true;
    nix-direnv = { enable = true; };
  };
  programs.emacs.enable = true;
  programs.fzf.enable = true;
  programs.bat = {
    enable = true;
    config.theme = "ansi";
  };

  xdg.configFile."alacritty/alacritty.yml".source = ../programs/alacritty.yml;
  xdg.configFile."oh-my-zsh/plugins/nix-shell".source = pkgs.fetchFromGitHub {
    owner = "chisui";
    repo = "zsh-nix-shell";
    rev = "f8574f27e1d7772629c9509b2116d504798fe30a";
    sha256 = "0svskd09vvbzqk2ziw6iaz1md25xrva6s6dhjfb471nqb13brmjq";
  };

  home.file.".ipython/profile_default/ipython_config.py".text = ''
    c.InteractiveShellApp.extensions = ["autoreload"]
    c.InteractiveShellApp.exec_lines = ["%autoreload 2"]
  '';

  home.file.".ghc/ghci.conf".text = ''
    :set prompt "\x03BB> "
    :set prompt-cont " > "

    :set +t

    :def hoogle \x -> return $ ":!hoogle \"" ++ x ++ "\""
    :def doc \x -> return $ ":!hoogle --info \"" ++ x ++ "\""
  '';
}
