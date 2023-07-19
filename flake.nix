{
  description =
    "NixOS configuration and home-manager configurations for mac and debian gnu/linux";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    nur.url = "github:nix-community/nur";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    syncorate-el.url = "github:sebastiant/syncorate.el";
    git-mob = {
      url = "github:frost/git-mob/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    swarm = {
      url = "github:swarm-game/swarm";
      flake = false;
    };
  };

  outputs = { swarm, git-mob, syncorate-el, darwin, home-manager, nur, nixos-hardware, nixpkgs, ... }:
    let
      sebastiant-emacs-overlay = import ./programs/emacs/overlay.nix;
      swarm-overlay = import ./programs/emacs/swarm.nix;
      homeManagerConfFor = config:
        { ... }: {
          nixpkgs.overlays = [
            nur.overlay
            syncorate-el.overlays.emacs
            (swarm-overlay swarm)
            sebastiant-emacs-overlay
            git-mob.overlays.default
          ];
          imports = [ config ];
        };
      darwinSystem = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./hosts/macbook/darwin-configuration.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.users.sebastian =
              homeManagerConfFor ./hosts/macbook/home.nix;
          }
        ];
        specialArgs = { inherit nixpkgs; };
      };
      debianSystem = home-manager.lib.homeManagerConfiguration {
        configuration = homeManagerConfFor ./hosts/t14-debian/home.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/sebastian";
        username = "sebastian";
        stateVersion = "21.05";
      };
    in {
      nixosConfigurations.t14 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-t14
          ./hosts/t14-nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useUserPackages = true;
            home-manager.users.sebastian =
              homeManagerConfFor ./hosts/t14-nixos/home.nix;
          }
        ];
        specialArgs = { inherit nixpkgs; };
      };
      debian = debianSystem.activationPackage;
      defaultPackage.x86_64-linux = debianSystem.activationPackage;
      defaultPackage.x86_64-darwin = darwinSystem.system;
    };
}
