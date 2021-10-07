{
  description = "NixOS configuration and home-manager configurations for mac and debian gnu/linux";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = {emacs-overlay, darwin, home-manager, nixpkgs, ...}:
    let
      homeManagerConfFor = config: { ... }: {
          nixpkgs.overlays = [ emacs-overlay.overlay ];
          imports = [ config ];
        };
    in {
    nixosConfigurations.t14 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/t14-nixos/configuration.nix
        home-manager.nixosModules.home-manager {
          home-manager.useUserPackages = true;
          home-manager.users.sebastian = homeManagerConfFor ./hosts/t14-nixos/home.nix;
        }
      ];
    };
    homeManagerConfigurations = {
      t14-debian = home-manager.lib.homeManagerConfiguration {
        home-manager.users.sebastian = homeManagerConfFor ./hosts/t14-nixos/home.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/sebastian";
        username = "sebastian";
        stateVersion = "21.05";
      };
      macbook = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./hosts/macbook/darwin-configuration.nix
          home-manager.darwinModules.home-manager {
            home-manager.users.sebastian = homeManagerConfFor ./hosts/macbook/home.nix;
          }
        ];
      };
    };
  };
}
