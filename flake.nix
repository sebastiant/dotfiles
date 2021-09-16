{
  description = "NixOS configuration and home-manager configurations for mac and debian gnu/linux";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };
  outputs = { home-manager, nixpkgs, ... }: {
    nixosConfigurations.t14 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/t14-nixos/configuration.nix
        home-manager.nixosModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.sebastian = { ... }: {
            imports = [ ./hosts/t14-nixos/home.nix ];
          };
        }
      ];
    };
    homeManagerConfigurations = {
      t14-debian = home-manager.lib.homeManagerConfiguration {
        configuration = import ./hosts/t14-debian/home.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/sebastian";
        username = "sebastian";
        stateVersion = "21.05";
      };
    };
  };
}
