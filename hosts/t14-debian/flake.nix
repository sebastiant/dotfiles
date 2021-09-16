{
  description = "t14-debian home-manager flake";

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, homeManager}: {
    homeConfigurations = {
      "sebastian@t14-debian" = homeManager.lib.homeManagerConfiguration {
        configuration = import ./home.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/sebastian";
        username = "sebastian";
        stateVersion = "21.05";
      };
    };
  };
}
