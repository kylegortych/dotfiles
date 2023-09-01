let
  pkgs = import <nixpkgs> {};
  system = "x86_64-linux";
in
pkgs.nixosTest {
  system = system;
  configFile = ./configuration.nix;
}
