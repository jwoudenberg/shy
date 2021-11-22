{ pkgs, ... }:

pkgs.haskellPackages.callCabal2nix "shy" ./. { }
