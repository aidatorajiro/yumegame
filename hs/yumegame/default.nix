let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskell.packages.ghc96.developPackage {
  root = ./.;
}

