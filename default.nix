{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit  (haskellPackages) cabal
    recursionSchemes
    lens
    free;

in cabal.mkDerivation (self: {
  pname = "LinProg";
  version = "0.0.0.1";
  src = ./.;
  buildDepends = [
    recursionSchemes
    lens
    free
  ];
})
