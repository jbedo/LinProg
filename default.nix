{ stdenv ? (import <nixpkgs> {}).stdenv
, fetchurl ? (import <nixpkgs> {}).fetchurl
, haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit  (haskellPackages) cabal
    recursionSchemes
    lens
    free;

  lpSolve = import ./lp_solve.nix {inherit stdenv fetchurl;};

in cabal.mkDerivation (self: {
  pname = "LinProg";
  version = "0.0.0.1";
  src = ./.;
  buildDepends = [
    recursionSchemes
    lens
    free
    lpSolve
  ];
})
