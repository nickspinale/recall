{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, stdenv }:
      mkDerivation {
        pname = "recall";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ array base ];
        homepage = "https://github.com/nickspinale/recall.git";
        description = "Memoization using type families";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
