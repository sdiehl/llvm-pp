let
  pkgs = import <nixpkgs> {};
  zlib = pkgs.zlib;
  ncurses = pkgs.ncurses;
  haskellPackages = pkgs.haskellPackages;
  cabal = haskellPackages.cabal;
  cabalInstall = haskellPackages.cabalInstall;
in cabal.mkDerivation (self: {
  pname = "llvm-pp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildDepends = [
    zlib
    ncurses
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    haskellPackages.ansiWlPprint
  ];
  buildTools = [ "cabalInstall_1_18_1_3" ];
  testDepends = [
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    haskellPackages.ansiWlPprint
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
