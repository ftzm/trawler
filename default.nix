let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc865";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
  compiler.developPackage {
    root = ./.;
    source-overrides = {
      streamly = builtins.fetchTarball
        "https://github.com/composewell/streamly/archive/3da7028.tar.gz";
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
