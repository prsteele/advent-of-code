{ sources ? import ./nix/sources.nix
, pkgsF ? import sources.nixpkgs
}:
let
  # Disable tests for these packages
  dontCheck = [
  ];

  # Disable Haddocks for these packages
  dontHaddock = [
  ];

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          advent-of-code = haskellPackagesNew: haskellPackagesOld: {
            advent-of-code = haskellPackagesNew.callCabal2nix "advent-of-code" ./. { };
          };
          
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                value = haskellPackagesNew.callPackage (./. + "/nix/haskell/${file}") { };
              };
            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix/haskell);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = name: {
                inherit name;
                value = function haskellPackagesOld.${name};
              };
            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_:_: {});

          # Manual overrides for packages
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };
        in
          pkgs.haskellPackages.override {
            overrides = composeExtensionsList [
              advent-of-code
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck dontCheck)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddock)
              manualOverrides
            ];
          };
    };
  };

  pkgs = pkgsF { inherit config; };

in
{ advent-of-code = pkgs.haskellPackages.advent-of-code;
  pkgs = pkgs;
}
