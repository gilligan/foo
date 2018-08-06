{ ghcVersion ? "ghc843"
, pkgs ? import ./nix/nixpkgs.nix }:

let 

  inherit (pkgs) iana-etc cacert;
  inherit (pkgs.haskell.lib) buildStrictly justStaticExecutables overrideCabal;
  inherit (pkgs.lib.strings) splitString;
  inherit (builtins) elemAt;

  ghc = pkgs.haskell.packages."${ghcVersion}";
  cabal2nix = ghc.callCabal2nix;
  addDepsToEnv = drv: deps: drv.env.overrideAttrs(oldAttrs: { buildInputs = [ deps ]; });

  hairport-ghcid = pkgs.writeScriptBin "hairport-ghcid"
  ''
    ${ghc.ghcid}/bin/ghcid -c "${ghc.cabal-install}/bin/cabal new-repl library:hairport"
  '';

in rec {

  hairport = justStaticExecutables (cabal2nix "hairport" (pkgs.lib.cleanSource ./.) {});

  shell = addDepsToEnv hairport [ pkgs.cabal-install 
                                  pkgs.haskellPackages.ghcid 
                                  pkgs.haskellPackages.stylish-haskell 
                                  hairport-ghcid
                                ];

}
