{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, servant
      , servant-server, stdenv, text, transformers, wai, wai-extra, warp
      }:
      mkDerivation {
        pname = "hairport";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring servant servant-server text transformers wai
          wai-extra warp
        ];
        executableHaskellDepends = [
          aeson base servant servant-server transformers wai warp
        ];
        homepage = "https://github.hc.ag/tpflug/hairport";
        description = "Haskell/Servant based airport service";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
