{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/1f0e61e3688e53926a1a5aa749a9a0c487dbb559"; # hls 2.15
    flake-utils.url = "github:numtide/flake-utils";
    miso = {
      url = # path:/home/dan/study/haskell/miso/miso;
        "github:dmjio/miso";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc914";

        sourceFilter = root: with nixpkgs.lib.fileset; toSource {
          inherit root;
          fileset = fileFilter
            (file: file.name == "LICENSE" ||
                   # skip cabal.project.local
                   builtins.any file.hasExt [ "cabal" "hs" "md" "css" ])
            root;
        };
        packageName = "miso-promise";
        pkgs = import nixpkgs { inherit system; };
        # pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.haskell.lib) dontCheck; #  enableCabalFlag;

        haskellPackages = pkgs.haskell.packages.${ghcName}.extend(final: prev: {
          miso =
            dontCheck
              # (enableCabalFlag "template-haskell"
                (final.callCabal2nix "miso" "${inputs.miso}" { });
        });
      in {
        packages.default =
            haskellPackages.callCabal2nix packageName (sourceFilter ./.) {};

        devShells = {
          default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              # code
              ghcid
              cabal-install
              pandoc
              # (import uphack { inherit pkgs; })
            ]) ++ [ haskellPackages.haskell-language-server ]
            ;
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
            # [ hls.devShells.${system}.default ] ++
            shellHook = ''
              export PS1='N$ '
              echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
            '';
          };
        };
    });
}
