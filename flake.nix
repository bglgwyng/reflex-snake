{
  description = "dEsCrIpTiOn";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      name = "reflex-snake";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            ${name} = final.haskell-nix.cabalProject'
              {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc965";
                shell.tools = {
                  cabal = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  stylish-haskell = "latest";
                  hpack = "latest";
                };
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.${name}.flake { };
      in
      (flake // rec {
        packages.default = flake.packages."${name}:exe:app";
      }));
}
