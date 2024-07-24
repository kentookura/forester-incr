{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.flake-utils.follows = "flake-utils";
    opam-nix.inputs.opam-repository.follows = "opam-repository";
    forester-irmin.url = "sourcehut:~jonsterling/ocaml-forester-irmin";
    forester-irmin.flake = false;
  };
  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      opam-repository,
      forester-irmin,
    }@inputs:
    let
      package = "forester_incr";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        query = {
          ocaml-base-compiler = "5.2.0";
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          forester_irmin = "*";
          odoc = "*";
          dream-serve = "*";
        };
        scope = on.buildOpamProject' {
          repos = [
            "${opam-repository}"
            forester-irmin
          ];
          pinDepends = true;
        } ./. query;
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope overlay;
        main = scope'.${package};
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames query) scope');
      in
      {
        legacyPackages = scope';
        packages.default = main;
        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          buildInputs = devPackages;
        };
      }
    );
}
