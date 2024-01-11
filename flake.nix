{
  description = "Example kickstart OCaml application project.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) dockerTools ocaml-ng mkShell;
          inherit (dockerTools) buildImage;
          inherit (ocaml-ng) ocamlPackages_5_1;
          inherit (ocamlPackages_5_1) buildDunePackage janeStreet;
          name = "example";
          version = "0.1.0";
        in {
          devShells = {
            default = mkShell { inputsFrom = [ self'.packages.default ]; };
          };

          packages = {
            default = buildDunePackage {
              inherit version;
              pname = name;
              src = ./.;

              buildInputs = [
                janeStreet.base
                janeStreet.async
                janeStreet.core_unix
                janeStreet.ppx_let
                ocamlPackages_5_1.cmdliner
                ocamlPackages_5_1.ppx_deriving
                ocamlPackages_5_1.odoc
                ocamlPackages_5_1.utop
                ocamlPackages_5_1.fmt
                ocamlPackages_5_1.jingoo
                ocamlPackages_5_1.omd
                ocamlPackages_5_1.ocaml-lsp
                ocamlPackages_5_1.alcotest
                ocamlPackages_5_1.cohttp
                ocamlPackages_5_1.yojson
              ];
            };

            docker = buildImage {
              inherit name;
              tag = version;
              config = { Cmd = "${self'.packages.default}/bin/${name}"; };
            };
          };
        };
    };
}

